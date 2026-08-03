(in-package #:cl-stack-http)

;;; Platform-selectable http-protocol backends.
;;; Soft-load ASDF systems — backends are not hard deps of cl-stack-http.

(defvar *preferred-backend* :auto
  "Default backend keyword for ENSURE-HTTP-BACKEND.
   :auto | :dexador | :async | :winhttp | an HTTP-BACKEND instance.")

(defparameter *backend-systems*
  '((:dexador . "http-backend-dexador")
    (:async . "http-backend-async")
    (:winhttp . "http-backend-winhttp"))
  "Keyword → ASDF system name.")

(defun %backend-system (name)
  (or (cdr (assoc name *backend-systems*))
      (error 'http-protocol-error
             :message (format nil "unknown backend keyword ~S" name))))

(defun load-http-backend (name)
  "ASDF-load the system for NAME (:dexador|:async|:winhttp). Returns NAME."
  (let ((sys (%backend-system name)))
    (asdf:load-system sys)
    name))

(defun %try-make-backend (name)
  "Load system and construct backend, or NIL if unavailable / unsupported."
  (handler-case
      (progn
        (load-http-backend name)
        (ecase name
          (:dexador
           (funcall (find-symbol "MAKE-DEXADOR-BACKEND" :http-backend-dexador)))
          (:async
           ;; Prefer libuv event backend when present (Windows-primary stack default).
           (handler-case (asdf:load-system "event-backend-libuv")
             (error () nil))
           (when (and (find-package :event-backend-libuv)
                      (fboundp (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))
                      (find-package :event-protocol)
                      (boundp (find-symbol "*EVENT-BACKEND*" :event-protocol)))
             (setf (symbol-value (find-symbol "*EVENT-BACKEND*" :event-protocol))
                   (funcall (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))))
           (funcall (find-symbol "MAKE-ASYNC-BACKEND" :http-backend-async)))
          (:winhttp
           (funcall (find-symbol "MAKE-WINHTTP-BACKEND" :http-backend-winhttp)))))
    (asdf:missing-component () nil)
    (error (e)
      (when (typep e 'unsupported-operation)
        (return-from %try-make-backend nil))
      ;; Missing optional event backend / platform stub — treat as unavailable.
      (warn "cl-stack-http: backend ~S unavailable: ~A" name e)
      nil)))

(defun %auto-backend-order ()
  "Platform preference for :auto — sync-friendly defaults."
  #+win32 '(:winhttp :async :dexador)
  #-win32 '(:dexador :async))

(defun ensure-http-backend (&optional (prefer *preferred-backend*))
  "Return an HTTP-BACKEND, loading systems as needed.

   PREFER — :auto (platform order), :dexador, :async, :winhttp,
   an HTTP-BACKEND instance, or NIL → *HTTP-BACKEND* then :auto."
  (cond
    ((http-backend-p prefer) prefer)
    ((null prefer)
     (or *http-backend* (ensure-http-backend :auto)))
    ((eq prefer :auto)
     (or (loop for name in (%auto-backend-order)
               for b = (%try-make-backend name)
               when b return b)
         (error 'http-protocol-error
                :message "no http backend available (tried auto order)")))
    ((keywordp prefer)
     (or (%try-make-backend prefer)
         (error 'http-protocol-error
                :message (format nil "backend ~S unavailable" prefer))))
    (t (error 'http-protocol-error
              :message (format nil "invalid backend prefer ~S" prefer)))))

(defmacro with-backend ((&optional (prefer '*preferred-backend*)) &body body)
  "Bind *HTTP-BACKEND* (and keep *PREFERRED-BACKEND*) around BODY."
  (let ((b (gensym "BACKEND")))
    `(let* ((,b (ensure-http-backend ,prefer))
            (*http-backend* ,b)
            (*preferred-backend*
             (if (keywordp ,prefer) ,prefer *preferred-backend*)))
       ,@body)))

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

(defun %ensure-system-loaded (system)
  "ASDF-load SYSTEM unless it is already loaded. ENSURE-HTTP-BACKEND runs on
   every facade request: a bare ASDF:LOAD-SYSTEM re-plans the whole tree each
   time (about 130 ms per GET with dexador, #29). Use ASDF:LOAD-SYSTEM to
   pick up edited sources."
  (unless (asdf:component-loaded-p system)
    (asdf:load-system system))
  system)

(defun load-http-backend (name)
  "ASDF-load the system for NAME (:dexador|:async|:winhttp) unless it is
   already loaded. Returns NAME."
  (%ensure-system-loaded (%backend-system name))
  name)

(defun %soft-symbol (name package)
  "Symbol NAME in PACKAGE, or NIL when PACKAGE is not loaded."
  (let ((p (find-package package)))
    (and p (find-symbol name p))))

(defun %wire-async-event-context ()
  "Give http-backend-async an event backend, so (with-backend (:async) (get …))
   needs no manual wiring (#27). A blocking SEND takes the bound
   event-protocol:*event-backend* + *event-loop*, or else makes a fresh loop
   from http-backend-async:*event-backend-maker*; nothing set the maker, so it
   signalled \"No event loop\".

   Loads event-backend-libuv when it is installed. When unset, sets
   event-protocol:*event-backend* to a libuv backend and the maker to a thunk
   returning the bound event-protocol:*event-backend* (libuv when nothing is
   bound). A caller's own *event-backend* or maker is never replaced.
   Returns true when a blocking SEND can find an event backend."
  (handler-case (%ensure-system-loaded "event-backend-libuv")
    (error () nil))
  (let* ((maker (%soft-symbol "*EVENT-BACKEND-MAKER*" :http-backend-async))
         (eb (%soft-symbol "*EVENT-BACKEND*" :event-protocol))
         (el (%soft-symbol "*EVENT-LOOP*" :event-protocol))
         (make-libuv (%soft-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))
         (libuv-p (and make-libuv (fboundp make-libuv))))
    (when (and libuv-p eb (null (symbol-value eb)))
      (setf (symbol-value eb) (funcall make-libuv)))
    (when (and maker eb (null (symbol-value maker))
               (or libuv-p (symbol-value eb)))
      (setf (symbol-value maker)
            (lambda ()
              (or (symbol-value eb)
                  (and libuv-p (funcall make-libuv))
                  (error 'http-protocol-error
                         :message "http-backend-async: no event backend (install event-backend-libuv or bind event-protocol:*event-backend*)")))))
    (and maker
         (or (symbol-value maker)
             (and eb el (symbol-value eb) (symbol-value el)))
         t)))

(defun %try-make-backend (name &key auto)
  "Load system and construct backend, or NIL if unavailable / unsupported.
   AUTO (the :auto order): :async counts as available only when it can find
   an event backend (%WIRE-ASYNC-EVENT-CONTEXT); an explicit :async is made
   either way, for a caller who binds its own event loop."
  (handler-case
      (progn
        (load-http-backend name)
        (ecase name
          (:dexador
           (funcall (find-symbol "MAKE-DEXADOR-BACKEND" :http-backend-dexador)))
          (:async
           (when (or (%wire-async-event-context) (not auto))
             (funcall (find-symbol "MAKE-ASYNC-BACKEND" :http-backend-async))))
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
  "Platform preference for :auto (README backend table). Windows: winhttp →
   async → dexador. Elsewhere the stack's own async engine comes first (#27):
   async (with an event backend, libuv by default) → dexador. ENSURE-HTTP-BACKEND
   takes the first one available; async without an event backend is skipped."
  #+win32 '(:winhttp :async :dexador)
  #-win32 '(:async :dexador))

(defun ensure-http-backend (&optional (prefer *preferred-backend*))
  "Return an HTTP-BACKEND, loading systems as needed.

   PREFER — :auto (platform order, see %AUTO-BACKEND-ORDER), :dexador, :async,
   :winhttp, an HTTP-BACKEND instance, or NIL → *HTTP-BACKEND* then :auto.
   :async (explicit, or chosen by :auto) also wires an event backend when
   event-backend-libuv is installed, so blocking calls need no event loop."
  (cond
   ((http-backend-p prefer) prefer)
   ((null prefer)
    (or *http-backend* (ensure-http-backend :auto)))
   ((eq prefer :auto)
    (or (loop for name in (%auto-backend-order)
              for b = (%try-make-backend name :auto t)
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

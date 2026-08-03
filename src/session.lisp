(in-package #:cl-stack-http)

;;; Session = http-client + bound backend + default codecs (requests.Session).

(defclass http-session ()
  ((backend :initarg :backend :reader session-backend)
   (client :initarg :client :reader session-client)
   (json :initarg :json :reader session-json-p :initform t)
   (sexp :initarg :sexp :reader session-sexp-p :initform t)))

(defun http-session-p (x) (typep x 'http-session))

(defun make-session (&rest keys
                     &key (backend nil backendp)
                       (preferred *preferred-backend*)
                       (json t) (sexp t)
                       base-url headers cookie-jar auth timeout retry
                       max-redirects proxy pool verify
                     &allow-other-keys)
  "Create an HTTP-SESSION. BACKEND defaults via ENSURE-HTTP-BACKEND."
  (declare (ignore base-url headers cookie-jar auth timeout retry
                   max-redirects proxy pool verify))
  (let* ((backend (if backendp
                      (ensure-http-backend backend)
                      (ensure-http-backend preferred)))
         (client-keys (remove-from-plist keys :backend :preferred :json :sexp))
         (client (apply #'make-http-client backend client-keys)))
    (make-instance 'http-session
                   :backend backend
                   :client client
                   :json json
                   :sexp sexp)))

(defun call-with-session (session fn)
  (let ((*http-backend* (session-backend session))
        (*http-client* (session-client session)))
    (with-default-codecs (:json (session-json-p session)
                         :sexp (session-sexp-p session))
      (funcall fn session))))

(defmacro with-session ((var &rest session-keys) &body body)
  "Bind VAR to a fresh HTTP-SESSION and run BODY with backend/client/codecs."
  `(call-with-session (make-session ,@session-keys)
                      (lambda (,var) ,@body)))

(defun session-request (session method url &rest keys)
  "Issue METHOD against URL using SESSION's backend/client/codecs + path coerce."
  (call-with-session
   session
   (lambda (s)
     (apply #'request method url
            :backend (session-backend s)
            :client (session-client s)
            :preferred nil
            keys))))

(defun session-get (session url &rest keys)
  (apply #'session-request session :get url keys))

(defun session-post (session url &rest keys)
  (apply #'session-request session :post url keys))

(defun session-put (session url &rest keys)
  (apply #'session-request session :put url keys))

(defun session-patch (session url &rest keys)
  (apply #'session-request session :patch url keys))

(defun session-delete (session url &rest keys)
  (apply #'session-request session :delete url keys))

(defun session-head (session url &rest keys)
  (apply #'session-request session :head url keys))

(defun session-options (session url &rest keys)
  (apply #'session-request session :options url keys))

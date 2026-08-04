(in-package #:cl-stack-http)

;;; Session = http-client + bound backend + default codecs (requests.Session / httpx.Client).

(defclass http-session ()
  ((backend :initarg :backend :reader session-backend)
   (client :initarg :client :reader session-client)
   (params :initarg :params :accessor session-params :initform nil
           :documentation "Default query params merged into every request.")
   (cert :initarg :cert :accessor session-cert :initform nil)
   (trust-env :initarg :trust-env :accessor session-trust-env-p :initform t)
   (json :initarg :json :reader session-json-p :initform t)
   (sexp :initarg :sexp :reader session-sexp-p :initform t)))

(defun http-session-p (x) (typep x 'http-session))

(defun make-session (&rest keys
                     &key (backend nil backendp)
                       (preferred *preferred-backend*)
                       (json t) (sexp t)
                       params cert
                       (trust-env t)
                       (client-class 'http-client)
                       base-url headers cookie-jar auth retry
                       max-redirects proxy pool verify
                     &allow-other-keys)
  "Create an HTTP-SESSION.

   Defaults (httpx-ish): TIMEOUT 5.0 when omitted, TRUST-ENV T (env proxy + netrc).
   PARAMS — default query alist. CERT — client certificate designator.
   CLIENT-CLASS — subclass of HTTP-CLIENT for CLOS hooks (PREPARE-REQUEST /
   HANDLE-RESPONSE). Default HTTP-CLIENT."
  (declare (ignore base-url headers cookie-jar auth retry
                   max-redirects proxy pool verify))
  (let* ((backend (if backendp
                      (ensure-http-backend backend)
                      (ensure-http-backend preferred)))
         (client-keys (remove-from-plist keys :backend :preferred :json :sexp
                                              :params :cert :trust-env
                                              :client-class))
         (client-keys (if (eq (getf keys :timeout :%missing) :%missing)
                          (list* :timeout 5.0 client-keys)
                          client-keys))
         (client-keys
           (if (and trust-env (eq (getf keys :proxy :%missing) :%missing))
               (list* :proxy (make-http-proxy-config :system t) client-keys)
               client-keys))
         (client (progn
                   (unless (subtypep client-class 'http-client)
                     (error 'http-protocol-error
                            :message
                            (format nil ":client-class ~S is not a subtype of HTTP-CLIENT"
                                    client-class)))
                   (let ((c (apply #'make-http-client backend client-keys)))
                     (if (typep c client-class)
                         c
                         (change-class c client-class))))))
    (make-instance 'http-session
                   :backend backend
                   :client client
                   :params params
                   :cert cert
                   :trust-env trust-env
                   :json json
                   :sexp sexp)))

(defun close-session (session)
  "Clear connection pool (httpx Client.close). Safe to call multiple times."
  (when (http-session-p session)
    (let ((pool (http-client-pool (session-client session))))
      (when (and pool (not (eq pool t)))
        (ignore-errors (pool-clear pool)))))
  session)

(defun call-with-session (session fn)
  (let ((*http-backend* (session-backend session))
        (*http-client* (session-client session)))
    (with-default-codecs (:json (session-json-p session)
                         :sexp (session-sexp-p session))
      (funcall fn session))))

(defmacro with-session ((var &rest session-keys) &body body)
  "Bind VAR to a fresh HTTP-SESSION; close pool on exit."
  `(let ((,var (make-session ,@session-keys)))
     (unwind-protect
          (call-with-session ,var (lambda (,var) ,@body))
       (close-session ,var))))

(defun %session-call-keys (session keys)
  (let* ((trust (if (eq (getf keys :trust-env :%missing) :%missing)
                    (session-trust-env-p session)
                    (getf keys :trust-env)))
         (cert (or (getf keys :cert) (session-cert session)))
         (keys (remove-from-plist keys :cert :trust-env)))
    (list* :backend (session-backend session)
           :client (session-client session)
           :preferred nil
           :default-params (session-params session)
           :cert cert
           :trust-env trust
           keys)))

(defun session-request (session method url &rest keys)
  "Issue METHOD against URL using SESSION defaults + path coerce."
  (call-with-session
   session
   (lambda (s)
     (apply #'request method url (%session-call-keys s keys)))))

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

(defun session-stream (session method url &rest keys)
  (apply #'session-request session method url :want-stream t keys))

(defun session-request-async (session method url &rest keys)
  (call-with-session
   session
   (lambda (s)
     (apply #'request-async method url (%session-call-keys s keys)))))

(defun session-get-async (session url &rest keys)
  (apply #'session-request-async session :get url keys))

(defun session-post-async (session url &rest keys)
  (apply #'session-request-async session :post url keys))

(defun session-put-async (session url &rest keys)
  (apply #'session-request-async session :put url keys))

(defun session-patch-async (session url &rest keys)
  (apply #'session-request-async session :patch url keys))

(defun session-delete-async (session url &rest keys)
  (apply #'session-request-async session :delete url keys))

(defun session-head-async (session url &rest keys)
  (apply #'session-request-async session :head url keys))

(defun session-options-async (session url &rest keys)
  (apply #'session-request-async session :options url keys))

(defun session-stream-async (session method url &rest keys)
  (apply #'session-request-async session method url :want-stream t keys))

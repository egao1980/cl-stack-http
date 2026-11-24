;;;; cl-stack-http#34: the proxy a facade request goes through, on both
;;;; backends. A request :PROXY wins over the session's, and a session :PROXY
;;;; wins over the environment (requests: Session.proxies over the environment
;;;; proxies merged under trust_env). With :TRUST-ENV and no explicit proxy
;;;; the environment names it; with :TRUST-ENV NIL it is not read.

(in-package #:cl-stack-http/tests)

(defparameter *proxy-variables*
  '("http_proxy" "HTTP_PROXY" "https_proxy" "HTTPS_PROXY"
    "all_proxy" "ALL_PROXY" "no_proxy" "NO_PROXY")
  "The environment variables http-protocol reads for the system proxy.")

(defparameter +unresolvable-url+ "http://origin.invalid/a"
  "A .invalid host never resolves (RFC 6761): a request that goes direct
   fails, one that goes through a proxy reaches it.")

(defun %call-with-proxy-environment (bindings thunk)
  "THUNK with the proxy variables BINDINGS names ((name . value)) set and the
   others blank, which http-protocol reads as unset. Restores them after (an
   unset one comes back blank). *DEFAULT-PROXY-CONFIG* is rebound, so the
   environment snapshot it caches is this one."
  (let ((saved (mapcar (lambda (name) (cons name (uiop:getenv name)))
                       *proxy-variables*))
        (*default-proxy-config* nil))
    (unwind-protect
         (progn
           (dolist (name *proxy-variables*)
             (setf (uiop:getenv name)
                   (or (cdr (assoc name bindings :test #'string=)) "")))
           (funcall thunk))
      (loop for (name . value) in saved
            do (setf (uiop:getenv name) (or value ""))))))

(defun %proxy-response (name)
  (%octets (format nil "HTTP/1.1 200 OK~C~CContent-Length: ~D~C~C~C~C~A"
                   #\Return #\Newline (length name)
                   #\Return #\Newline #\Return #\Newline name)))

(defun %serve-proxy-connection (client name record)
  "Answer each request head on CLIENT with 200 NAME, after (RECORD head)."
  (handler-case
      (unwind-protect
           (let ((stream (usocket:socket-stream client)))
             (loop for head = (%read-request-head stream)
                   while head
                   do (funcall record head)
                      (write-sequence (%proxy-response name) stream)
                      (finish-output stream)))
        (ignore-errors (usocket:socket-close client)))
    (error () nil)))

(defun %accept-proxy-connections (server stopping name record)
  (handler-case
      (loop (let ((client (usocket:socket-accept server)))
              (when (car stopping)
                (ignore-errors (usocket:socket-close client))
                (return))
              (bt:make-thread (lambda () (%serve-proxy-connection client name record))
                              :name "cl-stack-http recording proxy conn")))
    (error () nil)))

(defun %recording-proxy (name)
  "Loopback stand-in for an HTTP proxy: every request is answered 200 with
   body NAME and its head recorded. → (values proxy-url heads-fn stop-fn).
   STOP-FN wakes the acceptor before closing the listener, as
   %KEEP-ALIVE-ORIGIN does."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t :backlog 64
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (stopping (list nil))
         (heads (list nil))
         (lock (bt:make-lock "cl-stack-http recording proxy"))
         (record (lambda (head)
                   (bt:with-lock-held (lock) (push head (car heads)))))
         (acceptor (bt:make-thread
                    (lambda () (%accept-proxy-connections server stopping name record))
                    :name "cl-stack-http recording proxy")))
    (values (format nil "http://127.0.0.1:~D" port)
            (lambda () (bt:with-lock-held (lock) (reverse (car heads))))
            (lambda ()
              (setf (car stopping) t)
              (when (bt:thread-alive-p acceptor)
                (ignore-errors
                 (usocket:socket-close (usocket:socket-connect "127.0.0.1" port))))
              (ignore-errors (bt:join-thread acceptor))
              (ignore-errors (usocket:socket-close server))))))

(defun %call-with-proxies (thunk)
  "THUNK on a plist of three recording proxies, :SESSION :REQUEST and :ENV,
   each (url . heads-fn) and answering with its role's name. Stops them after."
  (let ((proxies '())
        (stops '()))
    (unwind-protect
         (progn
           (dolist (role '(:session :request :env))
             (multiple-value-bind (url heads stop)
                 (%recording-proxy (string-downcase role))
               (push stop stops)
               (setf proxies (list* role (cons url heads) proxies))))
           (funcall thunk proxies))
      (mapc #'funcall stops))))

(defun %proxied-text (thunk)
  "RESPONSE-TEXT of THUNK's response: the role of the proxy that answered. A
   request that went direct fails on the .invalid host: → (:ERROR type text)."
  (handler-case (response-text (funcall thunk))
    (error (e) (list :error (type-of e) (princ-to-string e)))))

(defun %send-via (via backend session-keys request-keys)
  "One GET of +UNRESOLVABLE-URL+ → its HTTP-RESPONSE. VIA :SESSION sends
   SESSION-GET on a session made with BACKEND and SESSION-KEYS,
   :SESSION-ASYNC SESSION-GET-ASYNC on a libuv loop, :GET a bare GET with
   SESSION-KEYS as its own keys."
  (if (eq via :get)
      (apply #'get +unresolvable-url+ :preferred backend
             (append session-keys request-keys))
      (let ((s (apply #'make-session :backend backend session-keys)))
        (unwind-protect
             (if (eq via :session-async)
                 (let* ((out (%run-promises-on-libuv
                              (lambda ()
                                (list (apply #'session-get-async s +unresolvable-url+
                                             request-keys)))))
                        (res (if (listp out) (first out) out)))
                   (if (typep res 'http-response)
                       res
                       (error "session-get-async: ~A" res)))
                 (apply #'session-get s +unresolvable-url+ request-keys))
          (close-session s)))))

(defun %which-proxy (backend &key env session request
                               (trust-env nil trust-env-p) (via :session))
  "GET +UNRESOLVABLE-URL+ on BACKEND with three recording proxies up. ENV,
   SESSION and REQUEST name the proxy (:ENV :SESSION :REQUEST) that
   http_proxy, the session's :PROXY and the request's :PROXY point at; NIL
   leaves that one unset. TRUST-ENV, when given, goes to the session (or the
   bare GET). VIA as for %SEND-VIA.
   → (values the answering proxy's role or the error, the heads each saw)"
  (%call-with-proxies
   (lambda (p)
     (let* ((session-keys (append (when session
                                    (list :proxy (car (getf p session))))
                                  (when trust-env-p
                                    (list :trust-env trust-env))))
            (request-keys (append (when request
                                    (list :proxy (car (getf p request))))
                                  (list :timeout 5)))
            (got (%call-with-proxy-environment
                  (when env
                    (list (cons "http_proxy" (car (getf p env)))))
                  (lambda ()
                    (%proxied-text
                     (lambda ()
                       (%send-via via backend session-keys request-keys)))))))
       (values got
               (loop for role in '(:session :request :env)
                     collect role
                     collect (funcall (cdr (getf p role)))))))))

(defun %proxy-saw-p (heads role)
  "T when the proxy for ROLE got the absolute-form GET of +UNRESOLVABLE-URL+."
  (some (lambda (head)
          (eql 0 (search (format nil "GET ~A " +unresolvable-url+) head)))
        (getf heads role)))

(defun %proxy-test-backends ()
  "dexador always; async too when its stack is installed."
  (if (%async-stack-installed-p) '(:dexador :async) '(:dexador)))

(deftest session-proxy-wins-under-trust-env
  (dolist (backend (%proxy-test-backends))
    (testing (format nil "~(~A~): a session :proxy, trust-env by default, no proxy variable"
                     backend)
      (multiple-value-bind (got heads) (%which-proxy backend :session :session)
        (ok (equal "session" got) (format nil "answered by ~S" got))
        (ok (%proxy-saw-p heads :session) (format nil "proxy heads ~S" heads))))
    (testing (format nil "~(~A~): a session :proxy wins over http_proxy" backend)
      (multiple-value-bind (got heads)
          (%which-proxy backend :session :session :env :env)
        (ok (equal "session" got) (format nil "answered by ~S" got))
        (ok (null (getf heads :env)) (format nil "http_proxy heads ~S" (getf heads :env)))))
    (testing (format nil "~(~A~): a request :proxy wins over the session's" backend)
      (let ((got (%which-proxy backend :session :session :request :request :env :env)))
        (ok (equal "request" got) (format nil "answered by ~S" got))))))

(deftest environment-proxy-under-trust-env
  (dolist (backend (%proxy-test-backends))
    (testing (format nil "~(~A~): a session without :proxy takes http_proxy" backend)
      (let ((got (%which-proxy backend :env :env)))
        (ok (equal "env" got) (format nil "answered by ~S" got))))
    (testing (format nil "~(~A~): a bare GET takes http_proxy" backend)
      (let ((got (%which-proxy backend :env :env :via :get)))
        (ok (equal "env" got) (format nil "answered by ~S" got))))))

(deftest trust-env-nil-does-not-read-the-environment
  (dolist (backend (%proxy-test-backends))
    (testing (format nil "~(~A~): a :trust-env nil session goes direct past http_proxy"
                     backend)
      (multiple-value-bind (got heads)
          (%which-proxy backend :env :env :trust-env nil)
        (ok (consp got) (format nil "answered by ~S" got))
        (ok (null (getf heads :env)) (format nil "http_proxy heads ~S" (getf heads :env)))))
    (testing (format nil "~(~A~): a bare GET with :trust-env nil goes direct" backend)
      (let ((got (%which-proxy backend :env :env :trust-env nil :via :get)))
        (ok (consp got) (format nil "answered by ~S" got))))
    (testing (format nil "~(~A~): a :trust-env nil session keeps its :proxy" backend)
      (let ((got (%which-proxy backend :env :env :session :session :trust-env nil)))
        (ok (equal "session" got) (format nil "answered by ~S" got))))))

(deftest session-proxy-for-async-calls
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (testing "async: SESSION-GET-ASYNC goes through the session :proxy, not http_proxy"
        (let ((got (%which-proxy :async :session :session :env :env
                                        :via :session-async)))
          (ok (equal "session" got) (format nil "answered by ~S" got))))))

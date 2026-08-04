(in-package #:cl-stack-http)

;;; requests/httpx-shaped facade over http: + path coerce + codecs.

(defun %merge-params (base extra)
  "Merge query param alists; EXTRA wins on duplicate keys."
  (cond
    ((null base) extra)
    ((null extra) base)
    (t
     (append extra
             (remove-if (lambda (pair)
                          (assoc (car pair) extra :test #'equalp))
                        base)))))

(defun %apply-json-kwarg (keys)
  "httpx json= → :data + :data-type :json."
  (let ((jsonp (not (eq (getf keys :json :%missing) :%missing))))
    (if (not jsonp)
        keys
        (progn
          (when (getf keys :data)
            (error 'http-protocol-error
                   :message "cannot pass both :json and :data"))
          (let* ((json (getf keys :json))
                 (keys (copy-list keys)))
            (remf keys :json)
            (setf (getf keys :data) json
                  (getf keys :data-type) :json)
            keys)))))

(defun %apply-cert (keys cert)
  "Stash cert paths on request :extras for backends that honor them."
  (multiple-value-bind (cert-path key-path) (normalize-cert (or cert (getf keys :cert)))
    (if (null cert-path)
        keys
        (let* ((keys (copy-list keys))
               (extras (copy-list (getf keys :extras))))
          (setf (getf extras :certificate) cert-path)
          (when key-path (setf (getf extras :key) key-path))
          (setf (getf keys :extras) extras)
          keys))))

(defun %prepare-keys (keys &key (slurp :auto) filesystem backend
                             default-params cert)
  "Coerce path-like :files/:content; apply :json; merge params; strip local keys."
  (let* ((keys (%apply-json-kwarg (copy-list keys)))
         (keys (%apply-cert keys cert))
         (files (getf keys :files))
         (content (getf keys :content))
         (params (%merge-params default-params (getf keys :params))))
    (when files
      (setf (getf keys :files)
            (coerce-files files :slurp slurp :filesystem filesystem
                                :backend backend)))
    (when content
      (setf (getf keys :content)
            (coerce-upload content :slurp slurp :filesystem filesystem
                                   :backend backend)))
    (when params
      (setf (getf keys :params) params))
    (remove-from-plist keys :slurp :filesystem :preferred :cert
                            :trust-env)))

(defun %resolve-proxy (proxy trust-env)
  (cond
    ((not trust-env) proxy)
    (proxy proxy)
    (t (make-http-proxy-config :system t))))

(defun %resolve-auth (auth url trust-env)
  (cond
    (auth auth)
    ((not trust-env) nil)
    (t (netrc-auth-for-url url))))

(defun %plist-to-request (method url keys)
  (apply #'make-http-request :method method :url url
         (loop for (k v) on keys by #'cddr
               unless (member k '(:backend :client))
                 collect k and collect v)))

(defun request (method url &rest keys
                &key (backend nil backendp)
                  (preferred *preferred-backend*)
                  (client nil clientp)
                  (slurp :auto) filesystem
                  auth cert
                  (trust-env t)
                  raise-for-status
                  default-params
                &allow-other-keys)
  "Sync request with path coerce + default codecs. Selects backend if unbound.

   Extra DX vs http-protocol:
   - :JSON → :DATA + :DATA-TYPE :JSON
   - :CERT client certificate (stored on :extras)
   - :TRUST-ENV T → env/system proxy + ~/.netrc basic auth when :AUTH omitted
   - :AUTH (:DIGEST user pass) → 401 challenge retry (CLOS auth protocol)
   - :AUTH AUTH-OBJECT (e.g. cl-stack-oauth2:oauth2-auth) → PREPARE-AUTH + HANDLE-AUTH-RESPONSE
   - path / httpx file tuples in :FILES / :CONTENT"
  (let* ((backend (if backendp backend (ensure-http-backend preferred)))
         (*http-backend* backend)
         (auth (%resolve-auth auth url trust-env))
         (managedp (auth-object-p auth))
         (proxy (%resolve-proxy (getf keys :proxy) trust-env))
         (http-keys (%prepare-keys
                     (remove-from-plist keys :backend :preferred :slurp
                                             :filesystem :auth :cert
                                             :trust-env :default-params)
                     :slurp slurp :filesystem filesystem :backend backend
                     :cert cert
                     :default-params default-params))
         (http-keys (let ((k (copy-list http-keys)))
                      (when proxy (setf (getf k :proxy) proxy))
                      k)))
    (with-default-codecs ()
      (if managedp
          (let* ((client (if clientp
                             client
                             (or *http-client* (make-http-client backend))))
                 (req0 (%plist-to-request method url http-keys))
                 (wire (prepare-auth auth req0))
                 (req (if wire (copy-request-with-auth req0 wire
                                                       :raise-for-status nil)
                          req0))
                 (res (send backend client req))
                 (final (handle-auth-response auth backend client req res)))
            (when raise-for-status
              (raise-for-status final))
            final)
          (let* ((wire (prepare-auth auth nil))
                 (http-keys (if wire
                                (let ((k (copy-list http-keys)))
                                  (setf (getf k :auth) wire)
                                  k)
                                http-keys)))
            (apply #'http:request method url
                   :backend backend
                   :raise-for-status raise-for-status
                   (append (when clientp (list :client client))
                           http-keys)))))))

(defun get (url &rest keys &key &allow-other-keys)
  (apply #'request :get url keys))

(defun head (url &rest keys &key &allow-other-keys)
  (apply #'request :head url keys))

(defun options (url &rest keys &key &allow-other-keys)
  (apply #'request :options url keys))

(defun post (url &rest keys &key &allow-other-keys)
  (apply #'request :post url keys))

(defun put (url &rest keys &key &allow-other-keys)
  (apply #'request :put url keys))

(defun patch (url &rest keys &key &allow-other-keys)
  (apply #'request :patch url keys))

(defun delete (url &rest keys &key &allow-other-keys)
  (apply #'request :delete url keys))

(defun request-async (method url &rest keys
                      &key (backend nil backendp)
                        (preferred *preferred-backend*)
                        (client nil clientp)
                        (slurp :auto) filesystem
                        auth cert
                        (trust-env t)
                        default-params
                      &allow-other-keys)
  "Async request → Blackbird promise of HTTP-RESPONSE.
   HANDLE-AUTH-RESPONSE (Digest/OAuth2 401 retry) is sync-only; PREPARE-AUTH runs."
  (let* ((backend (if backendp backend (ensure-http-backend preferred)))
         (*http-backend* backend)
         (auth (%resolve-auth auth url trust-env))
         (wire-auth (prepare-auth auth nil))
         (proxy (%resolve-proxy (getf keys :proxy) trust-env))
         (http-keys (%prepare-keys
                     (remove-from-plist keys :backend :preferred :slurp
                                             :filesystem :auth :cert
                                             :trust-env :default-params)
                     :slurp slurp :filesystem filesystem :backend backend
                     :cert cert
                     :default-params default-params))
         (http-keys (let ((k (copy-list http-keys)))
                      (when proxy (setf (getf k :proxy) proxy))
                      (when wire-auth (setf (getf k :auth) wire-auth))
                      k)))
    (with-default-codecs ()
      (apply #'http:request-async method url
             :backend backend
             (append (when clientp (list :client client))
                     http-keys)))))

(defun get-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :get url keys))

(defun head-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :head url keys))

(defun options-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :options url keys))

(defun post-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :post url keys))

(defun put-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :put url keys))

(defun patch-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :patch url keys))

(defun delete-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :delete url keys))

(defun stream (method url &rest keys &key &allow-other-keys)
  (apply #'request method url :want-stream t keys))

(defun stream-async (method url &rest keys &key &allow-other-keys)
  (apply #'request-async method url :want-stream t keys))

(defun json (method url data &rest keys &key &allow-other-keys)
  "Request with JSON body. Returns (values decoded response)."
  (let ((res (apply #'request method url :json data keys)))
    (values (ignore-errors (response-json res)) res)))

(defun sexp (method url data &rest keys &key &allow-other-keys)
  "Request with :DATA + :DATA-TYPE :sexp. Returns (values decoded response)."
  (let ((res (apply #'request method url :data data :data-type :sexp keys)))
    (values (ignore-errors (response-data res :sexp)) res)))

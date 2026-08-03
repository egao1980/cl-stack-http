(in-package #:cl-stack-http)

;;; requests/httpx-shaped facade over http: + path coerce + codecs.

(defun %prepare-keys (keys &key (slurp t) filesystem)
  "Coerce path-like :files / :content; drop cl-stack-http-only keys."
  (let* ((keys (copy-list keys))
         (files (getf keys :files))
         (content (getf keys :content)))
    (when files
      (setf (getf keys :files)
            (coerce-files files :slurp slurp :filesystem filesystem)))
    (when content
      (setf (getf keys :content)
            (coerce-upload content :slurp slurp :filesystem filesystem)))
    (remove-from-plist keys :slurp :filesystem :preferred)))

(defun request (method url &rest keys
                &key (backend nil backendp)
                  (preferred *preferred-backend*)
                  client
                  (slurp t) filesystem
                &allow-other-keys)
  "Sync request with path coerce + default codecs. Selects backend if unbound."
  (declare (ignore client))
  (let* ((backend (if backendp backend (ensure-http-backend preferred)))
         (*http-backend* backend)
         (http-keys (%prepare-keys
                     (remove-from-plist keys :backend :preferred :slurp :filesystem)
                     :slurp slurp :filesystem filesystem)))
    (with-default-codecs ()
      (apply #'http:request method url :backend backend http-keys))))

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
                        client
                        (slurp t) filesystem
                      &allow-other-keys)
  "Async request → Blackbird promise of HTTP-RESPONSE."
  (declare (ignore client))
  (let* ((backend (if backendp backend (ensure-http-backend preferred)))
         (*http-backend* backend)
         (http-keys (%prepare-keys
                     (remove-from-plist keys :backend :preferred :slurp :filesystem)
                     :slurp slurp :filesystem filesystem)))
    (with-default-codecs ()
      (apply #'http:request-async method url :backend backend http-keys))))

(defun get-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :get url keys))

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
  "Request with :DATA + :DATA-TYPE :json. Returns (values decoded response) when
   response looks like JSON; else (values nil response)."
  (let ((res (apply #'request method url :data data :data-type :json keys)))
    (values (ignore-errors (response-data res :json)) res)))

(defun sexp (method url data &rest keys &key &allow-other-keys)
  "Request with :DATA + :DATA-TYPE :sexp. Returns (values decoded response)."
  (let ((res (apply #'request method url :data data :data-type :sexp keys)))
    (values (ignore-errors (response-data res :sexp)) res)))

(in-package #:cl-stack-http)

;;; pathlib ↔ http-file bridge. Wire layer stays FS-free.

(defun effective-slurp (slurp &optional backend)
  "Resolve SLURP. :auto → T for dexador / unknown backend; NIL for stream-capable backends."
  (cond
    ((eq slurp :auto)
     (let ((name (and backend (http-backend-p backend) (backend-name backend))))
       (cond
         ((null name) t)                      ; safe default (memory FS / no backend)
         ((string-equal name "dexador") t)    ; dexador cannot write Gray streams
         (t nil))))
    (t slurp)))

(defun path-http-file (designator &key content-type field-name (slurp :auto) filesystem backend)
  "Build an HTTP-FILE from a pathlib designator (path / pathname / string).

   SLURP T — read-bytes into memory.
   SLURP NIL — open a binary input stream (caller/backend owns lifetime).
   SLURP :auto (default) — T on dexador, NIL otherwise."
  (let* ((path:*filesystem* (or filesystem path:*filesystem*))
         (p (path:ensure-path designator))
         (filename (path:name p))
         (ct (or content-type (guess-content-type filename)))
         (len (ignore-errors (path:file-size p)))
         (slurp (effective-slurp slurp backend)))
    (if slurp
        (let ((bytes (path:read-bytes p)))
          (make-http-file bytes
                          :filename filename
                          :content-type ct
                          :content-length (or len (length bytes))
                          :field-name field-name))
        (let ((stream (open (path:path-pathname p)
                            :direction :input
                            :element-type '(unsigned-byte 8)
                            :if-does-not-exist :error)))
          (make-http-file stream
                          :filename filename
                          :content-type ct
                          :content-length len
                          :field-name field-name)))))

(defun %httpx-file-tuple-p (x)
  "httpx files value: (filename content [content-type [headers]]).
   Proper lists only — alist pairs (name . path) must not match."
  (and (consp x)
       (not (http-file-p x))
       (alexandria:proper-list-p x)
       (<= 2 (length x) 4)
       (or (stringp (first x)) (null (first x)))
       (not (keywordp (first x)))
       ;; content is 2nd element — not a keyword (rejects plists)
       (not (keywordp (second x)))))

(defun coerce-upload (x &key field-name content-type (slurp :auto) filesystem backend filename)
  "Coerce X to HTTP-FILE when it looks like a path or httpx file tuple."
  (cond
    ((http-file-p x) x)
    ((%httpx-file-tuple-p x)
     (destructuring-bind (fname content &optional ct &rest more) x
       (declare (ignore more))
       (coerce-upload content
                      :field-name field-name
                      :content-type (or content-type ct)
                      :slurp slurp
                      :filesystem filesystem
                      :backend backend
                      :filename fname)))
    ((or (typep x 'path:path) (pathnamep x)
         (and (stringp x)
              (or (and (>= (length x) 1) (char= (char x 0) #\/))
                  (and (>= (length x) 2) (char= (char x 1) #\:))
                  (uiop:file-exists-p x))))
     (let ((file (path-http-file x :field-name field-name
                                   :content-type content-type
                                   :slurp slurp
                                   :filesystem filesystem
                                   :backend backend)))
       (when filename
         (setf (http-file-filename file) filename))
       file))
    ((or (streamp x) (typep x '(vector (unsigned-byte 8))) (stringp x))
     (make-http-file x
                     :filename filename
                     :content-type (or content-type
                                       (when filename (guess-content-type filename))
                                       "application/octet-stream")
                     :field-name field-name))
    (t x)))

(defun coerce-files (files &key (slurp :auto) filesystem backend)
  "Map :files alist/list values through COERCE-UPLOAD (paths + httpx tuples)."
  (when files
    (mapcar (lambda (entry)
              (if (and (consp entry) (not (%httpx-file-tuple-p entry)))
                  (cons (car entry)
                        (coerce-upload (cdr entry)
                                       :field-name (car entry)
                                       :slurp slurp
                                       :filesystem filesystem
                                       :backend backend))
                  (coerce-upload entry :slurp slurp :filesystem filesystem
                                       :backend backend)))
            files)))

(defun %response-octets (response)
  (let ((body (response-body response)))
    (cond
      ((null body) #())
      ((typep body '(vector (unsigned-byte 8))) body)
      ((stringp body) (babel:string-to-octets body :encoding :utf-8))
      ((streamp body) (slurp-octets body))
      (t (slurp-octets (body-stream response))))))

(defun %with-download-write-policy (dest overwrite create-parents thunk)
  "Run THUNK under create-parents / overwrite policy for DEST."
  (flet ((do-write ()
           (if create-parents
               (path:with-auto-create-parents () (funcall thunk))
               (funcall thunk))))
    (cond
      ((not (path:exists-p dest))
       (do-write))
      (overwrite
       (path:with-auto-overwrite () (do-write)))
      (t
       (restart-case
           (error 'path:path-exists-error
                  :path dest
                  :filesystem (path:path-filesystem dest)
                  :message (format nil "refusing to overwrite ~A" dest))
         (overwrite ()
           :report "Overwrite existing file"
           (path:with-auto-overwrite () (do-write)))))))
  dest)

(defun %write-download (dest octets &key overwrite create-parents)
  "Write OCTETS to DEST path. Honours overwrite / create-parents policy."
  (%with-download-write-policy
   dest overwrite create-parents
   (lambda () (path:write-bytes dest octets))))

(defun %write-download-stream (dest input &key overwrite create-parents
                               (buffer-size *http-stream-buffer-size*))
  "Copy INPUT stream to DEST in BUFFER-SIZE chunks (O(buffer), not O(body))."
  (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8)))
        (first t))
    (%with-download-write-policy
     dest overwrite create-parents
     (lambda ()
       (loop for n = (read-sequence buf input)
             while (plusp n)
             do (let ((chunk (if (= n buffer-size)
                                 buf
                                 (subseq buf 0 n))))
                  (if first
                      (progn
                        (path:write-bytes dest chunk)
                        (setf first nil))
                      (path:write-bytes dest chunk :append t)))
             finally (when first
                       (path:write-bytes dest #()))))))
  dest)

(defun %persist-response-body (dest response &key overwrite create-parents)
  "Write RESPONSE body to DEST. Streams when body is a stream (no full slurp)."
  (let ((body (response-body response)))
    (unwind-protect
         (cond
           ((streamp body)
            (%write-download-stream dest body
                                    :overwrite overwrite
                                    :create-parents create-parents))
           (t
            (%write-download dest (%response-octets response)
                             :overwrite overwrite
                             :create-parents create-parents)))
      (ignore-errors (close-response response)))))

(defun %basename-from-url (url)
  "Last path segment of URL, or \"download\"."
  (let* ((u (quri:uri url))
         (p (or (quri:uri-path u) "/"))
         (slash (position #\/ p :from-end t))
         (name (if slash (subseq p (1+ slash)) p)))
    (if (and name (plusp (length name))) name "download")))

(defun %directory-download-dest-p (dest)
  "T when DEST is a directory pathname (trailing slash) or an existing dir."
  (let ((pn (path:path-pathname dest)))
    (or (path:directory-pathname-p dest)
        (uiop:directory-pathname-p pn)
        (ignore-errors (path:directory-p dest)))))

(defun resolve-download-path (dest response &key filename url)
  "Resolve final file path for DOWNLOAD.

   FILENAME — string → use it; T / :content-disposition → require CD name;
   NIL → if DEST is a directory, use Content-Disposition filename (RFC 6266),
   else DEST as-is. Falls back to URL basename when DEST is a directory and
   no CD name is present.

   When the chosen name has no extension, append one from the response
   Content-Type (trivial-mimes), e.g. application/json → `.json`."
  (let* ((dest (path:ensure-path dest))
         (cd (content-disposition-filename
              (response-header response "content-disposition")))
         (ct (response-header response "content-type"))
         (dirp (%directory-download-dest-p dest))
         (name (cond
                 ((stringp filename) filename)
                 ((or (eq filename t) (eq filename :content-disposition))
                  (or cd
                      (error 'http-protocol-error
                             :message "Content-Disposition filename missing")))
                 (dirp (or cd (and url (%basename-from-url url)) "download"))
                 (t nil)))
         (name (when name (ensure-filename-extension name ct))))
    (if name
        (path:join dest name)
        dest)))

(defun download (url path &rest keys
                 &key (backend nil backendp) client
                   (overwrite nil) filesystem
                   (create-parents t)
                   (filename nil)
                 &allow-other-keys)
  "GET URL and stream body to PATH (pathlib). Returns (values path response).

   Uses `:want-stream t` so peak Lisp memory stays O(buffer), not O(body).
   If PATH is a directory (trailing slash / existing dir), the file name comes
   from Content-Disposition (filename / filename*), else the URL basename.
   FILENAME string overrides; T / :content-disposition requires a CD name."
  (declare (ignore client))
  (let* ((trust-env (getf keys :trust-env t))
         (http-keys (remove-from-plist keys :overwrite :filesystem :create-parents
                                            :backend :preferred :trust-env :cert
                                            :slurp :default-params :raise-for-status
                                            :filename))
         (http-keys (if (and trust-env
                             (eq (getf http-keys :proxy :%missing) :%missing))
                        (list* :proxy (make-http-proxy-config :system t) http-keys)
                        http-keys))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (path:*filesystem* (or filesystem path:*filesystem*))
         (dest (path:ensure-path path))
         (response (apply #'http:get url :force-binary t :want-stream t
                          :backend backend http-keys))
         (final (resolve-download-path dest response
                                       :filename filename :url url)))
    (values (%persist-response-body final response
                                    :overwrite overwrite
                                    :create-parents create-parents)
            response)))

(defun %normalize-download-pair (entry)
  "Normalize ENTRY to (values url path). Accepts (url . path) or (url path)."
  (cond
    ((and (consp entry) (not (consp (cdr entry))))
     (values (car entry) (cdr entry)))
    ((and (consp entry) (alexandria:proper-list-p entry) (= 2 (length entry)))
     (values (first entry) (second entry)))
    (t (error 'http-protocol-error
              :message (format nil "download-many entry must be (url . path) or (url path), got ~S"
                               entry)))))

(defun download-many (pairs &rest keys)
  "Download PAIRS sequentially. Each entry is (url . path) or (url path).

   Same keyword args as DOWNLOAD (`:overwrite`, `:filesystem`, …).
   Returns a list of (path . response) in input order."
  (mapcar (lambda (entry)
            (multiple-value-bind (url path) (%normalize-download-pair entry)
              (multiple-value-bind (final response)
                  (apply #'download url path keys)
                (cons final response))))
          pairs))

(defun upload (path url &rest keys
               &key (method :post) (backend nil backendp) client
                 content-type field-name (slurp :auto) filesystem
                 (as :content)
               &allow-other-keys)
  "Upload PATH to URL. AS :content (raw body) or :files (multipart field)."
  (declare (ignore client))
  (let* ((trust-env (getf keys :trust-env t))
         (http-keys (remove-from-plist keys :method :backend :content-type
                                            :field-name :slurp :filesystem :as
                                            :preferred :trust-env :cert
                                            :default-params))
         (http-keys (if (and trust-env
                             (eq (getf http-keys :proxy :%missing) :%missing))
                        (list* :proxy (make-http-proxy-config :system t) http-keys)
                        http-keys))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (file (path-http-file path
                               :content-type content-type
                               :field-name field-name
                               :slurp slurp
                               :filesystem filesystem
                               :backend backend)))
    (ecase as
      (:content
       (apply #'http:request method url :content file
              :backend backend http-keys))
      (:files
       (apply #'http:request method url
              :files (list (cons (or field-name "file") file))
              :backend backend
              http-keys)))))

(defun download-async (url path &rest keys
                       &key (backend nil backendp) client
                         (overwrite nil) filesystem
                         (create-parents t)
                         (filename nil)
                       &allow-other-keys)
  "Async DOWNLOAD. Promise resolves to (PATH . RESPONSE).
   Same FILENAME / directory Content-Disposition rules as DOWNLOAD."
  (declare (ignore client))
  (let* ((trust-env (getf keys :trust-env t))
         (http-keys (remove-from-plist keys :overwrite :filesystem :create-parents
                                            :backend :preferred :trust-env :cert
                                            :slurp :default-params :raise-for-status
                                            :filename))
         (http-keys (if (and trust-env
                             (eq (getf http-keys :proxy :%missing) :%missing))
                        (list* :proxy (make-http-proxy-config :system t) http-keys)
                        http-keys))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (path:*filesystem* (or filesystem path:*filesystem*))
         (dest (path:ensure-path path)))
    (blackbird:attach
     (apply #'http:get-async url :force-binary t :want-stream t
            :backend backend http-keys)
     (lambda (response)
       (let ((final (resolve-download-path dest response
                                           :filename filename :url url)))
         (cons (%persist-response-body final response
                                       :overwrite overwrite
                                       :create-parents create-parents)
               response))))))

(defun upload-async (path url &rest keys
                     &key (method :post) (backend nil backendp) client
                       content-type field-name (slurp :auto) filesystem
                       (as :content)
                     &allow-other-keys)
  "Async UPLOAD → Blackbird promise of HTTP-RESPONSE."
  (declare (ignore client))
  (let* ((trust-env (getf keys :trust-env t))
         (http-keys (remove-from-plist keys :method :backend :content-type
                                            :field-name :slurp :filesystem :as
                                            :preferred :trust-env :cert
                                            :default-params))
         (http-keys (if (and trust-env
                             (eq (getf http-keys :proxy :%missing) :%missing))
                        (list* :proxy (make-http-proxy-config :system t) http-keys)
                        http-keys))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (file (path-http-file path
                               :content-type content-type
                               :field-name field-name
                               :slurp slurp
                               :filesystem filesystem
                               :backend backend)))
    (ecase as
      (:content
       (apply #'http:request-async method url :content file
              :backend backend http-keys))
      (:files
       (apply #'http:request-async method url
              :files (list (cons (or field-name "file") file))
              :backend backend
              http-keys)))))

(in-package #:cl-stack-http)

;;; pathlib ↔ http-file bridge. Wire layer stays FS-free.

(defun path-http-file (designator &key content-type field-name (slurp t) filesystem)
  "Build an HTTP-FILE from a pathlib designator (path / pathname / string).

   SLURP T (default) — read-bytes into memory (safe across event-loop threads).
   SLURP NIL — open a binary input stream (caller/backend owns lifetime)."
  (let* ((path:*filesystem* (or filesystem path:*filesystem*))
         (p (path:ensure-path designator))
         (filename (path:name p))
         (ct (or content-type (guess-content-type filename)))
         (len (ignore-errors (path:file-size p))))
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

(defun coerce-upload (x &key field-name content-type (slurp t) filesystem)
  "Coerce X to HTTP-FILE when it looks like a filesystem path; else return X."
  (cond
    ((http-file-p x) x)
    ((typep x 'path:path)
     (path-http-file x :field-name field-name :content-type content-type
                       :slurp slurp :filesystem filesystem))
    ((pathnamep x)
     (path-http-file x :field-name field-name :content-type content-type
                       :slurp slurp :filesystem filesystem))
    ;; Absolute / existing relative path strings only — avoid treating form values as paths.
    ((and (stringp x)
          (or (and (>= (length x) 1) (char= (char x 0) #\/))
              (and (>= (length x) 2) (char= (char x 1) #\:)) ; Windows drive
              (uiop:file-exists-p x)))
     (path-http-file x :field-name field-name :content-type content-type
                       :slurp slurp :filesystem filesystem))
    (t x)))

(defun coerce-files (files &key (slurp t) filesystem)
  "Map :files alist/list values through COERCE-UPLOAD."
  (when files
    (mapcar (lambda (entry)
              (if (consp entry)
                  (cons (car entry)
                        (coerce-upload (cdr entry)
                                       :field-name (car entry)
                                       :slurp slurp
                                       :filesystem filesystem))
                  (coerce-upload entry :slurp slurp :filesystem filesystem)))
            files)))

(defun %response-octets (response)
  (let ((body (response-body response)))
    (cond
      ((null body) #())
      ((typep body '(vector (unsigned-byte 8))) body)
      ((stringp body) (babel:string-to-octets body :encoding :utf-8))
      ((streamp body) (slurp-octets body))
      (t (slurp-octets (body-stream response))))))

(defun %write-download (dest octets &key overwrite create-parents)
  "Write OCTETS to DEST path. Honours overwrite / create-parents policy."
  (flet ((do-write ()
           (let ((thunk (lambda () (path:write-bytes dest octets))))
             (if create-parents
                 (path:with-auto-create-parents () (funcall thunk))
                 (funcall thunk)))))
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

(defun download (url path &rest keys
                 &key (backend nil backendp) client
                   (overwrite nil) filesystem
                   (create-parents t)
                 &allow-other-keys)
  "GET URL and write body octets to PATH (pathlib). Returns (values path response).

   OVERWRITE NIL → signals PATH-EXISTS-ERROR (restart OVERWRITE available).
   CREATE-PARENTS T → auto-create parent directories."
  (declare (ignore client))
  (let* ((http-keys (remove-from-plist keys :overwrite :filesystem :create-parents
                                            :backend))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (path:*filesystem* (or filesystem path:*filesystem*))
         (dest (path:ensure-path path))
         (response (apply #'http:get url :force-binary t http-keys)))
    (values (%write-download dest (%response-octets response)
                             :overwrite overwrite
                             :create-parents create-parents)
            response)))

(defun upload (path url &rest keys
               &key (method :post) (backend nil backendp) client
                 content-type field-name (slurp t) filesystem
                 (as :content)
               &allow-other-keys)
  "Upload PATH to URL. AS :content (raw body) or :files (multipart field).

   Returns HTTP-RESPONSE."
  (declare (ignore client))
  (let* ((http-keys (remove-from-plist keys :method :backend :content-type
                                            :field-name :slurp :filesystem :as))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (file (path-http-file path
                               :content-type content-type
                               :field-name field-name
                               :slurp slurp
                               :filesystem filesystem)))
    (ecase as
      (:content
       (apply #'http:request method url :content file http-keys))
      (:files
       (apply #'http:request method url
              :files (list (cons (or field-name "file") file))
              http-keys)))))

(defun download-async (url path &rest keys
                       &key (backend nil backendp) client
                         (overwrite nil) filesystem
                         (create-parents t)
                       &allow-other-keys)
  "Async DOWNLOAD. Promise resolves to (PATH . RESPONSE)."
  (declare (ignore client))
  (let* ((http-keys (remove-from-plist keys :overwrite :filesystem :create-parents
                                            :backend))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (path:*filesystem* (or filesystem path:*filesystem*))
         (dest (path:ensure-path path)))
    (blackbird:attach
     (apply #'http:get-async url :force-binary t http-keys)
     (lambda (response)
       (cons (%write-download dest (%response-octets response)
                              :overwrite overwrite
                              :create-parents create-parents)
             response)))))

(defun upload-async (path url &rest keys
                     &key (method :post) (backend nil backendp) client
                       content-type field-name (slurp t) filesystem
                       (as :content)
                     &allow-other-keys)
  "Async UPLOAD → Blackbird promise of HTTP-RESPONSE."
  (declare (ignore client))
  (let* ((http-keys (remove-from-plist keys :method :backend :content-type
                                            :field-name :slurp :filesystem :as))
         (backend (if backendp backend (ensure-http-backend)))
         (*http-backend* backend)
         (file (path-http-file path
                               :content-type content-type
                               :field-name field-name
                               :slurp slurp
                               :filesystem filesystem)))
    (ecase as
      (:content
       (apply #'http:request-async method url :content file http-keys))
      (:files
       (apply #'http:request-async method url
              :files (list (cons (or field-name "file") file))
              http-keys)))))

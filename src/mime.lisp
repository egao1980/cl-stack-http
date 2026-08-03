(in-package #:cl-stack-http)

(defun guess-content-type (filename-or-path &optional default)
  "Guess MIME type from filename / path name. Falls back to DEFAULT
   or application/octet-stream."
  (let* ((name (etypecase filename-or-path
                 (null nil)
                 (string filename-or-path)
                 (pathname (file-namestring filename-or-path))
                 (path:path (or (path:name filename-or-path)
                                (path:as-namestring filename-or-path)))))
         (mime (when (and name (plusp (length name)))
                 (ignore-errors (trivial-mimes:mime name)))))
    (or mime default "application/octet-stream")))

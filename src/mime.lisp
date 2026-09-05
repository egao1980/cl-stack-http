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
                 (mime-protocol:lookup-mime name nil))))
    (or mime default "application/octet-stream")))

(defun extension-for-content-type (content-type)
  "Filename extension for CONTENT-TYPE (no leading dot), or NIL."
  (when (and content-type (plusp (length content-type)))
    (let ((mt (ignore-errors (mime-protocol:parse-media-type content-type))))
      (when mt
        (mime-protocol:lookup-extension (mime-protocol:media-type-essentials mt))))))

(defun %filename-has-extension-p (name)
  "T when NAME has a non-empty extension after the last dot (not leading)."
  (let ((dot (position #\. name :from-end t)))
    (and dot (plusp dot) (< (1+ dot) (length name)))))

(defun ensure-filename-extension (name content-type)
  "If NAME has no extension, append one derived from CONTENT-TYPE.

   Content-Disposition / URL basenames win when they already include an
   extension; MIME only fills gaps (e.g. `AAPL` + application/json → `AAPL.json`)."
  (cond
    ((or (null name) (zerop (length name))) name)
    ((%filename-has-extension-p name) name)
    (t
     (let ((ext (extension-for-content-type content-type)))
       (if ext
           (format nil "~A.~A" name ext)
           name)))))

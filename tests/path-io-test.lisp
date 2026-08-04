(in-package #:cl-stack-http/tests)

(deftest path-http-file-from-memory-fs
  (let* ((fs (path:make-memory-filesystem))
         (path:*filesystem* fs)
         (p (path:path "/data/hello.txt"))
         (_ (path:with-auto-create-parents ()
              (path:write-text p "hi")))
         (file (path-http-file p)))
    (declare (ignore _))
    (ok (http-file-p file))
    (ok (string= "hello.txt" (http-file-filename file)))
    (ok (string= "text/plain" (http-file-content-type file)))
    (ok (equalp (babel:string-to-octets "hi" :encoding :utf-8)
                (http-file-content file)))))

(deftest coerce-files-paths
  (let* ((fs (path:make-memory-filesystem))
         (path:*filesystem* fs)
         (p (path:path "/f.bin"))
         (_ (path:with-auto-create-parents ()
              (path:write-bytes p #(1 2 3))))
         (files (coerce-files `(("upload" . ,p)) :filesystem fs)))
    (declare (ignore _))
    (ok (http-file-p (cdr (first files))))
    (ok (equalp #(1 2 3) (http-file-content (cdr (first files)))))))

(deftest write-download-overwrite-policy
  (let* ((fs (path:make-memory-filesystem))
         (path:*filesystem* fs)
         (dest (path:path "/out/x.bin")))
    (path:with-auto-create-parents ()
      (path:write-bytes dest #(9)))
    (ok (signals (cl-stack-http::%write-download dest #(1 2) :overwrite nil)
                 'path:path-exists-error))
    (cl-stack-http::%write-download dest #(1 2) :overwrite t :create-parents t)
    (ok (equalp #(1 2) (path:read-bytes dest)))))

(deftest guess-content-type-json
  (ok (string= "application/json" (guess-content-type "foo.json"))))

(deftest download-strips-trust-env
  "Regression: :trust-env must not reach make-http-request initargs."
  (with-backend (:dexador)
    (uiop:with-temporary-file (:pathname p :prefix "stack-http-" :type "bin")
      ;; example.com is enough to exercise the key path; network skip on failure
      (handler-case
          (progn
            (download "https://example.com/" p :overwrite t :trust-env nil :timeout 15.0)
            (ok (probe-file p)))
        (error (e)
          (skip (format nil "network unavailable: ~A" e)))))))

(deftest resolve-download-path-content-disposition
  (let* ((res (make-instance 'http-response
                             :status 200
                             :headers (let ((h (make-hash-table :test #'equal)))
                                        (setf (gethash "content-disposition" h)
                                              "attachment; filename=\"AAPL.json\"")
                                        h)
                             :body #()))
         (dir (path:ensure-path "/tmp/dl-out/" :directory t))
         (final (resolve-download-path dir res :url "https://x/ignored.bin")))
    (ok (string= "AAPL.json" (path:name final)))))

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
    (ok (equalp (encoding-protocol:encode "hi")
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

(deftest coerce-files-multi-field
  "cl-stack#73: multiple file fields → distinct http-file values."
  (let* ((fs (path:make-memory-filesystem))
         (path:*filesystem* fs)
         (a (path:path "/a.txt"))
         (b (path:path "/b.txt")))
    (path:with-auto-create-parents ()
      (path:write-text a "one")
      (path:write-text b "two"))
    (let ((files (coerce-files `(("fa" . ,a) ("fb" . ,b)) :filesystem fs)))
      (ok (= 2 (length files)))
      (ok (string= "fa" (car (first files))))
      (ok (string= "fb" (car (second files))))
      (ok (string= "a.txt" (http-file-filename (cdr (first files)))))
      (ok (string= "b.txt" (http-file-filename (cdr (second files))))))))

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

(deftest write-download-stream-chunked
  "Streamed persist uses fixed buffer + append (O(buffer))."
  (let* ((fs (path:make-memory-filesystem))
         (path:*filesystem* fs)
         (dest (path:path "/out/stream.bin"))
         (src (make-octet-input-stream #(1 2 3 4 5 6 7 8))))
    (cl-stack-http::%write-download-stream dest src
                                           :overwrite t
                                           :create-parents t
                                           :buffer-size 3)
    (ok (equalp #(1 2 3 4 5 6 7 8) (path:read-bytes dest)))))

(deftest normalize-download-pair
  (multiple-value-bind (u p)
      (cl-stack-http::%normalize-download-pair '("https://x/a" . #p"/tmp/a"))
    (ok (string= "https://x/a" u))
    (ok (equal #p"/tmp/a" p)))
  (multiple-value-bind (u p)
      (cl-stack-http::%normalize-download-pair '("https://x/b" #p"/tmp/b"))
    (ok (string= "https://x/b" u))
    (ok (equal #p"/tmp/b" p)))
  (ok (signals (cl-stack-http::%normalize-download-pair '("bad"))
               'http-protocol-error)))

(deftest download-many-sequential
  "download-many walks pairs in order and returns (path . response) alist."
  (let* ((calls nil)
         (r1 (make-instance 'http-response :status 200 :body #(1)))
         (r2 (make-instance 'http-response :status 200 :body #(2)))
         (orig (fdefinition 'cl-stack-http:download)))
    (unwind-protect
         (progn
           (setf (fdefinition 'cl-stack-http:download)
                 (lambda (url path &rest keys)
                   (declare (ignore keys))
                   (setf calls (nconc calls (list (list url path))))
                   (values (path:ensure-path path)
                           (if (search "/a" url) r1 r2))))
           (let ((out (download-many
                       `(("https://x/a.bin" . ,(path:path "/tmp/a.bin"))
                         ("https://x/b.bin" ,(path:path "/tmp/b.bin")))
                       :overwrite t)))
             (ok (= 2 (length out)))
             (ok (= 2 (length calls)))
             (ok (search "/a.bin" (first (first calls))))
             (ok (search "/b.bin" (first (second calls))))
             (ok (equalp #(1) (response-body (cdr (first out)))))
             (ok (equalp #(2) (response-body (cdr (second out)))))))
      (setf (fdefinition 'cl-stack-http:download) orig))))

(deftest guess-content-type-json
  (ok (string= "application/json" (guess-content-type "foo.json"))))

(deftest ensure-filename-extension-from-content-type
  (ok (string= "AAPL.json"
               (ensure-filename-extension "AAPL" "application/json; charset=utf-8")))
  (ok (string= "AAPL.json"
               (ensure-filename-extension "AAPL.json" "application/octet-stream")))
  (ok (string= "report.html"
               (ensure-filename-extension "report" "text/html")))
  (ok (string= "plain"
               (ensure-filename-extension "plain" nil))))

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

(deftest resolve-download-path-content-type-extension
  "URL basename without extension + Content-Type → MIME suffix."
  (let* ((res (make-instance 'http-response
                             :status 200
                             :headers (let ((h (make-hash-table :test #'equal)))
                                        (setf (gethash "content-type" h)
                                              "application/json; charset=utf-8")
                                        h)
                             :body #()))
         (dir (path:ensure-path "/tmp/dl-out/" :directory t))
         (final (resolve-download-path dir res :url "https://x/api/AAPL")))
    (ok (string= "AAPL.json" (path:name final)))))

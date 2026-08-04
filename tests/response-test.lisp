(in-package #:cl-stack-http/tests)

(defun %fake-response (&key (status 200) body headers)
  (make-instance 'http-response
                 :status status
                 :body body
                 :headers (let ((ht (make-hash-table :test #'equal)))
                            (dolist (p headers ht)
                              (setf (gethash (string-downcase (car p)) ht)
                                    (cdr p))))))

(deftest response-text-charset
  (let ((res (%fake-response
              :body (babel:string-to-octets "café" :encoding :utf-8)
              :headers '(("content-type" . "text/plain; charset=utf-8")))))
    (ok (string= "café" (response-text res)))
    (ok (eq :utf-8 (detect-encoding res)))))

(deftest response-json-helper
  (install-default-codecs)
  (let* ((wire (babel:string-to-octets "{\"a\":1}" :encoding :utf-8))
         (res (%fake-response
               :body wire
               :headers '(("content-type" . "application/json")))))
    (ok (= 1 (gethash "a" (response-json res))))
    (ok (response-ok-p res))))

(deftest iter-lines-splits
  (let* ((text (format nil "a~C~Cb~Cc" #\Return #\Newline #\Newline))
         (res (%fake-response :body (babel:string-to-octets text :encoding :utf-8)
                              :headers '(("content-type" . "text/plain; charset=utf-8")))))
    (ok (equal '("a" "b" "c") (iter-lines res)))))

(deftest iter-bytes-chunks
  (let* ((octets (babel:string-to-octets "abcdefgh" :encoding :utf-8))
         (res (%fake-response :body octets))
         (chunks (iter-bytes res :chunk-size 3)))
    (ok (= 3 (length chunks)))
    (ok (equalp octets (apply #'concatenate '(vector (unsigned-byte 8)) chunks)))))

(deftest raise-for-status-exported
  (ok (fboundp 'raise-for-status))
  (ok (signals (raise-for-status (%fake-response :status 404 :body #()))
               'http-client-error)))

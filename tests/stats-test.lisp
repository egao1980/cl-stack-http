;;;; Response timing / download byte stats.

(in-package #:cl-stack-http/tests)

(defclass %stats-probe-backend (http-backend)
  ()
  (:default-initargs :name "stats-probe"))

(defmethod send ((backend %stats-probe-backend) client request &key)
  (declare (ignore backend client))
  (make-instance 'http-response
                 :status 200
                 :headers (make-hash-table :test #'equal)
                 :body (babel:string-to-octets "hello-stats" :encoding :utf-8)
                 :url (http-request-url request)))

(deftest response-elapsed-and-bytes-eager
  (testing "send annotates elapsed and body byte count"
    (let* ((backend (make-instance '%stats-probe-backend))
           (client (make-instance 'http-client :backend backend))
           (req (make-http-request :url "https://example.test/stats"))
           (resp (send backend client req))
           (nbytes (length (babel:string-to-octets "hello-stats" :encoding :utf-8))))
      (ok (numberp (response-elapsed resp)))
      (ok (>= (response-elapsed resp) 0d0))
      (ok (= (response-bytes-downloaded resp) nbytes)))))

(deftest map-response-bytes-counts-and-resets
  (testing "streaming recounts response-bytes-downloaded from zero"
    (let* ((payload (babel:string-to-octets "abcdef" :encoding :utf-8))
           (resp (make-instance 'http-response
                                :status 200
                                :headers (make-hash-table :test #'equal)
                                :body payload
                                :bytes-downloaded 999))
           (total 0))
      (map-response-bytes resp
                          (lambda (chunk)
                            (incf total (length chunk)))
                          :chunk-size 2)
      (ok (= total (length payload)))
      (ok (= (response-bytes-downloaded resp) (length payload))))))

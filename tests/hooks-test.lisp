(in-package #:cl-stack-http/tests)

(defclass %hook-client (http-client)
  ((prepares :initform 0 :accessor %hook-prepares)
   (handles :initform 0 :accessor %hook-handles)
   (last-status :initform nil :accessor %hook-last-status)))

(defmethod prepare-request ((client %hook-client) request)
  (incf (%hook-prepares client))
  ;; Add a marker header before URL finalize.
  (setf (http-request-headers request)
        (acons "x-hook" "1" (http-request-headers request)))
  request)

(defmethod handle-response ((client %hook-client) request response)
  (declare (ignore request))
  (incf (%hook-handles client))
  (setf (%hook-last-status client) (response-status response))
  response)

(defclass %hook-probe-backend (http-backend)
  ((last-headers :initform nil :accessor %hook-last-headers))
  (:default-initargs :name "hook-probe"))

(defmethod send ((backend %hook-probe-backend) client request &key)
  (declare (ignore client))
  (setf (%hook-last-headers backend) (http-request-headers request))
  (make-instance 'http-response
                 :status 200
                 :headers (make-hash-table :test #'equal)
                 :body #()
                 :url (http-request-url request)))

(deftest prepare-and-handle-request-hooks
  (let* ((backend (make-instance '%hook-probe-backend))
         (client (make-instance '%hook-client :backend backend))
         (req (make-http-request :url "http://ex.com/x" :params '(("q" . "1"))))
         (res (send backend client req)))
    (ok (= 1 (%hook-prepares client)))
    (ok (= 1 (%hook-handles client)))
    (ok (= 200 (%hook-last-status client)))
    (ok (= 200 (response-status res)))
    (ok (equal "1" (cdr (assoc "x-hook" (%hook-last-headers backend)
                               :test #'string-equal))))
    ;; params finalized after prepare-request
    (ok (search "q=1" (response-url res) :test #'char-equal))))

(deftest make-session-client-class
  (let ((s (make-session :preferred :dexador
                         :client-class '%hook-client
                         :trust-env nil
                         :base-url "https://example.com/")))
    (ok (typep (session-client s) '%hook-client))
    (ok (string= "https://example.com/"
                 (http-client-base-url (session-client s))))))

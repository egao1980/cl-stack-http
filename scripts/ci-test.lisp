;;;; Phase 2: fresh image, configure ASDF from cl-repo install, run tests.

(setf *debugger-hook*
      (lambda (c h)
        (declare (ignore h))
        (format *error-output* "~&UNHANDLED: ~A~%" c)
        (uiop:quit 1)))

(setf asdf:*compile-file-failure-behaviour* :warn)

(defun call-with-ci-muffles (fn)
  #+sbcl
  (handler-bind ((sb-ext:defconstant-uneql
                  (lambda (c)
                    (declare (ignore c))
                    (let ((r (find-restart 'continue)))
                      (when r (invoke-restart r))))))
    (funcall fn))
  #-sbcl
  (funcall fn))

(call-with-ci-muffles (lambda () (asdf:load-system "cl-repository-client")))

(cl-repository-client/asdf-integration:configure-asdf-source-registry)
(cl-repository-client/asdf-integration:load-system-init-files)

(defun ci-assert-http-protocol-api ()
  (asdf:load-system "http-protocol")
  (format t "~&; ci: http-protocol from ~a (version ~a)~%"
          (asdf:system-source-directory (asdf:find-system "http-protocol"))
          (asdf:component-version (asdf:find-system "http-protocol")))
  (unless (and (find-package :http-protocol)
               (fboundp (find-symbol "PREPARE-REQUEST-BODY" :http-protocol))
               (fboundp (find-symbol "RESPONSE-DATA" :http-protocol))
               (macro-function (find-symbol "WITH-DATA-DESERIALIZER" :http-protocol)))
    (error "http-protocol missing 0.2.1 API — need OCI http-protocol:0.2.1+")))

(call-with-ci-muffles
 (lambda ()
   (dolist (n '("rove" "alexandria" "babel" "trivial-mimes"
                "blackbird" "cl-cookie" "dexador" "chipz" "salza2"
                "cl-unicode" "bordeaux-threads" "trivial-gray-streams"
                "ironclad" "quri" "com.inuoe.jzon" "closer-mop"
                "float-features" "json-protocol" "json-backend-jzon"))
     (unless (asdf:find-system n nil)
       (format t "~&; ci: ql fallback ~a~%" n)
       (ql:quickload n :silent t)))
   (ci-assert-http-protocol-api)
   (asdf:load-system "cl-stack-pathlib")
   (asdf:load-system "http-backend-dexador")
   (asdf:load-system "cl-stack-http")
   (asdf:test-system "cl-stack-http")))

(uiop:quit 0)

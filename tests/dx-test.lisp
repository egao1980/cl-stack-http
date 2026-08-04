(in-package #:cl-stack-http/tests)

(deftest json-kwarg-prepare
  (let ((keys (cl-stack-http::%apply-json-kwarg
               (list :json '(("a" . 1)) :headers '(("x" . "y"))))))
    (ok (equal '(("a" . 1)) (getf keys :data)))
    (ok (eq :json (getf keys :data-type)))
    (ok (eq :missing (getf keys :json :missing)))))

(deftest json-kwarg-conflict
  (ok (signals (cl-stack-http::%apply-json-kwarg
                (list :json 1 :data 2))
               'http-protocol-error)))

(deftest files-tuple-coerce
  (let* ((octets (babel:string-to-octets "hi" :encoding :utf-8))
         (file (coerce-upload `("note.txt" ,octets "text/plain")
                              :field-name "f"
                              :slurp t)))
    (ok (http-file-p file))
    (ok (string= "note.txt" (http-file-filename file)))
    (ok (string= "text/plain" (http-file-content-type file)))
    (ok (equalp octets (http-file-content file)))))

(deftest effective-slurp-auto-dexador
  (let ((b (ensure-http-backend :dexador)))
    (ok (eq t (effective-slurp :auto b)))
    (ok (eq t (effective-slurp :auto nil)))
    (ok (eq t (effective-slurp t nil)))
    (ok (eq nil (effective-slurp nil nil)))))

(deftest session-default-timeout-and-params
  (let ((s (make-session :preferred :dexador
                         :params '(("q" . "1"))
                         :trust-env nil)))
    (ok (http-session-p s))
    (ok (equal '(("q" . "1")) (session-params s)))
    (ok (numberp (http-client-timeout (session-client s))))
    (close-session s)))

(deftest merge-params-extra-wins
  (ok (equal '(("a" . "2") ("b" . "1"))
             (cl-stack-http::%merge-params
              '(("a" . "1") ("b" . "1"))
              '(("a" . "2"))))))

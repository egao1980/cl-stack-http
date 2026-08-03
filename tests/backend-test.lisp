(in-package #:cl-stack-http/tests)

(deftest ensure-dexador-backend
  (let ((b (ensure-http-backend :dexador)))
    (ok (http-backend-p b))
    (ok (string-equal "dexador" (backend-name b)))))

(deftest with-backend-binds-star
  (with-backend (:dexador)
    (ok (http-backend-p *http-backend*))
    (ok (eq :dexador *preferred-backend*))))

(deftest auto-backend-selects-something
  (let ((b (ensure-http-backend :auto)))
    (ok (http-backend-p b))))

(in-package #:cl-stack-http/tests)

(deftest make-session-with-dexador
  (let ((s (make-session :preferred :dexador :base-url "https://example.com/")))
    (ok (http-session-p s))
    (ok (http-backend-p (session-backend s)))
    (ok (http-client-p (session-client s)))
    (ok (string= "https://example.com/"
                 (http-client-base-url (session-client s))))))

(deftest with-session-binds-client
  (with-session (s :preferred :dexador)
    (ok (eq s (find s (list s)))) ; session object identity
    (ok (http-backend-p *http-backend*))
    (ok (http-client-p *http-client*))
    (ok (functionp *json-encoder*))))

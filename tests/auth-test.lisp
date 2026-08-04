(in-package #:cl-stack-http/tests)

(deftest digest-auth-constructor
  (ok (digest-auth-p (digest-auth "u" "p")))
  (ok (equal '("u" "p") (cdr (digest-auth "u" "p")))))

(deftest digest-authorization-header
  (let* ((chal (cl-stack-http::%parse-auth-params
                "Digest realm=\"x\", nonce=\"abc\", qop=\"auth\", opaque=\"o\""))
         (hdr (compute-digest-authorization "user" "pass" :get "/x" chal
                                            :nc "00000001" :cnonce "deadbeef")))
    (ok (search "Digest " hdr :test #'char=))
    (ok (search "username=\"user\"" hdr))
    (ok (search "response=" hdr))
    (ok (search "qop=auth" hdr))))

(deftest netrc-parse-roundtrip
  (uiop:with-temporary-file (:pathname p :stream s)
    (write-line "machine example.com login alice password s3cret" s)
    (write-line "default login bob password x" s)
    (finish-output s)
    (let ((entries (parse-netrc p)))
      (ok (= 2 (length entries)))
      (ok (string= "alice" (getf (first entries) :login)))
      (ok (equal '(:basic "alice" "s3cret")
                 (netrc-auth-for-url "https://example.com/a" entries)))
      (ok (equal '(:basic "bob" "x")
                 (netrc-auth-for-url "https://other.test/" entries))))))

(deftest normalize-cert-shapes
  (multiple-value-bind (c k) (normalize-cert #p"/c.pem")
    (ok (equal #p"/c.pem" c))
    (ok (null k)))
  (multiple-value-bind (c k) (normalize-cert '(#p"/c.pem" #p"/k.pem"))
    (ok (equal #p"/c.pem" c))
    (ok (equal #p"/k.pem" k))))

(deftest auth-protocol-wire-passthrough
  (ok (null (auth-object-p '(:basic "u" "p"))))
  (ok (equal '(:bearer "t") (prepare-auth '(:bearer "t") nil)))
  (ok (auth-object-p (digest-auth "u" "p")))
  (ok (null (prepare-auth (digest-auth "u" "p") nil))))

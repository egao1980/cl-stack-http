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

;;; RFC 7616 §3.9.1 and RFC 2617 §3.5 vectors.

(defun %digest-response (header)
  (getf (cl-stack-http::%parse-auth-params header) :response))

(defparameter *rfc7616-challenge*
  (list :realm "http-auth@example.org"
        :qop "auth, auth-int"
        :nonce "7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v"
        :opaque "FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS"))

(deftest-parametrize digest-rfc7616-vectors
    ((algorithm expected)
     ("MD5" "8ca523f5e9506fed4657c9700eebdbec")
     ("SHA-256" "753927fa0e85d155564e2e272a28d1802ca10daf4496794697cf8db5856cb6c1"))
  (let ((hdr (compute-digest-authorization
              "Mufasa" "Circle of Life" :get "/dir/index.html"
              (list* :algorithm algorithm *rfc7616-challenge*)
              :nc "00000001" :cnonce "f2/wE4q74E6zIJEtWaHKaf5wv/H5QzzpXusqGemxURZJ")))
    (ok (string= expected (%digest-response hdr)))
    (ok (search (format nil "algorithm=~A" algorithm) hdr))
    (ok (search "qop=auth," hdr))))

(deftest digest-rfc2617-vector
  (let ((hdr (compute-digest-authorization
              "Mufasa" "Circle Of Life" :get "/dir/index.html"
              (list :realm "testrealm@host.com" :qop "auth,auth-int"
                    :nonce "dcd98b7102dd2f0e8b11d0f600bfb0c093"
                    :opaque "5ccc069c403ebaf9f0171e9517f40e41")
              :nc "00000001" :cnonce "0a4f113b")))
    (ok (string= "6629fae49393a05397450978507c4ef1" (%digest-response hdr)))
    (ng (search "algorithm=" hdr))))

(deftest digest-sess-variant
  (let* ((chal (list :realm "r" :nonce "n" :qop "auth" :algorithm "SHA-256-sess"))
         (h (lambda (s) (cl-stack-http::%digest-hex :sha256 s)))
         (ha1 (funcall h (format nil "~A:~A:~A"
                                 (funcall h "u:r:p") "n" "c")))
         (ha2 (funcall h "GET:/x"))
         (want (funcall h (format nil "~A:n:00000001:c:auth:~A" ha1 ha2)))
         (hdr (compute-digest-authorization "u" "p" :get "/x" chal :cnonce "c")))
    (ok (string= want (%digest-response hdr)))
    (ng (string= want (%digest-response
                       (compute-digest-authorization
                        "u" "p" :get "/x" (list :realm "r" :nonce "n" :qop "auth" :algorithm "SHA-256")
                        :cnonce "c"))))))

(deftest digest-unknown-algorithm-signals
  (ok (signals (compute-digest-authorization
                "u" "p" :get "/x" (list :realm "r" :nonce "n" :algorithm "ROT13")
                :cnonce "c")
               'http-protocol-error)))

(deftest digest-cnonce-is-csprng
  (let* ((state (make-random-state nil))
         (a (let ((*random-state* (make-random-state state)))
              (cl-stack-http::make-cnonce)))
         (b (let ((*random-state* (make-random-state state)))
              (cl-stack-http::make-cnonce))))
    (ok (= 32 (length a)))
    (ng (string= a b))))

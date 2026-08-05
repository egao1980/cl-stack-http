(in-package #:cl-stack-http/tests)

(deftest json-roundtrip
  (install-default-codecs)
  (let* ((data (alexandria:alist-hash-table
                '(("a" . 1) ("b" . "x") ("z" . :null)) :test #'equal))
         (wire (encode-json data))
         (back (decode-json wire)))
    (ok (search "\"a\"" wire))
    (ok (= 1 (gethash "a" back)))
    (ok (string= "x" (gethash "b" back)))
    (ok (eq :null (gethash "z" back)))))

(deftest json-via-http-protocol-serdes
  (install-default-codecs)
  (multiple-value-bind (octets ct len)
      (encode-http-data '(("ok" . t)) :json nil)
    (declare (ignore len))
    (ok (search "application/json" ct :test #'char-equal))
    (let ((val (decode-http-data octets :json ct)))
      (ok (gethash "ok" val)))))

(deftest sexp-roundtrip
  (let* ((data '(:foo 1 :bar ("a" "b")))
         (wire (encode-sexp data))
         (back (decode-sexp wire)))
    (ok (equal data back)))
  (multiple-value-bind (octets ct len)
      (encode-http-data '(1 2 3) :sexp nil)
    (declare (ignore len))
    (ok (search "x-lisp" ct :test #'char-equal))
    (ok (equal '(1 2 3) (decode-http-data octets :sexp ct)))))

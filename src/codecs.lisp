(in-package #:cl-stack-http)

;;; Default JSON (yason) + S-expression codecs for http-protocol serdes.

(defun encode-json (data)
  "Serialize DATA to a UTF-8 JSON string (yason)."
  (with-output-to-string (out)
    (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase))
      (cond
        ((hash-table-p data) (yason:encode data out))
        ((and (listp data) data (every #'consp data)
              (every (lambda (c) (or (stringp (car c)) (symbolp (car c)))) data))
         (yason:encode-alist data out))
        (t (yason:encode data out))))))

(defun decode-json (octets-or-string)
  "Parse JSON octets/string → Lisp (objects as hash-tables)."
  (let ((s (etypecase octets-or-string
             (string octets-or-string)
             ((vector (unsigned-byte 8))
              (babel:octets-to-string octets-or-string :encoding :utf-8)))))
    (yason:parse s :object-as :hash-table)))

(defun encode-sexp (data)
  "Serialize DATA with PRINT — readable, *READ-EVAL*-safe decode."
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-pretty* nil)
          (*print-circle* t)
          (*print-readably* t)
          (*package* (find-package :cl-user)))
      (prin1-to-string data))))

(defun decode-sexp (octets-or-string)
  "READ from UTF-8 octets/string with *READ-EVAL* NIL."
  (let ((s (etypecase octets-or-string
             (string octets-or-string)
             ((vector (unsigned-byte 8))
              (babel:octets-to-string octets-or-string :encoding :utf-8)))))
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package :cl-user)))
        (read-from-string s)))))

(defmethod encode-http-data (data (type (eql :sexp)) content-type)
  (let* ((octets (babel:string-to-octets (encode-sexp data) :encoding :utf-8))
         (ct (or content-type "application/x-lisp; charset=utf-8")))
    (values octets ct (length octets))))

(defun %body-octets (body)
  (cond
    ((null body) #())
    ((typep body '(vector (unsigned-byte 8))) body)
    ((stringp body) (babel:string-to-octets body :encoding :utf-8))
    ((streamp body) (slurp-octets body))
    ((http-file-p body) (%body-octets (http-file-content body)))
    (t (error 'http-protocol-error
              :message (format nil "cannot decode body of type ~S" (type-of body))))))

(defmethod decode-http-data (body (type (eql :sexp)) content-type &key headers)
  (declare (ignore content-type headers))
  (decode-sexp (%body-octets body)))

(defun install-default-codecs (&key (json t) (sexp t))
  "Install JSON/sexp hooks into http-protocol dynamic registries.
   Safe to call multiple times. Returns T."
  (when json
    (setf *json-encoder* #'encode-json
          *json-decoder* #'decode-json)
    (setf *data-serializers* (acons :json #'encode-json
                                    (remove :json *data-serializers* :key #'car))
          *data-deserializers* (acons :json #'decode-json
                                      (remove :json *data-deserializers* :key #'car))))
  (when sexp
    (setf *data-serializers* (acons :sexp #'encode-sexp
                                    (remove :sexp *data-serializers* :key #'car))
          *data-deserializers* (acons :sexp #'decode-sexp
                                      (remove :sexp *data-deserializers* :key #'car))))
  t)

(defmacro with-default-codecs ((&key (json t) (sexp t)) &body body)
  "Bind JSON/sexp codecs dynamically around BODY (does not mutate globals)."
  `(let ((*json-encoder* (if ,json #'encode-json *json-encoder*))
         (*json-decoder* (if ,json #'decode-json *json-decoder*))
         (*data-serializers*
          (append (when ,json (list (cons :json #'encode-json)))
                  (when ,sexp (list (cons :sexp #'encode-sexp)))
                  *data-serializers*))
         (*data-deserializers*
          (append (when ,json (list (cons :json #'decode-json)))
                  (when ,sexp (list (cons :sexp #'decode-sexp)))
                  *data-deserializers*)))
     ,@body))

;; Install on load so :data-type :json / :sexp work out of the box.
(install-default-codecs)

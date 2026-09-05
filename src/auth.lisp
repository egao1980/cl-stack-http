(in-package #:cl-stack-http)

;;; Digest challenge-response + client-cert / netrc DX.

(defun digest-auth (username password)
  "Build :auth value for Digest (RFC 7616). Handled by cl-stack-http retry, not protocol."
  (list :digest username password))

(defun digest-auth-p (auth)
  (and (consp auth) (eq (first auth) :digest)))

(defun normalize-cert (cert)
  "Normalize :cert → (values cert-path key-path).
   CERT: pathname/string | (cert . key) | (cert key) | NIL."
  (cond
    ((null cert) (values nil nil))
    ((or (pathnamep cert) (stringp cert)) (values cert nil))
    ((and (consp cert) (not (consp (cdr cert)))) ; (cert . key)
     (values (car cert) (cdr cert)))
    ((and (listp cert) (<= 1 (length cert) 2))
     (values (first cert) (second cert)))
    (t (error 'http-protocol-error
              :message (format nil "invalid :cert ~S" cert)))))

(defun %parse-auth-params (header)
  "Parse 'Digest k=\"v\", …' → plist of keyword → string."
  (let* ((s (string-trim '(#\Space #\Tab) header))
         (start (if (and (>= (length s) 6)
                         (string-equal "digest" s :end2 6))
                    6
                    0))
         (out nil)
         (i start)
         (n (length s)))
    (labels ((skip-ws ()
               (loop while (and (< i n) (member (char s i) '(#\Space #\Tab #\,)))
                     do (incf i)))
             (read-token ()
               (skip-ws)
               (let ((a i))
                 (loop while (and (< i n)
                                  (not (member (char s i) '(#\= #\Space #\Tab #\,))))
                       do (incf i))
                 (subseq s a i)))
             (read-value ()
               (skip-ws)
               (if (and (< i n) (char= (char s i) #\"))
                   (progn
                     (incf i)
                     (let ((a i))
                       (loop while (and (< i n) (char/= (char s i) #\")) do (incf i))
                       (prog1 (subseq s a i) (when (< i n) (incf i)))))
                   (read-token))))
      (loop
        (skip-ws)
        (when (>= i n) (return))
        (let ((key (read-token)))
          (when (zerop (length key)) (return))
          (skip-ws)
          (unless (and (< i n) (char= (char s i) #\=))
            (return))
          (incf i)
          (push (intern (string-upcase key) :keyword) out)
          (push (read-value) out))))
    (nreverse out)))

(defun %ensure-ironclad ()
  (unless (find-package :ironclad)
    (asdf:load-system "ironclad")))

(defun %md5-hex (string)
  (%ensure-ironclad)
  (let* ((digest-sequence (find-symbol "DIGEST-SEQUENCE" :ironclad))
         (bytes->hex (find-symbol "BYTE-ARRAY-TO-HEX-STRING" :ironclad))
         (octets (encoding-protocol:encode string))
         (digest (funcall digest-sequence :md5 octets)))
    (funcall bytes->hex digest)))

(defun compute-digest-authorization (username password method uri challenge
                                     &key (nc "00000001") cnonce)
  "Build Authorization header value for a Digest challenge plist."
  (let* ((realm (getf challenge :realm))
         (nonce (getf challenge :nonce))
         (opaque (getf challenge :opaque))
         (qop (getf challenge :qop))
         (algorithm (or (getf challenge :algorithm) "MD5"))
         (cnonce (or cnonce (format nil "~8,'0x" (random (expt 2 32)))))
         (ha1 (%md5-hex (format nil "~A:~A:~A" username realm password)))
         (ha2 (%md5-hex (format nil "~A:~A" (string-upcase (string method)) uri)))
         (response
           (if (and qop (search "auth" qop :test #'char-equal))
               (%md5-hex (format nil "~A:~A:~A:~A:~A:~A"
                                 ha1 nonce nc cnonce "auth" ha2))
               (%md5-hex (format nil "~A:~A:~A" ha1 nonce ha2))))
         (parts (list (format nil "username=~S" username)
                      (format nil "realm=~S" realm)
                      (format nil "nonce=~S" nonce)
                      (format nil "uri=~S" uri)
                      (format nil "response=~S" response))))
    (unless (string-equal algorithm "MD5")
      (warn "cl-stack-http: Digest algorithm ~A treated as MD5" algorithm))
    (when (and qop (search "auth" qop :test #'char-equal))
      (setf parts (append parts
                          (list "qop=auth"
                                (format nil "nc=~A" nc)
                                (format nil "cnonce=~S" cnonce)))))
    (when opaque
      (setf parts (append parts (list (format nil "opaque=~S" opaque)))))
    (format nil "Digest ~{~A~^, ~}" parts)))

(defun %www-authenticate (response)
  (response-header response "www-authenticate"))

(defun digest-challenge-p (header)
  (and header (search "digest" (string-downcase header) :test #'char=)))

(defun retry-with-digest (backend client request response auth)
  "If RESPONSE is 401 Digest, retry REQUEST with Authorization. Else NIL."
  (unless (and (http-response-p response)
               (= 401 (response-status response))
               (digest-auth-p auth)
               (digest-challenge-p (%www-authenticate response)))
    (return-from retry-with-digest nil))
  (let* ((user (second auth))
         (pass (third auth))
         (chal (%parse-auth-params (%www-authenticate response)))
         (url (http-request-url request))
         (uri (let ((u (quri:uri url)))
                (or (quri:uri-path u) "/")))
         (header (compute-digest-authorization
                  user pass (http-request-method request) uri chal))
         (headers (acons "authorization" header
                         (remove "authorization" (http-request-headers request)
                                 :key #'car :test #'string-equal)))
         (retry (make-http-request
                 :method (http-request-method request)
                 :url url
                 :headers headers
                 :content (http-request-content request)
                 :data (http-request-data request)
                 :data-type (http-request-data-type request)
                 :form-data (http-request-form-data request)
                 :files (http-request-files request)
                 :params (http-request-params request)
                 :timeout (http-request-timeout request)
                 :retry (http-request-retry request)
                 :max-redirects (http-request-max-redirects request)
                 :proxy (http-request-proxy request)
                 :cookies (http-request-cookies request)
                 :auth nil
                 :range (http-request-range request)
                 :accept-encoding (http-request-accept-encoding request)
                 :content-encoding (http-request-content-encoding request)
                 :decompress (http-request-decompress request)
                 :force-binary (http-request-force-binary request)
                 :want-stream (http-request-want-stream request)
                 :raise-for-status (http-request-raise-for-status request)
                 :extras (http-request-extras request))))
    (send backend client retry)))

;;; --- netrc -----------------------------------------------------------------

(defun parse-netrc (&optional (path (merge-pathnames ".netrc" (user-homedir-pathname))))
  "Parse netrc → list of plists (:machine :login :password [:default t])."
  (unless (probe-file path)
    (return-from parse-netrc nil))
  (let ((entries nil)
        (cur nil))
    (labels ((flush ()
               (when cur
                 (push cur entries)
                 (setf cur nil)))
             (put (k v) (setf cur (nconc cur (list k v)))))
      (with-open-file (in path :direction :input :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil nil)
                while line
                do (let* ((cut (or (position #\# line) (length line)))
                          (trimmed (string-trim '(#\Space #\Tab) (subseq line 0 cut))))
                     (when (plusp (length trimmed))
                       (let ((tokens (remove "" (uiop:split-string trimmed
                                                                   :separator '(#\Space #\Tab))
                                             :test #'string=)))
                         (loop with i = 0
                               while (< i (length tokens))
                               do (let ((tok (nth i tokens)))
                                    (incf i)
                                    (flet ((next ()
                                             (prog1 (nth i tokens) (incf i))))
                                      (cond
                                        ((string-equal tok "machine")
                                         (flush)
                                         (put :machine (next)))
                                        ((string-equal tok "default")
                                         (flush)
                                         (put :default t))
                                        ((string-equal tok "login")
                                         (put :login (next)))
                                        ((string-equal tok "password")
                                         (put :password (next)))
                                        ((string-equal tok "account")
                                         (next))
                                        (t nil))))))))))
        (flush)))
    (nreverse entries)))

(defun netrc-auth-for-url (url &optional netrc)
  "Return (:basic login password) from netrc for URL host, or NIL."
  (let* ((host (ignore-errors (quri:uri-host (quri:uri url))))
         (entries (or netrc (parse-netrc)))
         (match (or (find host entries
                          :key (lambda (e) (getf e :machine))
                          :test #'equalp)
                    (find-if (lambda (e) (getf e :default)) entries))))
    (when (and match (getf match :login) (getf match :password))
      (list :basic (getf match :login) (getf match :password)))))

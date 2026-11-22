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

(defparameter *digest-algorithms*
  '(("MD5" :md5 nil) ("MD5-SESS" :md5 t)
    ("SHA-256" :sha256 nil) ("SHA-256-SESS" :sha256 t)
    ("SHA-512-256" :sha512/256 nil) ("SHA-512-256-SESS" :sha512/256 t))
  "RFC 7616 §3.3 algorithm token → (crypto-protocol digest, session-variant-p).")

(defun %digest-algorithm (token)
  "Map a challenge `algorithm` (absent ⇒ MD5) to (values digest sess-p).
   Unknown tokens signal HTTP-PROTOCOL-ERROR rather than guessing."
  (let ((entry (assoc (string-upcase (or token "MD5")) *digest-algorithms*
                      :test #'string=)))
    (unless entry
      (error 'http-protocol-error
             :message (format nil "unsupported Digest algorithm ~S" token)))
    (values (second entry) (third entry))))

(defun %digest-hex (algorithm string)
  "Lower-case hex of ALGORITHM over the UTF-8 octets of STRING (RFC 7616 §3.4)."
  (string-downcase
   (encoding-protocol:encode
    (crypto-protocol:digest (encoding-protocol:encode string) :algorithm algorithm)
    :encoding :base16)))

(defun %digest-qop (qop)
  "Pick \"auth\" from the challenge's comma-separated QOP list, or NIL.
   auth-int alone is not supported (it needs the entity body hash)."
  (when qop
    (let ((options (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                           (uiop:split-string qop :separator ","))))
      (cond
        ((member "auth" options :test #'string-equal) "auth")
        ((member "auth-int" options :test #'string-equal)
         (error 'http-protocol-error
                :message "Digest qop=auth-int is not supported"))
        (t nil)))))

(defun make-cnonce ()
  "Fresh client nonce: 16 CSPRNG octets as hex (secrets-protocol)."
  (secrets-protocol:token-hex 16))

(defun compute-digest-authorization (username password method uri challenge
                                     &key (nc "00000001") cnonce)
  "Build the Authorization header value for a Digest challenge plist (RFC 7616).
   Honours the challenge `algorithm` (MD5, SHA-256, SHA-512-256 and their
   -sess variants) and qop=auth. CNONCE defaults to MAKE-CNONCE."
  (multiple-value-bind (algorithm sess-p) (%digest-algorithm (getf challenge :algorithm))
    (let* ((realm (getf challenge :realm))
           (nonce (getf challenge :nonce))
           (opaque (getf challenge :opaque))
           (qop (%digest-qop (getf challenge :qop)))
           (cnonce (or cnonce (make-cnonce)))
           (ha1 (let ((base (%digest-hex algorithm
                                         (format nil "~A:~A:~A" username realm password))))
                  (if sess-p
                      (%digest-hex algorithm (format nil "~A:~A:~A" base nonce cnonce))
                      base)))
           (ha2 (%digest-hex algorithm
                             (format nil "~A:~A" (string-upcase (string method)) uri)))
           (response
             (if qop
                 (%digest-hex algorithm (format nil "~A:~A:~A:~A:~A:~A"
                                                ha1 nonce nc cnonce qop ha2))
                 (%digest-hex algorithm (format nil "~A:~A:~A" ha1 nonce ha2))))
           (parts (list (format nil "username=~S" username)
                        (format nil "realm=~S" realm)
                        (format nil "nonce=~S" nonce)
                        (format nil "uri=~S" uri)
                        (format nil "response=~S" response))))
      (when (getf challenge :algorithm)
        (setf parts (append parts
                            (list (format nil "algorithm=~A" (getf challenge :algorithm))))))
      (when (or qop sess-p)
        (setf parts (append parts (list (format nil "cnonce=~S" cnonce)))))
      (when qop
        (setf parts (append parts (list (format nil "qop=~A" qop)
                                        (format nil "nc=~A" nc)))))
      (when opaque
        (setf parts (append parts (list (format nil "opaque=~S" opaque)))))
      (format nil "Digest ~{~A~^, ~}" parts))))

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

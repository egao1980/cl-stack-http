(in-package #:cl-stack-http)

;;; Response ergonomics (httpx r.text / r.content / r.json / iterators).

(defun response-content (response)
  "Raw response body as octet vector (slurps streams)."
  (%response-octets response))

(defun charset-from-content-type (content-type)
  "Parse charset= from a Content-Type header → encoding-protocol keyword or NIL."
  (when (and content-type (plusp (length content-type)))
    (let* ((s (string-downcase content-type))
           (pos (search "charset=" s :test #'char=)))
      (when pos
        (let* ((start (+ pos (length "charset=")))
               (end (or (position #\; s :start start) (length s)))
               (raw (string-trim '(#\Space #\Tab #\" #\') (subseq s start end))))
          (when (plusp (length raw))
            (cond
              ((or (string= raw "utf-8") (string= raw "utf8")) :utf-8)
              ((or (string= raw "iso-8859-1") (string= raw "latin-1")
                   (string= raw "latin1"))
               :iso-8859-1)
              ((string= raw "us-ascii") :ascii)
              (t (ignore-errors (intern (string-upcase raw) :keyword))))))))))

(defun detect-encoding (response &optional override)
  "Encoding for RESPONSE-TEXT. OVERRIDE wins, else Content-Type charset, else :utf-8."
  (or override
      (charset-from-content-type (response-header response "content-type"))
      :utf-8))

(defun response-text (response &key encoding (errorp nil))
  "Decode response body as text (httpx r.text). Default UTF-8 / charset=."
  (let* ((enc (detect-encoding response encoding))
         (octets (response-content response)))
    (handler-case (encoding-protocol:decode octets :encoding enc)
      (error (e)
        (if errorp
            (error e)
            ;; Lenient fallback (mojibake possible) — matches httpx soft decode.
            (encoding-protocol:decode octets :encoding :iso-8859-1))))))

(defun response-json (response &optional (type :json) &key content-type)
  "Decode response body as JSON (httpx r.json)."
  (response-data response type
                 :content-type (or content-type
                                   (response-header response "content-type"))))

(defun response-ok-p (response)
  "T when status is 2xx."
  (let ((s (response-status response)))
    (and (integerp s) (<= 200 s 299))))

(defun %response-input-stream (response)
  "Binary input stream for RESPONSE body (does not take ownership beyond body)."
  (let ((body (response-body response)))
    (cond
      ((streamp body) body)
      ((typep body '(vector (unsigned-byte 8)))
       (make-octet-input-stream body))
      ((stringp body)
       (make-octet-input-stream (encoding-protocol:encode body)))
      ((null body) (make-octet-input-stream #()))
      (t (body-stream response)))))

(defun map-response-bytes (response fn &key (chunk-size 8192))
  "Call FN with successive octet chunks (httpx iter_bytes).

   Resets then sets RESPONSE-BYTES-DOWNLOADED to octets read
   (httpx num_bytes_downloaded). Safe after eager annotate."
  (check-type chunk-size (integer 1 *))
  (setf (response-bytes-downloaded response) 0)
  (let* ((in (%response-input-stream response))
         (buf (make-array chunk-size :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf in)
          while (plusp n)
          do (incf (response-bytes-downloaded response) n)
             (funcall fn (if (= n chunk-size)
                             (copy-seq buf)
                             (subseq buf 0 n))))))

(defun iter-bytes (response &key (chunk-size 8192))
  "Return a list of octet chunks (eager). Prefer MAP-RESPONSE-BYTES for large bodies."
  (let ((chunks nil))
    (map-response-bytes response (lambda (c) (push c chunks)) :chunk-size chunk-size)
    (nreverse chunks)))

(defun map-response-lines (response fn &key encoding)
  "Call FN with successive text lines (httpx iter_lines). Universal newlines → #\Newline."
  (let* ((text (response-text response :encoding encoding))
         (start 0)
         (len (length text)))
    (loop while (< start len)
          do (let ((pos (or (position #\Newline text :start start)
                            (position #\Return text :start start)
                            len)))
               (let* ((line (subseq text start pos))
                      (next (cond
                              ((>= pos len) len)
                              ((and (< pos (1- len))
                                    (char= (char text pos) #\Return)
                                    (char= (char text (1+ pos)) #\Newline))
                               (+ pos 2))
                              (t (1+ pos)))))
                 ;; strip trailing CR from CRLF split on #\Return only
                 (when (and (plusp (length line))
                            (char= (char line (1- (length line))) #\Return))
                   (setf line (subseq line 0 (1- (length line)))))
                 (funcall fn line)
                 (setf start next))))))

(defun iter-lines (response &key encoding)
  "Return a list of text lines (eager)."
  (let ((lines nil))
    (map-response-lines response (lambda (l) (push l lines)) :encoding encoding)
    (nreverse lines)))

(defun close-response (response &key abort)
  "Close streamed body + release pooled connection (httpx stream exit)."
  (when response
    (let ((body (response-body response)))
      (when (streamp body)
        (ignore-errors (close body :abort abort))))
    (ignore-errors (release-response-connection response :abort abort)))
  response)

(defmacro with-stream ((response-var method url &rest keys) &body body)
  "httpx-style streaming request. RESPONSE-VAR bound; body stream closed on exit."
  `(let ((,response-var (stream ,method ,url ,@keys)))
     (unwind-protect (progn ,@body)
       (close-response ,response-var))))

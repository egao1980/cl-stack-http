(in-package #:cl-stack-http)

;;; CLOS hooks — requests hooks= / httpx event_hooks= analogue.
;;; Prefer specializing PREPARE-REQUEST / HANDLE-RESPONSE on client mixins
;;; (or :around on SEND). Auth stays on PREPARE-AUTH / HANDLE-AUTH-RESPONSE.
;;; Also records RESPONSE-ELAPSED + initial RESPONSE-BYTES-DOWNLOADED.

(defgeneric prepare-request (client request)
  (:documentation
   "Pre-send hook (httpx request event). Return REQUEST or a replacement.

    Runs inside SEND / SEND-ASYNC :around, before protocol :before
    (base-url join + params finalize) — safe to add :params / headers.")
  (:method ((client t) request)
    (declare (ignore client))
    request))

(defgeneric handle-response (client request response)
  (:documentation
   "Post-send hook (httpx response event). Return RESPONSE or a replacement.

    Runs after the backend returns a successful HTTP-RESPONSE.
    Does not see transport errors (those go to ERROR-CALLBACK / conditions).
    Auth challenge/retry stays on HANDLE-AUTH-RESPONSE.")
  (:method ((client t) request response)
    (declare (ignore client request))
    response))

(defun %elapsed-seconds (t0)
  (/ (float (- (get-internal-real-time) t0) 1d0)
     internal-time-units-per-second))

(defun %body-nbytes (body)
  (cond
    ((typep body '(vector (unsigned-byte 8))) (length body))
    ((stringp body) (length (babel:string-to-octets body :encoding :utf-8)))
    ((null body) 0)
    (t nil)))                       ; stream / unknown — leave counter alone

(defun %annotate-response-stats (response elapsed)
  "Set ELAPSED; for non-stream bodies set BYTES-DOWNLOADED from body size."
  (when (http-response-p response)
    (setf (response-elapsed response) elapsed)
    (let ((n (%body-nbytes (response-body response))))
      (when n
        (setf (response-bytes-downloaded response) n))))
  response)

(defmethod send :around ((backend http-backend) (client http-client) request &rest keys)
  (let* ((req (prepare-request client request))
         (t0 (get-internal-real-time))
         (res (apply #'call-next-method backend client req keys))
         (elapsed (%elapsed-seconds t0))
         (final (handle-response client req res)))
    (%annotate-response-stats final elapsed)))

(defmethod send-async :around ((backend http-backend) (client http-client) request
                               &rest keys &key callback &allow-other-keys)
  (let* ((req (prepare-request client request))
         (keys (copy-list keys))
         (cb callback)
         (t0 (get-internal-real-time)))
    (setf (getf keys :callback)
          (lambda (res)
            (let* ((elapsed (%elapsed-seconds t0))
                   (final (handle-response client req res))
                   (final (%annotate-response-stats final elapsed)))
              (when cb (funcall cb final))
              final)))
    (apply #'call-next-method backend client req keys)))

(in-package #:cl-stack-http)

;;; CLOS hooks — requests hooks= / httpx event_hooks= analogue.
;;; Prefer specializing PREPARE-REQUEST / HANDLE-RESPONSE on client mixins
;;; (or :around on SEND). Auth stays on PREPARE-AUTH / HANDLE-AUTH-RESPONSE.

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

(defmethod send :around ((backend http-backend) (client http-client) request &rest keys)
  (let* ((req (prepare-request client request))
         (res (apply #'call-next-method backend client req keys)))
    (handle-response client req res)))

(defmethod send-async :around ((backend http-backend) (client http-client) request
                               &rest keys &key callback &allow-other-keys)
  (let* ((req (prepare-request client request))
         (keys (copy-list keys))
         (cb callback))
    (setf (getf keys :callback)
          (lambda (res)
            (let ((final (handle-response client req res)))
              (when cb (funcall cb final))
              final)))
    (apply #'call-next-method backend client req keys)))

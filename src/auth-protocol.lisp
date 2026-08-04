(in-package #:cl-stack-http)

;;; CLOS auth protocol — requests AuthBase / httpx.Auth analogue.
;;; Wire values (string | :basic | :bearer) pass through; rich objects
;;; specialize AUTH-OBJECT-P / PREPARE-AUTH / HANDLE-AUTH-RESPONSE.
;;; OAuth2 lives in cl-stack-oauth2; JWT crypto in cl-stack-jwt.

(defvar *auth-in-flight* nil
  "Non-NIL while nested token/challenge work runs — implementations must not recurse.")

(defclass auth-object ()
  ()
  (:documentation "Marker mixin for managed auth (digest, oauth2, …)."))

(defgeneric auth-object-p (auth)
  (:documentation "T when AUTH participates in prepare + response handling.")
  (:method ((auth t)) nil)
  (:method ((auth auth-object)) t))

(defgeneric prepare-auth (auth request)
  (:documentation
   "Pre-send: ensure credentials. Return wire auth for http-protocol:
    NIL | string | (:basic u p) | (:bearer tok).
    NIL means no Authorization yet (e.g. Digest waits for 401).")
  (:method ((auth t) request)
    (declare (ignore request))
    auth))

(defgeneric handle-auth-response (auth backend client request response)
  (:documentation
   "Post-send: challenge / refresh / retry once. Return final http-response.")
  (:method ((auth t) backend client request response)
    (declare (ignore auth backend client request))
    response))

(defgeneric auth-retry-p (auth response)
  (:documentation "T if RESPONSE warrants a managed retry.")
  (:method ((auth t) response)
    (declare (ignore auth response))
    nil))

(defun copy-request-with-auth (request wire-auth &key (raise-for-status nil rfs-p))
  "Clone REQUEST with Authorization from WIRE-AUTH (replaces prior auth header)."
  (let* ((headers (remove "authorization" (http-request-headers request)
                          :key #'car :test #'string-equal))
         (headers (inject-auth-range-headers headers :auth wire-auth)))
    (make-http-request
     :method (http-request-method request)
     :url (http-request-url request)
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
     :auth wire-auth
     :range (http-request-range request)
     :accept-encoding (http-request-accept-encoding request)
     :content-encoding (http-request-content-encoding request)
     :decompress (http-request-decompress request)
     :force-binary (http-request-force-binary request)
     :want-stream (http-request-want-stream request)
     :raise-for-status (if rfs-p raise-for-status
                           (http-request-raise-for-status request))
     :extras (http-request-extras request))))

;;; --- Digest implements the protocol (list form kept for DX) ---------------

(defmethod auth-object-p ((auth cons))
  (digest-auth-p auth))

(defmethod prepare-auth ((auth cons) request)
  (if (digest-auth-p auth)
      nil
      (call-next-method)))

(defmethod auth-retry-p ((auth cons) response)
  (and (digest-auth-p auth)
       (http-response-p response)
       (= 401 (response-status response))
       (digest-challenge-p (%www-authenticate response))))

(defmethod handle-auth-response ((auth cons) backend client request response)
  (if (digest-auth-p auth)
      (or (retry-with-digest backend client request response auth) response)
      (call-next-method)))

(defpackage #:cl-stack-http
  (:nicknames #:stack-http)
  (:use #:cl #:http-protocol)
  (:shadow #:get #:delete #:stream)
  (:import-from #:alexandria #:remove-from-plist)
  (:local-nicknames (#:http #:http)
                    (#:path #:cl-stack-pathlib))
  (:export
   ;; backend selection
   #:*preferred-backend*
   #:ensure-http-backend
   #:with-backend
   #:load-http-backend
   ;; codecs
   #:install-default-codecs
   #:with-default-codecs
   #:encode-json
   #:decode-json
   #:encode-sexp
   #:decode-sexp
   ;; mime
   #:guess-content-type
   ;; path / FS
   #:path-http-file
   #:coerce-upload
   #:coerce-files
   #:effective-slurp
   #:download
   #:upload
   #:download-async
   #:upload-async
   ;; response DX
   #:response-content
   #:response-text
   #:response-json
   #:response-ok-p
   #:detect-encoding
   #:charset-from-content-type
   #:map-response-bytes
   #:iter-bytes
   #:map-response-lines
   #:iter-lines
   #:close-response
   #:with-stream
   #:raise-for-status
   ;; auth / env
   #:digest-auth
   #:digest-auth-p
   #:normalize-cert
   #:parse-netrc
   #:netrc-auth-for-url
   #:compute-digest-authorization
   ;; session
   #:http-session
   #:http-session-p
   #:session-backend
   #:session-client
   #:session-params
   #:session-cert
   #:session-trust-env-p
   #:make-session
   #:close-session
   #:with-session
   #:session-request
   #:session-get
   #:session-post
   #:session-put
   #:session-patch
   #:session-delete
   #:session-head
   #:session-options
   #:session-stream
   #:session-request-async
   #:session-get-async
   #:session-post-async
   #:session-put-async
   #:session-patch-async
   #:session-delete-async
   #:session-head-async
   #:session-options-async
   #:session-stream-async
   ;; facade (requests-shaped)
   #:request
   #:get
   #:post
   #:put
   #:patch
   #:delete
   #:head
   #:options
   #:request-async
   #:get-async
   #:post-async
   #:put-async
   #:patch-async
   #:delete-async
   #:head-async
   #:options-async
   #:json
   #:sexp
   #:stream
   #:stream-async))

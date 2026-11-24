(defpackage #:cl-stack-http
  (:nicknames #:stack-http)
  (:use #:cl #:http-protocol)
  (:shadow #:get #:delete #:stream)
  (:import-from #:alexandria #:remove-from-plist)
  (:local-nicknames (#:http #:http)
                    (#:path #:cl-stack-pathlib)
                    (#:stack-json #:json-protocol))
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
   #:extension-for-content-type
   #:ensure-filename-extension
   ;; path / FS
   #:path-http-file
   #:coerce-upload
   #:coerce-files
   #:effective-slurp
   #:download
   #:download-many
   #:upload
   #:download-async
   #:upload-async
   #:resolve-download-path
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
   ;; http-protocol re-exports (#26): one package for the requests-shaped
   ;; surface, so (stack-http:response-status r) reads. Same symbols as
   ;; http-protocol's, not new ones.
   ;;   response
   #:http-response
   #:http-response-p
   #:response-status
   #:response-headers
   #:response-header
   #:response-body
   #:response-data
   #:response-url
   #:response-http-version
   #:response-cookies
   #:response-history
   #:response-request
   #:response-elapsed
   #:response-bytes-downloaded
   #:response-trailers
   #:body-stream
   ;;   conditions
   #:http-error
   #:http-protocol-error
   #:http-connection-error
   #:http-timeout-error
   #:http-tls-error
   #:http-redirect-error
   #:http-canceled
   #:http-incomplete-response
   #:http-status-error
   #:http-status-error-status
   #:http-status-error-response
   #:http-client-error
   #:http-server-error
   ;;   transport policy (:timeout / :retry values), secrets, backend binding
   #:make-http-timeout
   #:make-http-retry
   #:make-secret
   #:reveal
   #:*http-backend*
   ;; auth / env
   #:digest-auth
   #:digest-auth-p
   #:normalize-cert
   #:parse-netrc
   #:netrc-auth-for-url
   #:compute-digest-authorization
   ;; CLOS auth protocol (OAuth2 → cl-stack-oauth2; JWT crypto → cl-stack-jwt)
   #:*auth-in-flight*
   #:auth-object
   #:auth-object-p
   #:prepare-auth
   #:handle-auth-response
   #:auth-retry-p
   #:copy-request-with-auth
   ;; CLOS request/response hooks (httpx event_hooks shape)
   #:prepare-request
   #:handle-response
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
   #:call-with-session
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

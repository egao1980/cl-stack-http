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
   #:download
   #:upload
   #:download-async
   #:upload-async
   ;; session
   #:http-session
   #:http-session-p
   #:session-backend
   #:session-client
   #:make-session
   #:with-session
   #:session-request
   #:session-get
   #:session-post
   #:session-put
   #:session-patch
   #:session-delete
   #:session-head
   #:session-options
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
   #:json
   #:sexp
   #:stream
   #:stream-async))

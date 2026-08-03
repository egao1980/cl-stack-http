(defpackage #:cl-stack-http/tests
  (:use #:cl #:rove #:cl-stack-http #:http-protocol)
  (:shadowing-import-from #:cl-stack-http #:get #:delete #:stream)
  (:local-nicknames (#:path #:cl-stack-pathlib)
                    (#:http #:http)))

(in-package #:cl-stack-http/tests)

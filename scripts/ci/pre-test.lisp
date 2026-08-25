;;;; Guard the http-protocol 0.2.1+ API the facade needs.

(asdf:load-system "http-protocol")
(format t "~&; ci: http-protocol from ~a (version ~a)~%"
        (asdf:system-source-directory (asdf:find-system "http-protocol"))
        (asdf:component-version (asdf:find-system "http-protocol")))
(unless (and (find-package :http-protocol)
             (fboundp (find-symbol "PREPARE-REQUEST-BODY" :http-protocol))
             (fboundp (find-symbol "RESPONSE-DATA" :http-protocol))
             (macro-function (find-symbol "WITH-DATA-DESERIALIZER" :http-protocol)))
  (error "http-protocol missing 0.2.1 API — need OCI http-protocol:0.2.1+"))

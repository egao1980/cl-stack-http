(defsystem "cl-stack-http"
  :version "0.1.12"
  :description "requests/httpx-like HTTP client over http-protocol (JSON/sexp, pathlib FS, selectable backends)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("http-protocol"
               "cl-stack-pathlib"
               "json-backend-jzon"
               "alexandria"
               "encoding-protocol"
               "crypto-protocol"
               "secrets-protocol"
               "crypto-backend-ironclad"
               "mime-protocol"
               "quri")
  :properties
  (:cl-repo
   (:ci (:with ("http-backend-dexador" "http-backend-async" "event-backend-libuv")
         :load-before-test ("http-protocol" "cl-stack-pathlib" "http-backend-dexador"
                            "http-backend-async" "event-backend-libuv"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend")
               (:file "codecs")
               (:file "mime")
               (:file "path-io")
               (:file "auth")
               (:file "auth-protocol")
               (:file "hooks")
               (:file "response")
               (:file "facade")
               (:file "session"))
  :in-order-to ((test-op (test-op "cl-stack-http/tests"))))

(defsystem "cl-stack-http/tests"
  :depends-on ("cl-stack-http" "http-backend-dexador" "usocket" "bordeaux-threads" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "facade-surface-test")
               (:file "codecs-test")
               (:file "path-io-test")
               (:file "backend-test")
               (:file "session-test")
               (:file "hooks-test")
               (:file "stats-test")
               (:file "response-test")
               (:file "auth-test")
               (:file "dx-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

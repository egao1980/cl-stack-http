(defsystem "cl-stack-http"
  :version "0.1.8"
  :description "requests/httpx-like HTTP client over http-protocol (JSON/sexp, pathlib FS, selectable backends)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("http-protocol"
               "cl-stack-pathlib"
               "json-backend-jzon"
               "alexandria"
               "babel"
               "trivial-mimes"
               "quri"
               "cl-base64")
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
  :depends-on ("cl-stack-http" "http-backend-dexador" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
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
             (symbol-call :rove :run c)))

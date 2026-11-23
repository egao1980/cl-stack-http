;;;; Evidence for cl-stack-http#26 (one package for the requests-shaped
;;;; surface) and cl-stack-http#27 (:auto order, :async needs no manual
;;;; event-loop wiring). Each test is the spec a fix must satisfy.

(in-package #:cl-stack-http/tests)

;;; #26 — response readers and conditions reachable as cl-stack-http:<name>

(defparameter *facade-protocol-names*
  '("RESPONSE-STATUS" "RESPONSE-HEADER" "RESPONSE-HEADERS" "RESPONSE-URL"
    "RESPONSE-HTTP-VERSION" "RESPONSE-HISTORY" "RESPONSE-COOKIES"
    "RESPONSE-ELAPSED" "RESPONSE-BYTES-DOWNLOADED"
    "HTTP-ERROR" "HTTP-STATUS-ERROR" "HTTP-STATUS-ERROR-STATUS"
    "HTTP-TIMEOUT-ERROR")
  "http-protocol names the QUICKSTART / cookbook call through cl-stack-http.")

(deftest response-readers-external-in-facade
  (testing "cl-stack-http re-exports the http-protocol response readers and conditions"
    (dolist (name *facade-protocol-names*)
      (multiple-value-bind (sym status) (find-symbol name :cl-stack-http)
        (ok (eq :external status)
            (format nil "cl-stack-http:~(~A~) is external (got ~S)" name status))
        (ok (eq sym (find-symbol name :http-protocol))
            (format nil "cl-stack-http:~(~A~) is the http-protocol symbol" name))))))

(deftest quickstart-http-snippet-reads
  (testing "QUICKSTART section 3 form reads with a single-colon prefix"
    (let ((form "(cl-stack-http:response-status (cl-stack-http:get \"https://httpbin.org/get\"))"))
      (ok (handler-case (progn (let ((*package* (find-package :cl-user)))
                                 (read-from-string form))
                               t)
            (reader-error (e)
              (format *error-output* "~&; read failed: ~A~%" e)
              nil))
          "reads without a READER-ERROR"))))

;;; #27 — backend selection with the async stack installed

(defun %async-stack-installed-p ()
  (and (asdf:find-system "http-backend-async" nil)
       (asdf:find-system "event-backend-libuv" nil)
       t))

(defparameter +async-skip-message+
  "http-backend-async / event-backend-libuv not installed: cl-repo install http-backend-async event-backend-libuv")

(defun %octets (string)
  (encoding-protocol:encode string :encoding :utf-8))

(defun %one-shot-server (body)
  "Listener on 127.0.0.1 that answers one request with 200 BODY, then closes.
   → (values port thread). Pair with %FINISH-ONE-SHOT."
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server))
         (thread
          (bt:make-thread
           (lambda ()
             (handler-case
                 (unwind-protect
                     (let* ((client (usocket:socket-accept server))
                            (stream (usocket:socket-stream client)))
                       (unwind-protect
                           (progn
                             (loop with tail = 0
                                   for b = (read-byte stream nil nil)
                                   while b
                                   do (setf tail (logand #xffffffff (logior (ash tail 8) b)))
                                   until (= tail #x0d0a0d0a))
                             (write-sequence
                              (%octets
                               (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/plain~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C~A"
                                       #\Return #\Newline #\Return #\Newline
                                       (length body) #\Return #\Newline
                                       #\Return #\Newline #\Return #\Newline body))
                              stream)
                             (finish-output stream))
                         (usocket:socket-close client)))
                   (usocket:socket-close server))
               (error () nil)))
           :name "cl-stack-http one-shot origin")))
    (values port thread)))

(defun %finish-one-shot (port thread)
  "Unblock THREAD's accept when the client never connected, then join it."
  (when (bt:thread-alive-p thread)
    (ignore-errors (usocket:socket-close (usocket:socket-connect "127.0.0.1" port))))
  (bt:join-thread thread))

(deftest auto-prefers-async-when-libuv-present
  #+win32 (skip "Windows :auto is winhttp-first by design")
  #-win32
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (testing "README / QUICKSTART: :auto is async/libuv first off Windows"
        (let ((b (ensure-http-backend :auto)))
          (ok (string-equal "async" (backend-name b))
              (format nil ":auto chose ~A, want async" (backend-name b)))))))

(deftest async-backend-needs-no-manual-event-maker
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (testing "(with-backend (:async) (get …)) works without binding *event-backend-maker*"
        ;; Loads http-backend-async + libuv; the maker symbol exists after this.
        (ensure-http-backend :async)
        (let ((maker (find-symbol "*EVENT-BACKEND-MAKER*" :http-backend-async))
              (eb (find-symbol "*EVENT-BACKEND*" :event-protocol))
              (el (find-symbol "*EVENT-LOOP*" :event-protocol)))
          ;; Fresh image state: nobody wired an event loop by hand.
          (progv (list maker eb el) (list nil nil nil)
            (multiple-value-bind (port thread) (%one-shot-server "async-ok")
              (let ((res (handler-case
                             (with-backend (:async)
                               (get (format nil "http://127.0.0.1:~D/" port)
                                    :timeout 5 :trust-env nil))
                           (error (e) e))))
                (%finish-one-shot port thread)
                (ok (typep res 'http-response)
                    (format nil "got ~A" res))
                (when (typep res 'http-response)
                  (ok (= 200 (response-status res)))
                  (ok (string= "async-ok" (response-text res)))))))))))

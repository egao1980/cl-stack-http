(in-package #:cl-stack-http/tests)

(deftest ensure-dexador-backend
  (let ((b (ensure-http-backend :dexador)))
    (ok (http-backend-p b))
    (ok (string-equal "dexador" (backend-name b)))))

(deftest with-backend-binds-star
  (with-backend (:dexador)
    (ok (http-backend-p *http-backend*))
    (ok (eq :dexador *preferred-backend*))))

(deftest auto-backend-selects-something
  (let ((b (ensure-http-backend :auto)))
    (ok (http-backend-p b))))

(defparameter *facade-extra-protocol-names*
  '("HTTP-RESPONSE" "HTTP-RESPONSE-P" "RESPONSE-BODY" "RESPONSE-DATA"
    "RESPONSE-REQUEST" "RESPONSE-TRAILERS" "BODY-STREAM"
    "HTTP-PROTOCOL-ERROR" "HTTP-CONNECTION-ERROR" "HTTP-TLS-ERROR"
    "HTTP-REDIRECT-ERROR" "HTTP-CANCELED" "HTTP-INCOMPLETE-RESPONSE"
    "HTTP-STATUS-ERROR-RESPONSE" "HTTP-CLIENT-ERROR" "HTTP-SERVER-ERROR"
    "MAKE-HTTP-TIMEOUT" "MAKE-HTTP-RETRY" "MAKE-SECRET" "REVEAL"
    "*HTTP-BACKEND*")
  "Re-exports beyond the QUICKSTART set: each must be http-protocol's own
   external symbol (a typo here would intern a fresh cl-stack-http symbol).")

(deftest facade-reexports-are-http-protocol-externals
  (dolist (name *facade-extra-protocol-names*)
    (multiple-value-bind (sym status) (find-symbol name :cl-stack-http)
      (multiple-value-bind (psym pstatus) (find-symbol name :http-protocol)
        (ok (and (eq :external status) (eq :external pstatus) (eq sym psym))
            (format nil "~A: cl-stack-http ~S, http-protocol ~S" name status pstatus))))))

(deftest facade-condition-handlers-catch-protocol-conditions
  (testing "handler-case on stack-http:http-status-error catches a raise-for-status"
    (let* ((res (make-instance 'http-response :status 404 :headers nil
                                              :body (make-array 0 :element-type '(unsigned-byte 8))
                                              :url "http://h.test/x"))
           (c (handler-case (progn (raise-for-status res) nil)
                (cl-stack-http:http-status-error (e) e))))
      (ok (typep c 'http-protocol:http-status-error))
      (ok (= 404 (cl-stack-http:http-status-error-status c))))))

(defmacro with-function-replaced ((name replacement) &body body)
  "Run BODY with the global function NAME replaced by REPLACEMENT."
  (let ((old (gensym "OLD")))
    `(let ((,old (fdefinition ',name)))
       (unwind-protect
           (progn (setf (fdefinition ',name) ,replacement) ,@body)
         (setf (fdefinition ',name) ,old)))))

(deftest auto-falls-back-to-dexador-without-event-backend
  #+win32 (skip "Windows :auto is winhttp-first by design")
  #-win32
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (with-function-replaced (cl-stack-http::%wire-async-event-context
                               (constantly nil))
        (testing ":auto for -ASYNC calls skips async when no event backend can be found"
          (ok (string-equal "dexador"
                            (backend-name (cl-stack-http::%ensure-http-backend :auto t)))))
        (testing "an explicit :async is still made (the caller may bind its own loop)"
          (ok (string-equal "async" (backend-name (ensure-http-backend :async))))))))

(deftest async-wiring-keeps-caller-event-backend-and-maker
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (progn
        (ensure-http-backend :async)
        (let* ((maker (find-symbol "*EVENT-BACKEND-MAKER*" :http-backend-async))
               (eb (find-symbol "*EVENT-BACKEND*" :event-protocol))
               (el (find-symbol "*EVENT-LOOP*" :event-protocol))
               (mine (funcall (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv)))
               (calls 0)
               (my-maker (lambda () (incf calls) mine)))
          (progv (list maker eb el) (list my-maker mine nil)
            (ensure-http-backend :async)
            (ok (eq my-maker (symbol-value maker)) "the caller's maker is kept")
            (ok (eq mine (symbol-value eb)) "the caller's *event-backend* is kept")
            (multiple-value-bind (port thread) (%one-shot-server "mine")
              (let ((res (handler-case
                             (with-backend (:async)
                               (get (format nil "http://127.0.0.1:~D/" port)
                                    :timeout 5 :trust-env nil))
                           (error (e) e))))
                (%finish-one-shot port thread)
                (ok (and (typep res 'http-response)
                         (string= "mine" (response-text res)))
                    (format nil "GET through the caller's maker: ~A" res))
                (ok (= 1 calls) "the blocking GET made its loop from the caller's maker"))))))))

(deftest wired-maker-follows-bound-event-backend
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (progn
        (ensure-http-backend :async)
        (let* ((maker (find-symbol "*EVENT-BACKEND-MAKER*" :http-backend-async))
               (eb (find-symbol "*EVENT-BACKEND*" :event-protocol))
               (el (find-symbol "*EVENT-LOOP*" :event-protocol))
               (mine (funcall (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))))
          (progv (list maker eb el) (list nil nil nil)
            (ensure-http-backend :async)
            (let ((wired (symbol-value maker)))
              (ok (functionp wired) "the facade set a maker")
              (ok (eq (symbol-value eb) (funcall wired))
                  "the maker returns the bound event-protocol:*event-backend*")
              (progv (list eb) (list mine)
                (ok (eq mine (funcall wired))
                    "a rebinding of *event-backend* is followed at request time"))))))))

(deftest ensure-backend-does-not-replan-loaded-system
  (testing "#29: a loaded backend system is not ASDF-planned again per request"
    (ensure-http-backend :dexador)
    (let ((t0 (get-internal-real-time)))
      (dotimes (i 10) (ensure-http-backend :dexador))
      (let ((ms (/ (- (get-internal-real-time) t0)
                   (/ internal-time-units-per-second 1000.0) 10)))
        ;; asdf:load-system on the loaded dexador tree was ~130 ms a call.
        (ok (< ms 20) (format nil "~,2F ms per ensure-http-backend :dexador" ms))))))

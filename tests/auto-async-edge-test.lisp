;;;; Prosecution probes for cl-stack-http#27 / #29 with the :auto order now
;;;; async-first: a missing async stack, threads, a call from a loop callback.

(in-package #:cl-stack-http/tests)

(defun %read-request-head (stream)
  "One request head from STREAM as a string, or NIL at EOF."
  (let ((head (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (loop with tail = 0
          for b = (read-byte stream nil nil)
          unless b do (return-from %read-request-head nil)
          do (vector-push-extend b head)
             (setf tail (logand #xffffffff (logior (ash tail 8) b)))
          until (= tail #x0d0a0d0a))
    (map 'string #'code-char head)))

(defun %keep-alive-origin ()
  "Loopback HTTP/1.1 keep-alive origin, one thread per connection, every
   request answered 200 \"ok\". → (values port stop-fn)"
  (let* ((server (usocket:socket-listen "127.0.0.1" 0
                                        :reuse-address t :backlog 64
                                        :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port server)))
    (labels ((serve (client)
               (handler-case
                   (unwind-protect
                        (let ((stream (usocket:socket-stream client)))
                          (loop while (%read-request-head stream)
                                do (write-sequence
                                    (%octets (format nil "HTTP/1.1 200 OK~C~CContent-Length: 2~C~C~C~Cok"
                                                     #\Return #\Newline #\Return #\Newline
                                                     #\Return #\Newline))
                                    stream)
                                   (finish-output stream)))
                     (ignore-errors (usocket:socket-close client)))
                 (error () nil)))
             (accept-loop ()
               (handler-case
                   (loop (let ((client (usocket:socket-accept server)))
                           (bt:make-thread (lambda () (serve client))
                                           :name "cl-stack-http keep-alive conn")))
                 (error () nil))))
      (let ((acceptor (bt:make-thread #'accept-loop :name "cl-stack-http keep-alive origin")))
        (values port
                (lambda ()
                  (ignore-errors (usocket:socket-close server))
                  (ignore-errors (bt:join-thread acceptor))))))))

(defun %call-with-watchdog (seconds thunk)
  "THUNK on a new thread. → its value, the condition it signalled, or
   :HUNG when it has not returned after SECONDS (the thread is left behind)."
  (let* ((box (list :pending))
         (thread (bt:make-thread
                  (lambda ()
                    (setf (car box)
                          (handler-case (funcall thunk)
                            (serious-condition (c) c))))
                  :name "cl-stack-http watchdog")))
    (loop repeat (ceiling seconds 0.05)
          while (eq :pending (car box))
          do (sleep 0.05))
    (if (eq :pending (car box))
        (progn (ignore-errors (bt:destroy-thread thread)) :hung)
        (car box))))

(deftest auto-without-async-stack-stays-cheap
  (testing "#29 with the async-first order: a missing http-backend-async is not looked up per request"
    (ensure-http-backend :dexador)
    (let ((cl-stack-http::*backend-systems*
            (list* '(:async . "http-backend-async-not-installed")
                   cl-stack-http::*backend-systems*)))
      (ensure-http-backend :auto)
      (let ((t0 (get-internal-real-time)))
        (dotimes (i 10) (ensure-http-backend :auto))
        (let ((ms (/ (- (get-internal-real-time) t0)
                     (/ internal-time-units-per-second 1000.0) 10)))
          (ok (< ms 20) (format nil "~,2F ms per ensure-http-backend :auto (async not installed; search functions ~S)"
                                ms asdf:*system-definition-search-functions*)))))))

(defun %threaded-gets (url preferred &key (threads 8) (each 3))
  "THREADS threads, EACH blocking GETs of URL with :PREFERRED PREFERRED.
   → (values result-count failures); a failure is the status or the error text."
  (let* ((results (list nil))
         (lock (bt:make-lock))
         (workers
          (loop repeat threads
                collect (bt:make-thread
                         (lambda ()
                           (dotimes (i each)
                             (let ((r (handler-case
                                          (response-status
                                           (get url :preferred preferred
                                                    :timeout 10 :trust-env nil))
                                        (error (e) (princ-to-string e)))))
                               (bt:with-lock-held (lock) (push r (car results))))))
                         :name "cl-stack-http GET worker"))))
    (mapc #'bt:join-thread workers)
    (values (length (car results)) (remove 200 (car results)))))

(deftest auto-async-get-from-threads
  #+win32 (skip "Windows :auto is winhttp-first by design")
  #-win32
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (multiple-value-bind (port stop) (%keep-alive-origin)
        (unwind-protect
            (let ((url (format nil "http://127.0.0.1:~D/" port)))
              (testing "control: 8 threads x 3 GETs with :preferred :dexador"
                (multiple-value-bind (n bad) (%threaded-gets url :dexador)
                  (ok (and (= 24 n) (null bad))
                      (format nil ":dexador: ~D results, failures ~S" n bad))))
              (testing "requests in a thread pool: 8 threads x 3 GETs with the default :auto"
                ;; The prosecution asserted ":auto picks async here" to show
                ;; that async-first blocking :auto breaks under threads. The fix
                ;; keeps blocking :auto dexador first until one libuv loop per
                ;; thread is safe (event-backend-libuv#31).
                (ok (string-equal "dexador" (backend-name (ensure-http-backend :auto)))
                    "blocking :auto picks dexador here")
                (multiple-value-bind (n bad) (%threaded-gets url :auto)
                  (ok (and (= 24 n) (null bad))
                      (format nil ":auto: ~D results, failures ~S" n bad)))))
          (funcall stop)))))

(deftest auto-get-from-a-loop-callback
  #+win32 (skip "Windows :auto is winhttp-first by design")
  #-win32
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (multiple-value-bind (port stop) (%keep-alive-origin)
        (unwind-protect
             (let* ((url (format nil "http://127.0.0.1:~D/" port))
                    (make-libuv (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv))
                    (res (%call-with-watchdog
                          10
                          (lambda ()
                            (let* ((eb (funcall make-libuv))
                                   (el (event-protocol:make-event-loop eb))
                                   (seen nil))
                              (event-protocol:with-event-backend (eb)
                                (event-protocol:with-event-loop-var (el)
                                  (event-protocol:defer eb el
                                    (lambda ()
                                      ;; A blocking requests-style call from a
                                      ;; callback (a timer, a server handler).
                                      (setf seen (handler-case
                                                     (response-status (get url :timeout 5 :trust-env nil))
                                                   (error (e) e)))))
                                  (event-protocol:run eb el :stop-when-idle t)))
                              (ignore-errors (event-protocol:close-event-loop eb el))
                              seen)))))
               (ok (eql 200 res)
                   (format nil "blocking :auto GET inside a loop callback: ~A" res)))
          (funcall stop)))))

(defun %run-promises-on-libuv (thunk)
  "Call THUNK on a fresh libuv loop; it returns a list of Blackbird promises.
   The loop stops when all have settled (idle pooled connections would keep
   it alive, http-backend-async#79). → the list of values / conditions (THUNK's
   own error as a one-element list), or :HUNG after 10 s."
  (%call-with-watchdog
   10
   (lambda ()
     (let* ((eb (funcall (find-symbol "MAKE-LIBUV-BACKEND" :event-backend-libuv)))
            (el (event-protocol:make-event-loop eb))
            (results nil))
       (unwind-protect
           (event-protocol:with-event-backend (eb)
             (event-protocol:with-event-loop-var (el)
               (event-protocol:defer eb el
                 (lambda ()
                   (block deferred
                     (let* ((promises (handler-case (funcall thunk)
                                        (error (e)
                                          (setf results (list e))
                                          (event-protocol:stop eb el)
                                          (return-from deferred))))
                            (box (make-list (length promises) :initial-element :pending))
                            (left (length promises)))
                       (setf results box)
                       (loop for p in promises
                             for i from 0
                             do (let ((i i))
                                  (flet ((settle (v)
                                           (setf (nth i box) v)
                                           (when (zerop (decf left))
                                             (event-protocol:stop eb el))))
                                    (blackbird:catcher
                                     (blackbird:attach p #'settle)
                                     (error (e) (settle e))))))))))
               (event-protocol:run eb el :stop-when-idle t)))
         (ignore-errors (event-protocol:close-event-loop eb el)))
       results))))

(deftest auto-get-async-takes-the-async-backend
  #+win32 (skip "Windows :auto is winhttp-first by design")
  #-win32
  (if (not (%async-stack-installed-p))
      (skip +async-skip-message+)
      (multiple-value-bind (port stop) (%keep-alive-origin)
        (unwind-protect
            (let ((url (format nil "http://127.0.0.1:~D/" port)))
              (testing "three get-async calls with the default :auto (dexador has no SEND-ASYNC)"
                (let ((res (%run-promises-on-libuv
                            (lambda ()
                              (loop repeat 3
                                    collect (get-async url :timeout 5 :trust-env nil))))))
                  (ok (and (listp res) (every (lambda (r) (and (typep r 'http-response)
                                                               (= 200 (response-status r))))
                                              res))
                      (format nil "get-async x3: ~S" res))))
              (testing "a default session: blocking calls on dexador, -async calls on async"
                (let ((s (make-session :trust-env nil)))
                  (unwind-protect
                      (progn
                        (ok (string-equal "dexador" (backend-name (session-backend s))))
                        (ok (= 200 (response-status (session-get s url))) "session-get")
                        (let ((res (%run-promises-on-libuv
                                    (lambda () (list (session-get-async s url)
                                                     (session-get-async s url))))))
                          (ok (and (listp res)
                                   (every (lambda (r) (and (typep r 'http-response)
                                                           (= 200 (response-status r))))
                                          res))
                              (format nil "session-get-async x2: ~S" res))))
                    (close-session s))))
              (testing "a session made with an explicit backend keeps it for -async calls"
                (let ((s (make-session :preferred :dexador :trust-env nil)))
                  (unwind-protect
                      (let ((res (%run-promises-on-libuv
                                  (lambda () (list (session-get-async s url))))))
                        (ok (and (listp res) (typep (first res) 'unsupported-operation))
                            (format nil "dexador session-get-async: ~S" res)))
                    (close-session s)))))
          (funcall stop)))))

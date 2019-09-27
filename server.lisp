;;;; overlord-cli.lisp

(defpackage #:overlord-cli
  (:use #:cl #:alexandria #:serapeum #:overlord #:trivial-gray-streams)
  (:import-from #:trivia #:match #:ematch #:plist #:property)
  (:import-from #:overlord/safer-read #:safer-read)
  (:export
   #:save-client
   #:start-server
   #:stop-server))

(in-package #:overlord-cli)

(defconst localhost "127.0.0.1")

(defunit eof)

(def server-name "overlord-server")

(defun gen-auth ()
  (~> 32
      ironclad:random-data
      cl-base64:usb8-array-to-base64-string))

(defclass server ()
  ((name :initarg :name :type string :accessor server-name)
   (master-socket :accessor master-socket)
   (host :initarg :host :accessor host)
   (lock :reader monitor :initform (bt:make-lock))
   (stopped :initform t :accessor stopped)
   (auth :initarg :auth :reader auth))
  (:default-initargs
   :name server-name
   :host localhost
   :auth (gen-auth)))

(defvar *server* (make 'server))

(defgeneric server-start (server))
(defgeneric server-stop (server))

(defun start-server (&key fg)
  (unless (stopped *server*)
    (error "Server is already running. Stop it with ~s."
           'stop-server))
  (flet ((start () (server-start *server*)))
    (if fg (start)
        (bt:make-thread
         (dynamic-closure
          '(*trace-output* *message-stream*)
          #'start)
         :name "Overlord CLI server")))
  *server*)

(defun stop-server ()
  (server-stop *server*))

(exit-hooks:add-exit-hook #'stop-server)

(defparameter *servers-dir*
  (uiop:xdg-cache-home "overlord-cli" "server/"))

(defgeneric server-file (server)
  (:method ((name string))
    (path-join *servers-dir*
               (make-pathname :name name))))

(defgeneric clear-server-file (server)
  (:method ((name string))
    (uiop:delete-file-if-exists (server-file name))))

(def opts
  `((("verbose" #\v) :type boolean :optional t :documentation "be verbose")
    (("help" #\h #\?) :type boolean :optional t :documentation "be helpful")
    (("version" #\V) :type boolean :optional t :documentation "print version")
    (("debug" #\d) :type boolean :optional t :documentation "print debug information")
    (("jobs" #\j) :type integer :optional t :initial-value ,(or *jobs* nproc)
                  :documentation "max # of parallel jobs")))

(defclass plexer-stream (fundamental-character-output-stream)
  ((dest-stream :initarg :dest-stream :reader dest-stream)
   (prefix :initarg :prefix))
  (:documentation "Wrap another stream with the following behavior:
whenever data is written to the wrapper stream, what is written to the
wrapped stream is a readable form beginning with the supplied prefix.

E.g. if you write \"hello\" to the wrapper stream, what is written to the wrapped stream (module buffering) is (:prefix \"hello\").

This is for multiplexing."))

(defmethods plexer-stream (self dest-stream prefix buffer)
  (:method stream-write-char (self char)
    (prog1 char
      (stream-write-string self (string char))))
  (:method stream-write-string (self string &optional start end)
    (prog1 string
      (let ((string
              (if start
                  (subseq string start end)
                  string)))
        (write `(,prefix ,string)
               :stream dest-stream
               :readably t
               :pretty nil))))
  (:method stream-advance-to-column (self col)
    (stream-advance-to-column dest-stream col)))

;;; Implement passthrough methods for plexer streams.
(macrolet ((passthrough-1 (fn)
             `(defmethod ,fn ((s plexer-stream))
                (,fn (dest-stream s))))
           (passthrough (&rest fns)
             `(progn
                ,@(mapcar (op `(passthrough-1 ,_))
                          fns))))
  (passthrough stream-line-column
               stream-start-line-p
               stream-terpri
               stream-fresh-line
               stream-finish-output
               stream-force-output
               stream-clear-output))

(defun plex (stream prefix)
  (make 'plexer-stream
        :dest-stream stream
        :prefix prefix))

(defun call/stream-capture (stream fn)
  "Auxiliary function for `with-stream-capture'."
  (with-open-stream (*standard-output* (plex stream :out))
    (with-open-stream (*error-output* (plex stream :err))
      (block nil
        (restart-case
            (handler-bind ((serious-condition
                             (lambda (e)
                               (invoke-restart 'die e 1))))
              (progn
                (funcall fn)
                0))
          (die (err status)
            (princ err *error-output*)
            (return status)))))))

(defmacro with-stream-capture ((&key stream) &body body)
  "Run BODY, multiplexing stdout and stderr to STREAM (using instances
of `plexer-stream'). Also, any error will be written to
`*error-output*', then quashed.

Return 0 if there were no errors, 1 otherwise."
  (with-thunk (body)
    `(call/stream-capture ,stream ,body)))

(defun call/current-dir (dir fn)
  (let* ((dir (uiop:pathname-directory-pathname dir))
         (*default-pathname-defaults* dir)
         (overlord:*base* dir))
    (funcall fn)))

(defmacro with-current-dir ((dir &key) &body body)
  (with-thunk (body)
    `(call/current-dir ,dir ,body)))

(defmethods server (self master-socket client-sockets lock kernel
                         name host stopped auth)
  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~:[running~;stopped~]" stopped)))
  (:method server-loop (self)
    (loop until stopped
          for client-socket
            = (progn
                ;; Not necessary, but prevents an error when the
                ;; server is stopped.
                (usocket:wait-for-input master-socket)
                (unless master-socket
                  (return-from server-loop))
                (usocket:socket-accept master-socket))
          for client-stream = (usocket:socket-stream client-socket)
          do (bt:make-thread
              (lambda ()
                (unwind-protect
                     (handle-stream self client-stream)
                  (close client-stream)
                  (usocket:socket-close client-socket))))))
  (:method handle-stream (self stream)
    (let ((status
            (block status
              (with-stream-capture (:stream stream)
                (ematch (safer-read stream :fail eof)
                  ((plist :auth client-auth :args args :dir dir :makeflags _)
                   (check-auth self client-auth)
                   (with-current-dir (dir)
                     (multiple-value-bind (options free-args)
                         (handler-bind ((serious-condition
                                          (lambda (e)
                                            (invoke-restart 'die e 2))))
                           (command-line-arguments:process-command-line-options
                            opts args))
                       (trivia:match options
                         ((trivia:property :version t)
                          (print-server-version))
                         ((trivia:property :help t)
                          (command-line-arguments:show-option-help opts :sort-names t))
                         (otherwise
                          (handler-bind ((trivia:match-error
                                           (lambda (e)
                                             (invoke-restart 'die e 2))))
                            (apply #'interpret-args self free-args options))))))))))))
      (write `(:status ,status)
             :stream stream
             :pretty nil
             :readably t)
      (finish-output stream)))
  (:method check-auth (self client-auth)
    (unless (equal auth client-auth)
      (error "Authorization failed.")))
  (:method server-start (self)
    (when stopped
      (message "Starting server")
      (setf stopped nil
            master-socket (usocket:socket-listen host 0))
      (write-server-file self)
      (message "Listening...")
      (unwind-protect
           (server-loop self)
        (server-stop self))))
  (:method server-stop (self)
    (clear-server-file self)
    (setf stopped t)
    (when master-socket
      (usocket:socket-close (nix master-socket)))
    (message "Server stopped."))
  (:method server-file (self)
    (server-file (server-name self)))
  (:method write-server-file (self)
    (assert (not (find #\Space auth)))
    (with-output-to-file (out (server-file self) :if-exists :supersede)
      (let ((port (usocket:get-local-port master-socket)))
        (format out "~a ~a ~a" host port auth))))
  (:method clear-server-file (self)
    (clear-server-file name)))

(defun print-server-version ()
  (format t "Overlord version ~a"
          (asdf:system-version (asdf:find-system "overlord"))))

(defmethod interpret-args ((self server) (args list)
                           &key (jobs (or *jobs* nproc))
                                &allow-other-keys)
  "Interpret ARGS.
Whatever is output to `*standard-output*' will be written to stdout;
whatever is output to `*error-output*' will be written to stderr."
  (ematch args
    ((eql eof))
    ((list "stop")
     (server-stop self))
    ((list* "echo" words)
     (write-string (string-join words " "))
     (terpri))
    ((list "make" system)
     (let ((*jobs* jobs))
       (asdf:make (asdf:find-system system))))
    ((list "load" system)
     (asdf:load-system (asdf:find-system system)))
    ((list "require" system)
     (cl:require (asdf:find-system system)))
    ((list "build" "file" target)
     (overlord:build
      (path-join *default-pathname-defaults*
                 (uiop:unix-namestring target))
      :jobs jobs))
    ((list "build" "package" package)
     (overlord:build
      (or (find-package package)
          (error "No such package as ~a" package))
      :jobs jobs))
    ((list "build" "symbol" package name)
     (let* ((package
              (or (find-package name)
                  (error "No such package as ~a" package)))
            (name (string-invert-case name))
            (symbol
              (or (find-symbol name package)
                  (error "No such symbol as ~a in ~a" name package))))
       (overlord:build symbol :jobs jobs)))
    ((list "init")
     (overlord:start-project (current-dir)))
    ((list "make")
     (let ((*jobs* jobs))
       (make-system-in-current-dir)))
    ((list "threads" "on")
     (setf (overlord:use-threads-p) t)
     (message "Threads on."))
    ((list "threads" "off")
     (setf (overlord:use-threads-p) nil)
     (message "Threads off."))
    ((list "threads")
     (message
      (if (overlord:use-threads-p)
          "Threads on"
          "Threads off")))))

(defun current-dir ()
  (uiop:pathname-directory-pathname *default-pathname-defaults*))

(defun make-system-in-current-dir ()
  (let* ((current-dir
           (uiop:pathname-directory-pathname *default-pathname-defaults*)))
    (make-system-in-dir current-dir)))

(defun make-system-in-dir (current-dir)
  (multiple-value-bind (system-name must-register?)
      (make-system-in-dir-1 current-dir)
    (when must-register?
      (pushnew current-dir asdf:*central-registry* :test #'equal))
    (asdf:make system-name)))

(defun make-system-in-dir-1 (current-dir)
  (check-type current-dir (satisfies uiop:directory-pathname-p))
  (let* ((.asd (overlord/util:locate-dominating-file current-dir "*.asd")))
    (unless .asd
      (error "No ASDF file. Use `overlord init` to start a project."))
    (let* ((system-name (pathname-name .asd))
           (system (asdf:find-system system-name nil))
           (unknown-system? (not system))
           (same-dir?
             (and system
                  (equal (truename current-dir)
                         (uiop:pathname-directory-pathname
                          (asdf:system-relative-pathname "overlord-cli" "")))))
           (must-register?
             (or unknown-system? (not same-dir?))))
      (values system-name must-register?))))

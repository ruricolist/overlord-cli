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
        (let ((ready nil))
          (bt:make-thread
           (dynamic-closure
            '(*trace-output* *message-stream*)
            (lambda ()
              (setf ready t)
              (start)))
           :name "Overlord CLI server")
          (loop until ready do (sleep 0.1)))))
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

(def jobs-option
  `(("jobs" #\j) :type integer :optional t :initial-value ,(or *jobs* nproc)
                 :documentation "max # of parallel jobs"))

(def system-option
  `(("system" #\s)
    :type string
    :optional nil
    :documentation "relevant system"))

(def help-option
  '(("help" #\h #\?) :type boolean :optional t :documentation "Print this help."))

(def global-opts
  `((("verbose" #\v) :type boolean :optional t :documentation "Be verbose.")
    (("version" #\V) :type boolean :optional t :documentation "Print version.")
    (("debug" #\d) :type boolean :optional t :documentation "Print debug information.")
    ,help-option))

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
                            global-opts args))
                       (declare (ignore free-args))
                       (trivia:match options
                         ((trivia:property :version t)
                          (print-server-version))
                         ((trivia:property :help t)
                          (format t "~&Usage: overlord <command> [<args>]~%")
                          (command-line-arguments:show-option-help global-opts :sort-names t)
                          (format t "~2&Subcommands:~%~:{ ~32a ~a~%~}"
                                  (mapcar (op (list (string-join (subcommand-prefix _1) " ")
                                                    (subcommand-summary _1)))
                                          (list-subcommands))))
                         (otherwise
                          (handler-bind ((trivia:match-error
                                           (lambda (e)
                                             (invoke-restart 'die e 2))))
                            ;; The raw args, not the free args.
                            (interpret-args self args))))))))))))
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

(defclass subcommand ()
  ((prefix :initarg :prefix :type list :reader subcommand-prefix)
   (summary :initarg :summary :type string :reader subcommand-summary)
   (options :initarg :options :type list :reader subcommand-options)
   (arity :initarg :arity :type (integer 0 *) :reader subcommand-arity)
   (variadic? :initarg :variadic :type boolean :reader subcommand-variadic?)
   (fn :initarg :fn :type function :reader subcommand-fn))
  (:default-initargs
   :summary "NO DOCS"
   :options '()
   :arity 0
   :variadic nil
   :prefix (required-argument :prefix)
   :fn (required-argument :fn)))

(def subcommands
  (list
   (make 'subcommand
         :prefix '("stop")
         :summary "Stop the server."
         :fn (lambda ()
               (server-stop *server*)))
   (make 'subcommand
         :prefix '("echo")
         :summary "Repeat any options provided."
         :variadic t
         :options '()
         :fn (lambda (&rest words)
               (write-string (string-join words " "))
               (terpri)))
   (make 'subcommand
         :prefix '("make")
         :summary "Build a system with ASDF."
         :options `(,jobs-option
                    (("system") :type string :optional t :documentation "System."))
         :fn (lambda (&key ((:jobs *jobs*) (or *jobs* nproc))
                      system)
               (if system
                   (make-system-in-current-dir)
                   (asdf:make (asdf:find-system system)))))
   (make 'subcommand
         :prefix '("load")
         :summary "Load a system."
         :arity 1
         :options (list system-option)
         :fn (lambda (system)
               (asdf:load-system (asdf:find-system system))))
   (make 'subcommand
         :prefix '("require")
         :summary "Require a system."
         :options (list system-option)
         :arity 1
         :fn (lambda (system)
               (let ((system (asdf:find-system system)))
                 (unless (asdf:component-loaded-p system)
                   (asdf:load-system system)))))
   (make 'subcommand
         :prefix '("build" "file")
         :summary "Build a file."
         :options `(,jobs-option
                    (("file") :type string :optional nil :documentation "File to build."))
         :arity 1
         :fn (lambda (file &key (jobs (or *jobs* nproc)))
               (let* ((file (uiop:parse-unix-namestring file))
                      (file (path-join *default-pathname-defaults* file)))
                 (overlord:build file :jobs jobs))))
   (make 'subcommand
         :prefix '("build" "package")
         :summary "Build a package."
         :options `(,jobs-option
                    (("package") :type string :optional nil :documentation "Package to build."))
         :arity 1
         :fn (lambda (package &key (jobs (or *jobs* nproc)))
               (overlord:build
                (or (find-package package)
                    (error "No such package as ~a" package))
                :jobs jobs)))
   (make 'subcommand
         :prefix '("build" "symbol")
         :summary "Build a symbol."
         :options `(,jobs-option
                    (("package") :type string :optional nil :documentation "Package to build.")
                    (("symbol") :type string :optional nil :documentation "Package to build."))
         :fn (lambda (package symbol &key (jobs (or *jobs* nproc)))
               (let* ((package
                        (or (find-package package)
                            (error "No such package as ~a" package)))
                      (symbol (string-invert-case symbol))
                      (symbol
                        (or (find-symbol symbol package)
                            (error "No such symbol as ~a in ~a" symbol package))))
                 (overlord:build symbol :jobs jobs))))
   (make 'subcommand
         :prefix '("init")
         :summary "Set up a project system."
         :fn (lambda ()
               (overlord:start-project (current-dir))))
   (make 'subcommand
         :prefix '("help")
         :summary "Explain a command."
         :variadic t
         :fn (lambda (&rest prefix)
               (let ((sc (find prefix subcommands
                               :key #'subcommand-prefix
                               :test #'equal)))
                 (unless sc
                   (error "No such subcommand as: ~{~a~^ ~}" prefix))
                 (print-subcommand-help sc))))))

(defmethod print-subcommand-help ((self subcommand))
  (with-slots (prefix summary options arity) self
    (let ((args (loop for i below arity collect (fmt "arg~a" (1+ i)))))
      (format t "Usage: overlord ~{~a~^ ~}~@[ ~{<~a>~^ ~}~]~%" prefix args))
    (format t "~&~a~%" summary)
    (command-line-arguments:show-option-help options)))

(defun list-subcommands ()
  subcommands)

(defmethod handle-subcommand-line ((self subcommand) args)
  (with-slots (options fn arity variadic? prefix summary) self
    (let* ((args (drop (length prefix) args))
           (name (string-join prefix "-"))
           (options (cons help-option options))
           (fn (lambda (&rest args)
                 (let ((kwargs (drop-while (complement #'keywordp) args)))
                   (if (getf kwargs :help)
                       (print-subcommand-help self)
                       (apply fn args)))))
           (fn (if (not variadic?) fn
                   (lambda (&rest args)
                     (let ((positional (take arity args))
                           (rest (nth arity args))
                           (options (drop (1+ arity) args)))
                       (multiple-value-call fn
                         (values-list positional)
                         (values-list rest)
                         (values-list options)))))))
      (command-line-arguments:handle-command-line options
                                                  fn
                                                  :positional-arity arity
                                                  :rest-arity variadic?
                                                  :name name
                                                  :command-line args))))

(defmethod subcommand-matches? ((self subcommand) args)
  (with-slots (prefix) self
    (starts-with-subseq prefix args :test #'equal)))

(defmethod interpret-args ((*server* server) (args list))
  "Interpret ARGS.
Whatever is output to `*standard-output*' will be written to stdout;
whatever is output to `*error-output*' will be written to stderr."
  (when (eql eof args)
    (return-from interpret-args))
  (dolist (subcommand subcommands)
    (when (subcommand-matches? subcommand args)
      (return-from interpret-args
        (handle-subcommand-line subcommand args))))
  (error "No subcommand matches ~a." args))

(defun current-dir ()
  (uiop:pathname-directory-pathname *default-pathname-defaults*))

(defun make-system-in-current-dir ()
  (let* ((current-dir (current-dir)))
    (make-system-in-dir current-dir)))

(defun make-system-in-dir (current-dir)
  (multiple-value-bind (system-name must-register?)
      (directory-system current-dir)
    (when must-register?
      (pushnew current-dir asdf:*central-registry* :test #'equal))
    (asdf:make system-name)))

(defun directory-system (dir)
  "Return the name of the system defined in the current directory.
If the system is not on the ASDF path, return T as the second value."
  (check-type dir (satisfies uiop:directory-pathname-p))
  (let* ((.asd (overlord/util:locate-dominating-file dir "*.asd")))
    (unless .asd
      (error "No ASDF file. Try overlord init to start a project?"))
    (let* ((system-name (pathname-name .asd))
           (system (asdf:find-system system-name nil))
           (unknown-system? (not system))
           (same-dir?
             (and system
                  (equal (truename dir)
                         (uiop:pathname-directory-pathname
                          (asdf:system-relative-pathname "overlord-cli" "")))))
           (must-register?
             (or unknown-system? (not same-dir?))))
      (values system-name must-register?))))

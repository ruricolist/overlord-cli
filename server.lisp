;;;; overlord-cli.lisp

(defpackage #:overlord-cli
  (:use #:cl #:alexandria #:serapeum #:overlord)
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

(defun start-server ()
  (server-start *server*))

(defun stop-server ()
  (server-stop *server*))

(defparameter *servers-dir*
  (uiop:xdg-cache-home "overlord-cli" "server/"))

(defgeneric server-file (server)
  (:method ((name string))
    (path-join *servers-dir*
               (make-pathname :name name))))

(defgeneric clear-server-file (server)
  (:method ((name string))
    (uiop:delete-file-if-exists (server-file name))))

(opts:define-opts
  (:name
   :version
   :description "Show the client and server versions."
   :short #\v
   :long "version"))

(defun call/stream-capture (fn)
  "Auxiliary function for `with-stream-capture'."
  (handler-case
      (with-open-stream (*standard-output* (make-string-output-stream))
        (with-open-stream (*error-output* (make-string-output-stream))
          (funcall fn)
          (values 0
                  (get-output-stream-string *standard-output*)
                  (get-output-stream-string *error-output*))))
    (serious-condition (e)
      (values 1 "" (princ-to-string e)))))

(defmacro with-stream-capture ((&key) &body body)
  "Run BODY, returning three values: a status code (0 for success), a
string containing whatever whatever was output to `*standard-output*',
and a string containing whatever was output to `*error-output*'."
  (with-thunk (body)
    `(call/stream-capture ,body)))

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
          for client-socket = (usocket:socket-accept master-socket)
          do (unwind-protect
                  (with-open-stream (client-stream (usocket:socket-stream client-socket))
                    (handle-stream self client-stream))
               (usocket:socket-close client-socket))))
  (:method handle-stream (self stream)
    (multiple-value-bind (status out err)
        (with-stream-capture ()
          (ematch (safer-read stream :fail eof)
            ((plist :auth client-auth :args args :dir dir)
             (message "Server received: ~a" args)
             (check-auth self client-auth)
             (with-current-dir (dir)
               (multiple-value-bind (options free-args)
                   (opts:get-opts args)
                 (trivia:match options
                   ((trivia:property :version t)
                    (print-server-version))
                   (otherwise
                    (interpret-args self free-args))))))))
      (write (list status out err)
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
  (format t "Overlord version ~a" (asdf:system-version (asdf:find-system "overlord"))))

(defmethod interpret-args ((self server) (args list))
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
    ((list "eval" form)
     (prin1
      (let* ((*readtable* (named-readtables:find-readtable :standard))
             (form (safer-read form))
             (*package* (find-package :cl-user)))
        (eval form))))
    ((list "make" system)
     (asdf:make (asdf:find-system system)))
    ((list "load" system)
     (asdf:load-system (asdf:find-system system)))
    ((list "require" system)
     (cl:require (asdf:find-system system)))
    ((list "build" target)
     (overlord:build (uiop:unix-namestring target)))
    ((list "build" package name)
     (let* ((package
              (or (find-package name)
                  (error "No such package as ~a" package)))
            (name (string-invert-case name))
            (symbol
              (or (find-symbol name package)
                  (error "No such symbol as ~a in ~a" name package))))
       (overlord:build symbol)))))

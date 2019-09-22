;;;; overlord-cli.lisp

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

(defgeneric read-server-file (server)
  (:method ((name string))
    (ematch (tokens (read-file-into-string (server-file name)))
      ((list host port auth)
       (values host (parse-integer port) auth)))))

(defgeneric clear-server-file (server)
  (:method ((name string))
    (uiop:delete-file-if-exists (server-file name))))

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
        (handler-case
            (ematch (safer-read stream :fail eof)
              ((plist :auth client-auth :args args :dir dir)
               (setf dir (uiop:pathname-directory-pathname dir))
               (force-output *message-stream*)
               (with-open-stream (*standard-output* (make-string-output-stream))
                 (with-open-stream (*error-output* (make-string-output-stream))
                   (check-auth self client-auth)
                   (let ((*default-pathname-defaults* dir)
                         (overlord:*base* dir))
                     (interpret-args self args))
                   (values 0
                           (get-output-stream-string *standard-output*)
                           (get-output-stream-string *error-output*))))))
          (serious-condition (e)
            (values 1 "" (princ-to-string e))))
      (write (list status out err)
             :stream stream
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
  (:method read-server-file (self)
    (read-server-file name))
  (:method clear-server-file (self)
    (clear-server-file name)))

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
    ((list "version")
     (format t "Overlord version ~a" (asdf:system-version (asdf:find-system "overlord"))))
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

(defclass client ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (auth :initarg :auth :accessor auth)))

(defmethods client (self host port auth)
  (:method client-send (self (arguments list))
    (handler-case
        (usocket:with-client-socket (sock stream host port :timeout 10)
          (write (list :auth auth
                       :args arguments
                       :dir (uiop:getcwd))
                 :stream stream
                 :readably t)
          (force-output stream)
          (ematch (safer-read stream :fail '(1 "" ""))
            ((list (and status (type fixnum))
                   (and out (type string))
                   (and err (type string)))
             (values status out err))))
      (usocket:timeout-error ()
        (values 1 "" "Connection attempt timed out -- is server running?")))))

(defun make-client (server-name)
  (multiple-value-bind (host port auth) (read-server-file server-name)
    (make 'client :host host :port port :auth auth)))

(defun client-entry-point (&aux (stdout uiop:*stdout*)
                                (stderr uiop:*stderr*)
                                (*message-stream* stderr)
                                (arguments (uiop:command-line-arguments))
                                ;; TODO set from arguments
                                (server-name server-name))
  (assert (every #'stringp arguments))
  (when (equal (uiop:command-line-arguments) "--version")
    (message "Overlord client ~a"
             (asdf:system-version "overlord-cli"))
    (uiop:quit 0))
  (nest
   #+sbcl sb-sys:without-gcing
   (mvlet* ((client
             (handler-case
                 (make-client server-name)
               (file-error ()
                 (princ "No server is running." stderr)
                 (uiop:quit 2))))
            (status out err
             (client-send client arguments)))
     (check-type status integer)
     (write-string out stdout)
     (write-string err stderr)
     (uiop:quit status))))

(defun save-client (filename)
  (setf filename (path-join (user-homedir-pathname) filename))
  (setf uiop:*image-entry-point* #'client-entry-point)
  (uiop:delete-file-if-exists filename)
  (multiple-value-call #'uiop:dump-image
    filename
    :allow-other-keys t
    :executable t
    :purify t
    #+sb-core-compression (values :compression t))
  (format uiop:*stderr* "Client saved to ~a~%" filename)
  (finish-output)
  (uiop:quit 0))

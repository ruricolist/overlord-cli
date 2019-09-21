;;;; overlord-cli.lisp

(in-package #:overlord-cli)

(defconst localhost "127.0.0.1")

(def port 3535)

(defclass server ()
  (master-socket
   (host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (element-type :initarg :element-type)
   (lock :reader monitor :initform (bt:make-lock))
   (stopped :initform t :accessor stopped))
  (:default-initargs
   :host localhost
   :element-type 'character
   :port port))

(defconst eof "eof")

(defvar *server* (make 'server))

(defgeneric start (server))
(defgeneric stop (server))

(defun start-server ()
  (start *server*))

(defun stop-server ()
  (stop *server*))

(defmethods server (self master-socket client-sockets lock kernel
                         element-type host port stopped)
  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~:[not stopped~;stopped~]" stopped)))
  (:method server-loop (self)
    (loop until stopped
          for client-socket = (usocket:socket-accept master-socket)
          do (unwind-protect
                  (with-open-stream (client-stream (usocket:socket-stream client-socket))
                    (handle-stream self client-stream))
               (usocket:socket-close client-socket))))
  (:method handle-stream (self stream)
    (multiple-value-bind (status out err)
        (let ((args (read stream nil eof)))
          (message "Server received: ~a" args)
          (force-output *message-stream*)
          (with-open-stream (*standard-output* (make-string-output-stream))
            (with-open-stream (*error-output* (make-string-output-stream))
              (interpret-args self args)
              (values 0
                      (get-output-stream-string *standard-output*)
                      (get-output-stream-string *error-output*)))))
      (write (list status out err)
             :stream stream
             :readably t)
      (finish-output stream)))
  (:method start (self)
    (unless stopped
      (return-from start))
    (message "Starting server")
    (setf stopped nil
          master-socket (usocket:socket-listen host port))
    (message "Listening...")
    (force-output *message-stream*)
    (unwind-protect
         (server-loop self)
      (setf stopped t)
      (message "Server stopped, closing sockets..."))
    (message "Server stopped."))
  (:method stop (self)
    (setf stopped t)))

(defmethod interpret-args ((self server) (args list))
  (ematch args
    ((eql eof))
    ((list "stop")
     (stop self))
    ((list* "echo" words)
     (write-string (string-join words " "))
     (terpri))
    ((list "eval" form)
     (prin1
      (let ((*package* (find-package :cl-user))
            (*readtable* (named-readtables:find-readtable :standard)))
        (eval (read form)))))
    ((list "version")
     (princ (asdf:system-version "overlord")))
    ((list "make" system)
     (asdf:make system))
    ((list "build" "file" target)
     (overlord:build (uiop:unix-namestring target)))))

(defclass client ()
  ((host :initarg :host)
   (port :initarg :port)
   (element-type :initarg :element-type))
  (:default-initargs
   :host localhost
   :element-type 'character
   :port port))

(defmethods client (self host port element-type)
  (:method client-send (self (message list))
    (handler-case
        (usocket:with-client-socket (sock stream host port :element-type element-type
                                                           :timeout 10)
          (format stream "~s" message)
          (force-output stream)
          (ematch (read stream nil '(0 "" ""))
            ((list (and status (type fixnum))
                   (and out (type string))
                   (and err (type string)))
             (values status out err))))
      (usocket:timeout-error ()
        (values -1 "" "Connection attempt timed out -- is server running?")))))

(defun quiet-close-socket (socket)
  (ignoring usocket:socket-error
    (usocket:socket-close socket)))

(defun client-entry-point (&aux (stdout uiop:*stdout*)
                                (stderr uiop:*stderr*)
                                (arguments (uiop:command-line-arguments)))
  (assert (every #'stringp arguments))
  (when (equal (uiop:command-line-arguments) "--version")
    (format t "Overlord client ~a"
            (asdf:system-version "overlord-cli"))
    (uiop:quit 0))
  (mvlet* ((client (make 'client :port port))
           (status out err
            (client-send client arguments)))
    (check-type status integer)
    (write-string out stdout)
    (write-string err stderr)
    (uiop:quit status)))

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
  (format t "Client saved to ~a~%" filename)
  (finish-output)
  (uiop:quit 0))

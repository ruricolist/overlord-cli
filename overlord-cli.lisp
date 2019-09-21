;;;; overlord-cli.lisp

(in-package #:overlord-cli)

(defconst localhost "127.0.0.1")

(defunit eof)

(def server-name "overlord-server")

(defclass server ()
  ((name :initarg :name :type string :accessor server-name)
   (master-socket :accessor master-socket)
   (host :initarg :host :accessor host)
   (element-type :initarg :element-type)
   (lock :reader monitor :initform (bt:make-lock))
   (stopped :initform t :accessor stopped))
  (:default-initargs
   :name server-name
   :host localhost
   :element-type 'character))

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
      ((list host port)
       (values host (parse-integer port))))))

(defgeneric clear-server-file (server)
  (:method ((name string))
    (uiop:delete-file-if-exists (server-file name))))

(defmethods server (self master-socket client-sockets lock kernel
                         element-type name host stopped)
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
    ;; (assert (not (find #\Space auth)))
    (with-output-to-file (out (server-file self) :if-exists :supersede)
      (format out "~a ~a" host (usocket:get-local-port master-socket))))
  (:method read-server-file (self)
    (read-server-file name))
  (:method clear-server-file (self)
    (clear-server-file name)))

(defmethod interpret-args ((self server) (args list))
  (handler-case
      (interpret-args-1 self args)
    (serious-condition (e)
      (princ e *error-output*))))

(defmethod interpret-args-1 ((self server) (args list))
  (ematch args
    ((eql eof))
    ((list "stop")
     (server-stop self))
    ((list* "echo" words)
     (write-string (string-join words " "))
     (terpri))
    ((list "eval" form)
     (prin1
      (let ((*package* (find-package :cl-user))
            (*readtable* (named-readtables:find-readtable :standard)))
        (eval (read form)))))
    ((list "version")
     (format t "Overlord version ~a" (asdf:system-version (asdf:find-system "overlord"))))
    ((list "make" system)
     (asdf:make (asdf:find-system system)))
    ((list "load" system)
     (asdf:load-system (asdf:find-system system)))
    ((list "build" "file" target)
     (overlord:build (uiop:unix-namestring target)))))

(defclass client ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (element-type :initarg :element-type))
  (:default-initargs
   :element-type 'character))

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

(defun make-client (server-name)
  (multiple-value-bind (host port) (read-server-file server-name)
    (make 'client :host host :port port)))

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
                 (uiop:quit -1))))
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

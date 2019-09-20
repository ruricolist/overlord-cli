;;;; overlord-cli.lisp

(in-package #:overlord-cli)

(defconst localhost "127.0.0.1")

(def port 3535)

(defclass server ()
  (master-socket
   (host :initarg :host)
   (port :initarg :port)
   (client-sockets :type list :initform '())
   (element-type :initarg :element-type)
   (lock :reader monitor :initform (bt:make-lock))
   (stopped :initform t))
  (:default-initargs
   :host localhost
   :element-type 'character
   :port port))

(defconst eof "eof")

(defvar *server* (make 'server))

(defun start-server ()
  (server-start *server*))

(defun stop-server ()
  (server-stop *server*))

(defmethods server (self master-socket client-sockets lock kernel
                         element-type host port stopped)
  (:method server-loop (self)
    (loop until stopped do
      (let* ((sockets (cons master-socket client-sockets))
             (ready (usocket:wait-for-input sockets :ready-only t)))
        (loop for sock in ready
              until stopped
              do (if (eq sock master-socket)
                     (let ((client-socket
                             (usocket:socket-accept master-socket :element-type element-type)))
                       (synchronized (self)
                         (push client-socket client-sockets)))
                     (handler-case
                         (let* ((form (read (usocket:socket-stream sock) nil eof))
                                ;; Print package names.
                                (stdout (make-string-output-stream))
                                (stderr (make-string-output-stream))
                                (*standard-output* stdout)
                                (*error-output* stderr))
                           (unwind-protect
                                (progn
                                  (ematch form
                                    ((eql eof))
                                    ((list "stop")
                                     (server-stop self))
                                    ((list* "echo" words)
                                     (write-string (string-join words " "))
                                     (terpri))
                                    ((list "make" system)
                                     (asdf:make system)))
                                  (let ((stream (usocket:socket-stream sock)))
                                    (write (list 0
                                                 (get-output-stream-string stdout)
                                                 (get-output-stream-string stderr))
                                           :stream stream
                                           :readably t)
                                    (force-output stream)))
                             (close stdout)
                             (close stderr)))
                       (error (e)
                         (format *error-output* "An error: ~a" e)
                         (synchronized (self)
                           (removef client-sockets sock))
                         (quiet-close-socket sock))))))))
  (:method server-start (self)
    (unless stopped
      (return-from server-start))
    (message "Starting server")
    (setf stopped nil
          master-socket (usocket:socket-listen host port :backlog 256))
    (message "Listening...")
    (force-output *message-stream*)
    (unwind-protect
         (server-loop self)
      (message "Server stopped, closing sockets...")
      (mapc #'quiet-close-socket (nix client-sockets))
      (quiet-close-socket (nix master-socket)))
    (message "Server stopped."))
  (:method server-stop (self)
    (setf stopped t)))

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
    (usocket:with-client-socket (sock stream host port :element-type element-type)
      (format stream "~s" message)
      (force-output stream)
      (ematch (read stream nil '(0 "" ""))
        ((list (and status (type fixnum))
               (and out (type string))
               (and err (type string)))
         (values status out err))))))

(defun client-send* (client msg)
  (multiple-value-bind (status out err)
      (client-send client msg)
    (declare (ignore status))
    (write-string out *standard-output*)
    (write-string err *error-output*)
    (values)))

(defun quiet-close-socket (socket)
  (ignoring usocket:socket-error
    (usocket:socket-close socket)))

(defun client-entry-point (&aux (stdout uiop:*stdout*)
                                (stderr uiop:*stderr*))
  (handler-case
      (mvlet* ((client (make 'client :port port))
               (arguments (uiop:command-line-arguments))
               (message (filter #'stringp arguments))
               (status out err
                (client-send client message)))
        (write-string out stdout)
        (write-string err stderr)
        (finish-output out)
        (finish-output err)
        (uiop:quit status))
    (error (e)
      (fmt uiop:*stderr* "~a~%" e)
      (uiop:quit -1))))

(defun save-client (filename)
  (setf filename (path-join (user-homedir-pathname) filename))
  (setf uiop:*image-entry-point* #'client-entry-point)
  (uiop:delete-file-if-exists filename)
  (uiop:dump-image filename
                   :allow-other-keys t
                   :executable t
                   :purify t
                   :compression t)
  (uiop:quit 0))

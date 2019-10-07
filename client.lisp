(defpackage #:overlord-cli/client
  (:use #:cl #:st-json)
  (:export #:save-client
           #:client-entry-point))
(in-package #:overlord-cli/client)

(declaim (optimize compilation-speed space))

(defvar *debug* nil)

(defparameter *server-file*
  (uiop:xdg-cache-home "overlord-cli/server/overlord-server"))

(defun tokens (string)
  "Split a string on whitespace."
  (let ((tokens '())
        (stream (make-string-output-stream))
        (i 0))
    (loop until (eql i (length string))
          for char = (aref string i)
          if (eql char #\Space)
            do (push (get-output-stream-string stream) tokens)
               (loop while (eql (aref string i) #\Space)
                     do (incf i))
          else do
            (write-char char stream)
            (incf i))
    (push (get-output-stream-string stream) tokens)
    (close stream)
    (nreverse tokens)))

(defun read-file-into-string (file)
  (with-open-file (in file
                      :direction :input
                      :element-type 'character)
    (let* ((len (file-length in))
           (str (make-string len)))
      (read-sequence str in)
      str)))

(defun read-server-file ()
  (unless (uiop:file-exists-p *server-file*)
    (error "No server is running."))
  (destructuring-bind (host port auth)
      (tokens (read-file-into-string *server-file*))
    (values host (parse-integer port) auth)))

(defclass client ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (auth :initarg :auth :accessor auth)))

(defmethod client-send (self (arguments list))
  (with-slots (host port auth) self
    (usocket:with-client-socket (sock stream host port :timeout 10 :element-type 'character)
      (let ((msg (jso
                  "auth" auth
                  "args" arguments
                  "dir" (uiop:unix-namestring (uiop:getcwd))
                  "makeflags" (or (uiop:getenv "MAKEFLAGS") ""))))
        (when *debug*
          (format uiop:*stderr* "~&DBG: ~s~%" msg))
        (write-json msg stream)
        (finish-output stream)
        (usocket:socket-shutdown sock :output))
      (loop for form = (handler-case (read-json stream)
                         (json-eof-error () nil))
            while form
            collect form))))

(defun make-client ()
  (multiple-value-bind (host port auth) (read-server-file)
    (make-instance 'client :host host :port port :auth auth)))

(defun client-entry-point (&key ((:argv arguments)
                                 (uiop:command-line-arguments))
                           &aux (stdout uiop:*stdout*)
                                (stderr uiop:*stderr*))
  (handler-case
      (progn
        (assert (every #'stringp arguments))
        (when (intersection '("-V" "--version") arguments :test #'equal)
          (format stderr "~&Overlord client version ~a~%"
                  (asdf:system-version
                   (asdf:find-system "overlord-cli"))))
        (when (find "--debug" arguments :test #'equal)
          (setf *debug* t))
        (let* ((client (make-client))
               (forms (client-send client arguments)))
          (dolist (form forms)
            (when *debug*
              (format stderr "~&DBG: ~s~%" form))
            (destructuring-bind (key data) form
              (cond
                ((equal key "status") (uiop:quit data))
                ((equal key "out") (write-string data stdout))
                ((equal key "err") (write-string data stderr))
                (t (error "Bad message from server: ~a" form)))))))
    (serious-condition (e)
      (princ e stderr)
      (uiop:quit 1))))

(defun save-client (filename)
  "Write the client to FILENAME, as an executable.
If FILENAME exists, it is overwritten.

Under SBCL, uses compression when available."
  (setf filename (uiop:merge-pathnames* filename (user-homedir-pathname)))
  (when (uiop:os-windows-p)
    (setf filename (uiop:merge-pathnames* (make-pathname :type "exe") filename)))
  (setf uiop:*image-entry-point* #'client-entry-point)
  (uiop:delete-file-if-exists filename)
  (multiple-value-call #'uiop:dump-image
    filename
    :allow-other-keys t
    :executable t
    :purify t
    #+sb-core-compression (values :compression 9))
  (format uiop:*stderr* "Client saved to ~a~%" filename)
  (finish-output)
  (uiop:quit 0))

(defpackage #:overlord-cli/client
  (:use #:cl)
  (:export #:save-client))
(in-package #:overlord-cli/client)

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

(defun read-server-file ()
  (with-open-file (in *server-file*
                      :direction :input
                      :element-type 'character)
    (let* ((len (file-length in))
           (str (make-string len)))
      (read-sequence str in)
      (destructuring-bind (host port auth)
          (tokens str)
        (values host (parse-integer port) auth)))))

(defclass client ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (auth :initarg :auth :accessor auth)))

(defmethod client-send (self (arguments list))
  (with-slots (host port auth) self
    (handler-case
        (usocket:with-client-socket (sock stream host port :timeout 10)
          (write (list :auth auth
                       :args arguments
                       ;; NB No reader macros!
                       :dir (uiop:unix-namestring (uiop:getcwd)))
                 :stream stream
                 :pretty nil
                 :readably t)
          (force-output stream)
          (destructuring-bind (status out err)
              (handler-case
                  (read stream nil '(1 "" ""))
                (serious-condition () '(1 "" "")))
            (values status out err)))
      (usocket:timeout-error ()
        (values 1 "" "Connection attempt timed out -- is server running?")))))

(defun make-client ()
  (multiple-value-bind (host port auth) (read-server-file)
    (make-instance 'client :host host :port port :auth auth)))

(defun client-entry-point (&aux (stdout uiop:*stdout*)
                                (stderr uiop:*stderr*)
                                (arguments (uiop:command-line-arguments)))
  (assert (every #'stringp arguments))
  (when (intersection '("-v" "--version") arguments :test #'equal)
    (format stderr "Overlord client version ~a" (asdf:system-version "overlord-cli")))
  (#-sbcl progn
   #+sbcl sb-sys:without-gcing
   (let ((client
           (handler-case
               (make-client)
             (file-error ()
               (princ "No server is running." stderr)
               (uiop:quit 2)))))
     (multiple-value-bind (status out err)
         (client-send client arguments)
       (check-type status integer)
       (write-string out stdout)
       (write-string err stderr)
       (uiop:quit status)))))

(defun save-client (filename)
  (setf filename (uiop:unix-namestring filename))
  (setf filename (uiop:merge-pathnames* filename (user-homedir-pathname)))
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

(defpackage :overlord-cli/client
  (:use :cl)
  (:export :save-client))
(in-package :overlord-cli/client)

(defvar *server-file*
  (uiop:xdg-cache-home "overlord-cli/server"))

(defun read-server-file ()
  (with-open-file (in *server-file*
                      :direction :input
                      :element-type 'character)
    (let ((*package* :keyword))
      (values (string-downcase (read in))
              (parse-integer (string (read in)))
              (string-downcase (read in))))))

(defclass client ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (auth :initarg :auth :accessor auth)))

(defmethod client-send ((self client) (arguments list))
  (with-slots (host port auth) self
    (handler-case
        (usocket:with-client-socket (sock stream host port :timeout 10)
          (write (cons auth arguments)
                 :stream stream
                 :readably t)
          (force-output stream)
          (destructuring-bind (status out err)
              (read stream nil '(1 "" ""))
            (values status out err)))
      (usocket:timeout-error ()
        (values 1 "" "Connection attempt timed out -- is server running?")))))

(defun make-client ()
  (multiple-value-bind (host port auth)
      (read-server-file)
    (make-instance 'client
                   :host host
                   :port port
                   :auth auth)))

(defun client-entry-point (&aux (stdout uiop:*stdout*)
                                (stderr uiop:*stderr*)
                                (arguments (uiop:command-line-arguments)))
  (assert (every #'stringp arguments))
  (when (equal (uiop:command-line-arguments) "--version")
    (format uiop:*stderr* "Overlord client ~a"
            (asdf:system-version "overlord-cli"))
    (force-output uiop:*stderr*)
    (uiop:quit 0))
  ;; TODO How do you do this in Clozure?
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
  (setf filename
        (make-pathname :name filename
                       :defaults (user-homedir-pathname)))
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

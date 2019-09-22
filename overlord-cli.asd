;;;; overlord-cli.asd

(defsystem "overlord-cli"
  :description "Describe overlord-cli here"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("overlord"
               "usocket"
               "lparallel"
               "named-readtables"
               "overlord/safer-read"
               "ironclad" "cl-base64"
               "unix-opts")
  :components ((:file "package")
               (:file "server")))

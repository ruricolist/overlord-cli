;;;; overlord-cli.asd

(defsystem "overlord-cli"
  :description "Implementation of the server."
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
  :components ((:file "server")))

(defsystem "overlord-cli/client"
  :description "Implementation of the client, with minimal dependencies."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("usocket")
  :components ((:file "client")))

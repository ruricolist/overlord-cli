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
               "st-json"
               "ironclad" "cl-base64"
               "command-line-arguments"
               "trivial-gray-streams"
               "exit-hooks")
  :components ((:file "server")))

(defsystem "overlord-cli/client"
  :description "Implementation of the client, with minimal dependencies."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("usocket" "st-json")
  :components ((:file "client"))
  :build-operation program-op
  :build-pathname "overlord"
  :entry-point "overlord-cli/client:client-entry-point")

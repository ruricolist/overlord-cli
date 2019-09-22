;;;; package.lisp

(defpackage #:overlord-cli
  (:use #:cl #:alexandria #:serapeum #:overlord)
  (:import-from #:trivia #:match #:ematch #:plist #:property)
  (:import-from #:overlord/safer-read #:safer-read)
  (:export
   #:save-client
   #:start-server
   #:stop-server))

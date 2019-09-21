;;;; package.lisp

(defpackage #:overlord-cli
  (:use #:cl #:alexandria #:serapeum #:overlord)
  (:import-from #:trivia #:match #:ematch)
  (:import-from #:overlord/safer-read #:safer-read)
  (:export
   #:save-client
   #:start-server
   #:stop-server))

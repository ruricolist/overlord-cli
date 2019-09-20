;;;; package.lisp

(defpackage #:overlord-cli
  (:use #:cl #:alexandria #:serapeum #:overlord)
  (:import-from #:trivia #:match #:ematch)
  (:export
   #:save-client
   #:start-server
   #:stop-server))

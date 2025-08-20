;;;; test/package.lisp

(defpackage #:com.danielkeogh.json-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:jz #:com.inuoe.jzon)
                    (#:json #:com.danielkeogh.json))
  (:export #:run!
           #:all-tests))

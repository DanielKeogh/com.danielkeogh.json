;;;; com.danielkeogh.json.asd

(asdf:defsystem #:com.danielkeogh.json
  :description "JSON schemas for Rest services and openapi documentation."
  :author "Daniel Keogh"
  :license  "MIT"
  :serial t
  :depends-on (#:com.inuoe.jzon #:cl-ppcre)
  :components ((:file "dates")
               (:file "types")
               (:file "json")))

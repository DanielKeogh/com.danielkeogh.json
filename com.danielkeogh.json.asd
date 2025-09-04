;;;; com.danielkeogh.json.asd

(asdf:defsystem #:com.danielkeogh.json
  :description "JSON schemas for Rest services and openapi documentation."
  :author "Daniel Keogh"
  :license  "MIT"
  :serial t
  :depends-on (#:alexandria
               #:cl-ppcre
               #:com.inuoe.jzon
               #:local-time)
  :components ((:file "dates")
               (:file "types")
               (:file "json")))

;;;; com.danielkeogh.json-tests.asdf

(asdf:defsystem #:com.danielkeogh.json-tests
  :description "Tests for :com.danielkeogh.json"
  :author "Daniel Keogh"
  :license  "MIT"
  :depends-on (#:com.danielkeogh.json #:fiveam #:com.inuoe.jzon)
  :components ((:module "tests"
		            :serial t
		            :components ((:file "main")))))

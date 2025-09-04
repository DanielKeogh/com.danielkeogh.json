;;;; test/main.lisp

(defpackage #:com.danielkeogh.json-tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:jz #:com.inuoe.jzon)
                    (#:json #:com.danielkeogh.json))
  (:export #:run!
           #:all-tests))

(in-package #:com.danielkeogh.json-tests)

(def-suite all-tests
  :description "Main tests for com.danielkeogh.json")

(in-suite all-tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character
   #\# #\H
   (lambda (stream sub-char arg)
     (declare (ignore sub-char arg))
     (let ((contents (read stream t nil t)))
       (let ((ht (make-hash-table :test #'equal)))
         (loop for (k v) on contents by #'cddr
               do (setf (gethash k ht) v))
         ht)))))

(test schema-value-nil
  (let ((schema (json:define-schema '(:value))))
    ;; parse
    (is (equal "happy" (json:parse-string schema "\"happy\"")))
    (is (equal 25 (json:parse-string schema "25")))
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    ;; write
    (is (equal "\"foo\"" (json:write-string schema "foo")))
    (is (equal "24" (json:write-string schema 24)))
    (signals json:schema-mismatch-error (json:write-string schema '(1 2 3)))))

(test schema-value-number
  (let ((schema (json:define-schema '(:value :type "number"))))
    ;; parse
    (is (equal 55 (json:parse-string schema "55")))
    (is (equal -55 (json:parse-string schema "-55")))
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    (is (equal 9999999999999999999999999999999999999999999
               (json:parse-string schema "9999999999999999999999999999999999999999999")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))
    ;; write
    (is (equal "55" (json:write-string schema 55)))
    (is (equal "0.8" (json:write-string schema 4/5)))
    (is (equal "0.8" (json:write-string schema 0.8)))
    (signals json:schema-mismatch-error (json:write-string schema "Hiya"))))

(test schema-value-number-format-float
  (let ((schema (json:define-schema '(:value :type "number" :format "float"))))
    ;; parse
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    (signals json:schema-mismatch-error (json:parse-string schema "55"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))
    ;; write
    (is (equal "2.5" (json:write-string schema 2.5)))
    (is (equal "2.5" (json:write-string schema 2.5d0)))
    (signals json:schema-mismatch-error (json:write-string schema 5))))

(test schema-value-number-format-double
  (let ((schema (json:define-schema '(:value :type "number" :format "double"))))
    ;; parse
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    (signals json:schema-mismatch-error (json:parse-string schema "55"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))
    ;; write
    (is (equal "2.5" (json:write-string schema 2.5d0)))
    (signals json:schema-mismatch-error (json:write-string schema 2.5))
    (signals json:schema-mismatch-error (json:write-string schema 5))))

(test schema-value-integer
  (let ((schema (json:define-schema '(:value :type "integer"))))
    ;; parse
    (is (equal 2 (json:parse-string schema "2")))
    (signals json:schema-mismatch-error (json:parse-string schema "55.5"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))
    ;; write
    (is (equal "2" (json:write-string schema 2)))
    (is (equal "-2" (json:write-string schema -2)))
    (signals json:schema-mismatch-error (json:write-string schema "\"happy\""))
    (signals json:schema-mismatch-error (json:write-string schema 9/10))))

(test schema-value-integer-format-int32
  (let ((schema (json:define-schema '(:value :type "integer" :format "int32"))))
    ;; parse
    (is (equal 2 (json:parse-string schema "2")))
    (signals json:schema-mismatch-error (json:parse-string schema "99999999999999999999999999999999999999999"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    ;; write
    (is (equal "2" (json:write-string schema 2)))
    (is (equal "99999" (json:write-string schema 99999)))
    (signals json:schema-mismatch-error  (json:write-string schema "2"))))

(test schema-value-integer-format-int64
  (let ((schema (json:define-schema '(:value :type "integer" :format "int32"))))
    (is (equal 2 (json:parse-string schema "2")))
    (signals json:schema-mismatch-error (json:parse-string schema "99999999999999999999999999999999999999999"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    ;; write
    (is (equal "2" (json:write-string schema 2)))
    (is (equal "99999" (json:write-string schema 99999)))
    (signals json:schema-mismatch-error  (json:write-string schema "2"))))

(test schema-value-string
  (let ((schema (json:define-schema '(:value :type "string"))))
    ;; parse
    (is (equal "happy" (json:parse-string schema "\"happy\"")))
    (is (equal "" (json:parse-string schema "\"\"")))
    (signals json:schema-mismatch-error (json:parse-string schema "55"))
    ;; write
    (is (equal "\"happy\"" (json:write-string schema "happy")))
    (is (equal   "\"\"" (json:write-string schema "")))
    (signals json:schema-mismatch-error (json:write-string schema 55))))

(test schema-value-string-format-date
  (let ((schema (json:define-schema '(:value :type "string" :format "date"))))
    ;; parse
    (is (local-time:timestamp=
         (local-time:encode-timestamp 0 0 0 0 28 2 2019)
         (json:parse-string schema "\"2019-02-28\"")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"2019-02-31\""))
    (signals json:schema-mismatch-error (json:parse-string schema  "\"2025-08-13T14:30:00.123+02:01\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"abracadabra\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    ;; write
    (is (equal "\"2019-02-28\""
               (json:write-string schema (local-time:encode-timestamp 0 0 0 0 28 2 2019))))
    (signals json:schema-mismatch-error (json:write-string schema (local-time:encode-timestamp 0 1 0 0 28 2 2019)))))

(test schema-value-string-format-date-time
  (let ((schema (json:define-schema '(:value :type "string" :format "date-time"))))
    ;; parse
    (is (local-time:timestamp=
         (local-time:encode-timestamp 123000000 0 30 14 13 8 2025 :offset (* 121 60))
         (json:parse-string schema "\"2025-08-13T14:30:00.123+02:01\"")))
    (is (local-time:timestamp=
         (local-time:encode-timestamp 123000000 0 30 14 13 8 2025 :offset (* -121 60))
         (json:parse-string schema "\"2025-08-13T14:30:00.123-02:01\"")))
    (is (local-time:timestamp=
         (local-time:encode-timestamp 123000000 0 30 14 13 8 2025 :offset 0)
         (json:parse-string schema "\"2025-08-13T14:30:00.123Z\"")))
    (is (local-time:timestamp=
         (local-time:encode-timestamp 123000000 0 30 14 13 8 2025 :offset 0)
         (json:parse-string schema "\"2025-08-13t14:30:00.123z\"")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"2019-02-31\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"abracadabra\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    ;; write
    (is (equal "\"2019-02-28T00:00:00.000000Z\""
               (json:write-string schema
                                  (local-time:encode-timestamp 0 0 0 0 28 2 2019
                                                               :timezone local-time:+utc-zone+))))
    (is (equal "\"2019-02-28T11:11:19.123000Z\""
               (json:write-string schema
                                  (local-time:encode-timestamp 123000000 19 11 11 28 2 2019
                                                               :timezone local-time:+utc-zone+))))))

(test schema-value-boolean
  (let ((schema (json:define-schema '(:value :type "boolean"))))
    ;; parse
    (is (equal t (json:parse-string schema "true")))
    (is (null (json:parse-string schema "false")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"japan\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    ;; write
    (is (equal "true" (json:write-string schema t)))
    (is (equal "false" (json:write-string schema nil)))
    (signals json:schema-mismatch-error (json:write-string schema 0))
    (signals json:schema-mismatch-error (json:write-string schema ""))))

(test schema-array
  (let ((schema (json:define-schema '(:array :values (:value)))))
    ;; parse
    (is (equalp (vector 1 2 3) (json:parse-string schema "[1, 2, 3]")))
    (is (equalp (vector 1 "2" 3) (json:parse-string schema "[1, \"2\", 3]")))
    ;; write
    (is (equal "[1,2,3]" (json:write-string schema (vector 1 2 3))))
    (is (equal "[1,2,3]" (json:write-string schema (list 1 2 3))))
    (is (equal "[1,\"2\",3]" (json:write-string schema (vector 1 "2" 3))))
    (is (equal "[]" (json:write-string schema nil)))
    (is (equal "[]" (json:write-string schema (vector))))
    (signals json:schema-mismatch-error (json:write-string schema "abcde"))
    (signals json:schema-mismatch-error (json:write-string schema 44)))

  (let ((schema (json:define-schema '(:array :values (:value :type "integer")))))
    ;; parse
    (is (equalp (vector 1 2 3) (json:parse-string schema "[1, 2, 3]")))
    (signals json:schema-mismatch-error (json:parse-string schema "[1, \"2\", 3]"))
    ;; write
    (is (equal "[1,2,3]"  (json:write-string schema (vector 1 2 3))))
    (is (equal "[]" (json:write-string schema (vector))))
    (signals json:schema-mismatch-error (json:write-string schema (vector 1 "2" 3)))
    (signals json:schema-mismatch-error (json:write-string schema (list 1 "2" 3)))))

(defstruct test-struct
  field1
  field2)

(defun getf-equal (plist key &optional default)
  (loop for (k v) on plist by #'cddr
        when (equal k key)
          return v
        finally (return default)))

(test schema-object
  (let ((schema (json:define-schema '(:object :properties ((:key "foo" :value (:value)))))))
    ;; parse
    (is (equalp #H("foo" "bar") (json:parse-string schema "{\"foo\": \"bar\"}")))
    (is (equalp #H("foo" 25) (json:parse-string schema "{\"foo\": 25}")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    (signals json:schema-mismatch-error (json:parse-string schema  "{\"foo\": {\"eshay\": \"\"}}"))
    ;; write
    (is (equal "{\"foo\":\"bar\"}" (json:write-string schema #H("foo" "bar"))))
    (is (equal "{\"foo\":25}" (json:write-string schema #H("foo" 25))))
    (signals json:schema-mismatch-error (json:write-string schema ""))
    (signals json:schema-mismatch-error (json:write-string schema #("baz" "xyz"))))

  (let ((schema (json:define-schema `(:object
                                      :get-object ,(lambda () (list 1))
                                      :properties ((:key "foo"
                                                    :setter ,(lambda (obj val)
                                                               (nconc obj (list "foo" val)))
                                                    :getter ,(lambda (obj)
                                                               (getf-equal (cdr obj) "foo"))
                                                    :value (:value)))))))
    ;; parse
    (is (equalp '(1 "foo" "bar") (json:parse-string schema "{\"foo\": \"bar\"}")))
    (is (equalp '(1 "foo" 25) (json:parse-string schema "{\"foo\": 25}")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    (signals json:schema-mismatch-error (json:parse-string schema  "{\"foo\": {\"eshay\": \"\"}}"))
    ;; write
    (is (equal "{\"foo\":\"bar\"}" (json:write-string schema '(1 "foo" "bar"))))
    (is (equal "{\"foo\":25}" (json:write-string schema '(1 "foo" 25))))
    (signals json:schema-mismatch-error (json:write-string schema "")))

  (let ((schema (json:define-schema `(:object
                                      :get-object ,(lambda () (make-test-struct))
                                      :properties ((:key "foo"
                                                    :accessor test-struct-field1
                                                    :value (:value)))))))
    ;; parse
    (is (equalp (make-test-struct :field1 "bar") (json:parse-string schema "{\"foo\": \"bar\"}")))
    (is (equalp (make-test-struct :field1 25) (json:parse-string schema "{\"foo\": 25}")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    (signals json:schema-mismatch-error (json:parse-string schema  "{\"foo\": {\"eshay\": \"\"}}"))
    ;; write
    (is (equal "{\"foo\":\"bar\"}" (json:write-string schema (make-test-struct :field1 "bar"))))
    (is (equal  "{\"foo\":25}" (json:write-string schema (make-test-struct :field1 25))))
    (signals json:schema-mismatch-error (json:write-string schema ""))))

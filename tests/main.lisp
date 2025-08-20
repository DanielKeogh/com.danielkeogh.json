;;;; test/main.lisp

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
    (is (equal "happy" (json:parse-string schema "\"happy\"")))
    (is (equal 25 (json:parse-string schema "25")))
    (is (equal 2.5d0 (json:parse-string schema "2.5")))))

(test schema-value-number
  (let ((schema (json:define-schema '(:value :type "number"))))
    (is (equal 55 (json:parse-string schema "55")))
    (is (equal -55 (json:parse-string schema "-55")))
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    (is (equal 9999999999999999999999999999999999999999999
               (json:parse-string schema "9999999999999999999999999999999999999999999")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))))

(test schema-value-number-format-float
  (let ((schema (json:define-schema '(:value :type "number" :format "float"))))
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    (signals json:schema-mismatch-error (json:parse-string schema "55"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))))

(test schema-value-number-format-double
  (let ((schema (json:define-schema '(:value :type "number" :format "double"))))
    (is (equal 2.5d0 (json:parse-string schema "2.5")))
    (signals json:schema-mismatch-error (json:parse-string schema "55"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))))

(test schema-value-integer
  (let ((schema (json:define-schema '(:value :type "integer"))))
    (is (equal 2 (json:parse-string schema "2")))
    (signals json:schema-mismatch-error (json:parse-string schema "55.5"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"happy\""))))

(test schema-value-integer-format-int32
  (let ((schema (json:define-schema '(:value :type "integer" :format "int32"))))
    (is (equal 2 (json:parse-string schema "2")))
    (signals json:schema-mismatch-error (json:parse-string schema "99999999999999999999999999999999999999999"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))))

(test schema-value-integer-format-int64
  (let ((schema (json:define-schema '(:value :type "integer" :format "int32"))))
    (is (equal 2 (json:parse-string schema "2")))
    (signals json:schema-mismatch-error (json:parse-string schema "99999999999999999999999999999999999999999"))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))))

(test schema-value-string
  (let ((schema (json:define-schema '(:value :type "string"))))
    (is (equal "happy" (json:parse-string schema "\"happy\"")))
    (is (equal "" (json:parse-string schema "\"\"")))
    (signals json:schema-mismatch-error (json:parse-string schema "55"))))

(test schema-value-string-format-date
  (let ((schema (json:define-schema '(:value :type "string" :format "date"))))
    (is (equal "2019-02-28" (json:parse-string schema "\"2019-02-28\"")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"2019-02-31\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"abracadabra\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))))

(test schema-value-string-format-date-time
  (let ((schema (json:define-schema '(:value :type "string" :format "date-time"))))
    (is (equal "2025-08-13T14:30:00.123+02:01" (json:parse-string schema "\"2025-08-13T14:30:00.123+02:01\"")))
    (is (equal "2025-08-13T14:30:00.123-02:01" (json:parse-string schema "\"2025-08-13T14:30:00.123-02:01\"")))
    (is (equal "2025-08-13T14:30:00.123Z" (json:parse-string schema "\"2025-08-13T14:30:00.123Z\"")))
    (is (equal "2025-08-13t14:30:00.123z" (json:parse-string schema "\"2025-08-13t14:30:00.123z\"")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"2019-02-31\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"abracadabra\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))))

(test schema-value-boolean
  (let ((schema (json:define-schema '(:value :type "boolean"))))
    (is (equal t (json:parse-string schema "true")))
    (is (null (json:parse-string schema "false")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"japan\""))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))))

(test schema-array
  (let ((schema (json:define-schema '(:array :values (:value)))))
    (is (equalp (vector 1 2 3) (json:parse-string schema "[1, 2, 3]")))
    (is (equalp (vector 1 "2" 3) (json:parse-string schema "[1, \"2\", 3]"))))

  (let ((schema (json:define-schema '(:array :values (:value :type "integer")))))
    (is (equalp (vector 1 2 3) (json:parse-string schema "[1, 2, 3]")))
    (signals json:schema-mismatch-error (json:parse-string schema "[1, \"2\", 3]"))))

(test schema-object
  (let ((schema (json:define-schema '(:object :properties ((:key "foo" :value (:value)))))))
    (is (equalp #H("foo" "bar") (json:parse-string schema "{\"foo\": \"bar\"}")))
    (is (equalp #H("foo" 25) (json:parse-string schema "{\"foo\": 25}")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    (signals json:schema-mismatch-error (json:parse-string schema  "{\"foo\": {\"eshay\": \"\"}}")))

  (let ((schema (json:define-schema `(:object
                                      :get-object ,(lambda () (list 1))
                                      :properties ((:key "foo"
                                                    :setter ,(lambda (obj val)
                                                               (nconc obj (list "foo" val)))
                                                    :value (:value)))))))
    (is (equalp '(1 "foo" "bar") (json:parse-string schema "{\"foo\": \"bar\"}")))
    (is (equalp '(1 "foo" 25) (json:parse-string schema "{\"foo\": 25}")))
    (signals json:schema-mismatch-error (json:parse-string schema "\"\""))
    (signals json:schema-mismatch-error (json:parse-string schema  "{\"foo\": {\"eshay\": \"\"}}"))))

;;;; types.lisp

(defpackage #:com.danielkeogh.json.types
  (:use #:cl)
  (:local-nicknames (#:dates #:com.danielkeogh.json.dates))
  (:export
   #:def-simple-type
   #:get-validator-fn
   #:type-exists-p
   #:value-valid-p))

(in-package #:com.danielkeogh.json.types)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *json-simple-types* (make-hash-table :test 'equal))

  (defun def-simple-type (name format format-p)
    (let ((format-hash
            (or (gethash name *json-simple-types*)
                (setf (gethash name *json-simple-types*) (make-hash-table :test 'equal)))))
      (setf (gethash format format-hash) format-p))))

(defun get-validator-fn (type format)
  (let ((formats (gethash type *json-simple-types*)))
    (gethash format formats)))

(defun type-exists-p (type format)
  (get-validator-fn type format))

(defun value-valid-p (value type format)
  (funcall (get-validator-fn type format) value))

;;; Type definitions
(def-simple-type nil nil (lambda (v) (declare (ignore v)) t))
(def-simple-type "number" nil #'numberp)
(def-simple-type "number" "float" #'floatp)
(def-simple-type "number" "double" (lambda (v) (typep v 'double-float)))
(def-simple-type "integer" nil #'integerp)
(def-simple-type "integer" "int32" (lambda (v) (and (integerp v) (<= #.(ash -1 31) v) (<= v #. (1- (ash 1 32))))))
(def-simple-type "integer" "int64" (lambda (v) (and (integerp v) (<= #.(ash -1 63) v) (<= v #. (1- (ash 1 64))))))
(def-simple-type "string" nil #'stringp)
(def-simple-type "string" "date" #'dates:parse-rfc3339-date)
(def-simple-type "string" "date-time" #'dates:parse-rfc3339-date-time)
(def-simple-type "string" "password" #'stringp)
(def-simple-type "string" "byte" #'stringp)
(def-simple-type "string" "binary" #'stringp)
(def-simple-type "boolean" nil (lambda (v) (or (eq v t) (null v))))


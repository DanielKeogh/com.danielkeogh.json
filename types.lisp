;;;; types.lisp

(defpackage #:com.danielkeogh.json.types
  (:use #:cl)
  (:local-nicknames (#:dates #:com.danielkeogh.json.dates))
  (:export
   #:def-simple-type
   #:type-exists-p
   #:cl-value-valid-p
   #:json-value-valid-p
   #:use-validator-fn-result-p))

(in-package #:com.danielkeogh.json.types)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (simple-type-descriptor (:conc-name st-))
    (name nil :type (or null string) :read-only t)
    (format nil :type (or null string) :read-only t)
    (json-validator-fn nil :type (function (t)) :read-only t)
    (cl-validator-fn nil :type (function (t)) :read-only t)
    (use-validator-fn-result nil :type boolean :read-only t))

  (defvar *json-simple-types* (make-hash-table :test 'equal))

  (defun def-simple-type (name format format-p
                          &key use-p-result cl-format-p)
    (let ((format-hash
            (or (gethash name *json-simple-types*)
                (setf (gethash name *json-simple-types*) (make-hash-table :test 'equal))))
          (descriptor (make-simple-type-descriptor :name name
                                                   :format format
                                                   :json-validator-fn format-p
                                                   :cl-validator-fn (or cl-format-p format-p)
                                                   :use-validator-fn-result use-p-result)))
      (setf (gethash format format-hash) descriptor))))

(defun type-exists-p (type format)
  (let ((formats (gethash type *json-simple-types*)))
    (gethash format formats)))

(defun get-cl-validator-fn (type format)
  (st-cl-validator-fn (type-exists-p type format)))

(defun get-json-validator-fn (type format)
  (st-json-validator-fn (type-exists-p type format)))

(defun json-value-valid-p (value type format)
  (funcall (get-json-validator-fn type format) value))

(defun cl-value-valid-p (value type format)
  (funcall (get-cl-validator-fn type format) value))

(defun use-validator-fn-result-p (type format)
  (st-use-validator-fn-result (type-exists-p type format)))

;;; Type definitions
(def-simple-type nil nil (lambda (v)
                           (or (numberp v)
                               (stringp v)
                               (characterp v)
                               (symbolp v))))
(def-simple-type "number" nil #'numberp)
(def-simple-type "number" "float" #'floatp)
(def-simple-type "number" "double" (lambda (v) (typep v 'double-float)))
(def-simple-type "integer" nil #'integerp)
(def-simple-type "integer" "int32" (lambda (v) (and (integerp v) (<= #.(ash -1 31) v) (<= v #.(1- (ash 1 32))))))
(def-simple-type "integer" "int64" (lambda (v) (and (integerp v) (<= #.(ash -1 63) v) (<= v #.(1- (ash 1 64))))))
(def-simple-type "string" nil #'stringp)
(def-simple-type "string" "date" #'dates:parse-rfc3339-date
  :use-p-result t
  :cl-format-p (lambda (v) (when (typep v 'local-time:timestamp)
                             (multiple-value-bind (ns sec min hour day month year)
                                 (local-time:decode-timestamp v)
                               (unless (< 0 (+ ns sec min hour))
                                 (format nil "~4,'0D-~2,'0D-~2,'0D" year month day))))))

(def-simple-type "string" "date-time" #'dates:parse-rfc3339-date-time
  :use-p-result t
  :cl-format-p (lambda (v) (when (typep v 'local-time:timestamp)
                             (local-time:format-rfc3339-timestring nil v :timezone local-time:+utc-zone+))))

(def-simple-type "string" "password" #'stringp)
(def-simple-type "string" "byte" #'stringp)
(def-simple-type "string" "binary" #'stringp)
(def-simple-type "boolean" nil (lambda (v) (or (eq v t) (null v))))

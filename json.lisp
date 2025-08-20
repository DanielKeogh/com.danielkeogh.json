;;;; json.lisp

(defpackage #:com.danielkeogh.json
  (:use #:cl)
  (:local-nicknames (#:jz :com.inuoe.jzon)
                    (#:types :com.danielkeogh.json.types))
  (:export
   #:define-schema
   #:parse
   #:parse-string
   ;; error types
   #:malformed-schema-error
   #:invalid-value-error
   #:schema-mismatch-error))

(in-package #:com.danielkeogh.json)

;;; schema definition structs

(defstruct (json-schema-definition (:conc-name jsd-))
  (root nil)
  (named-components nil))

(defstruct (json-array-definition (:conc-name jad-))
  (values nil))

(defstruct (json-value-definition (:conc-name jvd-))
  (type nil)
  (format nil)
  (parse-fn nil))

(defstruct (json-object-property (:conc-name jop-))
  (key nil)
  (value nil)
  (setter-fn nil))

(defstruct (json-object-definition (:conc-name jod-))
  (make-object-fn nil)
  (properties (make-hash-table :test 'equal)))

(defstruct (json-any-definition (:conc-name jac-)))

;;; Error types

(define-condition malformed-schema-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c stream)
             (format stream "Malformed schema error: ~A" (error-message c))))
  (:documentation "Error for improperly defined schema syntax in define-schema."))

(define-condition invalid-value-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c stream)
             (format stream "Invalid value error: ~A" (error-message c))))
  (:documentation "Error for when json values fail validation."))

(define-condition schema-mismatch-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c stream)
             (format stream "Schema mismatch error: ~A" (error-message c))))
  (:documentation "Error for when json structure does not match schema."))

;;; Schema builder

(defun define-schema (body)
  (labels
      ((assert-equal (actual expected)
         (unless (equalp expected actual)
           (error "Expected ~a, but was ~a" expected actual)))

       (define-next (schema)
         (let ((type (car schema)))
           (case type
             (:value (define-value schema))
             (:object (define-object schema))
             (:array (define-array schema))
             (nil (define-any))
             (t (error 'malformed-schema-error :message (format nil "Expected :value :object or :array but was ~a" type))))))

       (define-any ()
         (make-json-any-definition))

       (define-value (schema)
         (assert-equal (car schema) :value)
         (let* ((schema (cdr schema))
                (type (getf schema :type))
                (format (getf schema :format))
                (parse-fn (getf schema :parser)))
           (unless (types:type-exists-p type format)
             (error 'malformed-schema-error :message (format nil "Type and format combination ~a ~a does not exist. To define new types use com.danielkeogh.json.types:def-simple-type." type format))
             )
           (make-json-value-definition
            :type type
            :format format
            :parse-fn parse-fn)))
       
       (define-object (schema)
         (assert-equal (car schema) :object)
         (loop with schema = (cdr schema)
               with make-object-fn = (getf schema :get-object) 
               with result = (make-json-object-definition :make-object-fn make-object-fn)
               with props-table = (jod-properties result)
               for property-def in (getf schema :properties)

               for key = (getf property-def :key)
               for value-def = (getf property-def :value)
               for setter = (getf property-def :setter)
               for value = (define-next value-def)
               for def = (make-json-object-property :key key :value value :setter-fn setter)
               do (setf (gethash key props-table) def)
               finally (return result)))

       (define-array (schema)
         (assert-equal (car schema) :array)
         (let* ((values-schema (getf (cdr schema) :values))
                (values-def (define-next values-schema)))
           (make-json-array-definition :values values-def))))
    (make-json-schema-definition :root (define-next body))))

(defun parse (schema-definition stream)
  (jz:with-parser (parser stream)
    (labels
        ((assert-schema (schema type value)
           (unless (typep schema type)
             (error 'schema-mismatch-error :message (format nil "Schema expect ~a but found ~a" schema value))))

         (parse-next (schema type val)
           (case type
             (:value (parse-value schema val))
             (:begin-object (parse-object schema))
             (:begin-array (parse-array schema))
             (nil nil)
             (t (error 'schema-mismatch-error :message (format nil "Unexpected token pair ~a" (list type val))))))

         (parse-value (schema value)
           (assert-schema schema 'json-value-definition value)
           (let* ((type (jvd-type schema))
                  (format (jvd-format schema))
                  (parse-fn (jvd-parse-fn schema)))
             (unless (types:value-valid-p value type format)
               (if (null format)
                   (error 'schema-mismatch-error
                          :message (format nil "Expected a ~a but value was ~a" type value))
                   (error 'schema-mismatch-error
                          :message (format nil "Expected a ~a of format ~a but value was ~a" type format value))))

             (if parse-fn
                 (funcall parse-fn value)
                 value)))
         
         (parse-object (schema)
           (assert-schema schema 'json-object-definition "an object")
           (let* ((make-object-fn (jod-make-object-fn schema))
                  (object (if make-object-fn (funcall make-object-fn)
                              (make-hash-table :test 'equal)))
                 (properties (jod-properties schema)))
             (loop for (key-sym key) = (multiple-value-list (jz:parse-next parser))
                   until (eq :end-object key-sym)
                   do
                      (progn
                        (unless (eq key-sym :object-key)
                          (error 'malformed-schema-error :message "Expecting objects to be key-value pairs"))
                        (let ((property (gethash key properties)))
                          (unless property
                            (error 'malformed-schema-error :message (format nil "Key ~a is not recognised as a part of the schema ~a" key schema)))
                          (let* ((val-schema (jop-value property)))
                            (multiple-value-bind (next-key next-val) (jz:parse-next parser)
                              (let ((val (parse-next val-schema next-key next-val))
                                    (setter (jop-setter-fn property)))
                                (if setter
                                    (funcall setter object val)
                                    (setf (gethash key object) val))))))))
             object))

         (parse-array (schema)
           (assert-schema schema 'json-array-definition "an object")
           (loop for (next-type next-val) = (multiple-value-list (jz:parse-next parser))
                 until (eq next-type :end-array)
                 collect (parse-next (jad-values schema) next-type next-val) into r
                 finally (return (coerce r 'vector)))))

      (let ((root-schema (jsd-root schema-definition)))
        (multiple-value-bind (type val) (jz:parse-next parser)
          (parse-next root-schema type val))))))

(defun parse-string (schema string)
  (with-input-from-string (stream string)
    (parse schema stream)))

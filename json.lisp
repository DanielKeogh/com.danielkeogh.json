;;;; json.lisp

(defpackage #:com.danielkeogh.json
  (:use #:cl)
  (:local-nicknames (#:jz :com.inuoe.jzon)
                    (#:types :com.danielkeogh.json.types))
  (:shadow #:write #:write-string)
  (:export
   #:define-schema
   #:parse
   #:parse-string
   #:write
   #:write-string
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
  (setter-fn nil)
  (getter-fn nil))

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
           (if type
               (case type
                 (:value (define-value schema))
                 (:object (define-object schema))
                 (:array (define-array schema))
                 (t (error 'malformed-schema-error
                           :message (format nil "Expected :value :object or :array but was ~a" type))))
               (define-any))))

       (define-any ()
         (make-json-any-definition))

       (define-value (schema)
         (assert-equal (car schema) :value)
         (let* ((schema (cdr schema))
                (type (getf schema :type))
                (format (getf schema :format))
                (parse-fn (getf schema :parser)))
           (unless (types:type-exists-p type format)
             (error 'malformed-schema-error
                    :message (format nil "Type and format combination ~a ~a does not exist. To define new types use com.danielkeogh.json.types:def-simple-type." type format))
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
               for accessor = (getf property-def :accessor)
               for getter = (getf property-def :getter)
               for setter = (getf property-def :setter)
               for value = (define-next value-def)
               for setter-fn = (if accessor
                                   (eval (alexandria:with-gensyms (obj val)
                                           `(lambda (,obj ,val) (setf (,accessor ,obj) ,val))))
                                   (or setter
                                       (lambda (obj val) (setf (gethash key obj) val))))
               for getter-fn = (or accessor
                                   getter
                                   (lambda (obj) (gethash key obj)))
               for def = (make-json-object-property :key key :value value :setter-fn setter-fn :getter-fn getter-fn)
               do (when (and accessor (or getter setter))
                    (error 'malformed-schema-error
                           :message (format nil "Should not have both :ACCESSOR and :GETTER or :SETTER defined on :OBJECT")))

                  (setf (gethash key props-table) def)
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
             (error 'schema-mismatch-error
                    :message (format nil "Schema expect ~a but found ~a" schema value))))

         (parse-next (schema type val)
           (case type
             (:value (parse-value schema val))
             (:begin-object (parse-object schema))
             (:begin-array (parse-array schema))
             (nil nil)
             (t (error 'schema-mismatch-error
                       :message (format nil "Unexpected token pair ~a" (list type val))))))

         (parse-value (schema val)
           (assert-schema schema 'json-value-definition val)
           (let* ((type (jvd-type schema))
                  (format (jvd-format schema))
                  (parse-fn (jvd-parse-fn schema))
                  (validation-result (types:json-value-valid-p val type format))
                  (value (if (types:use-validator-fn-result-p type format)
                             validation-result
                             val)))
             (unless validation-result
               (if (null format)
                   (error 'schema-mismatch-error
                          :message (format nil "Expected a ~a but value was ~a" type val))
                   (error 'schema-mismatch-error
                          :message (format nil "Expected a ~a of format ~a but value was ~a" type format val))))

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
                          (error 'malformed-schema-error
                                 :message "Expecting objects to be key-value pairs"))
                        (let ((property (gethash key properties)))
                          (unless property
                            (error 'malformed-schema-error
                                   :message (format nil "Key ~a is not recognised as a part of the schema ~a" key schema)))
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

(defun write (schema obj stream)
  (jz:with-writer* (:stream stream)
    (labels ((assert-type (val type)
               (unless (typep val type)
                 (error 'schema-mismatch-error
                        :message (format nil "Expected ~a but was ~a" type val))))

             (write-any (obj)
               (jz:write-value* obj))

             (write-next (schema obj)
               (case (type-of schema)
                 (json-value-definition (write-value schema obj))
                 (json-object-definition (write-object schema obj))
                 (json-array-definition (write-array schema obj))
                 (json-any-definition (write-any obj))
                 (t (error 'schema-mismatch-error
                           :message (format nil "Unexpected schema type: ~a" (type-of schema))))))

             (write-value (schema val)
               (let* ((type (jvd-type schema))
                      (format (jvd-format schema))
                      (validation-result (types:cl-value-valid-p val type format))
                      (value (if (types:use-validator-fn-result-p type format)
                                 validation-result
                                 val)))
                 (unless validation-result
                   (error 'schema-mismatch-error
                          :message (format nil "Value ~a does not match schema of type ~a and format ~a" obj type format)))
                 (jz:write-value* value)))

             (write-object (schema obj)
               (jz:with-object*
                 (loop for property-def being the hash-values of (jod-properties schema)
                       for key = (jop-key property-def)
                       for getter = (jop-getter-fn property-def)
                       for val = (handler-case (funcall getter obj)
                                   (error (c)
                                     (error 'schema-mismatch-error
                                            :message (format nil "Property ~a on object ~a getter failed with error ~a"
                                                             key obj c))))
                       do
                          (jz:write-key* key)
                          (write-next (jop-value property-def) val))))

             (write-array (schema seq)
               (assert-type seq 'sequence)
               (when (stringp seq) (error 'schema-mismatch-error
                                          :message (format nil "Expected array not a string, but found ~a" seq)))
               (jz:with-array*
                 (map nil (lambda (v)
                            (write-next (jad-values schema) v))
                      seq))))
      (write-next (jsd-root schema) obj))))

(defun parse-string (schema string)
  (with-input-from-string (stream string)
    (parse schema stream)))

(defun write-string (schema obj)
  (with-output-to-string (stream)
    (write schema obj stream)))

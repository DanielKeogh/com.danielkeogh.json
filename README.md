# com.danielkeogh.json

This library tries to take the pain out of defining and documenting your json API by defining a declarative schema. Provided is a comprehensive toolbox including:

* A declarative syntax for defining API's.
<!-- TODO * [OpenAPI](https://www.openapis.org/) doc generation. -->
<!-- TODO * Server-side parser generation. -->
<!-- TODO * Validation to defend against malicious requests. -->

It is designed with performance in-mind, utilizing `:com.inuoe.jzon` for parsing, and pre-building internal representations of your schema to avoid unecessary computation when serialising into new objects.

## Packages

* `com.danielkeogh.json` - declaritively define a json parser using dsls.
* `com.danielkeogh.json.dates` - contains utilities for verifying strings are dates.
* `com.danielkeogh.json.types` - defines all type and format pairs from [OpenApi 3.0](https://swagger.io/docs/specification/v3_0/data-models/data-types/) as well as providing functions for defining new types.

## Quick start

### 1) Define (or extend) simple types

Simple types are `(type, format) -> predicate` entries.
Built-ins include `number (double/float), integer (int32/int64), string (date/date-time/password/byte/binary), boolean`

Add your own:

```lisp
(com.danielkeogh.json.types:def-simple-type "string" "uuid"
  (lambda (s)
    (and (stringp s)
         (let ((re "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"))
           (cl-ppcre:scan re s)))))
```

### 2) Build a schema

Use `define-schema` with a small DSL:

* `(:value :type "..." [:format "..."] [:parser fn])`
* `(:object :get-object fn :properties (list ...))`

  * Each property: `(:key "..." :value <schema> [:setter fn])`
* `(:array :values <schema>)`

```lisp
(in-package #:com.danielkeogh.json)

(defparameter *user-schema*
  (define-schema
    `(:object
      :get-object ,(lambda () (make-hash-table :test 'equal)) ; default if omitted
      :properties
      ((:key "name"
        :value (:value :type "string"))
       (:key "age"
        :value (:value :type "integer" :format "int32"))
       (:key "tags"
        :value (:array :values (:value :type "string")))
       (:key "joined_at"
        :value (:value :type "string" :format "date-time")
        ;; On parse, put value into table under "joined_at"
        ;; (omit :setter to have default put into hash-table)
        )))))
```

### 3) Parse JSON

```lisp
(parse-string *user-schema*
  "{\"name\":\"Ada\",\"age\":37,\"tags\":[\"admin\",\"ops\"],\"joined_at\":\"2024-06-01T12:00:00Z\"}")
;; => #<HASH-TABLE ...> with:
;;    "name"      => "Ada"
;;    "age"       => 37
;;    "tags"      => #("admin" "ops")   ; arrays become vectors
;;    "joined_at" => <as parsed/validated>
```

Or from a stream:

```lisp
(with-open-file (in "user.json")
  (parse *user-schema* in))
```

---

## Parsing details

* **Objects**

  * Default container: `hash-table` (test `equal`).
  * Provide `:get-object` to return a pre-made object (struct/instance/plist/etc.).
  * For each property, you can provide a `:setter` of `(lambda (object value) ...)`.
    If omitted and the object is a hash-table, the key is stored as `gethash`.

* **Arrays**

  * Parsed into a **vector**. Element schema is applied to each item.

* **Values**

  * Validated via `(types:value-valid-p value type format)`.
  * Optional `:parser` hook `(lambda (value) ...)` can transform values *after* validation.

    * Example: use `:parser` to convert an RFC3339 string into a timestamp object, while keeping format validation separate.

---

## Error handling

* `malformed-schema-error`
  Raised when the **schema** is invalid or doesn’t match allowed constructs (e.g., unknown property keys, wrong top-level tags).
* `schema-mismatch-error`
  Raised when **JSON input** does not satisfy the schema (wrong type/format, unexpected structure).

## Schema DSL reference

* **Value**

  ```lisp
  (:value :type "string" :format "date-time" :parser #'my-parser)
  ```
* **Object**

  ```lisp
  (:object
   :get-object (lambda () (make-instance 'user))
   :properties
   ((:key "name"
     :value (:value :type "string")
     :setter (lambda (u v) (setf (slot-value u 'name) v)))
    (:key "tags"
     :value (:array :values (:value :type "string")))))
  ```
* **Array**

  ```lisp
  (:array :values (:value :type "integer" :format "int32"))
  ```

## Minimal end-to-end example

```lisp
(in-package #:cl-user)
(use-package '(:com.danielkeogh.json :com.danielkeogh.json.types))

(defparameter *schema*
  (define-schema
    '(:object
      :properties
      ((:key "id"    :value (:value :type "integer" :format "int64"))
       (:key "email" :value (:value :type "string"))
       (:key "tags"  :value (:array :values (:value :type "string")))))))

(parse-string *schema*
  "{\"id\":123,\"email\":\"a@b.com\",\"tags\":[\"news\",\"pro\"]}")
;; => #<HASH-TABLE ...>
```

---

## License

MIT

;;;; dates.lisp

(defpackage #:com.danielkeogh.json.dates
  (:use #:cl)
  (:export
   #:parse-rfc3339-date
   #:parse-rfc3339-date-time))

(in-package #:com.danielkeogh.json.dates)

(defun days-in-month-valid-p (year month day)
  (let ((days-in-month (case month
                         ((1 3 5 7 8 10 12) 31)
                         ((4 6 9 11) 30)
                         (2 (if (and (zerop (mod year 4))
                                     (or (not (zerop (mod year 400)))
                                         (zerop (mod year 400))))
                                29
                                28)))))
    (<= 1 day days-in-month)))

(let ((date-time-scanner (ppcre:create-scanner
"^(\\d{4})-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])[Tt]([01][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9])(?:\\.([0-9]+))?([Zz]|([+-])([01][0-9]|2[0-3]):([0-5][0-9]))$")))
  (defun parse-rfc3339-date-time (date-time-string)
    "Parse an RFC 3339 date-time string into a universal time.
Returns NIL if the string is invalid."
    (multiple-value-bind (start end starts ends)
        (ppcre:scan date-time-scanner date-time-string)
      (declare (ignore end))
      (when start
        (labels ((parse-int-slot (slot)
                   (parse-integer date-time-string :start (aref starts slot) :end (aref ends slot))))
          (let* ((year (parse-int-slot 0))
                 (month (parse-int-slot 1))
                 (day (parse-int-slot 2))
                 (hour (parse-int-slot 3))
                 (minute (parse-int-slot 4))
                 (second (parse-int-slot 5))
                 (frac (if (aref starts 6) (parse-int-slot 6) 0))
                 (tz (when (aref starts 7)
                            (subseq date-time-string (aref starts 7) (aref ends 7))))
                 (tz-sign (when (aref starts 8)
                            (subseq date-time-string (aref starts 8) (aref ends 8))))
                 (tz-hours (when (aref starts 9) (parse-int-slot 9)))
                 (tz-minutes (when (aref starts 10) (parse-int-slot 10)))
                 (offset (if (string-equal "z" tz)
                             0
                             (let ((sign (if (string= "+" tz-sign) 1 -1)))
                               (* sign (+ tz-hours (/ tz-minutes 60)))))))
            (when (days-in-month-valid-p year month day)
              (values (encode-universal-time second minute hour day month year offset) frac))))))))

(let ((date-scanner (ppcre:create-scanner
                     "^(\\d{4})-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])$")))
  (defun parse-rfc3339-date (date-string)
    "Return an encoded utc datetime if DATE-STRING is a valid RFC 3339 full-date (YYYY-MM-DD)."
    (multiple-value-bind (start end starts ends)
        (ppcre:scan date-scanner date-string)
      (declare (ignore end))
      (when start
        (let* ((y (parse-integer date-string :start (aref starts 0) :end (aref ends 0)))
               (m (parse-integer date-string :start (aref starts 1) :end (aref ends 1)))
               (d (parse-integer date-string :start (aref starts 2) :end (aref ends 2))))
          (when (days-in-month-valid-p y m d)
            (encode-universal-time 0 0 0 d m y)))))))

;;; notyet/rrule.scm --- expand recurring events (a practical RRULE subset)
;;;
;;; Supported: FREQ=DAILY/WEEKLY/MONTHLY/YEARLY, INTERVAL, COUNT, UNTIL,
;;; BYDAY (weekly: weekdays; monthly: weekdays with or without ordinal such
;;; as 2TU or -1FR), BYMONTHDAY (monthly/yearly), BYMONTH (yearly), plus
;;; EXDATE in the caller.  Anything else (BYSETPOS, BYWEEKNO, hourly, ...)
;;; is reported as unsupported; the caller then shows only the first
;;; occurrence and says so.

(define-module (notyet rrule)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (notyet time)
  #:export (parse-rrule rrule-supported? expand-rrule describe-rrule))

(define (parse-rrule s)
  "Alist of (KEY . value-string) for an RRULE value."
  (filter-map (lambda (part)
                (match (string-split part #\=)
                  ((k v) (cons (string-upcase (string-trim-both k))
                               (string-trim-both v)))
                  (_ #f)))
              (string-split s #\;)))

(define %supported-keys
  '("FREQ" "INTERVAL" "COUNT" "UNTIL" "BYDAY" "BYMONTHDAY" "BYMONTH" "WKST"))

(define (rrule-supported? rule)
  (and (member (assoc-ref rule "FREQ") '("DAILY" "WEEKLY" "MONTHLY" "YEARLY"))
       (every (lambda (kv) (member (car kv) %supported-keys)) rule)))

(define %weekdays '(("SU" . 0) ("MO" . 1) ("TU" . 2) ("WE" . 3)
                    ("TH" . 4) ("FR" . 5) ("SA" . 6)))

(define (parse-byday s)
  "List of (ordinal-or-#f . weekday) for a BYDAY value like MO,2TU,-1FR."
  (filter-map (lambda (x)
                (let ((m (string-match "^([+-]?[0-9]+)?([A-Z]{2})$"
                                       (string-upcase (string-trim-both x)))))
                  (and m (assoc (match:substring m 2) %weekdays)
                       (cons (and=> (match:substring m 1) string->number)
                             (assoc-ref %weekdays (match:substring m 2))))))
              (string-split s #\,)))

(define (numbers s) (filter-map string->number (string-split s #\,)))

(define (nth-weekday-of-month y m ord wd)
  "Day of month of the ORD-th weekday WD (negative counts from the end)."
  (let* ((dim (days-in-month y m))
         (days (filter (lambda (d)
                         (= wd (parts-wday
                                (instant->parts (local->instant y m d 12 0 0)))))
                       (iota dim 1))))
    (cond ((and (> ord 0) (<= ord (length days))) (list-ref days (- ord 1)))
          ((and (< ord 0) (<= (- ord) (length days)))
           (list-ref days (+ (length days) ord)))
          (else #f))))

(define* (expand-rrule rrule-string dtstart duration window-start window-end
                       #:key (tz (%time-zone)) (exdates '()) (limit 5000))
  "Start instants of the occurrences of a series starting at DTSTART that
overlap [WINDOW-START, WINDOW-END), excluding EXDATES.  DTSTART's wall-clock
time is kept in zone TZ across DST changes.  Returns #f if the rule is not
supported."
  (let ((rule (parse-rrule rrule-string)))
    (and
     (rrule-supported? rule)
     (let* ((freq (assoc-ref rule "FREQ"))
            (interval (or (and=> (assoc-ref rule "INTERVAL") string->number) 1))
            (count (and=> (assoc-ref rule "COUNT") string->number))
            (until (and=> (assoc-ref rule "UNTIL")
                          (lambda (u) (call-with-values
                                          (lambda () (ical-time->instant u))
                                        (lambda (t date?)
                                          ;; A date-only UNTIL includes that day
                                          (if date? (+ t 86399) t))))))
            (byday (and=> (assoc-ref rule "BYDAY") parse-byday))
            (bymonthday (and=> (assoc-ref rule "BYMONTHDAY") numbers))
            (bymonth (and=> (assoc-ref rule "BYMONTH") numbers))
            (p (instant->parts dtstart tz))
            (y0 (list-ref p 0)) (m0 (list-ref p 1)) (d0 (list-ref p 2))
            (h (list-ref p 3)) (mi (list-ref p 4)) (s (list-ref p 5))
            (wd0 (list-ref p 6)))
       (define (at y m d) (local->instant y m d h mi s tz))
       (define (valid y m d) (and (>= d 1) (<= d (days-in-month y m)) (at y m d)))
       (define (month-days y m)
         ;; candidate days of one month for MONTHLY/YEARLY
         (sort
          (delete-duplicates
           (cond
            (bymonthday
             (filter-map (lambda (n)
                           (let ((d (if (< n 0) (+ (days-in-month y m) n 1) n)))
                             (and (>= d 1) (<= d (days-in-month y m)) d)))
                         bymonthday))
            (byday
             (append-map
              (match-lambda
                ((#f . wd)
                 (filter (lambda (d)
                           (= wd (parts-wday
                                  (instant->parts (local->instant y m d 12 0 0)))))
                         (iota (days-in-month y m) 1)))
                ((ord . wd)
                 (let ((d (nth-weekday-of-month y m ord wd))) (if d (list d) '()))))
              byday))
            (else (if (<= d0 (days-in-month y m)) (list d0) '()))))
          <))
       (define (period-candidates k)
         ;; occurrences in the k-th period, chronological
         (match freq
           ("DAILY"
            (let ((t (add-days dtstart (* k interval) tz))) (list t)))
           ("WEEKLY"
            (let* ((monday (add-days (local->instant y0 m0 d0 h mi s tz)
                                     (- (modulo (+ wd0 6) 7)) tz))
                   (week (add-days monday (* 7 k interval) tz))
                   (wds (if byday (map cdr byday) (list wd0))))
              (sort (map (lambda (wd) (add-days week (modulo (+ wd 6) 7) tz))
                         (delete-duplicates wds))
                    <)))
           ("MONTHLY"
            (let* ((m-idx (+ (- m0 1) (* k interval)))
                   (y (+ y0 (floor-quotient m-idx 12)))
                   (m (+ 1 (floor-remainder m-idx 12))))
              (filter-map (lambda (d) (valid y m d)) (month-days y m))))
           ("YEARLY"
            (let ((y (+ y0 (* k interval))))
              (sort
               (append-map
                (lambda (m)
                  (filter-map (lambda (d) (valid y m d))
                              (if (or bymonthday byday) (month-days y m)
                                  (list d0))))
                (or bymonth (list m0)))
               <)))))
       (let loop ((k 0) (n 0) (out '()) (steps 0))
         (if (> steps limit)
             (reverse out)
             (let walk ((cands (period-candidates k)) (n n) (out out))
               (match cands
                 (() (loop (+ k 1) n out (+ steps 1)))
                 ((t . rest)
                  (cond
                   ((< t dtstart) (walk rest n out))
                   ((or (and count (>= n count)) (and until (> t until))
                        (>= t window-end))
                    (reverse out))
                   (else
                    (walk rest (+ n 1)
                          (if (and (> (+ t (max duration 1)) window-start)
                                   (not (memv t exdates)))
                              (cons t out)
                              out)))))))))))))

(define (describe-rrule rrule-string)
  "Short human description, e.g. \"weekly on Mon, Wed until 2026-12-01\"."
  (let* ((rule (parse-rrule rrule-string))
         (freq (or (assoc-ref rule "FREQ") "?"))
         (interval (or (and=> (assoc-ref rule "INTERVAL") string->number) 1))
         (names #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
    (string-append
     (if (= interval 1)
         (string-downcase freq)
         (format #f "every ~a ~a" interval
                 (match freq ("DAILY" "days") ("WEEKLY" "weeks")
                        ("MONTHLY" "months") ("YEARLY" "years") (_ freq))))
     (or (and=> (assoc-ref rule "BYDAY")
                (lambda (b)
                  (string-append
                   " on "
                   (string-join
                    (map (match-lambda
                           ((#f . wd) (vector-ref names wd))
                           ((o . wd) (format #f "~a ~a" o (vector-ref names wd))))
                         (parse-byday b))
                    ", "))))
         "")
     (or (and=> (assoc-ref rule "COUNT")
                (lambda (c) (string-append ", " c " times")))
         "")
     (or (and=> (assoc-ref rule "UNTIL")
                (lambda (u) (string-append " until "
                                           (format-date (ical-time->instant u)))))
         "")
     (if (rrule-supported? rule) "" " (not fully supported)"))))

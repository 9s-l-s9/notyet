;;; notyet/time.scm --- instants, time zones, human date input
;;;
;;; Instants are POSIX seconds.  Wall-clock arithmetic goes through libc
;;; (mktime/localtime with an explicit zone), so DST is handled by tzdata.

(define-module (notyet time)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (%time-zone %now
            local->instant instant->parts parts-wday
            day-start add-days add-months days-in-month
            ical-time->instant ical-utc ical-date ical-local
            valid-zone known-zone
            parse-when parse-duration parse-date-word
            format-date format-time format-wday format-iso format-duration
            week-start month-start))

(define %time-zone (make-parameter "Europe/Berlin"))
(define %now (make-parameter #f))       ; override for tests

(define (now) (or (%now) (current-time)))

(define %zone-dirs
  (delay
    (let ((home (or (getenv "HOME") "")))
      (filter (lambda (d) (and d (file-exists? d)))
              (list (getenv "TZDIR") "/usr/share/zoneinfo"
                    (string-append home "/.guix-home/profile/share/zoneinfo")
                    (string-append home "/.guix-profile/share/zoneinfo")
                    "/run/current-system/profile/share/zoneinfo")))))

(define %zone-cache (make-hash-table))

(define (zone tz)
  "TZ as libc understands it: the absolute zoneinfo path when the named zone
is found (Guix's libc ignores TZDIR), otherwise TZ unchanged."
  (or (hash-ref %zone-cache tz)
      (let ((spec (if (or (string-prefix? "/" tz) (member tz '("UTC" "GMT")))
                      tz
                      (or (any (lambda (d)
                                 (let ((f (string-append d "/" tz)))
                                   (and (file-exists? f) f)))
                               (force %zone-dirs))
                          tz))))
        (hash-set! %zone-cache tz spec)
        spec)))

;;; Wall clock

(define (make-tm y mo d h mi s)
  (let ((tm (localtime 0 "UTC")))
    (set-tm:year tm (- y 1900)) (set-tm:mon tm (- mo 1)) (set-tm:mday tm d)
    (set-tm:hour tm h) (set-tm:min tm mi) (set-tm:sec tm s)
    (set-tm:isdst tm -1)
    tm))

(define* (local->instant y mo d #:optional (h 0) (mi 0) (s 0)
                         (tz (%time-zone)))
  "Instant of wall-clock time Y-MO-D H:MI:S in zone TZ.  Out-of-range
fields are normalized (day 32 is the 1st of the next month)."
  (car (mktime (make-tm y mo d h mi s) (zone tz))))

(define* (instant->parts t #:optional (tz (%time-zone)))
  "List (year month day hour minute second weekday) of instant T in TZ;
weekday 0 = Sunday."
  (let ((tm (localtime t (zone tz))))
    (list (+ 1900 (tm:year tm)) (+ 1 (tm:mon tm)) (tm:mday tm)
          (tm:hour tm) (tm:min tm) (tm:sec tm) (tm:wday tm))))

(define (parts-wday parts) (list-ref parts 6))

(define* (day-start t #:optional (tz (%time-zone)))
  (match (instant->parts t tz)
    ((y mo d . _) (local->instant y mo d 0 0 0 tz))))

(define* (add-days t n #:optional (tz (%time-zone)))
  "T moved by N calendar days, keeping the wall-clock time."
  (match (instant->parts t tz)
    ((y mo d h mi s _) (local->instant y mo (+ d n) h mi s tz))))

(define (leap? y) (and (zero? (modulo y 4))
                       (or (not (zero? (modulo y 100))) (zero? (modulo y 400)))))

(define (days-in-month y m)
  (case m
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (else (if (leap? y) 29 28))))

(define* (add-months t n #:optional (tz (%time-zone)))
  (match (instant->parts t tz)
    ((y mo d h mi s _)
     (let* ((m0 (+ (- mo 1) n))
            (y2 (+ y (floor-quotient m0 12)))
            (m2 (+ 1 (floor-remainder m0 12))))
       (local->instant y2 m2 (min d (days-in-month y2 m2)) h mi s tz)))))

(define* (week-start t #:optional (tz (%time-zone)))
  "Monday 00:00 of the week containing T."
  (let ((wd (parts-wday (instant->parts t tz))))
    (add-days (day-start t tz) (- (modulo (+ wd 6) 7)) tz)))

(define* (month-start t #:optional (tz (%time-zone)))
  (match (instant->parts t tz)
    ((y mo . _) (local->instant y mo 1 0 0 0 tz))))

;;; Zones

(define %windows-zones
  '(("W. Europe Standard Time" . "Europe/Berlin")
    ("Central Europe Standard Time" . "Europe/Budapest")
    ("Romance Standard Time" . "Europe/Paris")
    ("Central European Standard Time" . "Europe/Warsaw")
    ("GMT Standard Time" . "Europe/London")
    ("Greenwich Standard Time" . "Atlantic/Reykjavik")
    ("FLE Standard Time" . "Europe/Kiev")
    ("GTB Standard Time" . "Europe/Bucharest")
    ("Eastern Standard Time" . "America/New_York")
    ("Central Standard Time" . "America/Chicago")
    ("Mountain Standard Time" . "America/Denver")
    ("Pacific Standard Time" . "America/Los_Angeles")
    ("India Standard Time" . "Asia/Kolkata")
    ("China Standard Time" . "Asia/Shanghai")
    ("Tokyo Standard Time" . "Asia/Tokyo")
    ("AUS Eastern Standard Time" . "Australia/Sydney")
    ("Singapore Standard Time" . "Asia/Singapore")
    ("UTC" . "UTC")))

(define (known-zone tzid)
  "Map a TZID to an IANA zone name, or #f if it is not recognizable."
  (and tzid
       (let ((tzid (string-trim-both tzid)))
         (cond
          ((assoc tzid %windows-zones) => cdr)
          ((member tzid '("UTC" "GMT" "Etc/UTC" "Z")) "UTC")
          ;; Some producers prefix a vendor path: /mozilla.org/.../Europe/Berlin
          ((string-match "([A-Z][A-Za-z_]+/[A-Za-z_]+(/[A-Za-z_]+)?)$" tzid)
           => (lambda (m) (match:substring m 1)))
          (else #f)))))

(define (valid-zone tzid)
  (or (known-zone tzid) (%time-zone)))

;;; iCalendar values

(define (digits s start end) (string->number (substring s start end)))

(define* (ical-time->instant value #:optional tzid)
  "Parse an iCalendar DATE or DATE-TIME.  Returns two values: the instant and
whether VALUE was a date only (all-day).  Dates are midnight in the user's
zone; floating times use the user's zone too."
  (let ((v (string-trim-both value)))
    (cond
     ((= (string-length v) 8)
      (values (local->instant (digits v 0 4) (digits v 4 6) (digits v 6 8)
                              0 0 0 (%time-zone))
              #t))
     ((>= (string-length v) 15)
      (let ((y (digits v 0 4)) (mo (digits v 4 6)) (d (digits v 6 8))
            (h (digits v 9 11)) (mi (digits v 11 13)) (s (digits v 13 15)))
        (values (local->instant y mo d h mi s
                                (if (string-suffix? "Z" v) "UTC" (valid-zone tzid)))
                #f)))
     (else (error "bad iCalendar date" value)))))

(define (ical-utc t) (strftime "%Y%m%dT%H%M%SZ" (gmtime t)))
(define* (ical-date t #:optional (tz (%time-zone)))
  (strftime "%Y%m%d" (localtime t (zone tz))))
(define (ical-local t tz) (strftime "%Y%m%dT%H%M%S" (localtime t (zone tz))))

;;; Formatting

(define* (format-date t #:optional (tz (%time-zone)))
  (strftime "%Y-%m-%d" (localtime t (zone tz))))
(define* (format-time t #:optional (tz (%time-zone)))
  (strftime "%H:%M" (localtime t (zone tz))))
(define %wday-names #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define* (format-wday t #:optional (tz (%time-zone)))
  (vector-ref %wday-names (parts-wday (instant->parts t tz))))
(define* (format-iso t #:optional (tz (%time-zone)))
  (let ((s (strftime "%Y-%m-%dT%H:%M:%S%z" (localtime t (zone tz)))))
    ;; +0100 -> +01:00
    (string-append (substring s 0 (- (string-length s) 2)) ":"
                   (substring s (- (string-length s) 2)))))

(define (format-duration secs)
  "Compact duration: 1d, 2h30m, 15m."
  (let* ((neg (< secs 0)) (s (abs secs))
         (w (quotient s 604800)) (d (quotient (remainder s 604800) 86400))
         (h (quotient (remainder s 86400) 3600))
         (m (quotient (remainder s 3600) 60)))
    (let ((out (string-append
                (if (and (> w 0) (= d 0) (= h 0) (= m 0)) (format #f "~aw" w) "")
                (if (and (> w 0) (or (> d 0) (> h 0) (> m 0)))
                    (format #f "~ad" (+ (* 7 w) d))
                    (if (> d 0) (format #f "~ad" d) ""))
                (if (> h 0) (format #f "~ah" h) "")
                (if (> m 0) (format #f "~am" m) ""))))
      (string-append (if neg "-" "")
                     (if (string-null? out) "0m" out)))))

;;; Human input

(define %day-words
  '(("today" . 0) ("heute" . 0) ("tomorrow" . 1) ("morgen" . 1)
    ("yesterday" . -1) ("gestern" . -1) ("übermorgen" . 2)
    ("uebermorgen" . 2) ("vorgestern" . -2)))

(define %weekday-words
  ;; libc weekday numbers, 0 = Sunday
  '(("sunday" . 0) ("sun" . 0) ("su" . 0) ("sonntag" . 0) ("so" . 0)
    ("monday" . 1) ("mon" . 1) ("mo" . 1) ("montag" . 1)
    ("tuesday" . 2) ("tue" . 2) ("tu" . 2) ("dienstag" . 2) ("di" . 2)
    ("wednesday" . 3) ("wed" . 3) ("we" . 3) ("mittwoch" . 3) ("mi" . 3)
    ("thursday" . 4) ("thu" . 4) ("th" . 4) ("donnerstag" . 4) ("do" . 4)
    ("friday" . 5) ("fri" . 5) ("fr" . 5) ("freitag" . 5)
    ("saturday" . 6) ("sat" . 6) ("sa" . 6) ("samstag" . 6)))

(define* (parse-date-word w #:optional base)
  "Midnight of the day named by W, or #f.  Accepts 2026-11-10, 10.11.2026,
10.11., today/heute, tomorrow/morgen, weekday names (next such day,
today included), +3d, -1w.  BASE (an instant) anchors relative words."
  (let* ((tz (%time-zone))
         (today (day-start (or base (now)) tz))
         (w (string-downcase (string-trim-both w))))
    (cond
     ((string-match "^([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})$" w)
      => (lambda (m) (local->instant (string->number (match:substring m 1))
                                     (string->number (match:substring m 2))
                                     (string->number (match:substring m 3))
                                     0 0 0 tz)))
     ((string-match "^([0-9]{1,2})\\.([0-9]{1,2})\\.([0-9]{4})?$" w)
      => (lambda (m)
           (let* ((d (string->number (match:substring m 1)))
                  (mo (string->number (match:substring m 2)))
                  (y (if (match:substring m 3)
                         (string->number (match:substring m 3))
                         (car (instant->parts today tz))))
                  (t (local->instant y mo d 0 0 0 tz)))
             ;; 10.11. without a year means the next such date
             (if (and (not (match:substring m 3)) (< t today))
                 (local->instant (+ y 1) mo d 0 0 0 tz)
                 t))))
     ((assoc w %day-words) => (lambda (kv) (add-days today (cdr kv) tz)))
     ((assoc w %weekday-words)
      => (lambda (kv)
           (let ((wd (parts-wday (instant->parts today tz))))
             (add-days today (modulo (- (cdr kv) wd) 7) tz))))
     ((string-match "^([+-])([0-9]+)([dwm])$" w)
      => (lambda (m)
           (let ((n (* (if (string=? (match:substring m 1) "-") -1 1)
                       (string->number (match:substring m 2)))))
             (match (match:substring m 3)
               ("d" (add-days today n tz))
               ("w" (add-days today (* 7 n) tz))
               ("m" (add-months today n tz))))))
     (else #f))))

(define (parse-clock w)
  "Seconds after midnight for 14:00, 9:30, 14h, 14.30 Uhr; or #f."
  (let ((w (string-downcase (string-trim-both w))))
    (cond
     ((string-match "^([0-9]{1,2})[:.]([0-9]{2})(h|uhr)?$" w)
      => (lambda (m)
           (let ((h (string->number (match:substring m 1)))
                 (mi (string->number (match:substring m 2))))
             (and (< h 24) (< mi 60) (+ (* 3600 h) (* 60 mi))))))
     ((string-match "^([0-9]{1,2})(h|uhr)$" w)
      => (lambda (m)
           (let ((h (string->number (match:substring m 1))))
             (and (< h 24) (* 3600 h)))))
     (else #f))))

(define* (parse-when str #:optional base)
  "Parse a human point in time.  Returns two values: the instant and whether
a clock time was given (#f means a whole day).  A clock time alone refers to
the day of BASE (default today).  Returns #f, #f if STR is not understood."
  (let* ((tz (%time-zone))
         (str (regexp-substitute/global #f "([0-9])T([0-9])" str
                                        'pre 1 " " 2 'post))
         (words (remove string-null?
                        (string-split (string-trim-both str) #\space))))
    (let loop ((ws words) (day #f) (clock #f))
      (match ws
        (()
         (cond
          ((and (not day) (not clock)) (values #f #f))
          (else
           (let ((d (or day (day-start (or base (now)) tz))))
             (if clock
                 (match (instant->parts d tz)
                   ((y mo dd . _)
                    (values (local->instant y mo dd (quotient clock 3600)
                                            (quotient (remainder clock 3600) 60)
                                            0 tz)
                            #t)))
                 (values d #f))))))
        ((w . rest)
         (cond
          ((and (not clock) (parse-clock w)) => (lambda (c) (loop rest day c)))
          ((member (string-downcase w) '("um" "at" "uhr")) (loop rest day clock))
          ((and (not day) (parse-date-word w base))
           => (lambda (d) (loop rest d clock)))
          (else (values #f #f))))))))

(define (parse-duration str)
  "Seconds for 90m, 1h, 1h30m, 2d, 1w, 45min, 1:30; or #f."
  (let ((s (string-downcase (string-trim-both str))))
    (cond
     ((string-match "^([0-9]+):([0-9]{2})$" s)
      => (lambda (m) (+ (* 3600 (string->number (match:substring m 1)))
                        (* 60 (string->number (match:substring m 2))))))
     ((string-match "^([+-])?(([0-9]+(w|d|h|min|m|s))+)$" s)
      => (lambda (m)
           (let ((sign (if (equal? (match:substring m 1) "-") -1 1)))
             (* sign
                (fold (lambda (part acc)
                        (let ((n (string->number (match:substring part 1)))
                              (u (match:substring part 2)))
                          (+ acc (* n (match u
                                        ("w" 604800) ("d" 86400) ("h" 3600)
                                        ((or "m" "min") 60) ("s" 1))))))
                      0
                      (list-matches "([0-9]+)(w|d|h|min|m|s)"
                                    (match:substring m 2)))))))
     (else #f))))

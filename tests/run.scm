;;; tests/run.scm --- notyet test suite
;;;
;;;   guile -L . tests/run.scm

(use-modules (srfi srfi-64)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-11)
             (ice-9 match)
             (notyet ical)
             (notyet time)
             (notyet rrule)
             (notyet store)
             (notyet query)
             (notyet config))

(define tz "Europe/Berlin")
(%time-zone tz)
(%now (local->instant 2026 11 5 12 0 0 tz))   ; Thursday

(define (iso t) (format-iso t tz))

(test-begin "notyet")

;;; ical

(define sample
  (string-append
   "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\n"
   "UID:a@b\r\n"
   "SUMMARY:Gespräch\\, wichtig\\; sehr\r\n"
   "ATTENDEE;CN=\"Doe, Jane\";ROLE=REQ-PARTICIPANT:mailto:jane@x.org\r\n"
   "DESCRIPTION:line one\\nline two that is long enough to be folded somewh\r\n"
   " ere in the middle\r\n"
   "CATEGORIES:Fokus,Bewerbung\\, Acme\r\n"
   "DTSTART;TZID=Europe/Berlin:20261110T140000\r\n"
   "DTEND;TZID=Europe/Berlin:20261110T150000\r\n"
   "X-CUSTOM;FOO=bar:kept\r\n"
   "BEGIN:VALARM\r\nACTION:EMAIL\r\nTRIGGER;RELATED=START:-PT15M\r\nEND:VALARM\r\n"
   "END:VEVENT\r\nEND:VCALENDAR\r\n"))

(define parsed (parse-ical sample))
(define vevent (car (component-children (car parsed))))

(test-equal "text unescape" "Gespräch, wichtig; sehr" (component-text vevent "SUMMARY"))
(test-equal "folded line joined"
  "line one\nline two that is long enough to be folded somewhere in the middle"
  (component-text vevent "DESCRIPTION"))
(test-equal "quoted param keeps comma" "Doe, Jane"
  (prop-param (component-prop vevent "ATTENDEE") "CN"))
(test-equal "categories split on unescaped commas" '("Fokus" "Bewerbung, Acme")
  (split-unescaped-commas (component-ref vevent "CATEGORIES")))
(test-equal "roundtrip is stable" (serialize-ical parsed) (serialize-ical (parse-ical (serialize-ical parsed))))
(test-assert "x-property survives" (string-contains (serialize-ical parsed) "X-CUSTOM;FOO=bar:kept"))
(test-assert "long lines are folded to 75 octets"
  (every (lambda (l) (<= (bytevector-length (string->utf8 (string-trim-right l #\return))) 75))
         (string-split (serialize-ical (list (make-component "X" (list (make-prop "DESCRIPTION" '() (make-string 300 #\ä))) '())))
                       #\newline)))
(test-equal "escape/unescape" "a,b;c\\d\ne" (ical-unescape (ical-escape "a,b;c\\d\ne")))

;;; time

(test-equal "TZID time" "2026-11-10T14:00:00+01:00"
  (iso (ical-time->instant "20261110T140000" "Europe/Berlin")))
(test-equal "UTC time" "2026-11-10T15:00:00+01:00" (iso (ical-time->instant "20261110T140000Z")))
(test-equal "Windows zone name" "2026-07-10T14:00:00+02:00"
  (iso (ical-time->instant "20260710T140000" "W. Europe Standard Time")))
(test-equal "summer time" "2026-07-01T09:00:00+02:00"
  (iso (local->instant 2026 7 1 9 0 0 tz)))
(test-equal "add-days across DST keeps wall clock" "2026-10-26T09:00:00+01:00"
  (iso (add-days (local->instant 2026 10 24 9 0 0 tz) 2)))

(define (when-iso s) (call-with-values (lambda () (parse-when s)) (lambda (t c) (and t (list (iso t) c)))))
(test-equal "morgen 14:00" '("2026-11-06T14:00:00+01:00" #t) (when-iso "morgen 14:00"))
(test-equal "weekday is next such day" '("2026-11-09T00:00:00+01:00" #f) (when-iso "mo"))
(test-equal "today's weekday is today" '("2026-11-05T00:00:00+01:00" #f) (when-iso "do"))
(test-equal "German date without year" '("2026-11-10T09:30:00+01:00" #t) (when-iso "10.11. 9:30"))
(test-equal "past German date rolls to next year" '("2027-01-03T00:00:00+01:00" #f) (when-iso "3.1."))
(test-equal "relative days" '("2026-11-10T00:00:00+01:00" #f) (when-iso "+5d"))
(test-equal "time only is today" '("2026-11-05T14:00:00+01:00" #t) (when-iso "14:00"))
(test-equal "ISO with T" '("2026-11-10T14:00:00+01:00" #t) (when-iso "2026-11-10T14:00"))
(test-equal "nonsense" #f (when-iso "quatsch"))
(test-equal "durations" '(5400 5400 172800 -900 2700)
  (map parse-duration '("90m" "1h30m" "2d" "-15m" "45min")))
(test-equal "ical durations" '(-900 86400 5400)
  (map ical-duration->seconds '("-PT15M" "P1D" "PT1H30M")))
(test-equal "ical duration out" '("-PT15M" "P1D" "PT0S" "-P1DT2H")
  (map seconds->ical-duration '(-900 86400 0 -93600)))

;;; rrule

(define (exp rule start win-a win-b . kw)
  (map iso (apply expand-rrule rule start 3600 win-a win-b kw)))
(define mon-9 (local->instant 2026 10 19 9 0 0 tz))   ; Monday before DST end

(test-equal "weekly over DST keeps 09:00"
  '("2026-10-19T09:00:00+02:00" "2026-10-26T09:00:00+01:00" "2026-11-02T09:00:00+01:00")
  (exp "FREQ=WEEKLY" mon-9 mon-9 (local->instant 2026 11 3 0 0 0 tz)))
(test-equal "weekly BYDAY with COUNT"
  '("2026-10-19T09:00:00+02:00" "2026-10-21T09:00:00+02:00" "2026-10-26T09:00:00+01:00")
  (exp "FREQ=WEEKLY;BYDAY=MO,WE;COUNT=3" mon-9 mon-9 (local->instant 2027 1 1 0 0 0 tz)))
(test-equal "window cuts, COUNT counts from the start"
  '("2026-10-28T09:00:00+01:00")
  (exp "FREQ=WEEKLY;BYDAY=MO,WE;COUNT=4" mon-9 (local->instant 2026 10 27 0 0 0 tz)
       (local->instant 2027 1 1 0 0 0 tz)))
(test-equal "monthly last Friday"
  '("2026-10-30T09:00:00+01:00" "2026-11-27T09:00:00+01:00")
  (exp "FREQ=MONTHLY;BYDAY=-1FR" mon-9 mon-9 (local->instant 2026 12 1 0 0 0 tz)))
(test-equal "until date includes that day"
  '("2026-10-19T09:00:00+02:00" "2026-10-20T09:00:00+02:00")
  (exp "FREQ=DAILY;UNTIL=20261020" mon-9 mon-9 (local->instant 2027 1 1 0 0 0 tz)))
(test-equal "exdate removed"
  '("2026-10-19T09:00:00+02:00" "2026-10-21T09:00:00+02:00")
  (exp "FREQ=DAILY;COUNT=3" mon-9 mon-9 (local->instant 2027 1 1 0 0 0 tz)
       #:exdates (list (local->instant 2026 10 20 9 0 0 tz))))
(test-equal "monthly on the 31st skips short months"
  '("2026-10-31T09:00:00+01:00" "2026-12-31T09:00:00+01:00")
  (exp "FREQ=MONTHLY" (local->instant 2026 10 31 9 0 0 tz) mon-9 (local->instant 2027 1 15 0 0 0 tz)))
(test-equal "unsupported rule" #f
  (expand-rrule "FREQ=MONTHLY;BYSETPOS=-1;BYDAY=MO,TU" mon-9 3600 mon-9 (+ mon-9 9999999)))

;;; store + query on a temporary calendar

(define dir (string-append (or (getenv "TMPDIR") "/tmp") "/notyet-test-" (number->string (getpid))))
(mkdir dir)
(define (put name text)
  (call-with-output-file (string-append dir "/" name) (lambda (p) (display text p))))
(define (vev uid summary start end . extra)
  (string-append "BEGIN:VCALENDAR\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nUID:" uid
                 "\r\nSUMMARY:" summary "\r\nDTSTART:" start "\r\nDTEND:" end "\r\n"
                 (string-concatenate (map (lambda (l) (string-append l "\r\n")) extra))
                 "END:VEVENT\r\nEND:VCALENDAR\r\n"))
(put "1.ics" (vev "1" "Interview Acme" "20261106T090000Z" "20261106T100000Z"
                  "CATEGORIES:Gespräch" "LOCATION:Teams"))
(put "2.ics" (vev "2" "Fokus: Thesis schreiben" "20261110T080000Z" "20261110T100000Z"
                  "CATEGORIES:Fokus"))
(put "3.ics" (vev "3" "Frist Initech Online-Test" "20261112" "20261113"))
(put "4.ics" (vev "4" "Weekly sync" "20261019T080000Z" "20261019T083000Z"
                  "RRULE:FREQ=WEEKLY;BYDAY=MO"
                  "ORGANIZER;CN=Tim:mailto:tim@example.org"))
(put "5.ics" (vev "5" "notyet-pool: Fokus" "20000101T080000Z" "20000101T081500Z"))

(define cfg `((calendars ("t" . ,dir)) (me "me@example.org")
              (types ("Fokus" (color . "green")) ("Gespräch" (color . "blue")))))

(define (titles q)
  (let-values (((pred window) (compile-query q cfg)))
    (map (lambda (o) (event-summary (occ-event o)))
         (filter (lambda (o) (and (not (event-pool-type (occ-event o))) (pred o)))
                 (occurrences (load-events cfg) window)))))

(test-equal "word search ignores case and umlauts" '("Interview Acme") (titles "acme"))
(test-equal "tag" '("Fokus: Thesis schreiben") (titles "tag:fokus"))
(test-equal "tag with umlaut folded" '("Interview Acme") (titles "tag:gesprach"))
(test-equal "date range expands series"
  '("Interview Acme" "Weekly sync" "Fokus: Thesis schreiben")
  (titles "date:2026-11-06..2026-11-10"))
(test-equal "all-day date" '("Frist Initech Online-Test") (titles "date:2026-11-12 is:allday"))
(test-equal "or and not" '("Interview Acme" "Frist Initech Online-Test")
  (titles "(acme or frist) not tag:fokus"))
(test-equal "dash negation" '("Frist Initech Online-Test") (titles "is:allday -acme"))
(test-equal "invitation" '("Weekly sync") (titles "is:invite"))
(test-equal "loc" '("Interview Acme") (titles "loc:teams"))
(test-equal "ref prefix" '("Interview Acme") (titles "ref:2026-11-06/interview"))
(test-equal "pool placeholders hidden" '() (titles "notyet-pool"))
(test-equal "pool listed" 1 (length (pool-events (load-events cfg) "fokus")))
(test-equal "mo..fr is the coming working week"
  (list (local->instant 2026 11 9 0 0 0 tz) (local->instant 2026 11 14 0 0 0 tz))
  (let ((w (date-interval "mo..fr"))) (list (car w) (cdr w))))
(test-equal "slug" "bewerbungsgespraech-stelle-data-science"
  (slugify "Bewerbungsgespräch: Stelle Data Science"))

(for-each (lambda (f) (delete-file (string-append dir "/" f)))
          '("1.ics" "2.ics" "3.ics" "4.ics" "5.ics"))
(rmdir dir)

(define fails (test-runner-fail-count (test-runner-current)))
(test-end "notyet")
(exit (if (zero? fails) 0 1))

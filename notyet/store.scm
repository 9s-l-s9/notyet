;;; notyet/store.scm --- events on disk (a vdir: one .ics file per item)
;;;
;;; An <event> is one VEVENT inside one file.  An <occ> is one occurrence of
;;; an event in time: a plain event has one, a recurring series has one per
;;; repetition in the window asked for.  Occurrences are named by readable
;;; references, DATE/slug-of-title, never by UID.

(define-module (notyet store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (notyet ical)
  #:use-module (notyet time)
  #:use-module (notyet rrule)
  #:use-module (notyet config)
  #:export (<event> event? event-calendar event-file event-vcal event-comp
            event-uid event-summary event-location event-description
            event-tags event-type event-start event-end event-all-day?
            event-rrule event-recurring? event-recurrence-id
            event-organizer event-organizer-name event-attendees
            event-alarms event-foreign? event-pool-type event-status
            <occ> make-occ occ? occ-event occ-start occ-end occ-ref
            load-events occurrences
            slugify fold-text strip-html
            ical-duration->seconds seconds->ical-duration
            make-valarm new-vcal fresh-uid uid->filename
            write-vcal! event-with-comp delete-event!
            pool-events take-pool-event))

;;; Events

(define-record-type <event>
  (make-event calendar file vcal comp)
  event?
  (calendar event-calendar)  ; calendar name from the config
  (file event-file)          ; absolute path
  (vcal event-vcal)          ; all top-level components of the file
  (comp event-comp))         ; this VEVENT

(define (event-uid e) (or (component-ref (event-comp e) "UID") ""))
(define (event-summary e)
  (string-trim-both (or (component-text (event-comp e) "SUMMARY") "")))
(define (event-location e)
  (or (and=> (component-text (event-comp e) "LOCATION") string-trim-both) ""))
(define (event-description e)
  (strip-html (or (component-text (event-comp e) "DESCRIPTION") "")))
(define (event-status e) (component-ref (event-comp e) "STATUS"))

(define (event-tags e)
  (delete-duplicates
   (remove string-null?
           (map string-trim-both
                (append-map (lambda (p) (split-unescaped-commas (prop-value p)))
                            (component-props-named (event-comp e) "CATEGORIES"))))))

(define (event-type e cfg)
  "The first tag that names a configured type, or #f."
  (let ((types (type-names cfg)))
    (find (lambda (tag) (any (lambda (t) (string-ci=? t tag)) types))
          (event-tags e))))

(define (dt-of comp name)
  "Instant and all-day flag of date property NAME, or #f #f."
  (match (component-prop comp name)
    (#f (values #f #f))
    (p (call-with-values
           (lambda () (ical-time->instant (prop-value p) (prop-param p "TZID")))
         (lambda (t date?)
           (values t (or date? (equal? (prop-param p "VALUE") "DATE"))))))))

(define (event-start e) (call-with-values (lambda () (dt-of (event-comp e) "DTSTART"))
                          (lambda (t ignored) t)))
(define (event-all-day? e)
  (call-with-values (lambda () (dt-of (event-comp e) "DTSTART")) (lambda (ignored d) d)))

(define (event-end e)
  (let ((start (event-start e)))
    (call-with-values (lambda () (dt-of (event-comp e) "DTEND"))
      (lambda (t ignored)
        (cond
         (t t)
         ((component-ref (event-comp e) "DURATION")
          => (lambda (d) (+ start (or (ical-duration->seconds d) 0))))
         ((event-all-day? e) (add-days start 1))
         (else start))))))

(define (event-rrule e) (component-ref (event-comp e) "RRULE"))
(define (event-recurring? e) (and (event-rrule e) #t))
(define (event-recurrence-id e)
  (call-with-values (lambda () (dt-of (event-comp e) "RECURRENCE-ID"))
    (lambda (t ignored) t)))

(define (event-exdates e)
  (append-map
   (lambda (p)
     (map (lambda (v) (call-with-values
                          (lambda () (ical-time->instant v (prop-param p "TZID")))
                        (lambda (t ignored) t)))
          (string-split (prop-value p) #\,)))
   (component-props-named (event-comp e) "EXDATE")))

(define (mailto v)
  (let ((v (string-trim-both v)))
    (if (string-prefix-ci? "mailto:" v) (substring v 7) v)))

(define (event-organizer e)
  (and=> (component-prop (event-comp e) "ORGANIZER")
         (lambda (p) (string-downcase (mailto (prop-value p))))))

(define (event-organizer-name e)
  (and=> (component-prop (event-comp e) "ORGANIZER")
         (lambda (p)
           (let ((cn (prop-param p "CN")) (addr (mailto (prop-value p))))
             (if cn (string-append cn " <" addr ">") addr)))))

(define (event-attendees e)
  (map (lambda (p)
         (let ((cn (prop-param p "CN")) (addr (mailto (prop-value p))))
           (if (and cn (not (string=? cn addr)))
               (string-append cn " <" addr ">")
               addr)))
       (component-props-named (event-comp e) "ATTENDEE")))

(define (event-foreign? e cfg)
  "True if someone else organizes E (an invitation)."
  (let ((org (event-organizer e)))
    (and org
         (not (member org (map string-downcase (config-ref cfg 'me '())))))))

(define (event-alarms e)
  "List of (action . seconds-relative-to-start) for the event's VALARMs."
  (filter-map
   (lambda (c)
     (and (string=? (component-name c) "VALARM")
          (let ((trig (component-ref c "TRIGGER")))
            (and trig
                 (cons (or (component-ref c "ACTION") "?")
                       (or (ical-duration->seconds trig) 0))))))
   (component-children (event-comp e))))

(define %pool-prefix "notyet-pool:")
(define (event-pool-type e)
  "For a colour-pool placeholder (title notyet-pool: TYPE), its TYPE."
  (let ((s (event-summary e)))
    (and (string-prefix-ci? %pool-prefix s)
         (string-trim-both (substring s (string-length %pool-prefix))))))

;;; Durations

(define (ical-duration->seconds s)
  "Seconds of an iCalendar DURATION like -PT15M or P1DT2H, or #f."
  (let ((m (string-match
            "^([+-])?P(([0-9]+)W)?(([0-9]+)D)?(T(([0-9]+)H)?(([0-9]+)M)?(([0-9]+)S)?)?$"
            (string-trim-both s))))
    (and m
         (let ((n (lambda (i) (or (and=> (match:substring m i) string->number) 0))))
           (* (if (equal? (match:substring m 1) "-") -1 1)
              (+ (* 604800 (n 3)) (* 86400 (n 5)) (* 3600 (n 8))
                 (* 60 (n 10)) (n 12)))))))

(define (seconds->ical-duration secs)
  (let* ((s (abs secs)) (d (quotient s 86400)) (r (remainder s 86400))
         (h (quotient r 3600)) (m (quotient (remainder r 3600) 60))
         (sec (remainder r 60)))
    (string-append
     (if (< secs 0) "-" "") "P"
     (if (> d 0) (format #f "~aD" d) "")
     (if (or (> h 0) (> m 0) (> sec 0) (= s 0))
         (string-append "T" (if (> h 0) (format #f "~aH" h) "")
                        (if (> m 0) (format #f "~aM" m) "")
                        (if (or (> sec 0) (= s 0)) (format #f "~aS" sec) ""))
         ""))))

;;; Text helpers

(define (fold-text s)
  "Lower case without diacritics (Gespräch -> gesprach, ß -> ss)."
  (list->string
   (filter (lambda (c) (not (eq? (char-general-category c) 'Mn)))
           (string->list
            (string-normalize-nfd
             (string-downcase (regexp-substitute/global #f "ß" s 'pre "ss" 'post)))))))

(define (slugify s)
  (let* ((s (string-downcase s))
         (s (fold (lambda (pair acc)
                    (regexp-substitute/global #f (car pair) acc 'pre (cdr pair) 'post))
                  s '(("ä" . "ae") ("ö" . "oe") ("ü" . "ue") ("ß" . "ss"))))
         (s (fold-text s))
         (s (regexp-substitute/global #f "[^a-z0-9]+" s 'pre "-" 'post))
         (s (string-trim-both s #\-)))
    (if (> (string-length s) 48)
        (let ((cut (string-rindex s #\- 0 48)))
          (substring s 0 (if (and cut (> cut 20)) cut 48)))
        (if (string-null? s) "untitled" s))))

(define %entities '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                    ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'")
                    ("&nbsp;" . " ")))

(define (strip-html s)
  "Plain text for descriptions that some producers send as HTML."
  (if (not (string-match "<(p|br|div|span|a|ul|li|em|strong|b|i)[ />]" s))
      s
      (let* ((s (regexp-substitute/global #f "<(br|BR) */?>" s 'pre "\n" 'post))
             (s (regexp-substitute/global #f "</(p|div|li|ul|h[1-6])>" s
                                          'pre "\n" 'post))
             (s (regexp-substitute/global #f "<li[^>]*>" s 'pre "- " 'post))
             (s (regexp-substitute/global #f "<a [^>]*href=\"([^\"]*)\"[^>]*>"
                                          s 'pre "[" 1 "] " 'post))
             (s (regexp-substitute/global #f "<[^>]+>" s 'pre 'post))
             (s (fold (lambda (kv acc)
                        (regexp-substitute/global #f (car kv) acc 'pre (cdr kv) 'post))
                      s %entities))
             (s (regexp-substitute/global #f "\n[ \t]*\n([ \t]*\n)+" s
                                          'pre "\n\n" 'post)))
        (string-trim-both s))))

;;; Loading

(define (ics-files dir)
  (if (file-exists? dir)
      (map (lambda (f) (string-append dir "/" f))
           (or (scandir dir (lambda (f) (string-suffix? ".ics" f))) '()))
      '()))

(define (load-events cfg)
  "All VEVENTs of all configured calendars."
  (append-map
   (match-lambda
     ((name . dir)
      (append-map
       (lambda (file)
         (catch #t
           (lambda ()
             (let ((vcal (read-ical-file file)))
               (append-map
                (lambda (top)
                  (filter-map (lambda (c)
                                (and (string=? (component-name c) "VEVENT")
                                     (component-ref c "DTSTART")
                                     (make-event name file vcal c)))
                              (component-children top)))
                vcal)))
           (lambda (key . args)
             (format (current-error-port) "notyet: skipping ~a: ~a ~s~%"
                     file key args)
             '())))
       (ics-files dir))))
   (config-calendars cfg)))

;;; Occurrences

(define-record-type <occ>
  (make-occ event start end)
  occ?
  (event occ-event)
  (start occ-start)
  (end occ-end))

(define (occ-ref o)
  "Readable reference: DATE/slug, e.g. 2026-11-06/bewerbungsgespraech."
  (string-append (format-date (occ-start o)) "/"
                 (slugify (event-summary (occ-event o)))))

(define (overlaps? start end a b)
  (and (< start b) (> (max end (+ start 1)) a)))

(define* (occurrences events #:optional window)
  "Occurrences of EVENTS.  With WINDOW (start . end), recurring series are
expanded inside it and everything outside is dropped; without, each series
appears once, at its first start.  Sorted by start."
  (let* ((overrides (filter event-recurrence-id events))
         (overridden (map (lambda (e) (cons (event-uid e) (event-recurrence-id e)))
                          overrides)))
    (sort
     (append-map
      (lambda (e)
        (let ((start (event-start e)) (end (event-end e)))
          (cond
           ((and (event-recurring? e) (not (event-recurrence-id e)) window)
            (let* ((tz (or (and=> (component-prop (event-comp e) "DTSTART")
                                  (lambda (p) (known-zone (prop-param p "TZID"))))
                           (%time-zone)))
                   (starts (expand-rrule (event-rrule e) start (- end start)
                                         (car window) (cdr window)
                                         #:tz tz #:exdates (event-exdates e))))
              (if starts
                  (filter-map
                   (lambda (t)
                     (and (not (member (cons (event-uid e) t) overridden))
                          (make-occ e t (+ t (- end start)))))
                   starts)
                  ;; unsupported rule: show the first occurrence only
                  (if (overlaps? start end (car window) (cdr window))
                      (list (make-occ e start end))
                      '()))))
           ((not window) (list (make-occ e start end)))
           ((overlaps? start end (car window) (cdr window))
            (list (make-occ e start end)))
           (else '()))))
      events)
     (lambda (a b) (< (occ-start a) (occ-start b))))))

;;; Writing

(define (new-vcal vevent)
  (list (make-component
         "VCALENDAR"
         (list (make-prop "VERSION" '() "2.0")
               (make-prop "PRODID" '() "-//notyet//notyet//EN"))
         (list vevent))))

(define* (make-valarm action offset summary #:optional attendee)
  "A VALARM firing OFFSET seconds relative to the start (negative = before)."
  (make-component
   "VALARM"
   (append
    (list (make-prop "ACTION" '() action)
          (make-prop "TRIGGER" '(("RELATED" . "START"))
                     (seconds->ical-duration offset))
          (make-prop "DESCRIPTION" '() (ical-escape summary)))
    (if (string=? action "EMAIL")
        (append (list (make-prop "SUMMARY" '() (ical-escape summary)))
                (if attendee
                    (list (make-prop "ATTENDEE" '()
                                     (string-append "mailto:" attendee)))
                    '()))
        '()))
   '()))

(define (uid->filename uid)
  (string-append (regexp-substitute/global #f "[^A-Za-z0-9@._-]" uid 'pre "_" 'post)
                 ".ics"))

(define (fresh-uid cfg start summary events)
  "UID like 20261110-bewerbung-acme@domain, unique among EVENTS."
  (let* ((base (string-append (ical-date start) "-" (slugify summary)))
         (domain (config-ref cfg 'uid-domain "notyet.invalid"))
         (taken (map event-uid events)))
    (let loop ((n 1))
      (let ((uid (string-append base (if (= n 1) "" (format #f "-~a" n))
                                "@" domain)))
        (if (member uid taken) (loop (+ n 1)) uid)))))

(define (write-vcal! file vcal)
  "Write components VCAL to FILE atomically."
  (let ((tmp (string-append file ".notyet-tmp")))
    (call-with-output-file tmp
      (lambda (port) (put-string port (serialize-ical vcal)))
      #:encoding "UTF-8")
    (rename-file tmp file)))

(define (event-with-comp e new-comp)
  "The file contents of E with its VEVENT replaced by NEW-COMP."
  (map (lambda (top)
         (component-with-children
          top (map (lambda (c) (if (eq? c (event-comp e)) new-comp c))
                   (component-children top))))
       (event-vcal e)))

(define (delete-event! e)
  "Remove E and its overrides (same UID) from its file; delete the file when
no event is left."
  (let* ((uid (event-uid e))
         (vcal (map (lambda (top)
                      (component-with-children
                       top (remove (lambda (c)
                                     (and (string=? (component-name c) "VEVENT")
                                          (equal? (component-ref c "UID") uid)))
                                   (component-children top))))
                    (event-vcal e))))
    (if (any (lambda (top)
               (any (lambda (c) (string=? (component-name c) "VEVENT"))
                    (component-children top)))
             vcal)
        (write-vcal! (event-file e) vcal)
        (delete-file (event-file e)))))

;;; Colour pool

(define (pool-events events type)
  "Placeholders for TYPE, oldest first."
  (sort (filter (lambda (e) (and=> (event-pool-type e)
                                   (lambda (t) (string-ci=? t type))))
                events)
        (lambda (a b) (< (event-start a) (event-start b)))))

(define (take-pool-event events type)
  (match (pool-events events type) (() #f) ((e . _) e)))

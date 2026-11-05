;;; notyet/format.scm --- how occurrences are printed
;;;
;;; One line per occurrence for lists (grep-friendly), a header block for
;;; details (the same block the text export writes), and JSON.

(define-module (notyet format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:use-module (notyet time)
  #:use-module (notyet store)
  #:use-module (notyet rrule)
  #:use-module (notyet ical)
  #:use-module (notyet config)
  #:export (%color? occ-line occ-details occs->json describe-alarm when-text))

(define %color? (make-parameter #f))

(define %ansi
  '(("red" . "31") ("green" . "32") ("yellow" . "33") ("blue" . "34")
    ("magenta" . "35") ("purple" . "35") ("cyan" . "36") ("teal" . "36")
    ("grey" . "90") ("gray" . "90") ("orange" . "38;5;208")
    ("pink" . "38;5;205") ("brown" . "38;5;130") ("olive" . "38;5;100")))

(define (paint color text)
  (match (and (%color?) color (assoc (string-downcase color) %ansi))
    (#f text)
    ((_ . code) (string-append "\x1b[" code "m" text "\x1b[0m"))))

(define (dim text) (if (%color?) (string-append "\x1b[2m" text "\x1b[0m") text))

(define (type-color e cfg)
  (and=> (event-type e cfg)
         (lambda (t) (type-ref (type-spec cfg t) 'color))))

(define (when-text o)
  "2026-11-06 Fri 16:30-17:30, all-day, or spans across days."
  (let* ((e (occ-event o)) (s (occ-start o)) (n (occ-end o))
         (day (string-append (format-date s) " " (format-wday s))))
    (cond
     ((event-all-day? e)
      (let ((last (add-days n -1)))
        (if (> last s)
            (string-append day " all-day → " (format-date last) " " (format-wday last))
            (string-append day " all-day"))))
     ((<= n s) (string-append day " " (format-time s)))
     ((string=? (format-date s) (format-date (- n 1)))
      (string-append day " " (format-time s) "-" (format-time n)))
     (else (string-append day " " (format-time s) " → " (format-date n) " "
                          (format-wday n) " " (format-time n))))))

(define (first-line s max)
  (let* ((l (car (string-split (string-trim-both s) #\newline)))
         (l (string-trim-both l)))
    (if (> (string-length l) max) (string-append (substring l 0 (- max 1)) "…") l)))

(define (occ-line o cfg)
  (let* ((e (occ-event o))
         (tags (event-tags e))
         (loc (event-location e)))
    (string-append
     (when-text o) "  "
     (paint (type-color e cfg) (event-summary e))
     (if (event-recurring? e) " ↻" "")
     (if (null? tags) ""
         (string-append "  " (string-join
                              (map (lambda (t)
                                     (let ((spec (type-spec cfg t)))
                                       (paint (and spec (type-ref spec 'color))
                                              (string-append "#" t))))
                                   tags)
                              " ")))
     (if (string-null? loc) "" (string-append "  @" (first-line loc 32)))
     "  " (dim (string-append "[" (occ-ref o) "]")))))

(define (describe-alarm a)
  (match a
    ((action . secs)
     (string-append
      (string-downcase action) " "
      (cond ((= secs 0) "at start")
            ((< secs 0) (string-append (format-duration (- secs)) " before"))
            (else (string-append (format-duration secs) " after")))))))

(define (occ-details o cfg)
  "Header lines and notes, as in the text export."
  (let* ((e (occ-event o))
         (field (lambda (label value)
                  (if (or (not value) (and (string? value) (string-null? value)))
                      ""
                      (string-append label ": " value "\n")))))
    (string-append
     (field "Title" (event-summary e))
     (field "Ref" (occ-ref o))
     (field "When" (string-append (when-text o) " (" (%time-zone) ")"))
     (field "Repeats" (and=> (event-rrule e) describe-rrule))
     (field "Type" (event-type e cfg))
     (field "Tags" (string-join (event-tags e) ", "))
     (field "Location" (event-location e))
     (field "Reminder" (string-join (map describe-alarm (event-alarms e)) ", "))
     (field "Organizer" (event-organizer-name e))
     (field "Attendees" (string-join (event-attendees e) ", "))
     (field "Invitation" (and (event-foreign? e cfg) "yes (read-only unless --force)"))
     (field "Status" (event-status e))
     (field "Calendar" (event-calendar e))
     (field "UID" (event-uid e))
     (field "File" (event-file e))
     (let ((d (event-description e)))
       (if (string-null? d) "" (string-append "\n" d "\n"))))))

;;; JSON (no dependency)

(define (json-string s)
  (string-append
   "\""
   (string-concatenate
    (map (lambda (c)
           (case c
             ((#\") "\\\"") ((#\\) "\\\\") ((#\newline) "\\n")
             ((#\return) "\\r") ((#\tab) "\\t")
             (else (if (< (char->integer c) 32)
                       (format #f "\\u~4,'0x" (char->integer c))
                       (string c)))))
         (string->list s)))
   "\""))

(define (json v)
  (cond
   ((eq? v #t) "true")
   ((eq? v #f) "false")
   ((eq? v 'null) "null")
   ((null? v) "[]")
   ((number? v) (number->string v))
   ((string? v) (json-string v))
   ((and (pair? v) (pair? (car v)) (string? (caar v)))   ; object
    (string-append "{" (string-join (map (lambda (kv)
                                           (string-append (json-string (car kv)) ":"
                                                          (json (cdr kv))))
                                         v)
                                    ",")
                   "}"))
   ((list? v) (string-append "[" (string-join (map json v) ",") "]"))
   (else (json-string (format #f "~a" v)))))

(define (occs->json occs cfg)
  (json
   (map (lambda (o)
          (let ((e (occ-event o)))
            `(("ref" . ,(occ-ref o))
              ("title" . ,(event-summary e))
              ("start" . ,(format-iso (occ-start o)))
              ("end" . ,(format-iso (occ-end o)))
              ("all_day" . ,(event-all-day? e))
              ("type" . ,(or (event-type e cfg) 'null))
              ("tags" . ,(event-tags e))
              ("location" . ,(event-location e))
              ("reminders" . ,(map describe-alarm (event-alarms e)))
              ("recurring" . ,(event-recurring? e))
              ("invitation" . ,(and (event-foreign? e cfg) #t))
              ("organizer" . ,(or (event-organizer-name e) ""))
              ("calendar" . ,(event-calendar e))
              ("description" . ,(event-description e))
              ("uid" . ,(event-uid e))
              ("file" . ,(event-file e)))))
        occs)))

;;; notyet/cli.scm --- the notyet command
;;;
;;; Events are chosen by search terms, never by ID.  Commands that change
;;; something (new, edit, tag, rm) write the .ics file, then run the
;;; configured sync command and refresh the text export.

(define-module (notyet cli)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (notyet ical)
  #:use-module (notyet time)
  #:use-module (notyet config)
  #:use-module (notyet store)
  #:use-module (notyet query)
  #:use-module (notyet format)
  #:export (main))

(define %version "0.1.0")

;;; Options

(define (parse-args args valued flags)
  "Split ARGS into an options alist and the remaining words.  VALUED options
take a value (repeatable; all values kept in order), FLAGS do not.  A lone
-- ends option parsing."
  (let loop ((args args) (opts '()) (words '()))
    (match args
      (() (values (reverse opts) (reverse words)))
      (("--" . rest) (values (reverse opts) (append (reverse words) rest)))
      (((? (cut string-prefix? "--" <>) a) . rest)
       (let* ((eq (string-index a #\=))
              (name (substring a 2 (or eq (string-length a)))))
         (cond
          ((member name flags) (loop rest (acons name #t opts) words))
          ((member name valued)
           (cond (eq (loop rest (acons name (substring a (+ eq 1)) opts) words))
                 ((pair? rest) (loop (cdr rest) (acons name (car rest) opts) words))
                 (else (fail "--~a needs a value" name))))
          (else (fail "unknown option --~a" name)))))
      ((w . rest) (loop rest opts (cons w words))))))

(define (opt opts name) (assoc-ref opts name))
(define (opts-all opts name)
  (filter-map (match-lambda ((k . v) (and (string=? k name) v))) opts))

(define (fail fmt . args)
  (throw 'notyet-error (apply format #f fmt args)))

(define (words->query words)
  ;; Keep each shell word whole: re-quote words with spaces.
  (string-join (map (lambda (w)
                      (if (and (string-index w #\space) (not (string-index w #\")))
                          (let ((i (string-index w #\:)))
                            (if (and i (not (string-index (substring w 0 i) #\space)))
                                (string-append (substring w 0 (+ i 1)) "\""
                                               (substring w (+ i 1)) "\"")
                                (string-append "\"" w "\"")))
                          w))
                    words)
               " "))

;;; Selecting

(define* (select cfg query-string #:key default-window)
  "Occurrences matching QUERY-STRING.  Colour-pool placeholders are left out
unless the query asks for is:pool."
  (let-values (((pred window) (compile-query query-string cfg)))
    (let ((pool? (query-mentions? query-string "is:pool")))
      (filter (lambda (o)
                (and (or pool? (not (event-pool-type (occ-event o))))
                     (pred o)))
              (occurrences (load-events cfg) (or window default-window))))))

(define (distinct-events occs)
  (delete-duplicates (map occ-event occs) eq?))

(define (print-occs occs cfg fmt limit)
  (let ((occs (if limit (take occs (min limit (length occs))) occs)))
    (match fmt
      ("json" (display (occs->json occs cfg)) (newline))
      ("refs" (for-each (lambda (o) (display (occ-ref o)) (newline)) occs))
      (_ (for-each (lambda (o) (display (occ-line o cfg)) (newline)) occs)))))

(define (one-event occs cfg what)
  "The single event behind OCCS, or fail with the candidates listed."
  (match (distinct-events occs)
    (() (fail "no event matches"))
    ((e) e)
    (es
     (for-each (lambda (o) (display (occ-line o cfg)) (newline))
               (take occs (min 15 (length occs))))
     (fail "~a events match; ~a needs exactly one.  Narrow the search, e.g. with date:YYYY-MM-DD or ref:..."
           (length es) what))))

(define (check-own e cfg opts what)
  (when (and (event-foreign? e cfg) (not (opt opts "force")))
    (fail "~s is an invitation from ~a; ~a only changes your copy and the organizer is not told.  Repeat with --force if that is what you want."
          (event-summary e) (event-organizer-name e) what)))

;;; Changing

(define (now-utc) (ical-utc (current-time)))

(define (touch comp)
  "Update DTSTAMP, LAST-MODIFIED and SEQUENCE after a change."
  (let ((seq (or (and=> (component-ref comp "SEQUENCE") string->number) 0)))
    (component-set (component-set (component-set comp "DTSTAMP" (now-utc))
                                  "LAST-MODIFIED" (now-utc))
                   "SEQUENCE" (number->string (+ seq 1)))))

(define (set-times comp start end all-day? tzid)
  "Write DTSTART/DTEND: dates for all-day, the event's own zone if it had a
known one, UTC otherwise."
  (let ((comp (component-remove comp "DURATION"))
        (zone (known-zone tzid)))
    (cond
     (all-day?
      (component-set (component-set comp "DTSTART" (ical-date start) '(("VALUE" . "DATE")))
                     "DTEND" (ical-date end) '(("VALUE" . "DATE"))))
     ((and zone (not (string=? zone "UTC")))
      (component-set (component-set comp "DTSTART" (ical-local start zone)
                                    `(("TZID" . ,tzid)))
                     "DTEND" (ical-local end zone) `(("TZID" . ,tzid))))
     (else
      (component-set (component-set comp "DTSTART" (ical-utc start))
                     "DTEND" (ical-utc end))))))

(define (set-tags comp tags)
  (let ((comp (component-remove comp "CATEGORIES")))
    (if (null? tags)
        comp
        (component-set comp "CATEGORIES" (string-join (map ical-escape tags) ",")))))

(define (reminder-offset s)
  "Seconds relative to start for --remind: 1d, 15m, 0 / start; or 'none."
  (cond
   ((member (string-downcase s) '("none" "no" "keine" "off")) 'none)
   ((member (string-downcase s) '("0" "start" "at-start")) 0)
   ((parse-duration s) => (lambda (d) (- (abs d))))
   (else (fail "cannot read reminder ~s (try 1d, 2h, 15m, start, none)" s))))

(define (set-reminders comp offsets summary cfg)
  (let* ((kids (remove (lambda (c) (string=? (component-name c) "VALARM"))
                       (component-children comp)))
         (me (match (config-ref cfg 'me '()) ((m . _) m) (_ #f)))
         (action (config-ref cfg 'reminder-action "EMAIL")))
    (component-with-children
     comp (append kids (map (lambda (off) (make-valarm action off summary me))
                            offsets)))))

(define (time-of-day t)
  (match (instant->parts t) ((_ _ _ h mi s _) (+ (* 3600 h) (* 60 mi) s))))

(define (on-day day secs)
  (match (instant->parts day)
    ((y mo d . _) (local->instant y mo d (quotient secs 3600)
                                  (quotient (remainder secs 3600) 60)
                                  (remainder secs 60)))))

(define (shift t secs)
  (if (zero? (remainder secs 86400)) (add-days t (quotient secs 86400)) (+ t secs)))

(define (when! str base)
  (let-values (((t clock?) (parse-when str base)))
    (unless t (fail "cannot read time ~s (try 2026-11-10 14:00, morgen 9:30, fr, +3d)" str))
    (values t clock?)))

;;; Sync and export

(define (run-sync cfg)
  (match (config-ref cfg 'sync-command #f)
    (#f #t)
    ((prog . args)
     (force-output)
     (let ((status (apply system* prog args)))
       (unless (zero? (status:exit-val status))
         (format (current-error-port)
                 "notyet: warning: sync command failed (~a); the change is saved locally and goes out with the next sync~%"
                 (status:exit-val status)))
       (zero? (status:exit-val status))))))

(define (delete-tree dir)
  (when (file-exists? dir)
    (nftw dir (lambda (path stat flag base level)
                (case flag
                  ((directory-processed) (rmdir path))
                  ((regular symlink) (delete-file path)))
                #t)
          'depth 'physical)))

(define (export! cfg)
  "Write the plain-text mirror: one file per event plus agenda.txt."
  (match (and=> (config-ref cfg 'export-dir #f) expand-home)
    (#f #t)
    (dir
     (let* ((tmp (string-append dir ".notyet-new"))
            (old (string-append dir ".notyet-old"))
            (events (remove event-pool-type (load-events cfg)))
            (today (day-start (current-time))))
       (delete-tree tmp)
       (mkdir-p tmp)
       (call-with-output-file (string-append tmp "/.notyet-export")
         (cut display "This directory is generated by notyet export.\n" <>))
       (for-each
        (lambda (o)
          (let* ((e (occ-event o))
                 (s (occ-start o))
                 (year (string-append tmp "/" (strftime "%Y" (localtime s))))
                 (name (string-append (format-date s) "_"
                                      (if (event-all-day? e) "allday"
                                          (string-delete #\: (format-time s)))
                                      "_" (slugify (event-summary e)) ".txt")))
            (mkdir-p year)
            (call-with-output-file (string-append year "/" name)
              (cut display (occ-details o cfg) <>)
              #:encoding "UTF-8")))
        (occurrences events))
       (call-with-output-file (string-append tmp "/agenda.txt")
         (lambda (port)
           (parameterize ((%color? #f))
             (format port "# notyet agenda ~a .. ~a, generated ~a~%"
                     (format-date today) (format-date (add-days today 60))
                     (format-iso (current-time)))
             (for-each (lambda (o) (display (occ-line o cfg) port) (newline port))
                       (occurrences events (cons today (add-days today 60))))))
         #:encoding "UTF-8")
       ;; Swap in; only ever remove a directory notyet created.
       (when (file-exists? dir)
         (unless (file-exists? (string-append dir "/.notyet-export"))
           (fail "export-dir ~a exists and was not made by notyet; refusing to replace it" dir))
         (delete-tree old)
         (rename-file dir old))
       (rename-file tmp dir)
       (delete-tree old)
       #t))))

(define (mkdir-p dir)
  (unless (file-exists? dir)
    (mkdir-p (dirname dir))
    (mkdir dir)))

(define (after-change cfg opts)
  (unless (opt opts "no-sync")
    (run-sync cfg)
    (catch 'notyet-error
      (lambda () (export! cfg))
      (lambda (key msg) (format (current-error-port) "notyet: export: ~a~%" msg)))))

;;; Commands

(define* (cmd-search args #:key default-window)
  (let-values (((opts words) (parse-args args '("format" "limit") '())))
    (let ((cfg (load-config)))
      (print-occs (select cfg (words->query words) #:default-window default-window)
                  cfg (or (opt opts "format") "text")
                  (and=> (opt opts "limit") string->number)))))

(define (cmd-agenda args)
  ;; notyet agenda [DAYS] [QUERY...]
  (let* ((days (match args
                 (((? string->number n) . _) (string->number n))
                 (_ 7)))
         (rest (if (and (pair? args) (string->number (car args))) (cdr args) args))
         (today (day-start (current-time))))
    (let-values (((opts words) (parse-args rest '("format" "limit") '())))
      (let* ((cfg (load-config))
             (q (words->query words))
             (q (if (query-mentions-date? q) q
                    (string-append q " date:today..+" (number->string (max 0 (- days 1))) "d"))))
        (print-occs (select cfg q) cfg (or (opt opts "format") "text")
                    (and=> (opt opts "limit") string->number))))))

(define (query-mentions-date? q)
  (or (string-contains-ci q "date:") (string-contains-ci q "datum:")))

(define (cmd-show args)
  (let-values (((opts words) (parse-args args '("format" "limit") '())))
    (let* ((cfg (load-config))
           (occs (select cfg (words->query words)))
           (limit (or (and=> (opt opts "limit") string->number) 10)))
      (when (null? occs) (fail "no event matches"))
      (if (equal? (opt opts "format") "json")
          (print-occs occs cfg "json" limit)
          (begin
            (for-each (lambda (o) (display (occ-details o cfg)) (display "\n---\n"))
                      (take occs (min limit (length occs))))
            (when (> (length occs) limit)
              (format #t "(~a more; use --limit)~%" (- (length occs) limit))))))))

(define (cmd-count args)
  (let ((cfg (load-config)))
    (display (length (select cfg (words->query args))))
    (newline)))

(define %edit-valued
  '("title" "start" "end" "for" "shift" "date" "location" "desc" "tag" "untag"
    "remind" "type" "calendar"))
(define %edit-flags '("all-day" "force" "dry-run" "no-sync" "no-pool"))

(define (apply-type tags type cfg)
  "TAGS with every configured type removed and TYPE added (canonical case)."
  (let ((spec (or (type-spec cfg type)
                  (fail "unknown type ~s; known: ~a" type
                        (string-join (type-names cfg) ", ")))))
    (append (list (car spec))
            (remove (lambda (t) (type-spec cfg t)) tags))))

(define (edit-tags tags add del)
  (append (remove (lambda (t) (any (cut string-ci=? t <>) del)) tags)
          (remove (lambda (t) (any (cut string-ci=? t <>) tags)) add)))

(define (cmd-new args)
  (let-values (((opts words) (parse-args args %edit-valued %edit-flags)))
    (let* ((cfg (load-config))
           (title (string-join words " "))
           (type (and=> (opt opts "type")
                        (lambda (t) (car (or (type-spec cfg t)
                                             (fail "unknown type ~s; known: ~a" t
                                                   (string-join (type-names cfg) ", ")))))))
           (spec (and type (type-spec cfg type)))
           (check (when (string-null? title) (fail "a title is needed: notyet new \"Title\" --start ...")))
           (start-str (or (opt opts "start") (fail "--start is needed"))))
      (let-values (((start clock?) (when! start-str #f)))
        (let* ((all-day? (or (opt opts "all-day")
                             (and (not clock?) (or (not spec) (type-ref spec 'all-day #t))
                                  (not clock?))))
               (start (if all-day? (day-start start) start))
               (end (cond
                     ((opt opts "end")
                      => (lambda (s)
                           (let-values (((t c?) (when! s start)))
                             (if all-day? (add-days (day-start t) 1) t))))
                     ((opt opts "for")
                      => (lambda (d) (shift start (or (parse-duration d)
                                                      (fail "cannot read duration ~s" d)))))
                     (all-day? (add-days start 1))
                     (else (+ start (or (and=> (type-ref spec 'duration) parse-duration)
                                        3600)))))
               (tags (let ((t (edit-tags '() (opts-all opts "tag") '())))
                       (if type (apply-type t type cfg) t)))
               (reminders (let ((given (opts-all opts "remind")))
                            (filter-map
                             (lambda (r) (let ((x (reminder-offset r))) (and (number? x) x)))
                             (if (pair? given) given
                                 (match (type-ref spec 'reminder #f)
                                   (#f '()) ((? string? r) (list r)) ((? list? rs) rs))))))
               (events (load-events cfg))
               (calendar (or (opt opts "calendar") (config-ref cfg 'default-calendar)
                             (caar (config-calendars cfg))))
               (dir (or (calendar-dir cfg calendar) (fail "unknown calendar ~s" calendar)))
               (pool (and type (not (opt opts "no-pool"))
                          (take-pool-event events type)))
               (uid (if pool (event-uid pool) (fresh-uid cfg start title events)))
               (comp (make-component "VEVENT"
                                     (list (make-prop "UID" '() uid)
                                           (make-prop "DTSTAMP" '() (now-utc))
                                           (make-prop "CREATED" '() (now-utc))
                                           (make-prop "SEQUENCE" '() "0"))
                                     '()))
               (comp (set-times comp start end all-day? #f))
               (comp (component-set-text comp "SUMMARY" title))
               (comp (if (opt opts "location") (component-set-text comp "LOCATION" (opt opts "location")) comp))
               (comp (if (opt opts "desc") (component-set-text comp "DESCRIPTION" (opt opts "desc")) comp))
               (comp (set-tags comp tags))
               (comp (if pool
                         ;; keep the server's SEQUENCE moving forward
                         (component-set comp "SEQUENCE"
                                        (number->string
                                         (+ 1 (or (and=> (component-ref (event-comp pool) "SEQUENCE")
                                                         string->number) 0))))
                         comp))
               (comp (set-reminders comp reminders title cfg))
               (file (if pool (event-file pool)
                         (string-append dir "/" (uid->filename uid))))
               (vcal (if pool (event-with-comp pool comp) (new-vcal comp)))
               (shown (make-occ (make-event-for-display calendar file vcal comp) start end)))
          (cond
           ((opt opts "dry-run")
            (display "would create: ") (display (occ-line shown cfg)) (newline))
           (else
            (write-vcal! file vcal)
            (display "created: ") (display (occ-line shown cfg)) (newline)
            (when type
              (let ((color (type-ref spec 'color)))
                (cond
                 (pool (format #t "colour: took a ~a placeholder (~a left)~%" type
                               (- (length (pool-events events type)) 1)))
                 (color (format #t "colour: no notyet-pool: ~a placeholder left, so Mailfence shows the calendar colour; see notyet pool~%" type)))))
            (after-change cfg opts))))))))

(define (make-event-for-display calendar file vcal comp)
  ((record-constructor <event>) calendar file vcal comp))

(define (cmd-edit args)
  (let-values (((opts words) (parse-args args %edit-valued %edit-flags)))
    (let* ((cfg (load-config))
           (q (words->query words))
           (check (when (string-null? (string-trim-both q)) (fail "which event? give search terms")))
           (occs (select cfg q))
           (e (one-event occs cfg "edit"))
           (o (find (lambda (o) (eq? (occ-event o) e)) occs)))
      (check-own e cfg opts "editing")
      (when (and (event-recurring? e) (or (opt opts "start") (opt opts "date") (opt opts "shift")))
        (format #t "note: ~s repeats; the change applies to the whole series~%" (event-summary e)))
      (let* ((comp (event-comp e))
             (old-start (event-start e)) (old-end (event-end e))
             (dur (- old-end old-start))
             (tzid (and=> (component-prop comp "DTSTART") (cut prop-param <> "TZID")))
             (all-day? (or (opt opts "all-day") (event-all-day? e)))
             (start
              (let* ((s old-start)
                     (s (match (opt opts "date")
                          (#f s)
                          (d (let-values (((t ignored) (when! d s)))
                               (on-day t (time-of-day s))))))
                     (s (match (opt opts "start")
                          (#f s)
                          (str (let-values (((t clock?) (when! str s)))
                                 (if clock? t (on-day t (time-of-day s)))))))
                     (s (match (opt opts "shift")
                          (#f s)
                          (d (shift s (or (parse-duration d) (fail "cannot read duration ~s" d)))))))
                (if all-day? (day-start s) s)))
             (end (cond
                   ((opt opts "end")
                    => (lambda (str) (let-values (((t clock?) (when! str start)))
                                       (if all-day? (add-days (day-start t) 1) t))))
                   ((opt opts "for")
                    => (lambda (d) (shift start (or (parse-duration d) (fail "cannot read duration ~s" d)))))
                   ((and (opt opts "all-day") (not (event-all-day? e)))
                    (add-days (day-start (max start (- (+ start dur) 1))) 1))
                   (else (shift start dur))))
             (check (when (< end start) (fail "the end would be before the start")))
             (times-changed? (or (not (= start old-start)) (not (= end old-end))
                                 (not (eq? all-day? (event-all-day? e)))))
             (comp (if times-changed? (set-times comp start end all-day? tzid) comp))
             (comp (match (opt opts "title") (#f comp) (t (component-set-text comp "SUMMARY" t))))
             (comp (match (opt opts "location")
                     (#f comp) ("" (component-remove comp "LOCATION"))
                     (l (component-set-text comp "LOCATION" l))))
             (comp (match (opt opts "desc")
                     (#f comp) ("" (component-remove comp "DESCRIPTION"))
                     (d (component-set-text comp "DESCRIPTION" d))))
             (tags (edit-tags (event-tags e) (opts-all opts "tag") (opts-all opts "untag")))
             (tags (match (opt opts "type") (#f tags) (t (apply-type tags t cfg))))
             (comp (if (equal? tags (event-tags e)) comp (set-tags comp tags)))
             (comp (match (opts-all opts "remind")
                     (() comp)
                     (rs (set-reminders comp (filter number? (map reminder-offset rs))
                                        (or (component-text comp "SUMMARY") "") cfg))))
             (comp (touch comp))
             (vcal (event-with-comp e comp))
             (after (make-occ (make-event-for-display (event-calendar e) (event-file e) vcal comp)
                              start end)))
        (format #t "before: ~a~%after:  ~a~%" (occ-line o cfg) (occ-line after cfg))
        (unless (opt opts "dry-run")
          (write-vcal! (event-file e) vcal)
          (after-change cfg opts))))))

(define (cmd-tag args)
  ;; notyet tag +A -B [--] QUERY...
  (let loop ((args args) (add '()) (del '()))
    (match args
      (((? (cut string-prefix? "+" <>) t) . rest) (loop rest (cons (substring t 1) add) del))
      (((? (lambda (t) (and (string-prefix? "-" t) (not (string-prefix? "--" t)))) t) . rest)
       (loop rest add (cons (substring t 1) del)))
      (_
       (let-values (((opts words) (parse-args args '() '("dry-run" "no-sync"))))
         (when (and (null? add) (null? del)) (fail "usage: notyet tag +TAG -TAG QUERY..."))
         (let* ((cfg (load-config))
                (q (words->query words))
                (check (when (string-null? (string-trim-both q)) (fail "which events? give search terms")))
                (es (distinct-events (select cfg q))))
           (when (null? es) (fail "no event matches"))
           (for-each
            (lambda (e)
              (let ((tags (edit-tags (event-tags e) (reverse add) (reverse del))))
                (unless (equal? tags (event-tags e))
                  (let* ((comp (touch (set-tags (event-comp e) tags)))
                         (vcal (event-with-comp e comp)))
                    (format #t "~a: ~a~%" (event-summary e)
                            (if (null? tags) "(no tags)" (string-join tags ", ")))
                    (unless (opt opts "dry-run") (write-vcal! (event-file e) vcal))))))
            es)
           (unless (opt opts "dry-run") (after-change cfg opts))))))))

(define (cmd-rm args)
  (let-values (((opts words) (parse-args args '() '("confirm" "force" "no-sync"))))
    (let* ((cfg (load-config))
           (q (words->query words))
           (check (when (string-null? (string-trim-both q)) (fail "which event? give search terms")))
           (occs (select cfg q))
           (e (one-event occs cfg "rm"))
           (o (find (lambda (o) (eq? (occ-event o) e)) occs)))
      (check-own e cfg opts "deleting (the server may send the organizer a decline)")
      (if (not (opt opts "confirm"))
          (format #t "would delete~a: ~a~%repeat with --confirm~%"
                  (if (event-recurring? e) " the whole series" "") (occ-line o cfg))
          (begin
            (delete-event! e)
            (format #t "deleted: ~a~%" (occ-line o cfg))
            (after-change cfg opts))))))

(define (cmd-pool args)
  (let ((cfg (load-config)))
    (match args
      (("add" type n)
       (let* ((spec (or (type-spec cfg type) (fail "unknown type ~s" type)))
              (n (or (string->number n) (fail "count?")))
              (events (load-events cfg))
              (have (length (pool-events events (car spec))))
              (dir (calendar-dir cfg #f)))
         (for-each
          (lambda (i)
            (let* ((title (string-append "notyet-pool: " (car spec)))
                   (start (local->instant 2000 1 1 (+ 8 (quotient (+ have i) 60))
                                          (remainder (+ have i) 60) 0))
                   (uid (fresh-uid cfg start (string-append title "-" (number->string (+ have i 1)))
                                   events))
                   (comp (make-component "VEVENT"
                                         (list (make-prop "UID" '() uid)
                                               (make-prop "DTSTAMP" '() (now-utc))
                                               (make-prop "SEQUENCE" '() "0"))
                                         '()))
                   (comp (set-times comp start (+ start 900) #f #f))
                   (comp (component-set-text comp "SUMMARY" title)))
              (write-vcal! (string-append dir "/" (uid->filename uid)) (new-vcal comp))))
          (iota n))
         (run-sync cfg)
         (format #t "added ~a placeholder(s) for ~a on 2000-01-01.  Now give them the colour ~a in the Mailfence web calendar (open each, Farbe, Speichern).~%"
                 n (car spec) (or (type-ref spec 'color) "of your choice"))))
      (_
       (let ((events (load-events cfg)))
         (for-each
          (lambda (t)
            (format #t "~12a ~2d placeholder(s)~a~%" (car t)
                    (length (pool-events events (car t)))
                    (match (type-ref t 'color) (#f "") (c (string-append "  colour " c)))))
          (config-ref cfg 'types '()))
         (display "\nRefill: notyet pool add TYPE N, then colour the new events in the web calendar.\n"))))))

(define (cmd-types)
  (let ((cfg (load-config)))
    (for-each
     (lambda (t)
       (format #t "~12a~@[ colour ~a~]~@[ · ~a~]~@[ · reminder ~a~]~@[ · ~a~]~%"
               (car t) (type-ref t 'color)
               (and=> (type-ref t 'duration) (cut string-append "duration " <>))
               (match (type-ref t 'reminder) (#f #f) ((? string? r) r) (rs (string-join rs ", ")))
               (type-ref t 'about)))
     (config-ref cfg 'types '()))))

(define %usage "\
notyet — search and edit a calendar the way notmuch handles mail

  notyet agenda [DAYS] [QUERY...]     next DAYS days (default 7)
  notyet search QUERY...              one line per occurrence
  notyet show QUERY...                full details
  notyet count QUERY...
  notyet new TITLE --start WHEN [--end WHEN | --for DUR] [--type T]
             [--tag T]... [--location L] [--desc D] [--remind DUR]...
             [--all-day] [--calendar C] [--no-pool] [--dry-run]
  notyet edit QUERY... [--title T] [--start WHEN] [--end WHEN] [--for DUR]
             [--date DAY] [--shift ±DUR] [--location L] [--desc D]
             [--type T] [--tag T]... [--untag T]... [--remind DUR|none]...
             [--all-day] [--force] [--dry-run]
  notyet tag +TAG -TAG QUERY...
  notyet rm QUERY... [--confirm] [--force]
  notyet types | pool [add TYPE N] | sync | export

  Output: --format text|json|refs, --limit N.  Changes run the sync command
  and refresh the export unless --no-sync.

Query terms (joined by AND; also or, not, -term, parentheses):
  word \"a phrase\" title: loc: desc: tag: type: organizer: with: calendar:
  ref:DATE/slug  uid:  date:today date:2026-11 date:mo..fr date:today..+7d
  date:this-week date:past  is:allday is:recurring is:invite is:mine is:pool
  has:reminder has:type

WHEN: 2026-11-10 14:00, 10.11. 9:30, morgen 14:00, fr, +3d, 14:00 (today)
DUR:  90m, 1h30m, 2d, 1w
")

(define (main args)
  (setlocale LC_ALL "")
  (set-port-encoding! (current-output-port) "UTF-8")
  (let ((cfg (load-config)))
    (%time-zone (config-ref cfg 'timezone "Europe/Berlin")))
  (parameterize ((%color? (and (isatty? (current-output-port))
                               (not (getenv "NO_COLOR"))
                               (not (member "--format=json" args)))))
    (catch 'notyet-error
      (lambda ()
        (match (cdr args)
          ((or () ("help") ("--help") ("-h")) (display %usage))
          (("--version") (format #t "notyet ~a~%" %version))
          (("agenda" . rest) (cmd-agenda rest))
          (((or "search" "list") . rest) (cmd-search rest))
          (("show" . rest) (cmd-show rest))
          (("count" . rest) (cmd-count rest))
          (((or "new" "add") . rest) (cmd-new rest))
          (("edit" . rest) (cmd-edit rest))
          (("tag" . rest) (cmd-tag rest))
          (((or "rm" "delete") . rest) (cmd-rm rest))
          (("pool" . rest) (cmd-pool rest))
          (("types") (cmd-types))
          (("sync") (let ((cfg (load-config))) (run-sync cfg) (export! cfg)))
          (("export") (export! (load-config)))
          ((cmd . _) (fail "unknown command ~s; see notyet help" cmd))))
      (lambda (key msg)
        (format (current-error-port) "notyet: ~a~%" msg)
        (exit 1)))))

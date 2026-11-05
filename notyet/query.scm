;;; notyet/query.scm --- notmuch-style search terms for events
;;;
;;;   acme                      word anywhere (title, place, notes, tags, people)
;;;   "online test"              phrase
;;;   title:interview  loc:teams  desc:link  organizer:meier  with:alex
;;;   tag:Bewerbung  type:Fokus  calendar:mailfence  uid:...  ref:2026-11-06/...
;;;   date:today  date:2026-11  date:mo..fr  date:today..+7d  date:past
;;;   is:allday is:recurring is:invite is:mine is:pool   has:reminder
;;;   a and b   a or b   not a   -a   ( ... )
;;;
;;; Matching ignores case and diacritics.  Terms are joined by AND.

(define-module (notyet query)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (notyet time)
  #:use-module (notyet store)
  #:use-module (notyet config)
  #:export (compile-query date-interval query-mentions?))

;;; Tokens

(define (tokenize str)
  "Words, key:value pairs, quoted phrases and parentheses."
  (let loop ((cs (string->list str)) (cur '()) (in-quote? #f) (out '()))
    (define (flush)
      (if (null? cur) out (cons (list->string (reverse cur)) out)))
    (match cs
      (() (reverse (flush)))
      ((#\" . rest) (loop rest cur (not in-quote?) out))
      (((? char-whitespace? c) . rest)
       (if in-quote? (loop rest (cons c cur) #t out) (loop rest '() #f (flush))))
      (((and c (or #\( #\))) . rest)
       (if in-quote?
           (loop rest (cons c cur) #t out)
           (loop rest '() #f (cons (string c) (flush)))))
      ((c . rest) (loop rest (cons c cur) in-quote? out)))))

;;; Dates

(define %far-past 0)
(define %far-future 4102444800)          ; 2100-01-01

(define* (unit-interval w #:optional base)
  "The interval named by one date word: a day, a month, a year or a week.
Relative day words (fr, +3d) count from BASE when given."
  (let ((tz (%time-zone)) (w (string-downcase w)))
    (cond
     ((member w '("now" "jetzt"))
      (let ((t (or (%now) (current-time)))) (cons t (+ t 1))))
     ((member w '("past" "vergangen"))
      (cons %far-past (or (%now) (current-time))))
     ((member w '("future" "upcoming" "kommend" "zukunft"))
      (cons (or (%now) (current-time)) %far-future))
     ((string-match "^([0-9]{4})-([0-9]{1,2})$" w)
      => (lambda (m)
           (let ((a (local->instant (string->number (match:substring m 1))
                                    (string->number (match:substring m 2)) 1 0 0 0 tz)))
             (cons a (add-months a 1 tz)))))
     ((string-match "^([0-9]{4})$" w)
      => (lambda (m)
           (let ((y (string->number (match:substring m 1))))
             (cons (local->instant y 1 1 0 0 0 tz) (local->instant (+ y 1) 1 1 0 0 0 tz)))))
     ((member w '("this-week" "week" "diese-woche" "woche"))
      (let ((a (week-start (or (%now) (current-time)) tz))) (cons a (add-days a 7 tz))))
     ((member w '("next-week" "naechste-woche" "nächste-woche"))
      (let ((a (add-days (week-start (or (%now) (current-time)) tz) 7 tz)))
        (cons a (add-days a 7 tz))))
     ((member w '("last-week" "letzte-woche"))
      (let ((a (add-days (week-start (or (%now) (current-time)) tz) -7 tz)))
        (cons a (add-days a 7 tz))))
     ((member w '("this-month" "month" "dieser-monat" "monat"))
      (let ((a (month-start (or (%now) (current-time)) tz))) (cons a (add-months a 1 tz))))
     ((member w '("next-month" "naechster-monat" "nächster-monat"))
      (let ((a (add-months (month-start (or (%now) (current-time)) tz) 1 tz)))
        (cons a (add-months a 1 tz))))
     ((member w '("last-month" "letzter-monat"))
      (let ((a (add-months (month-start (or (%now) (current-time)) tz) -1 tz)))
        (cons a (add-months a 1 tz))))
     ((parse-date-word w base)
      => (lambda (d) (cons d (add-days d 1 tz))))
     (else #f))))

(define (date-interval str)
  "(start . end) for date:STR, or #f.  A..B includes the whole of B."
  (match (string-split-dots str)
    ((a) (unit-interval a))
    (("" "") #f)
    (("" b) (and=> (unit-interval b) (lambda (ib) (cons %far-past (cdr ib)))))
    ((a "") (and=> (unit-interval a) (lambda (ia) (cons (car ia) %far-future))))
    ;; B counts from the start of A: mo..fr is one working week.
    ((a b) (let* ((ia (unit-interval a)) (ib (and ia (unit-interval b (car ia)))))
             (and ia ib (cons (car ia) (cdr ib)))))
    (_ #f)))

(define (string-split-dots s)
  (match (string-contains s "..")
    (#f (list s))
    (i (list (substring s 0 i) (substring s (+ i 2))))))

;;; Terms

(define (contains? hay needle) (string-contains (fold-text hay) (fold-text needle)))

(define (haystack e)
  (string-join (append (list (event-summary e) (event-location e)
                             (event-description e)
                             (or (event-organizer-name e) ""))
                       (event-tags e) (event-attendees e))
               "\n"))

(define (term->pred term cfg)
  "Predicate on occurrences for one term."
  (define (on-event f) (lambda (o) (f (occ-event o))))
  (match (string-index term #\:)
    ((? (lambda (i) (and i (> i 0)
                         (member (string-downcase (substring term 0 i))
                                 %keys))) i)
     (let ((key (string-downcase (substring term 0 i)))
           (val (substring term (+ i 1))))
       (match key
         ((or "title" "subject" "titel")
          (on-event (lambda (e) (contains? (event-summary e) val))))
         ((or "location" "loc" "ort")
          (on-event (lambda (e) (contains? (event-location e) val))))
         ((or "desc" "description" "notes" "notiz")
          (on-event (lambda (e) (contains? (event-description e) val))))
         ((or "tag" "type" "typ")
          (on-event (lambda (e) (any (lambda (t) (string=? (fold-text t) (fold-text val)))
                                     (event-tags e)))))
         ((or "organizer" "from" "von")
          (on-event (lambda (e) (contains? (or (event-organizer-name e) "") val))))
         ((or "with" "attendee" "mit")
          (on-event (lambda (e) (any (lambda (a) (contains? a val)) (event-attendees e)))))
         ((or "calendar" "cal" "kalender")
          (on-event (lambda (e) (string-ci=? (event-calendar e) val))))
         ("uid" (on-event (lambda (e) (string-contains (event-uid e) val))))
         ("ref" (lambda (o) (string-prefix? (fold-text val) (occ-ref o))))
         ((or "date" "datum")
          (match (date-interval val)
            (#f (error (format #f "cannot read date: ~a" val)))
            ((a . b) (lambda (o) (and (< (occ-start o) b)
                                      (> (max (occ-end o) (+ 1 (occ-start o))) a))))))
         ("is"
          (match (string-downcase val)
            ((or "allday" "ganztags") (on-event event-all-day?))
            ((or "recurring" "serie") (on-event event-recurring?))
            ((or "invite" "invitation" "einladung") (on-event (lambda (e) (event-foreign? e cfg))))
            ((or "mine" "eigen") (on-event (lambda (e) (not (event-foreign? e cfg)))))
            ("pool" (on-event (lambda (e) (and (event-pool-type e) #t))))
            (x (error (format #f "unknown is:~a" x)))))
         ("has"
          (match (string-downcase val)
            ((or "reminder" "alarm" "erinnerung") (on-event (lambda (e) (pair? (event-alarms e)))))
            ((or "location" "ort") (on-event (lambda (e) (not (string-null? (event-location e))))))
            ((or "tag" "tags") (on-event (lambda (e) (pair? (event-tags e)))))
            ((or "type" "typ") (on-event (lambda (e) (and (event-type e cfg) #t))))
            (x (error (format #f "unknown has:~a" x))))))))
    (_ (on-event (lambda (e) (contains? (haystack e) term))))))

(define %keys
  '("title" "subject" "titel" "location" "loc" "ort" "desc" "description"
    "notes" "notiz" "tag" "type" "typ" "organizer" "from" "von" "with"
    "attendee" "mit" "calendar" "cal" "kalender" "uid" "ref" "date" "datum"
    "is" "has"))

;;; Parser: or < and < not < atom

(define (compile-query str cfg)
  "Two values: a predicate on occurrences, and the bounding window
(start . end) of the date: terms that are not negated, or #f."
  (let ((windows '()))
    (define (parse-or toks)
      (let-values (((p toks) (parse-and toks)))
        (let loop ((p p) (toks toks))
          (match toks
            (((? (lambda (t) (string-ci=? t "or"))) . rest)
             (let-values (((q toks) (parse-and rest)))
               (loop (let ((p p)) (lambda (o) (or (p o) (q o)))) toks)))
            (_ (values p toks))))))
    (define (parse-and toks)
      (let-values (((p toks) (parse-not toks #f)))
        (let loop ((p p) (toks toks))
          (match toks
            ((or () (")" . _)) (values p toks))
            (((? (lambda (t) (string-ci=? t "or"))) . _) (values p toks))
            (((? (lambda (t) (string-ci=? t "and"))) . rest) (loop p rest))
            (_ (let-values (((q toks) (parse-not toks #f)))
                 (loop (let ((p p)) (lambda (o) (and (p o) (q o)))) toks)))))))
    (define (parse-not toks negated?)
      (match toks
        (((? (lambda (t) (string-ci=? t "not"))) . rest)
         (let-values (((p toks) (parse-not rest (not negated?))))
           (values (lambda (o) (not (p o))) toks)))
        (((? (lambda (t) (and (> (string-length t) 1) (string-prefix? "-" t)
                              (char-alphabetic? (string-ref t 1))))
             t) . rest)
         (let-values (((p ignored) (parse-not (list (substring t 1)) (not negated?))))
           (values (lambda (o) (not (p o))) rest)))
        (_ (parse-atom toks negated?))))
    (define (parse-atom toks negated?)
      (match toks
        (("(" . rest)
         (let-values (((p toks) (parse-or rest)))
           (match toks
             ((")" . rest) (values p rest))
             (_ (error "missing )")))))
        ((t . rest)
         (let ((key-len (cond ((string-prefix-ci? "date:" t) 5)
                              ((string-prefix-ci? "datum:" t) 6)
                              (else #f))))
           (when (and key-len (not negated?))
             (and=> (date-interval (substring t key-len))
                    (lambda (w) (set! windows (cons w windows))))))
         (values (term->pred t cfg) rest))
        (() (error "query ends too early"))))
    (let ((toks (tokenize str)))
      (if (null? toks)
          (values (const #t) #f)
          (let-values (((p rest) (parse-or toks)))
            (unless (null? rest)
              (error (format #f "unexpected ~a in query" (car rest))))
            (values p
                    (and (pair? windows)
                         (cons (apply min (map car windows))
                               (apply max (map cdr windows))))))))))

(define (query-mentions? str word)
  "Whether the query text contains the term WORD (e.g. \"is:pool\")."
  (any (lambda (t) (string-ci=? t word)) (tokenize str)))

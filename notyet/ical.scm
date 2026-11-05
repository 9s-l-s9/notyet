;;; notyet/ical.scm --- lossless iCalendar (RFC 5545) reader and writer
;;;
;;; A calendar file is parsed into a tree of components (VCALENDAR, VEVENT,
;;; VALARM, VTIMEZONE, ...).  Every property keeps its name, its parameters
;;; exactly as written (quotes included) and its raw, still-escaped value, so
;;; writing a parsed file back changes nothing except line folding.  Edits
;;; replace whole properties; everything notyet does not understand
;;; (X- properties, VTIMEZONE, attendee parameters) survives untouched.

(define-module (notyet ical)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:export (make-component component? component-name component-props
            component-children
            make-prop prop? prop-name prop-params prop-value
            parse-ical serialize-ical read-ical-file
            component-prop component-props-named component-ref
            component-text prop-text prop-param
            component-with-props component-with-children
            component-set component-set-text component-remove
            component-add
            ical-escape ical-unescape split-unescaped-commas))

(define-record-type <component>
  (make-component name props children)
  component?
  (name component-name)           ; upper-case string, e.g. "VEVENT"
  (props component-props)         ; list of <prop>, in file order
  (children component-children))  ; list of <component>

(define-record-type <prop>
  (make-prop name params value)
  prop?
  (name prop-name)       ; upper-case string
  (params prop-params)   ; alist (NAME . raw-value), raw = as written
  (value prop-value))    ; raw value, still escaped

;;; Reading

(define (unfold-lines text)
  "Split TEXT into logical content lines, joining folded continuations."
  (let loop ((lines (string-split text #\newline)) (acc '()))
    (match lines
      (() (reverse acc))
      ((line . rest)
       (let ((line (string-trim-right line #\return)))
         (cond
          ((and (pair? acc)
                (> (string-length line) 0)
                (memv (string-ref line 0) '(#\space #\tab)))
           (loop rest (cons (string-append (car acc) (substring line 1))
                            (cdr acc))))
          ((string-null? (string-trim-both line)) (loop rest acc))
          (else (loop rest (cons line acc)))))))))

(define (parse-content-line line)
  "Parse NAME *(;PARAM=VALUE) : VALUE.  Quoted parameter values may contain
the delimiters ; : and , ."
  (let ((n (string-length line)))
    (define (scan-until i stops)
      (let loop ((j i))
        (if (and (< j n) (not (memv (string-ref line j) stops)))
            (loop (+ j 1))
            j)))
    (let* ((name-end (scan-until 0 '(#\; #\:)))
           (name (string-upcase (substring line 0 name-end))))
      (let loop ((i name-end) (params '()))
        (cond
         ((>= i n) (make-prop name (reverse params) ""))
         ((char=? (string-ref line i) #\:)
          (make-prop name (reverse params) (substring line (+ i 1))))
         (else                          ; at #\;
          (let* ((pstart (+ i 1))
                 (pend (scan-until pstart '(#\= #\; #\:)))
                 (pname (string-upcase (substring line pstart pend))))
            (if (or (>= pend n) (not (char=? (string-ref line pend) #\=)))
                (loop pend (cons (cons pname "") params))
                ;; Raw value: up to the next unquoted ; or :
                (let vloop ((j (+ pend 1)) (in-quote? #f))
                  (cond
                   ((>= j n)
                    (loop j (cons (cons pname (substring line (+ pend 1) j))
                                  params)))
                   ((char=? (string-ref line j) #\")
                    (vloop (+ j 1) (not in-quote?)))
                   ((and (not in-quote?) (memv (string-ref line j) '(#\; #\:)))
                    (loop j (cons (cons pname (substring line (+ pend 1) j))
                                  params)))
                   (else (vloop (+ j 1) in-quote?))))))))))))

(define (parse-ical text)
  "Parse iCalendar TEXT into a list of top-level components."
  (let loop ((lines (map parse-content-line (unfold-lines text)))
             ;; stack of frames: (name props-rev children-rev)
             (stack (list (list #f '() '()))))
    (define (add-child frame child)
      (match frame ((name props kids) (list name props (cons child kids)))))
    (match lines
      (()
       ;; Close anything left open (truncated files) rather than failing.
       (let close ((stack stack))
         (match stack
           (((#f _ kids)) (reverse kids))
           (((name props kids) parent . rest)
            (close (cons (add-child parent
                                    (make-component name (reverse props)
                                                    (reverse kids)))
                         rest))))))
      ((p . rest)
       (cond
        ((string=? (prop-name p) "BEGIN")
         (loop rest (cons (list (string-upcase (prop-value p)) '() '()) stack)))
        ((string=? (prop-name p) "END")
         (match stack
           (((#f _ _)) (loop rest stack))          ; stray END
           (((name props kids) parent . others)
            (loop rest (cons (add-child parent
                                        (make-component name (reverse props)
                                                        (reverse kids)))
                             others)))))
        (else
         (match stack
           (((name props kids) . others)
            (loop rest (cons (list name (cons p props) kids) others))))))))))

(define (read-ical-file file)
  (parse-ical (call-with-input-file file get-string-all #:encoding "UTF-8")))

;;; Writing

(define (char-octets c)
  (let ((i (char->integer c)))
    (cond ((< i #x80) 1) ((< i #x800) 2) ((< i #x10000) 3) (else 4))))

(define (fold-line line)
  "Fold LINE at 75 octets without splitting a UTF-8 character."
  (let loop ((chars (string->list line)) (cur '()) (len 0) (limit 75) (out '()))
    (match chars
      (() (string-join (reverse (cons (list->string (reverse cur)) out))
                       "\r\n "))
      ((c . rest)
       (let ((w (char-octets c)))
         (if (> (+ len w) limit)
             (loop chars '() 0 74 (cons (list->string (reverse cur)) out))
             (loop rest (cons c cur) (+ len w) limit out)))))))

(define (prop->line p)
  (string-append
   (prop-name p)
   (string-concatenate
    (map (match-lambda
           ((k . "") (string-append ";" k))
           ((k . v) (string-append ";" k "=" v)))
         (prop-params p)))
   ":" (prop-value p)))

(define (component->lines c)
  (append (list (string-append "BEGIN:" (component-name c)))
          (map prop->line (component-props c))
          (append-map component->lines (component-children c))
          (list (string-append "END:" (component-name c)))))

(define (serialize-ical components)
  "Serialize a list of components with CRLF line ends and folding."
  (string-concatenate
   (map (lambda (l) (string-append (fold-line l) "\r\n"))
        (append-map component->lines components))))

;;; Text values

(define (ical-unescape s)
  (let loop ((cs (string->list s)) (out '()))
    (match cs
      (() (list->string (reverse out)))
      ((#\\ (or #\n #\N) . rest) (loop rest (cons #\newline out)))
      ((#\\ c . rest) (loop rest (cons c out)))
      ((c . rest) (loop rest (cons c out))))))

(define (ical-escape s)
  (string-concatenate
   (map (lambda (c)
          (case c
            ((#\\) "\\\\") ((#\;) "\\;") ((#\,) "\\,")
            ((#\newline) "\\n") ((#\return) "")
            (else (string c))))
        (string->list s))))

(define (split-unescaped-commas s)
  "Split raw value S on commas that are not escaped; unescape each part."
  (let loop ((cs (string->list s)) (cur '()) (parts '()))
    (define (flush) (cons (ical-unescape (list->string (reverse cur))) parts))
    (match cs
      (() (reverse (flush)))
      ((#\\ c . rest) (loop rest (cons* c #\\ cur) parts))
      ((#\, . rest) (loop rest '() (flush)))
      ((c . rest) (loop rest (cons c cur) parts)))))

;;; Access

(define (component-prop c name)
  (find (lambda (p) (string=? (prop-name p) name)) (component-props c)))

(define (component-props-named c name)
  (filter (lambda (p) (string=? (prop-name p) name)) (component-props c)))

(define (component-ref c name)
  "Raw value of the first property NAME, or #f."
  (and=> (component-prop c name) prop-value))

(define (prop-text p) (ical-unescape (prop-value p)))

(define (component-text c name)
  "Unescaped text of the first property NAME, or #f."
  (and=> (component-prop c name) prop-text))

(define (prop-param p name)
  "Parameter NAME of P, unquoted, or #f."
  (and=> (assoc name (prop-params p))
         (lambda (kv)
           (let ((v (cdr kv)))
             (if (and (>= (string-length v) 2)
                      (string-prefix? "\"" v) (string-suffix? "\"" v))
                 (substring v 1 (- (string-length v) 1))
                 v)))))

;;; Functional update

(define (component-with-props c props)
  (make-component (component-name c) props (component-children c)))

(define (component-with-children c children)
  (make-component (component-name c) (component-props c) children))

(define (component-remove c name)
  (component-with-props
   c (remove (lambda (p) (string=? (prop-name p) name)) (component-props c))))

(define* (component-set c name value #:optional (params '()))
  "Replace all NAME properties by one with raw VALUE, at the position of the
first old one (or at the end)."
  (let ((new (make-prop name params value)))
    (let loop ((ps (component-props c)) (out '()) (placed? #f))
      (match ps
        (() (component-with-props
             c (reverse (if placed? out (cons new out)))))
        ((p . rest)
         (if (string=? (prop-name p) name)
             (loop rest (if placed? out (cons new out)) #t)
             (loop rest (cons p out) placed?)))))))

(define* (component-set-text c name text #:optional (params '()))
  (component-set c name (ical-escape text) params))

(define (component-add c prop)
  (component-with-props c (append (component-props c) (list prop))))

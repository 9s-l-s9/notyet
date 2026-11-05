;;; notyet/config.scm --- user configuration
;;;
;;; ~/.config/notyet/config.scm (or $NOTYET_CONFIG) holds one alist:
;;;
;;;   ((calendars ("mailfence" . "~/Calendar/mailfence/events"))
;;;    (default-calendar . "mailfence")
;;;    (timezone . "Europe/Berlin")
;;;    (me "me@example.org")                 ; your addresses (organizer check)
;;;    (uid-domain . "example.org")          ; right side of new UIDs
;;;    (sync-command "vdirsyncer" "sync")    ; run after every change, or #f
;;;    (export-dir . "~/Calendar/text")      ; plain-text mirror, or #f
;;;    (reminder-action . "EMAIL")           ; or "DISPLAY"
;;;    (types ("Fokus" (color . "green") (duration . "2h"))
;;;           ("Frist" (color . "red") (all-day . #t) (reminder . "2d"))))

(define-module (notyet config)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (ice-9 match)
  #:export (load-config config-ref expand-home config-file
            config-calendars calendar-dir type-spec type-names type-ref))

(define (expand-home path)
  (if (and (string? path) (string-prefix? "~/" path))
      (string-append (getenv "HOME") (substring path 1))
      path))

(define (config-file)
  (or (getenv "NOTYET_CONFIG")
      (string-append (or (getenv "XDG_CONFIG_HOME")
                         (string-append (getenv "HOME") "/.config"))
                     "/notyet/config.scm")))

(define %defaults
  '((calendars ("default" . "~/Calendar/default"))
    (default-calendar . #f)
    (timezone . "Europe/Berlin")
    (me)
    (uid-domain . "notyet.invalid")
    (sync-command . #f)
    (export-dir . #f)
    (reminder-action . "EMAIL")
    (types)))

(define* (load-config #:optional (file (config-file)))
  (let ((user (if (file-exists? file)
                  (call-with-input-file file read)
                  '())))
    (unless (list? user)
      (error "config must be one alist:" file))
    (append user %defaults)))

(define* (config-ref cfg key #:optional default)
  (match (assq key cfg)
    (#f default)
    ((_ . v) v)))

(define (config-calendars cfg)
  "Alist (name . absolute-dir)."
  (map (match-lambda ((name . dir) (cons name (expand-home dir))))
       (config-ref cfg 'calendars '())))

(define (calendar-dir cfg name)
  (assoc-ref (config-calendars cfg)
             (or name (config-ref cfg 'default-calendar)
                 (caar (config-calendars cfg)))))

(define (type-names cfg) (map car (config-ref cfg 'types '())))

(define (type-spec cfg name)
  "The type entry matching NAME case-insensitively, as (name . props), or #f."
  (find (lambda (t) (string-ci=? (car t) name)) (config-ref cfg 'types '())))

(define* (type-ref spec key #:optional default)
  (match (and spec (assq key (cdr spec)))
    ((_ . v) v)
    (_ default)))

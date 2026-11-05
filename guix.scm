;;; guix.scm -- build notyet as a Guix package.
;;;
;;;     guix build -f guix.scm
;;;     guix shell -f guix.scm -- notyet agenda
;;;     guix shell -m manifest.scm -- guile -L . tests/run.scm

(use-modules (guix packages)
             (guix gexp)
             (guix build-system guile)
             ((guix licenses) #:prefix license:)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages guile))

(define %source-dir (dirname (current-filename)))

(package
  (name "notyet")
  (version "0.1.0")
  (source (local-file %source-dir "notyet-source"
                      #:recursive? #t
                      #:select? (lambda (file stat)
                                  (not (or (string-contains file "/.git")
                                           (string-suffix? ".go" file)
                                           (string-suffix? ".log" file))))))
  (build-system guile-build-system)
  (arguments
   (list
    #:not-compiled-file-regexp "(tests/|guix\\.scm$|manifest\\.scm$)"
    #:phases
    #~(modify-phases %standard-phases
        (add-after 'build 'check
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "TZDIR" (search-input-directory inputs "share/zoneinfo"))
            (invoke "guile" "-L" "." "--no-auto-compile" "tests/run.scm")))
        (add-after 'check 'install-script
          (lambda* (#:key inputs #:allow-other-keys)
            (let* ((bin (string-append #$output "/bin"))
                   (script (string-append bin "/notyet"))
                   (version (target-guile-effective-version))
                   (site (string-append #$output "/share/guile/site/" version))
                   (ccache (string-append #$output "/lib/guile/" version
                                          "/site-ccache")))
              (mkdir-p bin)
              (call-with-output-file script
                (lambda (port)
                  ;; Guile's meta switch: the kernel passes one argument.
                  (format port "#!~a \\~%--no-auto-compile -e main -s~%!#~%"
                          (search-input-file inputs "/bin/guile"))
                  (display "(use-modules (notyet cli))\n" port)))
              (chmod script #o755)
              (wrap-program script
                `("GUILE_LOAD_PATH" prefix (,site))
                `("GUILE_LOAD_COMPILED_PATH" prefix (,ccache)))))))))
  (native-inputs (list guile-3.0 tzdata))
  (inputs (list guile-3.0 bash-minimal))
  (synopsis "Search and edit a CalDAV calendar the way notmuch handles mail")
  (description
   "notyet works on a vdir (a directory of iCalendar files, as kept in sync
by vdirsyncer).  Events are found with notmuch-style search terms instead of
IDs (@code{tag:Fokus date:today..+7d acme}), printed one per line, as
details or as JSON, and created, edited, tagged and deleted from the command
line.  Event types carry defaults (duration, reminders, colour), a plain-text
export mirrors the calendar for grep and agents, and a pool of pre-coloured
placeholder events carries server-side colours that CalDAV cannot set.")
  (home-page "https://github.com/9s-l-s9/notyet")
  (license license:gpl3+))

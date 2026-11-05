;; Development environment.
;; Usage: guix shell -m manifest.scm -- guile -L . tests/run.scm
(specifications->manifest
 (quote ("guile" "tzdata" "vdirsyncer")))

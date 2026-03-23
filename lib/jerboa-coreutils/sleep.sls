#!chezscheme
;;; sleep.sls -- Pause for a specified amount of time

(library (jerboa-coreutils sleep)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (init-security!)
    (parameterize ((program-name "sleep"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing operand"))
          (let ((total (fold-left + 0 (map parse-duration (hash-ref opt 'rest)))))
            (thread-sleep! total)))
        args
        'program: "sleep"
        'help: "Pause for NUMBER seconds. SUFFIX may be 's' (seconds, default), 'm' (minutes), 'h' (hours), or 'd' (days)."
        (rest-arguments 'rest 'help: "NUMBER[SUFFIX]..."))))

  ;; Parse a duration string like "1.5s", "2m", "3h", "1d"
  (def (parse-duration str)
    (let* ((len (string-length str))
           (last-char (if (> len 0) (string-ref str (- len 1)) #\nul)))
      (let-values (((num-str mult)
                    (case last-char
                      ((#\s) (values (substring str 0 (- len 1)) 1))
                      ((#\m) (values (substring str 0 (- len 1)) 60))
                      ((#\h) (values (substring str 0 (- len 1)) 3600))
                      ((#\d) (values (substring str 0 (- len 1)) 86400))
                      (else  (values str 1)))))
        (let ((num (string->number num-str)))
          (unless num
            (die "invalid time interval '~a'" str))
          (when (< num 0)
            (die "invalid time interval '~a'" str))
          (* num mult)))))

  ) ;; end library

#!chezscheme
;;; printenv.sls -- Print environment variables

(library (jerboa-coreutils printenv)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (get-environment-variables)
    ;; Chez Scheme doesn't have get-environment-variables directly,
    ;; parse /proc/self/environ
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let* ((p (open-file-input-port "/proc/self/environ"))
               (bv (get-bytevector-all p)))
          (close-port p)
          (if (eof-object? bv)
            '()
            (let ((str (utf8->string bv)))
              (let split-nul ((i 0) (start 0) (acc '()))
                (cond
                  ((>= i (string-length str))
                   (reverse
                     (if (> i start)
                       (cons (substring str start i) acc)
                       acc)))
                  ((eqv? (string-ref str i) #\nul)
                   (if (= i start)
                     (split-nul (+ i 1) (+ i 1) acc)
                     (split-nul (+ i 1) (+ i 1)
                       (cons (substring str start i) acc))))
                  (else (split-nul (+ i 1) start acc))))))))))

  (def (parse-env-entry entry)
    (let ((len (string-length entry)))
      (let loop ((i 0))
        (cond
          ((>= i len) #f)
          ((eqv? (string-ref entry i) #\=)
           (cons (substring entry 0 i)
                 (substring entry (+ i 1) len)))
          (else (loop (+ i 1)))))))

  (def (main . args)
    (parameterize ((program-name "printenv"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((vars (hash-ref opt 'rest))
                  (delim (if (hash-get opt 'null) #\nul #\newline)))
              (if (null? vars)
                ;; Print all environment variables
                (for-each
                  (lambda (entry)
                    (let ((pair (parse-env-entry entry)))
                      (when pair
                        (display (car pair))
                        (display "=")
                        (display (cdr pair))
                        (write-char delim))))
                  (get-environment-variables))
                ;; Print specific variables
                (let ((fail? #f))
                  (for-each
                    (lambda (name)
                      (let ((val (getenv name #f)))
                        (if val
                          (begin (display val) (write-char delim))
                          (set! fail? #t))))
                    vars)
                  (exit (if fail? 1 0))))))
        args
        'program: "printenv"
        'help: "Print the values of the specified environment VARIABLE(s). If no VARIABLE is specified, print name and value pairs for them all."
        (flag 'null "-0" "--null"
          'help: "end each output line with NUL, not newline")
        (rest-arguments 'rest 'help: "variable names"))))

  ) ;; end library

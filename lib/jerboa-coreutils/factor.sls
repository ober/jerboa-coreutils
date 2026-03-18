#!chezscheme
;;; factor.sls -- Factorize numbers into prime factors

(library (jerboa-coreutils factor)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "factor"))
      (cond
        ((and (pair? args) (member (car args) '("--help")))
         (displayln "Usage: factor [NUMBER]...")
         (displayln "Print the prime factors of each specified integer NUMBER."))
        ((and (pair? args) (member (car args) '("--version")))
         (version-info "factor"))
        ((null? args)
         ;; Read from stdin
         (let loop ()
           (let ((line (get-line (current-input-port))))
             (unless (eof-object? line)
               (let ((n (string->number (string-trim line))))
                 (if n
                   (print-factors n)
                   (warn "'~a' is not a valid positive integer" (string-trim line))))
               (loop)))))
        (else
         (for-each
           (lambda (arg)
             (let ((n (string->number arg)))
               (if (and n (integer? n) (> n 0))
                 (print-factors (inexact->exact n))
                 (warn "'~a' is not a valid positive integer" arg))))
           args)))))

  (def (print-factors n)
    (display n)
    (display ":")
    (let ((factors (factorize n)))
      (for-each
        (lambda (f) (display " ") (display f))
        factors))
    (newline))

  (def (factorize n)
    (if (<= n 1)
      '()
      (let loop ((n n) (d 2) (factors '()))
        (cond
          ((> (* d d) n)
           (reverse (if (> n 1) (cons n factors) factors)))
          ((zero? (modulo n d))
           (loop (/ n d) d (cons d factors)))
          (else
           (loop n (if (= d 2) 3 (+ d 2)) factors))))))

  (def (string-trim str)
    (let* ((len (string-length str))
           (start (let loop ((i 0))
                    (if (and (< i len) (char-whitespace? (string-ref str i)))
                      (loop (+ i 1)) i)))
           (end (let loop ((i (- len 1)))
                  (if (and (> i start) (char-whitespace? (string-ref str i)))
                    (loop (- i 1)) (+ i 1)))))
      (substring str start end)))

  ) ;; end library

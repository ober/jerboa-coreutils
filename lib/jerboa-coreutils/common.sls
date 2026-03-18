#!chezscheme
;;; common.sls -- Shared utilities for jerboa-coreutils

(library (jerboa-coreutils common)
  (export
    program-name
    die
    warn
    try-help
    split-long-opts
    EXIT_SUCCESS
    EXIT_FAILURE)

  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            printf fprintf)
          (std format))

  ;; Exit codes
  (define EXIT_SUCCESS 0)
  (define EXIT_FAILURE 1)

  ;; Current program name - set from argv[0]
  (define program-name (make-parameter "coreutils"))

  ;; Print error message to stderr and exit with code
  (define (die fmt . args)
    (eprintf "~a: ~?~n" (program-name) fmt args)
    (exit EXIT_FAILURE))

  ;; Print warning message to stderr
  (define (warn fmt . args)
    (eprintf "~a: ~?~n" (program-name) fmt args))

  ;; Print "Try 'prog --help' for more information."
  (define (try-help)
    (eprintf "Try '~a --help' for more information.~n" (program-name))
    (exit EXIT_FAILURE))

  ;; Split --key=value args into --key value for getopt compatibility
  (define (split-long-opts args)
    (let loop ((rest args) (acc '()))
      (if (null? rest)
        (reverse acc)
        (let ((arg (car rest)))
          (let ((eqpos (string-index-eq arg)))
            (if (and eqpos
                     (> (string-length arg) 2)
                     (char=? (string-ref arg 0) #\-)
                     (char=? (string-ref arg 1) #\-))
              (loop (cdr rest)
                    (cons (substring arg (+ eqpos 1) (string-length arg))
                          (cons (substring arg 0 eqpos) acc)))
              (loop (cdr rest) (cons arg acc))))))))

  (define (string-index-eq str)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((char=? (string-ref str i) #\=) i)
        (else (loop (+ i 1))))))

  ) ;; end library

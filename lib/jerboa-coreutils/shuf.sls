#!chezscheme
;;; shuf.sls -- Shuffle lines randomly

(library (jerboa-coreutils shuf)
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
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "shuf"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((lines
                   (cond
                     ((hash-get opt 'echo) (hash-ref opt 'rest))
                     ((hash-get opt 'input-range)
                      (let ((parts (string-split-at (hash-ref opt 'input-range) #\-)))
                        (unless (= (length parts) 2)
                          (die "invalid input range: '~a'" (hash-ref opt 'input-range)))
                        (let ((lo (string->number (car parts)))
                              (hi (string->number (cadr parts))))
                          (unless (and lo hi)
                            (die "invalid input range: '~a'" (hash-ref opt 'input-range)))
                          (let loop ((i lo) (acc '()))
                            (if (> i hi)
                              (reverse acc)
                              (loop (+ i 1) (cons (number->string i) acc)))))))
                     (else
                      ;; Read from file or stdin
                      (let ((port (if (or (null? (hash-ref opt 'rest)) (string=? (car (hash-ref opt 'rest)) "-"))
                                    (current-input-port)
                                    (open-input-file (car (hash-ref opt 'rest))))))
                        (let loop ((acc '()))
                          (let ((line (get-line port)))
                            (if (eof-object? line)
                              (begin
                                (when (and (pair? (hash-ref opt 'rest)) (not (string=? (car (hash-ref opt 'rest)) "-")))
                                  (close-port port))
                                (reverse acc))
                              (loop (cons line acc)))))))))
                 (shuffled (fisher-yates-shuffle (list->vector lines)))
                 (count (if (hash-get opt 'head-count)
                          (min (string->number (hash-ref opt 'head-count)) (vector-length shuffled))
                          (vector-length shuffled)))
                 (delim (if (hash-get opt 'zero) #\nul #\newline)))
            (let loop ((i 0))
              (when (< i count)
                (display (vector-ref shuffled i))
                (write-char delim)
                (loop (+ i 1))))))
        args
        'program: "shuf"
        'help: "Write a random permutation of the input lines to standard output."
        (flag 'echo "-e" "--echo"
          'help: "treat each ARG as an input line")
        (option 'input-range "-i" "--input-range"
          'help: "treat each number LO through HI as an input line"
          'default: #f)
        (option 'head-count "-n" "--head-count"
          'help: "output at most COUNT lines"
          'default: #f)
        (flag 'zero "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest))))

  (def (fisher-yates-shuffle vec)
    (let ((n (vector-length vec))
          (result (vector-copy vec)))
      (let loop ((i (- n 1)))
        (when (> i 0)
          (let ((j (secure-random-integer (+ i 1))))
            (let ((tmp (vector-ref result i)))
              (vector-set! result i (vector-ref result j))
              (vector-set! result j tmp)))
          (loop (- i 1))))
      result))

  (def (string-split-at str ch)
    (let ((len (string-length str)))
      (let loop ((i 0) (start 0) (acc '()))
        (cond
          ((>= i len)
           (reverse (cons (substring str start len) acc)))
          ((eqv? (string-ref str i) ch)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
          (else
           (loop (+ i 1) start acc))))))

  ) ;; end library

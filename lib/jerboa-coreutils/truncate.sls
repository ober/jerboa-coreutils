#!chezscheme
;;; truncate.sls -- Truncate files to specified size

(library (jerboa-coreutils truncate)
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

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-truncate (foreign-procedure "truncate" (string long) int))

  (def (parse-size str)
    ;; Parse size with optional +/-/< />/ % prefix and K/M/G suffix
    (let* ((len (string-length str))
           (mode (cond
                   ((and (> len 0) (eqv? (string-ref str 0) #\+)) 'extend)
                   ((and (> len 0) (eqv? (string-ref str 0) #\-)) 'reduce)
                   ((and (> len 0) (eqv? (string-ref str 0) #\<)) 'at-most)
                   ((and (> len 0) (eqv? (string-ref str 0) #\>)) 'at-least)
                   ((and (> len 0) (eqv? (string-ref str 0) #\/)) 'round-down)
                   ((and (> len 0) (eqv? (string-ref str 0) #\%)) 'round-up)
                   (else 'absolute)))
           (numstr (if (eq? mode 'absolute) str
                     (substring str 1 len)))
           (suffix-mult (parse-suffix numstr))
           (numpart (car suffix-mult))
           (mult (cdr suffix-mult))
           (n (string->number numpart)))
      (unless n
        (die "invalid number: '~a'" str))
      (values mode (* n mult))))

  (def (parse-suffix str)
    (let ((len (string-length str)))
      (if (= len 0)
        (cons str 1)
        (let ((last-ch (char-upcase (string-ref str (- len 1)))))
          (case last-ch
            ((#\K) (cons (substring str 0 (- len 1)) 1024))
            ((#\M) (cons (substring str 0 (- len 1)) (* 1024 1024)))
            ((#\G) (cons (substring str 0 (- len 1)) (* 1024 1024 1024)))
            ((#\T) (cons (substring str 0 (- len 1)) (* 1024 1024 1024 1024)))
            (else (cons str 1)))))))

  (def (get-file-size path)
    ;; Use Chez's file-length or stat
    (let ((p (open-file-input-port path)))
      (let ((sz (port-length p)))
        (close-port p)
        sz)))

  (def (main . args)
    (parameterize ((program-name "truncate"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing file operand"))
          (unless (hash-get opt 'size)
            (unless (hash-get opt 'reference)
              (die "you must specify either '--size=SIZE' or '--reference=FILE'")))
          (let-values (((mode size)
                        (if (hash-get opt 'size)
                          (parse-size (hash-ref opt 'size))
                          (values 'absolute
                            (get-file-size (hash-ref opt 'reference))))))
            (for-each
              (lambda (file)
                (with-catch
                  (lambda (e)
                    (warn "cannot open '~a' for writing: ~a" file (error-message e)))
                  (lambda ()
                    ;; Create file if it doesn't exist (unless -c)
                    (unless (or (hash-get opt 'no-create) (file-exists? file))
                      (close-port (open-output-file file)))
                    (when (file-exists? file)
                      (let* ((current-size (get-file-size file))
                             (new-size (case mode
                                         ((absolute) size)
                                         ((extend) (+ current-size size))
                                         ((reduce) (- current-size size))
                                         ((at-most) (min current-size size))
                                         ((at-least) (max current-size size))
                                         ((round-down)
                                          (if (zero? size) 0
                                            (* (quotient current-size size) size)))
                                         ((round-up)
                                          (if (zero? size) 0
                                            (* (ceiling (/ current-size size)) size)))
                                         (else size)))
                             (new-size (max 0 (inexact->exact (floor new-size)))))
                        (ffi-truncate file new-size))))))
              (hash-ref opt 'rest))))
        args
        'program: "truncate"
        'help: "Shrink or extend the size of each FILE to the specified size."
        (option 'size "-s" "--size"
          'help: "set or adjust the file size by SIZE bytes"
          'default: #f)
        (option 'reference "-r" "--reference"
          'help: "base size on RFILE"
          'default: #f)
        (flag 'no-create "-c" "--no-create"
          'help: "do not create any files")
        (rest-arguments 'rest))))

  ) ;; end library

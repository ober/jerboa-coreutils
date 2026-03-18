#!chezscheme
;;; mkfifo.sls -- Create named pipes

(library (jerboa-coreutils mkfifo)
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

  (define ffi-mkfifo (foreign-procedure "mkfifo" (string int) int))

  ;; Parse octal mode string like "0666" or "666" -> integer
  (def (parse-mode-str str default)
    (with-catch
      (lambda (e) default)
      (lambda ()
        (let* ((s (if (and (> (string-length str) 0)
                           (eqv? (string-ref str 0) #\0))
                    (substring str 1 (string-length str))
                    str))
               (n (string->number s 8)))
          (if n n default)))))

  (def (main . args)
    (parameterize ((program-name "mkfifo"))
      (call-with-getopt
        (lambda (_ opt)
            (when (null? (hash-ref opt 'rest))
              (die "missing operand"))
            (let ((mode (if (hash-get opt 'mode)
                          (parse-mode-str (hash-ref opt 'mode) #o666)
                          #o666)))
              (for-each
                (lambda (path)
                  (let ((rc (ffi-mkfifo path mode)))
                    (when (< rc 0)
                      (die "cannot create fifo '~a'" path))))
                (hash-ref opt 'rest))))
        args
        'program: "mkfifo"
        'help: "Create named pipes (FIFOs) with the given NAMEs."
        (option 'mode "-m" "--mode"
          'help: "set file permission bits to MODE, not a=rw - umask"
          'default: #f)
        (rest-arguments 'rest))))

  ) ;; end library

#!chezscheme
;;; dir.sls -- List directory contents in columnar format
;;; dir is equivalent to 'ls -C -b'

(library (jerboa-coreutils dir)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (filter-hidden lst)
    (cond
      ((null? lst) '())
      ((eqv? (string-ref (car lst) 0) #\.)
       (filter-hidden (cdr lst)))
      (else (cons (car lst) (filter-hidden (cdr lst))))))

  (def (list-directory dir show-all)
    (with-catch
      (lambda (e)
        (warn "cannot open directory '~a': ~a" dir (error-message e))
        '())
      (lambda ()
        (let* ((raw (directory-list dir))
               (names (if show-all
                        raw
                        (filter-hidden raw))))
          (list-sort string<? names)))))

  ;; Print names one per line (simplified columnar output)
  (def (print-names names)
    (for-each displayln names))

  (def (main . args)
    (parameterize ((program-name "dir"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((paths (if (null? (hash-ref opt 'rest)) '(".") (hash-ref opt 'rest))))
            (let ((show-header (> (length paths) 1)))
              (let loop ((dirs paths) (first #t))
                (unless (null? dirs)
                  (unless first (newline))
                  (when show-header
                    (displayln (car dirs) ":"))
                  (if (file-directory? (car dirs))
                    (print-names (list-directory (car dirs) (hash-get opt 'all)))
                    (displayln (car dirs)))
                  (loop (cdr dirs) #f))))))
        args
        'program: "dir"
        'help: "List directory contents."
        (flag 'all "-a" "--all"
          'help: "do not ignore entries starting with .")
        (rest-arguments 'rest))))

  ) ;; end library

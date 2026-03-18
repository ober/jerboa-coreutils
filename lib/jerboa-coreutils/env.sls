#!chezscheme
;;; env.sls -- Run command with modified environment

(library (jerboa-coreutils env)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            filter)
          (except (jerboa core) filter-map setenv)
          (only (std sugar) with-catch)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; FFI unsetenv for actually removing environment variables
  (define _load-ffi-env (begin (load-shared-object #f) (void)))
  (define ffi-unsetenv (foreign-procedure "unsetenv" (string) int))

  (def (main . args)
    (parameterize ((program-name "env"))
      (let loop ((args args)
                 (unsets '())
                 (sets '())
                 (ignore-env #f)
                 (chdir #f))
        (cond
          ((null? args)
           ;; No command: print environment
           (let ((env (if ignore-env '()
                        (get-environment-variables))))
             ;; Apply unsets
             (let ((env (filter (lambda (pair)
                                  (not (member (car pair) unsets)))
                                env)))
               ;; Apply sets
               (let ((env (append (map (lambda (s)
                                         (let ((eq (string-index s #\=)))
                                           (cons (substring s 0 eq)
                                                 (substring s (+ eq 1) (string-length s)))))
                                       sets)
                                  env)))
                 (for-each
                   (lambda (pair)
                     (display (car pair))
                     (display "=")
                     (displayln (cdr pair)))
                   env)))))
          ;; Options
          ((string=? (car args) "-i")
           (loop (cdr args) unsets sets #t chdir))
          ((string=? (car args) "--ignore-environment")
           (loop (cdr args) unsets sets #t chdir))
          ((and (>= (string-length (car args)) 2)
                (string=? (substring (car args) 0 2) "-u"))
           (let ((var (if (> (string-length (car args)) 2)
                        (substring (car args) 2 (string-length (car args)))
                        (if (pair? (cdr args)) (cadr args)
                          (die "option requires an argument -- 'u'")))))
             (loop (if (> (string-length (car args)) 2) (cdr args) (cddr args))
                   (cons var unsets) sets ignore-env chdir)))
          ((string=? (car args) "--unset")
           (if (pair? (cdr args))
             (loop (cddr args) (cons (cadr args) unsets) sets ignore-env chdir)
             (die "option '--unset' requires an argument")))
          ((and (>= (string-length (car args)) 2)
                (string=? (substring (car args) 0 2) "-C"))
           (let ((dir (if (> (string-length (car args)) 2)
                        (substring (car args) 2 (string-length (car args)))
                        (if (pair? (cdr args)) (cadr args)
                          (die "option requires an argument -- 'C'")))))
             (loop (if (> (string-length (car args)) 2) (cdr args) (cddr args))
                   unsets sets ignore-env dir)))
          ((string=? (car args) "--chdir")
           (if (pair? (cdr args))
             (loop (cddr args) unsets sets ignore-env (cadr args))
             (die "option '--chdir' requires an argument")))
          ((member (car args) '("--help" "-h"))
           (displayln "Usage: env [OPTION]... [-] [NAME=VALUE]... [COMMAND [ARG]...]")
           (displayln "Set each NAME to VALUE in the environment and run COMMAND."))
          ((member (car args) '("--version"))
           (version-info "env"))
          ((string=? (car args) "-")
           (loop (cdr args) unsets sets #t chdir))
          ((and (> (string-length (car args)) 0)
                (string-index (car args) #\=))
           ;; NAME=VALUE assignment
           (loop (cdr args) unsets (append sets (list (car args))) ignore-env chdir))
          (else
           ;; Command to run
           (when chdir
             (current-directory chdir))
           ;; Set up environment
           (when ignore-env
             (for-each (lambda (pair) (ffi-unsetenv (car pair)))
                       (get-environment-variables)))
           (for-each (lambda (var) (ffi-unsetenv var)) unsets)
           (for-each (lambda (s)
                       (let ((eq (string-index s #\=)))
                         (setenv (substring s 0 eq)
                                 (substring s (+ eq 1) (string-length s)))))
                     sets)
           ;; Exec via system process
           (let* ((cmd (car args))
                  (cmd-args (cdr args))
                  (cmd-str (string-join (cons cmd cmd-args) " ")))
             (let ((status (system cmd-str)))
               (exit status))))))))

  (def (string-index str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  (def (filter pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
      (else (filter pred (cdr lst)))))

  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  (def (get-environment-variables)
    ;; Chez Scheme doesn't have get-environment-variables directly,
    ;; use the one from jerboa core or parse /proc/self/environ
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let* ((p (open-file-input-port "/proc/self/environ"))
               (bv (get-bytevector-all p)))
          (close-port p)
          (if (eof-object? bv)
            '()
            (let ((str (utf8->string bv)))
              (filter-map
                (lambda (entry)
                  (let ((eq (string-index entry #\=)))
                    (if eq
                      (cons (substring entry 0 eq)
                            (substring entry (+ eq 1) (string-length entry)))
                      #f)))
                (string-split-nul str))))))))

  (def (string-split-nul str)
    (let ((len (string-length str)))
      (let loop ((i 0) (start 0) (acc '()))
        (cond
          ((>= i len)
           (reverse (if (> i start)
                      (cons (substring str start i) acc)
                      acc)))
          ((eqv? (string-ref str i) #\nul)
           (loop (+ i 1) (+ i 1)
                 (if (> i start)
                   (cons (substring str start i) acc)
                   acc)))
          (else
           (loop (+ i 1) start acc))))))

  (def (filter-map proc lst)
    (cond
      ((null? lst) '())
      (else
        (let ((result (proc (car lst))))
          (if result
            (cons result (filter-map proc (cdr lst)))
            (filter-map proc (cdr lst)))))))

  (def (setenv name value)
    (putenv name value))

  ) ;; end library

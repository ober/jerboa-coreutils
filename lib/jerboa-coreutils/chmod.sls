#!chezscheme
;;; chmod.sls -- Change file permissions

(library (jerboa-coreutils chmod)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            logbit?)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-chmod (foreign-procedure "chmod" (string int) int))

  (define ffi-lstat-mode
    (let ((lstat-fn (foreign-procedure "lstat" (string u8* int) int)))
      (lambda (path)
        (let ((buf (make-bytevector 256 0)))
          (let ((rc (lstat-fn path buf 256)))
            (if (< rc 0) -1
              ;; st_mode is at different offsets depending on platform
              ;; Use the system stat command as fallback
              (let ((mode-str
                     (with-catch
                       (lambda (e) #f)
                       (lambda ()
                         (let-values (((to-stdin from-stdout from-stderr pid)
                                       (open-process-ports
                                         (string-append "stat -c '%a' " path)
                                         (buffer-mode block)
                                         (native-transcoder))))
                           (close-port to-stdin)
                           (let ((result (get-line from-stdout)))
                             (close-port from-stdout)
                             (close-port from-stderr)
                             result))))))
                (if (and mode-str (not (eof-object? mode-str)))
                  (let ((n (string->number mode-str 8)))
                    (if n n -1))
                  -1))))))))

  (define ffi-stat-isdir
    (lambda (path)
      (if (file-directory? path) 1 0)))

  ;; Parse octal mode string
  (def (parse-octal-mode str)
    (string->number str 8))

  (def (string-split-commas str)
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (cons (substring str start i) acc)))
        ((eqv? (string-ref str i) #\,)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
        (else
         (loop (+ i 1) start acc)))))

  (def (logbit? bit n) (not (zero? (bitwise-and n (expt 2 bit)))))

  ;; Parse symbolic mode clause
  (def (apply-one-symbolic clause current-mode)
    (let* ((len (string-length clause))
           (who-end (let loop ((i 0))
                      (cond
                        ((>= i len) i)
                        ((or (eqv? (string-ref clause i) #\u)
                             (eqv? (string-ref clause i) #\g)
                             (eqv? (string-ref clause i) #\o)
                             (eqv? (string-ref clause i) #\a))
                         (loop (+ i 1)))
                        (else i)))))
      (if (>= who-end len)
        current-mode
        (let* ((who-str (substring clause 0 who-end))
               (op (string-ref clause who-end))
               (perm-str (if (< (+ who-end 1) len)
                           (substring clause (+ who-end 1) len)
                           ""))
               (who-bits (cond
                           ((string=? who-str "") 7)
                           (else
                            (let loop ((i 0) (m 0))
                              (if (>= i (string-length who-str))
                                m
                                (loop (+ i 1)
                                      (bitwise-ior m
                                              (case (string-ref who-str i)
                                                ((#\u) 1)
                                                ((#\g) 2)
                                                ((#\o) 4)
                                                ((#\a) 7)
                                                (else 0)))))))))
               (perm-val (let loop ((i 0) (b 0))
                           (if (>= i (string-length perm-str))
                             b
                             (loop (+ i 1)
                                   (bitwise-ior b
                                           (case (string-ref perm-str i)
                                             ((#\r) 4)
                                             ((#\w) 2)
                                             ((#\x) 1)
                                             (else 0)))))))
               (add-mask
                (bitwise-ior
                 (if (logbit? 0 who-bits) (* perm-val #o100) 0)
                 (if (logbit? 1 who-bits) (* perm-val #o10) 0)
                 (if (logbit? 2 who-bits) perm-val 0))))
          (case op
            ((#\+) (bitwise-ior current-mode add-mask))
            ((#\-) (bitwise-and current-mode (bitwise-not add-mask)))
            ((#\=)
             (let ((clear-mask
                    (bitwise-ior
                     (if (logbit? 0 who-bits) #o700 0)
                     (if (logbit? 1 who-bits) #o070 0)
                     (if (logbit? 2 who-bits) #o007 0))))
               (bitwise-ior (bitwise-and current-mode (bitwise-not clear-mask)) add-mask)))
            (else current-mode))))))

  (def (apply-symbolic-mode mode-str current-mode)
    (let loop ((parts (string-split-commas mode-str)) (mode current-mode))
      (if (null? parts)
        mode
        (loop (cdr parts) (apply-one-symbolic (car parts) mode)))))

  (def (parse-mode str current-mode)
    (if (and (> (string-length str) 0)
             (char<=? #\0 (string-ref str 0))
             (char<=? (string-ref str 0) #\7))
      (parse-octal-mode str)
      (apply-symbolic-mode str current-mode)))

  (def (do-chmod path mode-str verbose changes-only recursive)
    (let* ((cur-mode (ffi-lstat-mode path)))
      (cond
        ((< cur-mode 0)
         (warn "cannot access '~a': No such file or directory" path))
        (else
         (let ((new-mode (parse-mode mode-str cur-mode)))
           (if (not new-mode)
             (warn "invalid mode: '~a'" mode-str)
             (let ((rc (ffi-chmod path new-mode)))
               (cond
                 ((< rc 0)
                  (warn "cannot chmod '~a'" path))
                 (verbose
                  (when (or (not changes-only) (not (= cur-mode new-mode)))
                    (displayln "mode of '" path "' changed from "
                               (number->string cur-mode 8) " to "
                               (number->string new-mode 8)))))))))))
    ;; Recurse if directory
    (when (and recursive (= (ffi-stat-isdir path) 1))
      (with-catch
        (lambda (e) (warn "cannot open directory '~a'" path))
        (lambda ()
          (let ((files (directory-list path)))
            (for-each
              (lambda (name)
                (do-chmod (string-append path "/" name)
                          mode-str verbose changes-only recursive))
              files))))))

  (def (main . args)
    (parameterize ((program-name "chmod"))
      (call-with-getopt
        (lambda (_ opt)
          (when (null? (hash-ref opt 'rest))
            (die "missing operand"))
          (let ((mode-str (car (hash-ref opt 'rest)))
                (files (cdr (hash-ref opt 'rest))))
            (when (null? files)
              (die "missing operand after '~a'" mode-str))
            (for-each
              (lambda (file)
                (do-chmod file mode-str (hash-get opt 'verbose) (hash-get opt 'changes) (hash-get opt 'recursive)))
              files)))
        args
        'program: "chmod"
        'help: "Change the file mode bits of each given file according to MODE."
        (flag 'recursive "-R" "--recursive"
          'help: "change files and directories recursively")
        (flag 'verbose "-v" "--verbose"
          'help: "output a diagnostic for every file processed")
        (flag 'changes "-c" "--changes"
          'help: "like verbose but report only when a change is made")
        (rest-arguments 'rest))))

  ) ;; end library

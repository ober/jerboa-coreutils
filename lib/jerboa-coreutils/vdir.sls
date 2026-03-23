#!chezscheme
;;; vdir.sls -- List directory contents in long format
;;; vdir is equivalent to 'ls -l -b'

(library (jerboa-coreutils vdir)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-lstat (foreign-procedure "coreutils_ls_lstat" (string int) int))
  (define ffi-stat-get (foreign-procedure "coreutils_ls_stat_get" (int) long-long))
  (define ffi-readlink (foreign-procedure "coreutils_ls_readlink" (string) string))
  (define ffi-time-format (foreign-procedure "coreutils_time_format" (long) string))
  (define ffi-uid-to-name (foreign-procedure "coreutils_uid_to_name" (int) string))
  (define ffi-gid-to-name (foreign-procedure "coreutils_gid_to_name" (int) string))

  ;; Entry: #(name full-path mode nlink uid gid size mtime blocks)

  (def (make-entry name dir)
    (let ((full (if (string=? dir ".")
                  name
                  (string-append dir "/" name))))
      (let ((rc (ffi-lstat full 0)))
        (if (< rc 0)
          #f
          (let ((mode (ffi-stat-get 0))
                (nlink (ffi-stat-get 1))
                (uid (ffi-stat-get 2))
                (gid (ffi-stat-get 3))
                (size (ffi-stat-get 4))
                (mtime (ffi-stat-get 6))
                (blocks (ffi-stat-get 9)))
            (vector name full mode nlink uid gid size mtime blocks))))))

  (def (mode->rwx mode)
    (let* ((p (bitwise-and mode #o7777))
           (b (lambda (bit ch) (if (not (zero? (bitwise-and p bit))) ch #\-))))
      (string
       (cond
         ((= (bitwise-and mode #o170000) #o140000) #\s)
         ((= (bitwise-and mode #o170000) #o120000) #\l)
         ((= (bitwise-and mode #o170000) #o100000) #\-)
         ((= (bitwise-and mode #o170000) #o060000) #\b)
         ((= (bitwise-and mode #o170000) #o040000) #\d)
         ((= (bitwise-and mode #o170000) #o020000) #\c)
         ((= (bitwise-and mode #o170000) #o010000) #\p)
         (else #\?))
       (b #o400 #\r) (b #o200 #\w)
       (if (not (zero? (bitwise-and p #o4000)))
         (if (not (zero? (bitwise-and p #o100))) #\s #\S)
         (b #o100 #\x))
       (b #o040 #\r) (b #o020 #\w)
       (if (not (zero? (bitwise-and p #o2000)))
         (if (not (zero? (bitwise-and p #o010))) #\s #\S)
         (b #o010 #\x))
       (b #o004 #\r) (b #o002 #\w)
       (if (not (zero? (bitwise-and p #o1000)))
         (if (not (zero? (bitwise-and p #o001))) #\t #\T)
         (b #o001 #\x)))))

  (def (left-pad str width)
    (let ((len (string-length str)))
      (if (>= len width) str
        (string-append (make-string (- width len) #\space) str))))

  (def (right-pad str width)
    (let ((len (string-length str)))
      (if (>= len width) str
        (string-append str (make-string (- width len) #\space)))))

  ;; Format time like ls: recent files show HH:MM, old files show year
  (def (format-time epoch)
    (or (ffi-time-format epoch) "?"))

  ;; Look up username by uid
  (def (uid->name uid)
    (or (ffi-uid-to-name uid) (number->string uid)))

  ;; Look up group name by gid
  (def (gid->name gid)
    (or (ffi-gid-to-name gid) (number->string gid)))

  ;; Read symlink target
  (def (read-symlink path)
    (ffi-readlink path))

  (def (filter-hidden lst)
    (cond
      ((null? lst) '())
      ((eqv? (string-ref (car lst) 0) #\.)
       (filter-hidden (cdr lst)))
      (else (cons (car lst) (filter-hidden (cdr lst))))))

  (def (filter-map-entries proc lst)
    (cond
      ((null? lst) '())
      (else
       (let ((r (proc (car lst))))
         (if r
           (cons r (filter-map-entries proc (cdr lst)))
           (filter-map-entries proc (cdr lst)))))))

  (def (list-directory dir show-all)
    (with-catch
      (lambda (e)
        (warn "cannot open directory '~a': ~a" dir (error-message e))
        '())
      (lambda ()
        (let* ((raw (directory-list dir))
               (names (if show-all
                        raw
                        (filter-hidden raw)))
               (sorted (list-sort string-ci<? names)))
          (filter-map-entries
            (lambda (n) (make-entry n dir))
            sorted)))))

  (def (print-long-format entries)
    (unless (null? entries)
      (let* ((total-blocks (apply + (map (lambda (e)
                                            (quotient (+ (vector-ref e 8) 1) 2))
                                          entries))))
        (displayln "total " total-blocks))
      (let* ((nlink-w (apply max 1 (map (lambda (e)
                        (string-length (number->string (vector-ref e 3)))) entries)))
             (owner-strs (map (lambda (e) (uid->name (vector-ref e 4))) entries))
             (group-strs (map (lambda (e) (gid->name (vector-ref e 5))) entries))
             (owner-w (apply max 1 (map string-length owner-strs)))
             (group-w (apply max 1 (map string-length group-strs)))
             (size-w (apply max 1 (map (lambda (e)
                       (string-length (number->string (vector-ref e 6)))) entries))))
        (let loop ((es entries) (os owner-strs) (gs group-strs))
          (unless (null? es)
            (let* ((e (car es))
                   (mode (vector-ref e 2))
                   (is-link (= (bitwise-and mode #o170000) #o120000)))
              (display (mode->rwx mode))
              (display " ")
              (display (left-pad (number->string (vector-ref e 3)) nlink-w))
              (display " ")
              (display (right-pad (car os) owner-w))
              (display " ")
              (display (right-pad (car gs) group-w))
              (display " ")
              (display (left-pad (number->string (vector-ref e 6)) size-w))
              (display " ")
              (display (format-time (vector-ref e 7)))
              (display " ")
              (display (vector-ref e 0))
              (when is-link
                (let ((target (read-symlink (vector-ref e 1))))
                  (when target
                    (display " -> ")
                    (display target))))
              (newline))
            (loop (cdr es) (cdr os) (cdr gs)))))))

  (def (main . args)
    (init-security!)
    (parameterize ((program-name "vdir"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((paths (if (null? (hash-ref opt 'rest)) '(".") (hash-ref opt 'rest))))
            (let ((show-header (> (length paths) 1)))
              (let loop ((dirs paths) (first #t))
                (unless (null? dirs)
                  (unless first (newline))
                  (when show-header
                    (displayln (car dirs) ":"))
                  (print-long-format (list-directory (car dirs) (hash-get opt 'all)))
                  (loop (cdr dirs) #f))))))
        args
        'program: "vdir"
        'help: "List directory contents in long format."
        (flag 'all "-a" "--all"
          'help: "do not ignore entries starting with .")
        (rest-arguments 'rest))))

  ) ;; end library

#!chezscheme
;;; du.sls -- Disk usage

(library (jerboa-coreutils du)
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
  (define ffi-du-stat (foreign-procedure "coreutils_du_stat" (string int) long))

  ;; Get file info via FFI stat: returns (isdir size blocks islink) or #f
  (def (du-stat-info path)
    (let ((isdir (ffi-du-stat path 0)))
      (if (< isdir 0)
        #f
        (let ((size (ffi-du-stat path 1))
              (blocks (ffi-du-stat path 2))
              (islink (ffi-du-stat path 3)))
          (list isdir size blocks islink)))))

  (def (human-readable-size bytes)
    (cond
      ((>= bytes (* 1024 1024 1024))
       (string-append (number->string (quotient bytes (* 1024 1024 1024))) "G"))
      ((>= bytes (* 1024 1024))
       (string-append (number->string (quotient bytes (* 1024 1024))) "M"))
      ((>= bytes 1024)
       (string-append (number->string (quotient bytes 1024)) "K"))
      (else
       (string-append (number->string bytes) "B"))))

  (def (blocks->display blocks human kb bytes-mode)
    (cond
      (bytes-mode (number->string (* blocks 512)))
      (human (human-readable-size (* blocks 512)))
      (kb (number->string (quotient (* blocks 512) 1024)))
      (else (number->string (quotient (* blocks 512) 1024)))))

  (def (glob-matches? pattern name)
    (let loop ((pi 0) (ni 0))
      (cond
        ((and (>= pi (string-length pattern)) (>= ni (string-length name))) #t)
        ((>= pi (string-length pattern)) #f)
        ((eqv? (string-ref pattern pi) #\*)
         (let try ((ni2 ni))
           (if (loop (+ pi 1) ni2) #t
             (if (< ni2 (string-length name)) (try (+ ni2 1)) #f))))
        ((>= ni (string-length name)) #f)
        ((eqv? (string-ref pattern pi) (string-ref name ni))
         (loop (+ pi 1) (+ ni 1)))
        (else #f))))

  (def (any-matches? patterns name)
    (let loop ((ps patterns))
      (if (null? ps) #f
        (if (glob-matches? (car ps) name) #t
          (loop (cdr ps))))))

  (def (du-path path depth max-depth show-all human kb bytes-mode exclude-pats)
    (let ((info (du-stat-info path)))
      (if (not info)
        0
        (let ((isdir (list-ref info 0))
              (blocks (list-ref info 2))
              (islink (list-ref info 3)))
          (if (or (= islink 1) (= isdir 0))
            ;; Regular file or symlink
            (let ((b (max 0 blocks)))
              (when (and show-all
                         (or (not max-depth) (<= depth max-depth)))
                (displayln (blocks->display b human kb bytes-mode) "\t" path))
              b)
            ;; Directory
            (let ((total blocks))
              (let ((subtotal
                     (with-catch
                       (lambda (e) 0)
                       (lambda ()
                         (let ((files (directory-list path)))
                           (let loop ((fs files) (acc 0))
                             (if (null? fs) acc
                               (let* ((name (car fs))
                                      (child (string-append path "/" name))
                                      (excluded (any-matches? exclude-pats name)))
                                 (if excluded
                                   (loop (cdr fs) acc)
                                   (let ((sub (du-path child (+ depth 1) max-depth
                                                       show-all human kb bytes-mode
                                                       exclude-pats)))
                                     (loop (cdr fs) (+ acc sub))))))))))))
                (let ((dir-total (+ total subtotal)))
                  (when (or (not max-depth) (<= depth max-depth))
                    (displayln (blocks->display dir-total human kb bytes-mode) "\t" path))
                  dir-total))))))))

  (def (main . args)
    (init-security!)
    (parameterize ((program-name "du"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((files (if (null? (hash-ref opt 'rest)) '(".") (hash-ref opt 'rest)))
                 (max-depth (if (hash-get opt 'max-depth)
                              (let ((n (string->number (hash-ref opt 'max-depth))))
                                (if n (inexact->exact n) #f))
                              (if (hash-get opt 'summarize) 0 #f)))
                 (exclude-pats (if (hash-get opt 'exclude) (list (hash-ref opt 'exclude)) '()))
                 (show-all (hash-get opt 'all-files))
                 (human (hash-get opt 'human-readable))
                 (kb (hash-get opt 'kilobytes))
                 (bytes-mode (hash-get opt 'bytes)))
            (let ((total
                   (let loop ((fs files) (acc 0))
                     (if (null? fs) acc
                       (loop (cdr fs)
                             (+ acc (du-path (car fs) 0 max-depth
                                             show-all human kb bytes-mode
                                             exclude-pats)))))))
              (when (hash-get opt 'total)
                (displayln (blocks->display total human kb bytes-mode) "\ttotal")))))
        args
        'program: "du"
        'help: "Summarize disk usage of the set of FILEs, recursively for directories."
        (flag 'all-files "-a" "--all"
          'help: "write counts for all files, not just directories")
        (flag 'bytes "-b" "--bytes"
          'help: "equivalent to '--apparent-size --block-size=1'")
        (flag 'total "-c" "--total"
          'help: "produce a grand total")
        (flag 'human-readable "--human-readable"
          'help: "print sizes in human readable format")
        (flag 'kilobytes "-k"
          'help: "like --block-size=1K")
        (flag 'summarize "-s" "--summarize"
          'help: "display only a total for each argument")
        (option 'max-depth "-d" "--max-depth"
          'help: "print the total for a directory only if it is N or fewer levels below"
          'default: #f)
        (option 'exclude "--exclude"
          'help: "exclude files that match PATTERN"
          'default: #f)
        (rest-arguments 'rest))))

  ) ;; end library

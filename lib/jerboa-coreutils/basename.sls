#!chezscheme
;;; basename.sls -- Strip directory and suffix from filenames

(library (jerboa-coreutils basename)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "basename"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((names (hash-ref opt 'rest))
                (suffix (or (hash-get opt 'suffix) ""))
                (delim (if (hash-get opt 'zero) #\nul #\newline)))
            (cond
              ((null? names)
               (die "missing operand"))
              ((or (hash-get opt 'multiple) (hash-get opt 'suffix))
               (for-each
                 (lambda (name)
                   (display (strip-basename name suffix))
                   (write-char delim))
                 names))
              ((and (= (length names) 2) (string=? suffix ""))
               ;; basename NAME SUFFIX form
               (display (strip-basename (car names) (cadr names)))
               (write-char delim))
              ((= (length names) 1)
               (display (strip-basename (car names) suffix))
               (write-char delim))
              (else
               (die "extra operand '~a'" (cadr names))))))
        args
        'program: "basename"
        'help: "Print NAME with any leading directory components removed. If specified, also remove a trailing SUFFIX."
        (flag 'multiple "-a" "--multiple"
          'help: "support multiple arguments and treat each as a NAME")
        (option 'suffix "-s" "--suffix"
          'help: "remove a trailing SUFFIX; implies -a"
          'default: #f)
        (flag 'zero "-z" "--zero"
          'help: "end each output line with NUL, not newline")
        (rest-arguments 'rest))))

  ;; Strip directory and optional suffix from name
  (def (strip-basename name suffix)
    (let* (;; Remove trailing slashes
           (name (let loop ((n name))
                   (if (and (> (string-length n) 1)
                            (eqv? (string-ref n (- (string-length n) 1)) #\/))
                     (loop (substring n 0 (- (string-length n) 1)))
                     n)))
           ;; Get last component
           (base (cond
                   ((string=? name "/") "/")
                   (else
                     (let ((pos (string-last-index-of name #\/)))
                       (if pos
                         (substring name (+ pos 1) (string-length name))
                         name))))))
      ;; Remove suffix if applicable
      (if (and (> (string-length suffix) 0)
               (> (string-length base) (string-length suffix))
               (string=? (substring base
                           (- (string-length base) (string-length suffix))
                           (string-length base))
                         suffix))
        (substring base 0 (- (string-length base) (string-length suffix)))
        base)))

  (def (string-last-index-of str ch)
    (let loop ((i (- (string-length str) 1)))
      (cond
        ((< i 0) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (- i 1))))))

  ) ;; end library

#!chezscheme
;;; dirname.sls -- Strip last component from file name

(library (jerboa-coreutils dirname)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (def (main . args)
    (parameterize ((program-name "dirname"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((names (hash-ref opt 'rest))
                (delim (if (hash-get opt 'zero) #\nul #\newline)))
            (when (null? names)
              (die "missing operand"))
            (for-each
              (lambda (name)
                (display (compute-dirname name))
                (write-char delim))
              names)))
        args
        'program: "dirname"
        'help: "Output each NAME with its last non-slash component and trailing slashes removed; if NAME contains no /'s, output '.' (meaning the current directory)."
        (flag 'zero "-z" "--zero"
          'help: "end each output line with NUL, not newline")
        (rest-arguments 'rest))))

  (def (compute-dirname name)
    ;; Remove trailing slashes
    (let* ((name (let loop ((n name))
                   (if (and (> (string-length n) 1)
                            (eqv? (string-ref n (- (string-length n) 1)) #\/))
                     (loop (substring n 0 (- (string-length n) 1)))
                     n)))
           (pos (string-last-index-of name #\/)))
      (cond
        ((not pos) ".")
        ((= pos 0) "/")
        (else
          ;; Remove trailing slashes from result
          (let loop ((end pos))
            (if (and (> end 1) (eqv? (string-ref name (- end 1)) #\/))
              (loop (- end 1))
              (substring name 0 end)))))))

  (def (string-last-index-of str ch)
    (let loop ((i (- (string-length str) 1)))
      (cond
        ((< i 0) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (- i 1))))))

  ) ;; end library

#!chezscheme
;;; mknod.sls -- Create block/character devices

(library (jerboa-coreutils mknod)
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

  ;; mknod(path, mode, dev) -- we need to combine type + mode + makedev(major,minor)
  ;; Since makedev is a macro, we shell out to /usr/bin/mknod for the actual work
  (def (do-mknod name type-str mode major minor)
    (let* ((cmd (string-append "/usr/bin/mknod"
                  " -m " (number->string mode 8)
                  " " (shell-quote name)
                  " " type-str
                  (if (member type-str '("b" "c" "u"))
                    (string-append " " (number->string major) " " (number->string minor))
                    "")))
           (status (system cmd)))
      (unless (zero? status)
        (die "cannot create special file '~a'" name))))

  (def (shell-quote s)
    (string-append "'" (string-replace-all s "'" "'\\''") "'"))

  (def (string-replace-all str old new)
    (let ((olen (string-length old))
          (out (open-output-string)))
      (let loop ((i 0))
        (cond
          ((> (+ i olen) (string-length str))
           (display (substring str i (string-length str)) out)
           (get-output-string out))
          ((string=? (substring str i (+ i olen)) old)
           (display new out)
           (loop (+ i olen)))
          (else
           (write-char (string-ref str i) out)
           (loop (+ i 1)))))))

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
    (parameterize ((program-name "mknod"))
      (call-with-getopt
        (lambda (_ opt)
            (let ((rest (hash-ref opt 'rest)))
              (when (< (length rest) 2)
                (die "missing operand"))
              (let* ((name (list-ref rest 0))
                     (type-str (list-ref rest 1))
                     (mode (if (hash-get opt 'mode)
                             (parse-mode-str (hash-ref opt 'mode) #o666)
                             #o666)))
                (cond
                  ((member type-str '("b" "c" "u"))
                   (when (< (length rest) 4)
                     (die "special files require major and minor device numbers"))
                   (let* ((major-str (list-ref rest 2))
                          (minor-str (list-ref rest 3))
                          (major (string->number major-str))
                          (minor (string->number minor-str)))
                     (unless (and major minor)
                       (die "invalid device number"))
                     (do-mknod name type-str mode
                               (inexact->exact major)
                               (inexact->exact minor))))
                  ((string=? type-str "p")
                   (do-mknod name type-str mode 0 0))
                  (else
                   (die "invalid argument '~a' for type" type-str))))))
        args
        'program: "mknod"
        'help: "Create the special file NAME of the given TYPE."
        (option 'mode "-m" "--mode"
          'help: "set file permission bits to MODE, not a=rw - umask"
          'default: #f)
        (rest-arguments 'rest))))

  ) ;; end library

#!chezscheme
;;; who.sls -- Show who is logged in (delegates to /usr/bin/who)

(library (jerboa-coreutils who)
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

  (def (run-who who-args)
    (with-catch
      (lambda (e)
        (die "cannot run /usr/bin/who: ~a" (error-message e)))
      (lambda ()
        (let ((cmd (string-append "/usr/bin/who"
                     (let loop ((args who-args) (acc ""))
                       (if (null? args) acc
                         (loop (cdr args)
                               (string-append acc " " (shell-quote (car args)))))))))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((lines (let loop ((acc '()))
                           (let ((line (get-line from-stdout)))
                             (if (eof-object? line)
                               (reverse acc)
                               (loop (cons line acc)))))))
              (close-port from-stdout)
              (close-port from-stderr)
              (for-each displayln lines)))))))

  (def (main . args)
    (parameterize ((program-name "who"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((who-args
                 (append
                   (if (hash-get opt 'boot) '("-b") '())
                   (if (hash-get opt 'count) '("-q") '())
                   (if (hash-get opt 'heading) '("-H") '())
                   (if (hash-get opt 'login) '("-l") '())
                   (if (hash-get opt 'mesg) '("-T") '())
                   (if (hash-get opt 'all) '("-a") '())
                   (hash-ref opt 'rest))))
            (run-who who-args)))
        args
        'program: "who"
        'help: "Print information about users who are currently logged in."
        (flag 'all "-a" "--all"
          'help: "same as -b -d --login -p -r -t -T -u")
        (flag 'boot "-b" "--boot"
          'help: "time of last system boot")
        (flag 'count "-q" "--count"
          'help: "all login names and number of users logged on")
        (flag 'heading "-H" "--heading"
          'help: "print line of column headings")
        (flag 'login "-l" "--login"
          'help: "print system login processes")
        (flag 'mesg "-T" "--mesg"
          'help: "add user's message status as +, - or ?")
        (rest-arguments 'rest))))

  ) ;; end library

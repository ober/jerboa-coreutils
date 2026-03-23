#!chezscheme
;;; uname.sls -- Print system information

(library (jerboa-coreutils uname)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Use /usr/bin/uname to get system info since we can't easily
  ;; call uname() with struct via Chez foreign-procedure
  (def (run-uname flag)
    (with-catch
      (lambda (e) "unknown")
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "/usr/bin/uname " flag)
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((result (get-line from-stdout)))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result) "unknown" result))))))

  (def (uname-sysname)  (run-uname "-s"))
  (def (uname-nodename) (run-uname "-n"))
  (def (uname-release)  (run-uname "-r"))
  (def (uname-version)  (run-uname "-v"))
  (def (uname-machine)  (run-uname "-m"))

  (def (main . args)
    (parameterize ((program-name "uname"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((parts '())
                (all? (hash-get opt 'all)))
            (when (or all? (hash-get opt 'kernel-name))
              (set! parts (cons (uname-sysname) parts)))
            (when (or all? (hash-get opt 'nodename))
              (set! parts (cons (uname-nodename) parts)))
            (when (or all? (hash-get opt 'kernel-release))
              (set! parts (cons (uname-release) parts)))
            (when (or all? (hash-get opt 'kernel-version))
              (set! parts (cons (uname-version) parts)))
            (when (or all? (hash-get opt 'machine))
              (set! parts (cons (uname-machine) parts)))
            ;; -p processor and -i hardware platform (same as -m on Linux)
            (when all?
              (set! parts (cons (uname-machine) parts))
              (set! parts (cons (uname-machine) parts)))
            (when (or all? (hash-get opt 'operating-system))
              (set! parts (cons "GNU/Linux" parts)))
            ;; Default: just sysname
            (when (null? parts)
              (set! parts (list (uname-sysname))))
            (displayln (string-join (reverse parts) " "))))
        args
        'program: "uname"
        'help: "Print certain system information."
        (flag 'all "-a" "--all"
          'help: "print all information")
        (flag 'kernel-name "-s" "--kernel-name"
          'help: "print the kernel name")
        (flag 'nodename "-n" "--nodename"
          'help: "print the network node hostname")
        (flag 'kernel-release "-r" "--kernel-release"
          'help: "print the kernel release")
        (flag 'kernel-version "-v" "--kernel-version"
          'help: "print the kernel version")
        (flag 'machine "-m" "--machine"
          'help: "print the machine hardware name")
        (flag 'operating-system "-o" "--operating-system"
          'help: "print the operating system"))))

  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  ) ;; end library

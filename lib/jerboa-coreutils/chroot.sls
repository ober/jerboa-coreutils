#!chezscheme
;;; chroot.sls -- Change root directory

(library (jerboa-coreutils chroot)
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

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-chroot (foreign-procedure "chroot" (string) int))
  (define ffi-chdir (foreign-procedure "chdir" (string) int))

  (def (do-chroot newroot command args)
    (let ((rc (ffi-chroot newroot)))
      (when (< rc 0)
        (die "cannot change root directory to '~a'" newroot))
      (let ((rc2 (ffi-chdir "/")))
        (when (< rc2 0)
          (die "cannot change directory to /")))
      ;; exec the command via system
      (let* ((cmd-str (string-join (cons command args) " "))
             (status (system cmd-str)))
        (exit status))))

  (def (main . args)
    (parameterize ((program-name "chroot"))
      (init-security!)
      (install-process-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (cond
            ((null? (hash-ref opt 'rest))
             (die "missing operand"))
            (else
             (let* ((newroot (car (hash-ref opt 'rest)))
                    (rest-args (cdr (hash-ref opt 'rest)))
                    (command (if (pair? rest-args)
                               (car rest-args)
                               "/bin/sh"))
                    (cmd-args (if (pair? rest-args)
                                (cdr rest-args)
                                (if (string? (hash-get opt 'shell))
                                  (list "-i")
                                  '("-i"))))
                    (userspec (hash-get opt 'userspec))
                    (groups (hash-get opt 'groups)))
               ;; Handle --userspec: user:group
               (when userspec
                 (warn "userspec option not fully implemented"))
               (do-chroot newroot command cmd-args)))))
        args
        'program: "chroot"
        'help: "Run COMMAND with root directory set to NEWROOT."
        (option 'userspec "--userspec"
          'help: "specify user and group (ID or name) to use"
          'default: #f)
        (option 'groups "--groups"
          'help: "specify supplementary groups as g1,g2,..,gN"
          'default: #f)
        (option 'shell "--shell"
          'help: "use SHELL instead of default /bin/sh"
          'default: #f)
        (rest-arguments 'rest))))

  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  ) ;; end library

#!chezscheme
;;; common/security.sls -- Shared security infrastructure for jerboa-coreutils
;;;
;;; Centralizes security integration so individual commands can opt-in
;;; to Landlock, seccomp, capabilities, audit logging, and taint tracking
;;; with minimal boilerplate.

(library (jerboa-coreutils common security)
  (export
    ;; Seccomp profiles (pre-built for coreutils use cases)
    install-readonly-seccomp!
    install-io-seccomp!
    install-process-seccomp!

    ;; Landlock presets
    install-proc-only-landlock!
    install-readonly-landlock!
    install-readwrite-landlock!

    ;; Capability enforcement
    with-fs-read-capability
    with-fs-write-capability
    with-process-capability
    check-fs-read!
    check-fs-write!

    ;; Audit logging
    coreutils-audit-logger
    audit-file-access!
    audit-file-modify!
    audit-file-delete!
    audit-process-spawn!
    audit-process-signal!

    ;; Taint helpers
    taint-argv-path
    untaint-after-sanitize

    ;; Path sanitization (re-export from std)
    sanitize-path
    safe-path-join/checked

    ;; Secret wiping
    with-sensitive-buffer

    ;; Combined security initialization
    init-security!
    security-available?)

  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf))

  ;; ========== Feature Detection ==========
  ;; Gracefully degrade when security modules aren't available or
  ;; when the kernel doesn't support the feature.

  (define *seccomp-available* #f)
  (define *landlock-available* #f)
  (define *audit-logger* #f)

  (define (security-available?)
    (or *seccomp-available* *landlock-available*))

  ;; ========== Lazy Loading ==========
  ;; We use dynamic loading so the security modules are optional.
  ;; If jerboa stdlib isn't available, security features silently degrade.

  ;; --- Seccomp ---

  (define seccomp-install-proc #f)
  (define seccomp-make-filter-proc #f)
  (define seccomp-kill-action #f)

  (define (load-seccomp!)
    (with-catch
      (lambda (e) (set! *seccomp-available* #f))
      (lambda ()
        (let ((env (environment '(std security seccomp))))
          (set! seccomp-install-proc (eval 'seccomp-install! env))
          (set! seccomp-make-filter-proc (eval 'make-seccomp-filter env))
          (set! seccomp-kill-action (eval 'seccomp-kill env))
          (set! *seccomp-available*
            ((eval 'seccomp-available? env)))))))

  ;; Syscall lists for different profiles
  (define readonly-syscalls
    '(read write close fstat mmap mprotect munmap brk
      rt_sigaction rt_sigprocmask rt_sigreturn
      futex nanosleep getrandom arch_prctl set_tid_address
      set_robust_list sched_yield sigaltstack
      clock_gettime prctl prlimit64 rseq close_range
      openat newfstatat getdents access getcwd fcntl
      ioctl exit_group))

  (define io-syscalls
    '(read write close fstat mmap mprotect munmap brk
      rt_sigaction rt_sigprocmask rt_sigreturn
      futex nanosleep getrandom arch_prctl set_tid_address
      set_robust_list sched_yield sigaltstack
      clock_gettime prctl prlimit64 rseq close_range
      openat newfstatat getdents access getcwd fcntl
      ioctl exit_group
      ;; File modification
      rename mkdir rmdir creat link unlink
      ftruncate))

  (define process-syscalls
    '(read write close fstat mmap mprotect munmap brk
      rt_sigaction rt_sigprocmask rt_sigreturn
      futex nanosleep getrandom arch_prctl set_tid_address
      set_robust_list sched_yield sigaltstack
      clock_gettime prctl prlimit64 rseq close_range
      openat newfstatat getdents access getcwd fcntl
      ioctl exit_group
      ;; Process management
      clone fork execve wait4 kill getpid getppid))

  (define (install-readonly-seccomp!)
    (when *seccomp-available*
      (with-catch
        (lambda (e)
          (eprintf "warning: seccomp install failed: ~a~n"
                   (if (message-condition? e) (condition-message e) e)))
        (lambda ()
          (seccomp-install-proc
            (apply seccomp-make-filter-proc seccomp-kill-action
                   readonly-syscalls))))))

  (define (install-io-seccomp!)
    (when *seccomp-available*
      (with-catch
        (lambda (e)
          (eprintf "warning: seccomp install failed: ~a~n"
                   (if (message-condition? e) (condition-message e) e)))
        (lambda ()
          (seccomp-install-proc
            (apply seccomp-make-filter-proc seccomp-kill-action
                   io-syscalls))))))

  (define (install-process-seccomp!)
    (when *seccomp-available*
      (with-catch
        (lambda (e)
          (eprintf "warning: seccomp install failed: ~a~n"
                   (if (message-condition? e) (condition-message e) e)))
        (lambda ()
          (seccomp-install-proc
            (apply seccomp-make-filter-proc seccomp-kill-action
                   process-syscalls))))))

  ;; --- Landlock ---

  (define landlock-make-ruleset-proc #f)
  (define landlock-add-read-only-proc #f)
  (define landlock-add-read-write-proc #f)
  (define landlock-add-execute-proc #f)
  (define landlock-install-proc #f)

  (define (load-landlock!)
    (with-catch
      (lambda (e) (set! *landlock-available* #f))
      (lambda ()
        (let ((env (environment '(std security landlock))))
          (set! landlock-make-ruleset-proc (eval 'make-landlock-ruleset env))
          (set! landlock-add-read-only-proc (eval 'landlock-add-read-only! env))
          (set! landlock-add-read-write-proc (eval 'landlock-add-read-write! env))
          (set! landlock-add-execute-proc (eval 'landlock-add-execute! env))
          (set! landlock-install-proc (eval 'landlock-install! env))
          (set! *landlock-available*
            ((eval 'landlock-available? env)))))))

  (define (install-proc-only-landlock!)
    ;; Restrict to /proc, /sys, /dev/null, /dev/tty (for top, uptime, etc.)
    (when *landlock-available*
      (with-catch
        (lambda (e)
          (eprintf "warning: landlock install failed: ~a~n"
                   (if (message-condition? e) (condition-message e) e)))
        (lambda ()
          (let ((rs (landlock-make-ruleset-proc)))
            (landlock-add-read-only-proc rs "/proc" "/sys" "/dev" "/etc"
                                          "/usr/lib" "/lib")
            (landlock-install-proc rs))))))

  (define (install-readonly-landlock! . paths)
    ;; Restrict to reading only the specified paths + /proc, /dev, /etc, system libs
    (when *landlock-available*
      (with-catch
        (lambda (e)
          (eprintf "warning: landlock install failed: ~a~n"
                   (if (message-condition? e) (condition-message e) e)))
        (lambda ()
          (let ((rs (landlock-make-ruleset-proc)))
            (apply landlock-add-read-only-proc rs
                   "/proc" "/sys" "/dev" "/etc" "/usr/lib" "/lib"
                   paths)
            (landlock-install-proc rs))))))

  (define (install-readwrite-landlock! read-paths write-paths)
    ;; Read access to read-paths, read+write to write-paths
    (when *landlock-available*
      (with-catch
        (lambda (e)
          (eprintf "warning: landlock install failed: ~a~n"
                   (if (message-condition? e) (condition-message e) e)))
        (lambda ()
          (let ((rs (landlock-make-ruleset-proc)))
            (apply landlock-add-read-only-proc rs
                   "/proc" "/sys" "/dev" "/etc" "/usr/lib" "/lib"
                   read-paths)
            (apply landlock-add-read-write-proc rs write-paths)
            (landlock-install-proc rs))))))

  ;; --- Capabilities ---

  (define *current-capabilities* (make-parameter '()))

  (define-syntax with-fs-read-capability
    (syntax-rules ()
      [(_ body ...)
       (parameterize ([*current-capabilities*
                       (cons 'fs-read (*current-capabilities*))])
         body ...)]))

  (define-syntax with-fs-write-capability
    (syntax-rules ()
      [(_ body ...)
       (parameterize ([*current-capabilities*
                       (cons 'fs-write (*current-capabilities*))])
         body ...)]))

  (define-syntax with-process-capability
    (syntax-rules ()
      [(_ body ...)
       (parameterize ([*current-capabilities*
                       (cons 'process (*current-capabilities*))])
         body ...)]))

  (define (check-fs-read!)
    (unless (memq 'fs-read (*current-capabilities*))
      (error 'check-fs-read! "filesystem read capability not granted")))

  (define (check-fs-write!)
    (unless (memq 'fs-write (*current-capabilities*))
      (error 'check-fs-write! "filesystem write capability not granted")))

  ;; --- Audit Logging ---

  (define audit-make-logger-proc #f)
  (define audit-log-proc #f)

  (define (load-audit!)
    (with-catch
      (lambda (e) (void))
      (lambda ()
        (let ((env (environment '(std security audit))))
          (set! audit-make-logger-proc (eval 'make-audit-logger env))
          (set! audit-log-proc (eval 'audit-log! env))
          ;; Initialize logger if env var is set
          (let ((log-path (getenv "COREUTILS_AUDIT_LOG")))
            (when (and log-path (> (string-length log-path) 0))
              (set! *audit-logger*
                (audit-make-logger-proc log-path))))))))

  (define (coreutils-audit-logger)
    *audit-logger*)

  (define (audit-file-access! path)
    (when (and *audit-logger* audit-log-proc)
      (with-catch (lambda (e) (void))
        (lambda ()
          (audit-log-proc *audit-logger* 'file-access
            'resource: path)))))

  (define (audit-file-modify! path)
    (when (and *audit-logger* audit-log-proc)
      (with-catch (lambda (e) (void))
        (lambda ()
          (audit-log-proc *audit-logger* 'file-modify
            'resource: path)))))

  (define (audit-file-delete! path)
    (when (and *audit-logger* audit-log-proc)
      (with-catch (lambda (e) (void))
        (lambda ()
          (audit-log-proc *audit-logger* 'file-delete
            'resource: path)))))

  (define (audit-process-spawn! cmd)
    (when (and *audit-logger* audit-log-proc)
      (with-catch (lambda (e) (void))
        (lambda ()
          (audit-log-proc *audit-logger* 'process-spawn
            'command: cmd)))))

  (define (audit-process-signal! pid sig)
    (when (and *audit-logger* audit-log-proc)
      (with-catch (lambda (e) (void))
        (lambda ()
          (audit-log-proc *audit-logger* 'process-signal
            'pid: (number->string pid)
            'signal: (number->string sig))))))

  ;; --- Taint Tracking ---

  (define taint-file-proc #f)
  (define untaint-proc #f)
  (define check-untainted-proc #f)

  (define (load-taint!)
    (with-catch
      (lambda (e) (void))
      (lambda ()
        (let ((env (environment '(std security taint))))
          (set! taint-file-proc (eval 'taint-file env))
          (set! untaint-proc (eval 'untaint env))
          (set! check-untainted-proc (eval 'check-untainted! env))))))

  (define (taint-argv-path path)
    ;; Mark a path from command-line arguments as file-input tainted
    (if taint-file-proc
      (taint-file-proc path)
      path))

  (define (untaint-after-sanitize value)
    ;; Remove taint after validation/sanitization
    (if untaint-proc
      (untaint-proc value)
      value))

  ;; --- Path Sanitization ---

  (define sanitize-path-proc #f)

  (define (load-sanitize!)
    (with-catch
      (lambda (e) (void))
      (lambda ()
        (let ((env (environment '(std security sanitize))))
          (set! sanitize-path-proc (eval 'sanitize-path env))))))

  (define (sanitize-path path)
    ;; Canonicalize path and reject traversal attempts
    (if sanitize-path-proc
      (sanitize-path-proc path)
      path))

  (define (safe-path-join/checked base relative)
    ;; Join paths with traversal protection
    (let ((joined (string-append base "/" relative)))
      ;; Validate no NUL bytes
      (let check ((i 0))
        (when (< i (string-length relative))
          (when (char=? (string-ref relative i) #\nul)
            (error 'safe-path-join/checked "NUL byte in path"))
          (check (+ i 1))))
      ;; Validate no escape via ..
      (let ((sanitized (sanitize-path joined)))
        (let ((blen (string-length base)))
          (if (and (>= (string-length sanitized) blen)
                   (string=? (substring sanitized 0 blen) base))
            sanitized
            (error 'safe-path-join/checked
                   "path traversal detected" relative))))))

  ;; --- Secret Wiping ---

  (define wipe-bv-proc #f)

  (define (load-secret!)
    (with-catch
      (lambda (e) (void))
      (lambda ()
        (let ((env (environment '(std security secret))))
          (set! wipe-bv-proc (eval 'wipe-bytevector! env))))))

  (define-syntax with-sensitive-buffer
    (syntax-rules ()
      [(_ ([name size]) body ...)
       (let ([name (make-bytevector size 0)])
         (dynamic-wind
           (lambda () (void))
           (lambda () body ...)
           (lambda ()
             ;; Zero the buffer on scope exit
             (let loop ([i 0])
               (when (< i (bytevector-length name))
                 (bytevector-u8-set! name i 0)
                 (loop (+ i 1)))))))]))

  ;; --- Combined Initialization ---

  (define (init-security!)
    ;; Load all security modules. Call once at startup.
    ;; Failures are silently ignored — security degrades gracefully.
    (load-seccomp!)
    (load-landlock!)
    (load-audit!)
    (load-taint!)
    (load-sanitize!)
    (load-secret!))

  ) ;; end library

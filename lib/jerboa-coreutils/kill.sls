#!chezscheme
;;; kill.sls -- Send signals to processes

(library (jerboa-coreutils kill)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            string-upcase)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-kill-proc (foreign-procedure "kill" (int int) int))

  (def *signals*
    '(("HUP" . 1) ("INT" . 2) ("QUIT" . 3) ("ILL" . 4) ("TRAP" . 5)
      ("ABRT" . 6) ("BUS" . 7) ("FPE" . 8) ("KILL" . 9) ("USR1" . 10)
      ("SEGV" . 11) ("USR2" . 12) ("PIPE" . 13) ("ALRM" . 14) ("TERM" . 15)
      ("STKFLT" . 16) ("CHLD" . 17) ("CONT" . 18) ("STOP" . 19) ("TSTP" . 20)
      ("TTIN" . 21) ("TTOU" . 22) ("URG" . 23) ("XCPU" . 24) ("XFSZ" . 25)
      ("VTALRM" . 26) ("PROF" . 27) ("WINCH" . 28) ("IO" . 29) ("PWR" . 30)
      ("SYS" . 31)))

  (def (signal-name->number name)
    (let* ((uname (string-upcase name))
           (uname (if (and (> (string-length uname) 3)
                           (string=? (substring uname 0 3) "SIG"))
                    (substring uname 3 (string-length uname))
                    uname))
           (pair (assoc uname *signals*)))
      (if pair (cdr pair)
        (let ((n (string->number name)))
          (if n n
            (die "~a: invalid signal specification" name))))))

  (def (signal-number->name num)
    (let loop ((sigs *signals*))
      (cond
        ((null? sigs) (number->string num))
        ((= (cdar sigs) num) (caar sigs))
        (else (loop (cdr sigs))))))

  (def (string-upcase str)
    (let ((result (string-copy str)))
      (let loop ((i 0))
        (when (< i (string-length result))
          (string-set! result i (char-upcase (string-ref result i)))
          (loop (+ i 1))))
      result))

  (def (main . args)
    (parameterize ((program-name "kill"))
      (init-security!)
      (install-process-seccomp!)
      (with-process-capability
      (cond
        ((null? args)
         (die "usage error"))
        ;; -l: list signals
        ((member (car args) '("-l" "-L" "--list"))
         (if (pair? (cdr args))
           ;; Translate signal numbers to names
           (for-each
             (lambda (arg)
               (let ((n (string->number arg)))
                 (if n
                   (displayln (signal-number->name n))
                   (displayln (signal-name->number arg)))))
             (cdr args))
           ;; List all signals
           (for-each
             (lambda (pair)
               (display (cdr pair))
               (display ") SIG")
               (displayln (car pair)))
             *signals*)))
        ((member (car args) '("--help" "-h"))
         (displayln "Usage: kill [-s SIGNAL | -SIGNAL] PID...")
         (displayln "Send signals to processes."))
        ((member (car args) '("--version"))
         (version-info "kill"))
        (else
         (let loop ((args args) (signal 15))
           (cond
             ((null? args) (void))
             ;; -s SIGNAL
             ((string=? (car args) "-s")
              (if (pair? (cdr args))
                (loop (cddr args) (signal-name->number (cadr args)))
                (die "option requires an argument -- 's'")))
             ;; -SIGNAL (e.g., -9, -TERM)
             ((and (> (string-length (car args)) 1)
                   (eqv? (string-ref (car args) 0) #\-)
                   (not (string=? (car args) "--")))
              (loop (cdr args)
                    (signal-name->number
                      (substring (car args) 1 (string-length (car args))))))
             (else
              ;; PIDs
              (for-each
                (lambda (arg)
                  (let ((pid (string->number arg)))
                    (if pid
                      (begin
                        (audit-process-signal! pid signal)
                        (let ((rc (ffi-kill-proc pid signal)))
                          (unless (zero? rc)
                            (warn "~a: No such process" pid))))
                      (warn "~a: arguments must be process or job IDs" arg))))
                args)))))))))

  ) ;; end library

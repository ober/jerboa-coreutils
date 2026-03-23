#!chezscheme
;;; hostid.sls -- Print system host ID

(library (jerboa-coreutils hostid)
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

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define hostid-get (foreign-procedure "gethostid" () long))

  (def (main . args)
    (parameterize ((program-name "hostid"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (let* ((id (hostid-get))
                   (hex (number->string (if (< id 0)
                                          (+ id 4294967296)  ; treat as unsigned 32-bit
                                          id)
                                        16))
                   ;; Pad to 8 hex digits
                   (padded (let ((len (string-length hex)))
                             (if (< len 8)
                               (string-append (make-string (- 8 len) #\0) hex)
                               (substring hex (- len 8) len)))))
              (displayln padded)))
        args
        'program: "hostid"
        'help: "Print the numeric identifier for the current host.")))

  ) ;; end library

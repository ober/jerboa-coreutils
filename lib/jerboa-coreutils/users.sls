#!chezscheme
;;; users.sls -- List logged in users

(library (jerboa-coreutils users)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            list-sort)
          (except (jerboa core) string-split)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Run /usr/bin/who and extract usernames, print sorted on one line
  (def (get-logged-in-users)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        "/usr/bin/who"
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((users (let loop ((acc '()))
                         (let ((line (get-line from-stdout)))
                           (if (eof-object? line)
                             (begin
                               (close-port to-stdin)
                               (close-port from-stdout)
                               (close-port from-stderr)
                               (reverse acc))
                             (loop (cons (car (string-split line #\space)) acc)))))))
            ;; sort (but do NOT deduplicate - GNU users shows each session)
            (list-sort string<? users))))))

  (def (string-split str ch)
    (let loop ((i 0) (start 0) (parts '()))
      (cond
        ((>= i (string-length str))
         (reverse (cons (substring str start i) parts)))
        ((eqv? (string-ref str i) ch)
         (if (> i start)
           (loop (+ i 1) (+ i 1) (cons (substring str start i) parts))
           (loop (+ i 1) (+ i 1) parts)))
        (else
         (loop (+ i 1) start parts)))))

  (def (main . args)
    (parameterize ((program-name "users"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let ((users (get-logged-in-users)))
            (if (null? users)
              (newline)
              (displayln (string-join-spaces users)))))
        args
        'program: "users"
        'help: "Print the user names of users currently logged in to the current host.")))

  (def (string-join-spaces strs)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc " " (car rest)))))))

  (def (list-sort less? lst)
    (sort less? lst))

  ) ;; end library

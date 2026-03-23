#!chezscheme
;;; id.sls -- Print user/group IDs

(library (jerboa-coreutils id)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            filter)
          (except (jerboa core) filter-map)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-getuid  (foreign-procedure "getuid" () unsigned-int))
  (define ffi-geteuid (foreign-procedure "geteuid" () unsigned-int))
  (define ffi-getgid  (foreign-procedure "getgid" () unsigned-int))
  (define ffi-getegid (foreign-procedure "getegid" () unsigned-int))

  ;; Use /usr/bin/id to get group info since getgroups needs buffer management
  (def (get-groups-from-id)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        "/usr/bin/id -G"
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((result (get-line from-stdout)))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result)
              '()
              (filter-map string->number
                          (string-split-space result))))))))

  (def (gid->name gid)
    (with-catch
      (lambda (e) (number->string gid))
      (lambda ()
        ;; Use getent to resolve gid to name
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "/usr/bin/getent group " (number->string gid))
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((result (get-line from-stdout)))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result)
              (number->string gid)
              ;; getent group output: name:x:gid:members
              (let ((colon (string-index result #\:)))
                (if colon
                  (substring result 0 colon)
                  (number->string gid)))))))))

  (def (uid->name uid)
    (with-catch
      (lambda (e) (number->string uid))
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        (string-append "/usr/bin/getent passwd " (number->string uid))
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((result (get-line from-stdout)))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)
            (if (eof-object? result)
              (number->string uid)
              (let ((colon (string-index result #\:)))
                (if colon
                  (substring result 0 colon)
                  (number->string uid)))))))))

  (def (main . args)
    (parameterize ((program-name "id"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (let* ((uid (ffi-getuid))
                   (euid (ffi-geteuid))
                   (gid (ffi-getgid))
                   (egid (ffi-getegid))
                   (groups (get-groups-from-id))
                   (use-name (hash-get opt 'name))
                   (use-real (hash-get opt 'real))
                   (delim (if (hash-get opt 'zero) #\nul #\newline)))
              (cond
                ((hash-get opt 'user)
                 (let ((u (if use-real uid euid)))
                   (display (if use-name (uid->name u) u))
                   (write-char delim)))
                ((hash-get opt 'group)
                 (let ((g (if use-real gid egid)))
                   (display (if use-name (gid->name g) g))
                   (write-char delim)))
                ((hash-get opt 'groups)
                 (let ((gs (cons egid (filter (lambda (g) (not (= g egid))) groups))))
                   (display (string-join
                              (map (lambda (g)
                                     (if use-name (gid->name g) (number->string g)))
                                   gs)
                              " "))
                   (write-char delim)))
                (else
                 ;; Default full output
                 (display (string-append
                            "uid=" (number->string euid)
                            "(" (uid->name euid) ")"
                            " gid=" (number->string egid)
                            "(" (gid->name egid) ")"
                            " groups=" (string-join
                                         (map (lambda (g)
                                                (string-append (number->string g)
                                                  "(" (gid->name g) ")"))
                                              (cons egid (filter (lambda (g) (not (= g egid)))
                                                                 groups)))
                                         ",")))
                 (newline)))))
        args
        'program: "id"
        'help: "Print real and effective user and group IDs."
        (flag 'user "-u" "--user"
          'help: "print only the effective user ID")
        (flag 'group "-g" "--group"
          'help: "print only the effective group ID")
        (flag 'groups "-G" "--groups"
          'help: "print all group IDs")
        (flag 'name "-n" "--name"
          'help: "print a name instead of a number, for -ugG")
        (flag 'real "-r" "--real"
          'help: "print the real ID instead of the effective ID, for -ugG")
        (flag 'zero "-z" "--zero"
          'help: "delimit entries with NUL characters")
        (rest-arguments 'rest))))

  (def (string-join strs sep)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  (def (filter pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
      (else (filter pred (cdr lst)))))

  (def (filter-map proc lst)
    (cond
      ((null? lst) '())
      (else
        (let ((result (proc (car lst))))
          (if result
            (cons result (filter-map proc (cdr lst)))
            (filter-map proc (cdr lst)))))))

  (def (string-split-space str)
    (let loop ((i 0) (start #f) (parts '()))
      (cond
        ((>= i (string-length str))
         (reverse (if start
                    (cons (substring str start i) parts)
                    parts)))
        ((char-whitespace? (string-ref str i))
         (loop (+ i 1) #f
               (if start (cons (substring str start i) parts) parts)))
        (else
         (loop (+ i 1) (or start i) parts)))))

  (def (string-index str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  ) ;; end library

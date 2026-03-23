#!chezscheme
;;; groups.sls -- Print group membership

(library (jerboa-coreutils groups)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            filter)
          (except (jerboa core) filter-map)
          (only (std sugar) with-catch)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-getegid (foreign-procedure "getegid" () unsigned-int))

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
              (let ((colon (string-index result #\:)))
                (if colon
                  (substring result 0 colon)
                  (number->string gid)))))))))

  (def (main . args)
    (parameterize ((program-name "groups"))
      (init-security!)
      (install-proc-only-landlock!)
      (install-readonly-seccomp!)
      (cond
        ((and (pair? args) (member (car args) '("--help")))
         (displayln "Usage: groups [OPTION]... [USERNAME]...")
         (displayln "Print group memberships for each USERNAME."))
        ((and (pair? args) (member (car args) '("--version")))
         (version-info "groups"))
        (else
         (let* ((egid (ffi-getegid))
                (groups (get-groups-from-id))
                ;; Put egid first, then others
                (all (cons egid (filter (lambda (g) (not (= g egid))) groups))))
           (displayln (string-join (map gid->name all) " ")))))))

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

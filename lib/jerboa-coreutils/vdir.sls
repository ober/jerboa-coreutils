#!chezscheme
;;; vdir.sls -- List directory contents in long format
;;; vdir is equivalent to 'ls -l -b'

(library (jerboa-coreutils vdir)
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
          (jerboa-coreutils common version))

  ;; Entry: #(name full-path mode nlink uid gid size mtime blocks)

  (def (shell-quote str)
    (string-append "'" (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (eqv? c #\')
            (loop (+ i 1) (append (reverse (string->list "'\\''")) acc))
            (loop (+ i 1) (cons c acc)))))) "'"))

  (def (string-split-spaces str)
    (let loop ((i 0) (start #f) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (if start
                    (cons (substring str start i) acc)
                    acc)))
        ((eqv? (string-ref str i) #\space)
         (if start
           (loop (+ i 1) #f (cons (substring str start i) acc))
           (loop (+ i 1) #f acc)))
        (else
         (if start
           (loop (+ i 1) start acc)
           (loop (+ i 1) i acc))))))

  (def (make-entry name dir)
    (let ((full (if (string=? dir ".")
                  name
                  (string-append dir "/" name))))
      (with-catch
        (lambda (e) #f)
        (lambda ()
          (let* ((cmd (string-append "stat -c '%f %h %u %g %s %Y %b' "
                         (shell-quote full) " 2>/dev/null")))
            (let-values (((to-stdin from-stdout from-stderr pid)
                          (open-process-ports cmd (buffer-mode block) (native-transcoder))))
              (close-port to-stdin)
              (let ((line (get-line from-stdout)))
                (close-port from-stdout)
                (close-port from-stderr)
                (if (or (not line) (eof-object? line))
                  #f
                  (let ((parts (string-split-spaces line)))
                    (if (< (length parts) 7)
                      #f
                      (let ((mode (string->number (list-ref parts 0) 16))
                            (nlink (string->number (list-ref parts 1)))
                            (uid (string->number (list-ref parts 2)))
                            (gid (string->number (list-ref parts 3)))
                            (size (string->number (list-ref parts 4)))
                            (mtime (string->number (list-ref parts 5)))
                            (blocks (string->number (list-ref parts 6))))
                        (if (and mode nlink uid gid size mtime blocks)
                          (vector name full mode nlink uid gid size mtime blocks)
                          #f))))))))))))

  (def (mode->rwx mode)
    (let* ((p (bitwise-and mode #o7777))
           (b (lambda (bit ch) (if (not (zero? (bitwise-and p bit))) ch #\-))))
      (string
       (cond
         ((= (bitwise-and mode #o170000) #o140000) #\s)
         ((= (bitwise-and mode #o170000) #o120000) #\l)
         ((= (bitwise-and mode #o170000) #o100000) #\-)
         ((= (bitwise-and mode #o170000) #o060000) #\b)
         ((= (bitwise-and mode #o170000) #o040000) #\d)
         ((= (bitwise-and mode #o170000) #o020000) #\c)
         ((= (bitwise-and mode #o170000) #o010000) #\p)
         (else #\?))
       (b #o400 #\r) (b #o200 #\w)
       (if (not (zero? (bitwise-and p #o4000)))
         (if (not (zero? (bitwise-and p #o100))) #\s #\S)
         (b #o100 #\x))
       (b #o040 #\r) (b #o020 #\w)
       (if (not (zero? (bitwise-and p #o2000)))
         (if (not (zero? (bitwise-and p #o010))) #\s #\S)
         (b #o010 #\x))
       (b #o004 #\r) (b #o002 #\w)
       (if (not (zero? (bitwise-and p #o1000)))
         (if (not (zero? (bitwise-and p #o001))) #\t #\T)
         (b #o001 #\x)))))

  (def (left-pad str width)
    (let ((len (string-length str)))
      (if (>= len width) str
        (string-append (make-string (- width len) #\space) str))))

  (def (right-pad str width)
    (let ((len (string-length str)))
      (if (>= len width) str
        (string-append str (make-string (- width len) #\space)))))

  ;; Format time like ls: recent files show HH:MM, old files show year
  (def (format-time epoch)
    (with-catch
      (lambda (e) "?")
      (lambda ()
        (let* ((cmd (string-append "date -d @" (number->string epoch)
                       " '+%b %e %H:%M %Y' 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                "?"
                ;; line is like "Mar 18 14:30 2026"
                ;; For recent files (< 6 months), show "Mon DD HH:MM"
                ;; For old files, show "Mon DD  YYYY"
                (let* ((now-cmd "date +%s 2>/dev/null"))
                  (let-values (((to2 from2 err2 pid2)
                                (open-process-ports now-cmd (buffer-mode block) (native-transcoder))))
                    (close-port to2)
                    (let ((now-str (get-line from2)))
                      (close-port from2)
                      (close-port err2)
                      (let ((now (if (and now-str (not (eof-object? now-str)))
                                   (or (string->number now-str) 0)
                                   0)))
                        (let ((diff (- now epoch)))
                          ;; Parse the date output: "Mon DD HH:MM YYYY"
                          (let ((parts (string-split-spaces line)))
                            (if (< (length parts) 4)
                              line
                              (let ((mon (list-ref parts 0))
                                    (day (list-ref parts 1))
                                    (time-str (list-ref parts 2))
                                    (year (list-ref parts 3)))
                                (let ((padded-day (if (< (string-length day) 2)
                                                    (string-append " " day)
                                                    day)))
                                  (if (or (< diff 0) (> diff 15552000))
                                    (string-append mon " " padded-day "  " year)
                                    (string-append mon " " padded-day " " time-str))))))))))))))))))

  ;; Look up username by uid
  (def (uid->name uid)
    (with-catch
      (lambda (e) (number->string uid))
      (lambda ()
        (let* ((cmd (string-append "getent passwd " (number->string uid) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                (number->string uid)
                ;; passwd line: name:x:uid:gid:...
                (let loop ((i 0))
                  (cond
                    ((>= i (string-length line)) (number->string uid))
                    ((eqv? (string-ref line i) #\:) (substring line 0 i))
                    (else (loop (+ i 1))))))))))))

  ;; Look up group name by gid
  (def (gid->name gid)
    (with-catch
      (lambda (e) (number->string gid))
      (lambda ()
        (let* ((cmd (string-append "getent group " (number->string gid) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                (number->string gid)
                (let loop ((i 0))
                  (cond
                    ((>= i (string-length line)) (number->string gid))
                    ((eqv? (string-ref line i) #\:) (substring line 0 i))
                    (else (loop (+ i 1))))))))))))

  ;; Read symlink target
  (def (read-symlink path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let* ((cmd (string-append "readlink " (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line)) #f line)))))))

  (def (filter-hidden lst)
    (cond
      ((null? lst) '())
      ((eqv? (string-ref (car lst) 0) #\.)
       (filter-hidden (cdr lst)))
      (else (cons (car lst) (filter-hidden (cdr lst))))))

  (def (filter-map-entries proc lst)
    (cond
      ((null? lst) '())
      (else
       (let ((r (proc (car lst))))
         (if r
           (cons r (filter-map-entries proc (cdr lst)))
           (filter-map-entries proc (cdr lst)))))))

  (def (list-directory dir show-all)
    (with-catch
      (lambda (e)
        (warn "cannot open directory '~a': ~a" dir (error-message e))
        '())
      (lambda ()
        (let* ((raw (directory-list dir))
               (names (if show-all
                        raw
                        (filter-hidden raw)))
               (sorted (list-sort string-ci<? names)))
          (filter-map-entries
            (lambda (n) (make-entry n dir))
            sorted)))))

  (def (print-long-format entries)
    (unless (null? entries)
      (let* ((total-blocks (apply + (map (lambda (e)
                                            (quotient (+ (vector-ref e 8) 1) 2))
                                          entries))))
        (displayln "total " total-blocks))
      (let* ((nlink-w (apply max 1 (map (lambda (e)
                        (string-length (number->string (vector-ref e 3)))) entries)))
             (owner-strs (map (lambda (e) (uid->name (vector-ref e 4))) entries))
             (group-strs (map (lambda (e) (gid->name (vector-ref e 5))) entries))
             (owner-w (apply max 1 (map string-length owner-strs)))
             (group-w (apply max 1 (map string-length group-strs)))
             (size-w (apply max 1 (map (lambda (e)
                       (string-length (number->string (vector-ref e 6)))) entries))))
        (let loop ((es entries) (os owner-strs) (gs group-strs))
          (unless (null? es)
            (let* ((e (car es))
                   (mode (vector-ref e 2))
                   (is-link (= (bitwise-and mode #o170000) #o120000)))
              (display (mode->rwx mode))
              (display " ")
              (display (left-pad (number->string (vector-ref e 3)) nlink-w))
              (display " ")
              (display (right-pad (car os) owner-w))
              (display " ")
              (display (right-pad (car gs) group-w))
              (display " ")
              (display (left-pad (number->string (vector-ref e 6)) size-w))
              (display " ")
              (display (format-time (vector-ref e 7)))
              (display " ")
              (display (vector-ref e 0))
              (when is-link
                (let ((target (read-symlink (vector-ref e 1))))
                  (when target
                    (display " -> ")
                    (display target))))
              (newline))
            (loop (cdr es) (cdr os) (cdr gs)))))))

  (def (main . args)
    (parameterize ((program-name "vdir"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((paths (if (null? (hash-ref opt 'rest)) '(".") (hash-ref opt 'rest))))
            (let ((show-header (> (length paths) 1)))
              (let loop ((dirs paths) (first #t))
                (unless (null? dirs)
                  (unless first (newline))
                  (when show-header
                    (displayln (car dirs) ":"))
                  (print-long-format (list-directory (car dirs) (hash-get opt 'all)))
                  (loop (cdr dirs) #f))))))
        args
        'program: "vdir"
        'help: "List directory contents in long format."
        (flag 'all "-a" "--all"
          'help: "do not ignore entries starting with .")
        (rest-arguments 'rest))))

  ) ;; end library

#!chezscheme
;;; ls.sls -- List directory contents

(library (jerboa-coreutils ls)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            filter)
          (except (jerboa core) filter-map)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (define _load-ffi (begin (load-shared-object #f) (void)))

  (define ffi-isatty (foreign-procedure "isatty" (int) int))

  ;;; ========= Data structures =========
  ;; Entry: #(name full-path mode nlink uid gid size mtime ino blocks)
  ;; We get stat info via system stat command

  (def (shell-quote str)
    (string-append "'" (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (eqv? c #\')
            (loop (+ i 1) (append (reverse (string->list "'\\''")) acc))
            (loop (+ i 1) (cons c acc)))))) "'"))

  (def (make-entry name full-path follow-links)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let* ((flag (if follow-links "" "-L"))
               (cmd (string-append "stat " flag " -c '%f %h %u %g %s %Y %i %b' "
                       (shell-quote full-path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                #f
                (let ((parts (string-split-spaces line)))
                  (if (< (length parts) 8)
                    #f
                    (let ((mode (string->number (list-ref parts 0) 16))
                          (nlink (string->number (list-ref parts 1)))
                          (uid (string->number (list-ref parts 2)))
                          (gid (string->number (list-ref parts 3)))
                          (size (string->number (list-ref parts 4)))
                          (mtime (string->number (list-ref parts 5)))
                          (ino (string->number (list-ref parts 6)))
                          (blocks (string->number (list-ref parts 7))))
                      (if (and mode nlink uid gid size mtime ino blocks)
                        (vector name full-path mode nlink uid gid size mtime ino blocks)
                        #f)))))))))))

  (def (string-split-spaces str)
    (let loop ((i 0) (start #f) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (if start (cons (substring str start i) acc) acc)))
        ((char-whitespace? (string-ref str i))
         (loop (+ i 1) #f
               (if start (cons (substring str start i) acc) acc)))
        (else
         (loop (+ i 1) (or start i) acc)))))

  (def (entry-name e)      (vector-ref e 0))
  (def (entry-path e)      (vector-ref e 1))
  (def (entry-mode e)      (vector-ref e 2))
  (def (entry-nlink e)     (vector-ref e 3))
  (def (entry-uid e)       (vector-ref e 4))
  (def (entry-gid e)       (vector-ref e 5))
  (def (entry-size e)      (vector-ref e 6))
  (def (entry-mtime e)     (vector-ref e 7))
  (def (entry-ino e)       (vector-ref e 8))
  (def (entry-blocks e)    (vector-ref e 9))

  (def (entry-is-dir? e)
    (= (bitwise-and (entry-mode e) #o170000) #o040000))

  (def (entry-is-symlink? e)
    (= (bitwise-and (entry-mode e) #o170000) #o120000))

  ;;; ========= Mode formatting =========
  (def (mode->type-char mode)
    (let ((t (bitwise-and mode #o170000)))
      (cond
        ((= t #o140000) #\s) ((= t #o120000) #\l) ((= t #o100000) #\-)
        ((= t #o060000) #\b) ((= t #o040000) #\d) ((= t #o020000) #\c)
        ((= t #o010000) #\p) (else #\?))))

  (def (mode->rwx mode)
    (let* ((p (bitwise-and mode #o7777))
           (b (lambda (bit ch) (if (not (zero? (bitwise-and p bit))) ch #\-))))
      (string
       (mode->type-char mode)
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

  ;;; ========= Human-readable sizes =========
  (def (human-size size)
    (cond
      ((>= size (* 1024 1024 1024))
       (string-append (number->string (quotient size (* 1024 1024 1024))) "G"))
      ((>= size (* 1024 1024))
       (string-append (number->string (quotient size (* 1024 1024))) "M"))
      ((>= size 1024)
       (string-append (number->string (quotient size 1024)) "K"))
      (else (number->string size))))

  ;;; ========= Sorting =========
  (def (sort-entries entries sort-by reverse?)
    (let ((sorted
           (cond
             ((eq? sort-by 'time)
              (list-sort (lambda (a b) (> (entry-mtime a) (entry-mtime b))) entries))
             ((eq? sort-by 'size)
              (list-sort (lambda (a b) (> (entry-size a) (entry-size b))) entries))
             ((eq? sort-by 'name)
              (list-sort (lambda (a b) (string-ci<? (entry-name a) (entry-name b))) entries))
             (else entries))))
      (if reverse? (reverse sorted) sorted)))

  ;;; ========= Column formatting =========
  (def (right-pad str width)
    (let ((len (string-length str)))
      (if (>= len width) str
        (string-append str (make-string (- width len) #\space)))))

  (def (left-pad str width)
    (let ((len (string-length str)))
      (if (>= len width) str
        (string-append (make-string (- width len) #\space) str))))

  ;;; ========= Symlink target =========
  (def (read-symlink path)
    (with-catch
      (lambda (e) "")
      (lambda ()
        (let ((cmd (string-append "readlink " (shell-quote path) " 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line)) "" line)))))))

  ;;; ========= Time formatting =========
  (def (format-time mtime)
    (with-catch
      (lambda (e) "?")
      (lambda ()
        (let ((cmd (string-append "date -d @" (number->string mtime) " '+%b %e %H:%M' 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line)) "?" line)))))))

  ;;; ========= User/Group name lookup =========
  (def (uid->name uid)
    (with-catch
      (lambda (e) (number->string uid))
      (lambda ()
        (let ((cmd (string-append "getent passwd " (number->string uid) " 2>/dev/null | cut -d: -f1")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line) (string=? line ""))
                (number->string uid) line)))))))

  (def (gid->name gid)
    (with-catch
      (lambda (e) (number->string gid))
      (lambda ()
        (let ((cmd (string-append "getent group " (number->string gid) " 2>/dev/null | cut -d: -f1")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line) (string=? line ""))
                (number->string gid) line)))))))

  ;;; ========= Indicator character =========
  (def (indicator-char mode)
    (let ((type-bits (bitwise-and mode #o170000)))
      (cond
        ((= type-bits #o040000) "/")
        ((= type-bits #o120000) "@")
        ((= type-bits #o010000) "|")
        ((= type-bits #o140000) "=")
        ((not (zero? (bitwise-and mode #o111))) "*")
        (else ""))))

  ;;; ========= Color support =========
  (define *esc* (string (integer->char 27)))

  (def (make-ansi-code code)
    (string-append *esc* "[" code "m"))

  (def (color-for-entry entry)
    (let* ((mode (entry-mode entry))
           (type-bits (bitwise-and mode #o170000)))
      (cond
        ((= type-bits #o040000) (make-ansi-code "01;34"))
        ((= type-bits #o120000) (make-ansi-code "01;36"))
        ((= type-bits #o010000) (make-ansi-code "33"))
        ((= type-bits #o140000) (make-ansi-code "01;35"))
        ((= type-bits #o060000) (make-ansi-code "01;33"))
        ((= type-bits #o020000) (make-ansi-code "01;33"))
        ((not (zero? (bitwise-and mode #o4000))) (make-ansi-code "37;41"))
        ((not (zero? (bitwise-and mode #o2000))) (make-ansi-code "30;43"))
        ((not (zero? (bitwise-and mode #o111))) (make-ansi-code "01;32"))
        (else #f))))

  (define *color-reset* (make-ansi-code "0"))

  ;;; ========= Long format output =========
  (def (print-long-format entries show-inode human-readable classify use-color show-total)
    (when (null? entries)
      (when show-total (displayln "total 0")))
    (unless (null? entries)
      (let* ((nlink-width (apply max 1 (map (lambda (e)
                            (string-length (number->string (entry-nlink e)))) entries)))
             (owner-strs (map (lambda (e) (uid->name (entry-uid e))) entries))
             (group-strs (map (lambda (e) (gid->name (entry-gid e))) entries))
             (owner-width (apply max 1 (map string-length owner-strs)))
             (group-width (apply max 1 (map string-length group-strs)))
             (size-strs (map (lambda (e)
                               (if human-readable
                                 (human-size (entry-size e))
                                 (number->string (entry-size e))))
                             entries))
             (size-width (apply max 1 (map string-length size-strs)))
             (ino-width (if show-inode
                          (apply max 1 (map (lambda (e)
                                      (string-length (number->string (entry-ino e)))) entries))
                          0)))

        ;; Print total blocks
        (when show-total
          (let ((total-blocks (apply + (map (lambda (e)
                                              (quotient (+ (entry-blocks e) 1) 2))
                                            entries))))
            (displayln "total " total-blocks)))

        ;; Print each entry
        (let loop ((es entries) (os owner-strs) (gs group-strs) (ss size-strs))
          (unless (null? es)
            (let ((e (car es))
                  (owner-str (car os))
                  (group-str (car gs))
                  (size-str (car ss)))
              (when show-inode
                (display (left-pad (number->string (entry-ino e)) ino-width))
                (display " "))
              (display (mode->rwx (entry-mode e)))
              (display " ")
              (display (left-pad (number->string (entry-nlink e)) nlink-width))
              (display " ")
              (display (right-pad owner-str owner-width))
              (display " ")
              (display (right-pad group-str group-width))
              (display " ")
              (display (left-pad size-str size-width))
              (display " ")
              (display (format-time (entry-mtime e)))
              (display " ")
              (let ((color (and use-color (color-for-entry e))))
                (when color (display color))
                (display (entry-name e))
                (when color (display *color-reset*)))
              (when classify
                (display (indicator-char (entry-mode e))))
              (when (entry-is-symlink? e)
                (display " -> ")
                (display (read-symlink (entry-path e))))
              (newline))
            (loop (cdr es) (cdr os) (cdr gs) (cdr ss)))))))

  ;;; ========= Short format output =========
  (def (print-one-per-line entries show-inode classify use-color)
    (let ((ino-width (if (and show-inode (not (null? entries)))
                       (apply max 1 (map (lambda (e)
                                    (string-length (number->string (entry-ino e)))) entries))
                       0)))
      (for-each
        (lambda (e)
          (when show-inode
            (display (left-pad (number->string (entry-ino e)) ino-width))
            (display " "))
          (let ((color (and use-color (color-for-entry e))))
            (when color (display color))
            (display (entry-name e))
            (when color (display *color-reset*)))
          (when classify
            (display (indicator-char (entry-mode e))))
          (newline))
        entries)))

  ;;; ========= Directory listing =========
  (def (list-directory dir show-all show-almost-all follow-links)
    (with-catch
      (lambda (e)
        (warn "cannot open directory '~a': ~a" dir (error-message e))
        '())
      (lambda ()
        (let* ((raw (directory-list dir))
               (names (cond
                        (show-all
                         (append '("." "..") raw))
                        (show-almost-all raw)
                        (else
                         (filter (lambda (n) (not (eqv? (string-ref n 0) #\.))) raw)))))
          (filter-map
            (lambda (name)
              (let ((full (if (string=? dir "/")
                            (string-append "/" name)
                            (if (string=? dir ".")
                              name
                              (string-append dir "/" name)))))
                (make-entry name full follow-links)))
            names)))))

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

  ;;; ========= Recursive listing =========
  (def (list-dir-recursive dir show-all show-almost-all follow-links
                           sort-by reverse-sort long-format
                           show-inode human-readable classify use-color)
    (let* ((entries (list-directory dir show-all show-almost-all follow-links))
           (sorted (sort-entries entries sort-by reverse-sort)))
      (if long-format
        (print-long-format sorted show-inode human-readable classify use-color #t)
        (print-one-per-line sorted show-inode classify use-color))
      (for-each
        (lambda (sub)
          (when (and (entry-is-dir? sub)
                     (not (string=? (entry-name sub) "."))
                     (not (string=? (entry-name sub) "..")))
            (let ((subpath (string-append dir "/" (entry-name sub))))
              (newline)
              (displayln subpath ":")
              (list-dir-recursive subpath show-all show-almost-all follow-links
                                  sort-by reverse-sort long-format
                                  show-inode human-readable classify use-color))))
        sorted)))

  ;;; ========= Color option pre-processing =========
  (def (extract-color-args args)
    (let loop ((rest args) (acc '()) (color-mode #f))
      (if (null? rest)
        (values (reverse acc) color-mode)
        (let ((arg (car rest)))
          (cond
            ((string=? arg "--color")
             (loop (cdr rest) acc "always"))
            ((and (> (string-length arg) 8)
                  (string=? (substring arg 0 8) "--color="))
             (loop (cdr rest) acc (substring arg 8 (string-length arg))))
            (else
             (loop (cdr rest) (cons arg acc) color-mode)))))))

  (def (resolve-color-mode color-mode is-tty)
    (cond
      ((not color-mode) #f)
      ((member color-mode '("always" "yes" "force")) #t)
      ((member color-mode '("never" "no" "none")) #f)
      ((member color-mode '("auto" "tty" "if-tty")) is-tty)
      (else
       (warn "invalid argument '~a' for '--color'" color-mode)
       #f)))

  ;;; ========= Main =========
  (def (main . args)
    (let-values (((filtered-args color-mode) (extract-color-args args)))
      (parameterize ((program-name "ls"))
        (call-with-getopt
          (lambda (_ opt)
              (let* ((paths (if (null? (hash-ref opt 'rest)) '(".") (hash-ref opt 'rest)))
                     (long-format (hash-get opt 'long))
                     (show-all (hash-get opt 'all))
                     (show-almost-all (hash-get opt 'almost-all))
                     (one-per-line (hash-get opt 'one-per-line))
                     (recursive (hash-get opt 'recursive-ls))
                     (list-dirs (hash-get opt 'directory))
                     (reverse-sort (hash-get opt 'reverse-sort))
                     (sort-time (hash-get opt 'sort-time))
                     (sort-size (hash-get opt 'sort-size))
                     (human-readable (hash-get opt 'human-readable))
                     (classify (hash-get opt 'classify))
                     (show-inode (hash-get opt 'inode))
                     (follow-links #f)
                     (is-tty (= (ffi-isatty 1) 1))
                     (use-color (resolve-color-mode color-mode is-tty))
                     (one-per-line (or one-per-line (not is-tty) long-format))
                     (sort-by (cond (sort-time 'time) (sort-size 'size) (else 'name))))
              ;; Separate file args from directory args
              (let ((file-entries '())
                    (dir-args '())
                    (had-error? #f))
                (for-each
                  (lambda (path)
                    (if list-dirs
                      (let ((entry (make-entry path path follow-links)))
                        (if entry
                          (set! file-entries (cons entry file-entries))
                          (begin
                            (warn "cannot access '~a': No such file or directory" path)
                            (set! had-error? #t))))
                      (let ((entry (make-entry path path follow-links)))
                        (if (not entry)
                          (begin
                            (warn "cannot access '~a': No such file or directory" path)
                            (set! had-error? #t))
                          (if (entry-is-dir? entry)
                            (set! dir-args (cons path dir-args))
                            (set! file-entries (cons entry file-entries)))))))
                  paths)
                (set! file-entries (sort-entries (reverse file-entries) sort-by reverse-sort))
                (set! dir-args (reverse dir-args))
                ;; Print file entries
                (unless (null? file-entries)
                  (if long-format
                    (print-long-format file-entries show-inode human-readable classify use-color #f)
                    (print-one-per-line file-entries show-inode classify use-color)))
                ;; Print directories
                (let ((show-header (or (> (length dir-args) 1)
                                       (and (not (null? file-entries))
                                            (not (null? dir-args))))))
                  (let loop ((dirs dir-args) (first-dir (null? file-entries)))
                    (unless (null? dirs)
                      (unless first-dir (newline))
                      (when show-header
                        (displayln (car dirs) ":"))
                      (if recursive
                        (list-dir-recursive (car dirs) show-all show-almost-all
                                            follow-links sort-by reverse-sort long-format
                                            show-inode human-readable classify use-color)
                        (let* ((entries (list-directory (car dirs) show-all show-almost-all follow-links))
                               (sorted (sort-entries entries sort-by reverse-sort)))
                          (if long-format
                            (print-long-format sorted show-inode human-readable classify use-color #t)
                            (print-one-per-line sorted show-inode classify use-color))))
                      (loop (cdr dirs) #f))))
                (when had-error? (exit 2)))))
          filtered-args
          'program: "ls"
          'help: "List information about the FILEs (the current directory by default)."
          (flag 'all "-a" "--all"
            'help: "do not ignore entries starting with .")
          (flag 'almost-all "-A" "--almost-all"
            'help: "do not list implied . and ..")
          (flag 'long "-l"
            'help: "use a long listing format")
          (flag 'one-per-line "-1"
            'help: "list one file per line")
          (flag 'recursive-ls "-R" "--recursive"
            'help: "list subdirectories recursively")
          (flag 'directory "-d" "--directory"
            'help: "list directories themselves, not their contents")
          (flag 'reverse-sort "-r" "--reverse"
            'help: "reverse order while sorting")
          (flag 'sort-time "-t"
            'help: "sort by time, newest first")
          (flag 'sort-size "-S"
            'help: "sort by file size, largest first")
          (flag 'human-readable "--human-readable"
            'help: "with -l, print human readable sizes")
          (flag 'classify "-F" "--classify"
            'help: "append indicator (one of */=>@|) to entries")
          (flag 'inode "-i" "--inode"
            'help: "print the index number of each file")
          (rest-arguments 'rest)))))

  ) ;; end library

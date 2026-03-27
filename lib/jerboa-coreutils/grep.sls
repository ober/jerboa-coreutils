#!chezscheme
;;; grep.sls -- Search files for lines matching a pattern (GNU grep compatible)
;;;
;;; Supports: BRE (-G, default), ERE (-E), fixed-string (-F), PCRE (-P)
;;; All major GNU grep flags: -i -v -c -l -L -o -n -H -h -b -w -x -q -s
;;; -z -m -r/-R -A/-B/-C --color --include/--exclude/--exclude-dir -e/-f
;;; Exit: 0=match found, 1=no match, 2=error

(library (jerboa-coreutils grep)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            printf fprintf)
          (jerboa core)
          (only (std sugar) with-catch try catch finally)
          (only (std format) eprintf format)
          (std cli getopt)
          (only (std misc string) string-split string-contains string-index)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security)
          (jerboa-coreutils grep pcre2))

  ;; ========== Pattern compilation ==========

  ;; Escape all regex metacharacters for fixed-string mode
  (def (escape-fixed str)
    (let ([len (string-length str)])
      (let loop ([i 0] [acc '()])
        (if (>= i len)
          (list->string (reverse acc))
          (let ([c (string-ref str i)])
            (if (memv c '(#\. #\^ #\$ #\* #\+ #\? #\( #\) #\[ #\] #\{ #\} #\| #\\))
              (loop (+ i 1) (cons c (cons #\\ acc)))
              (loop (+ i 1) (cons c acc))))))))

  ;; Build the PCRE2 pattern string from user pattern + mode flags
  (def (build-pattern raw-pattern mode word-match? line-match?)
    (let* ([pat (case mode
                  [(bre) (bre->ere raw-pattern)]
                  [(ere) raw-pattern]
                  [(fixed) (escape-fixed raw-pattern)]
                  [(pcre) raw-pattern]
                  [else raw-pattern])]
           ;; Wrap for word/line matching
           [pat (if word-match?
                  (string-append "(?:^|\\W)(" pat ")(?:\\W|$)")
                  pat)]
           [pat (if (and line-match? (not word-match?))
                  (string-append "^(?:" pat ")$")
                  pat)])
      pat))

  ;; Compile pattern(s) into a single PCRE2 regex.
  ;; Multiple patterns are joined with |.
  (def (compile-patterns patterns mode case-insensitive? word-match? line-match?)
    (let* ([built (map (lambda (p) (build-pattern p mode word-match? line-match?))
                       patterns)]
           [combined (if (= (length built) 1)
                       (car built)
                       (string-append "(?:" (join-patterns built) ")"))]
           [flags (if case-insensitive? PCRE2_CASELESS 0)])
      (pcre2-compile combined flags)))

  (def (join-patterns pats)
    (if (null? pats)
      ""
      (let loop ([rest (cdr pats)] [acc (car pats)])
        (if (null? rest)
          acc
          (loop (cdr rest) (string-append acc "|" (car rest)))))))

  ;; ========== Color output ==========

  (def *color-match* "\x1b;[01;31m")   ;; bold red
  (def *color-filename* "\x1b;[35m")   ;; magenta
  (def *color-lineno* "\x1b;[32m")     ;; green
  (def *color-sep* "\x1b;[36m")        ;; cyan
  (def *color-reset* "\x1b;[m")

  ;; ========== Line matching ==========

  ;; Test if a line matches the compiled regex.
  ;; Returns #t/#f for simple match, or list of match results for -o mode.
  (def (line-matches? rx line only-matching?)
    (if only-matching?
      (pcre2-match-all rx line)
      (if (pcre2-search rx line 0) #t #f)))

  ;; ========== File/directory traversal ==========

  ;; Check if a filename matches a glob pattern (simple: *.ext style)
  (def (glob-match? pattern name)
    (cond
      ;; *.ext pattern
      [(and (> (string-length pattern) 1)
            (char=? (string-ref pattern 0) #\*))
       (let ([suffix (substring pattern 1 (string-length pattern))])
         (and (>= (string-length name) (string-length suffix))
              (string=? (substring name (- (string-length name) (string-length suffix))
                                   (string-length name))
                        suffix)))]
      ;; Exact match
      [else (string=? pattern name)]))

  ;; Check if name matches any pattern in the list
  (def (matches-any-glob? name patterns)
    (let loop ([pats patterns])
      (cond
        [(null? pats) #f]
        [(glob-match? (car pats) name) #t]
        [else (loop (cdr pats))])))

  ;; Get basename from path
  (def (path-basename path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) path]
        [(char=? (string-ref path i) #\/) (substring path (+ i 1) (string-length path))]
        [else (loop (- i 1))])))

  ;; List directory entries (excluding . and ..)
  (def (list-directory dir)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let ([entries (directory-list dir)])
          (filter (lambda (e) (and (not (string=? e "."))
                                   (not (string=? e ".."))))
                  entries)))))

  ;; Collect files to grep, handling recursion
  (def (collect-files paths recursive? deref? includes excludes exclude-dirs
                      suppress-errors?)
    (let loop ([todo paths] [acc '()])
      (if (null? todo)
        (reverse acc)
        (let ([path (car todo)])
          (if (string=? path "-")
            (loop (cdr todo) (cons "-" acc))
            (with-catch
              (lambda (e)
                (unless suppress-errors?
                  (eprintf "~a: ~a: No such file or directory~n"
                           (program-name) path))
                (loop (cdr todo) acc))
              (lambda ()
                (cond
                  [(file-directory? path)
                   (if recursive?
                     (let* ([base (path-basename path)]
                            [skip? (matches-any-glob? base exclude-dirs)])
                       (if skip?
                         (loop (cdr todo) acc)
                         (let* ([entries (list-directory path)]
                                [children (map (lambda (e)
                                                 (if (and (> (string-length path) 0)
                                                          (char=? (string-ref path
                                                                    (- (string-length path) 1))
                                                                  #\/))
                                                   (string-append path e)
                                                   (string-append path "/" e)))
                                               entries)])
                           (loop (append children (cdr todo)) acc))))
                     (begin
                       (unless suppress-errors?
                         (eprintf "~a: ~a: Is a directory~n"
                                  (program-name) path))
                       (loop (cdr todo) acc)))]
                  ;; Regular file — check include/exclude
                  [(file-regular? path)
                   (let ([name (path-basename path)])
                     (cond
                       ;; If includes specified, file must match one
                       [(and (not (null? includes))
                             (not (matches-any-glob? name includes)))
                        (loop (cdr todo) acc)]
                       ;; If excludes specified, file must not match any
                       [(matches-any-glob? name excludes)
                        (loop (cdr todo) acc)]
                       [else (loop (cdr todo) (cons path acc))]))]
                  ;; Symlinks in non-deref mode
                  [(and (file-exists? path) (not (file-regular? path))
                        (not (file-directory? path)))
                   ;; Skip special files silently
                   (loop (cdr todo) acc)]
                  [else
                   (unless suppress-errors?
                     (eprintf "~a: ~a: No such file or directory~n"
                              (program-name) path))
                   (loop (cdr todo) acc)]))))))))

  ;; ========== Output formatting ==========

  ;; Print the prefix (filename, line number, byte offset)
  (def (print-prefix filename lineno byte-offset
                     show-filename? show-lineno? show-byte-offset?
                     separator colorize?)
    (when show-filename?
      (if colorize?
        (begin (display *color-filename*) (display filename) (display *color-reset*))
        (display filename))
      (if colorize?
        (begin (display *color-sep*) (display separator) (display *color-reset*))
        (display separator)))
    (when show-lineno?
      (if colorize?
        (begin (display *color-lineno*) (display lineno) (display *color-reset*))
        (display lineno))
      (if colorize?
        (begin (display *color-sep*) (display separator) (display *color-reset*))
        (display separator)))
    (when show-byte-offset?
      (if colorize?
        (begin (display *color-lineno*) (display byte-offset) (display *color-reset*))
        (display byte-offset))
      (if colorize?
        (begin (display *color-sep*) (display separator) (display *color-reset*))
        (display separator))))

  ;; Print a line with color highlighting of matches
  (def (print-colored-line rx line)
    (let ([matches (pcre2-match-all rx line)])
      (if (null? matches)
        (display line)
        (let loop ([pos 0] [ms matches])
          (if (null? ms)
            ;; Print remainder
            (display (substring line pos (string-length line)))
            (let ([s (pcre2-match-result-start (car ms))]
                  [e (pcre2-match-result-end (car ms))])
              ;; Print text before match
              (when (> s pos)
                (display (substring line pos s)))
              ;; Print match in color
              (display *color-match*)
              (display (substring line s e))
              (display *color-reset*)
              (loop e (cdr ms))))))))

  ;; ========== Context tracking ==========

  (define-record-type context-state
    (fields
      (mutable before-buf)      ;; circular buffer of before-context lines
      (mutable before-meta)     ;; metadata (lineno, byte-offset) for before lines
      (mutable after-remaining) ;; lines of after-context still to print
      (mutable last-printed)    ;; last line number printed (for separator)
      (mutable any-printed?)))  ;; has anything been printed for this file?

  (def (make-fresh-context before-count)
    (make-context-state '() '() 0 0 #f))

  ;; ========== Core grep-file ==========

  (def (grep-file filename rx opts)
    (let ([show-filename? (hash-ref opts 'show-filename)]
          [show-lineno? (hash-get opts 'line-number)]
          [show-byte-offset? (hash-get opts 'byte-offset)]
          [invert? (hash-get opts 'invert-match)]
          [count-only? (hash-get opts 'count)]
          [quiet? (hash-get opts 'quiet)]
          [only-matching? (hash-get opts 'only-matching)]
          [files-with-match? (hash-get opts 'files-with-matches)]
          [files-without-match? (hash-get opts 'files-without-match)]
          [max-count (hash-ref opts 'max-count)]
          [colorize? (hash-ref opts 'colorize)]
          [null-sep? (hash-get opts 'null)]
          [zero-data? (hash-get opts 'null-data)]
          [before-ctx (hash-ref opts 'before-context)]
          [after-ctx (hash-ref opts 'after-context)]
          [label (hash-get opts 'label)]
          [initial-tab? (hash-get opts 'initial-tab)]
          [line-buffered? (hash-get opts 'line-buffered)]
          [suppress-errors? (hash-get opts 'no-messages)])

      (let ([display-name (cond
                            [(and (string=? filename "-") label) label]
                            [(string=? filename "-") "(standard input)"]
                            [else filename])]
            [file-sep (if null-sep? (string #\nul) "\n")]
            [match-count 0]
            [line-delim (if zero-data? #\nul #\newline)]
            [ctx (make-fresh-context before-ctx)])

        (define (open-file)
          (if (string=? filename "-")
            (current-input-port)
            (with-catch
              (lambda (e)
                (unless suppress-errors?
                  (eprintf "~a: ~a: No such file or directory~n"
                           (program-name) filename))
                #f)
              (lambda ()
                (audit-file-access! filename)
                (open-input-file filename)))))

        (define (process port)
          (let loop ([lineno 1]
                     [byte-offset 0]
                     [match-count 0])
            (if (>= match-count max-count)
              ;; Drain remaining after-context, then return count
              (begin
                (drain-after-context port ctx lineno byte-offset
                                    display-name show-filename? show-lineno?
                                    show-byte-offset? colorize? rx line-delim
                                    after-ctx before-ctx)
                match-count)
              (let ([line (read-line/delim port line-delim)])
                (if (eof-object? line)
                  match-count
                  (let* ([line-len (+ (string-length line) 1)] ;; +1 for delimiter
                         [raw-match (if only-matching?
                                      (line-matches? rx line #t)
                                      (if (pcre2-search rx line 0) #t #f))]
                         [matched? (if invert?
                                     (if only-matching?
                                       (null? raw-match)
                                       (not raw-match))
                                     (if only-matching?
                                       (and (pair? raw-match) (not (null? raw-match)))
                                       raw-match))])
                    (cond
                      ;; Quiet mode — just need to know if any match
                      [quiet?
                       (if matched?
                         1  ;; signal at least one match
                         (loop (+ lineno 1) (+ byte-offset line-len) match-count))]

                      ;; Count mode
                      [count-only?
                       (loop (+ lineno 1) (+ byte-offset line-len)
                             (if matched? (+ match-count 1) match-count))]

                      ;; Files-with-matches mode
                      [files-with-match?
                       (if matched?
                         (begin
                           (display display-name)
                           (display file-sep)
                           (when line-buffered? (flush-output-port))
                           1)
                         (loop (+ lineno 1) (+ byte-offset line-len) match-count))]

                      ;; Files-without-match mode
                      [files-without-match?
                       (if matched?
                         -1  ;; signal "has match" — will be filtered later
                         (loop (+ lineno 1) (+ byte-offset line-len) match-count))]

                      ;; Normal output
                      [matched?
                       ;; Print group separator if needed
                       (when (and (or (> before-ctx 0) (> after-ctx 0))
                                  (context-state-any-printed? ctx)
                                  (> lineno (+ (context-state-last-printed ctx) 1)))
                         (display "--")
                         (newline))

                       ;; Print buffered before-context (already in chronological order)
                       (let print-before ([buf (context-state-before-buf ctx)]
                                          [meta (context-state-before-meta ctx)])
                         (unless (null? buf)
                           (let ([ctx-line (car buf)]
                                 [ctx-lineno (caar meta)]
                                 [ctx-offset (cdar meta)])
                             (when (> ctx-lineno (context-state-last-printed ctx))
                               (print-prefix display-name ctx-lineno ctx-offset
                                             show-filename? show-lineno? show-byte-offset?
                                             "-" colorize?)
                               (display ctx-line)
                               (newline)
                               (context-state-last-printed-set! ctx ctx-lineno)))
                           (print-before (cdr buf) (cdr meta))))

                       ;; Print the matching line(s)
                       (if (and only-matching? (not invert?))
                         ;; -o mode: print each match on its own line
                         (for-each
                           (lambda (m)
                             (print-prefix display-name lineno
                                           (+ byte-offset (pcre2-match-result-start m))
                                           show-filename? show-lineno? show-byte-offset?
                                           ":" colorize?)
                             (if colorize?
                               (begin
                                 (display *color-match*)
                                 (display (pcre2-match-substring m))
                                 (display *color-reset*))
                               (display (pcre2-match-substring m)))
                             (newline))
                           raw-match)
                         ;; Normal: print the whole line
                         (begin
                           (print-prefix display-name lineno byte-offset
                                         show-filename? show-lineno? show-byte-offset?
                                         ":" colorize?)
                           (if colorize?
                             (print-colored-line rx line)
                             (display line))
                           (newline)))

                       (context-state-last-printed-set! ctx lineno)
                       (context-state-any-printed?-set! ctx #t)
                       (context-state-after-remaining-set! ctx after-ctx)
                       ;; Clear before buffer
                       (context-state-before-buf-set! ctx '())
                       (context-state-before-meta-set! ctx '())

                       (when line-buffered? (flush-output-port))
                       (loop (+ lineno 1) (+ byte-offset line-len) (+ match-count 1))]

                      ;; Non-matching line — maybe print as after-context
                      [else
                       (when (> (context-state-after-remaining ctx) 0)
                         (print-prefix display-name lineno byte-offset
                                       show-filename? show-lineno? show-byte-offset?
                                       "-" colorize?)
                         (display line)
                         (newline)
                         (context-state-last-printed-set! ctx lineno)
                         (context-state-any-printed?-set! ctx #t)
                         (context-state-after-remaining-set!
                           ctx (- (context-state-after-remaining ctx) 1))
                         (when line-buffered? (flush-output-port)))

                       ;; Add to before-context buffer
                       (when (> before-ctx 0)
                         (let ([buf (context-state-before-buf ctx)]
                               [meta (context-state-before-meta ctx)])
                           (if (>= (length buf) before-ctx)
                             (begin
                               (context-state-before-buf-set! ctx
                                 (append (cdr buf) (list line)))
                               (context-state-before-meta-set! ctx
                                 (append (cdr meta) (list (cons lineno byte-offset)))))
                             (begin
                               (context-state-before-buf-set! ctx
                                 (append buf (list line)))
                               (context-state-before-meta-set! ctx
                                 (append meta (list (cons lineno byte-offset))))))))

                       (loop (+ lineno 1) (+ byte-offset line-len) match-count)])))))))

        ;; Main file processing
        (let ([port (open-file)])
          (if (not port)
            0  ;; couldn't open file
            (let ([result (if (string=? filename "-")
                            (process port)
                            (dynamic-wind
                              (lambda () (void))
                              (lambda () (process port))
                              (lambda () (close-input-port port))))])
              ;; Handle count/files-without-match output
              (cond
                [count-only?
                 (when show-filename?
                   (display display-name)
                   (display ":"))
                 (display result)
                 (newline)
                 result]
                [files-without-match?
                 (when (not (= result -1))
                   (display display-name)
                   (display file-sep))
                 (if (= result -1) 0 1)]  ;; return match=found or not for exit code
                [else result])))))))

  ;; Drain remaining after-context lines after max-count reached
  (def (drain-after-context port ctx lineno byte-offset
                            display-name show-filename? show-lineno?
                            show-byte-offset? colorize? rx line-delim
                            after-ctx before-ctx)
    (let loop ([remaining (context-state-after-remaining ctx)]
               [lineno lineno]
               [byte-offset byte-offset])
      (when (> remaining 0)
        (let ([line (read-line/delim port line-delim)])
          (unless (eof-object? line)
            (print-prefix display-name lineno byte-offset
                          show-filename? show-lineno? show-byte-offset?
                          "-" colorize?)
            (display line)
            (newline)
            (context-state-last-printed-set! ctx lineno)
            (loop (- remaining 1)
                  (+ lineno 1)
                  (+ byte-offset (string-length line) 1)))))))

  ;; Custom line reader with delimiter support
  (def (read-line/delim port delim)
    (if (char=? delim #\newline)
      (get-line port)
      (let ([out (open-output-string)])
        (let loop ()
          (let ([c (read-char port)])
            (cond
              [(eof-object? c)
               (let ([s (get-output-string out)])
                 (if (string=? s "") c s))]
              [(char=? c delim)
               (get-output-string out)]
              [else
               (write-char c out)
               (loop)]))))))

  ;; ========== Argument parsing helpers ==========

  ;; Read patterns from a file (one per line)
  (def (read-patterns-file filename)
    (with-catch
      (lambda (e)
        (die "~a: No such file or directory" filename))
      (lambda ()
        (let ([port (open-input-file filename)])
          (dynamic-wind
            (lambda () (void))
            (lambda ()
              (let loop ([acc '()])
                (let ([line (get-line port)])
                  (if (eof-object? line)
                    (reverse acc)
                    (loop (cons line acc))))))
            (lambda () (close-input-port port)))))))

  ;; Parse -NUM shorthand for context
  (def (parse-context-shorthand args)
    (let loop ([rest args] [acc '()] [ctx #f])
      (if (null? rest)
        (values (reverse acc) ctx)
        (let ([arg (car rest)])
          (if (and (> (string-length arg) 1)
                   (char=? (string-ref arg 0) #\-)
                   (char-numeric? (string-ref arg 1))
                   ;; Make sure it's all digits
                   (let check ([i 1])
                     (if (>= i (string-length arg))
                       #t
                       (and (char-numeric? (string-ref arg i))
                            (check (+ i 1))))))
            (loop (cdr rest) acc
                  (string->number (substring arg 1 (string-length arg))))
            (loop (cdr rest) (cons arg acc) ctx))))))

  ;; Collect multiple -e and -f options manually from raw args
  (def (collect-patterns-from-args args)
    (let loop ([rest args] [patterns '()] [clean-args '()])
      (if (null? rest)
        (values (reverse patterns) (reverse clean-args))
        (let ([arg (car rest)])
          (cond
            ;; -e PATTERN
            [(and (string=? arg "-e") (not (null? (cdr rest))))
             (loop (cddr rest) (cons (cadr rest) patterns) clean-args)]
            [(and (> (string-length arg) 2)
                  (string=? (substring arg 0 2) "-e"))
             (loop (cdr rest)
                   (cons (substring arg 2 (string-length arg)) patterns)
                   clean-args)]
            ;; --regexp=PATTERN
            [(and (> (string-length arg) 9)
                  (string=? (substring arg 0 9) "--regexp="))
             (loop (cdr rest)
                   (cons (substring arg 9 (string-length arg)) patterns)
                   clean-args)]
            [(string=? arg "--regexp")
             (if (null? (cdr rest))
               (loop (cdr rest) patterns clean-args)
               (loop (cddr rest) (cons (cadr rest) patterns) clean-args))]
            ;; -f FILE
            [(and (string=? arg "-f") (not (null? (cdr rest))))
             (loop (cddr rest)
                   (append (reverse (read-patterns-file (cadr rest))) patterns)
                   clean-args)]
            [(and (> (string-length arg) 2)
                  (string=? (substring arg 0 2) "-f"))
             (loop (cdr rest)
                   (append (reverse (read-patterns-file
                                      (substring arg 2 (string-length arg))))
                           patterns)
                   clean-args)]
            ;; --file=FILE
            [(and (> (string-length arg) 7)
                  (string=? (substring arg 0 7) "--file="))
             (loop (cdr rest)
                   (append (reverse (read-patterns-file
                                      (substring arg 7 (string-length arg))))
                           patterns)
                   clean-args)]
            [(string=? arg "--file")
             (if (null? (cdr rest))
               (loop (cdr rest) patterns clean-args)
               (loop (cddr rest)
                     (append (reverse (read-patterns-file (cadr rest))) patterns)
                     clean-args))]
            ;; --include=GLOB  --exclude=GLOB  --exclude-dir=GLOB
            ;; These are multi-value options — pass through to clean args
            [else
             (loop (cdr rest) patterns (cons arg clean-args))])))))

  ;; Collect --include, --exclude, --exclude-dir from args (they can repeat)
  (def (collect-glob-opts args)
    (let loop ([rest args] [includes '()] [excludes '()] [exclude-dirs '()]
               [clean '()])
      (if (null? rest)
        (values (reverse includes) (reverse excludes)
                (reverse exclude-dirs) (reverse clean))
        (let ([arg (car rest)])
          (cond
            [(and (> (string-length arg) 10)
                  (string=? (substring arg 0 10) "--include="))
             (loop (cdr rest)
                   (cons (substring arg 10 (string-length arg)) includes)
                   excludes exclude-dirs clean)]
            [(and (string=? arg "--include") (not (null? (cdr rest))))
             (loop (cddr rest) (cons (cadr rest) includes)
                   excludes exclude-dirs clean)]
            [(and (> (string-length arg) 10)
                  (string=? (substring arg 0 10) "--exclude="))
             (loop (cdr rest) includes
                   (cons (substring arg 10 (string-length arg)) excludes)
                   exclude-dirs clean)]
            [(and (string=? arg "--exclude") (not (null? (cdr rest))))
             (loop (cddr rest) includes
                   (cons (cadr rest) excludes) exclude-dirs clean)]
            [(and (> (string-length arg) 14)
                  (string=? (substring arg 0 14) "--exclude-dir="))
             (loop (cdr rest) includes excludes
                   (cons (substring arg 14 (string-length arg)) exclude-dirs)
                   clean)]
            [(and (string=? arg "--exclude-dir") (not (null? (cdr rest))))
             (loop (cddr rest) includes excludes
                   (cons (cadr rest) exclude-dirs) clean)]
            [else
             (loop (cdr rest) includes excludes exclude-dirs
                   (cons arg clean))])))))

  ;; ========== Main entry point ==========

  (def (main . args)
    (parameterize ([program-name "grep"])
      (init-security!)
      ;; Skip seccomp — grep needs pread64, getuid, getgid, etc. for
      ;; recursive directory traversal and file type checks

      ;; Determine if invoked as egrep/fgrep from argv[0]
      (let* ([invoked-as (let ([env-cmd (getenv "_CU_CMD")])
                           (if env-cmd (path-basename env-cmd) "grep"))]
             [default-mode (cond
                             [(string=? invoked-as "egrep") 'ere]
                             [(string=? invoked-as "fgrep") 'fixed]
                             [else 'bre])])

        ;; Pre-process: extract -NUM context shorthand
        (let-values ([(args context-shorthand) (parse-context-shorthand args)])
          ;; Pre-process: extract -e/-f patterns and --include/--exclude
          (let-values ([(explicit-patterns args) (collect-patterns-from-args args)])
            (let-values ([(includes excludes exclude-dirs args)
                          (collect-glob-opts args)])
              ;; Split --key=value for getopt
              (let ([args (split-long-opts args)])
                (call-with-getopt
                  (lambda (_ opt)
                    (let* ([rest-args (hash-ref opt 'rest)]
                           ;; Mode selection
                           [mode (cond
                                   [(hash-get opt 'extended-regexp) 'ere]
                                   [(hash-get opt 'fixed-strings) 'fixed]
                                   [(hash-get opt 'perl-regexp) 'pcre]
                                   [(hash-get opt 'basic-regexp) 'bre]
                                   [else default-mode])]
                           ;; Collect patterns: -e/-f patterns, or first rest arg
                           [patterns
                            (if (null? explicit-patterns)
                              (if (null? rest-args)
                                (die "no pattern specified")
                                (list (car rest-args)))
                              explicit-patterns)]
                           ;; Files: remaining args after pattern consumed
                           [files
                            (if (null? explicit-patterns)
                              (if (null? rest-args)
                                '()
                                (cdr rest-args))
                              rest-args)]
                           ;; Flags
                           [case-insensitive? (hash-get opt 'ignore-case)]
                           [invert? (hash-get opt 'invert-match)]
                           [count-only? (hash-get opt 'count)]
                           [quiet? (hash-get opt 'quiet)]
                           [only-matching? (hash-get opt 'only-matching)]
                           [files-with-match? (hash-get opt 'files-with-matches)]
                           [files-without-match? (hash-get opt 'files-without-match)]
                           [show-lineno? (hash-get opt 'line-number)]
                           [show-byte-offset? (hash-get opt 'byte-offset)]
                           [word-match? (hash-get opt 'word-regexp)]
                           [line-match? (hash-get opt 'line-regexp)]
                           [null-sep? (hash-get opt 'null)]
                           [zero-data? (hash-get opt 'null-data)]
                           [suppress-errors? (hash-get opt 'no-messages)]
                           [recursive? (or (hash-get opt 'recursive)
                                           (hash-get opt 'dereference-recursive))]
                           [deref? (hash-get opt 'dereference-recursive)]
                           [label (hash-get opt 'label)]
                           [initial-tab? (hash-get opt 'initial-tab)]
                           [line-buffered? (hash-get opt 'line-buffered)]
                           [max-count-str (hash-get opt 'max-count)]
                           [max-count (if max-count-str
                                       (or (string->number max-count-str) -1)
                                       -1)]
                           [before-ctx-str (hash-get opt 'before-context)]
                           [after-ctx-str (hash-get opt 'after-context)]
                           [context-str (hash-get opt 'context)]
                           [before-ctx (or (and before-ctx-str
                                                (string->number before-ctx-str))
                                           (and context-str
                                                (string->number context-str))
                                           (and context-shorthand context-shorthand)
                                           0)]
                           [after-ctx (or (and after-ctx-str
                                               (string->number after-ctx-str))
                                          (and context-str
                                               (string->number context-str))
                                          (and context-shorthand context-shorthand)
                                          0)]
                           ;; Color handling
                           [color-opt (hash-get opt 'color)]
                           [colorize? (cond
                                        [(not color-opt) #f]
                                        [(string=? color-opt "never") #f]
                                        [(string=? color-opt "always") #t]
                                        [(string=? color-opt "auto")
                                         (let ([term (getenv "TERM")])
                                           (and term (not (string=? term "dumb"))))]
                                        [else #f])]
                           ;; Handle stdin default / recursive default
                           [files (cond
                                    [(null? files)
                                     (if recursive? '(".") '("-"))]
                                    [else files])]
                           ;; Collect actual files (with recursion)
                           [file-list (collect-files files recursive? deref?
                                                     includes excludes exclude-dirs
                                                     suppress-errors?)]
                           ;; Filename display: auto-detect based on file count
                           [multi-file? (> (length file-list) 1)]
                           [show-filename? (cond
                                             [(hash-get opt 'with-filename) #t]
                                             [(hash-get opt 'no-filename) #f]
                                             [else multi-file?])])

                      ;; Compile the regex
                      (let ([rx (with-catch
                                  (lambda (e)
                                    (eprintf "~a: ~a~n" (program-name)
                                             (if (message-condition? e)
                                               (condition-message e)
                                               "invalid regex"))
                                    (exit 2))
                                  (lambda ()
                                    (compile-patterns patterns mode
                                                      case-insensitive?
                                                      word-match? line-match?)))])

                        ;; Build options hash for grep-file
                        (let ([file-opts (make-hashtable string-hash string=?)])
                          (hashtable-set! file-opts "show-filename" show-filename?)
                          (hashtable-set! file-opts "line-number" show-lineno?)
                          (hashtable-set! file-opts "byte-offset" show-byte-offset?)
                          (hashtable-set! file-opts "invert-match" invert?)
                          (hashtable-set! file-opts "count" count-only?)
                          (hashtable-set! file-opts "quiet" quiet?)
                          (hashtable-set! file-opts "only-matching"
                            (and only-matching? (not invert?)))
                          (hashtable-set! file-opts "files-with-matches"
                            files-with-match?)
                          (hashtable-set! file-opts "files-without-match"
                            files-without-match?)
                          (hashtable-set! file-opts "max-count"
                            (if (< max-count 0) 999999999 max-count))
                          (hashtable-set! file-opts "colorize" colorize?)
                          (hashtable-set! file-opts "null" null-sep?)
                          (hashtable-set! file-opts "null-data" zero-data?)
                          (hashtable-set! file-opts "before-context" before-ctx)
                          (hashtable-set! file-opts "after-context" after-ctx)
                          (hashtable-set! file-opts "label" label)
                          (hashtable-set! file-opts "initial-tab" initial-tab?)
                          (hashtable-set! file-opts "line-buffered" line-buffered?)
                          (hashtable-set! file-opts "no-messages" suppress-errors?)

                          ;; Wrap hash in getopt-compatible accessor
                          (let ([opts-ht (make-eq-hashtable)])
                            (for-each
                              (lambda (key)
                                (hashtable-set! opts-ht
                                  (string->symbol key)
                                  (hashtable-ref file-opts key #f)))
                              (vector->list (hashtable-keys file-opts)))

                            ;; Process all files
                            (let ([total-matches
                                   (let loop ([flist file-list] [total 0])
                                     (if (null? flist)
                                       total
                                       (let ([count (grep-file (car flist) rx opts-ht)])
                                         ;; Quiet mode: exit immediately on first match
                                         (if (and quiet? (> count 0))
                                           (exit 0)
                                           (loop (cdr flist) (+ total count))))))])

                              ;; Clean up
                              (pcre2-free! rx)
                              (flush-output-port)

                              ;; Exit code: 0=match, 1=no match, 2=error
                              (exit (if (> total-matches 0) 0 1))))))))
                  args
                  'program: "grep"
                  'help: "Search for PATTERNS in each FILE."
                  ;; Pattern interpretation
                  (flag 'extended-regexp "-E" "--extended-regexp"
                    'help: "PATTERNS are extended regular expressions")
                  (flag 'fixed-strings "-F" "--fixed-strings"
                    'help: "PATTERNS are strings")
                  (flag 'basic-regexp "-G" "--basic-regexp"
                    'help: "PATTERNS are basic regular expressions")
                  (flag 'perl-regexp "-P" "--perl-regexp"
                    'help: "PATTERNS are Perl regular expressions")
                  ;; Matching
                  (flag 'ignore-case "-i" "--ignore-case"
                    'help: "ignore case distinctions in patterns and data")
                  (flag 'invert-match "-v" "--invert-match"
                    'help: "select non-matching lines")
                  (flag 'word-regexp "-w" "--word-regexp"
                    'help: "match only whole words")
                  (flag 'line-regexp "-x" "--line-regexp"
                    'help: "match only whole lines")
                  ;; Output
                  (flag 'count "-c" "--count"
                    'help: "print only a count of selected lines per FILE")
                  (flag 'quiet "-q" "--quiet"
                    'help: "suppress all normal output")
                  (flag 'only-matching "-o" "--only-matching"
                    'help: "show only nonempty parts of lines that match")
                  (flag 'files-with-matches "-l" "--files-with-matches"
                    'help: "print only names of FILEs with selected lines")
                  (flag 'files-without-match "-L" "--files-without-match"
                    'help: "print only names of FILEs with no selected lines")
                  (flag 'line-number "-n" "--line-number"
                    'help: "print line number with output lines")
                  (flag 'byte-offset "-b" "--byte-offset"
                    'help: "print the byte offset with output lines")
                  (flag 'with-filename "-H" "--with-filename"
                    'help: "print file name with output lines")
                  (flag 'no-filename "-h" "--no-filename"
                    'help: "suppress the file name prefix on output")
                  (flag 'null "-Z" "--null"
                    'help: "print 0 byte after FILE name")
                  (flag 'null-data "-z" "--null-data"
                    'help: "a data line ends in 0 byte, not newline")
                  (option 'max-count "-m" "--max-count"
                    'help: "stop after NUM selected lines" 'default: #f)
                  (flag 'no-messages "-s" "--no-messages"
                    'help: "suppress error messages")
                  (flag 'text "-a" "--text"
                    'help: "equivalent to --binary-files=text")
                  ;; Context
                  (option 'before-context "-B" "--before-context"
                    'help: "print NUM lines of leading context" 'default: #f)
                  (option 'after-context "-A" "--after-context"
                    'help: "print NUM lines of trailing context" 'default: #f)
                  (option 'context "-C" "--context"
                    'help: "print NUM lines of output context" 'default: #f)
                  ;; Color
                  (option 'color "--color" "--color"
                    'help: "use markers to highlight the matching strings"
                    'default: #f)
                  (option 'colour "--colour" "--colour"
                    'help: "use markers to highlight the matching strings"
                    'default: #f)
                  ;; File selection
                  (flag 'recursive "-r" "--recursive"
                    'help: "search directories recursively")
                  (flag 'dereference-recursive "-R" "--dereference-recursive"
                    'help: "likewise, but follow all symlinks")
                  ;; Misc
                  (option 'label "--label" "--label"
                    'help: "use LABEL as the standard input file name prefix"
                    'default: #f)
                  (flag 'initial-tab "-T" "--initial-tab"
                    'help: "make tabs line up (if necessary)")
                  (flag 'line-buffered "--line-buffered" "--line-buffered"
                    'help: "flush output on every line")
                  (rest-arguments 'rest)))))))))

  ) ;; end library

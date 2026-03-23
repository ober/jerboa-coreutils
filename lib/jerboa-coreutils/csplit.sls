#!chezscheme
;;; csplit.sls -- Split a file into sections determined by context lines

(library (jerboa-coreutils csplit)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            filter)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; Read all lines from a port
  (def (read-all-lines port)
    (let loop ((acc '()))
      (let ((line (get-line port)))
        (if (eof-object? line)
          (reverse acc)
          (loop (cons line acc))))))

  ;; Generate output filename: xx00, xx01, etc.
  (def (make-output-name prefix num digits)
    (let ((s (number->string num)))
      (string-append prefix (pad-num s digits))))

  (def (pad-num s width)
    (let loop ((result s))
      (if (>= (string-length result) width)
        result
        (loop (string-append "0" result)))))

  ;; Write lines to a file, return byte count
  (def (write-section lines filename quiet)
    (let ((port (open-output-file filename))
          (byte-count 0))
      (for-each
        (lambda (line)
          (display line port)
          (newline port)
          (set! byte-count (+ byte-count (string-length line) 1)))
        lines)
      (close-port port)
      (unless quiet
        (displayln byte-count))
      byte-count))

  (def (sublist lst start end)
    (let* ((dropped (let loop ((l lst) (n start))
                      (if (or (null? l) (<= n 0)) l
                        (loop (cdr l) (- n 1)))))
           (taken (let loop ((l dropped) (n (- end start)) (acc '()))
                    (if (or (null? l) (<= n 0)) (reverse acc)
                      (loop (cdr l) (- n 1) (cons (car l) acc))))))
      taken))

  ;; Find first line matching regex starting from pos
  (def (find-match lines regex start total)
    (let loop ((i start))
      (if (>= i total)
        #f
        (if (line-matches-pattern? (list-ref lines i) regex)
          i
          (loop (+ i 1))))))

  (def (line-matches-pattern? line pattern)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "printf '%s\\n' " (shell-quote line)
                     " | grep -q -e " (shell-quote pattern) " 2>/dev/null && echo y")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((result (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (and result (not (eof-object? result)) (string=? result "y"))))))))

  (def (string-contains-pattern? line pattern)
    (line-matches-pattern? line pattern))

  ;; Expand patterns: resolve {N} and {*}
  (def (expand-patterns pat-strings)
    (let loop ((rest pat-strings) (prev #f) (acc '()))
      (if (null? rest)
        (reverse acc)
        (let ((s (car rest)))
          (if (and (> (string-length s) 1)
                   (eqv? (string-ref s 0) #\{))
            (let* ((inner (substring s 1 (- (string-length s) 1)))
                   (count (if (string=? inner "*")
                            -1
                            (let ((n (string->number inner)))
                              (if n (inexact->exact n) 0)))))
              (if prev
                (if (= count -1)
                  (loop (cdr rest) prev (cons (cons 'repeat-inf prev) acc))
                  (let add ((n count) (a acc))
                    (if (<= n 0)
                      (loop (cdr rest) prev a)
                      (add (- n 1) (cons prev a)))))
                (loop (cdr rest) prev acc)))
            (loop (cdr rest) s (cons s acc)))))))

  ;; Main csplit logic
  (def (csplit-file lines patterns prefix digits quiet)
    (let ((total (length lines))
          (expanded (expand-patterns patterns)))
      (let loop ((pats expanded) (pos 0) (piece-num 0))
        (if (null? pats)
          (when (< pos total)
            (write-section (sublist lines pos total)
                          (make-output-name prefix piece-num digits)
                          quiet))
          (let ((pat (car pats)))
            (cond
              ((and (pair? pat) (eq? (car pat) 'repeat-inf))
               (let ((prev-str (cdr pat)))
                 (let repeat-loop ((rpos pos) (rpnum piece-num) (search-from (+ pos 1)))
                   (let ((result (apply-pattern lines prev-str search-from total)))
                     (if result
                       (let ((split-pos (car result)))
                         (if (>= split-pos rpos)
                           (begin
                             (write-section (sublist lines rpos split-pos)
                                           (make-output-name prefix rpnum digits)
                                           quiet)
                             (repeat-loop split-pos (+ rpnum 1) (+ split-pos 1)))
                           (loop (cdr pats) rpos rpnum)))
                       (loop (cdr pats) rpos rpnum))))))
              ((string? pat)
               (let ((result (apply-pattern lines pat pos total)))
                 (if result
                   (let ((split-pos (car result)))
                     (when (> split-pos pos)
                       (write-section (sublist lines pos split-pos)
                                     (make-output-name prefix piece-num digits)
                                     quiet))
                     (if (> split-pos pos)
                       (loop (cdr pats) split-pos (+ piece-num 1))
                       (loop (cdr pats) pos piece-num)))
                   (begin
                     (warn "'~a': match not found" pat)
                     (loop (cdr pats) pos piece-num)))))
              (else
               (loop (cdr pats) pos piece-num))))))))

  ;; Apply a single pattern
  (def (apply-pattern lines pat-str pos total)
    (cond
      ;; /REGEXP/ pattern
      ((and (> (string-length pat-str) 1)
            (eqv? (string-ref pat-str 0) #\/))
       (let* ((end (string-last-index pat-str #\/))
              (rx (if (and end (> end 0))
                    (substring pat-str 1 end)
                    (substring pat-str 1 (string-length pat-str))))
              (offset-str (if (and end (> end 0))
                            (substring pat-str (+ end 1) (string-length pat-str))
                            ""))
              (offset (if (string=? offset-str "") 0
                        (let ((n (string->number offset-str)))
                          (if n (inexact->exact n) 0))))
              (match-pos (find-match lines rx pos total)))
         (if match-pos
           (list (+ match-pos offset))
           #f)))
      ;; %REGEXP% pattern
      ((and (> (string-length pat-str) 1)
            (eqv? (string-ref pat-str 0) #\%))
       (let* ((end (string-last-index pat-str #\%))
              (rx (if (and end (> end 0))
                    (substring pat-str 1 end)
                    (substring pat-str 1 (string-length pat-str))))
              (match-pos (find-match lines rx pos total)))
         (if match-pos
           (list match-pos)
           #f)))
      ;; Line number
      (else
       (let ((n (string->number pat-str)))
         (if n
           (let ((target (- (inexact->exact n) 1)))
             (if (> target pos)
               (list target)
               #f))
           #f)))))

  (def (string-last-index str ch)
    (let loop ((i (- (string-length str) 1)))
      (cond
        ((< i 1) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (- i 1))))))

  (def (main . args)
    (parameterize ((program-name "csplit"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((prefix (or (hash-get opt 'prefix) "xx"))
                 (digits (if (hash-get opt 'suffix-format)
                           (let ((n (string->number (hash-ref opt 'suffix-format))))
                             (if n (inexact->exact n) 2))
                           2))
                 (quiet (hash-get opt 'quiet))
                 (files (hash-ref opt 'rest)))
              (when (null? files)
                (die "missing operand"))
              (let* ((input-file (car files))
                     (patterns (cdr files))
                     (port (if (string=? input-file "-")
                             (current-input-port)
                             (with-catch
                               (lambda (e) (die "~a: ~a" input-file (error-message e)))
                               (lambda () (open-input-file input-file)))))
                     (lines (read-all-lines port)))
                (unless (string=? input-file "-")
                  (close-port port))
                (when (null? patterns)
                  (die "missing pattern"))
                (csplit-file lines patterns prefix digits quiet))))
        args
        'program: "csplit"
        'help: "Split a file into sections determined by context lines."
        (option 'prefix "-f" "--prefix"
          'help: "use PREFIX instead of 'xx'"
          'default: #f)
        (option 'suffix-format "-n" "--digits"
          'help: "use specified number of digits instead of 2"
          'default: #f)
        (flag 'quiet "-s" "--quiet"
          'help: "do not print counts of output file sizes")
        (rest-arguments 'rest))))

  ) ;; end library

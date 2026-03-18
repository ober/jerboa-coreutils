#!chezscheme
;;; numfmt.sls -- Format numbers with human-readable suffixes

(library (jerboa-coreutils numfmt)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (only (std misc string) string-trim string-contains)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "numfmt"))
      ;; Pre-process --key=value into --key value
      (let ((args (split-long-opts args)))
      (call-with-getopt
        (lambda (_ opt)
            (let ((from-mode (or (hash-get opt 'from) "none"))
                  (to-mode (or (hash-get opt 'to) "none"))
                  (from-unit (if (hash-get opt 'from-unit) (string->number (hash-get opt 'from-unit)) 1))
                  (to-unit (if (hash-get opt 'to-unit) (string->number (hash-get opt 'to-unit)) 1))
                  (padding (if (hash-get opt 'padding) (string->number (hash-get opt 'padding)) 0))
                  (round-mode (or (hash-get opt 'round) "from-zero"))
                  (suffix (or (hash-get opt 'suffix) ""))
                  (header-lines (if (hash-get opt 'header) (string->number (hash-get opt 'header)) 0))
                  (zero-term? (hash-get opt 'zero-terminated))
                  (rest-nums (hash-ref opt 'rest)))
              (if (null? rest-nums)
                ;; Read from stdin
                (let ((delim (if zero-term? #\nul #\newline)))
                  (let loop ((hdr header-lines))
                    (let ((line (read-line/delim (current-input-port) delim)))
                      (unless (eof-object? line)
                        (if (> hdr 0)
                          (begin (display line) (newline)
                                 (loop (- hdr 1)))
                          (begin
                            (process-numfmt-line line from-mode to-mode
                                                from-unit to-unit padding
                                                round-mode suffix)
                            (loop 0)))))))
                ;; Process arguments
                (for-each
                  (lambda (num-str)
                    (process-numfmt-line num-str from-mode to-mode
                                        from-unit to-unit padding
                                        round-mode suffix))
                  rest-nums))))
        args
        'program: "numfmt"
        'help: "Reformat numbers."
        (option 'from "--from" "--from"
          'help: "auto-scale input numbers (auto, si, iec, iec-i)" 'default: #f)
        (option 'to "--to" "--to"
          'help: "auto-scale output numbers (auto, si, iec, iec-i)" 'default: #f)
        (option 'from-unit "--from-unit" "--from-unit"
          'help: "specify the input unit size" 'default: #f)
        (option 'to-unit "--to-unit" "--to-unit"
          'help: "specify the output unit size" 'default: #f)
        (option 'padding "--padding" "--padding"
          'help: "pad the output to N characters" 'default: #f)
        (option 'round "--round" "--round"
          'help: "rounding method: up, down, from-zero, towards-zero, nearest"
          'default: #f)
        (option 'suffix "--suffix" "--suffix"
          'help: "add SUFFIX to output numbers" 'default: #f)
        (option 'header "--header" "--header"
          'help: "print without conversion the first N header lines" 'default: #f)
        (flag 'zero-terminated "-z" "--zero-terminated"
          'help: "line delimiter is NUL, not newline")
        (rest-arguments 'rest)))))

  ;; Read line with custom delimiter
  (def (read-line/delim port delim)
    (if (eqv? delim #\newline)
      (get-line port)
      (let ((out (open-output-string)))
        (let loop ()
          (let ((c (read-char port)))
            (cond
              ((eof-object? c)
               (let ((s (get-output-string out)))
                 (if (string=? s "") c s)))
              ((eqv? c delim)
               (get-output-string out))
              (else
                (write-char c out)
                (loop))))))))

  (def (process-numfmt-line line from-mode to-mode from-unit to-unit
                             padding round-mode suffix)
    (let* ((trimmed (string-trim line))
           (val (parse-numfmt-input trimmed from-mode from-unit)))
      (if val
        (let* ((scaled (/ val to-unit))
               (result (format-numfmt-output scaled to-mode round-mode suffix)))
          (if (and padding (not (= padding 0)))
            (let ((padded (pad-string result padding)))
              (display padded))
            (display result))
          (newline))
        (begin
          (warn "invalid number: '~a'" trimmed)
          (display line)
          (newline)))))

  (def (parse-numfmt-input str mode unit)
    (if (equal? mode "none")
      (let ((n (string->number str)))
        (if n (* n unit) #f))
      ;; Parse with suffix
      (let* ((len (string-length str)))
        (if (= len 0) #f
          (let* ((last-c (string-ref str (- len 1)))
                 ;; Handle iec-i suffix (e.g., Ki, Mi)
                 (has-i? (and (> len 1) (eqv? last-c #\i)
                              (member mode '("iec-i" "auto"))))
                 (suffix-c (if has-i?
                             (string-ref str (- len 2))
                             last-c))
                 (suffix-info (parse-suffix suffix-c mode))
                 (num-str (cond
                            ((not (car suffix-info)) str)
                            (has-i? (substring str 0 (- len 2)))
                            (else (substring str 0 (- len 1)))))
                 (mult (cdr suffix-info)))
            (let ((n (string->number num-str)))
              (if n (* n mult unit) #f)))))))

  (def (parse-suffix c mode)
    (let ((base (if (member mode '("si" "auto")) 1000 1024)))
      (case (char-upcase c)
        ((#\K) (cons #t base))
        ((#\M) (cons #t (* base base)))
        ((#\G) (cons #t (* base base base)))
        ((#\T) (cons #t (* base base base base)))
        ((#\P) (cons #t (* base base base base base)))
        ((#\E) (cons #t (* base base base base base base)))
        (else (cons #f 1)))))

  (def (format-numfmt-output val mode round-mode suffix)
    (if (equal? mode "none")
      (string-append (format-number-plain val round-mode) suffix)
      ;; Format with suffix
      (let ((base (if (member mode '("si" "auto")) 1000 1024))
            (suffixes '("" "K" "M" "G" "T" "P" "E"))
            (iec-i? (equal? mode "iec-i")))
        (let loop ((v (inexact (abs val))) (rest suffixes))
          (if (or (null? (cdr rest)) (< v base))
            (let* ((sign (if (< val 0) "-" ""))
                   (suf (car rest))
                   (suf (if (and iec-i? (not (string=? suf "")))
                          (string-append suf "i") suf))
                   ;; Format: if there's a suffix, show one decimal place
                   (num-str (if (string=? suf "")
                              (format-number-plain v round-mode)
                              (format-one-decimal v))))
              (string-append sign num-str suf suffix))
            (loop (/ v base) (cdr rest)))))))

  ;; Format a number with exactly one decimal place (e.g., 1.0, 1.5)
  (def (format-one-decimal val)
    (let* ((rounded (/ (round (* val 10)) 10))
           (int-part (inexact->exact (floor rounded)))
           (frac (inexact->exact (round (* (- rounded int-part) 10)))))
      (when (>= frac 10)
        (set! int-part (+ int-part 1))
        (set! frac 0))
      (string-append (number->string int-part) "." (number->string frac))))

  (def (format-number-plain val round-mode)
    (let ((rounded (round-number (abs val) round-mode)))
      (string-append (if (< val 0) "-" "")
                     (format-rounded rounded))))

  (def (format-rounded val)
    (if (integer? val)
      (number->string (inexact->exact val))
      (let ((s (number->string (inexact val))))
        ;; Trim trailing zeros after decimal
        (if (string-contains s ".")
          (let loop ((i (- (string-length s) 1)))
            (cond
              ((eqv? (string-ref s i) #\0) (loop (- i 1)))
              ((eqv? (string-ref s i) #\.) (substring s 0 i))
              (else (substring s 0 (+ i 1)))))
          s))))

  (def (round-number val mode)
    (cond
      ((equal? mode "up") (ceiling val))
      ((equal? mode "down") (floor val))
      ((equal? mode "towards-zero") (truncate val))
      ((equal? mode "from-zero")
       (if (>= val 0) (ceiling val) (floor val)))
      ((equal? mode "nearest") (round val))
      (else (round val))))

  (def (pad-string str padding)
    (let ((width (abs padding))
          (len (string-length str)))
      (if (>= len width) str
        (let ((pad (make-string (- width len) #\space)))
          (if (> padding 0)
            (string-append pad str)
            (string-append str pad))))))

  ) ;; end library

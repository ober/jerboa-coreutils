#!chezscheme
;;; seq.sls -- Generate number sequences

(library (jerboa-coreutils seq)
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

  (def (parse-number str)
    (let ((n (string->number str)))
      (unless n
        (die "invalid floating point argument: '~a'" str))
      n))

  (def (string-index str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  (def (has-decimal? args)
    (any (lambda (s) (string-index s #\.)) args))

  (def (string-contains str sub)
    (let ((slen (string-length str))
          (sublen (string-length sub)))
      (let loop ((i 0))
        (cond
          ((> (+ i sublen) slen) #f)
          ((string=? (substring str i (+ i sublen)) sub) #t)
          (else (loop (+ i 1)))))))

  (def (max-decimals args)
    ;; Find max decimal places in the number arguments
    (apply max
      (map (lambda (s)
             (let ((dot (string-index s #\.)))
               (if dot
                 (- (string-length s) dot 1)
                 0)))
           args)))

  (def (string-pad-left str width ch)
    (let ((len (string-length str)))
      (if (>= len width)
        str
        (string-append (make-string (- width len) ch) str))))

  (def (format-float n decimal-places)
    ;; Format a number with fixed decimal places without using printf-style format
    (let* ((factor (expt 10 decimal-places))
           (rounded (inexact->exact (round (* n factor))))
           (negative? (< rounded 0))
           (abs-rounded (abs rounded))
           (int-part (quotient abs-rounded factor))
           (frac-part (remainder abs-rounded factor))
           (frac-str (number->string frac-part))
           (frac-padded (string-pad-left frac-str decimal-places #\0)))
      (string-append
        (if negative? "-" "")
        (number->string int-part)
        "."
        frac-padded)))

  ;; Simple printf-style format converter for seq's limited needs
  ;; Supports %g, %f, %e, and %NNf / %NN.DDf style specifiers
  (def (printf-format fmt n)
    (let ((len (string-length fmt)))
      (let loop ((i 0) (out (open-output-string)))
        (cond
          ((>= i len) (get-output-string out))
          ((and (eqv? (string-ref fmt i) #\%)
                (< (+ i 1) len))
           (cond
             ;; %% -> literal %
             ((eqv? (string-ref fmt (+ i 1)) #\%)
              (write-char #\% out)
              (loop (+ i 2) out))
             (else
              ;; Parse optional width and precision: %[width][.precision]specifier
              (let parse-spec ((j (+ i 1)) (width-chars '()))
                (cond
                  ((>= j len)
                   ;; Incomplete format spec, just output as-is
                   (display (substring fmt i len) out)
                   (loop len out))
                  ((eqv? (string-ref fmt j) #\g)
                   ;; %g -> general number format
                   (display (number->string (inexact n)) out)
                   (loop (+ j 1) out))
                  ((eqv? (string-ref fmt j) #\f)
                   ;; %f or %W.Df -> fixed decimal
                   (let* ((spec (list->string (reverse width-chars)))
                          (dot (string-index spec #\.))
                          (precision (if dot
                                      (let ((p (string->number (substring spec (+ dot 1) (string-length spec)))))
                                        (if p (inexact->exact p) 6))
                                      6)))
                     (display (format-float n precision) out)
                     (loop (+ j 1) out)))
                  ((eqv? (string-ref fmt j) #\e)
                   ;; %e -> scientific notation (basic)
                   (let* ((spec (list->string (reverse width-chars)))
                          (dot (string-index spec #\.))
                          (precision (if dot
                                      (let ((p (string->number (substring spec (+ dot 1) (string-length spec)))))
                                        (if p (inexact->exact p) 6))
                                      6))
                          (abs-n (abs n))
                          (exp-val (if (zero? abs-n) 0
                                     (inexact->exact (floor (log abs-n 10)))))
                          (mantissa (if (zero? abs-n) 0.0
                                      (/ n (expt 10.0 exp-val)))))
                     (display (format-float mantissa precision) out)
                     (display "e" out)
                     (if (>= exp-val 0)
                       (display "+" out)
                       (display "-" out))
                     (let ((es (number->string (abs exp-val))))
                       (when (< (string-length es) 2)
                         (display "0" out))
                       (display es out))
                     (loop (+ j 1) out)))
                  ((or (char-numeric? (string-ref fmt j))
                       (eqv? (string-ref fmt j) #\.))
                   (parse-spec (+ j 1) (cons (string-ref fmt j) width-chars)))
                  (else
                   ;; Unknown specifier, output as-is
                   (display (substring fmt i (+ j 1)) out)
                   (loop (+ j 1) out)))))))
          (else
           (write-char (string-ref fmt i) out)
           (loop (+ i 1) out))))))

  (def (number-width n)
    (string-length
      (if (integer? n)
        (number->string (inexact->exact n))
        (number->string n))))

  (def (main . args)
    (parameterize ((program-name "seq"))
      (call-with-getopt
        (lambda (_ opt)
          (let ((sep (or (hash-get opt 'separator) "\n"))
                (fmt (hash-get opt 'format))
                (equal-width (hash-get opt 'equal-width))
                (rest-args (hash-ref opt 'rest)))
            (when (null? rest-args)
              (die "missing operand"))
            (let-values (((first incr last)
                          (case (length rest-args)
                            ((1) (values 1 1 (parse-number (car rest-args))))
                            ((2) (values (parse-number (car rest-args))
                                         1
                                         (parse-number (cadr rest-args))))
                            ((3) (values (parse-number (car rest-args))
                                         (parse-number (cadr rest-args))
                                         (parse-number (caddr rest-args))))
                            (else (die "extra operand '~a'" (list-ref rest-args 3))))))
              (when (zero? incr)
                (die "invalid Zero increment value: '~a'" (cadr rest-args)))
              (let* ((use-float (or (has-decimal? rest-args)
                                    (and fmt (string-contains fmt "."))))
                     (width (if equal-width
                              (max (number-width first) (number-width last))
                              0))
                     (format-num
                       (cond
                         (fmt (lambda (n) (printf-format fmt n)))
                         ((and equal-width (not use-float))
                          (lambda (n)
                            (let ((s (number->string (inexact->exact n))))
                              (string-pad-left s width #\0))))
                         (use-float
                          (lambda (n)
                            (let ((s (format-float n (max-decimals rest-args))))
                              (if equal-width
                                (string-pad-left s width #\0)
                                s))))
                         (else (lambda (n) (number->string (inexact->exact n)))))))
                (let ((first-printed #f))
                  (let loop ((cur first))
                    (when (if (> incr 0) (<= cur last) (>= cur last))
                      (when first-printed
                        (display sep))
                      (display (format-num cur))
                      (set! first-printed #t)
                      (loop (+ cur incr))))
                  (when first-printed
                    (newline)))))))
        args
        'program: "seq"
        'help: "Print numbers from FIRST to LAST, in steps of INCREMENT."
        (option 'separator "-s" "--separator"
          'help: "use STRING to separate numbers ('default: \\n)"
          'default: #f)
        (option 'format "-f" "--format"
          'help: "use printf style floating-point FORMAT"
          'default: #f)
        (flag 'equal-width "-w" "--equal-width"
          'help: "equalize width by padding with leading zeroes")
        (rest-arguments 'rest))))

  ) ;; end library

#!chezscheme
;;; echo.sls -- Display a line of text

(library (jerboa-coreutils echo)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "echo"))
      (let loop ((args args)
                 (newline? #t)
                 (escapes? #f))
        (cond
          ;; Parse leading flags
          ((and (pair? args) (equal? (car args) "-n"))
           (loop (cdr args) #f escapes?))
          ((and (pair? args) (equal? (car args) "-e"))
           (loop (cdr args) newline? #t))
          ((and (pair? args) (equal? (car args) "-E"))
           (loop (cdr args) newline? #f))
          ;; Combined flags like -neE
          ((and (pair? args) (echo-flags? (car args)))
           (let-values (((nl? esc?) (parse-echo-flags (car args) newline? escapes?)))
             (loop (cdr args) nl? esc?)))
          (else
            ;; Output
            (let out-loop ((rest args) (first? #t))
              (when (pair? rest)
                (unless first? (display " "))
                (if escapes?
                  (let ((result (process-escapes (car rest))))
                    (display (car result))
                    (when (cdr result) ;; \c encountered - stop
                      (exit 0)))
                  (display (car rest)))
                (out-loop (cdr rest) #f)))
            (when newline? (newline)))))))

  ;; Check if arg is a valid echo flag string like -nEe, -neE, etc.
  (def (echo-flags? s)
    (and (string? s)
         (> (string-length s) 1)
         (eqv? (string-ref s 0) #\-)
         (let loop ((i 1))
           (if (>= i (string-length s)) #t
             (and (memv (string-ref s i) '(#\n #\e #\E))
                  (loop (+ i 1)))))))

  ;; Parse combined flags
  (def (parse-echo-flags s newline? escapes?)
    (let loop ((i 1) (nl? newline?) (esc? escapes?))
      (if (>= i (string-length s))
        (values nl? esc?)
        (case (string-ref s i)
          ((#\n) (loop (+ i 1) #f esc?))
          ((#\e) (loop (+ i 1) nl? #t))
          ((#\E) (loop (+ i 1) nl? #f))
          (else (values nl? esc?))))))

  ;; Process escape sequences. Returns (processed-string . stop?)
  (def (process-escapes str)
    (let ((out (open-output-string))
          (len (string-length str)))
      (let loop ((i 0))
        (cond
          ((>= i len)
           (cons (get-output-string out) #f))
          ((and (eqv? (string-ref str i) #\\) (< (+ i 1) len))
           (case (string-ref str (+ i 1))
             ((#\\) (write-char #\\ out) (loop (+ i 2)))
             ((#\a) (write-char #\alarm out) (loop (+ i 2)))
             ((#\b) (write-char #\backspace out) (loop (+ i 2)))
             ((#\c) (cons (get-output-string out) #t)) ;; stop
             ((#\e) (write-char (integer->char 27) out) (loop (+ i 2)))
             ((#\f) (write-char #\page out) (loop (+ i 2)))
             ((#\n) (write-char #\newline out) (loop (+ i 2)))
             ((#\r) (write-char #\return out) (loop (+ i 2)))
             ((#\t) (write-char #\tab out) (loop (+ i 2)))
             ((#\v) (write-char (integer->char 11) out) (loop (+ i 2)))
             ((#\0) ;; octal \0NNN
              (let oloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 3)
                         (char>=? (string-ref str j) #\0)
                         (char<=? (string-ref str j) #\7))
                  (oloop (+ j 1)
                         (+ (* val 8) (- (char->integer (string-ref str j)) (char->integer #\0)))
                         (+ count 1))
                  (begin
                    (write-char (integer->char (modulo val 256)) out)
                    (loop j)))))
             ((#\x) ;; hex \xHH
              (let xloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 2) (hex-digit? (string-ref str j)))
                  (xloop (+ j 1)
                         (+ (* val 16) (hex-value (string-ref str j)))
                         (+ count 1))
                  (if (> count 0)
                    (begin (write-char (integer->char val) out) (loop j))
                    (begin (write-char #\\ out) (write-char #\x out) (loop (+ i 2)))))))
             (else
               (write-char #\\ out)
               (write-char (string-ref str (+ i 1)) out)
               (loop (+ i 2)))))
          (else
            (write-char (string-ref str i) out)
            (loop (+ i 1)))))))

  (def (hex-digit? c)
    (or (and (char>=? c #\0) (char<=? c #\9))
        (and (char>=? c #\a) (char<=? c #\f))
        (and (char>=? c #\A) (char<=? c #\F))))

  (def (hex-value c)
    (cond
      ((and (char>=? c #\0) (char<=? c #\9))
       (- (char->integer c) (char->integer #\0)))
      ((and (char>=? c #\a) (char<=? c #\f))
       (+ 10 (- (char->integer c) (char->integer #\a))))
      ((and (char>=? c #\A) (char<=? c #\F))
       (+ 10 (- (char->integer c) (char->integer #\A))))
      (else 0)))

  ) ;; end library

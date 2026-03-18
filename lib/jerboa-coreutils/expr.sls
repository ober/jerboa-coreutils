#!chezscheme
;;; expr.sls -- Evaluate expressions

(library (jerboa-coreutils expr)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; expr - evaluate expressions
  ;; Tokens come from command-line arguments

  (def (expr-error msg)
    (eprintf "expr: ~a\n" msg)
    (exit 2))

  ;; Tokenizer state - just a list of args
  (define *tokens* '())

  (def (peek-token)
    (if (null? *tokens*) #f (car *tokens*)))

  (def (next-token!)
    (if (null? *tokens*)
      (expr-error "syntax error")
      (let ((t (car *tokens*)))
        (set! *tokens* (cdr *tokens*))
        t)))

  (def (expect-token! expected)
    (let ((t (next-token!)))
      (unless (string=? t expected)
        (expr-error (string-append "expected '" expected "' but got '" t "'")))
      t))

  ;; Value predicates
  (def (expr-val-null? v)
    (or (string=? v "") (string=? v "0")))

  (def (expr-val-true? v)
    (not (expr-val-null? v)))

  (def (to-integer v)
    (let ((n (string->number v)))
      (if (and n (exact? n) (integer? n))
        n
        #f)))

  ;; Grammar:
  ;; expr ::= or-expr
  ;; or-expr ::= and-expr ('|' and-expr)*
  ;; and-expr ::= cmp-expr ('&' cmp-expr)*
  ;; cmp-expr ::= add-expr (cmp-op add-expr)?
  ;; add-expr ::= mul-expr (('+' | '-') mul-expr)*
  ;; mul-expr ::= unary-expr (('*' | '/' | '%') unary-expr)*
  ;; unary-expr ::= '(' expr ')' | string-expr | NUMBER

  (def (parse-expr)
    (parse-or))

  (def (parse-or)
    (let ((left (parse-and)))
      (let loop ((left left))
        (let ((t (peek-token)))
          (if (and t (string=? t "|"))
            (begin
              (next-token!)
              (let ((right (parse-and)))
                (let ((result (if (expr-val-true? left) left right)))
                  (loop result))))
            left)))))

  (def (parse-and)
    (let ((left (parse-cmp)))
      (let loop ((left left))
        (let ((t (peek-token)))
          (if (and t (string=? t "&"))
            (begin
              (next-token!)
              (let ((right (parse-cmp)))
                (let ((result (if (and (expr-val-true? left) (expr-val-true? right))
                               left
                               "0")))
                  (loop result))))
            left)))))

  (def (parse-cmp)
    (let ((left (parse-add)))
      (let ((t (peek-token)))
        (if (and t (member t '("=" "!=" "<" "<=" ">" ">=")))
          (begin
            (next-token!)
            (let ((right (parse-add)))
              ;; Try numeric first, fall back to string
              (let ((ln (to-integer left))
                    (rn (to-integer right)))
                (let ((result
                       (if (and ln rn)
                         ;; numeric compare
                         (cond
                           ((string=? t "=")  (= ln rn))
                           ((string=? t "!=") (not (= ln rn)))
                           ((string=? t "<")  (< ln rn))
                           ((string=? t "<=") (<= ln rn))
                           ((string=? t ">")  (> ln rn))
                           ((string=? t ">=") (>= ln rn))
                           (else #f))
                         ;; string compare
                         (cond
                           ((string=? t "=")  (string=? left right))
                           ((string=? t "!=") (not (string=? left right)))
                           ((string=? t "<")  (string<? left right))
                           ((string=? t "<=") (string<=? left right))
                           ((string=? t ">")  (string>? left right))
                           ((string=? t ">=") (string>=? left right))
                           (else #f)))))
                  (if result "1" "0")))))
          left))))

  (def (parse-add)
    (let ((left (parse-mul)))
      (let loop ((left left))
        (let ((t (peek-token)))
          (if (and t (or (string=? t "+") (string=? t "-")))
            (begin
              (next-token!)
              (let ((right (parse-mul)))
                (let ((ln (to-integer left))
                      (rn (to-integer right)))
                  (unless (and ln rn)
                    (expr-error "non-integer argument"))
                  (let ((result (if (string=? t "+") (+ ln rn) (- ln rn))))
                    (loop (number->string result))))))
            left)))))

  (def (parse-mul)
    (let ((left (parse-unary)))
      (let loop ((left left))
        (let ((t (peek-token)))
          (if (and t (member t '("*" "/" "%")))
            (begin
              (next-token!)
              (let ((right (parse-unary)))
                (let ((ln (to-integer left))
                      (rn (to-integer right)))
                  (unless (and ln rn)
                    (expr-error "non-integer argument"))
                  (when (and (member t '("/" "%")) (zero? rn))
                    (expr-error "division by zero"))
                  (let ((result
                         (cond
                           ((string=? t "*") (* ln rn))
                           ((string=? t "/") (quotient ln rn))
                           ((string=? t "%") (remainder ln rn))
                           (else 0))))
                    (loop (number->string result))))))
            left)))))

  (def (parse-unary)
    (let ((t (peek-token)))
      (cond
        ((not t) (expr-error "syntax error: missing argument"))
        ((string=? t "(")
         (next-token!)
         (let ((result (parse-expr)))
           (expect-token! ")")
           result))
        ((string=? t "match")
         (next-token!)
         (let* ((s (parse-unary))
                (pattern (parse-unary)))
           (do-match s pattern)))
        ((string=? t "substr")
         (next-token!)
         (let* ((s (parse-unary))
                (pos-s (parse-unary))
                (len-s (parse-unary))
                (pos (to-integer pos-s))
                (len (to-integer len-s)))
           (unless (and pos len)
             (expr-error "non-integer argument"))
           (do-substr s pos len)))
        ((string=? t "index")
         (next-token!)
         (let* ((s (parse-unary))
                (chars (parse-unary)))
           (do-index s chars)))
        ((string=? t "length")
         (next-token!)
         (let ((s (parse-unary)))
           (number->string (string-length s))))
        (else
         ;; atom - could be : operator check after
         (let ((atom (next-token!)))
           (let ((next (peek-token)))
             (if (and next (string=? next ":"))
               (begin
                 (next-token!)
                 (let ((pattern (next-token!)))
                   (do-match atom pattern)))
               atom)))))))

  ;; Simple regex match using system grep for BRE support
  (def (do-match str pattern)
    ;; Anchor at start (POSIX expr uses anchored match)
    (let* ((anchored (if (and (> (string-length pattern) 0)
                              (eqv? (string-ref pattern 0) #\^))
                       pattern
                       (string-append "^" pattern)))
           ;; Check if pattern has \( \) groups
           (has-group (string-contains-substr? pattern "\\(")))
      (with-catch
        (lambda (e) "0")
        (lambda ()
          ;; Use grep -o or sed to do BRE matching
          (let ((cmd (string-append
                       "printf '%s' " (shell-quote str)
                       " | grep -oP " (shell-quote (bre->pcre anchored))
                       " 2>/dev/null | head -1")))
            (let-values (((to-stdin from-stdout from-stderr pid)
                          (open-process-ports cmd (buffer-mode block) (native-transcoder))))
              (close-port to-stdin)
              (let ((result (get-line from-stdout)))
                (close-port from-stdout)
                (close-port from-stderr)
                (if (or (not result) (eof-object? result))
                  "0"
                  (if has-group
                    ;; Try to extract group match via sed
                    (do-match-with-group str anchored)
                    (number->string (string-length result)))))))))))

  (def (do-match-with-group str pattern)
    (with-catch
      (lambda (e) "0")
      (lambda ()
        (let ((cmd (string-append
                     "printf '%s' " (shell-quote str)
                     " | sed -n 's/" pattern "/\\1/p' 2>/dev/null")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((result (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not result) (eof-object? result))
                ""
                result)))))))

  ;; Convert BRE to PCRE (simplified)
  (def (bre->pcre pattern)
    (let loop ((i 0) (acc '()))
      (if (>= i (string-length pattern))
        (list->string (reverse acc))
        (let ((c (string-ref pattern i)))
          (if (and (eqv? c #\\) (< (+ i 1) (string-length pattern)))
            (let ((next (string-ref pattern (+ i 1))))
              (cond
                ((eqv? next #\()
                 (loop (+ i 2) (cons #\( acc)))
                ((eqv? next #\))
                 (loop (+ i 2) (cons #\) acc)))
                (else
                 (loop (+ i 2) (cons next (cons #\\ acc))))))
            ;; Literal ( and ) in BRE -> escape them
            (if (or (eqv? c #\() (eqv? c #\)))
              (loop (+ i 1) (cons c (cons #\\ acc)))
              (loop (+ i 1) (cons c acc))))))))

  (def (shell-quote str)
    (string-append "'" (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (eqv? c #\')
            (loop (+ i 1) (append (reverse (string->list "'\\''")) acc))
            (loop (+ i 1) (cons c acc)))))) "'"))

  (def (string-contains-substr? str sub)
    (let ((slen (string-length str))
          (sublen (string-length sub)))
      (if (> sublen slen) #f
        (let loop ((i 0))
          (cond
            ((> (+ i sublen) slen) #f)
            ((string=? (substring str i (+ i sublen)) sub) #t)
            (else (loop (+ i 1))))))))

  (def (do-substr str pos len)
    (let* ((slen (string-length str))
           (p (max 0 (- pos 1)))  ;; 1-based
           (p2 (min slen (+ p len))))
      (if (>= p slen)
        ""
        (substring str p p2))))

  (def (do-index str chars)
    ;; Find first occurrence of any char in chars within str
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) "0")
        ((string-contains-char? chars (string-ref str i))
         (number->string (+ i 1)))
        (else (loop (+ i 1))))))

  (def (string-contains-char? str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) #t)
        (else (loop (+ i 1))))))

  (def (main . args)
    (parameterize ((program-name "expr"))
      (when (null? args)
        (die "missing operand"))
      (when (string=? (car args) "--help")
        (displayln "Usage: expr EXPRESSION")
        (displayln "  or:  expr OPTION")
        (displayln "Print the value of EXPRESSION to standard output.")
        (exit 0))
      (when (string=? (car args) "--version")
        (version-info "expr")
        (exit 0))
      (set! *tokens* args)
      (let ((result (parse-expr)))
        (unless (null? *tokens*)
          (expr-error "syntax error: extra tokens"))
        (displayln result)
        (exit (if (expr-val-null? result) 1 0)))))

  ) ;; end library

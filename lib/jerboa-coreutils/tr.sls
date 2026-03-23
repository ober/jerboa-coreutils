#!chezscheme
;;; tr.sls -- Translate, squeeze, delete characters

(library (jerboa-coreutils tr)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Expand combined flags like -cd -> -c -d, -ds -> -d -s
  (def (expand-tr-flags args)
    (let loop ((rest args) (acc '()))
      (if (null? rest)
        (reverse acc)
        (let ((arg (car rest)))
          (if (and (> (string-length arg) 2)
                   (eqv? (string-ref arg 0) #\-)
                   (not (eqv? (string-ref arg 1) #\-))
                   (tr-flags-only? arg 1))
            ;; Expand: -cd -> -c -d
            (let expand ((i 1) (expanded acc))
              (if (>= i (string-length arg))
                (loop (cdr rest) expanded)
                (expand (+ i 1)
                        (cons (string-append "-" (string (string-ref arg i)))
                              expanded))))
            (loop (cdr rest) (cons arg acc)))))))

  (def (tr-flags-only? str start)
    (let loop ((i start))
      (cond
        ((>= i (string-length str)) #t)
        ((memv (string-ref str i) '(#\c #\C #\d #\s #\t)) (loop (+ i 1)))
        (else #f))))

  (def (char-range start end)
    (let loop ((c (char->integer start)) (acc '()))
      (if (> c (char->integer end))
        (reverse acc)
        (loop (+ c 1) (cons (integer->char c) acc)))))

  (def (parse-octal str pos len)
    (let loop ((i pos) (val 0) (count 0))
      (if (and (< i len) (< count 3)
               (char>=? (string-ref str i) #\0)
               (char<=? (string-ref str i) #\7))
        (loop (+ i 1)
              (+ (* val 8) (- (char->integer (string-ref str i)) (char->integer #\0)))
              (+ count 1))
        (values val i))))

  (def (tr-filter pred lst)
    (let loop ((l lst) (acc '()))
      (cond
        ((null? l) (reverse acc))
        ((pred (car l)) (loop (cdr l) (cons (car l) acc)))
        (else (loop (cdr l) acc)))))

  (def (find-char-class-end str pos len)
    (let loop ((i pos))
      (if (>= i len) #f
        (if (and (eqv? (string-ref str i) #\:)
                 (< (+ i 1) len)
                 (eqv? (string-ref str (+ i 1)) #\]))
          i
          (loop (+ i 1))))))

  (def (expand-char-class name)
    (cond
      ((equal? name "upper") (char-range #\A #\Z))
      ((equal? name "lower") (char-range #\a #\z))
      ((equal? name "alpha") (append (char-range #\A #\Z) (char-range #\a #\z)))
      ((equal? name "digit") (char-range #\0 #\9))
      ((equal? name "alnum") (append (char-range #\A #\Z) (char-range #\a #\z)
                                     (char-range #\0 #\9)))
      ((equal? name "space") (list #\space #\tab #\newline #\return
                                   (integer->char 11) (integer->char 12)))
      ((equal? name "blank") (list #\space #\tab))
      ((equal? name "print") (char-range (integer->char 32) (integer->char 126)))
      ((equal? name "graph") (char-range (integer->char 33) (integer->char 126)))
      ((equal? name "punct")
       (tr-filter (lambda (c)
                 (and (not (char-alphabetic? c))
                      (not (char-numeric? c))
                      (not (eqv? c #\space))))
               (char-range (integer->char 33) (integer->char 126))))
      ((equal? name "cntrl") (append (char-range (integer->char 0) (integer->char 31))
                                     (list (integer->char 127))))
      ((equal? name "xdigit") (append (char-range #\0 #\9) (char-range #\A #\F)
                                      (char-range #\a #\f)))
      (else (die "invalid character class '~a'" name) '())))

  ;; Expand a SET string into a list of characters
  (def (expand-set str)
    (let ((len (string-length str)))
      (let loop ((i 0) (acc '()))
        (if (>= i len)
          (reverse acc)
          (let ((c (string-ref str i)))
            (cond
              ;; Character class [:alpha:] etc.
              ((and (eqv? c #\[) (< (+ i 2) len) (eqv? (string-ref str (+ i 1)) #\:))
               (let ((end (find-char-class-end str (+ i 2) len)))
                 (if end
                   (let* ((class-name (substring str (+ i 2) end))
                          (chars (expand-char-class class-name)))
                     (loop (+ end 2) (append (reverse chars) acc)))
                   (loop (+ i 1) (cons c acc)))))
              ;; Escape sequences
              ((eqv? c #\\)
               (if (< (+ i 1) len)
                 (let ((next (string-ref str (+ i 1))))
                   (cond
                     ((eqv? next #\n) (loop (+ i 2) (cons #\newline acc)))
                     ((eqv? next #\t) (loop (+ i 2) (cons #\tab acc)))
                     ((eqv? next #\r) (loop (+ i 2) (cons #\return acc)))
                     ((eqv? next #\a) (loop (+ i 2) (cons (integer->char 7) acc)))
                     ((eqv? next #\b) (loop (+ i 2) (cons #\backspace acc)))
                     ((eqv? next #\f) (loop (+ i 2) (cons (integer->char 12) acc)))
                     ((eqv? next #\v) (loop (+ i 2) (cons (integer->char 11) acc)))
                     ((eqv? next #\\) (loop (+ i 2) (cons #\\ acc)))
                     ;; Octal \NNN
                     ((and (char>=? next #\0) (char<=? next #\7))
                      (let-values (((val end) (parse-octal str (+ i 1) len)))
                        (loop end (cons (integer->char val) acc))))
                     (else (loop (+ i 2) (cons next acc)))))
                 (loop (+ i 1) (cons c acc))))
              ;; Range a-z: c is the range start, str[i+2] is the range end
              ((and (< (+ i 2) len)
                    (eqv? (string-ref str (+ i 1)) #\-))
               (let* ((end-c (string-ref str (+ i 2)))
                      (range-chars (char-range c end-c)))
                 (loop (+ i 3) (append (reverse range-chars) acc))))
              (else
               (loop (+ i 1) (cons c acc)))))))))

  (def (list->char-set chars)
    (let ((v (make-vector 256 #f)))
      (for-each (lambda (c) (vector-set! v (char->integer c) #t)) chars)
      v))

  (def (tr-last lst)
    (if (null? (cdr lst)) (car lst) (tr-last (cdr lst))))

  ;; Build a translation table (vector of 256 entries)
  (def (make-translate-table chars1 chars2 complement?)
    (let ((table (make-vector 256 #f)))
      (if complement?
        ;; Complement: translate all chars NOT in set1 using set2
        (let ((set1 (list->char-set chars1))
              (last-c2 (if (null? chars2) #f (tr-last chars2))))
          (let loop ((i 0))
            (when (< i 256)
              (if (not (vector-ref set1 i))
                (vector-set! table i (or last-c2 (integer->char i)))
                (vector-set! table i (integer->char i)))
              (loop (+ i 1))))
          ;; Now map complement chars to set2 in order
          (let ((complement-chars
                 (let loop ((i 0) (acc '()))
                   (if (>= i 256) (reverse acc)
                     (if (not (vector-ref set1 i))
                       (loop (+ i 1) (cons i acc))
                       (loop (+ i 1) acc))))))
            (let loop ((cs complement-chars) (c2 chars2))
              (when (and (pair? cs) (pair? c2))
                (vector-set! table (car cs) (car c2))
                (loop (cdr cs) (cdr c2))))))
        ;; Normal: map chars1[i] -> chars2[i]
        (begin
          ;; Initialize identity
          (let loop ((i 0))
            (when (< i 256)
              (vector-set! table i (integer->char i))
              (loop (+ i 1))))
          ;; Extend chars2 with its last char if shorter
          (let ((len2 (length chars2)))
            (let loop ((c1 chars1) (idx 0))
              (when (pair? c1)
                (let ((replacement (if (< idx len2)
                                     (list-ref chars2 idx)
                                     (if (> len2 0) (tr-last chars2) (car c1)))))
                  (vector-set! table (char->integer (car c1)) replacement))
                (loop (cdr c1) (+ idx 1)))))))
      table))

  (def (tr-translate chars1 chars2 complement?)
    (let ((table (make-translate-table chars1 chars2 complement?)))
      (let loop ()
        (let ((c (read-char)))
          (unless (eof-object? c)
            (write-char (vector-ref table (char->integer c)))
            (loop))))))

  (def (tr-delete chars1 complement?)
    (let ((set1 (list->char-set chars1)))
      (let loop ()
        (let ((c (read-char)))
          (unless (eof-object? c)
            (let ((in-set? (vector-ref set1 (char->integer c))))
              (if complement?
                (when in-set? (write-char c))
                (unless in-set? (write-char c))))
            (loop))))))

  (def (tr-squeeze chars1 complement?)
    (let ((set1 (list->char-set chars1)))
      (let loop ((last-c #f))
        (let ((c (read-char)))
          (unless (eof-object? c)
            (let ((in-set? (vector-ref set1 (char->integer c))))
              (let ((squeeze? (if complement? (not in-set?) in-set?)))
                (if (and squeeze? (eqv? c last-c))
                  (loop last-c)
                  (begin (write-char c) (loop c))))))))))

  (def (tr-translate-squeeze chars1 chars2 complement?)
    (let ((table (make-translate-table chars1 chars2 complement?))
          (set2 (list->char-set chars2)))
      (let loop ((last-c #f))
        (let ((c (read-char)))
          (unless (eof-object? c)
            (let* ((translated (vector-ref table (char->integer c)))
                   (in-set2? (vector-ref set2 (char->integer translated))))
              (if (and in-set2? (eqv? translated last-c))
                (loop last-c)
                (begin (write-char translated) (loop translated)))))))))

  (def (tr-delete-squeeze chars1 chars2 complement?)
    (let ((set1 (list->char-set chars1))
          (set2 (list->char-set chars2)))
      (let loop ((last-c #f))
        (let ((c (read-char)))
          (unless (eof-object? c)
            (let ((in-set1? (vector-ref set1 (char->integer c))))
              (let ((should-delete? (if complement? (not in-set1?) in-set1?)))
                (unless should-delete?
                  (let ((in-set2? (vector-ref set2 (char->integer c))))
                    (if (and in-set2? (eqv? c last-c))
                      (loop last-c)
                      (begin (write-char c) (loop c))))))))))))

  (def (main . args)
    ;; tr doesn't use getopt - it has unique argument parsing
    (parameterize ((program-name "tr"))
      (init-security!)
      (install-readonly-seccomp!)
      ;; Pre-process: expand combined flags like -cd -> -c -d, -ds -> -d -s
      (let ((args (expand-tr-flags args)))
      (let loop ((args args) (complement? #f) (delete? #f) (squeeze? #f))
        (cond
          ((and (pair? args) (member (car args) '("-c" "-C" "--complement")))
           (loop (cdr args) #t delete? squeeze?))
          ((and (pair? args) (member (car args) '("-d" "--delete")))
           (loop (cdr args) complement? #t squeeze?))
          ((and (pair? args) (member (car args) '("-s" "--squeeze-repeats")))
           (loop (cdr args) complement? delete? #t))
          ((and (pair? args) (member (car args) '("-t" "--truncate-set1")))
           ;; -t truncate SET1 to length of SET2 (default for translate)
           (loop (cdr args) complement? delete? squeeze?))
          ((and (pair? args) (member (car args) '("--help")))
           (display "Usage: tr [OPTION]... SET1 [SET2]\n")
           (display "Translate, squeeze, and/or delete characters.\n"))
          ((and (pair? args) (member (car args) '("--version")))
           (display "tr (jerboa-coreutils) 0.1.0\n"))
          (else
           ;; Remaining args are SET1 [SET2]
           (let ((set1 (if (pair? args) (car args) #f))
                 (set2 (if (and (pair? args) (pair? (cdr args))) (cadr args) #f)))
             (unless set1
               (die "missing operand"))
             (let ((chars1 (expand-set set1))
                   (chars2 (if set2 (expand-set set2) '())))
               (cond
                 (delete?
                  (if squeeze?
                    (tr-delete-squeeze chars1 chars2 complement?)
                    (tr-delete chars1 complement?)))
                 (squeeze?
                  (if set2
                    (tr-translate-squeeze chars1 chars2 complement?)
                    (tr-squeeze chars1 complement?)))
                 (else
                  (unless set2 (die "missing operand after '~a'" set1))
                  (tr-translate chars1 chars2 complement?)))))))))))

  ) ;; end library

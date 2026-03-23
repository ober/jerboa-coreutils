#!chezscheme
;;; ptx.sls -- Produce a permuted index of file contents

(library (jerboa-coreutils ptx)
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
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Default ignore words
  (define *default-ignore*
    '("a" "an" "and" "are" "as" "at" "be" "but" "by" "do" "for" "from"
      "had" "has" "have" "he" "her" "his" "how" "i" "if" "in" "is" "it"
      "its" "may" "my" "no" "not" "of" "on" "or" "our" "own" "say" "she"
      "so" "than" "that" "the" "their" "them" "then" "there" "these" "they"
      "this" "to" "too" "us" "was" "we" "what" "when" "where" "which"
      "who" "why" "will" "with" "would" "you" "your"))

  ;; Split a string into words
  (def (split-words str)
    (let loop ((i 0) (start #f) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (if start
                    (cons (substring str start i) acc)
                    acc)))
        ((or (char-alphabetic? (string-ref str i))
             (char-numeric? (string-ref str i)))
         (if start
           (loop (+ i 1) start acc)
           (loop (+ i 1) i acc)))
        (else
         (if start
           (loop (+ i 1) #f (cons (substring str start i) acc))
           (loop (+ i 1) #f acc))))))

  (def (ignore-word? word ignore-list)
    (member (string-downcase-ptx word) ignore-list))

  (def (string-downcase-ptx str)
    (list->string (map char-downcase (string->list str))))

  (def (truncate-string str width)
    (if (<= (string-length str) width)
      str
      (substring str 0 width)))

  (def (rjust str width)
    (if (>= (string-length str) width)
      (substring str (- (string-length str) width) (string-length str))
      (string-append (make-string (- width (string-length str)) #\space) str)))

  (def (ljust str width)
    (if (>= (string-length str) width)
      (substring str 0 width)
      (string-append str (make-string (- width (string-length str)) #\space))))

  (def (ptx-iota n)
    (let loop ((i 0) (acc '()))
      (if (>= i n) (reverse acc)
        (loop (+ i 1) (cons i acc)))))

  (def (string-join lst sep)
    (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc sep (car rest)))))))

  (def (generate-entries line line-words output-width ignore-list)
    (let ((ref-width (quotient output-width 3))
          (keyword-width (quotient output-width 4))
          (context-width (- output-width (quotient output-width 3) (quotient output-width 4) 4)))
      (for-each
        (lambda (word-idx)
          (let ((word (list-ref line-words word-idx)))
            (unless (ignore-word? word ignore-list)
              (let* ((left-words (let loop ((i (- word-idx 1)) (acc '()))
                                   (if (< i 0) acc
                                     (loop (- i 1) (cons (list-ref line-words i) acc)))))
                     (right-words (let loop ((i (+ word-idx 1)) (acc '()))
                                    (if (>= i (length line-words)) (reverse acc)
                                      (loop (+ i 1) (cons (list-ref line-words i) acc)))))
                     (left-context (string-join left-words " "))
                     (right-context (string-join right-words " "))
                     (left-part (rjust (truncate-string left-context ref-width) ref-width))
                     (kw-part (ljust word keyword-width))
                     (right-part (truncate-string right-context context-width)))
                (displayln left-part "  " kw-part " " right-part)))))
        (ptx-iota (length line-words)))))

  ;; Read ignore words from file
  (def (read-ignore-file path)
    (with-catch
      (lambda (e) *default-ignore*)
      (lambda ()
        (let* ((port (open-input-file path))
               (words (let loop ((acc '()))
                        (let ((line (get-line port)))
                          (if (eof-object? line)
                            (reverse acc)
                            (loop (append acc (split-words line))))))))
          (close-port port)
          (map string-downcase-ptx words)))))

  (def (main . args)
    (parameterize ((program-name "ptx"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let* ((output-width (if (hash-get opt 'width)
                                (let ((n (string->number (hash-ref opt 'width))))
                                  (if n (inexact->exact n) 72))
                                72))
                 (ignore-list (if (hash-get opt 'ignore-file)
                               (read-ignore-file (hash-ref opt 'ignore-file))
                               '()))
                 (files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
              (for-each
                (lambda (f)
                  (let* ((port (if (string=? f "-")
                                (current-input-port)
                                (with-catch
                                  (lambda (e) (die "~a: ~a" f (error-message e)))
                                  (lambda () (open-input-file f)))))
                         (lines (let loop ((acc '()))
                                  (let ((line (get-line port)))
                                    (if (eof-object? line)
                                      (reverse acc)
                                      (loop (cons line acc)))))))
                    (unless (string=? f "-")
                      (close-port port))
                    (for-each
                      (lambda (line)
                        (let ((words (split-words line)))
                          (unless (null? words)
                            (generate-entries line words output-width ignore-list))))
                      lines)))
                files)))
        args
        'program: "ptx"
        'help: "Produce a permuted index of file contents."
        (option 'width "-w" "--width"
          'help: "output width in columns (default 72)"
          'default: #f)
        (option 'ignore-file "-i" "--ignore-file"
          'help: "read ignore word list from FILE"
          'default: #f)
        (flag 'references "-r" "--references"
          'help: "first field of each line is a reference")
        (rest-arguments 'rest))))

  ) ;; end library

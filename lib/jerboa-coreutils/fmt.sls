#!chezscheme
;;; fmt.sls -- Format text to width

(library (jerboa-coreutils fmt)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def DEFAULT_WIDTH 75)

  ;; Split a string by whitespace, returning list of words (non-empty tokens)
  (def (split-words line)
    (let loop ((i 0) (start #f) (words '()))
      (cond
        ((>= i (string-length line))
         (reverse (if start
                    (cons (substring line start i) words)
                    words)))
        ((char-whitespace? (string-ref line i))
         (loop (+ i 1) #f
               (if start (cons (substring line start i) words) words)))
        (else
         (loop (+ i 1) (or start i) words)))))

  ;; Is this line a paragraph separator (empty or whitespace-only)?
  (def (blank-line? line)
    (let loop ((i 0))
      (cond
        ((>= i (string-length line)) #t)
        ((char-whitespace? (string-ref line i)) (loop (+ i 1)))
        (else #f))))

  ;; Does line start with whitespace (indented)? (paragraph break in fmt)
  (def (indented-line? line)
    (and (> (string-length line) 0)
         (char-whitespace? (string-ref line 0))))

  ;; Does a word end a sentence? (ends with . ! ?)
  (def (sentence-end? word)
    (let ((len (string-length word)))
      (and (> len 0)
           (memv (string-ref word (- len 1)) '(#\. #\! #\?)))))

  ;; Format a list of words into lines of at most width chars
  (def (format-paragraph words width uniform)
    (if (null? words)
      '()
      (let ((goal (max 1 (inexact->exact (floor (* width 0.93))))))
        (let loop ((rest (cdr words))
                   (cur-line (car words))
                   (prev-word (car words))
                   (lines '()))
          (if (null? rest)
            (reverse (cons cur-line lines))
            (let* ((word (car rest))
                   (sep (if (and uniform (sentence-end? prev-word)) "  " " "))
                   (candidate (string-append cur-line sep word))
                   (cand-len (string-length candidate))
                   (cur-len (string-length cur-line)))
              (if (or (> cand-len width)
                      (and (> cand-len goal)
                           (>= cur-len goal)))
                (loop (cdr rest) word word (cons cur-line lines))
                (loop (cdr rest) candidate word lines))))))))

  ;; Process a sequence of paragraphs from a port
  (def (process-port port width split-only uniform)
    (let loop ((line (get-line port))
               (cur-words '())
               (in-paragraph #f))
      (if (eof-object? line)
        ;; Flush remaining paragraph
        (unless (null? cur-words)
          (let ((formatted (format-paragraph (reverse cur-words) width uniform)))
            (for-each displayln formatted)))
        (cond
          ;; Blank line: paragraph break
          ((blank-line? line)
           (unless (null? cur-words)
             (let ((formatted (format-paragraph (reverse cur-words) width uniform)))
               (for-each displayln formatted))
             (set! cur-words '()))
           (newline)
           (loop (get-line port) '() #f))
          ;; Indented line: new paragraph
          ((indented-line? line)
           (unless (null? cur-words)
             (let ((formatted (format-paragraph (reverse cur-words) width uniform)))
               (for-each displayln formatted)))
           (if split-only
             (displayln line)
             (let ((words (split-words line)))
               (loop (get-line port) words #t)))
           (when split-only
             (loop (get-line port) '() #f)))
          ;; Normal line
          (else
           (if split-only
             ;; Split only: just output line as-is (or wrap if too long)
             (begin
               (if (<= (string-length line) width)
                 (displayln line)
                 ;; Wrap long line
                 (let ((words (split-words line)))
                   (for-each displayln (format-paragraph words width uniform))))
               (loop (get-line port) '() #f))
             ;; Normal: collect words
             (let ((words (split-words line)))
               (loop (get-line port)
                     (append (reverse words) cur-words)
                     #t))))))))

  (def (main . args)
    (parameterize ((program-name "fmt"))
      (call-with-getopt
        (lambda (_ opt)
            (let* ((width (if (hash-get opt 'width)
                            (let ((n (string->number (hash-ref opt 'width))))
                              (if (and n (> n 0))
                                (inexact->exact n)
                                DEFAULT_WIDTH))
                            DEFAULT_WIDTH))
                   (split-only (hash-get opt 'split))
                   (uniform (hash-get opt 'uniform))
                   (files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
              (for-each
                (lambda (file)
                  (if (string=? file "-")
                    (process-port (current-input-port) width split-only uniform)
                    (let ((p (with-catch
                               (lambda (e) (die "~a: ~a" file (error-message e)))
                               (lambda () (open-input-file file)))))
                      (process-port p width split-only uniform)
                      (close-port p))))
                files)))
        args
        'program: "fmt"
        'help: "Reformat each paragraph in the FILE(s), writing to standard output."
        (option 'width "-w" "--width"
          'help: "maximum line width ('default: 75)"
          'default: #f)
        (flag 'split "-s" "--split-only"
          'help: "split long lines, but do not refill")
        (flag 'uniform "-u" "--uniform-spacing"
          'help: "one space between words, two after sentences")
        (rest-arguments 'rest))))

  ) ;; end library

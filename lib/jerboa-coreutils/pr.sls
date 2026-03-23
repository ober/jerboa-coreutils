#!chezscheme
;;; pr.sls -- Convert text files for printing (paginate)

(library (jerboa-coreutils pr)
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

  (def (read-all-lines port)
    (let loop ((acc '()))
      (let ((line (get-line port)))
        (if (eof-object? line)
          (reverse acc)
          (loop (cons line acc))))))

  (def (current-date-string)
    (with-catch
      (lambda (e) "")
      (lambda ()
        (let ((cmd "/bin/date '+%Y-%m-%d %H:%M' 2>/dev/null"))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (eof-object? line) "" line)))))))

  (def (fit-string str width)
    (cond
      ((>= (string-length str) width)
       (substring str 0 width))
      (else
       (string-append str (make-string (- width (string-length str)) #\space)))))

  (def (make-header header-text page-num width date-str)
    (let* ((page-str (string-append "Page " (number->string page-num)))
           (line1 "")
           (line2 (string-append date-str
                                 (make-string (max 1 (- (quotient width 2)
                                                        (string-length date-str)))
                                              #\space)
                                 header-text
                                 (make-string (max 1 (- width
                                                        (string-length date-str)
                                                        (string-length header-text)
                                                        (string-length page-str)))
                                              #\space)
                                 page-str))
           (line3 ""))
      (list line1 line2 line3)))

  (def (number-line line num sep width)
    (let ((num-str (pad-number num width)))
      (string-append num-str sep line)))

  (def (pad-number n width)
    (let ((s (number->string n)))
      (let loop ((result s))
        (if (>= (string-length result) width) result
          (loop (string-append " " result))))))

  (def (paginate-file lines page-length page-width header-text numbering
                      number-sep first-line-number double-space)
    (let* ((date-str (current-date-string))
           (header-lines 3)
           (footer-lines 5)
           (body-lines (- page-length header-lines footer-lines))
           (total-lines (length lines))
           (line-num first-line-number))
      (let page-loop ((pos 0) (page-num 1))
        (when (< pos total-lines)
          ;; Print header
          (let ((header (make-header (or header-text "") page-num page-width date-str)))
            (for-each displayln header))
          ;; Print body
          (let body-loop ((i 0) (p pos))
            (when (and (< i body-lines) (< p total-lines))
              (let* ((line (list-ref lines p))
                     (output-line (if numbering
                                    (number-line line line-num number-sep 5)
                                    line)))
                (displayln output-line)
                (when double-space
                  (displayln ""))
                (set! line-num (+ line-num 1))
                (body-loop (+ i (if double-space 2 1)) (+ p 1)))))
          ;; Pad to fill body
          (let* ((lines-used (min body-lines (- total-lines pos)))
                 (padding (- body-lines lines-used)))
            (let pad-loop ((j 0))
              (when (< j padding)
                (displayln "")
                (pad-loop (+ j 1)))))
          ;; Print footer
          (let footer-loop ((j 0))
            (when (< j footer-lines)
              (displayln "")
              (footer-loop (+ j 1))))
          ;; Next page
          (let ((lines-consumed (if double-space
                                  (quotient body-lines 2)
                                  body-lines)))
            (page-loop (+ pos lines-consumed) (+ page-num 1)))))))

  (def (main . args)
    (parameterize ((program-name "pr"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
            (let* ((page-length (if (hash-get opt 'length)
                                 (let ((n (string->number (hash-ref opt 'length))))
                                   (if n (inexact->exact n) 66))
                                 66))
                   (page-width (if (hash-get opt 'width)
                                (let ((n (string->number (hash-ref opt 'width))))
                                  (if n (inexact->exact n) 72))
                                72))
                   (header-text (hash-get opt 'header))
                   (numbering (hash-get opt 'number))
                   (number-sep (if (hash-get opt 'number-separator) (hash-ref opt 'number-separator) "\t"))
                   (first-line-number (if (hash-get opt 'first-line-number)
                                       (let ((n (string->number (hash-ref opt 'first-line-number))))
                                         (if n (inexact->exact n) 1))
                                       1))
                   (double-space (hash-get opt 'double-space))
                   (files (if (null? (hash-ref opt 'rest)) '("-") (hash-ref opt 'rest))))
              (for-each
                (lambda (f)
                  (let* ((port (if (string=? f "-")
                                (current-input-port)
                                (with-catch
                                  (lambda (e) (die "~a: ~a" f (error-message e)))
                                  (lambda () (open-input-file f)))))
                         (lines (read-all-lines port)))
                    (unless (string=? f "-")
                      (close-port port))
                    (paginate-file lines page-length page-width
                                   (or header-text f)
                                   numbering number-sep
                                   first-line-number double-space)))
                files)))
        args
        'program: "pr"
        'help: "Paginate or columnate FILE(s) for printing."
        (option 'length "-l" "--length"
          'help: "set the page length to PAGE_LENGTH lines (default 66)"
          'default: #f)
        (option 'width "-w" "--width"
          'help: "set page width to PAGE_WIDTH columns (default 72)"
          'default: #f)
        (option 'header "-h" "--header"
          'help: "use a centered HEADER instead of filename in page header"
          'default: #f)
        (flag 'number "-n" "--number-lines"
          'help: "number lines")
        (option 'number-separator "-s" "--separator"
          'help: "separate columns with STRING (default TAB)"
          'default: #f)
        (option 'first-line-number "-N" "--first-line-number"
          'help: "start counting with NUMBER at 1st line of first page"
          'default: #f)
        (flag 'double-space "-d" "--double-space"
          'help: "double-space the output")
        (rest-arguments 'rest))))

  ) ;; end library

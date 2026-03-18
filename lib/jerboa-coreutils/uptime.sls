#!chezscheme
;;; uptime.sls -- Show system uptime

(library (jerboa-coreutils uptime)
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

  ;; Read /proc/uptime and return uptime in seconds as integer
  (def (read-uptime)
    (with-catch
      (lambda (e) 0)
      (lambda ()
        (let* ((p (open-input-file "/proc/uptime"))
               (line (get-line p)))
          (close-port p)
          (if (string? line)
            (let* ((parts (string-split-space line))
                   (secs-str (if (pair? parts) (car parts) "0")))
              (inexact->exact (floor (string->number secs-str))))
            0)))))

  ;; Read /proc/loadavg and return list of 3 load average strings
  (def (read-loadavg)
    (with-catch
      (lambda (e) (list "0.00" "0.00" "0.00"))
      (lambda ()
        (let* ((p (open-input-file "/proc/loadavg"))
               (line (get-line p)))
          (close-port p)
          (if (string? line)
            (let ((parts (string-split-space line)))
              (list (if (>= (length parts) 1) (list-ref parts 0) "0.00")
                    (if (>= (length parts) 2) (list-ref parts 1) "0.00")
                    (if (>= (length parts) 3) (list-ref parts 2) "0.00")))
            (list "0.00" "0.00" "0.00"))))))

  ;; Count logged-in users via who
  (def (count-users)
    (with-catch
      (lambda (e) 0)
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        "/usr/bin/who"
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((count (let loop ((n 0))
                         (let ((line (get-line from-stdout)))
                           (if (eof-object? line)
                             (begin
                               (close-port to-stdin)
                               (close-port from-stdout)
                               (close-port from-stderr)
                               n)
                             (loop (+ n 1)))))))
            count)))))

  ;; Zero-pad a number to width
  (def (zpad n width)
    (let ((s (number->string n)))
      (let loop ((s s))
        (if (>= (string-length s) width) s
          (loop (string-append "0" s))))))

  ;; Format uptime duration as "N days, HH:MM" or "HH:MM" or "N min"
  (def (format-uptime-duration secs)
    (let* ((days (quotient secs 86400))
           (rem (remainder secs 86400))
           (hours (quotient rem 3600))
           (mins (quotient (remainder rem 3600) 60))
           (hm (string-append (if (< hours 10)
                                (string-append " " (number->string hours))
                                (number->string hours))
                              ":" (zpad mins 2))))
      (cond
        ((>= days 1)
         (string-append (if (= days 1) "1 day, " (string-append (number->string days) " days, "))
                        hm))
        ((>= hours 1) hm)
        (else
         (string-append (number->string mins) " min")))))

  ;; Get current local time as HH:MM:SS string
  (def (current-time-str)
    (with-catch
      (lambda (e) "00:00:00")
      (lambda ()
        (let-values (((to-stdin from-stdout from-stderr pid)
                      (open-process-ports
                        "/bin/date +%H:%M:%S"
                        (buffer-mode block)
                        (native-transcoder))))
          (let ((line (get-line from-stdout)))
            (close-port to-stdin)
            (close-port from-stdout)
            (close-port from-stderr)
            (if (string? line) line "00:00:00"))))))

  (def (string-split-space str)
    (let loop ((i 0) (start #f) (parts '()))
      (cond
        ((>= i (string-length str))
         (reverse (if start
                    (cons (substring str start i) parts)
                    parts)))
        ((char-whitespace? (string-ref str i))
         (loop (+ i 1) #f
               (if start (cons (substring str start i) parts) parts)))
        (else
         (loop (+ i 1) (or start i) parts)))))

  (def (main . args)
    (parameterize ((program-name "uptime"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((uptime-secs (read-uptime))
                 (loadavg (read-loadavg))
                 (user-count (count-users))
                 (time-str (current-time-str))
                 (up-str (format-uptime-duration uptime-secs))
                 (user-str (if (= user-count 1) "1 user" (format "~a users" user-count)))
                 (load1 (list-ref loadavg 0))
                 (load5 (list-ref loadavg 1))
                 (load15 (list-ref loadavg 2)))
            (if (hash-get opt 'pretty)
              ;; -p: pretty format
              (displayln (string-append "up " up-str))
              ;; Default format
              (displayln (format " ~a up ~a,  ~a,  load average: ~a, ~a, ~a"
                                 time-str up-str user-str load1 load5 load15)))))
        args
        'program: "uptime"
        'help: "Tell how long the system has been running."
        (flag 'pretty "-p" "--pretty"
          'help: "show uptime in pretty format")
        (flag 'since "-s" "--since"
          'help: "system up since, in yyyy-mm-dd HH:MM:SS format"))))

  ) ;; end library

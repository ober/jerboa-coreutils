#!chezscheme
;;; top.sls -- Display Linux processes (like procps top)
;;;
;;; Reads /proc for process and system information.
;;; Supports batch mode (-b), iteration count (-n), delay (-d),
;;; specific PIDs (-p), sort field (-o), and user filter (-u).
;;;
;;; Interactive commands (when not in batch mode):
;;;   q       - quit
;;;   h / ?   - help screen
;;;   k       - kill a process (prompts for PID and signal)
;;;   r       - renice a process (prompts for PID and nice value)
;;;   d / s   - change delay interval
;;;   n / #   - change max number of displayed processes
;;;   M       - sort by memory usage
;;;   P       - sort by CPU usage
;;;   T       - sort by time
;;;   N       - sort by PID
;;;   R       - reverse sort order
;;;   u / U   - filter by user (empty clears filter)
;;;   c       - toggle full command line vs program name
;;;   i       - toggle idle processes (show only active)
;;;   l       - toggle uptime/load line
;;;   t       - toggle task/CPU summary lines
;;;   m       - toggle memory/swap lines
;;;   x       - toggle sort-field column highlight
;;;   z       - toggle color mode
;;;   B       - toggle bold
;;;   < / >   - move sort field left/right
;;;   Space   - immediate refresh
;;;   W       - write current settings summary
;;;
;;; Security: Uses Landlock to restrict filesystem to /proc, /sys, /dev, /etc.
;;; Uses seccomp to allow only read/openat/fstat/close syscalls.

(library (jerboa-coreutils top)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (except (std misc terminal)
            terminal-width terminal-height
            with-raw-mode with-alternate-screen)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; ========== FFI for kill/renice/terminal ==========

  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-kill (foreign-procedure "kill" (int int) int))
  (define ffi-setpriority (foreign-procedure "setpriority" (int int int) int))
  (define ffi-terminal-width (foreign-procedure "coreutils_terminal_width" (int) int))
  (define ffi-terminal-height (foreign-procedure "coreutils_terminal_height" (int) int))
  (define ffi-raw-mode-enter (foreign-procedure "coreutils_raw_mode_enter" (int) int))
  (define ffi-raw-mode-exit (foreign-procedure "coreutils_raw_mode_exit" (int) int))

  ;; ========== Terminal helpers (FFI-based, no fork/exec) ==========

  (define (terminal-width) (ffi-terminal-width 1))
  (define (terminal-height) (ffi-terminal-height 1))

  (define esc "\x1b;")

  (define (with-raw-mode thunk)
    (dynamic-wind
      (lambda () (ffi-raw-mode-enter 0))
      thunk
      (lambda () (ffi-raw-mode-exit 0))))

  (define (with-alternate-screen thunk)
    (dynamic-wind
      (lambda ()
        (display (string-append esc "[?1049h"))
        (flush-output-port (current-output-port)))
      thunk
      (lambda ()
        (display (string-append esc "[?1049l"))
        (flush-output-port (current-output-port)))))

  ;; ========== /proc Readers ==========

  ;; Read entire contents of a file, return string or #f on error
  (def (read-file-contents path)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let* ((p (open-input-file path))
               (data (get-string-all p)))
          (close-port p)
          (if (eof-object? data) "" data)))))

  ;; Split string on whitespace into list of strings
  (def (string-split-whitespace str)
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

  ;; Parse a number from string, return 0 on failure
  (def (safe-string->number s)
    (or (string->number s) 0))

  ;; ========== System Info ==========

  ;; Read /proc/uptime -> (uptime-secs idle-secs)
  (def (read-uptime)
    (let ((data (read-file-contents "/proc/uptime")))
      (if data
        (let ((parts (string-split-whitespace data)))
          (values (safe-string->number (if (pair? parts) (car parts) "0"))
                  (safe-string->number (if (>= (length parts) 2) (cadr parts) "0"))))
        (values 0 0))))

  ;; Read /proc/loadavg -> (load1 load5 load15 running total)
  (def (read-loadavg)
    (let ((data (read-file-contents "/proc/loadavg")))
      (if data
        (let* ((parts (string-split-whitespace data))
               (load1 (if (>= (length parts) 1) (list-ref parts 0) "0.00"))
               (load5 (if (>= (length parts) 2) (list-ref parts 1) "0.00"))
               (load15 (if (>= (length parts) 3) (list-ref parts 2) "0.00"))
               ;; 4th field is "running/total"
               (procs (if (>= (length parts) 4) (list-ref parts 3) "0/0"))
               (slash-pos (let lp ((i 0))
                            (cond ((>= i (string-length procs)) #f)
                                  ((char=? (string-ref procs i) #\/) i)
                                  (else (lp (+ i 1))))))
               (running (if slash-pos
                          (safe-string->number (substring procs 0 slash-pos))
                          0))
               (total (if slash-pos
                       (safe-string->number
                         (substring procs (+ slash-pos 1) (string-length procs)))
                       0)))
          (values load1 load5 load15 running total))
        (values "0.00" "0.00" "0.00" 0 0))))

  ;; Read /proc/meminfo -> alist of (key . value-in-kB)
  (def (read-meminfo)
    (let ((data (read-file-contents "/proc/meminfo")))
      (if data
        (let ((lines (string-split-lines data)))
          (let loop ((lines lines) (acc '()))
            (if (null? lines) acc
              (let* ((line (car lines))
                     (colon-pos (let lp ((i 0))
                                  (cond ((>= i (string-length line)) #f)
                                        ((char=? (string-ref line i) #\:) i)
                                        (else (lp (+ i 1)))))))
                (if colon-pos
                  (let* ((key (substring line 0 colon-pos))
                         (rest (string-split-whitespace
                                 (substring line (+ colon-pos 1) (string-length line))))
                         (val (if (pair? rest) (safe-string->number (car rest)) 0)))
                    (loop (cdr lines) (cons (cons key val) acc)))
                  (loop (cdr lines) acc))))))
        '())))

  (def (meminfo-get info key default)
    (let ((pair (assoc key info)))
      (if pair (cdr pair) default)))

  ;; Read /proc/stat for CPU times
  ;; Returns vector: #(user nice system idle iowait irq softirq steal)
  (def (read-cpu-stat)
    (let ((data (read-file-contents "/proc/stat")))
      (if data
        (let* ((lines (string-split-lines data))
               (cpu-line (if (pair? lines) (car lines) ""))
               (parts (string-split-whitespace cpu-line)))
          (if (and (pair? parts) (string=? (car parts) "cpu"))
            (let ((nums (map safe-string->number (cdr parts))))
              (let ((v (make-vector 8 0)))
                (let fill ((i 0) (ns nums))
                  (when (and (< i 8) (pair? ns))
                    (vector-set! v i (car ns))
                    (fill (+ i 1) (cdr ns))))
                v))
            (make-vector 8 0)))
        (make-vector 8 0))))

  ;; ========== Process Info ==========

  ;; Read /proc/[pid]/stat
  (def (read-proc-stat pid)
    (let ((data (read-file-contents (string-append "/proc/" (number->string pid) "/stat"))))
      (if (not data) #f
        ;; The comm field is in parens and can contain spaces, so find the last )
        (let* ((len (string-length data))
               (rparen (let lp ((i (- len 1)))
                         (cond ((< i 0) #f)
                               ((char=? (string-ref data i) #\)) i)
                               (else (lp (- i 1)))))))
          (if (not rparen) #f
            (let* ((lparen (let lp ((i 0))
                             (cond ((>= i len) #f)
                                   ((char=? (string-ref data i) #\() i)
                                   (else (lp (+ i 1))))))
                   (comm (if (and lparen rparen (< lparen rparen))
                           (substring data (+ lparen 1) rparen)
                           "?"))
                   ;; Fields after the closing paren (index 2 onwards in /proc/pid/stat)
                   (rest-str (if (< (+ rparen 2) len)
                               (substring data (+ rparen 2) len)
                               ""))
                   (fields (string-split-whitespace rest-str)))
              ;; fields[0]=state(2), fields[1]=ppid(3), ..., fields[11]=utime(13), fields[12]=stime(14)
              ;; fields[15]=priority(17), fields[16]=nice(18), fields[17]=num_threads(19)
              ;; fields[19]=starttime(21), fields[20]=vsize(22), fields[21]=rss(23)
              (let ((state (if (>= (length fields) 1) (list-ref fields 0) "?"))
                    (utime (if (>= (length fields) 12) (safe-string->number (list-ref fields 11)) 0))
                    (stime (if (>= (length fields) 13) (safe-string->number (list-ref fields 12)) 0))
                    (priority (if (>= (length fields) 16) (safe-string->number (list-ref fields 15)) 0))
                    (nice (if (>= (length fields) 17) (safe-string->number (list-ref fields 16)) 0))
                    (num-threads (if (>= (length fields) 18) (safe-string->number (list-ref fields 17)) 1))
                    (starttime (if (>= (length fields) 20) (safe-string->number (list-ref fields 19)) 0))
                    (vsize (if (>= (length fields) 21) (safe-string->number (list-ref fields 20)) 0))
                    (rss (if (>= (length fields) 22) (safe-string->number (list-ref fields 21)) 0)))
                (list pid comm state utime stime priority nice num-threads starttime vsize rss))))))))

  ;; Read /proc/[pid]/status for Uid
  (def (read-proc-uid pid)
    (let ((data (read-file-contents (string-append "/proc/" (number->string pid) "/status"))))
      (if (not data) 0
        (let ((lines (string-split-lines data)))
          (let loop ((lines lines))
            (if (null? lines) 0
              (let ((line (car lines)))
                (if (and (>= (string-length line) 4)
                         (string=? (substring line 0 4) "Uid:"))
                  (let ((parts (string-split-whitespace
                                 (substring line 4 (string-length line)))))
                    ;; First field is real UID
                    (if (pair? parts) (safe-string->number (car parts)) 0))
                  (loop (cdr lines))))))))))

  ;; Read /proc/[pid]/cmdline
  (def (read-proc-cmdline pid)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let* ((path (string-append "/proc/" (number->string pid) "/cmdline"))
               (p (open-file-input-port path))
               (data (get-bytevector-all p)))
          (close-port p)
          (if (or (eof-object? data) (= (bytevector-length data) 0))
            #f
            ;; Replace NUL bytes with spaces
            (let ((len (bytevector-length data)))
              (let loop ((i 0) (chars '()))
                (if (>= i len)
                  (list->string (reverse chars))
                  (let ((b (bytevector-u8-ref data i)))
                    (loop (+ i 1)
                          (cons (if (= b 0) #\space (integer->char b))
                                chars)))))))))))

  ;; Get username for UID (read /etc/passwd)
  (def *uid-cache* (make-hashtable equal-hash equal?))

  (def (uid->username uid)
    (or (hashtable-ref *uid-cache* uid #f)
        (let ((name (lookup-uid uid)))
          (hashtable-set! *uid-cache* uid name)
          name)))

  (def (lookup-uid uid)
    (let ((data (read-file-contents "/etc/passwd")))
      (if (not data) (number->string uid)
        (let ((lines (string-split-lines data)))
          (let loop ((lines lines))
            (if (null? lines)
              (number->string uid)
              (let* ((line (car lines))
                     (fields (string-split-colon line)))
                (if (and (>= (length fields) 3)
                         (= (safe-string->number (list-ref fields 2)) uid))
                  (car fields)
                  (loop (cdr lines))))))))))

  (def (string-split-colon str)
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (cons (substring str start i) acc)))
        ((char=? (string-ref str i) #\:)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) acc)))
        (else (loop (+ i 1) start acc)))))

  ;; List all numeric PIDs in /proc
  (def (list-pids)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let ((entries (directory-list "/proc")))
          (let loop ((es entries) (acc '()))
            (if (null? es) (reverse acc)
              (let ((n (string->number (car es))))
                (if (and n (> n 0))
                  (loop (cdr es) (cons n acc))
                  (loop (cdr es) acc)))))))))

  ;; ========== Formatting ==========

  (def (string-split-lines str)
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (if (> i start)
                    (cons (substring str start i) acc)
                    acc)))
        ((char=? (string-ref str i) #\newline)
         (loop (+ i 1) (+ i 1)
               (if (> i start)
                 (cons (substring str start i) acc)
                 acc)))
        (else (loop (+ i 1) start acc)))))

  ;; Format bytes as human-readable (KiB)
  (def (format-kb kb)
    (cond
      ((>= kb 1048576) (string-append (number->string (quotient kb 1048576)) "g"))
      ((>= kb 1024)    (string-append (number->string (quotient kb 1024)) "m"))
      (else            (string-append (number->string kb) ""))))

  ;; Format CPU time in jiffies to MM:SS.hh
  (def (format-time jiffies)
    (let* ((secs (quotient jiffies 100))
           (hundredths (remainder jiffies 100))
           (mins (quotient secs 60))
           (s (remainder secs 60)))
      (string-append
        (number->string mins) ":"
        (if (< s 10) (string-append "0" (number->string s)) (number->string s))
        "." (if (< hundredths 10)
               (string-append "0" (number->string hundredths))
               (number->string hundredths)))))

  ;; Pad/truncate string to width
  (def (pad-right str width)
    (let ((len (string-length str)))
      (cond
        ((= len width) str)
        ((> len width) (substring str 0 width))
        (else (string-append str (make-string (- width len) #\space))))))

  (def (pad-left str width)
    (let ((len (string-length str)))
      (cond
        ((= len width) str)
        ((> len width) (substring str (- len width) len))
        (else (string-append (make-string (- width len) #\space) str)))))

  ;; ========== CPU % Calculation ==========

  ;; Calculate CPU percentages from two snapshots
  (def (calc-cpu-pct prev curr)
    ;; Each vector: #(user nice system idle iowait irq softirq steal)
    (let* ((delta (lambda (i) (max 0 (- (vector-ref curr i) (vector-ref prev i)))))
           (d-user (delta 0))
           (d-nice (delta 1))
           (d-sys  (delta 2))
           (d-idle (delta 3))
           (d-iow  (delta 4))
           (d-irq  (delta 5))
           (d-sirq (delta 6))
           (d-steal (delta 7))
           (total (+ d-user d-nice d-sys d-idle d-iow d-irq d-sirq d-steal)))
      (if (= total 0)
        (values 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0)
        (let ((pct (lambda (v) (* 100.0 (/ v total)))))
          (values (pct d-user) (pct d-sys) (pct d-nice) (pct d-idle)
                  (pct d-iow) (pct d-irq) (pct d-sirq) (pct d-steal))))))

  ;; ========== Build Process Table ==========

  (def (get-clk-tck) 100)

  (def (build-process-list pids prev-proc-times uptime-secs delay-secs)
    ;; Returns list of process records and updated proc-times table
    (let ((clk-tck (get-clk-tck))
          (page-size 4096)
          (new-proc-times (make-hashtable equal-hash equal?)))
      (let loop ((pids pids) (procs '()))
        (if (null? pids)
          (values (reverse procs) new-proc-times)
          (let ((pid (car pids)))
            (let ((stat (read-proc-stat pid)))
              (if (not stat)
                (loop (cdr pids) procs)
                (let* ((uid (read-proc-uid pid))
                       (cmdline (read-proc-cmdline pid))
                       ;; stat: (pid comm state utime stime priority nice nthreads starttime vsize rss)
                       (comm (list-ref stat 1))
                       (state (list-ref stat 2))
                       (utime (list-ref stat 3))
                       (stime (list-ref stat 4))
                       (pri (list-ref stat 5))
                       (ni (list-ref stat 6))
                       (vsize-bytes (list-ref stat 9))
                       (rss-pages (list-ref stat 10))
                       (total-time (+ utime stime))
                       ;; CPU% from delta since last sample
                       (prev-time (hashtable-ref prev-proc-times pid #f))
                       (delta-time (if prev-time (max 0 (- total-time prev-time)) 0))
                       ;; delta jiffies / (clk_tck * delay)
                       (effective-delay (max 0.1 delay-secs))
                       (cpu-pct (if (and prev-time (> uptime-secs 0))
                                  (min 999.9 (* 100.0 (/ delta-time (* clk-tck effective-delay))))
                                  0.0))
                       ;; Memory
                       (virt-kb (quotient vsize-bytes 1024))
                       (res-kb (* rss-pages (quotient page-size 1024)))
                       (username (uid->username uid))
                       (command (or cmdline comm)))
                  (hashtable-set! new-proc-times pid total-time)
                  (loop (cdr pids)
                        (cons (vector pid username pri ni virt-kb res-kb
                                      0 ;; SHR placeholder
                                      state cpu-pct 0.0 ;; %MEM computed later
                                      total-time command)
                              procs))))))))))

  ;; ========== Sort Fields ==========

  ;; Ordered list of sort fields for < / > navigation
  (def *sort-fields* '("pid" "user" "%cpu" "%mem" "virt" "res" "time"))

  (def (next-sort-field field)
    (let loop ((fs *sort-fields*))
      (cond
        ((null? fs) (car *sort-fields*))
        ((null? (cdr fs)) (car *sort-fields*))
        ((string=? (car fs) field) (cadr fs))
        (else (loop (cdr fs))))))

  (def (prev-sort-field field)
    (let loop ((fs *sort-fields*) (prev (last-element *sort-fields*)))
      (cond
        ((null? fs) prev)
        ((string=? (car fs) field) prev)
        (else (loop (cdr fs) (car fs))))))

  (def (last-element lst)
    (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

  ;; Sort field to column index for highlighting
  (def (sort-field->col-index field)
    (case (string->symbol field)
      ((pid)  0)
      ((user) 1)
      ((%cpu cpu) 8)
      ((%mem mem res) 5)
      ((virt) 4)
      ((time time+) 10)
      (else 8)))

  ;; Sort processes by field
  (def (sort-procs procs field reverse?)
    (let* ((cmp (case (string->symbol field)
                  ((pid)  (lambda (a b) (> (vector-ref a 0) (vector-ref b 0))))
                  ((user) (lambda (a b) (string<? (vector-ref a 1) (vector-ref b 1))))
                  ((cpu %cpu)  (lambda (a b) (> (vector-ref a 8) (vector-ref b 8))))
                  ((mem %mem res)  (lambda (a b) (> (vector-ref a 5) (vector-ref b 5))))
                  ((virt) (lambda (a b) (> (vector-ref a 4) (vector-ref b 4))))
                  ((time time+) (lambda (a b) (> (vector-ref a 10) (vector-ref b 10))))
                  (else   (lambda (a b) (> (vector-ref a 8) (vector-ref b 8))))))
           (final-cmp (if reverse?
                        (lambda (a b) (cmp b a))
                        cmp)))
      (list-sort final-cmp procs)))

  ;; Count process states
  (def (count-states procs)
    (let ((running 0) (sleeping 0) (stopped 0) (zombie 0))
      (for-each
        (lambda (p)
          (let ((s (vector-ref p 7)))
            (cond
              ((string=? s "R") (set! running (+ running 1)))
              ((or (string=? s "S") (string=? s "I") (string=? s "D"))
               (set! sleeping (+ sleeping 1)))
              ((string=? s "T") (set! stopped (+ stopped 1)))
              ((string=? s "Z") (set! zombie (+ zombie 1)))
              (else (set! sleeping (+ sleeping 1))))))
        procs)
      (values running sleeping stopped zombie)))

  ;; ========== Display ==========

  ;; Format uptime duration
  (def (format-uptime-str secs)
    (let* ((s (inexact->exact (floor secs)))
           (days (quotient s 86400))
           (rem (remainder s 86400))
           (hours (quotient rem 3600))
           (mins (quotient (remainder rem 3600) 60)))
      (cond
        ((>= days 1)
         (string-append (number->string days) " day"
                        (if (= days 1) ", " "s, ")
                        (number->string hours) ":"
                        (if (< mins 10)
                          (string-append "0" (number->string mins))
                          (number->string mins))))
        (else
         (string-append (number->string hours) ":"
                        (if (< mins 10)
                          (string-append "0" (number->string mins))
                          (number->string mins)))))))

  ;; Format a float with 1 decimal place
  (def (format-float1 x)
    (let* ((rounded (inexact->exact (round (* x 10))))
           (whole (quotient rounded 10))
           (frac (abs (remainder rounded 10))))
      (string-append (number->string whole) "." (number->string frac))))

  ;; Get current time as HH:MM:SS
  (def (current-time-str)
    (with-catch
      (lambda (e) "??:??:??")
      (lambda ()
        (let* ((t (current-time 'time-utc))
               (d (time-utc->date t (local-time-zone-offset))))
          (format "~2,'0d:~2,'0d:~2,'0d"
                  (date-hour d) (date-minute d) (date-second d))))))

  ;; Get local timezone offset in seconds
  (def (local-time-zone-offset)
    (with-catch
      (lambda (e) 0)
      (lambda ()
        (let ((tz (getenv "TZ")))
          (if tz 0 0)))))

  ;; Display the header lines (respecting show-* toggles)
  (def (display-header uptime-secs load1 load5 load15
                        num-tasks running sleeping stopped zombie
                        cpu-user cpu-sys cpu-nice cpu-idle cpu-iow
                        cpu-hi cpu-si cpu-st
                        mem-total mem-free mem-used mem-bufcache
                        swap-total swap-free swap-used swap-avail
                        show-uptime? show-tasks? show-mem?
                        use-color? use-bold? sort-field highlight-sort?)
    ;; Line 1: top - HH:MM:SS up X days, H:MM, load average: ...
    (when show-uptime?
      (let ((line (format "top - ~a up ~a,  load average: ~a, ~a, ~a"
                          (current-time-str) (format-uptime-str uptime-secs)
                          load1 load5 load15)))
        (if (and use-color? use-bold?)
          (display (bold line))
          (display line))
        (newline)))
    ;; Line 2: Tasks
    (when show-tasks?
      (display (format "Tasks: ~a total, ~a running, ~a sleeping, ~a stopped, ~a zombie\n"
                       num-tasks running sleeping stopped zombie))
      ;; Line 3: %Cpu(s)
      (display (format "%Cpu(s): ~a us, ~a sy, ~a ni, ~a id, ~a wa, ~a hi, ~a si, ~a st\n"
                       (format-float1 cpu-user) (format-float1 cpu-sys)
                       (format-float1 cpu-nice) (format-float1 cpu-idle)
                       (format-float1 cpu-iow) (format-float1 cpu-hi)
                       (format-float1 cpu-si) (format-float1 cpu-st))))
    ;; Line 4: MiB Mem
    (when show-mem?
      (display (format "MiB Mem : ~a total, ~a free, ~a used, ~a buff/cache\n"
                       (format-float1 (/ mem-total 1024.0))
                       (format-float1 (/ mem-free 1024.0))
                       (format-float1 (/ mem-used 1024.0))
                       (format-float1 (/ mem-bufcache 1024.0))))
      ;; Line 5: MiB Swap
      (display (format "MiB Swap: ~a total, ~a free, ~a used. ~a avail Mem\n"
                       (format-float1 (/ swap-total 1024.0))
                       (format-float1 (/ swap-free 1024.0))
                       (format-float1 (/ swap-used 1024.0))
                       (format-float1 (/ swap-avail 1024.0)))))
    ;; Blank line + column header
    (newline)
    (let ((hdr (format "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a"
                       (pad-left "PID" 7) (pad-right "USER" 9)
                       (pad-left "PR" 3) (pad-left "NI" 4)
                       (pad-left "VIRT" 7) (pad-left "RES" 7)
                       (pad-left "SHR" 6) (pad-right "S" 2)
                       (pad-left "%CPU" 5) (pad-left "%MEM" 5)
                       (pad-left "TIME+" 10) "COMMAND")))
      (if (and use-color? use-bold?)
        (display (bold hdr))
        (display hdr))
      (newline)))

  ;; Display one process line
  (def (display-proc proc mem-total-kb use-color? highlight-sort? sort-field show-full-cmd? term-width)
    (let* ((pid (vector-ref proc 0))
           (user (vector-ref proc 1))
           (pri (vector-ref proc 2))
           (ni (vector-ref proc 3))
           (virt (vector-ref proc 4))
           (res (vector-ref proc 5))
           (shr (vector-ref proc 6))
           (state (vector-ref proc 7))
           (cpu-pct (vector-ref proc 8))
           (mem-pct (if (> mem-total-kb 0)
                      (* 100.0 (/ res mem-total-kb))
                      0.0))
           (total-time (vector-ref proc 10))
           (command (vector-ref proc 11))
           (max-cmd (max 1 (- term-width 72)))
           (cmd-display (if (and (not show-full-cmd?) (> (string-length command) max-cmd))
                          (substring command 0 max-cmd)
                          (if (> (string-length command) (max 1 (- term-width 72)))
                            (substring command 0 (max 1 (- term-width 72)))
                            command)))
           (line (format "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a"
                         (pad-left (number->string pid) 7)
                         (pad-right (if (> (string-length user) 8)
                                      (substring user 0 8)
                                      user) 9)
                         (pad-left (number->string pri) 3)
                         (pad-left (number->string ni) 4)
                         (pad-left (format-kb virt) 7)
                         (pad-left (format-kb res) 7)
                         (pad-left "0" 6)
                         (pad-right state 2)
                         (pad-left (format-float1 cpu-pct) 5)
                         (pad-left (format-float1 mem-pct) 5)
                         (pad-left (format-time total-time) 10)
                         cmd-display)))
      ;; Color running processes
      (cond
        ((and use-color? (string=? state "R"))
         (display (fg-color 'green line)))
        ((and use-color? (string=? state "Z"))
         (display (fg-color 'red line)))
        (else (display line)))
      (newline)))

  ;; ========== Interactive Input ==========

  ;; Open /dev/tty for reading keypresses (separate from stdin)
  (def (open-tty-input)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (open-input-file "/dev/tty"))))

  ;; Read a line from the tty port char-by-char (for prompts in raw mode)
  ;; Echoes characters, handles backspace, returns on Enter or Escape
  (def (read-prompt-line tty-port)
    (let loop ((chars '()))
      (let ((c (read-char tty-port)))
        (cond
          ((eof-object? c) (list->string (reverse chars)))
          ;; Enter (CR in raw mode)
          ((or (char=? c #\return) (char=? c #\newline))
           (newline)
           (flush-output-port (current-output-port))
           (list->string (reverse chars)))
          ;; Escape — cancel
          ((char=? c #\x1b)
           ;; Drain any remaining escape sequence chars
           (when (char-ready? tty-port) (read-char tty-port))
           (when (char-ready? tty-port) (read-char tty-port))
           #f)
          ;; Backspace / DEL
          ((or (char=? c #\backspace) (char=? c #\x7f))
           (if (pair? chars)
             (begin
               (display "\b \b")
               (flush-output-port (current-output-port))
               (loop (cdr chars)))
             (loop chars)))
          ;; Regular character
          (else
           (display c)
           (flush-output-port (current-output-port))
           (loop (cons c chars)))))))

  ;; Show a prompt at the bottom, read input, return string or #f
  (def (prompt-input tty-port msg)
    (display msg)
    (flush-output-port (current-output-port))
    (read-prompt-line tty-port))

  ;; Wait for any key
  (def (wait-for-key tty-port)
    (read-char tty-port))

  ;; ========== Help Screen ==========

  (def (display-help)
    (cursor-position 1 1)
    (clear-screen)
    (display (bold "Help for Interactive Commands — press any key to return\n"))
    (newline)
    (display "  Window manipulation\n")
    (display "    q         Quit\n")
    (display "    Space     Refresh display immediately\n")
    (display "    d or s    Change delay interval\n")
    (display "    n or #    Set maximum number of displayed tasks\n")
    (newline)
    (display "  Sorting\n")
    (display "    P         Sort by CPU usage\n")
    (display "    M         Sort by memory usage\n")
    (display "    T         Sort by cumulative time\n")
    (display "    N         Sort by PID\n")
    (display "    R         Reverse current sort order\n")
    (display "    <  or  >  Move sort field left/right\n")
    (newline)
    (display "  Filtering\n")
    (display "    u or U    Filter by user (empty clears)\n")
    (display "    i         Toggle idle process display\n")
    (newline)
    (display "  Display toggles\n")
    (display "    c         Toggle full command line\n")
    (display "    l         Toggle uptime/load average line\n")
    (display "    t         Toggle task/CPU summary lines\n")
    (display "    m         Toggle memory/swap lines\n")
    (display "    x         Toggle sort-field column highlight\n")
    (display "    z         Toggle color output\n")
    (display "    B         Toggle bold\n")
    (newline)
    (display "  Process control\n")
    (display "    k         Kill a process (signal)\n")
    (display "    r         Renice a process\n")
    (newline)
    (display "  Press any key to return...")
    (flush-output-port (current-output-port)))

  ;; ========== Main Loop ==========

  (def (run-top batch? delay-secs iterations sort-field filter-pids filter-user)
    ;; Initialize security
    (init-security!)

    (if batch?
      ;; Batch mode — no interactivity
      (run-top-batch delay-secs iterations sort-field filter-pids filter-user)
      ;; Interactive mode — keyboard handling
      (run-top-interactive delay-secs iterations sort-field filter-pids filter-user)))

  ;; Batch mode: simple loop with sleep
  (def (run-top-batch delay-secs iterations sort-field filter-pids filter-user)
    (let ((prev-cpu (read-cpu-stat))
          (prev-proc-times (make-hashtable equal-hash equal?)))
      (let iter-loop ((n 0))
        (when (or (not iterations) (< n iterations))
          (let-values (((uptime-secs idle-secs) (read-uptime)))
            (let-values (((load1 load5 load15 sched-running sched-total) (read-loadavg)))
              (let* ((curr-cpu (read-cpu-stat))
                     (meminfo (read-meminfo))
                     (mem-total (meminfo-get meminfo "MemTotal" 0))
                     (mem-free (meminfo-get meminfo "MemFree" 0))
                     (buffers (meminfo-get meminfo "Buffers" 0))
                     (cached (meminfo-get meminfo "Cached" 0))
                     (slab-recl (meminfo-get meminfo "SReclaimable" 0))
                     (mem-bufcache (+ buffers cached slab-recl))
                     (mem-used (- mem-total mem-free mem-bufcache))
                     (swap-total (meminfo-get meminfo "SwapTotal" 0))
                     (swap-free (meminfo-get meminfo "SwapFree" 0))
                     (swap-used (- swap-total swap-free))
                     (swap-avail (meminfo-get meminfo "MemAvailable" mem-free))
                     (all-pids (if (pair? filter-pids) filter-pids (list-pids))))
                (let-values (((procs new-proc-times)
                              (build-process-list all-pids prev-proc-times uptime-secs delay-secs)))
                  (let* ((filtered (if filter-user
                                     (filter (lambda (p)
                                               (string=? (vector-ref p 1) filter-user))
                                             procs)
                                     procs))
                         (sorted (sort-procs filtered sort-field #f)))
                    (let-values (((cpu-us cpu-sy cpu-ni cpu-id cpu-wa cpu-hi cpu-si cpu-st)
                                  (calc-cpu-pct prev-cpu curr-cpu)))
                      (let-values (((st-run st-sleep st-stop st-zombie)
                                    (count-states filtered)))
                        (display-header uptime-secs load1 load5 load15
                                        (length filtered) st-run st-sleep st-stop st-zombie
                                        cpu-us cpu-sy cpu-ni cpu-id cpu-wa cpu-hi cpu-si cpu-st
                                        mem-total mem-free mem-used mem-bufcache
                                        swap-total swap-free swap-used swap-avail
                                        #t #t #t #f #f sort-field #f)
                        (for-each (lambda (p) (display-proc p mem-total #f #f sort-field #f 200))
                                  sorted)
                        (newline)
                        (flush-output-port (current-output-port))
                        (set! prev-cpu curr-cpu)
                        (set! prev-proc-times new-proc-times)
                        (when (or (not iterations) (< (+ n 1) iterations))
                          (sleep (make-time 'time-duration 0
                                            (inexact->exact (floor delay-secs)))))
                        (iter-loop (+ n 1)))))))))))))

  ;; Interactive mode: raw mode + alternate screen + keypress handling
  (def (run-top-interactive init-delay init-iterations init-sort-field filter-pids init-filter-user)
    (let ((tty-in (open-tty-input)))
      (if (not tty-in)
        ;; Can't open /dev/tty — fall back to batch mode
        (run-top-batch init-delay init-iterations init-sort-field filter-pids init-filter-user)
        ;; Interactive mode
        (with-catch
          (lambda (e)
            (close-port tty-in)
            (cursor-show)
            (reset-style)
            (raise e))
          (lambda ()
            (with-raw-mode
              (lambda ()
                (with-alternate-screen
                  (lambda ()
                    (cursor-hide)
                    ;; Mutable state for interactive settings
                    (let ((delay-secs init-delay)
                          (max-lines 0)  ;; 0 = auto (fill screen)
                          (sort-field init-sort-field)
                          (reverse-sort? #f)
                          (filter-user init-filter-user)
                          (show-idle? #t)
                          (show-full-cmd? #f)
                          (show-uptime? #t)
                          (show-tasks? #t)
                          (show-mem? #t)
                          (use-color? #t)
                          (use-bold? #t)
                          (highlight-sort? #f)
                          (prev-cpu (read-cpu-stat))
                          (prev-proc-times (make-hashtable equal-hash equal?))
                          (quit? #f)
                          (iterations init-iterations))

                      (let iter-loop ((n 0))
                        (when (and (not quit?)
                                   (or (not iterations) (< n iterations)))

                          ;; Gather and display data
                          (let ((term-h (terminal-height))
                                (term-w (terminal-width)))
                            (let-values (((uptime-secs idle-secs) (read-uptime)))
                              (let-values (((load1 load5 load15 sched-running sched-total) (read-loadavg)))
                                (let* ((curr-cpu (read-cpu-stat))
                                       (meminfo (read-meminfo))
                                       (mem-total (meminfo-get meminfo "MemTotal" 0))
                                       (mem-free (meminfo-get meminfo "MemFree" 0))
                                       (buffers (meminfo-get meminfo "Buffers" 0))
                                       (cached (meminfo-get meminfo "Cached" 0))
                                       (slab-recl (meminfo-get meminfo "SReclaimable" 0))
                                       (mem-bufcache (+ buffers cached slab-recl))
                                       (mem-used (- mem-total mem-free mem-bufcache))
                                       (swap-total (meminfo-get meminfo "SwapTotal" 0))
                                       (swap-free (meminfo-get meminfo "SwapFree" 0))
                                       (swap-used (- swap-total swap-free))
                                       (swap-avail (meminfo-get meminfo "MemAvailable" mem-free))
                                       (all-pids (if (pair? filter-pids) filter-pids (list-pids))))
                                  (let-values (((procs new-proc-times)
                                                (build-process-list all-pids prev-proc-times
                                                                    uptime-secs delay-secs)))
                                    ;; Filter
                                    (let* ((user-filtered
                                             (if filter-user
                                               (filter (lambda (p)
                                                         (string=? (vector-ref p 1) filter-user))
                                                       procs)
                                               procs))
                                           (idle-filtered
                                             (if show-idle?
                                               user-filtered
                                               (filter (lambda (p)
                                                         (or (> (vector-ref p 8) 0.0)
                                                             (string=? (vector-ref p 7) "R")))
                                                       user-filtered)))
                                           (sorted (sort-procs idle-filtered sort-field reverse-sort?))
                                           ;; Calculate how many header lines
                                           (header-lines (+ (if show-uptime? 1 0)
                                                            (if show-tasks? 2 0)
                                                            (if show-mem? 2 0)
                                                            2)) ;; blank + column header
                                           (avail-lines (max 1 (- term-h header-lines 1)))
                                           (effective-max (if (and max-lines (> max-lines 0))
                                                            (min max-lines avail-lines)
                                                            avail-lines))
                                           (display-procs (if (> (length sorted) effective-max)
                                                            (list-head sorted effective-max)
                                                            sorted)))
                                      ;; CPU percentages
                                      (let-values (((cpu-us cpu-sy cpu-ni cpu-id cpu-wa cpu-hi cpu-si cpu-st)
                                                    (calc-cpu-pct prev-cpu curr-cpu)))
                                        ;; Count states
                                        (let-values (((st-run st-sleep st-stop st-zombie)
                                                      (count-states idle-filtered)))
                                          ;; Render
                                          (cursor-position 1 1)
                                          (clear-screen)
                                          (display-header uptime-secs load1 load5 load15
                                                          (length idle-filtered) st-run st-sleep st-stop st-zombie
                                                          cpu-us cpu-sy cpu-ni cpu-id cpu-wa cpu-hi cpu-si cpu-st
                                                          mem-total mem-free mem-used mem-bufcache
                                                          swap-total swap-free swap-used swap-avail
                                                          show-uptime? show-tasks? show-mem?
                                                          use-color? use-bold? sort-field highlight-sort?)
                                          ;; Display processes
                                          (for-each
                                            (lambda (p)
                                              (display-proc p mem-total use-color? highlight-sort?
                                                            sort-field show-full-cmd? term-w))
                                            display-procs)
                                          (flush-output-port (current-output-port))
                                          ;; Update state
                                          (set! prev-cpu curr-cpu)
                                          (set! prev-proc-times new-proc-times)

                                          ;; Poll for keypress during delay
                                          (let* ((delay-ms (inexact->exact (floor (* delay-secs 1000))))
                                                 (poll-interval-ms 50)
                                                 (polls (quotient delay-ms poll-interval-ms)))
                                            (let poll-loop ((p 0))
                                              (cond
                                                ;; Time's up — refresh
                                                ((>= p polls)
                                                 (iter-loop (+ n 1)))

                                                ;; Key available
                                                ((char-ready? tty-in)
                                                 (let ((ch (read-char tty-in)))
                                                   (cond
                                                     ((eof-object? ch)
                                                      (set! quit? #t))

                                                     ;; q — quit
                                                     ((char=? ch #\q)
                                                      (set! quit? #t))

                                                     ;; h or ? — help
                                                     ((or (char=? ch #\h) (char=? ch #\?))
                                                      (display-help)
                                                      (wait-for-key tty-in)
                                                      (iter-loop n)) ;; re-render same iteration

                                                     ;; Space — immediate refresh
                                                     ((char=? ch #\space)
                                                      (iter-loop (+ n 1)))

                                                     ;; P — sort by CPU
                                                     ((char=? ch #\P)
                                                      (set! sort-field "%cpu")
                                                      (iter-loop n))

                                                     ;; M — sort by memory
                                                     ((char=? ch #\M)
                                                      (set! sort-field "%mem")
                                                      (iter-loop n))

                                                     ;; T — sort by time
                                                     ((char=? ch #\T)
                                                      (set! sort-field "time")
                                                      (iter-loop n))

                                                     ;; N — sort by PID
                                                     ((char=? ch #\N)
                                                      (set! sort-field "pid")
                                                      (iter-loop n))

                                                     ;; R — reverse sort
                                                     ((char=? ch #\R)
                                                      (set! reverse-sort? (not reverse-sort?))
                                                      (iter-loop n))

                                                     ;; < — previous sort field
                                                     ((char=? ch #\<)
                                                      (set! sort-field (prev-sort-field sort-field))
                                                      (iter-loop n))

                                                     ;; > — next sort field
                                                     ((char=? ch #\>)
                                                      (set! sort-field (next-sort-field sort-field))
                                                      (iter-loop n))

                                                     ;; c — toggle full command
                                                     ((char=? ch #\c)
                                                      (set! show-full-cmd? (not show-full-cmd?))
                                                      (iter-loop n))

                                                     ;; i — toggle idle
                                                     ((char=? ch #\i)
                                                      (set! show-idle? (not show-idle?))
                                                      (iter-loop n))

                                                     ;; l — toggle uptime line
                                                     ((char=? ch #\l)
                                                      (set! show-uptime? (not show-uptime?))
                                                      (iter-loop n))

                                                     ;; t — toggle task/cpu lines
                                                     ((char=? ch #\t)
                                                      (set! show-tasks? (not show-tasks?))
                                                      (iter-loop n))

                                                     ;; m — toggle mem lines
                                                     ((char=? ch #\m)
                                                      (set! show-mem? (not show-mem?))
                                                      (iter-loop n))

                                                     ;; x — toggle sort highlight
                                                     ((char=? ch #\x)
                                                      (set! highlight-sort? (not highlight-sort?))
                                                      (iter-loop n))

                                                     ;; z — toggle color
                                                     ((char=? ch #\z)
                                                      (set! use-color? (not use-color?))
                                                      (iter-loop n))

                                                     ;; B — toggle bold
                                                     ((char=? ch #\B)
                                                      (set! use-bold? (not use-bold?))
                                                      (iter-loop n))

                                                     ;; d or s — change delay
                                                     ((or (char=? ch #\d) (char=? ch #\s))
                                                      (cursor-position term-h 1)
                                                      (clear-line)
                                                      (let ((input (prompt-input tty-in
                                                                     (string-append "Change delay from "
                                                                       (format-float1 delay-secs)
                                                                       " to: "))))
                                                        (when input
                                                          (let ((v (string->number input)))
                                                            (when (and v (> v 0))
                                                              (set! delay-secs (inexact->exact v))
                                                              (when (flonum? v)
                                                                (set! delay-secs v))
                                                              (set! delay-secs (if (exact? v) (exact->inexact v) v))))))
                                                      (iter-loop n))

                                                     ;; n or # — change max tasks
                                                     ((or (char=? ch #\n) (char=? ch #\#))
                                                      (cursor-position term-h 1)
                                                      (clear-line)
                                                      (let ((input (prompt-input tty-in "Maximum tasks = 0 is unlimited: ")))
                                                        (when input
                                                          (let ((v (string->number input)))
                                                            (when (and v (>= v 0))
                                                              (set! max-lines (inexact->exact v))))))
                                                      (iter-loop n))

                                                     ;; u or U — user filter
                                                     ((or (char=? ch #\u) (char=? ch #\U))
                                                      (cursor-position term-h 1)
                                                      (clear-line)
                                                      (let ((input (prompt-input tty-in "Which user (blank for all): ")))
                                                        (when input
                                                          (set! filter-user
                                                            (if (string=? input "") #f input))))
                                                      (iter-loop n))

                                                     ;; k — kill process
                                                     ((char=? ch #\k)
                                                      (cursor-position term-h 1)
                                                      (clear-line)
                                                      (let ((pid-str (prompt-input tty-in "PID to signal/kill: ")))
                                                        (when pid-str
                                                          (let ((pid (string->number pid-str)))
                                                            (when (and pid (> pid 0))
                                                              (cursor-position term-h 1)
                                                              (clear-line)
                                                              (let ((sig-str (prompt-input tty-in
                                                                              "Send pid signal [15/sigterm]: ")))
                                                                (let ((sig (if (or (not sig-str)
                                                                                   (string=? sig-str ""))
                                                                             15
                                                                             (or (string->number sig-str) 15))))
                                                                  (let ((rc (ffi-kill (inexact->exact pid)
                                                                                      (inexact->exact sig))))
                                                                    (when (< rc 0)
                                                                      (cursor-position term-h 1)
                                                                      (clear-line)
                                                                      (display (format "Signal ~a failed for PID ~a"
                                                                                       sig pid))
                                                                      (flush-output-port (current-output-port))
                                                                      (sleep (make-time 'time-duration 0 1))))))))))
                                                      (iter-loop n))

                                                     ;; r — renice process
                                                     ((char=? ch #\r)
                                                      (cursor-position term-h 1)
                                                      (clear-line)
                                                      (let ((pid-str (prompt-input tty-in "PID to renice: ")))
                                                        (when pid-str
                                                          (let ((pid (string->number pid-str)))
                                                            (when (and pid (> pid 0))
                                                              (cursor-position term-h 1)
                                                              (clear-line)
                                                              (let ((nice-str (prompt-input tty-in
                                                                               "Renice PID to value: ")))
                                                                (when nice-str
                                                                  (let ((nice-val (string->number nice-str)))
                                                                    (when nice-val
                                                                      (let ((rc (ffi-setpriority
                                                                                  0 ;; PRIO_PROCESS
                                                                                  (inexact->exact pid)
                                                                                  (inexact->exact nice-val))))
                                                                        (when (< rc 0)
                                                                          (cursor-position term-h 1)
                                                                          (clear-line)
                                                                          (display (format "Renice failed for PID ~a"
                                                                                           pid))
                                                                          (flush-output-port (current-output-port))
                                                                          (sleep (make-time 'time-duration 0 1))))))))))))
                                                      (iter-loop n))

                                                     ;; W — write settings summary
                                                     ((char=? ch #\W)
                                                      (cursor-position term-h 1)
                                                      (clear-line)
                                                      (display (format "sort=~a rev=~a delay=~a idle=~a color=~a user=~a"
                                                                       sort-field reverse-sort? delay-secs
                                                                       show-idle? use-color?
                                                                       (or filter-user "all")))
                                                      (flush-output-port (current-output-port))
                                                      (sleep (make-time 'time-duration 0 2))
                                                      (iter-loop n))

                                                     ;; Escape sequence — skip arrow keys etc.
                                                     ((char=? ch #\x1b)
                                                      (when (char-ready? tty-in) (read-char tty-in))
                                                      (when (char-ready? tty-in) (read-char tty-in))
                                                      (poll-loop p))

                                                     ;; Unknown key — ignore
                                                     (else
                                                      (poll-loop p)))))

                                                ;; No key — sleep a bit and poll again
                                                (else
                                                 (sleep (make-time 'time-duration
                                                          (* poll-interval-ms 1000000) 0))
                                                 (poll-loop (+ p 1))))))))))))))))

                    ;; Done — cleanup
                    (cursor-show)
                    (reset-style)))))))))))

  ;; ========== Entry Point ==========

  (def (parse-pid-list str)
    ;; Parse comma-separated PID list
    (map (lambda (s) (or (string->number s) 0))
         (string-split-colon-or-comma str)))

  (def (string-split-colon-or-comma str)
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (if (> i start)
                    (cons (substring str start i) acc)
                    acc)))
        ((or (char=? (string-ref str i) #\,)
             (char=? (string-ref str i) #\:))
         (loop (+ i 1) (+ i 1)
               (if (> i start)
                 (cons (substring str start i) acc)
                 acc)))
        (else (loop (+ i 1) start acc)))))

  (def (main . args)
    (parameterize ((program-name "top"))
      (call-with-getopt
        (lambda (rest opt)
          (let ((batch? (hash-get opt 'batch))
                (delay-secs (or (let ((d (hash-get opt 'delay)))
                                  (and d (string->number d)))
                                3.0))
                (iterations (let ((n (hash-get opt 'iterations)))
                              (and n (string->number n))))
                (sort-field (or (hash-get opt 'sort) "%cpu"))
                (filter-pids (let ((p (hash-get opt 'pid)))
                               (if p (parse-pid-list p) '())))
                (filter-user (hash-get opt 'user)))
            (run-top batch? delay-secs iterations sort-field filter-pids filter-user)))
        args
        'program: "top"
        'help: "Display Linux processes."
        (flag 'batch "-b" "--batch"
          'help: "batch mode — write output suitable for piping")
        (option 'delay "-d" "--delay"
          'help: "delay between updates in seconds (default 3)"
          'value: "SECS")
        (option 'iterations "-n" "--iterations"
          'help: "number of iterations before exiting"
          'value: "NUM")
        (option 'sort "-o" "--sort"
          'help: "sort field: %cpu, %mem, pid, virt, res, time (default: %cpu)"
          'value: "FIELD")
        (option 'pid "-p" "--pid"
          'help: "monitor only these PIDs (comma-separated)"
          'value: "PID[,PID...]")
        (option 'user "-u" "--user"
          'help: "show only processes for this user"
          'value: "USER"))))

  ) ;; end library

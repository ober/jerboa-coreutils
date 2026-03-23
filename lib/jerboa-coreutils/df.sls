#!chezscheme
;;; df.sls -- Disk free space

(library (jerboa-coreutils df)
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

  (def (human-size bytes)
    (cond
      ((>= bytes (* 1024 1024 1024))
       (string-append (number->string (quotient bytes (* 1024 1024 1024))) "G"))
      ((>= bytes (* 1024 1024))
       (string-append (number->string (quotient bytes (* 1024 1024))) "M"))
      ((>= bytes 1024)
       (string-append (number->string (quotient bytes 1024)) "K"))
      (else (string-append (number->string bytes) "B"))))

  (def (rjust s width)
    (let loop ((s s))
      (if (>= (string-length s) width) s
        (loop (string-append " " s)))))

  (def (ljust s width)
    (let loop ((s s))
      (if (>= (string-length s) width) s
        (loop (string-append s " ")))))

  ;; Read /proc/mounts
  (def (read-mounts)
    (with-catch
      (lambda (e) '())
      (lambda ()
        (let* ((p (open-input-file "/proc/mounts"))
               (result (let loop ((acc '()))
                         (let ((line (get-line p)))
                           (if (eof-object? line)
                             (begin (close-port p) (reverse acc))
                             (let ((parts (string-split-spaces line)))
                               (if (>= (length parts) 3)
                                 (loop (cons (list (list-ref parts 0)
                                                   (list-ref parts 1)
                                                   (list-ref parts 2))
                                             acc))
                                 (loop acc))))))))
          result))))

  (def (string-split-spaces str)
    (let loop ((i 0) (start #f) (acc '()))
      (cond
        ((>= i (string-length str))
         (reverse (if start (cons (substring str start i) acc) acc)))
        ((char-whitespace? (string-ref str i))
         (loop (+ i 1) #f
               (if start (cons (substring str start i) acc) acc)))
        (else
         (loop (+ i 1) (or start i) acc)))))

  ;; Get statvfs info via df command
  (def (do-statvfs mnt)
    (with-catch
      (lambda (e) #f)
      (lambda ()
        (let ((cmd (string-append "df -B1 " (shell-quote mnt) " 2>/dev/null | tail -1")))
          (let-values (((to-stdin from-stdout from-stderr pid)
                        (open-process-ports cmd (buffer-mode block) (native-transcoder))))
            (close-port to-stdin)
            (let ((line (get-line from-stdout)))
              (close-port from-stdout)
              (close-port from-stderr)
              (if (or (not line) (eof-object? line))
                #f
                (let ((parts (string-split-spaces line)))
                  (if (< (length parts) 4)
                    #f
                    (let ((total (string->number (list-ref parts 1)))
                          (used (string->number (list-ref parts 2)))
                          (avail (string->number (list-ref parts 3))))
                      (if (and total used avail)
                        (list total used avail)
                        #f)))))))))))

  (def (print-df-header human inode show-type)
    (if inode
      (displayln (ljust "Filesystem" 20)
                 (if show-type (string-append (ljust "Type" 10) " ") "")
                 (rjust "Inodes" 12) " "
                 (rjust "IUsed" 12) " "
                 (rjust "IFree" 12) " "
                 (rjust "IUse%" 6) " "
                 "Mounted on")
      (displayln (ljust "Filesystem" 20)
                 (if show-type (string-append (ljust "Type" 10) " ") "")
                 (rjust (if human "Size" "1K-blocks") 12) " "
                 (rjust "Used" 12) " "
                 (rjust "Available" 12) " "
                 (rjust "Use%" 6) " "
                 "Mounted on")))

  (def (print-df-row dev mnt fstype sv human kb inode show-type)
    (let* ((total-bytes (list-ref sv 0))
           (used-bytes (list-ref sv 1))
           (avail-bytes (list-ref sv 2))
           (use-pct (if (> total-bytes 0)
                      (quotient (* used-bytes 100) total-bytes)
                      0))
           (fmt-size (lambda (bytes)
                       (cond
                         (human (human-size bytes))
                         (else (number->string (quotient bytes 1024)))))))
      (unless inode
        (displayln (ljust dev 20)
                   (if show-type (string-append (ljust fstype 10) " ") "")
                   (rjust (fmt-size total-bytes) 12) " "
                   (rjust (fmt-size used-bytes) 12) " "
                   (rjust (fmt-size avail-bytes) 12) " "
                   (rjust (string-append (number->string use-pct) "%") 6) " "
                   mnt))))

  (def (main . args)
    (parameterize ((program-name "df"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((human (hash-get opt 'human-readable))
                 (kb (hash-get opt 'kilobytes))
                 (inode (hash-get opt 'inodes))
                 (show-type (hash-get opt 'print-type))
                 (files (hash-ref opt 'rest))
                   (mounts (if (null? files)
                             (read-mounts)
                             (map (lambda (f) (list f f "?")) files))))
              (print-df-header human inode show-type)
              (let loop ((mts mounts))
                (unless (null? mts)
                  (let* ((mt (car mts))
                         (dev (list-ref mt 0))
                         (mnt (list-ref mt 1))
                         (fstype (list-ref mt 2))
                         (skip (and (null? files)
                                    (member fstype '("proc" "sysfs" "devpts" "devtmpfs"
                                                     "cgroup" "cgroup2" "tmpfs" "hugetlbfs"
                                                     "mqueue" "pstore" "debugfs" "securityfs"
                                                     "fusectl" "bpf" "tracefs" "overlay"
                                                     "autofs" "ramfs")))))
                    (unless skip
                      (let ((sv (do-statvfs mnt)))
                        (when sv
                          (print-df-row dev mnt fstype sv human kb inode show-type))))
                    (loop (cdr mts)))))))
        args
        'program: "df"
        'help: "Report file system disk space usage."
        (flag 'human-readable "--human-readable"
          'help: "print sizes in human readable format (e.g., 1K 234M 2G)")
        (flag 'kilobytes "-k"
          'help: "like --block-size=1K")
        (flag 'inodes "-i" "--inodes"
          'help: "list inode information instead of block usage")
        (flag 'print-type "-T" "--print-type"
          'help: "print file system type")
        (rest-arguments 'rest))))

  ) ;; end library

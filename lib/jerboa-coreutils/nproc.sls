#!chezscheme
;;; nproc.sls -- Print the number of processing units available

(library (jerboa-coreutils nproc)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core)
          (only (std sugar) with-catch)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; Read /sys to get CPU count
  (def (get-nproc-all)
    (with-catch
      (lambda (e) 1)
      (lambda ()
        (let ((content (call-with-input-file "/sys/devices/system/cpu/present" get-line)))
          ;; Format is "0-N" where N+1 is the count
          (if (and content (string? content))
            (let ((dash (string-index content #\-)))
              (if dash
                (+ 1 (string->number (substring content (+ dash 1) (string-length content))))
                (+ 1 (string->number content))))
            1)))))

  (def (string-index str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  (def (string-split-char str ch)
    ;; Split string on first occurrence of ch, return first part
    (let ((idx (string-index str ch)))
      (if idx
        (substring str 0 idx)
        str)))

  (def (main . args)
    (parameterize ((program-name "nproc"))
      (call-with-getopt
        (lambda (_ opt)
            (let* ((n (if (hash-get opt 'all)
                        (get-nproc-all)
                        (let ((omp (getenv "OMP_NUM_THREADS" #f)))
                          (if omp
                            (let ((val (string->number (string-split-char omp #\,))))
                              (or val (get-nproc-all)))
                            (get-nproc-all)))))
                   (ignore (if (hash-get opt 'ignore) (string->number (hash-ref opt 'ignore)) 0))
                   (result (max 1 (- n (or ignore 0)))))
              (displayln result)))
        args
        'program: "nproc"
        'help: "Print the number of processing units available."
        (flag 'all "--all"
          'help: "print the number of installed processors")
        (option 'ignore "--ignore"
          'help: "if possible, exclude N processing units"
          'default: #f))))

  ) ;; end library

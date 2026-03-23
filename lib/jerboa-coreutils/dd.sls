#!chezscheme
;;; dd.sls -- Convert and copy a file

(library (jerboa-coreutils dd)
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

  ;; Parse a size value with optional suffixes: c=1, w=2, b=512, K=1024, M=1M, G=1G
  (def (parse-size str)
    (if (string=? str "") 0
      (let* ((len (string-length str))
             (last-ch (string-ref str (- len 1)))
             (multiplier
              (cond
                ((eqv? last-ch #\c) 1)
                ((eqv? last-ch #\w) 2)
                ((eqv? last-ch #\b) 512)
                ((or (eqv? last-ch #\K) (eqv? last-ch #\k)) 1024)
                ((or (eqv? last-ch #\M) (eqv? last-ch #\m)) (* 1024 1024))
                ((or (eqv? last-ch #\G) (eqv? last-ch #\g)) (* 1024 1024 1024))
                (else #f))))
        (if multiplier
          (let ((n (string->number (substring str 0 (- len 1)))))
            (if n (* (inexact->exact n) multiplier) #f))
          (let ((n (string->number str)))
            (if n (inexact->exact n) #f))))))

  ;; Find '=' position in string
  (def (string-find-eq str)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) #\=) i)
        (else (loop (+ i 1))))))

  ;; Parse dd operands from argument list
  (def (parse-dd-args args)
    (let ((opts (make-hashtable string-hash string=?)))
      (for-each
        (lambda (arg)
          (let ((eq-pos (string-find-eq arg)))
            (if eq-pos
              (let ((key (substring arg 0 eq-pos))
                    (val (substring arg (+ eq-pos 1) (string-length arg))))
                (hashtable-set! opts key val))
              (warn "unrecognized operand '~a'" arg))))
        args)
      opts))

  (def (ht-ref ht key default)
    (if (hashtable-contains? ht key)
      (hashtable-ref ht key default)
      default))

  (def (string-contains-dd str sub)
    (let ((slen (string-length str))
          (sublen (string-length sub)))
      (if (> sublen slen) #f
        (let loop ((i 0))
          (cond
            ((> (+ i sublen) slen) #f)
            ((string=? (substring str i (+ i sublen)) sub) #t)
            (else (loop (+ i 1))))))))

  (def (main . args)
    (parameterize ((program-name "dd"))
      (init-security!)
      (install-io-seccomp!)
      ;; Handle --help and --version
      (when (and (pair? args) (equal? (car args) "--help"))
        (displayln "Usage: dd [OPERAND]...")
        (displayln "Copy a file, converting and formatting according to the operands.")
        (displayln "")
        (displayln "  if=FILE     read from FILE instead of stdin")
        (displayln "  of=FILE     write to FILE instead of stdout")
        (displayln "  bs=BYTES    read and write up to BYTES bytes at a time")
        (displayln "  ibs=BYTES   read up to BYTES bytes at a time ('default: 512)")
        (displayln "  obs=BYTES   write BYTES bytes at a time ('default: 512)")
        (displayln "  count=N     copy only N input blocks")
        (displayln "  skip=N      skip N ibs-sized blocks at start of input")
        (displayln "  seek=N      skip N obs-sized blocks at start of output")
        (displayln "  conv=notrunc  do not truncate the output file")
        (displayln "  status=LEVEL  LEVEL of information to print to stderr")
        (displayln "                none=suppress everything, noxfer=suppress transfer stats")
        (exit 0))
      (when (and (pair? args) (equal? (car args) "--version"))
        (displayln "dd (jerboa-coreutils) 0.1")
        (exit 0))

      (let* ((opts (parse-dd-args args))
             (if-file (ht-ref opts "if" #f))
             (of-file (ht-ref opts "of" #f))
             (bs-str (ht-ref opts "bs" #f))
             (ibs-str (ht-ref opts "ibs" "512"))
             (obs-str (ht-ref opts "obs" "512"))
             (count-str (ht-ref opts "count" #f))
             (skip-str (ht-ref opts "skip" "0"))
             (seek-str (ht-ref opts "seek" "0"))
             (conv-str (ht-ref opts "conv" ""))
             (status-str (ht-ref opts "status" ""))
             ;; Parse sizes
             (ibs (if bs-str (parse-size bs-str) (parse-size ibs-str)))
             (obs (if bs-str (parse-size bs-str) (parse-size obs-str)))
             (count (if count-str (parse-size count-str) #f))
             (skip (parse-size skip-str))
             (seek (parse-size seek-str))
             (notrunc (string-contains-dd conv-str "notrunc"))
             (conv-ucase (string-contains-dd conv-str "ucase"))
             (conv-lcase (string-contains-dd conv-str "lcase"))
             (conv-swab (string-contains-dd conv-str "swab"))
             (status-none (string=? status-str "none"))
             (status-noxfer (or (string=? status-str "noxfer") status-none)))

        (unless ibs (die "invalid input block size"))
        (unless obs (die "invalid output block size"))

        ;; Open input (binary mode)
        (let ((in (if if-file
                    (with-catch
                      (lambda (e) (die "cannot open '~a' for reading: ~a" if-file (error-message e)))
                      (lambda () (open-file-input-port if-file)))
                    (standard-input-port)))
              (out (if of-file
                     (with-catch
                       (lambda (e) (die "cannot open '~a' for writing: ~a" of-file (error-message e)))
                       (lambda ()
                         (if notrunc
                           (open-file-output-port of-file (file-options no-fail no-truncate))
                           (open-file-output-port of-file (file-options no-fail)))))
                     (standard-output-port))))

          ;; Skip input blocks
          (when (> skip 0)
            (let ((skip-buf (make-bytevector ibs)))
              (let loop ((i 0))
                (when (< i skip)
                  (get-bytevector-n! in skip-buf 0 ibs)
                  (loop (+ i 1))))))

          ;; Seek output blocks (write zeros)
          (when (> seek 0)
            (let ((seek-buf (make-bytevector obs 0)))
              (let loop ((i 0))
                (when (< i seek)
                  (put-bytevector out seek-buf 0 obs)
                  (loop (+ i 1))))))

          ;; Main copy loop
          (let ((buf (make-bytevector ibs))
                (total-in 0)
                (total-out 0)
                (full-in 0)
                (partial-in 0)
                (full-out 0)
                (partial-out 0))
            (let loop ((blocks-read 0))
              (when (or (not count) (< blocks-read count))
                (let ((n (get-bytevector-n! in buf 0 ibs)))
                  (when (and (fixnum? n) (> n 0))
                    (set! total-in (+ total-in n))
                    (if (= n ibs)
                      (set! full-in (+ full-in 1))
                      (set! partial-in (+ partial-in 1)))
                    ;; Apply conv transforms and write
                    (let ((write-buf
                           (cond
                             (conv-swab
                              ;; Swap adjacent bytes in buf[0..n-1]
                              (let ((b (make-bytevector n)))
                                (let swap ((i 0))
                                  (cond
                                    ((>= (+ i 1) n)
                                     ;; Odd byte at end: copy as-is
                                     (when (= i (- n 1))
                                       (bytevector-u8-set! b i (bytevector-u8-ref buf i))))
                                    (else
                                     (bytevector-u8-set! b i (bytevector-u8-ref buf (+ i 1)))
                                     (bytevector-u8-set! b (+ i 1) (bytevector-u8-ref buf i))
                                     (swap (+ i 2)))))
                                b))
                             (conv-ucase
                              (let ((b (make-bytevector n)))
                                (let up ((i 0))
                                  (when (< i n)
                                    (let ((byte (bytevector-u8-ref buf i)))
                                      (bytevector-u8-set! b i
                                        (if (and (>= byte 97) (<= byte 122))
                                          (- byte 32)
                                          byte)))
                                    (up (+ i 1))))
                                b))
                             (conv-lcase
                              (let ((b (make-bytevector n)))
                                (let dn ((i 0))
                                  (when (< i n)
                                    (let ((byte (bytevector-u8-ref buf i)))
                                      (bytevector-u8-set! b i
                                        (if (and (>= byte 65) (<= byte 90))
                                          (+ byte 32)
                                          byte)))
                                    (dn (+ i 1))))
                                b))
                             (else buf))))
                      (put-bytevector out write-buf 0 n))
                    (set! total-out (+ total-out n))
                    (if (= n obs)
                      (set! full-out (+ full-out 1))
                      (set! partial-out (+ partial-out 1)))
                    (loop (+ blocks-read 1))))))

            ;; Flush output
            (flush-output-port out)

            ;; Close files
            (when if-file (close-port in))
            (when of-file (close-port out))

            ;; Print statistics
            (unless status-none
              (eprintf "~a+~a records in\n" full-in partial-in)
              (eprintf "~a+~a records out\n" full-out partial-out)
              (unless status-noxfer
                (eprintf "~a bytes copied\n" total-out))))))))

  ) ;; end library

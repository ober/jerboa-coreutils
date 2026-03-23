#!chezscheme
;;; base32.sls -- Base32 encoding/decoding

(library (jerboa-coreutils base32)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
)
          (except (jerboa core) bytes->string)
          (only (std sugar) with-catch)
          (only (std format) eprintf)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version)
          (jerboa-coreutils common security))

  ;; Base32 alphabet
  (def *b32-alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

  (def (base32-encode bv)
    (let* ((len (bytevector-length bv))
           (out (open-output-string)))
      (let loop ((i 0) (buffer 0) (bits 0))
        (cond
          ((and (>= i len) (zero? bits))
           (get-output-string out))
          ((< bits 5)
           (if (< i len)
             (loop (+ i 1)
                   (bitwise-ior (bitwise-arithmetic-shift-left buffer 8)
                                (bytevector-u8-ref bv i))
                   (+ bits 8))
             ;; Pad remaining bits
             (let* ((padded (bitwise-arithmetic-shift-left buffer (- 5 bits)))
                    (idx (bitwise-and padded #x1f)))
               (write-char (string-ref *b32-alphabet* idx) out)
               ;; Add padding
               (let* ((total-chars (+ 1 (quotient (* i 8) 5)))
                      ;; Round up to multiple of 8
                      (padded-len (* (ceiling (/ total-chars 8)) 8))
                      (padding (inexact->exact (- padded-len total-chars))))
                 (let pad-loop ((p 0))
                   (when (< p padding)
                     (write-char #\= out)
                     (pad-loop (+ p 1)))))
               (get-output-string out))))
          (else
           (let* ((shift (- bits 5))
                  (idx (bitwise-and (bitwise-arithmetic-shift-right buffer shift) #x1f)))
             (write-char (string-ref *b32-alphabet* idx) out)
             (loop i (bitwise-and buffer (- (bitwise-arithmetic-shift-left 1 shift) 1)) shift)))))))

  (def (b32-char->val c)
    (let ((cu (char-upcase c)))
      (cond
        ((and (char>=? cu #\A) (char<=? cu #\Z))
         (- (char->integer cu) (char->integer #\A)))
        ((and (char>=? cu #\2) (char<=? cu #\7))
         (+ 26 (- (char->integer cu) (char->integer #\2))))
        (else #f))))

  (def (base32-decode str)
    (let-values (((out extract) (open-bytevector-output-port)))
      (let ((len (string-length str)))
        (let loop ((i 0) (buffer 0) (bits 0))
          (if (>= i len)
            (extract)
            (let ((c (string-ref str i)))
              (cond
                ((eqv? c #\=)
                 (extract))
                (else
                 (let ((val (b32-char->val c)))
                   (if (not val)
                     (loop (+ i 1) buffer bits) ;; skip invalid
                     (let* ((new-buffer (bitwise-ior
                                          (bitwise-arithmetic-shift-left buffer 5) val))
                            (new-bits (+ bits 5)))
                       (if (>= new-bits 8)
                         (let* ((shift (- new-bits 8))
                                (byte (bitwise-and
                                        (bitwise-arithmetic-shift-right new-buffer shift)
                                        #xff)))
                           (put-u8 out byte)
                           (loop (+ i 1)
                                 (bitwise-and new-buffer
                                   (- (bitwise-arithmetic-shift-left 1 shift) 1))
                                 shift))
                         (loop (+ i 1) new-buffer new-bits)))))))))))))

  (def (main . args)
    (parameterize ((program-name "base32"))
      (init-security!)
      (install-readonly-seccomp!)
      (call-with-getopt
        (lambda (_ opt)
          (let* ((wrap (if (hash-get opt 'wrap) (string->number (hash-ref opt 'wrap)) 76))
                 (input-port (if (or (null? (hash-ref opt 'rest)) (string=? (car (hash-ref opt 'rest)) "-"))
                               (standard-input-port)
                               (open-input-file (car (hash-ref opt 'rest)))))
                 (input (read-all-bytes input-port)))
            (when (and (pair? (hash-ref opt 'rest)) (not (string=? (car (hash-ref opt 'rest)) "-")))
              (close-port input-port))
            (if (hash-get opt 'decode)
              (let* ((input-str (bytes->string input))
                     (clean (string-filter input-str
                              (lambda (c) (not (char-whitespace? c)))))
                     (decoded (base32-decode clean)))
                (write-u8vector decoded))
              (let* ((encoded (base32-encode input))
                     (out (if (and wrap (> wrap 0))
                            (wrap-string encoded wrap)
                            encoded)))
                (display out)
                (newline)))))
        args
        'program: "base32"
        'help: "Base32 encode or decode FILE, or standard input, to standard output."
        (flag 'decode "-d" "--decode"
          'help: "decode data")
        (option 'wrap "-w" "--wrap"
          'help: "wrap encoded lines after COLS character (default 76). Use 0 to disable."
          'default: #f)
        (rest-arguments 'rest))))

  (def (read-all-bytes port)
    (let-values (((out extract) (open-bytevector-output-port)))
      (let loop ()
        (let ((byte (get-u8 port)))
          (if (eof-object? byte)
            (extract)
            (begin (put-u8 out byte) (loop)))))))

  (def (bytes->string bv)
    (let ((len (bytevector-length bv)))
      (let ((str (make-string len)))
        (let loop ((i 0))
          (if (>= i len) str
            (begin (string-set! str i (integer->char (bytevector-u8-ref bv i)))
                   (loop (+ i 1))))))))

  (def (string-filter str pred)
    (let ((out (open-output-string)))
      (let loop ((i 0))
        (when (< i (string-length str))
          (when (pred (string-ref str i))
            (write-char (string-ref str i) out))
          (loop (+ i 1))))
      (get-output-string out)))

  (def (wrap-string str cols)
    (let ((len (string-length str)))
      (let loop ((i 0) (out (open-output-string)))
        (if (>= i len)
          (get-output-string out)
          (let ((end (min (+ i cols) len)))
            (display (substring str i end) out)
            (newline out)
            (loop end out))))))

  (def (write-u8vector bv)
    (let ((port (standard-output-port)))
      (let loop ((i 0))
        (when (< i (bytevector-length bv))
          (put-u8 port (bytevector-u8-ref bv i))
          (loop (+ i 1))))))

  ) ;; end library

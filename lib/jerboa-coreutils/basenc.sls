#!chezscheme
;;; basenc.sls -- Encode/decode data in various formats

(library (jerboa-coreutils basenc)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
)
          (except (jerboa core) bytes->string)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (std text base64)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  ;; ---- Base32 implementation ----
  (define *b32-alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

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
                   (bitwise-ior (bitwise-arithmetic-shift-left buffer 8) (bytevector-u8-ref bv i))
                   (+ bits 8))
             (let* ((padded (bitwise-arithmetic-shift-left buffer (- 5 bits)))
                    (idx (bitwise-and padded #x1f)))
               (write-char (string-ref *b32-alphabet* idx) out)
               (let* ((total-chars (+ 1 (quotient (* i 8) 5)))
                      (padded-len (* (inexact->exact (ceiling (/ total-chars 8))) 8))
                      (padding (- padded-len total-chars)))
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

  (def (base32-decode str)
    (let* ((len (string-length str))
           (acc '()))
      (let loop ((i 0) (buffer 0) (bits 0) (result '()))
        (if (>= i len)
          (u8-list->bytevector (reverse result))
          (let ((c (string-ref str i)))
            (cond
              ((eqv? c #\=)
               (u8-list->bytevector (reverse result)))
              (else
               (let ((val (b32-char->val c)))
                 (if (not val)
                   (loop (+ i 1) buffer bits result)
                   (let* ((new-buffer (bitwise-ior (bitwise-arithmetic-shift-left buffer 5) val))
                          (new-bits (+ bits 5)))
                     (if (>= new-bits 8)
                       (let* ((shift (- new-bits 8))
                              (byte (bitwise-and (bitwise-arithmetic-shift-right new-buffer shift) #xff)))
                         (loop (+ i 1)
                               (bitwise-and new-buffer (- (bitwise-arithmetic-shift-left 1 shift) 1))
                               shift
                               (cons byte result)))
                       (loop (+ i 1) new-buffer new-bits result))))))))))))

  (def (b32-char->val c)
    (let ((cu (char-upcase c)))
      (cond
        ((and (char>=? cu #\A) (char<=? cu #\Z))
         (- (char->integer cu) (char->integer #\A)))
        ((and (char>=? cu #\2) (char<=? cu #\7))
         (+ 26 (- (char->integer cu) (char->integer #\2))))
        (else #f))))

  ;; ---- Base16 (hex) implementation ----
  (def (base16-encode bv)
    (let* ((len (bytevector-length bv))
           (out (open-output-string)))
      (let loop ((i 0))
        (when (< i len)
          (let* ((b (bytevector-u8-ref bv i))
                 (hi (bitwise-arithmetic-shift-right b 4))
                 (lo (bitwise-and b #xF)))
            (write-char (hex-digit hi) out)
            (write-char (hex-digit lo) out))
          (loop (+ i 1))))
      (get-output-string out)))

  (def (hex-digit n)
    (integer->char
      (if (< n 10)
        (+ (char->integer #\0) n)
        (+ (char->integer #\A) (- n 10)))))

  (def (base16-decode str)
    (let* ((len (string-length str)))
      (let loop ((i 0) (result '()))
        (if (>= (+ i 1) len)
          (u8-list->bytevector (reverse result))
          (let ((hi (hex-val (string-ref str i)))
                (lo (hex-val (string-ref str (+ i 1)))))
            (if (and hi lo)
              (loop (+ i 2) (cons (+ (* hi 16) lo) result))
              (loop (+ i 2) result)))))))

  (def (hex-val c)
    (cond
      ((and (char>=? c #\0) (char<=? c #\9))
       (- (char->integer c) (char->integer #\0)))
      ((and (char>=? (char-upcase c) #\A) (char<=? (char-upcase c) #\F))
       (+ 10 (- (char->integer (char-upcase c)) (char->integer #\A))))
      (else #f)))

  ;; ---- Base2 (binary) implementation ----
  (def (base2-encode bv)
    (let* ((len (bytevector-length bv))
           (out (open-output-string)))
      (let loop ((i 0))
        (when (< i len)
          (let ((b (bytevector-u8-ref bv i)))
            (let bit-loop ((bit 7))
              (when (>= bit 0)
                (write-char (if (zero? (bitwise-and b (bitwise-arithmetic-shift-left 1 bit))) #\0 #\1) out)
                (bit-loop (- bit 1)))))
          (loop (+ i 1))))
      (get-output-string out)))

  (def (base2-decode str)
    (let* ((clean (string-filter-ws str))
           (len (string-length clean)))
      (let loop ((i 0) (result '()))
        (if (> (+ i 7) len)
          (u8-list->bytevector (reverse result))
          (let byte-loop ((j 0) (val 0))
            (if (>= j 8)
              (loop (+ i 8) (cons val result))
              (let ((bit (if (eqv? (string-ref clean (+ i j)) #\1) 1 0)))
                (byte-loop (+ j 1) (+ (* val 2) bit)))))))))

  (def (string-filter-ws str)
    (let ((out (open-output-string)))
      (let loop ((i 0))
        (when (< i (string-length str))
          (let ((c (string-ref str i)))
            (unless (char-whitespace? c)
              (write-char c out)))
          (loop (+ i 1))))
      (get-output-string out)))

  ;; ---- Common helpers ----
  (def (read-all-bytes port)
    (let loop ((acc '()))
      (let ((byte (get-u8 port)))
        (if (eof-object? byte)
          (u8-list->bytevector (reverse acc))
          (loop (cons byte acc))))))

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

  (def (write-bv-out bv)
    (let ((port (standard-output-port)))
      (let loop ((i 0))
        (when (< i (bytevector-length bv))
          (put-u8 port (bytevector-u8-ref bv i))
          (loop (+ i 1))))))

  (def (main . args)
    (parameterize ((program-name "basenc"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((decode (hash-get opt 'decode))
                 (wrap (if (hash-get opt 'wrap) (string->number (hash-ref opt 'wrap)) 76))
                 (input-port (if (or (null? (hash-ref opt 'rest)) (string=? (car (hash-ref opt 'rest)) "-"))
                               (standard-input-port)
                               (open-file-input-port (car (hash-ref opt 'rest)))))
                 (input (read-all-bytes input-port))
                 (encoding (cond
                             ((hash-get opt 'base64) 'base64)
                             ((hash-get opt 'base64url) 'base64)
                             ((hash-get opt 'base32) 'base32)
                             ((hash-get opt 'base32hex) 'base32)
                             ((hash-get opt 'base16) 'base16)
                             ((hash-get opt 'base2msbf) 'base2)
                             ((hash-get opt 'base2lsbf) 'base2)
                             (else 'base64))))
            (when (and (pair? (hash-ref opt 'rest)) (not (string=? (car (hash-ref opt 'rest)) "-")))
              (close-port input-port))
            (if decode
              ;; Decode
              (let* ((input-str (bytes->string input))
                     (clean (string-filter input-str
                              (lambda (c) (not (char-whitespace? c)))))
                     (decoded (case encoding
                                ((base64) (base64-decode clean))
                                ((base32) (base32-decode clean))
                                ((base16) (base16-decode clean))
                                ((base2) (base2-decode clean))
                                (else (base64-decode clean)))))
                (write-bv-out decoded))
              ;; Encode
              (let* ((encoded (case encoding
                                ((base64) (base64-encode input))
                                ((base32) (base32-encode input))
                                ((base16) (base16-encode input))
                                ((base2) (base2-encode input))
                                (else (base64-encode input))))
                     (out (if (and wrap (> wrap 0))
                            (wrap-string encoded wrap)
                            (string-append encoded "\n"))))
                (display out)))))
        args
        'program: "basenc"
        'help: "Encode/decode data and print to standard output."
        (flag 'decode "-d" "--decode"
          'help: "decode data")
        (flag 'base64 "--base64"
          'help: "same as base64 (RFC 4648 section 4)")
        (flag 'base64url "--base64url"
          'help: "file- and url-safe base64 (RFC 4648 section 5)")
        (flag 'base32 "--base32"
          'help: "same as base32 (RFC 4648 section 6)")
        (flag 'base32hex "--base32hex"
          'help: "extended hex base32 (RFC 4648 section 7)")
        (flag 'base16 "--base16"
          'help: "hex encoding (RFC 4648 section 8)")
        (flag 'base2msbf "--base2msbf"
          'help: "bit string with most significant bit first")
        (flag 'base2lsbf "--base2lsbf"
          'help: "bit string with least significant bit first")
        (option 'wrap "-w" "--wrap"
          'help: "wrap encoded lines after COLS characters (default 76). Use 0 to disable."
          'default: #f)
        (rest-arguments 'rest))))

  ) ;; end library

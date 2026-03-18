#!chezscheme
;;; base64.sls -- Base64 encoding/decoding

(library (jerboa-coreutils base64)
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
          (std text base64)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "base64"))
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
              ;; Decode
              (let* ((input-str (bytes->string input))
                     ;; Strip whitespace for decoding
                     (clean (string-filter input-str
                              (lambda (c) (not (char-whitespace? c)))))
                     (decoded (base64-decode clean)))
                (write-u8vector decoded))
              ;; Encode
              (let* ((encoded (base64-encode input))
                     (out (if (and wrap (> wrap 0))
                            (wrap-string encoded wrap)
                            encoded)))
                (display out)
                (newline)))))
        args
        'program: "base64"
        'help: "Base64 encode or decode FILE, or standard input, to standard output."
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
            (begin
              (put-u8 out byte)
              (loop)))))))

  (def (bytes->string bv)
    (let ((len (bytevector-length bv)))
      (let ((str (make-string len)))
        (let loop ((i 0))
          (if (>= i len) str
            (begin
              (string-set! str i (integer->char (bytevector-u8-ref bv i)))
              (loop (+ i 1))))))))

  (def (string-filter str pred)
    (let ((out (open-output-string)))
      (let loop ((i 0))
        (when (< i (string-length str))
          (let ((c (string-ref str i)))
            (when (pred c)
              (write-char c out)))
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

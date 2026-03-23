#!chezscheme
;;; common.sls -- Shared utilities for jerboa-coreutils

(library (jerboa-coreutils common)
  (export
    program-name
    die
    warn
    try-help
    split-long-opts
    shell-quote
    string-contains?
    secure-random-bytes
    secure-random-integer
    safe-path-join
    path-within-base?
    EXIT_SUCCESS
    EXIT_FAILURE)

  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            printf fprintf)
          (std format))

  ;; Exit codes
  (define EXIT_SUCCESS 0)
  (define EXIT_FAILURE 1)

  ;; Current program name - set from argv[0]
  (define program-name (make-parameter "coreutils"))

  ;; Print error message to stderr and exit with code
  (define (die fmt . args)
    (eprintf "~a: ~?~n" (program-name) fmt args)
    (exit EXIT_FAILURE))

  ;; Print warning message to stderr
  (define (warn fmt . args)
    (eprintf "~a: ~?~n" (program-name) fmt args))

  ;; Print "Try 'prog --help' for more information."
  (define (try-help)
    (eprintf "Try '~a --help' for more information.~n" (program-name))
    (exit EXIT_FAILURE))

  ;; Split --key=value args into --key value for getopt compatibility
  (define (split-long-opts args)
    (let loop ((rest args) (acc '()))
      (if (null? rest)
        (reverse acc)
        (let ((arg (car rest)))
          (let ((eqpos (string-index-eq arg)))
            (if (and eqpos
                     (> (string-length arg) 2)
                     (char=? (string-ref arg 0) #\-)
                     (char=? (string-ref arg 1) #\-))
              (loop (cdr rest)
                    (cons (substring arg (+ eqpos 1) (string-length arg))
                          (cons (substring arg 0 eqpos) acc)))
              (loop (cdr rest) (cons arg acc))))))))

  (define (string-index-eq str)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((char=? (string-ref str i) #\=) i)
        (else (loop (+ i 1))))))

  ;; ========== Shell quoting ==========
  ;; Single-quote a string for safe shell interpolation.
  ;; The only character that needs escaping inside single quotes is
  ;; the single quote itself: end quote, escaped quote, restart quote.
  (define (shell-quote str)
    (string-append "'" (let loop ((i 0) (acc '()))
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ((c (string-ref str i)))
          (if (eqv? c #\')
            (loop (+ i 1) (append (reverse (string->list "'\\''")) acc))
            (loop (+ i 1) (cons c acc)))))) "'"))

  ;; ========== String utilities ==========
  (define (string-contains? str sub)
    (let ((slen (string-length str))
          (sublen (string-length sub)))
      (if (> sublen slen) #f
        (let loop ((i 0))
          (cond
            ((> (+ i sublen) slen) #f)
            ((string=? (substring str i (+ i sublen)) sub) #t)
            (else (loop (+ i 1))))))))

  ;; ========== CSPRNG ==========
  ;; Read cryptographically secure random bytes from /dev/urandom.
  ;; This replaces insecure (random-integer N) for security-sensitive
  ;; uses like mktemp, shred, and shuf.
  (define (secure-random-bytes n)
    (let ((port (open-file-input-port "/dev/urandom"))
          (buf (make-bytevector n)))
      (dynamic-wind
        (lambda () (void))
        (lambda ()
          (let loop ((offset 0))
            (when (< offset n)
              (let ((got (get-bytevector-n! port buf offset (- n offset))))
                (when (and (fixnum? got) (> got 0))
                  (loop (+ offset got))))))
          buf)
        (lambda () (close-port port)))))

  ;; Return a cryptographically secure random integer in [0, bound).
  ;; Uses rejection sampling to avoid modulo bias.
  (define (secure-random-integer bound)
    (if (<= bound 1)
      0
      (if (<= bound 256)
        ;; Single-byte fast path with rejection sampling
        (let ((limit (- 256 (modulo 256 bound))))
          (let loop ()
            (let* ((buf (secure-random-bytes 1))
                   (val (bytevector-u8-ref buf 0)))
              (if (< val limit)
                (modulo val bound)
                (loop)))))
        ;; Multi-byte path: use 4 bytes for up to 2^32
        (let ((limit (- (expt 2 32) (modulo (expt 2 32) bound))))
          (let loop ()
            (let* ((buf (secure-random-bytes 4))
                   (val (+ (bytevector-u8-ref buf 0)
                           (* 256 (bytevector-u8-ref buf 1))
                           (* 65536 (bytevector-u8-ref buf 2))
                           (* 16777216 (bytevector-u8-ref buf 3)))))
              (if (< val limit)
                (modulo val bound)
                (loop))))))))

  ;; ========== Path safety ==========
  ;; Check if a path stays within a base directory.
  ;; Rejects NUL bytes and ".." traversal that escapes the base.
  (define (path-within-base? path base)
    ;; Reject NUL bytes
    (when (string-contains? path (string #\nul))
      (error 'path-within-base? "path contains NUL byte"))
    ;; Normalize: split on /, resolve . and .., check prefix
    (let* ((base-parts (normalize-path-parts (split-path base)))
           (path-parts (normalize-path-parts (split-path path)))
           (blen (length base-parts)))
      ;; path-parts must start with all of base-parts
      (and (>= (length path-parts) blen)
           (let loop ((bp base-parts) (pp path-parts))
             (cond
               ((null? bp) #t)
               ((string=? (car bp) (car pp))
                (loop (cdr bp) (cdr pp)))
               (else #f))))))

  ;; Join base + relative path, rejecting traversal escapes.
  ;; Returns the joined path or #f if the result would escape base.
  (define (safe-path-join base relative)
    ;; Reject NUL bytes
    (when (string-contains? relative (string #\nul))
      (error 'safe-path-join "path contains NUL byte"))
    ;; Reject absolute relative paths
    (when (and (> (string-length relative) 0)
               (eqv? (string-ref relative 0) #\/))
      (error 'safe-path-join "relative path must not be absolute"))
    (let* ((joined (string-append base "/" relative))
           (base-parts (normalize-path-parts (split-path base)))
           (joined-parts (normalize-path-parts (split-path joined)))
           (blen (length base-parts)))
      (if (and (>= (length joined-parts) blen)
               (let loop ((bp base-parts) (jp joined-parts))
                 (cond
                   ((null? bp) #t)
                   ((string=? (car bp) (car jp))
                    (loop (cdr bp) (cdr jp)))
                   (else #f))))
        joined
        #f)))

  ;; Split a path by /
  (define (split-path path)
    (let ((len (string-length path)))
      (let loop ((i 0) (start 0) (acc '()))
        (cond
          ((>= i len)
           (reverse (if (> i start)
                      (cons (substring path start i) acc)
                      acc)))
          ((eqv? (string-ref path i) #\/)
           (loop (+ i 1) (+ i 1)
                 (if (> i start)
                   (cons (substring path start i) acc)
                   acc)))
          (else (loop (+ i 1) start acc))))))

  ;; Normalize path parts: resolve "." and ".."
  (define (normalize-path-parts parts)
    (let loop ((rest parts) (acc '()))
      (cond
        ((null? rest) (reverse acc))
        ((string=? (car rest) ".") (loop (cdr rest) acc))
        ((string=? (car rest) "..")
         (loop (cdr rest)
               (if (null? acc) '() (cdr acc))))
        (else (loop (cdr rest) (cons (car rest) acc))))))

  ) ;; end library

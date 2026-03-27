#!chezscheme
;;; grep/pcre2.sls -- PCRE2 FFI bindings for grep
;;;
;;; Provides regex compilation and matching via libpcre2-8.
;;; Supports caseless, multiline, and UTF-8 modes.
;;; Includes BRE-to-ERE conversion for GNU grep -G compatibility.

(library (jerboa-coreutils grep pcre2)
  (export
    ;; Compilation and matching
    pcre2-compile pcre2-free!
    pcre2-search pcre2-match-all
    ;; Match result accessors
    pcre2-match-result? pcre2-match-result-start pcre2-match-result-end
    pcre2-match-substring
    ;; BRE/ERE conversion
    bre->ere
    ;; Flag constants
    PCRE2_CASELESS PCRE2_MULTILINE PCRE2_DOTALL PCRE2_UTF)

  (import (chezscheme))

  ;;; Constants
  (define PCRE2_CASELESS    #x00000008)
  (define PCRE2_MULTILINE   #x00000400)
  (define PCRE2_DOTALL      #x00000020)
  (define PCRE2_UTF         #x00080000)
  (define PCRE2_ZERO_TERMINATED (- (expt 2 64) 1))

  ;; Load shared library
  (define init (begin (load-shared-object "libpcre2-8.so") #t))

  ;;; FFI declarations
  (define c-pcre2-compile
    (foreign-procedure "pcre2_compile_8"
      (string size_t unsigned-32 uptr uptr uptr) uptr))

  (define c-pcre2-match-data-create
    (foreign-procedure "pcre2_match_data_create_from_pattern_8"
      (uptr uptr) uptr))

  (define c-pcre2-match
    (foreign-procedure "pcre2_match_8"
      (uptr string size_t size_t unsigned-32 uptr uptr) int))

  (define c-pcre2-get-ovector-pointer
    (foreign-procedure "pcre2_get_ovector_pointer_8"
      (uptr) uptr))

  (define c-pcre2-get-ovector-count
    (foreign-procedure "pcre2_get_ovector_count_8"
      (uptr) unsigned-32))

  (define c-pcre2-match-data-free
    (foreign-procedure "pcre2_match_data_free_8"
      (uptr) void))

  (define c-pcre2-code-free
    (foreign-procedure "pcre2_code_free_8"
      (uptr) void))

  ;;; Match result record: (start . end) byte positions in subject
  (define-record-type pcre2-match-result
    (fields start end subject))

  (define (pcre2-match-substring m)
    (substring (pcre2-match-result-subject m)
               (pcre2-match-result-start m)
               (pcre2-match-result-end m)))

  ;;; Compile a pattern string with flags. Returns compiled code pointer.
  ;;; Signals error on compilation failure.
  (define (pcre2-compile pattern flags)
    (let ([errcode-buf (foreign-alloc 4)]
          [erroff-buf  (foreign-alloc 8)])
      (let ([code (c-pcre2-compile
                    pattern
                    (string-length pattern)
                    flags
                    errcode-buf
                    erroff-buf
                    0)])
        (let ([off (foreign-ref 'size_t erroff-buf 0)])
          (foreign-free errcode-buf)
          (foreign-free erroff-buf)
          (when (= code 0)
            (error 'pcre2-compile
                   (format "failed to compile regex at offset ~a" off)
                   pattern))
          code))))

  ;;; Free a compiled regex
  (define (pcre2-free! code)
    (c-pcre2-code-free code))

  ;;; Search for first match in subject starting at offset.
  ;;; Returns pcre2-match-result or #f.
  (define (pcre2-search rx subject start)
    (let* ([len (string-length subject)]
           [md (c-pcre2-match-data-create rx 0)])
      (let ([rc (c-pcre2-match rx subject len start 0 md 0)])
        (if (< rc 0)
          (begin (c-pcre2-match-data-free md) #f)
          (let* ([ov-ptr (c-pcre2-get-ovector-pointer md)]
                 [s (foreign-ref 'size_t ov-ptr 0)]
                 [e (foreign-ref 'size_t ov-ptr 8)])
            (c-pcre2-match-data-free md)
            (make-pcre2-match-result s e subject))))))

  ;;; Find all non-overlapping matches in subject.
  ;;; Returns list of pcre2-match-result in order.
  (define (pcre2-match-all rx subject)
    (let ([len (string-length subject)])
      (let loop ([pos 0] [acc '()])
        (if (> pos len)
          (reverse acc)
          (let ([m (pcre2-search rx subject pos)])
            (if (not m)
              (reverse acc)
              (let ([s (pcre2-match-result-start m)]
                    [e (pcre2-match-result-end m)])
                ;; Advance past match; if zero-length, advance by 1
                (loop (if (= s e) (+ e 1) e)
                      (cons m acc)))))))))

  ;;; Convert POSIX BRE pattern to ERE (which PCRE2 understands).
  ;;; BRE: \( \) \{ \} \+ \? \| are special; bare versions are literal.
  ;;; ERE: ( ) { } + ? | are special; \-prefixed are literal.
  (define (bre->ere pattern)
    (let ([len (string-length pattern)])
      (let loop ([i 0] [acc '()])
        (if (>= i len)
          (list->string (reverse acc))
          (let ([c (string-ref pattern i)])
            (if (and (char=? c #\\) (< (+ i 1) len))
              (let ([next (string-ref pattern (+ i 1))])
                (case next
                  ;; BRE \( \) → ERE ( )
                  [(#\( #\)) (loop (+ i 2) (cons next acc))]
                  ;; BRE \{ \} → ERE { }
                  [(#\{ #\}) (loop (+ i 2) (cons next acc))]
                  ;; BRE \+ \? \| → ERE + ? |
                  [(#\+ #\? #\|) (loop (+ i 2) (cons next acc))]
                  ;; BRE \1-\9 backrefs — pass through
                  [(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   (loop (+ i 2) (cons next (cons #\\ acc)))]
                  ;; Other \X — pass through
                  [else (loop (+ i 2) (cons next (cons #\\ acc)))]))
              ;; Bare ( ) { } + ? | in BRE are literal — escape them
              (case c
                [(#\( #\) #\{ #\} #\+ #\? #\|)
                 (loop (+ i 1) (cons c (cons #\\ acc)))]
                [else
                 (loop (+ i 1) (cons c acc))])))))))

  ) ;; end library

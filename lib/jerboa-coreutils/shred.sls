#!chezscheme
;;; shred.sls -- Overwrite a file to hide its contents

(library (jerboa-coreutils shred)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
)
          (except (jerboa core) random-bytes)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (std cli getopt)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (define _load-ffi (begin (load-shared-object #f) (void)))
  (define ffi-file-size (foreign-procedure "coreutils_file_size" (string) long-long))

  ;; Get file size via FFI stat
  (def (get-file-size path)
    (ffi-file-size path))

  ;; Generate a bytevector of cryptographically secure random bytes
  (def (random-bytes n)
    (secure-random-bytes n))

  ;; Generate a bytevector of zero bytes
  (def (zero-bytes n)
    (make-bytevector n 0))

  ;; Write overwrite pass to a file
  (def (overwrite-pass path file-size generator verbose pass-label)
    (when verbose
      (eprintf "shred: ~a: pass ~a\n" path pass-label))
    (with-catch
      (lambda (e)
        (warn "~a: cannot overwrite: ~a" path (error-message e)))
      (lambda ()
        (let ((port (open-file-output-port path (file-options no-create no-truncate))))
          (let* ((buf-size 65536)
                 (remaining file-size))
            (let loop ((left remaining))
              (when (> left 0)
                (let* ((chunk-size (min buf-size left))
                       (buf (generator chunk-size)))
                  (put-bytevector port buf 0 chunk-size)
                  (loop (- left chunk-size))))))
          (flush-output-port port)
          (close-port port)))))

  ;; Shred a single file
  (def (shred-file path passes zero-pass remove verbose exact)
    (let ((file-size (get-file-size path)))
      (when (< file-size 0)
        (warn "~a: No such file or directory" path))
      (when (>= file-size 0)
        ;; Random data passes
        (let loop ((i 1))
          (when (<= i passes)
            (overwrite-pass path file-size random-bytes verbose
                           (string-append (number->string i) "/" (number->string passes)
                                          " (random)"))
            (loop (+ i 1))))
        ;; Zero pass
        (when zero-pass
          (overwrite-pass path file-size zero-bytes verbose
                         (string-append (number->string (+ passes 1)) "/"
                                        (number->string (+ passes 1)) " (000000)")))
        ;; Remove file
        (when remove
          (with-catch
            (lambda (e)
              (warn "~a: cannot remove: ~a" path (error-message e)))
            (lambda ()
              (delete-file path)
              (when verbose
                (eprintf "shred: ~a: removed\n" path))))))))

  (def (main . args)
    (parameterize ((program-name "shred"))
      (call-with-getopt
        (lambda (_ opt)
            (let* ((passes (if (hash-get opt 'iterations)
                             (let ((n (string->number (hash-ref opt 'iterations))))
                               (if n (inexact->exact n) 3))
                             3))
                   (files (hash-ref opt 'rest)))
              (when (null? files)
                (die "missing file operand"))
              (for-each
                (lambda (f)
                  (shred-file f passes (hash-get opt 'zero) (hash-get opt 'remove) (hash-get opt 'verbose) (hash-get opt 'exact)))
                files)))
        args
        'program: "shred"
        'help: "Overwrite the specified FILE(s) repeatedly, in order to make it harder for even very expensive hardware probing to recover the data."
        (option 'iterations "-n" "--iterations"
          'help: "overwrite N times instead of the default (3)"
          'default: #f)
        (flag 'zero "-z" "--zero"
          'help: "add a final overwrite with zeros to hide shredding")
        (flag 'remove "-u" "--remove"
          'help: "deallocate and remove file after overwriting")
        (flag 'verbose "-v" "--verbose"
          'help: "show progress")
        (flag 'exact "-x" "--exact"
          'help: "do not round file sizes up to the next full block")
        (rest-arguments 'rest))))

  ) ;; end library

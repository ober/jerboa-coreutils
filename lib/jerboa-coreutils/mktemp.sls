#!chezscheme
;;; mktemp.sls -- Create temporary files/directories

(library (jerboa-coreutils mktemp)
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

  (def (expand-template template)
    ;; Find the X's and replace with random chars
    (let* ((len (string-length template))
           (result (string-copy template))
           (chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
      (let loop ((i 0))
        (when (< i len)
          (when (eqv? (string-ref result i) #\X)
            (string-set! result i
              (string-ref chars (random-integer (string-length chars)))))
          (loop (+ i 1))))
      result))

  (def (count-trailing-x template)
    (let loop ((i (- (string-length template) 1)) (count 0))
      (if (and (>= i 0) (eqv? (string-ref template i) #\X))
        (loop (- i 1) (+ count 1))
        count)))

  (def (main . args)
    (parameterize ((program-name "mktemp"))
      (call-with-getopt
        (lambda (_ opt)
            (let* ((template (if (pair? (hash-ref opt 'rest))
                               (car (hash-ref opt 'rest))
                               "tmp.XXXXXXXXXX"))
                   (tmpdir (or (hash-get opt 'tmpdir)
                               (getenv "TMPDIR" "/tmp")))
                   (x-count (count-trailing-x template)))
              (when (< x-count 3)
                (die "too few X's in template '~a'" template))
              ;; Try up to 100 times
              (let loop ((attempts 0))
                (when (>= attempts 100)
                  (die "failed to create ~a after 100 attempts"
                    (if (hash-get opt 'directory) "directory" "file")))
                (let* ((name (expand-template template))
                       (path (if (string-index name #\/)
                               name
                               (string-append tmpdir "/" name))))
                  (with-catch
                    (lambda (e)
                      (loop (+ attempts 1)))
                    (lambda ()
                      (if (hash-get opt 'directory)
                        (begin
                          (create-directory path)
                          (displayln path))
                        (begin
                          ;; Create file exclusively
                          (let ((port (open-output-file path)))
                            (close-port port))
                          (displayln path)))))))))
        args
        'program: "mktemp"
        'help: "Create a temporary file or directory, safely, and print its name."
        (flag 'directory "-d" "--directory"
          'help: "create a directory, not a file")
        (flag 'quiet "-q" "--quiet"
          'help: "suppress diagnostics about file/dir-creation failure")
        (flag 'dry-run "-u" "--dry-run"
          'help: "do not create anything; merely print a name (unsafe)")
        (option 'tmpdir "-p" "--tmpdir"
          'help: "interpret TEMPLATE relative to DIR"
          'default: #f)
        (option 'suffix "--suffix"
          'help: "append SUFF to TEMPLATE"
          'default: #f)
        (rest-arguments 'rest))))

  (def (string-index str ch)
    (let loop ((i 0))
      (cond
        ((>= i (string-length str)) #f)
        ((eqv? (string-ref str i) ch) i)
        (else (loop (+ i 1))))))

  ) ;; end library

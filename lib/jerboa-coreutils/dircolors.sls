#!chezscheme
;;; dircolors.sls -- Output commands to set LS_COLORS

(library (jerboa-coreutils dircolors)
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

  ;; Default LS_COLORS database (a subset of GNU dircolors defaults)
  (define *default-database*
    '(("RESET"        . "rs=0")
      ("DIR"          . "di=01;34")
      ("LINK"         . "ln=01;36")
      ("MULTIHARDLINK". "mh=00")
      ("FIFO"         . "pi=40;33")
      ("SOCK"         . "so=01;35")
      ("DOOR"         . "do=01;35")
      ("BLK"          . "bd=40;33;01")
      ("CHR"          . "cd=40;33;01")
      ("ORPHAN"       . "or=40;31;01")
      ("MISSING"      . "mi=00")
      ("SETUID"       . "su=37;41")
      ("SETGID"       . "sg=30;43")
      ("CAPABILITY"   . "ca=30;41")
      ("STICKY_OTHER_WRITABLE" . "tw=30;42")
      ("OTHER_WRITABLE". "ow=34;42")
      ("STICKY"       . "st=37;44")
      ("EXEC"         . "ex=01;32")
      ("*.tar"  . "*.tar=01;31")  ("*.tgz"  . "*.tgz=01;31")
      ("*.arc"  . "*.arc=01;31")  ("*.arj"  . "*.arj=01;31")
      ("*.taz"  . "*.taz=01;31")  ("*.lha"  . "*.lha=01;31")
      ("*.lz4"  . "*.lz4=01;31")  ("*.lzh"  . "*.lzh=01;31")
      ("*.lzma" . "*.lzma=01;31") ("*.tlz"  . "*.tlz=01;31")
      ("*.txz"  . "*.txz=01;31")  ("*.tzo"  . "*.tzo=01;31")
      ("*.t7z"  . "*.t7z=01;31")  ("*.zip"  . "*.zip=01;31")
      ("*.z"    . "*.z=01;31")    ("*.dz"   . "*.dz=01;31")
      ("*.gz"   . "*.gz=01;31")   ("*.lrz"  . "*.lrz=01;31")
      ("*.lz"   . "*.lz=01;31")   ("*.lzo"  . "*.lzo=01;31")
      ("*.xz"   . "*.xz=01;31")   ("*.zst"  . "*.zst=01;31")
      ("*.tzst" . "*.tzst=01;31") ("*.bz2"  . "*.bz2=01;31")
      ("*.bz"   . "*.bz=01;31")   ("*.tbz"  . "*.tbz=01;31")
      ("*.tbz2" . "*.tbz2=01;31") ("*.tz"   . "*.tz=01;31")
      ("*.deb"  . "*.deb=01;31")  ("*.rpm"  . "*.rpm=01;31")
      ("*.jar"  . "*.jar=01;31")  ("*.war"  . "*.war=01;31")
      ("*.ear"  . "*.ear=01;31")  ("*.sar"  . "*.sar=01;31")
      ("*.rar"  . "*.rar=01;31")  ("*.alz"  . "*.alz=01;31")
      ("*.ace"  . "*.ace=01;31")  ("*.zoo"  . "*.zoo=01;31")
      ("*.cpio" . "*.cpio=01;31") ("*.7z"   . "*.7z=01;31")
      ("*.rz"   . "*.rz=01;31")   ("*.cab"  . "*.cab=01;31")
      ("*.wim"  . "*.wim=01;31")  ("*.swm"  . "*.swm=01;31")
      ("*.dwm"  . "*.dwm=01;31")  ("*.esd"  . "*.esd=01;31")
      ("*.jpg"  . "*.jpg=01;35")  ("*.jpeg" . "*.jpeg=01;35")
      ("*.gif"  . "*.gif=01;35")  ("*.bmp"  . "*.bmp=01;35")
      ("*.png"  . "*.png=01;35")  ("*.svg"  . "*.svg=01;35")
      ("*.tif"  . "*.tif=01;35")  ("*.tiff" . "*.tiff=01;35")
      ("*.mov"  . "*.mov=01;35")  ("*.mpg"  . "*.mpg=01;35")
      ("*.mpeg" . "*.mpeg=01;35") ("*.mkv"  . "*.mkv=01;35")
      ("*.webm" . "*.webm=01;35") ("*.mp4"  . "*.mp4=01;35")
      ("*.avi"  . "*.avi=01;35")  ("*.flv"  . "*.flv=01;35")
      ("*.wmv"  . "*.wmv=01;35")  ("*.ogv"  . "*.ogv=01;35")
      ("*.aac"  . "*.aac=00;36")  ("*.au"   . "*.au=00;36")
      ("*.flac" . "*.flac=00;36") ("*.m4a"  . "*.m4a=00;36")
      ("*.mid"  . "*.mid=00;36")  ("*.midi" . "*.midi=00;36")
      ("*.mp3"  . "*.mp3=00;36")  ("*.ogg"  . "*.ogg=00;36")
      ("*.wav"  . "*.wav=00;36")  ("*.opus" . "*.opus=00;36")))

  ;; Build LS_COLORS value from database
  (def (build-ls-colors db)
    (let loop ((entries db) (parts '()))
      (if (null? entries)
        (string-join-colon (reverse parts))
        (loop (cdr entries) (cons (cdr (car entries)) parts)))))

  (def (string-join-colon strs)
    (if (null? strs) ""
      (let loop ((rest (cdr strs)) (acc (car strs)))
        (if (null? rest) acc
          (loop (cdr rest) (string-append acc ":" (car rest)))))))

  ;; Print database in dircolors format
  (def (print-database db)
    (displayln "# Configuration file for dircolors, a utility to help you set the")
    (displayln "# LS_COLORS environment variable used by GNU ls with the --color option.")
    (displayln "")
    (for-each
      (lambda (entry)
        (let* ((val (cdr entry))
               (eq-pos (let loop ((i 0))
                         (if (>= i (string-length val)) #f
                           (if (eqv? (string-ref val i) #\=) i
                             (loop (+ i 1))))))
               (key (if eq-pos (substring val 0 eq-pos) val))
               (color (if eq-pos (substring val (+ eq-pos 1) (string-length val)) "0")))
          (if (and (> (string-length key) 0)
                   (eqv? (string-ref key 0) #\*))
            (displayln ".color " key " " color)
            (displayln "TERM xterm\n" key " " color))))
      db))

  ;; Read a dircolors file and build the database
  (def (read-dircolors-file path)
    (with-catch
      (lambda (e) *default-database*)
      (lambda ()
        (let* ((p (open-input-file path))
               (result
                (let loop ((acc '()))
                  (let ((line (get-line p)))
                    (if (eof-object? line)
                      (begin (close-port p) (reverse acc))
                      (let ((trimmed (string-trim line)))
                        (if (or (string=? trimmed "")
                                (eqv? (string-ref trimmed 0) #\#))
                          (loop acc)
                          (let ((parts (string-split-spaces trimmed)))
                            (if (>= (length parts) 2)
                              (loop (cons (cons (car parts)
                                                (string-append (car parts) "=" (cadr parts)))
                                          acc))
                              (loop acc))))))))))
          result))))

  (def (string-trim str)
    (let* ((len (string-length str))
           (start (let loop ((i 0))
                    (if (>= i len) i
                      (if (char-whitespace? (string-ref str i)) (loop (+ i 1)) i))))
           (end (let loop ((i (- len 1)))
                  (if (< i start) start
                    (if (char-whitespace? (string-ref str i)) (loop (- i 1)) (+ i 1))))))
      (substring str start end)))

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

  (def (main . args)
    (parameterize ((program-name "dircolors"))
      (call-with-getopt
        (lambda (_ opt)
          (let* ((db (if (pair? (hash-ref opt 'rest))
                       (read-dircolors-file (car (hash-ref opt 'rest)))
                       *default-database*))
                 (ls-colors (build-ls-colors db)))
            (cond
              ((hash-get opt 'print-database)
               (print-database db))
              ((hash-get opt 'csh)
               (displayln "setenv LS_COLORS '" ls-colors "'")
               (displayln "setenv LS_OPTIONS '--color=auto'"))
              (else
               ;; Default: Bourne shell output
               (displayln "LS_COLORS='" ls-colors "'")
               (displayln "export LS_COLORS")))))
        args
        'program: "dircolors"
        'help: "Output commands to set the LS_COLORS environment variable."
        (flag 'bourne "-b" "--bourne-shell"
          'help: "output Bourne shell code to set LS_COLORS")
        (flag 'csh "-c" "--c-shell"
          'help: "output C shell code to set LS_COLORS")
        (flag 'print-database "-p" "--print-database"
          'help: "print the byte counts")
        (rest-arguments 'rest))))

  ) ;; end library

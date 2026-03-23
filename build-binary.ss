#!chezscheme
;; build-binary.ss — Build the jerboa-coreutils multi-call native binary.
;;
;; Usage: cd jerboa-coreutils && make binary
;;        (or: scheme --libdirs lib:~/mine/jerboa/lib -q < build-binary.ss)
;;
;; Produces: bin/jerboa-coreutils  (self-contained ELF, ~30MB)
;;           bin/ls, bin/cat, ...  (symlinks to bin/jerboa-coreutils)
;;
;; All 108 coreutils + shared libs + Chez boot files are embedded as C byte
;; arrays. The resulting binary needs no external files or Chez installation.

(import (chezscheme))

;; --- Helper: embed a binary file as a C header ---
(define (file->c-header input-path output-path array-name size-name)
  (let* ((port (open-file-input-port input-path))
         (data (get-bytevector-all port))
         (size (bytevector-length data)))
    (close-port port)
    (call-with-output-file output-path
      (lambda (out)
        (fprintf out "/* Auto-generated from ~a — do not edit */\n" input-path)
        (fprintf out "static const unsigned char ~a[] = {\n" array-name)
        (let loop ((i 0))
          (when (< i size)
            (when (= 0 (modulo i 16)) (fprintf out "  "))
            (fprintf out "0x~2,'0x" (bytevector-u8-ref data i))
            (when (< (+ i 1) size) (fprintf out ","))
            (when (= 15 (modulo i 16)) (fprintf out "\n"))
            (loop (+ i 1))))
        (fprintf out "\n};\n")
        (fprintf out "static const unsigned int ~a = ~a;\n" size-name size))
      'replace)
    (printf "  ~a: ~a bytes\n" output-path size)))

;; --- Locate Chez install directory ---
(define chez-dir
  (or (getenv "CHEZ_DIR")
      (let* ((home (getenv "HOME"))
             (lib-dir (format "~a/.local/lib" home))
             (mt (symbol->string (machine-type)))
             (csv-dir
               (let lp ((dirs (guard (e (#t '()))
                                (directory-list lib-dir))))
                 (cond
                   ((null? dirs) #f)
                   ((and (> (string-length (car dirs)) 3)
                         (string=? "csv" (substring (car dirs) 0 3)))
                    (format "~a/~a/~a" lib-dir (car dirs) mt))
                   (else (lp (cdr dirs)))))))
        (and csv-dir
             (file-exists? (format "~a/main.o" csv-dir))
             csv-dir))))

(unless chez-dir
  (display "Error: Cannot find Chez install dir. Set CHEZ_DIR.\n")
  (exit 1))

(define jerboa-dir
  (or (getenv "JERBOA_DIR")
      (format "~a/mine/jerboa/lib" (getenv "HOME"))))

(define project-dir
  (or (getenv "PROJECT_DIR")
      (let* ((p (car (command-line)))
             ;; If called as scheme ... < build-binary.ss, use cwd
             (cwd (guard (e (#t ".")) (current-directory))))
        cwd)))

(define lib-dir (format "~a/lib" project-dir))
(define bin-dir (format "~a/bin" project-dir))
(define support-dir (format "~a/support" project-dir))

(printf "Chez dir:    ~a\n" chez-dir)
(printf "Jerboa dir:  ~a\n" jerboa-dir)
(printf "Project dir: ~a\n" project-dir)
(printf "Lib dir:     ~a\n" lib-dir)

;; --- Step 1: Compile all libraries + dispatch program (with WPO) ---
(printf "\n[1/5] Compiling all libraries and dispatch program...\n")
(parameterize ([compile-imported-libraries #t]
               [optimize-level 2]
               [generate-inspector-information #f]
               [debug-level 0]
               [generate-wpo-files #t])
  (library-directories (list lib-dir jerboa-dir))
  (compile-program (format "~a/dispatch.ss" project-dir)))

;; --- Step 2: Whole-program optimization ---
(printf "[2/5] Running whole-program optimization...\n")
(let ((missing (compile-whole-program
                 (format "~a/dispatch.wpo" project-dir)
                 (format "~a/dispatch-all.so" project-dir))))
  (unless (null? missing)
    (printf "  WPO: ~a libraries not incorporated (missing .wpo):\n" (length missing))
    (for-each (lambda (lib) (printf "    ~a\n" lib)) missing)))

;; --- Helper: collect library .so files in dependency order ---
(define (lib-so base)
  (let ((path (format "~a/~a.so" jerboa-dir base)))
    (if (file-exists? path)
      path
      (begin (printf "  WARNING: ~a not found\n" path) #f))))

(define (cu-so name)
  (let ((path (format "~a/jerboa-coreutils/~a.so" lib-dir name)))
    (if (file-exists? path)
      path
      (begin (printf "  WARNING: ~a not found\n" path) #f))))

;; All 108 utility names
(define *utils*
  '(true false yes echo printenv sleep whoami logname pwd
    basename dirname link unlink hostname nproc tty sync
    cat head tail wc tee tac nl fold expand unexpand
    cut paste join comm sort uniq tr numfmt
    mkdir rmdir mktemp touch ln readlink realpath
    truncate uname arch id groups seq factor
    env timeout kill nice nohup shuf base64 base32
    tsort hostid users uptime chroot mkfifo mknod
    pathchk fmt printf cksum md5sum sha1sum sha256sum sha512sum sha224sum sha384sum
    chmod chown chgrp stat du df date expr test who split dircolors
    install shred pinky basenc b2sum sum od csplit pr chcon runcon ptx
    cp mv rm ls dd stty stdbuf dir vdir rev top))

;; Collect .so files for the boot file (dependency order)
(define boot-libs
  (filter values
    (append
      ;; Jerboa core runtime
      (map lib-so '("jerboa/core"
                    "jerboa/runtime"
                    "std/error"
                    "std/format"
                    "std/sugar"
                    "std/misc/string"
                    "std/misc/list"
                    "std/misc/alist"
                    "std/misc/process"
                    "std/misc/ports"
                    "std/misc/thread"
                    "std/sort"
                    "std/crypto/digest"
                    "std/os/path"
                    "std/os/signal"
                    "std/os/temporaries"
                    "std/srfi/srfi-13"
                    "std/text/base64"
                    "std/misc/terminal"))
      ;; getopt (compiled in step 1)
      (list (let ((p (format "~a/std/cli/getopt.so" jerboa-dir)))
              (if (file-exists? p) p
                (begin (printf "  WARNING: getopt.so not found\n") #f))))
      ;; coreutils common modules
      (list (cu-so "common")
            (cu-so "common/version")
            (cu-so "common/security"))
      ;; All 108 utilities
      (map (lambda (u) (cu-so (symbol->string u))) *utils*))))

;; --- Step 3: Create libs-only boot file ---
(printf "[3/5] Creating boot file with ~a libraries...\n" (length boot-libs))
(apply make-boot-file
  (format "~a/coreutils.boot" project-dir)
  '("scheme" "petite")
  boot-libs)

;; --- Step 4: Embed as C headers ---
(printf "[4/5] Embedding boot files and dispatch program as C headers...\n")
(file->c-header
  (format "~a/dispatch-all.so" project-dir)
  (format "~a/cu_program.h" support-dir)
  "cu_program_data" "cu_program_size")
(file->c-header
  (format "~a/petite.boot" chez-dir)
  (format "~a/cu_petite.h" support-dir)
  "cu_petite_data" "cu_petite_size")
(file->c-header
  (format "~a/scheme.boot" chez-dir)
  (format "~a/cu_scheme.h" support-dir)
  "cu_scheme_data" "cu_scheme_size")
(file->c-header
  (format "~a/coreutils.boot" project-dir)
  (format "~a/cu_boot.h" support-dir)
  "cu_boot_data" "cu_boot_size")

;; --- Step 5: Compile C and link ---
(printf "[5/5] Compiling C and linking binary...\n")
(let ((inc (format "-I~a -I~a" chez-dir support-dir)))
  ;; coreutils-main.c
  (let ((cmd (format "gcc -c -O2 -o ~a/coreutils-main.o ~a/coreutils-main.c ~a -Wall 2>&1"
                     support-dir support-dir inc)))
    (printf "  ~a\n" cmd)
    (unless (= 0 (system cmd))
      (display "Error: coreutils-main.c compilation failed\n") (exit 1)))
  ;; libcoreutils.c (static object for embedding in binary)
  (let ((cmd (format "gcc -c -O2 -fPIC -o ~a/libcoreutils-obj.o ~a/libcoreutils.c -Wall 2>&1"
                     support-dir support-dir)))
    (printf "  ~a\n" cmd)
    (unless (= 0 (system cmd))
      (display "Error: libcoreutils.c compilation failed\n") (exit 1))))

;; --- Link binary ---
(let ((cmd (format
             "gcc -rdynamic -o ~a/jerboa-coreutils ~a/coreutils-main.o ~a/libcoreutils-obj.o -L~a -lkernel -llz4 -lz -lm -ldl -lpthread -lncurses -Wl,-rpath,~a 2>&1"
             bin-dir support-dir support-dir chez-dir chez-dir)))
  (printf "  ~a\n" cmd)
  (unless (= 0 (system cmd))
    (display "Error: Link failed\n") (exit 1)))

;; --- Create symlinks ---
(printf "Creating symlinks...\n")
(for-each
  (lambda (u)
    (let* ((name (symbol->string u))
           (link (format "~a/~a" bin-dir name)))
      (when (file-exists? link) (delete-file link))
      (system (format "ln -sf jerboa-coreutils ~a" link))))
  *utils*)

;; --- Cleanup intermediate files ---
(printf "Cleaning up intermediate files...\n")
(for-each (lambda (f)
            (when (file-exists? f) (delete-file f)))
  (list (format "~a/dispatch.so" project-dir)
        (format "~a/dispatch.wpo" project-dir)
        (format "~a/dispatch-all.so" project-dir)
        (format "~a/coreutils.boot" project-dir)
        (format "~a/cu_program.h" support-dir)
        (format "~a/cu_petite.h" support-dir)
        (format "~a/cu_scheme.h" support-dir)
        (format "~a/cu_boot.h" support-dir)
        (format "~a/coreutils-main.o" support-dir)
        (format "~a/libcoreutils-obj.o" support-dir)))

(let ((size (file-length (open-file-input-port
                           (format "~a/jerboa-coreutils" bin-dir)))))
  (printf "\n========================================\n")
  (printf "Build complete!\n\n")
  (printf "  Binary: bin/jerboa-coreutils  (~a MB)\n"
    (quotient size (* 1024 1024)))
  (printf "  Symlinks: ~a commands\n\n" (length *utils*))
  (printf "Run: bin/ls -la /tmp\n"))

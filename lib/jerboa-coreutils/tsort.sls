#!chezscheme
;;; tsort.sls -- Topological sort

(library (jerboa-coreutils tsort)
  (export main)
  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name
            filter list-sort)
          (jerboa core)
          (only (std sugar) with-catch)
          (only (std format) eprintf format)
          (jerboa-coreutils common)
          (jerboa-coreutils common version))

  (def (main . args)
    (parameterize ((program-name "tsort"))
      (cond
        ((and (pair? args) (member (car args) '("--help" "-h")))
         (displayln "Usage: tsort [OPTION] [FILE]")
         (displayln "Write totally ordered list consistent with the partial ordering in FILE.")
         (displayln "With no FILE, or when FILE is -, read standard input."))
        ((and (pair? args) (member (car args) '("--version")))
         (version-info "tsort"))
        (else
         (let ((port (if (or (null? args)
                             (string=? (car args) "-"))
                       (current-input-port)
                       (with-catch
                         (lambda (e) (die "~a" (error-message e)))
                         (lambda () (open-input-file (car args)))))))
           ;; Read all tokens
           (let ((tokens (read-tokens port)))
             (when (odd? (length tokens))
               (warn "input contains an odd number of tokens"))
             ;; Build adjacency list
             (let-values (((adj all-nodes) (build-graph tokens)))
               (let ((result (topological-sort adj all-nodes)))
                 (for-each displayln result))))
           (when (and (pair? args) (not (string=? (car args) "-")))
             (close-port port)))))))

  (def (read-tokens port)
    (let loop ((acc '()))
      (let ((tok (read port)))
        (if (eof-object? tok)
          (reverse acc)
          (loop (cons (if (symbol? tok)
                        (symbol->string tok)
                        (if (number? tok)
                          (number->string tok)
                          (with-output-to-string (lambda () (write tok)))))
                      acc))))))

  (def (build-graph tokens)
    ;; Returns (values adjacency-hash all-nodes-list)
    (let ((adj (make-hash-table))
          (all-nodes (make-hash-table)))
      (let loop ((rest tokens))
        (when (>= (length rest) 2)
          (let ((from (car rest))
                (to (cadr rest)))
            (hash-put! all-nodes from #t)
            (hash-put! all-nodes to #t)
            (unless (string=? from to)
              (let ((edges (hash-ref adj from '())))
                (unless (member to edges)
                  (hash-put! adj from (cons to edges)))))
            (loop (cddr rest)))))
      (values adj (hash-keys all-nodes))))

  (def (topological-sort adj all-nodes)
    ;; Kahn's algorithm
    (let ((in-degree (make-hash-table))
          (result '()))
      ;; Initialize in-degrees
      (for-each (lambda (n) (hash-put! in-degree n 0)) all-nodes)
      ;; Count in-degrees
      (hash-for-each
        (lambda (from edges)
          (for-each
            (lambda (to)
              (hash-put! in-degree to (+ 1 (hash-ref in-degree to 0))))
            edges))
        adj)
      ;; Find nodes with in-degree 0
      (let ((queue (filter (lambda (n) (zero? (hash-ref in-degree n 0))) all-nodes)))
        ;; Sort for deterministic output
        (let loop ((q (list-sort string<? queue)) (result '()))
          (if (null? q)
            (let ((remaining (filter (lambda (n) (> (hash-ref in-degree n 0) 0)) all-nodes)))
              (if (null? remaining)
                (reverse result)
                ;; Cycle detected
                (begin
                  (for-each
                    (lambda (n)
                      (eprintf "~a: ~a\n" (program-name)
                        (string-append n ": input contains a loop")))
                    (list-sort string<? remaining))
                  (append (reverse result) (list-sort string<? remaining)))))
            (let ((node (car q))
                  (rest (cdr q)))
              (let ((new-q rest))
                (for-each
                  (lambda (to)
                    (let ((new-deg (- (hash-ref in-degree to 0) 1)))
                      (hash-put! in-degree to new-deg)
                      (when (zero? new-deg)
                        (set! new-q (cons to new-q)))))
                  (hash-ref adj node '()))
                (loop (list-sort string<? new-q) (cons node result)))))))))

  (def (filter pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
      (else (filter pred (cdr lst)))))

  (def (list-sort less? lst)
    (sort less? lst))

  ) ;; end library

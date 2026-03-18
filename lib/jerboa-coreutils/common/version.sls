#!chezscheme
;;; common/version.sls -- Version information

(library (jerboa-coreutils common version)
  (export
    coreutils-version
    version-info)

  (import (except (chezscheme)
            make-hash-table hash-table? iota 1+ 1-
            getenv path-extension path-absolute?
            thread? make-mutex mutex? mutex-name)
          (jerboa core))

  (define coreutils-version "0.1.0")

  ;; Print standard version output
  (define (version-info program)
    (displayln program " (jerboa-coreutils) " coreutils-version))

  ) ;; end library

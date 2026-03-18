#!chezscheme
(import (except (chezscheme) make-hash-table hash-table? iota 1+ 1- getenv path-extension path-absolute? thread? make-mutex mutex? mutex-name))
(import (jerboa-coreutils numfmt))
(apply main (cdr (command-line)))

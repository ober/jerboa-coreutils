#!chezscheme
(import (except (chezscheme) make-hash-table hash-table? iota 1+ 1- getenv path-extension path-absolute? thread? make-mutex mutex? mutex-name))
(import (jerboa-coreutils base32))
(apply main (cdr (command-line)))

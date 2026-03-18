# jerboa-coreutils — Known Gaps & Issues

Status: 108/108 utilities ported (including `rev`). All load and run.
All items from the original gap list have been fixed. Remaining open items noted below.

---

## 1. Systemic Issues (affect many utilities)

### 1.1 Getopt does not support combined short flags/options

**Root cause:** Jerboa's `std/cli/getopt` treats `-la` as a single option name rather than
splitting it into `-l` and `-a`. Similarly `-n3` is not parsed as `-n 3`.

**Workaround:** Use space-separated form (`-l -a`, `-n 3`).

**Affected utilities (~85):** Virtually all that accept short options. Examples:
- `ls -la`, `ls -lh`, `ls -lR` — must use `ls -l -a`
- `head -n3`, `tail -n2`, `fold -w5`, `cut -d:` — must use `head -n 3`
- `sort -rn`, `sort -t:`, `wc -lw`, `nl -ba`, `expand -t4`
- `seq -s,`, `seq -f'%.2f'`

### ~~1.2 `--help` exits with code 1, not 0~~ **FIXED**

`call-with-getopt` in `getopt.sls` now intercepts `--help` before parsing and exits 0.

### 1.3 `--version` not universally registered

Only ~18 utilities register `--version`. The rest do not respond to it.

### ~~1.4 Compile warning from `std/sugar`~~ **FIXED**

The `with-catch` arity warning is suppressed via the `%apply1` indirection in
`~/mine/jerboa/lib/std/sugar.sls`. No warning appears at runtime.

---

## 2. Stdin / Binary Port Issues — ALL FIXED

### ~~2.1 Textual vs binary port mismatch~~ **FIXED**

All affected utilities now use `(standard-input-port)` for binary reads:
- `md5sum`, `sha1sum`, `sha224sum`, `sha256sum`, `sha384sum`, `sha512sum` — fixed
- `base64`, `base32` — fixed
- `cksum` — fixed

### ~~2.2 Digest functions return strings, not bytevectors~~ **FIXED**

All `*sum` utilities now use the hex string returned by digest functions directly,
without the broken `(bytevector->hex digest)` wrapper.

### ~~2.3 `base64` / `base32` completely broken~~ **FIXED**

- Encode: uses `(standard-input-port)` for stdin
- Decode: `write-u8vector` uses `(standard-output-port)` for binary output
- `basenc` decode output also fixed to use `(standard-output-port)`

---

## 3. FFI / Foreign Procedure Issues — ALL FIXED

### ~~3.1 Missing `load-shared-object` in `id` and `groups`~~ **FIXED**

Both now have `(define _load-ffi (begin (load-shared-object #f) (void)))`.

### ~~3.2 Broken `open-process-ports` usage in `readlink`~~ **FIXED**

`readlink` and `realpath` now use proper `let-values` destructuring for
`open-process-ports`' 4-value return.

---

## 4. Specific Utility Bugs — ALL FIXED

### ~~4.1 `link` — wrong `process` call arity~~ **FIXED**

`link.sls` now uses FFI `link(2)` syscall directly.

### ~~4.2 `env -` does not clear environment~~ **FIXED**

Uses FFI `unsetenv(3)` to properly unset environment variables.

### 4.3 `seq -f` format string incompatibility

`seq.sls` uses Chez `format` directives (`~`) but GNU seq expects `printf`-style `%`
format strings. Any `-f` argument will error. This is a non-trivial fix requiring
a printf-to-Scheme format translator.

### ~~4.4 `tee` cannot write to `/dev/null` or device files~~ **FIXED**

`tee.sls` uses `open-file-output-port` with `(file-options no-fail)`.

### ~~4.5 `dd conv=` options mostly unimplemented~~ **FIXED**

`dd.sls` now supports `conv=ucase`, `conv=lcase`, `conv=swab` in addition to `conv=notrunc`.

### 4.6 `csplit` pattern matching fragile

`csplit.sls`: passes raw regex to `grep -q` without `-e` flag. Patterns starting
with `-` are misinterpreted as grep options.

### ~~4.7 `tail -f` / `--follow` not implemented~~ **FIXED**

`tail.sls` now registers and implements `-f`/`--follow` with polling loop.

### ~~4.8 `touch -d` / `--date` not implemented~~ **FIXED**

`touch.sls` now registers `-d`/`--date` (falls back to current time when parsing fails).

### ~~4.9 `basenc` outputs extra trailing newline~~ **FIXED**

Output now ends with exactly one `\n`.

### ~~4.10 `printf %x` outputs uppercase hex~~ **FIXED**

`printf.sls` now outputs lowercase hex for `%x`.

### ~~4.11 `od` missing traditional short options~~ **FIXED**

`od.sls` now accepts `-x` (→ `-t x2`), `-c` (→ `-t c`), `-o` (→ `-t o2`), `-d` (→ `-t d2`)
via pre-processing in `main`.

### ~~4.12 `pr` missing `-h` short option for `--header`~~ **FIXED**

`pr.sls` now registers `"-h"` as the short form for `--header`.

---

## 5. Exit Code Issues — ALL FIXED

- ~~`cat`~~ — now exits 1 on file open errors
- ~~`head`~~ — now exits 1 on file open errors
- ~~`chown`~~ — now exits 1 on permission errors
- ~~`chgrp`~~ — now exits 1 on permission errors

---

## 6. Missing Utility — FIXED

- ~~`rev`~~ — implemented in `lib/jerboa-coreutils/rev.sls`

---

## 7. Not Yet Tested

The following areas have not been deeply validated:

- `ls -l` long-format column alignment and field accuracy
- `stat` output format strings
- `du` / `df` numeric accuracy at scale
- `test` / `[` full expression grammar
- `chmod` symbolic mode parsing (e.g., `u+x`, `go-w`)
- `chown user:group` colon-separated parsing
- `split` suffix generation and edge cases
- `shred` overwrite pattern correctness
- `install -m` mode handling
- `stty` terminal attribute manipulation
- `stdbuf` buffer mode enforcement
- `chroot`, `chcon`, `runcon` — require root / SELinux

---

## Summary

| Category | Status | Notes |
|----------|--------|-------|
| Getopt combined flags | Open | ~85 utils; workaround exists |
| `--help` exit code | **Fixed** | `call-with-getopt` intercepts `--help` → exit 0 |
| `--version` universally | Open | Only ~18 utils register it |
| Broken stdin/digest | **Fixed** | All `*sum`, `base64`, `base32`, `cksum` fixed |
| FFI load order | **Fixed** | `id`, `groups`, `link`, `env`, `test` fixed |
| `readlink`/`realpath` | **Fixed** | `let-values` for `open-process-ports` |
| `tee` device files | **Fixed** | `file-options no-fail` |
| `dd conv=` options | **Fixed** | `ucase`, `lcase`, `swab` added |
| `tail -f` | **Fixed** | Polling follow mode implemented |
| `touch -d` | **Fixed** | `-d`/`--date` option registered |
| `basenc` newline | **Fixed** | Single trailing `\n` |
| `printf %x` case | **Fixed** | Lowercase hex output |
| `od` short options | **Fixed** | `-x`, `-c`, `-o`, `-d` pre-processed |
| `pr -h` | **Fixed** | `-h` registered as `--header` short form |
| Exit codes | **Fixed** | `cat`, `head`, `chown`, `chgrp` exit 1 on error |
| `rev` utility | **Fixed** | Implemented from scratch |
| `sugar.sls` warning | **Fixed** | `%apply1` indirection suppresses arity warning |
| `seq -f` format | Open | Printf `%` vs Chez `~` format incompatibility |
| `csplit` pattern | Open | Missing `-e` flag to `grep` |

# jerboa-coreutils

GNU coreutils implemented in [Jerboa](https://github.com/jafourni/jerboa) — Gerbil Scheme syntax and APIs running on stock Chez Scheme.

This is a port of [gerbil-coreutils](https://github.com/jafourni/gerbil-coreutils) to the Jerboa platform, demonstrating that the same utility code works on an entirely different Scheme backend.

## Architecture

```
┌─────────────────────────────────────────┐
│  110 coreutils (true, cat, ls, ...)     │
│  .sls R6RS libraries using Jerboa APIs  │
└─────────────────────────────────────────┘
            ↓ imports
┌─────────────────────────────────────────┐
│  Jerboa: (jerboa core), (std/cli/getopt)│
│  Gerbil syntax: def, let-hash, match... │
│  Stdlib: format, sugar, sort, crypto... │
└─────────────────────────────────────────┘
            ↓ runs on
┌─────────────────────────────────────────┐
│  Stock Chez Scheme 10.x (unmodified)    │
└─────────────────────────────────────────┘
```

## Quick Start

```bash
# Prerequisites: Chez Scheme, gcc, jerboa installed at ~/mine/jerboa
make build

# Run utilities
bin/true
bin/echo "hello world"
echo "foo bar" | bin/cat -n
bin/seq 1 10
bin/ls -la
```

## Project Structure

```
jerboa-coreutils/
├── lib/jerboa-coreutils/       # 110 utility libraries (.sls)
│   ├── common.sls              # Shared error handling, exit codes
│   ├── common/io.sls           # File/line processing utilities
│   ├── common/version.sls      # Version information
│   ├── true.sls ... vdir.sls   # Individual utilities
├── bin/                         # Generated entry point scripts
├── support/
│   └── libcoreutils.c          # C shim for FFI (stat, ls, cp, etc.)
├── Makefile
└── README.md
```

## Utilities (110)

**Phase 1 - Basic:** true, false, yes, echo, printenv, sleep, whoami, logname, pwd, basename, dirname, link, unlink, hostname, nproc, tty, sync

**Phase 2 - Text:** cat, head, tail, wc, tee, tac, nl, fold, expand, unexpand

**Phase 3 - Advanced Text:** cut, paste, join, comm, sort, uniq, tr, numfmt

**Phase 4 - File Ops:** mkdir, rmdir, mktemp, touch, ln, readlink, realpath

**Phase 5-6 - Info:** truncate, uname, arch, id, groups

**Phase 7 - Transforms:** seq, factor

**Phase 8 - Process:** env, timeout, kill, nice, nohup

**Phase 9 - Encoding:** shuf, base64, base32

**Phase 10-13 - Misc:** tsort, hostid, users, uptime, chroot, mkfifo, mknod, pathchk, fmt, printf

**Phase 14 - Checksums:** cksum, md5sum, sha1sum, sha256sum, sha512sum, sha224sum, sha384sum

**Phase 15-18 - System:** chmod, chown, chgrp, stat, du, df, date, expr, test

**Phase 19-24 - Advanced:** who, split, dircolors, install, shred, pinky, basenc, b2sum, sum, od, csplit, pr, chcon, runcon, ptx, cp, mv, rm, ls, dd, stty, stdbuf, dir, vdir

## How It Differs from gerbil-coreutils

| Aspect | gerbil-coreutils | jerboa-coreutils |
|--------|-----------------|------------------|
| Backend | Gerbil compiler + Gambit VM | Chez Scheme 10.x |
| FFI | `begin-ffi` with inline C | C shim library + `foreign-procedure` |
| Build | `gerbil build` | `make build` (gcc + Chez) |
| Format | `.ss` Gerbil modules | `.sls` R6RS libraries |
| Getopt | 1-arg callback | 2-arg callback (cmd, hash) |

## License

Same as gerbil-coreutils.
# jerboa-coreutils

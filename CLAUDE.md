# jerboa-coreutils

## Build

- **Always use `gmake` on FreeBSD**, never `make`. The Makefile uses GNU make syntax (`$(CURDIR)`, `$(wildcard ...)`, `$<` in explicit rules) which is incompatible with FreeBSD's BSD make.
- Set `CHEZ_DIR` if Chez Scheme is not installed in `~/.local/lib/`. On FreeBSD: `CHEZ_DIR=/usr/local/lib/csv10.3.0/ta6fb gmake binary`

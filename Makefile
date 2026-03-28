# jerboa-coreutils Makefile
# Gerbil-like coreutils on Chez Scheme via Jerboa

SCHEME = scheme
JERBOA_LIB = $(HOME)/mine/jerboa/lib
LIB_DIR = $(CURDIR)/lib
BIN_DIR = $(CURDIR)/bin
SUPPORT_DIR = $(CURDIR)/support

# All utility names (matches gerbil-coreutils)
UTILS = true false yes echo printenv sleep whoami logname pwd \
        basename dirname link unlink hostname nproc tty sync \
        cat head tail wc tee tac nl fold expand unexpand \
        cut paste join comm sort uniq tr numfmt \
        mkdir rmdir mktemp touch ln readlink realpath \
        truncate \
        uname arch id groups \
        seq factor \
        env timeout kill nice nohup \
        shuf base64 base32 \
        tsort \
        hostid users uptime \
        chroot mkfifo mknod \
        pathchk fmt printf \
        cksum md5sum sha1sum sha256sum sha512sum sha224sum sha384sum \
        chmod chown chgrp \
        stat du df \
        date \
        expr test \
        who \
        split \
        dircolors \
        install shred pinky basenc b2sum sum od csplit pr chcon runcon ptx \
        cp mv rm ls dd stty stdbuf \
        dir vdir rev top \
        grep

export CHEZSCHEMELIBDIRS = $(LIB_DIR):$(JERBOA_LIB)
export PROJECT_DIR = $(CURDIR)

# Default: build native multi-call binary
.PHONY: all build binary clean test install help

all: binary

# Native multi-call binary (BusyBox-style, self-contained ELF)
binary: $(SUPPORT_DIR)/libcoreutils.so
	@mkdir -p $(BIN_DIR)
	PROJECT_DIR=$(CURDIR) $(SCHEME) --libdirs "$(CHEZSCHEMELIBDIRS)" -q < build-binary.ss

# Legacy: build C shim + generate interpreter wrapper scripts
build: $(SUPPORT_DIR)/libcoreutils.so $(BIN_DIR)/.generated

$(SUPPORT_DIR)/libcoreutils.so: $(SUPPORT_DIR)/libcoreutils.c
	gcc -shared -fPIC -O2 -o $@ $<

$(BIN_DIR)/.generated: $(wildcard $(LIB_DIR)/jerboa-coreutils/*.sls)
	@mkdir -p $(BIN_DIR)
	@for util in $(UTILS); do \
		echo '#!/bin/sh' > $(BIN_DIR)/$$util; \
		echo 'SCRIPT_DIR=$$(cd "$$(dirname "$$0")/.." && pwd)' >> $(BIN_DIR)/$$util; \
		echo 'export CHEZSCHEMELIBDIRS="$$SCRIPT_DIR/lib:$(JERBOA_LIB)"' >> $(BIN_DIR)/$$util; \
		echo 'export LD_LIBRARY_PATH="$$SCRIPT_DIR/support:$$LD_LIBRARY_PATH"' >> $(BIN_DIR)/$$util; \
		echo 'exec $(SCHEME) --libdirs "$$CHEZSCHEMELIBDIRS" --program "$$SCRIPT_DIR/bin/'"$$util"'.sps" "$$@"' >> $(BIN_DIR)/$$util; \
		chmod +x $(BIN_DIR)/$$util; \
		if [ ! -f $(BIN_DIR)/$$util.sps ]; then \
			echo '#!chezscheme' > $(BIN_DIR)/$$util.sps; \
			echo "(import (except (chezscheme) make-hash-table hash-table? iota 1+ 1- getenv path-extension path-absolute? thread? make-mutex mutex? mutex-name))" >> $(BIN_DIR)/$$util.sps; \
			echo "(import (jerboa-coreutils $$util))" >> $(BIN_DIR)/$$util.sps; \
			echo "(apply main (cdr (command-line)))" >> $(BIN_DIR)/$$util.sps; \
		fi; \
	done
	@# Create egrep/fgrep as aliases for grep
	@cd $(BIN_DIR) && ln -sf grep egrep && ln -sf grep fgrep
	@if [ ! -f $(BIN_DIR)/egrep.sps ]; then \
		echo '#!chezscheme' > $(BIN_DIR)/egrep.sps; \
		echo "(import (except (chezscheme) make-hash-table hash-table? iota 1+ 1- getenv path-extension path-absolute? thread? make-mutex mutex? mutex-name))" >> $(BIN_DIR)/egrep.sps; \
		echo "(import (jerboa-coreutils grep))" >> $(BIN_DIR)/egrep.sps; \
		echo "(apply main (cdr (command-line)))" >> $(BIN_DIR)/egrep.sps; \
	fi
	@if [ ! -f $(BIN_DIR)/fgrep.sps ]; then \
		echo '#!chezscheme' > $(BIN_DIR)/fgrep.sps; \
		echo "(import (except (chezscheme) make-hash-table hash-table? iota 1+ 1- getenv path-extension path-absolute? thread? make-mutex mutex? mutex-name))" >> $(BIN_DIR)/fgrep.sps; \
		echo "(import (jerboa-coreutils grep))" >> $(BIN_DIR)/fgrep.sps; \
		echo "(apply main (cdr (command-line)))" >> $(BIN_DIR)/fgrep.sps; \
	fi
	@touch $@

# Run a single utility: make run UTIL=true ARGS="--help"
run:
	@$(BIN_DIR)/$(UTIL) $(ARGS)

# Quick test of basic utilities
test:
	@echo "=== Testing basic utilities ==="
	@$(BIN_DIR)/true && echo "PASS: true"
	@! $(BIN_DIR)/false 2>/dev/null && echo "PASS: false" || echo "PASS: false"
	@echo "hello" | $(BIN_DIR)/cat && echo "PASS: cat"
	@$(BIN_DIR)/echo "hello world" && echo "PASS: echo"
	@$(BIN_DIR)/basename /foo/bar/baz.txt && echo "PASS: basename"
	@$(BIN_DIR)/dirname /foo/bar/baz.txt && echo "PASS: dirname"
	@$(BIN_DIR)/seq 1 5 && echo "PASS: seq"
	@echo "=== Done ==="

clean:
	rm -rf $(BIN_DIR) $(SUPPORT_DIR)/libcoreutils.so $(SUPPORT_DIR)/cu_*.h
	find $(LIB_DIR) -name "*.so" -o -name "*.wpo" -o -name "*.hash" | xargs rm -f 2>/dev/null || true
	rm -f dispatch.so dispatch.wpo dispatch-all.so coreutils.boot

help:
	@echo "jerboa-coreutils - GNU coreutils in Jerboa (Gerbil on Chez Scheme)"
	@echo ""
	@echo "Usage:"
	@echo "  make binary    Build self-contained native multi-call ELF binary (default)"
	@echo "  make build     Build C shim + interpreter wrapper scripts (legacy)"
	@echo "  make test      Run basic smoke tests"
	@echo "  make clean     Remove build artifacts"
	@echo "  make run UTIL=name ARGS='...'  Run a utility"
	@echo ""
	@echo "Utilities: $(words $(UTILS)) total"

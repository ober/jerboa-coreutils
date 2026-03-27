#!/bin/bash
# Functional test suite for jgrep — compares output against GNU grep
# Usage: ./tests/grep/run-tests.sh
#
# Requires: GNU grep in PATH, jgrep built via `make build`

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
JGREP="$PROJECT_DIR/bin/grep"
export LD_LIBRARY_PATH="$PROJECT_DIR/support:${LD_LIBRARY_PATH:-}"

# Test data directory
TDATA="$SCRIPT_DIR/data"
mkdir -p "$TDATA"

PASS=0
FAIL=0
SKIP=0
ERRORS=""

# ── Helpers ──────────────────────────────────────────────────────────

pass() { PASS=$((PASS+1)); }
fail() {
  FAIL=$((FAIL+1))
  ERRORS="${ERRORS}FAIL: $1\n"
  if [ "${VERBOSE:-}" = "1" ]; then
    echo "  EXPECTED: $(echo "$2" | head -5)"
    echo "  ACTUAL:   $(echo "$3" | head -5)"
  fi
}
skip() { SKIP=$((SKIP+1)); echo "  SKIP: $1"; }

# Compare jgrep vs GNU grep on same input.
# Usage: compare_pipe "description" "input" [grep-args...]
compare_pipe() {
  local desc="$1"; shift
  local input="$1"; shift

  local exp_out exp_rc act_out act_rc
  exp_out=$(printf '%s' "$input" | grep "$@" 2>/dev/null) || true
  exp_rc=$(printf '%s' "$input" | grep "$@" >/dev/null 2>&1; echo $?) || true
  act_out=$(printf '%s' "$input" | "$JGREP" "$@" 2>/dev/null) || true
  act_rc=$(printf '%s' "$input" | "$JGREP" "$@" >/dev/null 2>&1; echo $?) || true

  if [ "$exp_out" = "$act_out" ] && [ "$exp_rc" = "$act_rc" ]; then
    pass
  else
    fail "$desc" "out=[$exp_out] rc=$exp_rc" "out=[$act_out] rc=$act_rc"
    if [ "${VERBOSE:-}" = "1" ]; then
      if [ "$exp_out" != "$act_out" ]; then
        echo "    OUTPUT DIFF:"
        diff <(echo "$exp_out") <(echo "$act_out") | head -10 || true
      fi
      if [ "$exp_rc" != "$act_rc" ]; then
        echo "    EXIT CODE: expected=$exp_rc actual=$act_rc"
      fi
    fi
  fi
}

# Compare on files.
# Usage: compare_file "description" [grep-args...] file1 file2 ...
compare_file() {
  local desc="$1"; shift

  local exp_out exp_rc act_out act_rc
  exp_out=$(grep "$@" 2>/dev/null) || true
  exp_rc=$(grep "$@" >/dev/null 2>&1; echo $?) || true
  act_out=$("$JGREP" "$@" 2>/dev/null) || true
  act_rc=$("$JGREP" "$@" >/dev/null 2>&1; echo $?) || true

  if [ "$exp_out" = "$act_out" ] && [ "$exp_rc" = "$act_rc" ]; then
    pass
  else
    fail "$desc" "out=[$exp_out] rc=$exp_rc" "out=[$act_out] rc=$act_rc"
    if [ "${VERBOSE:-}" = "1" ]; then
      diff <(echo "$exp_out") <(echo "$act_out") | head -10 || true
    fi
  fi
}

# ── Create test fixtures ─────────────────────────────────────────────

cat > "$TDATA/lines.txt" << 'EOF'
hello world
foo bar
HELLO WORLD
hello foo
baz quux
line with 123 numbers
the end
EOF

cat > "$TDATA/names.txt" << 'EOF'
Alice Smith
Bob Jones
alice wonderland
ALICE UPPERCASE
Charlie Brown
alice smith duplicate
EOF

cat > "$TDATA/code.txt" << 'EOF'
int main(int argc, char *argv[]) {
    printf("hello world\n");
    return 0;
}

void foo_bar() {
    int x = 42;
    // comment with hello
}
EOF

cat > "$TDATA/empty.txt" <<< ""

cat > "$TDATA/patterns.txt" << 'EOF'
hello
foo
EOF

# Create directory structure for recursive tests
mkdir -p "$TDATA/tree/sub1" "$TDATA/tree/sub2" "$TDATA/tree/.hidden"
echo "match in root" > "$TDATA/tree/root.txt"
echo "match in sub1" > "$TDATA/tree/sub1/a.txt"
echo "no match here" > "$TDATA/tree/sub1/b.log"
echo "match in sub2" > "$TDATA/tree/sub2/c.txt"
echo "match hidden"  > "$TDATA/tree/.hidden/d.txt"

echo "=== jerboa-coreutils grep compatibility tests ==="
echo ""

# ── 1. Basic matching ────────────────────────────────────────────────

echo "--- Basic matching ---"

INPUT="hello world
foo bar
HELLO WORLD
hello foo
baz quux"

compare_pipe "basic match" "$INPUT" "hello"
compare_pipe "no match" "$INPUT" "zzz"
compare_pipe "empty pattern matches all" "$INPUT" ""
compare_pipe "dot metachar" "$INPUT" "h.llo"
compare_pipe "anchored start" "$INPUT" "^hello"
compare_pipe "anchored end" "$INPUT" 'world$'
compare_pipe "bracket class" "$INPUT" '[hH]ello'

# ── 2. Case sensitivity ─────────────────────────────────────────────

echo "--- Case sensitivity ---"

compare_pipe "-i ignore case" "$INPUT" -i "hello"
compare_pipe "-i no match" "$INPUT" -i "zzz"

# ── 3. Invert match ─────────────────────────────────────────────────

echo "--- Invert match ---"

compare_pipe "-v invert" "$INPUT" -v "hello"
compare_pipe "-v all match" "aaa" -v "aaa"
compare_pipe "-v -c count inverted" "$INPUT" -v -c "hello"

# ── 4. Count mode ───────────────────────────────────────────────────

echo "--- Count mode ---"

compare_pipe "-c count" "$INPUT" -c "hello"
compare_pipe "-c no match" "$INPUT" -c "zzz"
compare_pipe "-c -i count" "$INPUT" -c -i "hello"

# ── 5. Extended regex ───────────────────────────────────────────────

echo "--- Extended regex ---"

compare_pipe "-E alternation" "$INPUT" -E 'hello|baz'
compare_pipe "-E plus" "$INPUT" -E 'hel+'
compare_pipe "-E optional" "$INPUT" -E 'hel?lo'
compare_pipe "-E grouping" "$INPUT" -E '(hello|foo) (world|bar)'

# ── 6. Fixed strings ────────────────────────────────────────────────

echo "--- Fixed strings ---"

compare_pipe "-F literal dot" "foo.bar
fooXbar" -F "foo.bar"

compare_pipe "-F literal star" "foo*bar
foobar" -F "foo*bar"

compare_pipe "-F literal parens" "foo(bar)
foobar" -F "foo(bar)"

# ── 7. Word and line matching ───────────────────────────────────────

echo "--- Word and line matching ---"

compare_pipe "-w word" "foobar
foo bar
bar foo" -w "foo"

compare_pipe "-x full line" "$INPUT" -x "hello world"
compare_pipe "-x no match" "$INPUT" -x "hello"

# ── 8. Only matching ────────────────────────────────────────────────

echo "--- Only matching ---"

compare_pipe "-o only match" "$INPUT" -o -E "hello|foo"
compare_pipe "-o multiple per line" "abcabc" -o -E "abc"

# ── 9. Max count ────────────────────────────────────────────────────

echo "--- Max count ---"

compare_pipe "-m 1" "$INPUT" -m 1 "hello"
compare_pipe "-m 2" "$INPUT" -m 2 "hello"
compare_pipe "-m 0 (no output)" "$INPUT" -m 0 "hello"

# ── 10. Line numbers ────────────────────────────────────────────────

echo "--- Line numbers ---"

compare_pipe "-n line numbers" "$INPUT" -n "hello"
compare_pipe "-n -c count" "$INPUT" -n -c "hello"

# ── 11. Byte offset ─────────────────────────────────────────────────

echo "--- Byte offset ---"

compare_pipe "-b byte offset" "$INPUT" -b "hello"

# ── 12. Quiet mode ──────────────────────────────────────────────────

echo "--- Quiet mode ---"

compare_pipe "-q match (exit 0)" "$INPUT" -q "hello"
compare_pipe "-q no match (exit 1)" "$INPUT" -q "zzz"

# ── 13. File operations ─────────────────────────────────────────────

echo "--- File operations ---"

compare_file "-l files-with-matches" -l "hello" "$TDATA/lines.txt" "$TDATA/names.txt"
compare_file "-L files-without-match" -L "hello" "$TDATA/lines.txt" "$TDATA/names.txt"
compare_file "multi-file with filenames" -n "hello" "$TDATA/lines.txt" "$TDATA/code.txt"
compare_file "-h no filename" -h "hello" "$TDATA/lines.txt" "$TDATA/code.txt"
compare_file "-H force filename" -H "hello" "$TDATA/lines.txt"
compare_file "single file (no filename)" "hello" "$TDATA/lines.txt"
compare_file "-c multi-file count" -c "hello" "$TDATA/lines.txt" "$TDATA/code.txt"

# ── 14. Context lines ───────────────────────────────────────────────

echo "--- Context lines ---"

CTX_INPUT="line1
line2
MATCH
line4
line5
line6
MATCH
line8"

compare_pipe "-A after context" "$CTX_INPUT" -A 1 "MATCH"
compare_pipe "-B before context" "$CTX_INPUT" -B 1 "MATCH"
compare_pipe "-C symmetric context" "$CTX_INPUT" -C 1 "MATCH"
compare_pipe "-A 2" "$CTX_INPUT" -A 2 "MATCH"
compare_pipe "-B 2" "$CTX_INPUT" -B 2 "MATCH"

# ── 15. Multiple patterns ───────────────────────────────────────────

echo "--- Multiple patterns ---"

compare_pipe "-e multiple" "$INPUT" -e "hello" -e "baz"
compare_file "-f pattern file" -f "$TDATA/patterns.txt" "$TDATA/lines.txt"

# ── 16. Recursive search ────────────────────────────────────────────

echo "--- Recursive search ---"

# Recursive order is filesystem-dependent — sort both outputs for comparison
compare_sorted() {
  local desc="$1"; shift
  local exp_out exp_rc act_out act_rc
  exp_out=$(grep "$@" 2>/dev/null | sort) || true
  exp_rc=$(grep "$@" >/dev/null 2>&1; echo $?) || true
  act_out=$("$JGREP" "$@" 2>/dev/null | sort) || true
  act_rc=$("$JGREP" "$@" >/dev/null 2>&1; echo $?) || true
  if [ "$exp_out" = "$act_out" ] && [ "$exp_rc" = "$act_rc" ]; then
    pass
  else
    fail "$desc" "out=[$exp_out] rc=$exp_rc" "out=[$act_out] rc=$act_rc"
    if [ "${VERBOSE:-}" = "1" ]; then
      diff <(echo "$exp_out") <(echo "$act_out") | head -10 || true
    fi
  fi
}

compare_sorted "-r recursive" -r "match" "$TDATA/tree"
compare_sorted "-r --include" -r --include='*.txt' "match" "$TDATA/tree"
compare_sorted "-r --exclude" -r --exclude='*.log' "match" "$TDATA/tree"

# ── 17. BRE (basic regex, default) ──────────────────────────────────

echo "--- BRE mode ---"

compare_pipe "BRE dot" "$INPUT" "h.llo"
compare_pipe "BRE star" "$INPUT" "hel*o"
compare_pipe "BRE bracket" "$INPUT" '[a-z]ello'
compare_pipe "BRE anchors" "$INPUT" '^hello.*world$'

# ── 18. PCRE mode ───────────────────────────────────────────────────

echo "--- PCRE mode ---"

compare_pipe "-P lookahead" "$INPUT" -P 'hello(?= world)'
compare_pipe "-P lookbehind" "$INPUT" -P '(?<=hello )world'
compare_pipe "-P non-greedy" "$INPUT" -P 'h.+?o'

# ── 19. Null-terminated output ──────────────────────────────────────

echo "--- Null handling ---"

# -Z null after filenames (compare as hex to see null bytes)
EXP_Z=$(grep -lZ "hello" "$TDATA/lines.txt" 2>/dev/null | xxd -p) || true
ACT_Z=$("$JGREP" -lZ "hello" "$TDATA/lines.txt" 2>/dev/null | xxd -p) || true
if [ "$EXP_Z" = "$ACT_Z" ]; then pass; else fail "-Z null filenames" "$EXP_Z" "$ACT_Z"; fi

# ── 20. Edge cases ──────────────────────────────────────────────────

echo "--- Edge cases ---"

compare_pipe "empty input" "" "hello"
compare_pipe "single char match" "a" "a"
compare_pipe "very long line" "$(python3 -c 'print("a"*10000 + "needle" + "b"*10000)')" "needle"

# ── 21. Exit codes ──────────────────────────────────────────────────

echo "--- Exit codes ---"

# Match → 0
exp_rc=$(printf "hello\n" | grep "hello" >/dev/null 2>&1; echo $?)
act_rc=$(printf "hello\n" | "$JGREP" "hello" >/dev/null 2>&1; echo $?)
if [ "$exp_rc" = "$act_rc" ]; then pass; else fail "exit 0 on match" "$exp_rc" "$act_rc"; fi

# No match → 1
exp_rc=$(printf "hello\n" | grep "zzz" >/dev/null 2>&1; echo $?)
act_rc=$(printf "hello\n" | "$JGREP" "zzz" >/dev/null 2>&1; echo $?)
if [ "$exp_rc" = "$act_rc" ]; then pass; else fail "exit 1 on no match" "$exp_rc" "$act_rc"; fi

# Bad regex → 2
exp_rc=$(printf "hello\n" | grep -E '[invalid' >/dev/null 2>&1; echo $?)
act_rc=$(printf "hello\n" | "$JGREP" -E '[invalid' >/dev/null 2>&1; echo $?)
if [ "$exp_rc" = "$act_rc" ]; then pass; else fail "exit 2 on bad regex" "$exp_rc" "$act_rc"; fi

# ── Summary ──────────────────────────────────────────────────────────

echo ""
echo "==========================================="
echo "  PASS: $PASS   FAIL: $FAIL   SKIP: $SKIP"
echo "==========================================="

if [ $FAIL -gt 0 ]; then
  echo ""
  echo "Failures:"
  printf "$ERRORS"
  exit 1
fi

# Cleanup
rm -rf "$TDATA"
exit 0

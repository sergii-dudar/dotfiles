#!/bin/sh
# Regression suite for modules.java.refactor.
#
#   runall.sh [scenario ...]        (default: all; exit status = number of failed scenarios)
#
# Each scenario builds a fresh fixture project (mkfixture.sh) under $TMPDIR/java-refactor-tests, moves files or
# directories through the module (run.sh → headless Neovim in test_mode), compiles main + test sources with javac
# and checks scenario-specific expectations. WARN/ERROR lines the module logged during the scenario are printed.
set -u
S=$(cd "$(dirname "$0")" && pwd)
ROOT="${TMPDIR:-/tmp}/java-refactor-tests"
M=src/main/java/com/acme/core
T=src/test/java/com/acme/core
LOG="${XDG_STATE_HOME:-$HOME/.local/state}/nvim/java-refactor.log"
SEL="$*"
fails=0
errs=""
CUR=""

want() {
    [ -z "$SEL" ] && return 0
    case " $SEL " in
        *" $1 "*) return 0 ;;
    esac
    return 1
}

fail() {
    errs="$errs
  $1"
}
exists() { [ -e "$CUR/$1" ] || fail "missing: $1"; }
absent() { [ ! -e "$CUR/$1" ] || fail "should not exist: $1"; }
has() { grep -q -- "$2" "$CUR/$1" 2>/dev/null || fail "$1 lacks: $2"; }
lacks() { grep -q -- "$2" "$CUR/$1" 2>/dev/null && fail "$1 must not contain: $2"; }

assert_scenario() {
    case "$1" in
        s1) # partial move util/VerificationRequestUtil -> service: only its own test follows
            has "$M/service/VerificationRequestUtil.java" '^package com.acme.core.service;'
            has "$M/service/VerificationRequestUtil.java" '^import com.acme.core.util.CardUtil;'
            lacks "$M/service/VerificationRequestUtil.java" 'import com.acme.core.util.Objects;'
            has "$M/util/CardUtil.java" '^package com.acme.core.util;'
            has "$M/util/AppConstant.java" '^package com.acme.core.util;'
            has "$T/service/VerificationRequestUtilTest.java" '^package com.acme.core.service;'
            has "$T/service/VerificationRequestUtilTest.java" '^import com.acme.core.util.CardUtil;'
            exists "$T/util/CardUtilTest.java"
            exists "$T/util/CardUtilityTest.java"
            absent "$T/service/util"
            has "$M/other/Other.java" '^import com.acme.core.service.VerificationRequestUtil;'
            ;;
        s2) # util2/Only -> service empties util2: the whole test package follows, merged into existing tests
            has "$M/service/Only.java" '^package com.acme.core.service;'
            absent "$M/util2"
            absent "$T/util2"
            has "$T/service/OnlyTest.java" '^package com.acme.core.service;'
            has "$T/service/Helper.java" '^package com.acme.core.service;'
            exists "$T/service/SvcTest.java"
            has "$M/other/Other.java" '^import com.acme.core.service.Only;'
            ;;
        s3) # move + rename util/CardUtil -> service/CardHelper
            has "$M/service/CardHelper.java" 'class CardHelper {'
            has "$M/service/CardHelper.java" 'CardHelper.class.getSimpleName'
            lacks "$M/service/CardHelper.java" 'CardUtil'
            has "$M/util/VerificationRequestUtil.java" '^import com.acme.core.service.CardHelper;'
            has "$M/util/VerificationRequestUtil.java" 'CardHelper.mask'
            has "$T/service/CardHelperTest.java" 'class CardHelperTest'
            has "$T/service/CardHelperTest.java" 'CardHelper.mask'
            absent "$T/util/CardUtilTest.java"
            exists "$T/util/CardUtilityTest.java"
            has "$T/util/VerificationRequestUtilTest.java" '^import com.acme.core.service.CardHelper;'
            ;;
        s4) # directory move core/util -> core/helpers (package rename)
            absent "$M/util"
            absent "$T/util"
            has "$M/helpers/CardUtil.java" '^package com.acme.core.helpers;'
            has "$T/helpers/CardUtilTest.java" '^package com.acme.core.helpers;'
            has "$M/service/Svc.java" '^import com.acme.core.helpers.CardUtil;'
            has "$M/other/Other.java" '^import com.acme.core.helpers.\*;'
            ;;
        s5) # two siblings leave util for different packages
            has "$M/service/CardUtil.java" '^import com.acme.core.other.AppConstant;'
            has "$M/util/VerificationRequestUtil.java" '^import com.acme.core.service.CardUtil;'
            has "$M/util/VerificationRequestUtil.java" '^import com.acme.core.other.AppConstant;'
            has "$T/service/CardUtilTest.java" '^package com.acme.core.service;'
            ;;
        s6) # same-package rename util/CardUtil -> util/CardHelper: test renamed, CardUtilityTest untouched
            has "$M/util/CardHelper.java" 'class CardHelper {'
            has "$M/util/CardHelper.java" 'CardHelper.class'
            has "$M/util/VerificationRequestUtil.java" 'CardHelper.mask'
            has "$M/service/Svc.java" '^import com.acme.core.util.CardHelper;'
            has "$T/util/CardHelperTest.java" 'class CardHelperTest'
            has "$T/util/CardHelperTest.java" 'CardHelper.mask'
            absent "$T/util/CardUtilTest.java"
            has "$T/util/CardUtilityTest.java" 'class CardUtilityTest'
            ;;
        s7) # sub-package move util/CardUtil -> util/card/CardUtil
            has "$M/util/card/CardUtil.java" '^package com.acme.core.util.card;'
            has "$M/util/VerificationRequestUtil.java" '^import com.acme.core.util.card.CardUtil;'
            has "$T/util/card/CardUtilTest.java" '^package com.acme.core.util.card;'
            absent "$T/util/CardUtilTest.java"
            ;;
        s8) # batch: directory util2 -> util3 plus an unrelated single file move in the same run
            has "$M/util3/Only.java" '^package com.acme.core.util3;'
            has "$T/util3/OnlyTest.java" '^package com.acme.core.util3;'
            has "$M/service/CardUtil.java" '^package com.acme.core.service;'
            has "$T/service/CardUtilTest.java" '^package com.acme.core.service;'
            has "$M/other/Other.java" '^import com.acme.core.util3.Only;'
            ;;
    esac
}

run() {
    name=$1
    shift
    want "$name" || return 0
    P="$ROOT/$name"
    CUR="$P"
    errs=""
    "$S/mkfixture.sh" "$P"
    start=$(wc -l <"$LOG" 2>/dev/null || echo 0)

    out=$("$S/run.sh" "$P" "$@" 2>&1)

    echo "$out" | grep -q '^RESULT: true' || fail "module returned false"
    echo "$out" | grep -q '^main OK' \
        || fail "javac main failed: $(echo "$out" | sed -n '/=== JAVAC MAIN ===/,/=== JAVAC TEST ===/p' | grep -v '===' | head -6)"
    echo "$out" | grep -q '^test OK' \
        || fail "javac test failed: $(echo "$out" | sed -n '/=== JAVAC TEST ===/,$p' | grep -v '===' | head -6)"
    dups=$(find "$P/src" -name '*.java' | while IFS= read -r f; do
        d=$(grep '^import ' "$f" | sort | uniq -d)
        [ -n "$d" ] && echo "${f#"$P"/}: $d"
    done)
    [ -z "$dups" ] || fail "duplicate imports: $dups"
    assert_scenario "$name"

    if [ -z "$errs" ]; then
        echo "PASS $name"
    else
        echo "FAIL $name$errs"
        fails=$((fails + 1))
    fi
    warns=$(tail -n +"$((start + 1))" "$LOG" 2>/dev/null | grep -E '\[(WARN|ERROR)\]' | grep -v 'same simple name' | cut -c1-160)
    [ -z "$warns" ] || printf '  log WARN/ERROR:\n%s\n' "$(echo "$warns" | sed 's/^/    /')"
}

run s1 "$ROOT/s1/$M/util/VerificationRequestUtil.java" "$ROOT/s1/$M/service/VerificationRequestUtil.java"
run s2 "$ROOT/s2/$M/util2/Only.java" "$ROOT/s2/$M/service/Only.java"
run s3 "$ROOT/s3/$M/util/CardUtil.java" "$ROOT/s3/$M/service/CardHelper.java"
run s4 "$ROOT/s4/$M/util" "$ROOT/s4/$M/helpers"
run s5 "$ROOT/s5/$M/util/CardUtil.java" "$ROOT/s5/$M/service/CardUtil.java" \
    "$ROOT/s5/$M/util/AppConstant.java" "$ROOT/s5/$M/other/AppConstant.java"
run s6 "$ROOT/s6/$M/util/CardUtil.java" "$ROOT/s6/$M/util/CardHelper.java"
run s7 "$ROOT/s7/$M/util/CardUtil.java" "$ROOT/s7/$M/util/card/CardUtil.java"
run s8 "$ROOT/s8/$M/util2" "$ROOT/s8/$M/util3" "$ROOT/s8/$M/util/CardUtil.java" "$ROOT/s8/$M/service/CardUtil.java"

echo "=== $fails scenario(s) failed ==="
exit "$fails"

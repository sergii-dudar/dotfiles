#!/bin/sh
# Creates a tiny Maven-like Java project at $1 for exercising modules.java.refactor.
#
# Layout (main + mirrored tests):
#   core/util     AppConstant, CardUtil (self-reference via CardUtil.class), Objects (same simple name as
#                 java.util.Objects, imported by VerificationRequestUtil), VerificationRequestUtil
#   core/util2    Only            (package that becomes empty when its only class moves)
#   core/service  Svc             (explicit imports of util types)
#   core/other    Other           (wildcard import of util, import of util2.Only)
#   tests         CardUtilTest, CardUtilityTest (must never follow CardUtil), VerificationRequestUtilTest,
#                 OnlyTest + Helper (test-only class), SvcTest
set -eu
P="$1"
rm -rf "$P"
M="$P/src/main/java/com/acme/core"
T="$P/src/test/java/com/acme/core"
mkdir -p "$M/util" "$M/util2" "$M/service" "$M/other" "$T/util" "$T/util2" "$T/service"
printf '<project><modelVersion>4.0.0</modelVersion><groupId>a</groupId><artifactId>b</artifactId><version>1</version></project>\n' >"$P/pom.xml"

cat >"$M/util/AppConstant.java" <<'J'
package com.acme.core.util;

public final class AppConstant {
    public static final String X = "x";

    private AppConstant() {
    }
}
J
cat >"$M/util/CardUtil.java" <<'J'
package com.acme.core.util;

public final class CardUtil {
    private static final String NAME = CardUtil.class.getSimpleName();

    public static String mask(String pan) {
        return AppConstant.X + pan + NAME;
    }

    private CardUtil() {
    }
}
J
cat >"$M/util/Objects.java" <<'J'
package com.acme.core.util;

public final class Objects {
    private Objects() {
    }
}
J
cat >"$M/util/VerificationRequestUtil.java" <<'J'
package com.acme.core.util;

import java.util.Objects;

public final class VerificationRequestUtil {
    public static String require(String v) {
        return Objects.requireNonNull(CardUtil.mask(v)) + AppConstant.X;
    }

    private VerificationRequestUtil() {
    }
}
J
cat >"$M/util2/Only.java" <<'J'
package com.acme.core.util2;

public final class Only {
    public static String go() {
        return "go";
    }
}
J
cat >"$M/service/Svc.java" <<'J'
package com.acme.core.service;

import com.acme.core.util.CardUtil;
import com.acme.core.util.VerificationRequestUtil;

public class Svc {
    public String run(String v) {
        return VerificationRequestUtil.require(v) + CardUtil.mask(v);
    }
}
J
cat >"$M/other/Other.java" <<'J'
package com.acme.core.other;

import com.acme.core.util.*;
import com.acme.core.util2.Only;

public class Other {
    public String run(String v) {
        return VerificationRequestUtil.require(v) + CardUtil.mask(v) + Only.go();
    }
}
J
cat >"$T/util/CardUtilTest.java" <<'J'
package com.acme.core.util;

public class CardUtilTest {
    void t() {
        CardUtil.mask("1");
    }
}
J
cat >"$T/util/CardUtilityTest.java" <<'J'
package com.acme.core.util;

public class CardUtilityTest {
    void t() {
    }
}
J
cat >"$T/util/VerificationRequestUtilTest.java" <<'J'
package com.acme.core.util;

public class VerificationRequestUtilTest {
    void t() {
        VerificationRequestUtil.require("1");
        CardUtil.mask("1");
    }
}
J
cat >"$T/util2/OnlyTest.java" <<'J'
package com.acme.core.util2;

public class OnlyTest {
    void t() {
        Only.go();
        Helper.h();
    }
}
J
cat >"$T/util2/Helper.java" <<'J'
package com.acme.core.util2;

public class Helper {
    static void h() {
    }
}
J
cat >"$T/service/SvcTest.java" <<'J'
package com.acme.core.service;

import com.acme.core.util.VerificationRequestUtil;

public class SvcTest {
    void t() {
        new Svc().run("1");
        VerificationRequestUtil.require("1");
    }
}
J

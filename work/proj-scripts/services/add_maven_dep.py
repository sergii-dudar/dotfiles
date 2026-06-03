#!/usr/bin/env python3
"""
add_maven_dep.py — Add a Maven dependency to every service pom.xml in a project tree.

Scans a directory for Maven service modules and inserts a <dependency> block
in the right position, keeping the file tidy without touching anything else.

PLACEMENT RULES (tried in order)
  1. After the last *non-test* <dependency> whose <groupId> matches --near-group
     (defaults to the new dependency's own groupId).
  2. Before the first <dependency> that carries <scope>test</scope>.
  3. Just before the closing </dependencies> tag.

SERVICE DETECTION
  - Skips poms that contain <modules> (aggregator / parent poms).
  - Skips poms inside directories whose name ends with -api.
  - With --services-only (on by default) also skips poms that do not contain
    spring-boot-maven-plugin (library modules, shared DTOs, etc.).

USAGE
  python3 add_maven_dep.py <groupId> <artifactId> [OPTIONS]

EXAMPLES
  # Dry-run first — see exactly what would change
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics --dry-run

  # Apply for real in the current directory
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics

  # Add with an explicit version range
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics \\
      --version '[4.0.0,5.0.0)'

  # Add a test-scoped dependency
  python3 add_maven_dep.py org.wiremock.integrations wiremock-spring-boot --scope test

  # Target a specific directory instead of CWD
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics \\
      --dir /path/to/projects

  # Place near a *different* groupId than the new dep's own groupId
  python3 add_maven_dep.py org.springframework.boot spring-boot-starter-cache \\
      --near-group org.springframework.boot

  # Also process library modules (not just Spring Boot services)
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics \\
      --no-services-only

  # Force re-insert even when the artifact already exists (fixes wrong placement)
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics --force
"""

import argparse
import os
import re
import sys
from pathlib import Path

# ── ANSI colours ──────────────────────────────────────────────────────────────
GREEN  = '\033[0;32m'
YELLOW = '\033[1;33m'
RED    = '\033[0;31m'
CYAN   = '\033[0;36m'
BOLD   = '\033[1m'
RESET  = '\033[0m'

def ok(msg):   print(f'  {GREEN}✔{RESET}  {msg}')
def warn(msg): print(f'  {YELLOW}⚠{RESET}  {msg}')
def err(msg):  print(f'  {RED}✘{RESET}  {msg}')
def info(msg): print(f'  {CYAN}→{RESET}  {msg}')


# ── dependency block regex ─────────────────────────────────────────────────────
_DEP_RE = re.compile(r'<dependency>.*?</dependency>', re.DOTALL)


# ── core helpers ──────────────────────────────────────────────────────────────

def detect_indent(content: str) -> int:
    """Return the number of spaces used for a top-level <dependency> tag."""
    m = re.search(r'^( +)<dependency>', content, re.MULTILINE)
    return len(m.group(1)) if m else 8


def build_dep_block(group_id: str, artifact_id: str, version: str | None,
                    scope: str | None, indent: int) -> str:
    pad  = ' ' * indent
    cpad = ' ' * (indent + 4)
    lines = [
        f'{pad}<dependency>',
        f'{cpad}<groupId>{group_id}</groupId>',
        f'{cpad}<artifactId>{artifact_id}</artifactId>',
    ]
    if version:
        lines.append(f'{cpad}<version>{version}</version>')
    if scope:
        lines.append(f'{cpad}<scope>{scope}</scope>')
    lines.append(f'{pad}</dependency>')
    return '\n'.join(lines)


def remove_dep_block(content: str, group_id: str, artifact_id: str) -> str:
    """Remove a previously inserted dependency block (used with --force)."""
    pattern = re.compile(
        r'\n[ \t]*<dependency>[ \t]*\n'
        r'[ \t]*<groupId>' + re.escape(group_id) + r'</groupId>[ \t]*\n'
        r'[ \t]*<artifactId>' + re.escape(artifact_id) + r'</artifactId>[ \t]*\n'
        r'(?:[ \t]*<[^>]+>[^<]*</[^>]+>[ \t]*\n)*'
        r'[ \t]*</dependency>',
        re.DOTALL,
    )
    return pattern.sub('', content)


def find_insert_pos(content: str, near_group: str) -> tuple[int, str]:
    """
    Return (position, description) for the best insertion point.

    Position meaning depends on strategy:
      'after'  → insert at this offset (content[:pos] + new_dep + content[pos:])
      'before' → same semantics, but offset points to the start of a line
    """
    # Strategy 1 — after last non-test dep matching near_group
    last_end = None
    for m in _DEP_RE.finditer(content):
        block = m.group(0)
        if near_group in block and '<scope>test</scope>' not in block:
            last_end = m.end()

    if last_end is not None:
        pos = last_end
        if pos < len(content) and content[pos] == '\n':
            pos += 1   # consume the newline so the new dep starts on a fresh line
        return pos, f'after last non-test dep of {near_group!r}'

    # Strategy 2 — before first test-scoped dep
    for m in _DEP_RE.finditer(content):
        if '<scope>test</scope>' in m.group(0):
            line_start = content.rfind('\n', 0, m.start()) + 1
            return line_start, 'before first test-scope dep'

    # Strategy 3 — before </dependencies>
    close = content.rfind('</dependencies>')
    if close != -1:
        line_start = content.rfind('\n', 0, close) + 1
        return line_start, 'before </dependencies>'

    return -1, 'ERROR: no <dependencies> block found'


# ── pom collection ────────────────────────────────────────────────────────────

def collect_poms(root: Path, services_only: bool) -> list[Path]:
    """Walk root and return eligible service pom.xml paths."""
    poms = []
    root_parts_len = len(root.parts)

    for pom in sorted(root.rglob('pom.xml')):
        # Skip poms inside *-api directories
        relative_parts = pom.parts[root_parts_len:]
        if any(part.endswith('-api') for part in relative_parts):
            continue

        content = pom.read_text(encoding='utf-8')

        # Skip aggregator / parent poms
        if '<modules>' in content:
            continue

        # Optionally restrict to Spring Boot services
        if services_only and 'spring-boot-maven-plugin' not in content:
            continue

        poms.append(pom)

    return poms


# ── single-file processing ────────────────────────────────────────────────────

def process_pom(
    pom: Path,
    group_id: str,
    artifact_id: str,
    version: str | None,
    scope: str | None,
    near_group: str,
    dry_run: bool,
    force: bool,
) -> str:
    """
    Process one pom.xml. Returns one of: 'modified', 'dry-run', 'skipped', 'error'.
    """
    content = pom.read_text(encoding='utf-8')

    artifact_present = f'<artifactId>{artifact_id}</artifactId>' in content

    if artifact_present:
        if not force:
            warn('already present — skipping (use --force to reinsert)')
            return 'skipped'
        content = remove_dep_block(content, group_id, artifact_id)
        if f'<artifactId>{artifact_id}</artifactId>' in content:
            err('could not remove existing block (unusual format) — skipping')
            return 'error'
        info('removed existing block for reinsertion')

    insert_pos, strategy = find_insert_pos(content, near_group)
    if insert_pos == -1:
        err(strategy)
        return 'error'

    indent    = detect_indent(content)
    dep_block = build_dep_block(group_id, artifact_id, version, scope, indent)
    new_content = content[:insert_pos] + dep_block + '\n' + content[insert_pos:]

    info(strategy)

    if not dry_run:
        pom.write_text(new_content, encoding='utf-8')
        ok('written')
        return 'modified'

    ok('(dry-run)')
    return 'dry-run'


# ── CLI ───────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        prog='add_maven_dep.py',
        description='Add a Maven dependency to every service pom.xml in a project tree.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""examples:
  # dry-run — see what would change without writing anything
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics --dry-run

  # apply
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics

  # add with a version range
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics \\
      --version '[4.0.0,5.0.0)'

  # add a test-scoped dependency
  python3 add_maven_dep.py org.wiremock.integrations wiremock-spring-boot --scope test

  # target a specific directory
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics \\
      --dir /path/to/projects

  # place near a different groupId
  python3 add_maven_dep.py org.springframework.boot spring-boot-starter-cache \\
      --near-group org.springframework.boot

  # include library modules as well
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics \\
      --no-services-only

  # reinsert even if already present (fixes wrong placement)
  python3 add_maven_dep.py ua.raiffeisen.payments.common-infra infra-metrics --force
""",
    )

    parser.add_argument('group_id',    metavar='groupId')
    parser.add_argument('artifact_id', metavar='artifactId')

    parser.add_argument(
        '--version', '-V', metavar='VERSION', default=None,
        help='Version string, e.g. 1.2.3 or [4.0.0,5.0.0)  (omitted by default)',
    )
    parser.add_argument(
        '--scope', '-s', metavar='SCOPE', default=None,
        choices=['compile', 'test', 'provided', 'runtime', 'system', 'import'],
        help='Maven scope (default: compile — tag omitted from XML)',
    )
    parser.add_argument(
        '--dir', '-d', metavar='DIR', default='.',
        help='Root directory to scan (default: current directory)',
    )
    parser.add_argument(
        '--near-group', metavar='GROUPID', default=None,
        help='Place after last non-test dep of this groupId '
             '(default: same as the new dep\'s groupId)',
    )
    parser.add_argument(
        '--services-only', dest='services_only',
        action='store_true', default=True,
        help='Only process poms with spring-boot-maven-plugin (default: on)',
    )
    parser.add_argument(
        '--no-services-only', dest='services_only', action='store_false',
        help='Also process library / shared-model poms',
    )
    parser.add_argument(
        '--dry-run', '-n', action='store_true',
        help='Print what would change without writing any files',
    )
    parser.add_argument(
        '--force', action='store_true',
        help='Remove and reinsert even if the artifact already exists',
    )

    return parser.parse_args()


def main() -> None:
    args = parse_args()

    root = Path(args.dir).resolve()
    if not root.is_dir():
        print(f'{RED}Error:{RESET} {root} is not a directory', file=sys.stderr)
        sys.exit(1)

    near_group = args.near_group or args.group_id
    # Maven 'compile' is the default — omit the tag to keep poms tidy
    scope = args.scope if args.scope and args.scope != 'compile' else None

    poms = collect_poms(root, args.services_only)
    if not poms:
        print('No eligible pom.xml files found.')
        sys.exit(0)

    dep_label = f'{args.group_id}:{args.artifact_id}'
    if args.version:
        dep_label += f':{args.version}'
    if scope:
        dep_label += f'  (scope={scope})'

    label_width = 60
    print()
    print(f'{BOLD}{"DRY RUN — " if args.dry_run else ""}Adding {dep_label}{RESET}')
    print(f'{CYAN}  near-group : {near_group}{RESET}')
    print(f'{CYAN}  directory  : {root}{RESET}')
    print(f'{CYAN}  poms found : {len(poms)}{RESET}')
    print()

    counts: dict[str, int] = {'modified': 0, 'dry-run': 0, 'skipped': 0, 'error': 0}

    for pom in poms:
        rel = pom.relative_to(root)
        print(f'{BOLD}{rel}{RESET}')
        status = process_pom(
            pom,
            args.group_id,
            args.artifact_id,
            args.version,
            scope,
            near_group,
            args.dry_run,
            args.force,
        )
        counts[status] = counts.get(status, 0) + 1
        print()

    # ── summary ───────────────────────────────────────────────────────────────
    print(f'{BOLD}══ Summary {"(dry-run) " if args.dry_run else ""}══════════════════════════════{RESET}')
    action = 'would modify' if args.dry_run else 'modified'
    print(f'  {GREEN}{action:12}{RESET} {counts.get("modified", 0) + counts.get("dry-run", 0)}')
    print(f'  {YELLOW}{"skipped":12}{RESET} {counts.get("skipped", 0)}')
    print(f'  {RED}{"errors":12}{RESET} {counts.get("error", 0)}')
    print()

    sys.exit(1 if counts.get('error', 0) > 0 else 0)


if __name__ == '__main__':
    main()

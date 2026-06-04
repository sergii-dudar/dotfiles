#!/usr/bin/env bash
# watch_pr_pipelines.sh
# Poll a list of GitHub PRs until every PR's checks pass.
# Failed PRs keep being polled (you may restart them and the rerun can pass);
# stop monitoring with Ctrl-C, or use --once for a single pass.
#
# Usage:
#   watch_pr_pipelines.sh <pr-url> [<pr-url> ...]
#   watch_pr_pipelines.sh -f urls.txt
#   cat urls.txt | watch_pr_pipelines.sh
#
# Options:
#   -f, --file <path>      Read PR URLs from file (one per line, '#' comments ok)
#   -i, --interval <sec>   Poll interval in seconds (default: 30)
#   -1, --once             Run a single check pass and exit
#       --warn-pattern <g> Treat failed checks whose name matches glob <g> as
#                          warnings (yellow ⚠) instead of hard failures.
#                          Repeatable. Default: CI-*-API-LIBRARY*
#       --no-warn          Disable the default warn pattern
#       --no-color         Disable ANSI colors (also disables hyperlinks)
#       --no-links         Disable OSC 8 hyperlinks (keep colors)
#   -h, --help             Show this help
#
# Exit code:
#   0 if every PR ended up green
#   1 if interrupted with one or more PRs still failing
#   2 on usage/setup errors
#   130 if interrupted while still pending

set -euo pipefail

# ── colors ────────────────────────────────────────────────────────────────────
if [[ -t 1 ]]; then
  RED=$'\033[0;31m'; GREEN=$'\033[0;32m'; YELLOW=$'\033[1;33m'
  BLUE=$'\033[0;34m'; CYAN=$'\033[0;36m'; GREY=$'\033[0;90m'
  BOLD=$'\033[1m'; RESET=$'\033[0m'
  HYPERLINKS=1
else
  RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; GREY=''; BOLD=''; RESET=''
  HYPERLINKS=0
fi

# OSC 8 hyperlink. Clickable in iTerm2, Terminal.app, kitty, wezterm, modern
# alacritty, gnome-terminal. Falls back to plain text everywhere else.
osc8() {
  local url="$1" text="$2"
  if (( HYPERLINKS )) && [[ -n "$url" ]]; then
    printf '\033]8;;%s\033\\%s\033]8;;\033\\' "$url" "$text"
  else
    printf '%s' "$text"
  fi
}

# ── args ──────────────────────────────────────────────────────────────────────
INTERVAL=30
ONCE=0
URLS=()
URL_FILE=""
# Failed checks whose name matches any of these globs are treated as warnings
# (yellow ⚠), not failures — they don't block loop termination.
WARN_PATTERNS=()
DEFAULT_WARN_PATTERN='CI-*-API-LIBRARY*'

usage() { sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; }

while (($#)); do
  case "$1" in
    -f|--file)     URL_FILE="${2:-}"; shift 2 ;;
    -i|--interval) INTERVAL="${2:-30}"; shift 2 ;;
    -1|--once)     ONCE=1; shift ;;
    --no-color)    RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; GREY=''; BOLD=''; RESET=''; HYPERLINKS=0; shift ;;
    --no-links)    HYPERLINKS=0; shift ;;
    --warn-pattern) WARN_PATTERNS+=("${2:-}"); shift 2 ;;
    --no-warn)     WARN_PATTERNS=(__none__); shift ;;
    -h|--help)     usage; exit 0 ;;
    --)            shift; while (($#)); do URLS+=("$1"); shift; done ;;
    -*)            echo "${RED}Unknown option: $1${RESET}" >&2; exit 2 ;;
    *)             URLS+=("$1"); shift ;;
  esac
done

if [[ -n "$URL_FILE" ]]; then
  while IFS= read -r line; do
    line="${line%%#*}"
    line="${line//[$'\t\r\n ']/}"
    [[ -z "$line" ]] && continue
    URLS+=("$line")
  done < "$URL_FILE"
fi

if [[ ${#URLS[@]} -eq 0 ]] && [[ ! -t 0 ]]; then
  while IFS= read -r line; do
    line="${line%%#*}"
    line="${line//[$'\t\r\n ']/}"
    [[ -z "$line" ]] && continue
    URLS+=("$line")
  done
fi

if [[ ${#URLS[@]} -eq 0 ]]; then
  echo "${RED}No PR URLs provided.${RESET}" >&2
  usage
  exit 2
fi

# Default warn pattern unless user supplied any (or explicitly disabled).
if [[ ${#WARN_PATTERNS[@]} -eq 0 ]]; then
  WARN_PATTERNS=("$DEFAULT_WARN_PATTERN")
elif [[ "${WARN_PATTERNS[0]}" == "__none__" ]]; then
  WARN_PATTERNS=()
fi

# Bash glob match against any warn pattern
matches_warn_pattern() {
  local name="$1" p
  for p in "${WARN_PATTERNS[@]}"; do
    [[ "$name" == $p ]] && return 0
  done
  return 1
}

# ── preflight ─────────────────────────────────────────────────────────────────
command -v gh >/dev/null || { echo "${RED}gh CLI not installed${RESET}" >&2; exit 2; }
command -v jq >/dev/null || { echo "${RED}jq not installed${RESET}" >&2; exit 2; }
gh auth status >/dev/null 2>&1 || { echo "${RED}gh not authenticated (gh auth login)${RESET}" >&2; exit 2; }

# Short label = repo name + PR number, e.g. ua-payments-foo#42
short_label() {
  local url="$1"
  local repo pr
  repo=$(echo "$url" | sed -nE 's|.*/([^/]+)/pull/[0-9]+/?.*|\1|p')
  pr=$(echo "$url"   | sed -nE 's|.*/pull/([0-9]+)/?.*|\1|p')
  [[ -z "$repo" ]] && repo="?"
  [[ -z "$pr"   ]] && pr="?"
  printf '%s#%s' "$repo" "$pr"
}

# Compute display width for column alignment
LABEL_W=0
LABELS=()
for u in "${URLS[@]}"; do
  l=$(short_label "$u")
  LABELS+=("$l")
  (( ${#l} > LABEL_W )) && LABEL_W=${#l}
done

TMPDIR=$(mktemp -d -t watch_pr_pipelines.XXXXXX)

# Open /dev/tty as fd 9 so we can read single keystrokes during the
# inter-cycle wait, even when URLs were piped in via stdin. The block
# wrapper silences bash's own redirection error when no controlling tty
# exists. Original stty settings are restored on EXIT.
HAS_TTY=0
{ exec 9</dev/tty; } 2>/dev/null && HAS_TTY=1
STTY_ORIG=""
if (( HAS_TTY )); then
  STTY_ORIG=$(stty -g <&9 2>/dev/null || echo "")
  if [[ -n "$STTY_ORIG" ]]; then
    stty -icanon -echo min 0 time 0 <&9 2>/dev/null || STTY_ORIG=""
  fi
fi
cleanup() {
  rm -rf "$TMPDIR"
  if [[ -n "$STTY_ORIG" ]]; then
    stty "$STTY_ORIG" <&9 2>/dev/null || true
  fi
  (( HAS_TTY )) && exec 9<&-
  # Re-enable terminal auto-wrap (DECAWM) — disabled during the watch loop
  # so long lines truncate instead of wrapping, keeping prev_lines exact.
  [[ -t 1 ]] && printf '\033[?7h'
}
trap cleanup EXIT
[[ -t 1 ]] && printf '\033[?7l'

INTERRUPTED=0
on_int() { INTERRUPTED=1; }
trap on_int INT

# Wait up to $1 seconds, returning early on a control keypress.
wait_with_keys() {
  local total="$1" elapsed=0 key=""
  while (( elapsed < total )); do
    (( INTERRUPTED )) && return 0
    if [[ -n "$STTY_ORIG" ]] && IFS= read -rsn 1 -t 1 key <&9 2>/dev/null; then
      case "$key" in
        r|R|' '|$'\n')  return 0 ;;
        q|Q|$'\033')    INTERRUPTED=1; return 0 ;;
        '+'|'=')        INTERVAL=$((INTERVAL * 2)); ((INTERVAL > 600)) && INTERVAL=600; return 0 ;;
        '-'|'_')        INTERVAL=$((INTERVAL / 2)); ((INTERVAL < 1))   && INTERVAL=1;   return 0 ;;
      esac
    elif [[ -z "$STTY_ORIG" ]]; then
      sleep 1
    fi
    elapsed=$((elapsed + 1))
  done
}

# ── core: classify one PR ─────────────────────────────────────────────────────
# Buckets observed from gh: pass | fail | pending | skipping | cancel
# We treat skipping as success-equivalent. Failed checks matching any
# WARN_PATTERNS glob are tagged as warnings (don't block completion).
classify_pr() {
  local url="$1" out_file="$2"
  local raw rc
  raw=$(gh pr checks "$url" --json bucket,name,state 2>"$out_file.err") && rc=0 || rc=$?

  if (( rc != 0 )); then
    if grep -qiE 'no checks|no required' "$out_file.err"; then
      printf 'success\t0\t0\t0\t0\t0\tno-checks\n' > "$out_file"
    else
      printf 'error\t0\t0\t0\t0\t0\t%s\n' "$(head -c 120 "$out_file.err" | tr '\n\t' '  ')" > "$out_file"
    fi
    return
  fi

  local n_pass n_pending n_skip
  read -r n_pass n_pending n_skip < <(jq -r '
    [
      ([.[] | select(.bucket=="pass")]     | length),
      ([.[] | select(.bucket=="pending")]  | length),
      ([.[] | select(.bucket=="skipping")] | length)
    ] | @tsv' <<<"$raw" | tr '\t' ' ')

  # Walk failed/cancelled checks and split into warnings vs hard failures.
  local fail_names_raw
  fail_names_raw=$(jq -r '.[] | select(.bucket=="fail" or .bucket=="cancel") | .name' <<<"$raw")
  local n_warn=0 n_hardfail=0
  local warn_names=() hardfail_names=()
  while IFS= read -r name; do
    [[ -z "$name" ]] && continue
    if matches_warn_pattern "$name"; then
      n_warn=$((n_warn+1)); warn_names+=("$name")
    else
      n_hardfail=$((n_hardfail+1)); hardfail_names+=("$name")
    fi
  done <<<"$fail_names_raw"

  local status detail=""
  if (( n_hardfail > 0 )); then
    status=failure
    detail="failed: $(IFS=', '; echo "${hardfail_names[*]}")"
    (( n_warn > 0 )) && detail="$detail; warn: $(IFS=', '; echo "${warn_names[*]}")"
  elif (( n_pending > 0 )); then
    status=pending
    detail="running: $(jq -r '[.[] | select(.bucket=="pending") | .name] | join(", ")' <<<"$raw")"
  elif (( n_warn > 0 )); then
    status=warning
    detail="warn: $(IFS=', '; echo "${warn_names[*]}")"
  else
    status=success
  fi
  detail=$(printf '%s' "$detail" | head -c 200)

  # TSV: status \t pass \t hardfail \t warn \t pending \t skip \t detail
  printf '%s\t%d\t%d\t%d\t%d\t%d\t%s\n' \
    "$status" "$n_pass" "$n_hardfail" "$n_warn" "$n_pending" "$n_skip" "$detail" \
    > "$out_file"
}

# ── render one row ────────────────────────────────────────────────────────────
render_row() {
  local label="$1" url="$2" status="$3" pass="$4" hardfail="$5" warn="$6" pending="$7" skip="$8" detail="$9"
  local icon color
  case "$status" in
    success)  icon="✔"; color="$GREEN"  ;;
    warning)  icon="⚠"; color="$YELLOW" ;;
    failure)  icon="✖"; color="$RED"    ;;
    pending)  icon="…"; color="$YELLOW" ;;
    error)    icon="?"; color="$BLUE"   ;;
    *)        icon="·"; color="$GREY"   ;;
  esac
  local padded linked
  printf -v padded '%-*s' "$LABEL_W" "$label"
  linked=$(osc8 "$url" "$padded")
  local counts
  counts=$(printf '%s%d✓%s %s%d✗%s %s%d⚠%s %s%d…%s %s%d↷%s' \
    "$GREEN"  "$pass"     "$RESET" \
    "$RED"    "$hardfail" "$RESET" \
    "$YELLOW" "$warn"     "$RESET" \
    "$YELLOW" "$pending"  "$RESET" \
    "$GREY"   "$skip"     "$RESET")
  printf '  %s%s%s  %s  %s' "$color" "$icon" "$RESET" "$linked" "$counts"
  if [[ -n "$detail" ]]; then
    printf '  %s%s%s' "$GREY" "$detail" "$RESET"
  fi
  printf '\n'
}

# ── main loop ─────────────────────────────────────────────────────────────────
N=${#URLS[@]}
declare -a STATE       # final state per PR (success|failure|pending|error)
for ((i=0; i<N; i++)); do STATE[i]="pending"; done

cycle=0
prev_lines=0
start_ts=$(date +%s)

while :; do
  (( INTERRUPTED )) && { echo "${YELLOW}Interrupted — exiting.${RESET}" >&2; break; }
  cycle=$((cycle + 1))

  # Re-poll every PR that isn't done. `success` and `warning` are terminal
  # (warning = only warn-pattern checks failed). Failures keep being polled in
  # case of a restart.
  pids=()
  for ((i=0; i<N; i++)); do
    if [[ "${STATE[i]}" != "success" && "${STATE[i]}" != "warning" ]]; then
      classify_pr "${URLS[i]}" "$TMPDIR/$i.out" &
      pids+=($!)
    fi
  done
  for p in "${pids[@]}"; do wait "$p" || true; done

  # Read results back
  for ((i=0; i<N; i++)); do
    [[ -f "$TMPDIR/$i.out" ]] || continue
    IFS=$'\t' read -r st _np _nhf _nw _npend _ns _det < "$TMPDIR/$i.out"
    STATE[i]="$st"
  done

  # Redraw table (cursor up over previous block)
  if (( prev_lines > 0 )); then
    printf '\033[%dA' "$prev_lines"
  fi

  now_ts=$(date +%s)
  elapsed=$((now_ts - start_ts))
  printf '\r\033[K%s[%s] cycle %d  elapsed %ds  interval %ds%s\n' \
    "$BOLD" "$(date '+%H:%M:%S')" "$cycle" "$elapsed" "$INTERVAL" "$RESET"

  green=0; warn_count=0; pending_count=0; failed_count=0; error_count=0
  for ((i=0; i<N; i++)); do
    if [[ -f "$TMPDIR/$i.out" ]]; then
      IFS=$'\t' read -r st np nhf nw npend ns det < "$TMPDIR/$i.out"
    else
      st="pending"; np=0; nhf=0; nw=0; npend=0; ns=0; det=""
    fi
    printf '\033[K'
    render_row "${LABELS[i]}" "${URLS[i]}" "$st" "$np" "$nhf" "$nw" "$npend" "$ns" "$det"
    case "$st" in
      success) green=$((green+1)) ;;
      warning) warn_count=$((warn_count+1)) ;;
      failure) failed_count=$((failed_count+1)) ;;
      pending) pending_count=$((pending_count+1)) ;;
      error)   error_count=$((error_count+1)) ;;
    esac
  done
  printf '\033[K  %s%d/%d green%s  %s%d warn%s  %s%d failing%s  %s%d running%s  %s%d gh-error%s\n' \
    "$GREEN"  "$green"          "$N"           "$RESET" \
    "$YELLOW" "$warn_count"                    "$RESET" \
    "$RED"    "$failed_count"                  "$RESET" \
    "$YELLOW" "$pending_count"                 "$RESET" \
    "$BLUE"   "$error_count"                   "$RESET"
  prev_lines=$((N + 2))
  if [[ -n "$STTY_ORIG" ]]; then
    printf '\033[K  %s[r]efresh  [q]uit  [+/-] interval%s\n' "$GREY" "$RESET"
    prev_lines=$((prev_lines + 1))
  fi

  # Termination conditions
  if (( ONCE )); then
    break
  fi
  if (( INTERRUPTED )); then
    echo "${YELLOW}Interrupted — exiting.${RESET}" >&2
    break
  fi
  # Done when every PR is green or warning (warning-only checks don't block).
  # Failures keep being polled in case of a restart; user must Ctrl-C if they
  # don't intend to restart them.
  if (( pending_count == 0 && failed_count == 0 && error_count == 0 )); then
    break
  fi

  wait_with_keys "$INTERVAL"
done

# ── final summary ─────────────────────────────────────────────────────────────
echo
echo "${BOLD}Summary:${RESET}"
fail_total=0
for ((i=0; i<N; i++)); do
  st="${STATE[i]}"
  case "$st" in
    failure) fail_total=$((fail_total+1)) ;;
  esac
  printf '  %s  %s\n' "$st" "${URLS[i]}"
done

if (( INTERRUPTED )); then exit 130; fi
if (( fail_total > 0 )); then exit 1; fi
exit 0

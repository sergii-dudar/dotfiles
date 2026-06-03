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
#       --no-color         Disable ANSI colors
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
else
  RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; GREY=''; BOLD=''; RESET=''
fi

# ── args ──────────────────────────────────────────────────────────────────────
INTERVAL=30
ONCE=0
URLS=()
URL_FILE=""

usage() { sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; }

while (($#)); do
  case "$1" in
    -f|--file)     URL_FILE="${2:-}"; shift 2 ;;
    -i|--interval) INTERVAL="${2:-30}"; shift 2 ;;
    -1|--once)     ONCE=1; shift ;;
    --no-color)    RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; GREY=''; BOLD=''; RESET=''; shift ;;
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
cleanup() { rm -rf "$TMPDIR"; }
trap cleanup EXIT

INTERRUPTED=0
on_int() { INTERRUPTED=1; }
trap on_int INT

# ── core: classify one PR ─────────────────────────────────────────────────────
# Buckets observed from gh: pass | fail | pending | skipping | cancel
# We treat skipping as success-equivalent (won't block completion).
classify_pr() {
  local url="$1" out_file="$2"
  local raw rc
  raw=$(gh pr checks "$url" --json bucket,name,state 2>"$out_file.err") && rc=0 || rc=$?

  if (( rc != 0 )); then
    if grep -qiE 'no checks|no required' "$out_file.err"; then
      printf 'success\t0\t0\t0\t0\tno-checks\n' > "$out_file"
    else
      printf 'error\t0\t0\t0\t0\t%s\n' "$(head -c 120 "$out_file.err" | tr '\n\t' '  ')" > "$out_file"
    fi
    return
  fi

  # Counts: pass / fail / pending / other(skipping/cancel/etc)
  local counts
  counts=$(jq -r '
    [
      ([.[] | select(.bucket=="pass")]      | length),
      ([.[] | select(.bucket=="fail")]      | length),
      ([.[] | select(.bucket=="pending")]   | length),
      ([.[] | select(.bucket=="skipping")]  | length),
      ([.[] | select(.bucket=="cancel")]    | length)
    ] | @tsv' <<<"$raw")
  IFS=$'\t' read -r n_pass n_fail n_pending n_skip n_cancel <<<"$counts"

  local status detail=""
  if (( n_fail > 0 )); then
    status=failure
    detail=$(jq -r '[.[] | select(.bucket=="fail") | .name] | join(", ")' <<<"$raw" | head -c 120)
  elif (( n_cancel > 0 && n_pending == 0 )); then
    status=failure
    detail=$(jq -r '[.[] | select(.bucket=="cancel") | .name] | join(", ")' <<<"$raw" | head -c 120)
  elif (( n_pending > 0 )); then
    status=pending
    detail=$(jq -r '[.[] | select(.bucket=="pending") | .name] | join(", ")' <<<"$raw" | head -c 120)
  else
    status=success
  fi

  printf '%s\t%d\t%d\t%d\t%d\t%s\n' \
    "$status" "$n_pass" "$n_fail" "$n_pending" "$((n_skip + n_cancel))" "$detail" \
    > "$out_file"
}

# ── render one row ────────────────────────────────────────────────────────────
render_row() {
  local label="$1" status="$2" pass="$3" fail="$4" pending="$5" other="$6" detail="$7"
  local icon color
  case "$status" in
    success)  icon="✔"; color="$GREEN"  ;;
    failure)  icon="✖"; color="$RED"    ;;
    pending)  icon="…"; color="$YELLOW" ;;
    error)    icon="?"; color="$BLUE"   ;;
    *)        icon="·"; color="$GREY"   ;;
  esac
  local counts
  counts=$(printf '%s%s%d✓%s %s%d✗%s %s%d…%s %s%d↷%s' \
    "$GREEN"  "" "$pass"    "$RESET" \
    "$RED"      "$fail"     "$RESET" \
    "$YELLOW"   "$pending"  "$RESET" \
    "$GREY"     "$other"    "$RESET")
  printf '  %s%s%s  %-*s  %s' "$color" "$icon" "$RESET" "$LABEL_W" "$label" "$counts"
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
  cycle=$((cycle + 1))

  # Re-poll every PR that isn't fully green. We keep watching `failure` PRs too
  # because the user may restart their pipeline and a rerun may pass.
  pids=()
  for ((i=0; i<N; i++)); do
    if [[ "${STATE[i]}" != "success" ]]; then
      classify_pr "${URLS[i]}" "$TMPDIR/$i.out" &
      pids+=($!)
    fi
  done
  for p in "${pids[@]}"; do wait "$p" || true; done

  # Read results back
  for ((i=0; i<N; i++)); do
    [[ -f "$TMPDIR/$i.out" ]] || continue
    IFS=$'\t' read -r st _np _nf _npend _no _det < "$TMPDIR/$i.out"
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

  done_count=0; pending_count=0; failed_count=0; error_count=0
  for ((i=0; i<N; i++)); do
    if [[ -f "$TMPDIR/$i.out" ]]; then
      IFS=$'\t' read -r st np nf npend no det < "$TMPDIR/$i.out"
    else
      st="pending"; np=0; nf=0; npend=0; no=0; det=""
    fi
    printf '\033[K'
    render_row "${LABELS[i]}" "$st" "$np" "$nf" "$npend" "$no" "$det"
    case "$st" in
      success) done_count=$((done_count+1)) ;;
      failure) failed_count=$((failed_count+1)) ;;
      pending) pending_count=$((pending_count+1)) ;;
      error)   error_count=$((error_count+1)) ;;
    esac
  done
  printf '\033[K  %s%d/%d green%s  %s%d failing%s  %s%d running%s  %s%d gh-error%s\n' \
    "$GREEN"  "$done_count"    "$N"            "$RESET" \
    "$RED"    "$failed_count"                  "$RESET" \
    "$YELLOW" "$pending_count"                 "$RESET" \
    "$BLUE"   "$error_count"                   "$RESET"
  prev_lines=$((N + 2))

  # Termination conditions
  if (( ONCE )); then
    break
  fi
  if (( INTERRUPTED )); then
    echo "${YELLOW}Interrupted — exiting.${RESET}" >&2
    break
  fi
  # Done only when *every* PR is green. Failures keep being polled in case
  # of a restart; user must Ctrl-C if they don't intend to restart them.
  if (( pending_count == 0 && failed_count == 0 && error_count == 0 )); then
    break
  fi

  # Interruptible sleep (1s ticks so Ctrl-C feels snappy)
  for ((s=0; s<INTERVAL; s++)); do
    (( INTERRUPTED )) && break
    sleep 1
  done
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

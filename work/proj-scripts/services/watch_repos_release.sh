#!/usr/bin/env bash
# watch_repos_release.sh
# Per-repo: verify (a) every workflow run on the default-branch HEAD commit is green
# and (b) a release tag has been created for that HEAD (release pipeline finished).
#
# Loops until every repo is fully released, or until Ctrl-C / --once.
# Failed PRs/repos keep being polled — restarted pipelines are picked up automatically.
#
# Input formats (auto-detected, mixable, dedup'd):
#   • https://<host>/<owner>/<repo>/pull/<n>            (PR URL — pr number ignored)
#   • https://<host>/<owner>/<repo>                     (repo URL)
#   • <owner>/<repo>                                    (slug, host defaults to $GH_DEFAULT_HOST)
#
# Usage:
#   watch_repos_release.sh <input> [<input> ...]
#   watch_repos_release.sh -f list.txt
#   cat list.txt | watch_repos_release.sh
#
# Options:
#   -f, --file <path>      Read inputs from file (one per line, '#' comments ok)
#   -i, --interval <sec>   Poll interval in seconds (default: 60)
#   -1, --once             Run a single check pass and exit
#       --host <host>      Default GitHub host for slug inputs (default: code.rbi.tech)
#       --no-color         Disable ANSI colors
#   -h, --help             Show this help
#
# Exit code:
#   0   every repo is green AND has a release tag
#   1   interrupted with one or more repos in failure
#   2   usage / setup error
#   130 interrupted while still polling

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
INTERVAL=60
ONCE=0
DEFAULT_HOST="${GH_DEFAULT_HOST:-code.rbi.tech}"
INPUTS=()
INPUT_FILE=""

usage() { sed -n '2,32p' "$0" | sed 's/^# \{0,1\}//'; }

while (($#)); do
  case "$1" in
    -f|--file)     INPUT_FILE="${2:-}"; shift 2 ;;
    -i|--interval) INTERVAL="${2:-60}"; shift 2 ;;
    -1|--once)     ONCE=1; shift ;;
       --host)     DEFAULT_HOST="${2:-}"; shift 2 ;;
       --no-color) RED=''; GREEN=''; YELLOW=''; BLUE=''; CYAN=''; GREY=''; BOLD=''; RESET=''; shift ;;
    -h|--help)     usage; exit 0 ;;
    --)            shift; while (($#)); do INPUTS+=("$1"); shift; done ;;
    -*)            echo "${RED}Unknown option: $1${RESET}" >&2; exit 2 ;;
    *)             INPUTS+=("$1"); shift ;;
  esac
done

read_lines_into_inputs() {
  while IFS= read -r line; do
    line="${line%%#*}"
    line="${line//[$'\t\r\n ']/}"
    [[ -z "$line" ]] && continue
    INPUTS+=("$line")
  done
}

[[ -n "$INPUT_FILE" ]] && read_lines_into_inputs < "$INPUT_FILE"
[[ ${#INPUTS[@]} -eq 0 && ! -t 0 ]] && read_lines_into_inputs

if [[ ${#INPUTS[@]} -eq 0 ]]; then
  echo "${RED}No inputs provided.${RESET}" >&2; usage; exit 2
fi

# ── preflight ─────────────────────────────────────────────────────────────────
command -v gh >/dev/null || { echo "${RED}gh CLI not installed${RESET}" >&2; exit 2; }
command -v jq >/dev/null || { echo "${RED}jq not installed${RESET}" >&2; exit 2; }

# ── parse inputs → host/owner/repo, dedup ─────────────────────────────────────
parse_input() { # echoes "host<TAB>owner<TAB>repo" or empty on failure
  local in="$1" host owner repo path
  if [[ "$in" =~ ^https?://([^/]+)/(.+)$ ]]; then
    host="${BASH_REMATCH[1]}"
    path="${BASH_REMATCH[2]}"
    path="${path%/}"
    # strip /pull/N(/...) or /tree/X etc.
    path="${path%%/pull/*}"
    path="${path%%/tree/*}"
    path="${path%%/blob/*}"
    path="${path%%/issues/*}"
    IFS='/' read -r owner repo _ <<<"$path"
  elif [[ "$in" =~ ^([^/]+)/([^/]+)$ ]]; then
    host="$DEFAULT_HOST"; owner="${BASH_REMATCH[1]}"; repo="${BASH_REMATCH[2]}"
  else
    return 1
  fi
  [[ -z "$host" || -z "$owner" || -z "$repo" ]] && return 1
  printf '%s\t%s\t%s\n' "$host" "$owner" "$repo"
}

declare -a HOSTS OWNERS REPOS LABELS
declare -A SEEN
for in in "${INPUTS[@]}"; do
  parsed=$(parse_input "$in" || true)
  if [[ -z "$parsed" ]]; then
    echo "${YELLOW}Skipping unparsable input: $in${RESET}" >&2
    continue
  fi
  IFS=$'\t' read -r h o r <<<"$parsed"
  key="$h/$o/$r"
  [[ -n "${SEEN[$key]:-}" ]] && continue
  SEEN[$key]=1
  HOSTS+=("$h"); OWNERS+=("$o"); REPOS+=("$r"); LABELS+=("$r")
done

N=${#REPOS[@]}
if (( N == 0 )); then echo "${RED}No valid repos parsed.${RESET}" >&2; exit 2; fi

# Auth check against each unique host
declare -A HOST_AUTHED
for h in "${HOSTS[@]}"; do
  [[ -n "${HOST_AUTHED[$h]:-}" ]] && continue
  if ! GH_HOST="$h" gh auth status >/dev/null 2>&1; then
    echo "${RED}gh not authenticated for host: $h (gh auth login -h $h)${RESET}" >&2
    exit 2
  fi
  HOST_AUTHED[$h]=1
done

# Column width
LABEL_W=0
for l in "${LABELS[@]}"; do (( ${#l} > LABEL_W )) && LABEL_W=${#l}; done

TMPDIR=$(mktemp -d -t watch_repos_release.XXXXXX)
cleanup() { rm -rf "$TMPDIR"; }
trap cleanup EXIT
INTERRUPTED=0
trap 'INTERRUPTED=1' INT

# ── classify one repo ─────────────────────────────────────────────────────────
classify_repo() {
  local host="$1" owner="$2" repo="$3" out="$4"
  local err="$out.err"
  : >"$err"

  local gql
  gql=$(GH_HOST="$host" gh api graphql -f query='
    query($owner:String!,$repo:String!){
      repository(owner:$owner,name:$repo){
        defaultBranchRef{
          name
          target{ ... on Commit { oid committedDate } }
        }
        refs(refPrefix:"refs/tags/", first:10, orderBy:{field:TAG_COMMIT_DATE, direction:DESC}){
          nodes{
            name
            target{
              __typename
              ... on Commit { oid committedDate }
              ... on Tag { tagger{ date } target{ ... on Commit { oid committedDate } } }
            }
          }
        }
      }
    }' -f "owner=$owner" -f "repo=$repo" 2>>"$err") || {
      printf 'error\t-\t-\t-\t-\t-\t%s\n' "$(head -c 140 "$err" | tr '\n\t' '  ')" >"$out"
      return
  }

  local default_branch head_oid head_date
  default_branch=$(jq -r '.data.repository.defaultBranchRef.name // empty' <<<"$gql")
  head_oid=$(jq -r '.data.repository.defaultBranchRef.target.oid // empty' <<<"$gql")
  head_date=$(jq -r '.data.repository.defaultBranchRef.target.committedDate // empty' <<<"$gql")
  if [[ -z "$head_oid" ]]; then
    printf 'error\t-\t-\t-\t-\t-\tno default branch / commit\n' >"$out"
    return
  fi

  # Workflow runs on HEAD commit
  local runs
  runs=$(GH_HOST="$host" gh api \
    "repos/$owner/$repo/actions/runs?head_sha=$head_oid&per_page=50" 2>>"$err") || {
      printf 'error\t-\t-\t-\t-\t-\t%s\n' "$(head -c 140 "$err" | tr '\n\t' '  ')" >"$out"
      return
  }

  local counts
  counts=$(jq -r '
    [
      ([.workflow_runs[] | select(.status=="completed" and (.conclusion=="success" or .conclusion=="skipped" or .conclusion=="neutral"))] | length),
      ([.workflow_runs[] | select(.status=="completed" and (.conclusion=="failure" or .conclusion=="cancelled" or .conclusion=="timed_out" or .conclusion=="startup_failure" or .conclusion=="action_required" or .conclusion=="stale"))] | length),
      ([.workflow_runs[] | select(.status!="completed")] | length),
      ([.workflow_runs[]] | length),
      ([.workflow_runs[] | select(.status=="completed") | .updated_at] | sort | last // "")
    ] | @tsv' <<<"$runs")
  IFS=$'\t' read -r n_pass n_fail n_pending n_total last_finish <<<"$counts"

  local fail_names="" pending_names=""
  if (( n_fail > 0 )); then
    fail_names=$(jq -r '[.workflow_runs[] | select(.status=="completed" and (.conclusion=="failure" or .conclusion=="cancelled" or .conclusion=="timed_out" or .conclusion=="startup_failure" or .conclusion=="action_required" or .conclusion=="stale")) | .name] | unique | join(", ")' <<<"$runs")
  fi
  if (( n_pending > 0 )); then
    pending_names=$(jq -r '[.workflow_runs[] | select(.status!="completed") | .name] | unique | join(", ")' <<<"$runs")
  fi

  # Tag detection
  # Strong: any tag target oid == HEAD oid.
  # Fallback: any tag (committedDate or tagger.date) > last_finish.
  local tag_match="" tag_via=""
  tag_match=$(jq -r --arg head "$head_oid" '
    .data.repository.refs.nodes[]
    | (.target | (
        if .__typename=="Commit" then .oid
        elif .__typename=="Tag"  then (.target.oid // "")
        else "" end))
      as $oid
    | select($oid == $head)
    | .name' <<<"$gql" | head -1)
  if [[ -n "$tag_match" ]]; then
    tag_via="oid"
  elif [[ -n "$last_finish" ]]; then
    tag_match=$(jq -r --arg t "$last_finish" '
      .data.repository.refs.nodes[]
      | (if .target.__typename=="Tag" then (.target.tagger.date // .target.target.committedDate)
         else .target.committedDate end) as $d
      | select($d != null and $d > $t)
      | .name' <<<"$gql" | head -1)
    [[ -n "$tag_match" ]] && tag_via="date"
  fi

  # Latest tag (for display when nothing matches yet)
  local latest_tag
  latest_tag=$(jq -r '.data.repository.refs.nodes[0].name // ""' <<<"$gql")

  # Classify
  local status detail=""
  if (( n_pending > 0 )); then
    status=pending
    detail="runs: $pending_names"
  elif (( n_fail > 0 )); then
    status=failure
    detail="failed: $fail_names"
  elif (( n_total == 0 )); then
    # No runs at all on HEAD. Could be a docs-only commit. Treat as success only if a tag matches.
    if [[ -n "$tag_match" ]]; then
      status=success
    else
      status=pending
      detail="no runs on $head_oid"
    fi
  elif [[ -n "$tag_match" ]]; then
    status=success
  else
    status=pending
    detail="awaiting release tag (latest: ${latest_tag:-none})"
  fi

  # TSV: status \t pass \t fail \t running \t head_short \t tag_display \t detail
  local head_short="${head_oid:0:7}"
  local tag_display
  if [[ -n "$tag_match" ]]; then
    tag_display="$tag_match"
    [[ "$tag_via" == "date" ]] && tag_display="$tag_display*"
  else
    tag_display="${latest_tag:-—}?"
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$status" "$n_pass" "$n_fail" "$n_pending" "$head_short" "$tag_display" "$detail" \
    >"$out"
}

# ── render ────────────────────────────────────────────────────────────────────
render_row() {
  local label="$1" status="$2" pass="$3" fail="$4" pending="$5" head_sha="$6" tag="$7" detail="$8"
  local icon color
  case "$status" in
    success) icon="✔"; color="$GREEN"  ;;
    failure) icon="✖"; color="$RED"    ;;
    pending) icon="…"; color="$YELLOW" ;;
    error)   icon="?"; color="$BLUE"   ;;
    *)       icon="·"; color="$GREY"   ;;
  esac
  printf '  %s%s%s  %-*s  %s%s✓%s %s%s✗%s %s%s…%s   %s%s%s   tag:%s%s%s' \
    "$color" "$icon" "$RESET" \
    "$LABEL_W" "$label" \
    "$GREEN" "$pass"    "$RESET" \
    "$RED"   "$fail"    "$RESET" \
    "$YELLOW" "$pending" "$RESET" \
    "$GREY" "$head_sha" "$RESET" \
    "$CYAN" "$tag"      "$RESET"
  if [[ -n "$detail" ]]; then
    printf '   %s%s%s' "$GREY" "$detail" "$RESET"
  fi
  printf '\n'
}

# ── main loop ─────────────────────────────────────────────────────────────────
declare -a STATE
for ((i=0; i<N; i++)); do STATE[i]="pending"; done

cycle=0
prev_lines=0
start_ts=$(date +%s)

while :; do
  cycle=$((cycle + 1))

  pids=()
  for ((i=0; i<N; i++)); do
    if [[ "${STATE[i]}" != "success" ]]; then
      classify_repo "${HOSTS[i]}" "${OWNERS[i]}" "${REPOS[i]}" "$TMPDIR/$i.out" &
      pids+=($!)
    fi
  done
  for p in "${pids[@]}"; do wait "$p" || true; done

  for ((i=0; i<N; i++)); do
    [[ -f "$TMPDIR/$i.out" ]] || continue
    IFS=$'\t' read -r st _np _nf _nrun _hs _tag _det < "$TMPDIR/$i.out"
    STATE[i]="$st"
  done

  if (( prev_lines > 0 )); then
    printf '\033[%dA' "$prev_lines"
  fi

  now_ts=$(date +%s)
  elapsed=$((now_ts - start_ts))
  printf '\r\033[K%s[%s] cycle %d  elapsed %ds  interval %ds  repos: %d%s\n' \
    "$BOLD" "$(date '+%H:%M:%S')" "$cycle" "$elapsed" "$INTERVAL" "$N" "$RESET"

  green=0; failing=0; running=0; errored=0
  for ((i=0; i<N; i++)); do
    if [[ -f "$TMPDIR/$i.out" ]]; then
      IFS=$'\t' read -r st np nf nrun hs tag det < "$TMPDIR/$i.out"
    else
      st="pending"; np=0; nf=0; nrun=0; hs="-"; tag="-"; det=""
    fi
    printf '\033[K'
    render_row "${LABELS[i]}" "$st" "$np" "$nf" "$nrun" "$hs" "$tag" "$det"
    case "$st" in
      success) green=$((green+1)) ;;
      failure) failing=$((failing+1)) ;;
      pending) running=$((running+1)) ;;
      error)   errored=$((errored+1)) ;;
    esac
  done
  printf '\033[K  %s%d/%d released%s  %s%d failing%s  %s%d in-progress%s  %s%d gh-error%s\n' \
    "$GREEN"  "$green"  "$N"      "$RESET" \
    "$RED"    "$failing"           "$RESET" \
    "$YELLOW" "$running"           "$RESET" \
    "$BLUE"   "$errored"           "$RESET"
  prev_lines=$((N + 2))

  if (( ONCE )); then break; fi
  if (( INTERRUPTED )); then echo "${YELLOW}Interrupted — exiting.${RESET}" >&2; break; fi
  if (( running == 0 && failing == 0 && errored == 0 )); then break; fi

  for ((s=0; s<INTERVAL; s++)); do
    (( INTERRUPTED )) && break
    sleep 1
  done
done

echo
echo "${BOLD}Summary:${RESET}"
fail_total=0
for ((i=0; i<N; i++)); do
  st="${STATE[i]}"
  [[ "$st" == "failure" ]] && fail_total=$((fail_total+1))
  printf '  %-9s  %s/%s\n' "$st" "${OWNERS[i]}" "${REPOS[i]}"
done

if (( INTERRUPTED && fail_total > 0 )); then exit 1; fi
if (( INTERRUPTED )); then exit 130; fi
if (( fail_total > 0 )); then exit 1; fi
exit 0

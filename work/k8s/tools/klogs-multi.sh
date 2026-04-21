#!/usr/bin/env bash
set -euo pipefail

# ── klogs-multi ───────────────────────────────────────────────────────────────
# Collect logs from multiple Kubernetes deployments into per-pod files.
# Same behavior as klogs but supports multiple deployments.
# Requires: kubectl, fzf (for interactive selection)
# ──────────────────────────────────────────────────────────────────────────────

# ── Platform detection ─────────────────────────────────────────────────────────
OS="$(uname -s)"
case "$OS" in
    Darwin)
        command -v gsed &>/dev/null || { echo "ERROR: gsed is required on macOS (brew install gnu-sed)" >&2; exit 1; }
        SED="gsed"
        ;;
    Linux)
        SED="sed"
        ;;
    *)
        echo "ERROR: Unsupported platform: $OS" >&2; exit 1
        ;;
esac

# ── Defaults ──────────────────────────────────────────────────────────────────
NAMESPACE=""
CONTEXT=""
CONTAINER=""
FOLLOW=false
PREVIOUS=false
LIVE=false
OUTPUT=""
DEPLOYMENTS=()

# ── Colors ────────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m'

info()  { echo -e "${CYAN}[INFO]${NC} $*"; }
warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }
die()   { error "$@"; exit 1; }

# ── Usage ─────────────────────────────────────────────────────────────────────
usage() {
    cat <<EOF
${BOLD}klogs-multi${NC} — Kubernetes log collector for multiple deployments

${BOLD}USAGE${NC}
    klogs-multi [OPTIONS] [deploy1 deploy2 ...]

    If no deployments are given, opens fzf for interactive multi-select.
    In fzf, use TAB to select multiple items, then ENTER to confirm.

${BOLD}OPTIONS${NC}
    -n, --namespace <ns>    Kubernetes namespace (interactive if omitted)
    --context <ctx>         Kubernetes context (interactive if omitted)
    -c, --container <name>  Specific container within the pod
    -f, --follow            Dump existing logs then stream new ones
    -p, --previous          Collect logs from previous container instance
                            Combine with -f: dump previous + current + follow
    --live                  Stream only new logs from now (no history)
    --clean                 Remove all previously saved log files in ./klogs/
    -o, --output <path>     Output directory path (auto-generated if omitted)
    -h, --help              Show this help

${BOLD}EXAMPLES${NC}
    klogs-multi                              # interactive multi-select
    klogs-multi api-gateway user-service     # specific deployments
    klogs-multi -n production -f             # follow logs, production ns
    klogs-multi -p -f api-gateway auth-svc   # previous + current + follow
    klogs-multi --live api-gateway           # only new logs, no history
    klogs-multi --clean api-gateway          # clean old logs first

${BOLD}OUTPUT${NC}
    Logs are saved to ./klogs/<deploy>/<pod>.log
    Each deployment gets its own subdirectory with per-pod log files.
    With -p -f: previous logs in <pod>.previous.log, current in <pod>.log
    Search across all pods: rg --color=never '"ERROR"' ./klogs/**/*.log | jq .
EOF
    exit 0
}

# ── Clean ─────────────────────────────────────────────────────────────────────
clean_logs() {
    local dir="./klogs"
    if [[ ! -d "$dir" ]]; then
        return
    fi
    local size
    size=$(du -sh "$dir" | cut -f1 | tr -d ' ')
    rm -rf "$dir"
    info "Removed ${BOLD}$dir/${NC} ($size)"
}

# ── Arg parsing ───────────────────────────────────────────────────────────────
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -n|--namespace)  NAMESPACE="$2"; shift 2 ;;
            --context)       CONTEXT="$2"; shift 2 ;;
            -c|--container)  CONTAINER="$2"; shift 2 ;;
            -f|--follow)     FOLLOW=true; shift ;;
            -p|--previous)   PREVIOUS=true; shift ;;
            --live)          LIVE=true; FOLLOW=true; shift ;;
            --clean)         clean_logs; shift ;;
            -o|--output)     OUTPUT="$2"; shift 2 ;;
            -h|--help)       usage ;;
            -*)              die "Unknown option: $1 (see --help)" ;;
            *)               DEPLOYMENTS+=("$1"); shift ;;
        esac
    done
}

# ── Interactive selectors ─────────────────────────────────────────────────────
has_fzf() { command -v fzf &>/dev/null; }

select_with_menu() {
    local prompt="$1"
    shift
    local items=("$@")

    if has_fzf; then
        printf '%s\n' "${items[@]}" | fzf --prompt="$prompt " --height=15 --reverse
    else
        echo -e "${BOLD}$prompt${NC}" >&2
        local i=1
        for item in "${items[@]}"; do
            echo "  $i) $item" >&2
            ((++i))
        done
        local choice
        read -rp "Enter number (or press Enter for current): " choice
        if [[ -z "$choice" ]]; then
            return 1  # signal: use default
        fi
        if [[ "$choice" -ge 1 && "$choice" -le ${#items[@]} ]] 2>/dev/null; then
            echo "${items[$((choice - 1))]}"
        else
            die "Invalid selection: $choice"
        fi
    fi
}

select_context() {
    if [[ -n "$CONTEXT" ]]; then
        return
    fi

    local current
    current=$(kubectl config current-context 2>/dev/null || echo "")

    local contexts
    mapfile -t contexts < <(kubectl config get-contexts -o name 2>/dev/null)

    if [[ ${#contexts[@]} -eq 0 ]]; then
        die "No Kubernetes contexts found"
    fi

    if [[ ${#contexts[@]} -eq 1 ]]; then
        CONTEXT="${contexts[0]}"
        info "Using only available context: ${BOLD}$CONTEXT${NC}"
        return
    fi

    info "Current context: ${BOLD}${current:-none}${NC}"
    local selected
    if selected=$(select_with_menu "Select context:" "${contexts[@]}"); then
        CONTEXT="$selected"
    else
        CONTEXT="$current"
    fi
    info "Using context: ${BOLD}$CONTEXT${NC}"
}

select_namespace() {
    if [[ -n "$NAMESPACE" ]]; then
        return
    fi

    local ctx_flag=()
    [[ -n "$CONTEXT" ]] && ctx_flag=(--context "$CONTEXT")

    local current
    current=$(kubectl "${ctx_flag[@]}" config view --minify -o jsonpath='{..namespace}' 2>/dev/null || echo "default")
    [[ -z "$current" ]] && current="default"

    local namespaces
    mapfile -t namespaces < <(kubectl "${ctx_flag[@]}" get namespaces -o jsonpath='{.items[*].metadata.name}' 2>/dev/null | tr ' ' '\n' | sort)

    if [[ ${#namespaces[@]} -eq 0 ]]; then
        die "No namespaces found (check permissions)"
    fi

    info "Current namespace: ${BOLD}$current${NC}"
    local selected
    if selected=$(select_with_menu "Select namespace:" "${namespaces[@]}"); then
        NAMESPACE="$selected"
    else
        NAMESPACE="$current"
    fi
    info "Using namespace: ${BOLD}$NAMESPACE${NC}"
}

# ── kubectl wrapper ───────────────────────────────────────────────────────────
kctl() {
    local flags=()
    [[ -n "$CONTEXT" ]]   && flags+=(--context "$CONTEXT")
    [[ -n "$NAMESPACE" ]] && flags+=(--namespace "$NAMESPACE")
    kubectl "${flags[@]}" "$@"
}

# ── Deployment multi-select ───────────────────────────────────────────────────
select_deployments() {
    if [[ ${#DEPLOYMENTS[@]} -gt 0 ]]; then
        for dep in "${DEPLOYMENTS[@]}"; do
            kctl get deployment "$dep" &>/dev/null \
                || die "Deployment '$dep' not found in namespace '$NAMESPACE'"
        done
        return
    fi

    local deps
    mapfile -t deps < <(kctl get deployments -o jsonpath='{.items[*].metadata.name}' 2>/dev/null | tr ' ' '\n' | sort)

    if [[ ${#deps[@]} -eq 0 ]]; then
        die "No deployments found in namespace '$NAMESPACE'"
    fi

    if has_fzf; then
        info "Select deployments (TAB to multi-select, ENTER to confirm):"
        local selected
        selected=$(printf '%s\n' "${deps[@]}" | fzf --multi --prompt="Deployments: " --height=20 --reverse \
            --header="TAB=select  ENTER=confirm  ESC=cancel") \
            || die "No deployments selected"
        mapfile -t DEPLOYMENTS <<< "$selected"
    else
        echo -e "${BOLD}Select deployments:${NC}" >&2
        local i=1
        for dep in "${deps[@]}"; do
            echo "  $i) $dep" >&2
            ((++i))
        done
        local choices
        read -rp "Enter numbers (comma-separated, e.g. 1,3,5): " choices
        [[ -z "$choices" ]] && die "No deployments selected"
        IFS=',' read -ra nums <<< "$choices"
        for num in "${nums[@]}"; do
            num=$(echo "$num" | tr -d ' ')
            if [[ "$num" -ge 1 && "$num" -le ${#deps[@]} ]] 2>/dev/null; then
                DEPLOYMENTS+=("${deps[$((num - 1))]}")
            else
                die "Invalid selection: $num"
            fi
        done
    fi

    if [[ ${#DEPLOYMENTS[@]} -eq 0 ]]; then
        die "No deployments selected"
    fi

    info "Selected ${BOLD}${#DEPLOYMENTS[@]}${NC} deployment(s): ${DEPLOYMENTS[*]}"
}

# ── Pod discovery ─────────────────────────────────────────────────────────────
# Parallel arrays: ALL_PODS[i] belongs to POD_DEPLOY[i]
ALL_PODS=()
POD_DEPLOY=()

get_pods() {
    for dep in "${DEPLOYMENTS[@]}"; do
        local selector
        selector=$(kctl get deployment "$dep" -o jsonpath='{.spec.selector.matchLabels}' 2>/dev/null) \
            || die "Failed to get selector for deployment '$dep'"

        local label_selector
        label_selector=$(echo "$selector" | sed 's/[{}"]//g' | sed 's/:/=/g')

        local pods
        mapfile -t pods < <(kctl get pods -l "$label_selector" -o jsonpath='{.items[*].metadata.name}' | tr ' ' '\n')

        if [[ ${#pods[@]} -eq 0 ]]; then
            warn "No pods found for deployment '$dep' (skipping)"
            continue
        fi

        for pod in "${pods[@]}"; do
            [[ -z "$pod" ]] && continue
            ALL_PODS+=("$pod")
            POD_DEPLOY+=("$dep")
        done
    done

    if [[ ${#ALL_PODS[@]} -eq 0 ]]; then
        die "No pods found for any selected deployment"
    fi

    info "Found ${BOLD}${#ALL_PODS[@]}${NC} pod(s) across ${BOLD}${#DEPLOYMENTS[@]}${NC} deployment(s)"
}

# ── Output directory ──────────────────────────────────────────────────────────
OUTDIR=""
TIMESTAMP=""

setup_output() {
    TIMESTAMP=$(date +%Y-%m-%d_%H%M%S)
    if [[ -n "$OUTPUT" ]]; then
        OUTDIR="$OUTPUT"
    else
        OUTDIR="./klogs"
    fi
    mkdir -p "$OUTDIR"
}

pod_file() {
    local dep="$1" pod="$2" suffix="${3:-}"
    local dir="$OUTDIR/$dep"
    mkdir -p "$dir"
    if [[ -n "$suffix" ]]; then
        echo "$dir/${pod}.${suffix}.log"
    else
        echo "$dir/${pod}.log"
    fi
}

# ── Log collection ────────────────────────────────────────────────────────────
collect_logs() {
    setup_output

    # Strip leading k8s RFC3339 timestamps and ANSI color codes
    local ts_pattern='s/^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]*Z //'
    local ansi_pattern='s/\x1b\[[0-9;]*m//g'

    # Join multi-line JSON into single lines (streaming-safe)
    local join_json='
        /^\{/ { if (buf != "") { print buf; fflush() } buf = $0; next }
        buf != "" { buf = buf $0 }
        END { if (buf != "") print buf }'

    local container_flag=()
    [[ -n "$CONTAINER" ]] && container_flag=(-c "$CONTAINER")

    info "Collecting logs → ${BOLD}$OUTDIR/${NC}"

    local pids=()
    trap 'kill "${pids[@]}" 2>/dev/null; print_summary; exit 0' INT TERM

    # Helper: pipe kubectl logs through cleanup filters
    filter_json() {
        $SED -u -E -e "$ts_pattern" -e "$ansi_pattern" | awk "$join_json"
    }

    # ── Mode: -p -f  (previous + current + follow) ───────────────────────
    if [[ "$PREVIOUS" == true && "$FOLLOW" == true ]]; then
        info "Dumping previous container logs..."
        for i in "${!ALL_PODS[@]}"; do
            local pod="${ALL_PODS[$i]}" dep="${POD_DEPLOY[$i]}"
            local outfile
            outfile=$(pod_file "$dep" "$pod" "previous")
            info "  [previous] ${BOLD}$dep/$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" --previous 2>/dev/null || true; } \
                | filter_json > "$outfile"
        done
        info "Dumping current logs + following... Press ${BOLD}Ctrl+C${NC} to stop"
        for i in "${!ALL_PODS[@]}"; do
            local pod="${ALL_PODS[$i]}" dep="${POD_DEPLOY[$i]}"
            local outfile
            outfile=$(pod_file "$dep" "$pod")
            info "  [follow] ${BOLD}$dep/$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" -f 2>/dev/null || true; } \
                | filter_json > "$outfile" &
            pids+=($!)
        done
        wait "${pids[@]}" 2>/dev/null || true

    # ── Mode: --live  (only new logs, no history) ─────────────────────────
    elif [[ "$LIVE" == true ]]; then
        info "Following new logs only... Press ${BOLD}Ctrl+C${NC} to stop"
        for i in "${!ALL_PODS[@]}"; do
            local pod="${ALL_PODS[$i]}" dep="${POD_DEPLOY[$i]}"
            local outfile
            outfile=$(pod_file "$dep" "$pod")
            info "  [live] ${BOLD}$dep/$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" -f --tail=0 2>/dev/null || true; } \
                | filter_json > "$outfile" &
            pids+=($!)
        done
        wait "${pids[@]}" 2>/dev/null || true

    # ── Mode: -f  (current from start + follow) ──────────────────────────
    elif [[ "$FOLLOW" == true ]]; then
        info "Following logs... Press ${BOLD}Ctrl+C${NC} to stop"
        for i in "${!ALL_PODS[@]}"; do
            local pod="${ALL_PODS[$i]}" dep="${POD_DEPLOY[$i]}"
            local outfile
            outfile=$(pod_file "$dep" "$pod")
            info "  [follow] ${BOLD}$dep/$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" -f 2>/dev/null || true; } \
                | filter_json > "$outfile" &
            pids+=($!)
        done
        wait "${pids[@]}" 2>/dev/null || true

    # ── Mode: snapshot (default, or -p only) ──────────────────────────────
    else
        local extra_flags=()
        [[ "$PREVIOUS" == true ]] && extra_flags+=(--previous)
        for i in "${!ALL_PODS[@]}"; do
            local pod="${ALL_PODS[$i]}" dep="${POD_DEPLOY[$i]}"
            local outfile
            outfile=$(pod_file "$dep" "$pod")
            info "  Collecting ${BOLD}$dep/$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" "${extra_flags[@]}" 2>/dev/null || true; } \
                | filter_json > "$outfile"
        done
    fi

    trap - INT TERM
    print_summary
}

print_summary() {
    echo ""
    echo -e "${GREEN}${BOLD}Done!${NC}"
    echo -e "  Dir: ${BOLD}$OUTDIR/${NC}"
    local total=0
    for dep_dir in "$OUTDIR"/*/; do
        [[ -d "$dep_dir" ]] || continue
        local dep_name
        dep_name=$(basename "$dep_dir")
        echo -e "  ${BOLD}$dep_name/${NC}"
        for f in "$dep_dir"*.log; do
            [[ -f "$f" ]] || continue
            local lines size name
            lines=$(wc -l < "$f" | tr -d ' ')
            size=$(du -h "$f" | cut -f1 | tr -d ' ')
            name=$(basename "$f")
            echo -e "    ${name}: ${lines} lines (${size})"
            total=$((total + lines))
        done
    done
    echo -e "  Total: ${BOLD}$total${NC} lines"
    echo ""
    echo -e "  Search: ${CYAN}rg --color=never '\"ERROR\"' $OUTDIR/**/*.log | jq .${NC}"
}

# ── Main ──────────────────────────────────────────────────────────────────────
main() {
    parse_args "$@"
    select_context
    select_namespace
    select_deployments
    get_pods
    collect_logs
}

main "$@"

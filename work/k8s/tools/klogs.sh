#!/usr/bin/env bash
set -euo pipefail

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
RESOURCE_TYPE=""
RESOURCE_NAME=""

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
${BOLD}klogs${NC} — Kubernetes log collector

${BOLD}USAGE${NC}
    klogs [OPTIONS] <resource>

${BOLD}RESOURCE FORMATS${NC}
    deploy/name    deployment/name    Collect logs from all pods in a deployment
    pod/name                          Collect logs from a specific pod
    name                              Auto-detect (tries deployment, then pod)

${BOLD}OPTIONS${NC}
    -n, --namespace <ns>    Kubernetes namespace (interactive if omitted)
    --context <ctx>         Kubernetes context (interactive if omitted)
    -c, --container <name>  Specific container within the pod
    -f, --follow            Dump existing logs then stream new ones
    -p, --previous          Collect logs from previous container instance
                            Combine with -f: dump previous + current + follow
    --live                  Stream only new logs from now (no history)
    --clean                 Remove all previously saved log files in ./klogs/
    -o, --output <path>     Output file path (auto-generated if omitted)
    -h, --help              Show this help

${BOLD}EXAMPLES${NC}
    klogs deploy/my-app
    klogs deploy/my-app -n production --context prod-cluster
    klogs deploy/my-app -f              # current logs from start + follow
    klogs deploy/my-app -p -f           # previous + current + follow
    klogs deploy/my-app --live          # only new logs, no history
    klogs my-app -c sidecar --previous  # previous logs only (snapshot)

${BOLD}OUTPUT${NC}
    Logs are saved to ./klogs/<name>-<timestamp>/<pod>.log (one file per pod)
    With -p -f: previous logs in <pod>.previous.log, current in <pod>.log
    Search across all pods: rg --color=never '"ERROR"' ./klogs/<name>-*/*.log | jq .
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
    local positional=()

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
            *)               positional+=("$1"); shift ;;
        esac
    done

    [[ ${#positional[@]} -eq 0 ]] && die "Missing resource argument (see --help)"
    [[ ${#positional[@]} -gt 1 ]] && die "Too many positional arguments: ${positional[*]}"

    local resource="${positional[0]}"
    if [[ "$resource" == */* ]]; then
        RESOURCE_TYPE="${resource%%/*}"
        RESOURCE_NAME="${resource#*/}"
    else
        RESOURCE_TYPE="auto"
        RESOURCE_NAME="$resource"
    fi

    # Normalize type
    case "$RESOURCE_TYPE" in
        deploy|deployment|deployments) RESOURCE_TYPE="deployment" ;;
        pod|pods)                      RESOURCE_TYPE="pod" ;;
        auto)                          ;; # resolved later
        *)                             die "Unsupported resource type: $RESOURCE_TYPE" ;;
    esac
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
            ((i++))
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

# ── Resource resolution ───────────────────────────────────────────────────────
resolve_resource() {
    if [[ "$RESOURCE_TYPE" == "auto" ]]; then
        if kctl get deployment "$RESOURCE_NAME" &>/dev/null; then
            RESOURCE_TYPE="deployment"
            info "Auto-detected as deployment"
        elif kctl get pod "$RESOURCE_NAME" &>/dev/null; then
            RESOURCE_TYPE="pod"
            info "Auto-detected as pod"
        else
            die "Resource '$RESOURCE_NAME' not found as deployment or pod in namespace '$NAMESPACE'"
        fi
    fi
}

get_pods() {
    local pods=()

    if [[ "$RESOURCE_TYPE" == "deployment" ]]; then
        local selector
        selector=$(kctl get deployment "$RESOURCE_NAME" -o jsonpath='{.spec.selector.matchLabels}' 2>/dev/null) \
            || die "Deployment '$RESOURCE_NAME' not found in namespace '$NAMESPACE'"

        # Convert {"key":"val","key2":"val2"} to key=val,key2=val2
        local label_selector
        label_selector=$(echo "$selector" | sed 's/[{}"]//g' | sed 's/:/=/g' | sed 's/,/,/g')

        mapfile -t pods < <(kctl get pods -l "$label_selector" -o jsonpath='{.items[*].metadata.name}' | tr ' ' '\n')

        if [[ ${#pods[@]} -eq 0 ]]; then
            die "No pods found for deployment '$RESOURCE_NAME'"
        fi

        info "Found ${BOLD}${#pods[@]}${NC} pod(s) for deployment '$RESOURCE_NAME'" >&2
    else
        kctl get pod "$RESOURCE_NAME" &>/dev/null \
            || die "Pod '$RESOURCE_NAME' not found in namespace '$NAMESPACE'"
        pods=("$RESOURCE_NAME")
    fi

    printf '%s\n' "${pods[@]}"
}

# ── Output directory ──────────────────────────────────────────────────────────
OUTDIR=""
TIMESTAMP=""

setup_output() {
    TIMESTAMP=$(date +%Y-%m-%d_%H%M%S)
    if [[ -n "$OUTPUT" ]]; then
        OUTDIR="$OUTPUT"
    else
        OUTDIR="./klogs/${RESOURCE_NAME}-${TIMESTAMP}"
    fi
    mkdir -p "$OUTDIR"
}

pod_file() {
    local pod="$1" suffix="${2:-}"
    if [[ -n "$suffix" ]]; then
        echo "$OUTDIR/${pod}.${suffix}.log"
    else
        echo "$OUTDIR/${pod}.log"
    fi
}

# ── Log collection ────────────────────────────────────────────────────────────
collect_logs() {
    local pods=()
    mapfile -t pods < <(get_pods)

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
        for pod in "${pods[@]}"; do
            local outfile
            outfile=$(pod_file "$pod" "previous")
            info "  [previous] ${BOLD}$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" --previous 2>/dev/null || true; } \
                | filter_json > "$outfile"
        done
        info "Dumping current logs + following... Press ${BOLD}Ctrl+C${NC} to stop"
        for pod in "${pods[@]}"; do
            local outfile
            outfile=$(pod_file "$pod")
            info "  [follow] ${BOLD}$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" -f 2>/dev/null || true; } \
                | filter_json > "$outfile" &
            pids+=($!)
        done
        wait "${pids[@]}" 2>/dev/null || true

    # ── Mode: --live  (only new logs, no history) ─────────────────────────
    elif [[ "$LIVE" == true ]]; then
        info "Following new logs only... Press ${BOLD}Ctrl+C${NC} to stop"
        for pod in "${pods[@]}"; do
            local outfile
            outfile=$(pod_file "$pod")
            info "  [live] ${BOLD}$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" -f --tail=0 2>/dev/null || true; } \
                | filter_json > "$outfile" &
            pids+=($!)
        done
        wait "${pids[@]}" 2>/dev/null || true

    # ── Mode: -f  (current from start + follow) ──────────────────────────
    elif [[ "$FOLLOW" == true ]]; then
        info "Following logs... Press ${BOLD}Ctrl+C${NC} to stop"
        for pod in "${pods[@]}"; do
            local outfile
            outfile=$(pod_file "$pod")
            info "  [follow] ${BOLD}$pod${NC} → $outfile"
            { kctl logs "$pod" "${container_flag[@]}" -f 2>/dev/null || true; } \
                | filter_json > "$outfile" &
            pids+=($!)
        done
        wait "${pids[@]}" 2>/dev/null || true

    # ── Mode: snapshot (default, or -p only) ──────────────────────────────
    else
        local extra_flags=()
        [[ "$PREVIOUS" == true ]] && extra_flags+=(--previous)
        for pod in "${pods[@]}"; do
            local outfile
            outfile=$(pod_file "$pod")
            info "  Collecting ${BOLD}$pod${NC} → $outfile"
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
    for f in "$OUTDIR"/*.log; do
        [[ -f "$f" ]] || continue
        local lines size name
        lines=$(wc -l < "$f" | tr -d ' ')
        size=$(du -h "$f" | cut -f1 | tr -d ' ')
        name=$(basename "$f")
        echo -e "  ${name}: ${lines} lines (${size})"
        total=$((total + lines))
    done
    echo -e "  Total: ${BOLD}$total${NC} lines"
    echo ""
    echo -e "  Search: ${CYAN}rg --color=never '\"ERROR\"' $OUTDIR/*.log | jq .${NC}"
}

# ── Main ──────────────────────────────────────────────────────────────────────
main() {
    parse_args "$@"
    select_context
    select_namespace
    resolve_resource
    collect_logs
}

main "$@"

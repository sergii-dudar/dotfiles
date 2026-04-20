#!/usr/bin/env bash
set -euo pipefail

# ── klogs-multi ───────────────────────────────────────────────────────────────
# Stream logs from multiple deployments into a single color-coded real-time view.
# Requires: kubectl, fzf
# Note: Pods are resolved once at start. New pods from rollouts/scaling won't
#       be picked up automatically — restart the script if that happens.
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
TAIL_LINES=50
NO_COLOR=false
LIVE=false
DEPLOYMENTS=()

# ── Colors (8 distinct, cycle if more) ────────────────────────────────────────
# Using $'...' so variables hold actual escape bytes (safe in sed replacement)
PALETTE=(
    $'\033[0;36m'   # cyan
    $'\033[0;33m'   # yellow
    $'\033[0;32m'   # green
    $'\033[0;35m'   # magenta
    $'\033[0;34m'   # blue
    $'\033[1;31m'   # bold red
    $'\033[1;33m'   # bold yellow
    $'\033[1;36m'   # bold cyan
)
NC=$'\033[0m'
RED=$'\033[0;31m'
BOLD=$'\033[1m'
CYAN=$'\033[0;36m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[0;33m'

info()  { printf '%s\n' "${CYAN}[INFO]${NC} $*" >&2; }
warn()  { printf '%s\n' "${YELLOW}[WARN]${NC} $*" >&2; }
error() { printf '%s\n' "${RED}[ERROR]${NC} $*" >&2; }
die()   { error "$@"; exit 1; }

# ── Usage ─────────────────────────────────────────────────────────────────────
usage() {
    cat <<EOF
${BOLD}klogs-multi${NC} — Stream logs from multiple Kubernetes deployments

${BOLD}USAGE${NC}
    klogs-multi [OPTIONS] [deploy1 deploy2 ...]

    If no deployments are given, opens fzf for interactive multi-select.
    In fzf, use TAB to select multiple items, then ENTER to confirm.

${BOLD}OPTIONS${NC}
    -n, --namespace <ns>    Kubernetes namespace (interactive if omitted)
    --context <ctx>         Kubernetes context (interactive if omitted)
    -c, --container <name>  Specific container (applied to all pods)
    --tail <N>              Lines of history per pod (default: 50, 0 = no history)
    --live                  Stream only new logs (equivalent to --tail 0)
    --no-color              Disable color-coded prefixes
    -h, --help              Show this help

${BOLD}EXAMPLES${NC}
    klogs-multi                              # interactive multi-select
    klogs-multi api-gateway user-service     # specific deployments
    klogs-multi -n production --tail 100     # 100 lines history, production ns
    klogs-multi --live api-gateway auth-svc  # only new logs from now

${BOLD}OUTPUT FORMAT${NC}
    Each line is prefixed with a color-coded [deployment/pod] tag:
      [api-gateway/api-gateway-7f8b9-x2k4l] {"level":"info",...}
      [user-service/user-service-5c6d7-m3n8p] {"level":"debug",...}

    Press Ctrl+C to stop all streams.
EOF
    exit 0
}

# ── Arg parsing ───────────────────────────────────────────────────────────────
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -n|--namespace)  NAMESPACE="$2"; shift 2 ;;
            --context)       CONTEXT="$2"; shift 2 ;;
            -c|--container)  CONTAINER="$2"; shift 2 ;;
            --tail)          TAIL_LINES="$2"; shift 2
                             [[ "$TAIL_LINES" =~ ^[0-9]+$ ]] || die "--tail must be a non-negative integer, got: '$TAIL_LINES'" ;;
            --live)          LIVE=true; TAIL_LINES=0; shift ;;
            --no-color)      NO_COLOR=true; shift ;;
            -h|--help)       usage ;;
            -*)              die "Unknown option: $1 (see --help)" ;;
            *)               DEPLOYMENTS+=("$1"); shift ;;
        esac
    done
}

# ── Interactive selectors ─────────────────────────────────────────────────────
select_context() {
    if [[ -n "$CONTEXT" ]]; then return; fi

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
    selected=$(printf '%s\n' "${contexts[@]}" | fzf --prompt="Select context: " --height=15 --reverse) \
        || CONTEXT="$current"
    [[ -n "$selected" ]] && CONTEXT="$selected"
    info "Using context: ${BOLD}$CONTEXT${NC}"
}

select_namespace() {
    if [[ -n "$NAMESPACE" ]]; then return; fi

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
    selected=$(printf '%s\n' "${namespaces[@]}" | fzf --prompt="Select namespace: " --height=15 --reverse) \
        || NAMESPACE="$current"
    [[ -n "$selected" ]] && NAMESPACE="$selected"
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
        # Validate provided deployment names
        for dep in "${DEPLOYMENTS[@]}"; do
            kctl get deployment "$dep" &>/dev/null \
                || die "Deployment '$dep' not found in namespace '$NAMESPACE'"
        done
        return
    fi

    command -v fzf &>/dev/null || die "fzf is required for interactive selection (or pass deployment names as arguments)"

    local deps
    mapfile -t deps < <(kctl get deployments -o jsonpath='{.items[*].metadata.name}' 2>/dev/null | tr ' ' '\n' | sort)

    if [[ ${#deps[@]} -eq 0 ]]; then
        die "No deployments found in namespace '$NAMESPACE'"
    fi

    info "Select deployments (TAB to multi-select, ENTER to confirm):"
    local selected
    selected=$(printf '%s\n' "${deps[@]}" | fzf --multi --prompt="Deployments: " --height=20 --reverse \
        --header="TAB=select  ENTER=confirm  ESC=cancel") \
        || die "No deployments selected"

    mapfile -t DEPLOYMENTS <<< "$selected"

    if [[ ${#DEPLOYMENTS[@]} -eq 0 ]]; then
        die "No deployments selected"
    fi

    info "Selected ${BOLD}${#DEPLOYMENTS[@]}${NC} deployment(s): ${DEPLOYMENTS[*]}"
}

# ── Pod discovery ─────────────────────────────────────────────────────────────
# Populates parallel arrays: ALL_PODS[i] and POD_DEPLOY[i]
ALL_PODS=()
POD_DEPLOY=()

discover_pods() {
    local total=0

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
            ((total++))
        done
    done

    if [[ $total -eq 0 ]]; then
        die "No pods found for any selected deployment"
    fi

    # Warn if many pods
    if [[ $total -gt 20 ]]; then
        warn "Streaming from ${BOLD}$total${NC} pods — this may be resource-intensive"
    fi

    info "Streaming from ${BOLD}$total${NC} pod(s) across ${BOLD}${#DEPLOYMENTS[@]}${NC} deployment(s)"
}

# ── Color assignment ──────────────────────────────────────────────────────────
declare -A DEPLOY_COLOR=()

assign_colors() {
    local i=0
    for dep in "${DEPLOYMENTS[@]}"; do
        if [[ "$NO_COLOR" == true ]]; then
            DEPLOY_COLOR["$dep"]=""
        else
            DEPLOY_COLOR["$dep"]="${PALETTE[$((i % ${#PALETTE[@]}))]}"
        fi
        ((i++))
    done
}

# ── Streaming ─────────────────────────────────────────────────────────────────
PIDS=()

stream_logs() {
    local container_flag=()
    [[ -n "$CONTAINER" ]] && container_flag=(-c "$CONTAINER")

    local tail_flag=()
    if [[ "$TAIL_LINES" -eq 0 ]]; then
        tail_flag=(--tail=0)
    else
        tail_flag=(--tail="$TAIL_LINES")
    fi

    # SED determined at script start (gsed on macOS, sed on Linux)

    local reset=""
    [[ "$NO_COLOR" == false ]] && reset="$NC"

    for i in "${!ALL_PODS[@]}"; do
        local pod="${ALL_PODS[$i]}"
        local dep="${POD_DEPLOY[$i]}"
        local color="${DEPLOY_COLOR[$dep]}"
        local prefix
        if [[ "$NO_COLOR" == true ]]; then
            prefix="[${dep}/${pod}] "
        else
            prefix="${color}[${dep}/${pod}]${reset} "
        fi

        # Stream: prefix both stdout and stderr from kubectl logs
        (
            kctl logs "$pod" "${container_flag[@]}" "${tail_flag[@]}" -f 2>&1 \
                | $SED -u "s|^|$prefix|"
        ) &
        PIDS+=($!)
    done
}

cleanup() {
    # Prevent re-entry from EXIT after INT/TERM
    trap '' INT TERM EXIT
    echo "" >&2
    info "Stopping all log streams..."
    for pid in "${PIDS[@]}"; do
        kill "$pid" 2>/dev/null || true
    done
    sleep 0.2
    for pid in "${PIDS[@]}"; do
        kill -9 "$pid" 2>/dev/null || true
    done
    wait 2>/dev/null || true
    info "Done."
    exit 0
}

# ── Main ──────────────────────────────────────────────────────────────────────
main() {
    parse_args "$@"
    select_context
    select_namespace
    select_deployments
    discover_pods
    assign_colors

    echo "" >&2
    info "Streaming logs (Ctrl+C to stop)..."
    if [[ "$LIVE" == true ]]; then
        info "Mode: ${BOLD}live${NC} (new logs only)"
    else
        info "Mode: tail last ${BOLD}$TAIL_LINES${NC} lines + follow"
    fi
    echo "" >&2

    trap cleanup INT TERM EXIT

    stream_logs
    wait "${PIDS[@]}" 2>/dev/null || true
}

main "$@"

# shellcheck shell=bash
# Resolve the active keyboard layout under Wayland.
#
# Wayland has no compositor-agnostic API to query the active layout from a
# non-focused client (the xkb group is delivered over wl_keyboard only to the
# focused surface). So we dispatch per compositor via its own IPC. The resolvers
# differ only in how they extract the raw layout description; the shared
# _xkb_normalize_layout maps that description to a short uppercase code.
#
# Each private resolver echoes the code and returns 0 on success, or returns
# non-zero when its compositor is not the one currently running. The dispatcher
# tries them in order and prints the first hit. Non-matching resolvers
# short-circuit before spawning any process, so this stays fast.

_xkb_runtime_dir() {
    echo "${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
}

# Map an xkb layout description (as reported by every compositor below, e.g.
# "English (US)" / "Ukrainian") to a short uppercase code. Returns non-zero for
# an empty/null value so callers can fall through to the next resolver.
_xkb_normalize_layout() {
    case "$1" in
        "English (US)" | English*) echo " US" ;;
        Ukrainian*) echo " UA" ;;
        "" | null) return 1 ;;
        *) echo "$1" | cut -c1-2 | tr '[:lower:]' '[:upper:]' ;;
    esac
}

# --- sway (and i3-compatible IPC) ---
_xkb_layout_sway() {
    local sock="$SWAYSOCK"
    if [ -z "$sock" ]; then
        for s in "$(_xkb_runtime_dir)"/sway-ipc.*.sock; do
            [ -S "$s" ] && sock="$s"
        done
    fi
    { [ -n "$sock" ] && command -v swaymsg &>/dev/null; } || return 1

    # sway exposes the active layout description via xkb_active_layout_name.
    local name
    name=$(SWAYSOCK="$sock" swaymsg -t get_inputs 2>/dev/null |
    jq -r 'first(.[] | select(.type=="keyboard") | .xkb_active_layout_name) // empty')
    _xkb_normalize_layout "$name"
}

# --- Hyprland ---
_xkb_layout_hyprland() {
    local sig="$HYPRLAND_INSTANCE_SIGNATURE"
    if [ -z "$sig" ]; then
        for d in "$(_xkb_runtime_dir)"/hypr/*/; do
            [ -d "$d" ] && sig="$(basename "$d")"
        done
    fi
    { [ -n "$sig" ] && command -v hyprctl &>/dev/null; } || return 1

    # Hyprland exposes the active layout description via active_keymap.
    local name
    name=$(HYPRLAND_INSTANCE_SIGNATURE="$sig" hyprctl devices -j 2>/dev/null |
    jq -r '.keyboards[] | .active_keymap' | tail -n1)
    _xkb_normalize_layout "$name"
}

# --- dwl (and any compositor whose xkb patch writes the shared file) ---
_xkb_layout_dwl() {
    [ -r /tmp/dwl-keymap ] || return 1
    # dwl's xkb patch writes the active layout description to this file.
    local name
    name=$(cat /tmp/dwl-keymap)
    _xkb_normalize_layout "$name"
}

# Dispatcher: prints the normalized code, or returns non-zero if unknown.
xkb_wayland_layout() {
    _xkb_layout_sway || _xkb_layout_hyprland || _xkb_layout_dwl
}

# Only auto-run when executed directly, not when sourced.
if ! (return 0 2>/dev/null); then
    xkb_wayland_layout
fi
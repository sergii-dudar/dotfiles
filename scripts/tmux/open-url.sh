#!/usr/bin/env bash
# Open the URL under the mouse pointer: Control + left click opens links inside tmux the same way the terminals do
# outside it (alacritty hint, wezterm OpenLinkAtMouseCursor, ghostty cmd/ctrl+click).
#
#   open-url.sh at <pane_id> <x> <y> [hyperlink]   tmux mouse binding: open the URL under cell (x, y) of the pane,
#                                                  exit 1 when there is none (the binding then passes the click on)
#   open-url.sh open <url>                          open one URL with the platform opener (alacritty hint command)
#   open-url.sh find <x> [hyperlink]                print the URL covering column x of the line read from stdin
#
# tmux side (.tmux.conf):
#   bind -n C-MouseDown1Pane if-shell "! ~/dotfiles/scripts/tmux/open-url.sh at '#{pane_id}' '#{mouse_x}' '#{mouse_y}' #{q:mouse_hyperlink}" { select-pane -t = ; send-keys -M }
#
# An OSC 8 hyperlink under the pointer (#{mouse_hyperlink}) wins. Otherwise the clicked screen row is scanned with
# alacritty's URL regex and post-processing (cut at an unbalanced ) or ], drop trailing .,:;?!([' delimiters).
# Only that row is looked at: a URL wrapped onto the next row is opened up to the end of the clicked row.

set -u

# tmux jobs and alacritty's hint command start with the GUI session's PATH, which on macOS omits Homebrew -- so tmux
# and python3 would not be found. Cheap to add, no-op if absent.
for d in /opt/homebrew/bin /usr/local/bin; do
    [ -d "$d" ] && case ":$PATH:" in *":$d:"*) ;; *) PATH="$d:$PATH" ;; esac
done
export PATH

# macOS has no `python`, only `python3`; Arch has both.
PY="$(command -v python3 || command -v python || true)"

die() {
    printf '%s\n' "$*" >&2
    exit 1
}

# Fully detached, so the tmux job (and alacritty) does not wait for the browser.
open_url() {
    local opener
    if [ "$(uname -s)" = Darwin ]; then opener=open; else opener=xdg-open; fi
    command -v "$opener" >/dev/null 2>&1 || die "open-url.sh: $opener not found on PATH"
    nohup "$opener" "$1" >/dev/null 2>&1 &
}

# Prints the URL covering screen column $2 (0-based cell) of line $1; exit 1 when none. Wide characters (CJK, emoji)
# take two cells and combining marks none, so cells are mapped to string indexes first.
url_at() {
    "$PY" - "$1" "$2" <<'PYEOF'
import re
import sys
import unicodedata

line, col = sys.argv[1], int(sys.argv[2])

# alacritty's default hint regex (URL_REGEX in alacritty/src/config/ui_config.rs)
URL = re.compile(
    r"(ipfs:|ipns:|magnet:|mailto:|gemini://|gopher://|https://|http://|news:|file:|git://|ssh:|ftp://)"
    r"[^\x00-\x1f\x7f-\x9f<>\"\s{-}\\^⟨⟩`\\]+"
)


def cell_to_index(s):
    out = []
    for i, ch in enumerate(s):
        if unicodedata.combining(ch):
            continue
        out.extend([i] * (2 if unicodedata.east_asian_width(ch) in ("W", "F") else 1))
    return out


# alacritty's hint_post_processing: cut at the first unbalanced ) or ], then drop trailing delimiters
def post_process(url):
    depth = {"(": 0, "[": 0}
    for i, ch in enumerate(url):
        if ch in "([":
            depth[ch] += 1
        elif ch == ")":
            if depth["("] == 0:
                url = url[:i]
                break
            depth["("] -= 1
        elif ch == "]":
            if depth["["] == 0:
                url = url[:i]
                break
            depth["["] -= 1
    return url.rstrip(".,:;?!(['")


index_of = cell_to_index(line)
if col >= len(index_of):
    sys.exit(1)
idx = index_of[col]
for m in URL.finditer(line):
    url = post_process(m.group(0))
    if url and m.start() <= idx < m.start() + len(url):
        print(url)
        sys.exit(0)
sys.exit(1)
PYEOF
}

[ -n "$PY" ] || die 'open-url.sh: no python3/python on PATH'

case "${1:-}" in
    at)
        pane="${2:-}" x="${3:-}" y="${4:-}" hyperlink="${5:-}"
        { [ -n "$pane" ] && [ -n "$x" ] && [ -n "$y" ]; } || die 'usage: open-url.sh at <pane_id> <x> <y> [hyperlink]'
        if [ -n "$hyperlink" ]; then
            open_url "$hyperlink"
            exit 0
        fi
        line="$(tmux capture-pane -p -t "$pane" -S "$y" -E "$y" 2>/dev/null)" || exit 1
        url="$(url_at "$line" "$x")" || exit 1
        open_url "$url"
        ;;
    open)
        [ -n "${2:-}" ] || die 'usage: open-url.sh open <url>'
        open_url "$2"
        ;;
    find)
        [ -n "${2:-}" ] || die 'usage: open-url.sh find <x> [hyperlink] < line'
        if [ -n "${3:-}" ]; then
            printf '%s\n' "$3"
            exit 0
        fi
        url_at "$(cat)" "$2"
        ;;
    *)
        die "open-url.sh: unknown mode '${1:-}' (at|open|find)"
        ;;
esac

picker_type="${1:-}"

declare -A WMS
WMS[0]="$HOME/dotfiles/bin/desc/hyprland.desktop.runner.sh"
WMS[1]="$HOME/dotfiles/bin/desc/awesome.desktop.runner.sh"
WMS[2]="$HOME/dotfiles/bin/desc/xmonad.desktop.runner.sh"
WMS[3]="$HOME/dotfiles/bin/desc/bspwm.desktop.runner.sh"
WMS[4]="$HOME/dotfiles/bin/desc/dwm.desktop.runner.sh"
WMS[5]="$HOME/dotfiles/bin/desc/qtile.desktop.runner.sh"
WMS[6]="$HOME/dotfiles/bin/desc/sway.desktop.runner.sh"
# WMS[]="$HOME/dotfiles/bin/desc/i3.desktop.runner.sh"
TOTAL_WMS=7

function pick_by_month_number() {
    WS_MONTH_INDEX_TO_PICK=$(("$1" % "$TOTAL_WMS"))
    # bash "${WMS[$WS_MONTH_INDEX_TO_PICK]}"
    "${WMS[$WS_MONTH_INDEX_TO_PICK]}"
}

function pick_by_month() {
    year=$(date -u +%Y)
    month=$(date -u +%-m)
    MONTH_NUMBEER=$(( ("$year" - 2020) * 12 + "$month" ))
    pick_by_month_number "$MONTH_NUMBEER"
}

function pick_by_random() {
    random_number=$(( (RANDOM % 129) + 1 ))  # Generates a number from 1 to 129
    pick_by_month_number $random_number
}

case "$picker_type" in
    "month")
        pick_by_month
        ;;
    "random")
        pick_by_random
        ;;
    *)
        pick_by_random
        ;;
esac
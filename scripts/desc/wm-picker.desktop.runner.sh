picker_type="${1:-}"

declare -A WMS
WMS[0]="$HOME/dotfiles/scripts/desc/hyprland.desktop.runner.sh"
WMS[1]="$HOME/dotfiles/scripts/desc/awesome.desktop.runner.sh"
WMS[2]="$HOME/dotfiles/scripts/desc/xmonad.desktop.runner.sh"
WMS[3]="$HOME/dotfiles/scripts/desc/bspwm.desktop.runner.sh"
WMS[4]="$HOME/dotfiles/scripts/desc/dwm.desktop.runner.sh"
WMS[5]="$HOME/dotfiles/scripts/desc/qtile.desktop.runner.sh"
WMS[6]="$HOME/dotfiles/scripts/desc/sway.desktop.runner.sh"
# WMS[]="$HOME/dotfiles/scripts/desc/i3.desktop.runner.sh"
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

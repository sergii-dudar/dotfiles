picker_type="${1:-}"

function pick_by_month_number() {
    case "$1" in
        01)
            "$HOME/dotfiles/bin/desc/hyprland.desktop.runner.sh"
            ;;
        02)
            "$HOME/dotfiles/bin/desc/awesome.desktop.runner.sh"
            ;;
        03)
            "$HOME/dotfiles/bin/desc/xmonad.desktop.runner.sh"
            ;;
        04)
            "$HOME/dotfiles/bin/desc/bspwm.desktop.runner.sh"
            ;;
        05)
            "$HOME/dotfiles/bin/desc/hyprland.desktop.runner.sh"
            ;;
        06)
            "$HOME/dotfiles/bin/desc/dwm.desktop.runner.sh"
            ;;
        07)
            "$HOME/dotfiles/bin/desc/xmonad.desktop.runner.sh"
            ;;
        08)
            "$HOME/dotfiles/bin/desc/awesome.desktop.runner.sh"
            ;;
        09)
            "$HOME/dotfiles/bin/desc/qtile.desktop.runner.sh"
            ;;
        10)
            "$HOME/dotfiles/bin/desc/hyprland.desktop.runner.sh"
            ;;
        11)
            #"$HOME/dotfiles/bin/desc/i3.desktop.runner.sh"
            "$HOME/dotfiles/bin/desc/sway.desktop.runner.sh"
            ;;
        12)
            "$HOME/dotfiles/bin/desc/dwm.desktop.runner.sh"
            ;;
        *)
            # in any unknown situation, use xmonad ;)
            "$HOME/dotfiles/bin/desc/xmonad.desktop.runner.sh"
            ;;
    esac
}

function pick_by_month() {
    month=$(date +%m) # get current month number
    pick_by_month_number "$month"
}

function pick_by_random() {
    random_number=$(( (RANDOM % 129) + 1 ))  # Generates a number from 1 to 129
    whole_part=$(( random_number / 10 ))     # Integer division (floor division)

    if [[ $random_number -ge 100 ]]; then
        mnumber="$whole_part"
    elif [[ $random_number -lt 10 ]]; then
        mnumber="01"
    else
        mnumber="0${whole_part}"
    fi

    # echo "Whole part: $whole_part"
    # echo "Random Number: $random_number"
    # echo "Month: $mnumber"
    pick_by_month_number $mnumber
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
ram=(
    "${right_items_common[@]}"
    icon="$SYSTEM_RAM"
    icon.color="$RAM_ICON_COLOR"
    update_freq=10
    script="$PLUGIN_DIR/right/ram.sh"
    click_script="$SCRIPTS_DIR/run_external_bash.sh '/opt/homebrew/bin/btop && exit'"
    display=1
)

sketchybar --add item ram right \
    --set ram "${ram[@]}"
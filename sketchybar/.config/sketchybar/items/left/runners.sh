shared_run_settings_conf=(
    "${left_items_common[@]}"
    update_freq=0
    label.drawing=off
    background.drawing=off
    icon.padding_left=1.5
    icon.padding_right=1.5
    icon.y_offset=2
    display=1
)

run_settings_conf=(
    "${shared_run_settings_conf[@]}"
    icon.padding_left=3
    icon="$RUNNER_SETTINGS"
    icon.color="$RUNNER_SETTINGS_ICON_COLOR"
    icon.font.size=19
    icon.y_offset=2
    click_script="open -a System\ Settings"
)
# group.run.settings_updates_conf=(
"${shared_run_settings_conf[@]}"
#     icon="$RUNNER_UPDATES"
#     click_script="open \"x-apple.systempreferences:com.apple.Software-Update-Settings.extension\""
#     icon.color="0xff"
# )
run_kitty_conf=(
    "${shared_run_settings_conf[@]}"
    icon.font="sketchybar-app-font:Regular:19.0"
    icon=":kitty:"
    icon.color="$RUNNER_KITTY_ICON_COLOR"
    click_script="open -na kitty"
)
run_ghostty_conf=(
    "${shared_run_settings_conf[@]}"
    icon.font="sketchybar-app-font:Regular:20.0"
    icon=":ghostty:"
    icon.color="$RUNNER_GHOSTTY_ICON_COLOR"
    click_script="open -na Ghostty"
    icon.y_offset=2
)
run_wezterm_conf=(
    "${shared_run_settings_conf[@]}"
    icon.font="sketchybar-app-font:Regular:17.0"
    icon=":wezterm:"
    icon.color="$RUNNER_WEZTERM_ICON_COLOR"
    click_script="open -na WezTerm"
)
run_brave_browser_conf=(
    "${shared_run_settings_conf[@]}"
    icon.font="sketchybar-app-font:Regular:18.0"
    icon=":brave_browser:"
    icon.color="$RUNNER_BRAVE_BROWSER_ICON_COLOR"
    click_script="open -na Brave\ Browser"
)
run_idea_conf=(
    "${shared_run_settings_conf[@]}"
    icon.padding_right=6
    icon.font="sketchybar-app-font:Regular:17.0"
    icon=":idea:"
    icon.color="$RUNNER_IDEA_ICON_COLOR"
    click_script="open -a IntelliJ\ IDEA"
)

sketchybar \
    --add item group_run_settings left --set group_run_settings "${run_settings_conf[@]}" \
    --add item group_run_kitty left --set group_run_kitty "${run_kitty_conf[@]}" \
    --add item group_run_ghostty left --set group_run_ghostty "${run_ghostty_conf[@]}" \
    --add item group_run_wezterm left --set group_run_wezterm "${run_wezterm_conf[@]}" \
    --add item group_run_brave_browser left --set group_run_brave_browser "${run_brave_browser_conf[@]}" \
    --add item group_run_idea left --set group_run_idea "${run_idea_conf[@]}" \
    --add bracket runners_group '/group_run_.*/' \
    --set runners_group display=1
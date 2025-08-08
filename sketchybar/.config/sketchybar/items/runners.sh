#!/usr/bin/env bash

run_settings_conf=(
    update_freq=0
    label.drawing=off
    icon="􀣋 "
    click_script="open -a System\ Settings"
)
# run_settings_updates_conf=(
#     update_freq=0
#     label.drawing=off
#     icon=" "
#     click_script="open \"x-apple.systempreferences:com.apple.Software-Update-Settings.extension\""
# )
run_kitty_conf=(
    update_freq=0
    label.drawing=off
    icon.font="sketchybar-app-font:Regular:19.0"
    icon=":kitty:"
    click_script="open -na kitty"
)
run_ghostty_conf=(
    update_freq=0
    label.drawing=off
    icon.font="sketchybar-app-font:Regular:20.0"
    icon=":ghostty:"
    click_script="open -na Ghostty"
)
run_wezterm_conf=(
    update_freq=0
    label.drawing=off
    icon.font="sketchybar-app-font:Regular:17.0"
    icon=":wezterm:"
    click_script="open -na WezTerm"
)
run_brave_browser_conf=(
    update_freq=0
    label.drawing=off
    icon.font="sketchybar-app-font:Regular:18.0"
    icon=":brave_browser:"
    click_script="open -na Brave\ Browser"
)
run_idea_conf=(
    update_freq=0
    label.drawing=off
    icon.font="sketchybar-app-font:Regular:17.0"
    icon=":idea:"
    click_script="open -a IntelliJ\ IDEA"
)

sketchybar --add item run_settings left --set run_settings "${run_settings_conf[@]}" \
    --add item run_kitty left --set run_kitty "${run_kitty_conf[@]}" \
    --add item run_ghostty left --set run_ghostty "${run_ghostty_conf[@]}" \
    --add item run_wezterm left --set run_wezterm "${run_wezterm_conf[@]}" \
    --add item run_brave_browser left --set run_brave_browser "${run_brave_browser_conf[@]}" \
    --add item run_idea left --set run_idea "${run_idea_conf[@]}"

# --add item run_settings_updates left --set run_settings_updates "${run_settings_updates_conf[@]}" \

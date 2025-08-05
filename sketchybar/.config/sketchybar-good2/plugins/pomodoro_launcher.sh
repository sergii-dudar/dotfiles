#!/bin/sh

pomo_on() {
  sketchybar --set work_button drawing=on \
             --set break_button drawing=on \
             --set pomo_history drawing=on
}

pomo_off() {
  sketchybar --set work_button drawing=off \
             --set break_button drawing=off \
             --set pomo_history drawing=off
}

if [ "$1" = "on" ]; then
  pomo_on
elif [ "$1" = "off" ]; then
  pomo_off
else
  # 正しい drawing 状態を見て切り替え（work_buttonをチェック）
  if [ "$(sketchybar --query work_button | jq -r '.geometry.drawing')" = "on" ]; then
    pomo_off
  else
    pomo_on
  fi
fi

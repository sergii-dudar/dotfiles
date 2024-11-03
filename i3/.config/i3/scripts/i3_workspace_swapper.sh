#!/bin/sh
# put the name of the curently focused workspace in $focused variable
focused=$(i3-msg -t get_workspaces | jq -r '.. | select(.focused? and .focused == true) | .name')
# put the name of the curently focused display in $focused_display variable
focused_display=$(i3-msg -t get_workspaces | jq -r '.. | select(.focused? and .focused == true) | .output')
# put the argument given to the script in the $swap variable. this is the workspace we want to swap to. this is given to us by the keybinding for that workspace in the i3 config.
swap=$@
# put the name of the curently focused display in $focused_display variable
swap_display=$(i3-msg -t get_workspaces | jq -r ".[] | select(.name == \"$swap\") | .output")


if [ "$swap_display" = "$focused_display" ]
then
   i3-msg -- workspace number "$swap"
   focused_display=$(i3-msg -t get_workspaces | jq -r '.. | select(.focused? and .focused == true) | .output')
   new_focused=$(i3-msg -t get_workspaces | jq -r '.. | select(.focused? and .focused == true) | .name')
   if [ "$swap_display" != "$focused_display" ];then
      i3-msg -- move workspace to output "$swap_display"
      i3-msg -- workspace --no-auto-back-and-forth "$focused"
      i3-msg -- move workspace to output "$focused_display"
      sleep 0.1 ; i3-msg -- workspace --no-auto-back-and-forth "$new_focused"
   fi
else
   if [ -n "$swap_display" ];then
      # if the workspace exist on another monitor (if the $swap_display variable exist), then move the currently focused workspace to the monitor of the workspace we want to swap with
      i3-msg -- move workspace to output "$swap_display"
      # create or focus the workspace we want to swap with and then move that workspace to the monitor that was focused at the start when the keybinding was pressed
      i3-msg -- "workspace --no-auto-back-and-forth "$swap"; move workspace to output "$focused_display""
      sleep 0.1 ; i3-msg -- workspace --no-auto-back-and-forth "$swap"
   else
      i3-msg -- workspace number "$swap"
   fi
fi

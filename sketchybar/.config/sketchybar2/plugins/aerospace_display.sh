#!/usr/bin/env bash

# Source colors for consistent theming
source "$CONFIG_DIR/colors.sh"

# Get all available monitors
AVAILABLE_MONITORS=$(aerospace list-monitors)
FOCUSED_WORKSPACE=$(aerospace list-workspaces --focused)

# Create a cache file to store last known state
CACHE_FILE="/tmp/aerospace_display_state"

# Map aerospace monitor IDs to our logical naming
# M1 = Primary (Built-in Retina Display) = aerospace monitor 2
# M2 = External (MAG 274UPF E2) = aerospace monitor 1
MONITOR_M1_ID="2"  # Built-in Retina Display
MONITOR_M2_ID="1"  # External Monitor

# Check which monitors are currently connected
MONITOR_M1_CONNECTED=$(echo "$AVAILABLE_MONITORS" | grep "^$MONITOR_M1_ID " | wc -l)
MONITOR_M2_CONNECTED=$(echo "$AVAILABLE_MONITORS" | grep "^$MONITOR_M2_ID " | wc -l)

# Function to get the current workspace for a monitor
get_current_workspace_for_monitor() {
    local monitor_id="$1"
    local workspaces=$(aerospace list-workspaces --monitor "$monitor_id" 2>/dev/null)
    
    # If monitor doesn't exist, return empty
    if [ -z "$workspaces" ]; then
        echo ""
        return
    fi
    
    # Check if the focused workspace is on this monitor
    for ws in $workspaces; do
        if [ "$ws" = "$FOCUSED_WORKSPACE" ]; then
            echo "$ws"
            return
        fi
    done
    
    # If focused workspace is not on this monitor, return cached value or first workspace
    if [ -f "$CACHE_FILE" ]; then
        local cached_ws=$(grep "^$monitor_id:" "$CACHE_FILE" | cut -d: -f2)
        if [ -n "$cached_ws" ]; then
            # Verify the cached workspace still exists on this monitor
            for ws in $workspaces; do
                if [ "$ws" = "$cached_ws" ]; then
                    echo "$cached_ws"
                    return
                fi
            done
        fi
    fi
    
    # Fallback to first workspace on this monitor
    echo "$workspaces" | head -n1
}

# Update cache with current state
update_cache() {
    local monitor_id="$1"
    local workspace="$2"
    
    # Create or update cache file
    if [ -f "$CACHE_FILE" ]; then
        # Remove old entry for this monitor
        grep -v "^$monitor_id:" "$CACHE_FILE" > "$CACHE_FILE.tmp"
        mv "$CACHE_FILE.tmp" "$CACHE_FILE"
    fi
    
    # Add current entry
    echo "$monitor_id:$workspace" >> "$CACHE_FILE"
}

# Get current workspace for each monitor (if connected)
MONITOR_M1_CURRENT=""
MONITOR_M2_CURRENT=""

if [ "$MONITOR_M1_CONNECTED" -gt 0 ]; then
    MONITOR_M1_CURRENT=$(get_current_workspace_for_monitor "$MONITOR_M1_ID")
    if [ -n "$MONITOR_M1_CURRENT" ]; then
        update_cache "$MONITOR_M1_ID" "$MONITOR_M1_CURRENT"
    fi
fi

if [ "$MONITOR_M2_CONNECTED" -gt 0 ]; then
    MONITOR_M2_CURRENT=$(get_current_workspace_for_monitor "$MONITOR_M2_ID")
    if [ -n "$MONITOR_M2_CURRENT" ]; then
        update_cache "$MONITOR_M2_ID" "$MONITOR_M2_CURRENT"
    fi
fi

# Handle display logic based on what's connected
if [ "$MONITOR_M1_CONNECTED" -gt 0 ] && [ "$MONITOR_M2_CONNECTED" -gt 0 ]; then
    # Both monitors connected - show both
    sketchybar --set monitor_1_workspace \
               icon="M1:$MONITOR_M1_CURRENT" \
               drawing=on \
               icon.width=52 \
               icon.align=center \
               icon.padding_left=4 \
               icon.padding_right=4 \
               click_script="aerospace workspace $MONITOR_M1_CURRENT"
    
    sketchybar --set monitor_2_workspace \
               icon="M2:$MONITOR_M2_CURRENT" \
               drawing=on \
               icon.width=52 \
               icon.align=center \
               icon.padding_left=4 \
               icon.padding_right=4 \
               click_script="aerospace workspace $MONITOR_M2_CURRENT"
    
    # Highlight the currently focused workspace
    if [ "$MONITOR_M1_CURRENT" = "$FOCUSED_WORKSPACE" ]; then
        sketchybar --set monitor_1_workspace \
                   icon.color=$WHITE \
                   icon.font="SF Pro:Semibold:12.0" \
                   background.color=$BACKGROUND_2
        
        sketchybar --set monitor_2_workspace \
                   icon.color=$GREY \
                   icon.font="SF Pro:Medium:12.0" \
                   background.color=$BACKGROUND_1
    elif [ "$MONITOR_M2_CURRENT" = "$FOCUSED_WORKSPACE" ]; then
        sketchybar --set monitor_2_workspace \
                   icon.color=$WHITE \
                   icon.font="SF Pro:Semibold:12.0" \
                   background.color=$BACKGROUND_2
        
        sketchybar --set monitor_1_workspace \
                   icon.color=$GREY \
                   icon.font="SF Pro:Medium:12.0" \
                   background.color=$BACKGROUND_1
    else
        # Neither monitor has focus, show both as inactive
        sketchybar --set monitor_1_workspace \
                   icon.color=$GREY \
                   icon.font="SF Pro:Medium:12.0" \
                   background.color=$BACKGROUND_1
        
        sketchybar --set monitor_2_workspace \
                   icon.color=$GREY \
                   icon.font="SF Pro:Medium:12.0" \
                   background.color=$BACKGROUND_1
    fi
    
elif [ "$MONITOR_M1_CONNECTED" -gt 0 ]; then
    # Only M1 (primary) connected - show just the workspace number
    sketchybar --set monitor_1_workspace \
               icon="$MONITOR_M1_CURRENT" \
               drawing=on \
               icon.color=$WHITE \
               icon.font="SF Pro:Semibold:14.0" \
               icon.width=32 \
               icon.align=center \
               icon.padding_left=4 \
               icon.padding_right=4 \
               background.color=$BACKGROUND_2 \
               click_script="aerospace workspace $MONITOR_M1_CURRENT"
    
    sketchybar --set monitor_2_workspace drawing=off
    
elif [ "$MONITOR_M2_CONNECTED" -gt 0 ]; then
    # Only M2 (external) connected - show just the workspace number
    sketchybar --set monitor_2_workspace \
               icon="$MONITOR_M2_CURRENT" \
               drawing=on \
               icon.color=$WHITE \
               icon.font="SF Pro:Semibold:14.0" \
               icon.width=32 \
               icon.align=center \
               icon.padding_left=4 \
               icon.padding_right=4 \
               background.color=$BACKGROUND_2 \
               click_script="aerospace workspace $MONITOR_M2_CURRENT"
    
    sketchybar --set monitor_1_workspace drawing=off
    
else
    # No monitors detected - hide both (shouldn't happen)
    sketchybar --set monitor_1_workspace drawing=off
    sketchybar --set monitor_2_workspace drawing=off
fi 
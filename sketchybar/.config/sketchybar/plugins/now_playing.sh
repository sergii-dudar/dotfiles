#!/usr/bin/env bash

# Source colors for consistent theming
#source "$CONFIG_DIR/colors.sh"
LAT_PLAYING_LABEL_FILE="$HOME/dotfiles/temp/now_playing_last.label"

# Function to get Spotify info
# function get_spotify_info() {
#     local state=$(osascript -e 'tell application "Spotify" to player state as string' 2>/dev/null)
#     PLAYER="Spotify"
#     if [[ "$state" == "playing" ]]; then
#         STATUS="playing"
#         ARTIST=$(osascript -e 'tell application "Spotify" to artist of current track as string' 2>/dev/null)
#         TRACK=$(osascript -e 'tell application "Spotify" to name of current track as string' 2>/dev/null)
#         ICON=" "
#         COLOR=$ACCENT_QUATERNARY
#     elif [[ "$state" == "paused" ]]; then
#         ICON=" "
#         STATUS="paused"
#     else
#         ICON=" "
#         STATUS="stopped"
#     fi
# }

# Function to get Apple Music info
function get_apple_music_info() {
    local state=$(osascript -e 'tell application "Music" to player state as string' 2>/dev/null)
    PLAYER="iTunes"
    if [[ "$state" == "playing" ]]; then
        STATUS="playing"
        ARTIST=$(osascript -e 'tell application "Music" to artist of current track as string' 2>/dev/null)
        TRACK=$(osascript -e 'tell application "Music" to name of current track as string' 2>/dev/null)
        ICON=" "
        COLOR=$ACCENT_QUATERNARY
    elif [[ "$state" == "paused" ]]; then
        ICON=" "
        STATUS="paused"
    else
        ICON=" "
        STATUS="stopped"
    fi
}

# Function to get MPD info
function get_mpd_info() {
    local state=$(~/.cargo/bin/rmpc status | jq -r '.state')
    PLAYER="MPD"
    if [[ "$state" == "Play" ]]; then
        STATUS="playing"
        local song_json=$(~/.cargo/bin/rmpc song)
        ARTIST=$(echo $song_json | jq -r '.metadata.artist')
        TRACK=$(echo $song_json | jq -r '.metadata.title')
        ICON=" "
        COLOR=$ACCENT_SECONDARY
    elif [[ "$state" == "Pause" ]]; then
        ICON=" "
        STATUS="paused"
    else
        ICON=" "
        STATUS="stopped"
    fi
}

function update_music_item() {
    # Update the Now Playing item (only if we're showing it)
    sketchybar --set "$NAME" icon="$ICON" \
        label="$LABEL"

    # icon.color="$COLOR" \
        # label.color=$WHITE \
        # label.font="SF Pro:Medium:12.0"
}

function process_player_info() {
    if [[ "$STATUS" == "playing" ]]; then
        sketchybar --set "$NAME" drawing=on
        full_lable="$PLAYER: ${TRACK} • ${ARTIST}"
        if [[ ${#full_lable} -gt 60 ]]; then
            LABEL="${full_lable:0:60}..."
        else
            LABEL="$full_lable"
        fi

        echo -e "$full_lable" > "${LAT_PLAYING_LABEL_FILE}"
        update_music_item

        exit 0
    fi
}

get_mpd_info
process_player_info

get_apple_music_info
process_player_info

# get_spotify_info
# process_player_info

# in case no any player in status - playing
if [[ "$STATUS" == "paused" ]]; then
    sketchybar --set "$NAME" drawing=on
    ICON=" "
    COLOR=$ACCENT_TERTIARY
    if [ -f "$LAT_PLAYING_LABEL_FILE" ]; then
        LABEL=$(bat -pp "$LAT_PLAYING_LABEL_FILE")
    else
        LABEL="Paused"
    fi
    update_music_item
else
    # No music playing - hide the item completely
    sketchybar --set "$NAME" drawing=off
fi

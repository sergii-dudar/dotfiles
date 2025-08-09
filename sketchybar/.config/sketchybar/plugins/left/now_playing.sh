#!/usr/bin/env bash

source "$CONFIG_DIR/icons.sh"
source "$CONFIG_DIR/colors.sh"

LAST_PLAYING_LABEL_FILE="$HOME/dotfiles/temp/now_playing_last.label"
if [ -f "$LAST_PLAYING_LABEL_FILE" ]; then
    LAST_PLAYING_LABEL=$(bat -pp "$LAST_PLAYING_LABEL_FILE")
else
    LAST_PLAYING_LABEL="Paused"
fi

# Mouse control: supported only to MPD
if [[ "$SENDER" == "mouse."* ]] && [[ "$LAST_PLAYING_LABEL" == "MPD "* ]]; then
    # echo "sender: $SENDER, button: $BUTTON, modifier: $MODIFIER, scroll_delta: $SCROLL_DELTA" > /tmp/logs.txt
    case "$SENDER" in
        "mouse.clicked")
            case "$BUTTON" in
                "left")
                    if [ "$MODIFIER" = "ctrl" ]; then
                        ~/.cargo/bin/rmpc prev
                    else
                        ~/.cargo/bin/rmpc togglepause
                    fi
                    ;;
                "right")
                    if [ "$MODIFIER" = "ctrl" ]; then
                        ~/.cargo/bin/rmpc next
                    fi
                    ;;
            esac
            ;;
        "mouse.scrolled")
            if [ "$SCROLL_DELTA" -gt 0 ]; then
                ~/.cargo/bin/rmpc volume +5
            else
                ~/.cargo/bin/rmpc volume -5
            fi
            ;;
    esac
fi

# Function to get Spotify info
# function get_spotify_info() {
#     local state=$(osascript -e 'tell application "Spotify" to player state as string' 2>/dev/null)
#     PLAYER="Spotify"
#     if [[ "$state" == "playing" ]]; then
#         STATUS="playing"
#         ARTIST=$(osascript -e 'tell application "Spotify" to artist of current track as string' 2>/dev/null)
#         TRACK=$(osascript -e 'tell application "Spotify" to name of current track as string' 2>/dev/null)
#     elif [[ "$state" == "paused" ]]; then
#         STATUS="paused"
#     else
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
    elif [[ "$state" == "paused" ]]; then
        STATUS="paused"
    else
        STATUS="stopped"
    fi
}

# Function to get MPD info
function get_mpd_info() {
    local state=$(~/.cargo/bin/rmpc status | jq -r '.state')
    local vol=$(~/.cargo/bin/rmpc volume)
    PLAYER="MPD [ ${vol}%]"
    if [[ "$state" == "Play" ]]; then
        STATUS="playing"
        local song_json=$(~/.cargo/bin/rmpc song)
        ARTIST=$(echo $song_json | jq -r '.metadata.artist')
        TRACK=$(echo $song_json | jq -r '.metadata.title')
    elif [[ "$state" == "Pause" ]]; then
        STATUS="paused"
    else
        STATUS="stopped"
    fi
}

function update_music_item() {
    # Update the Now Playing item (only if we're showing it)
    sketchybar --set "$NAME" \
        icon="$ICON" \
        icon.color="$ICON_COLOR" \
        label="$LABEL"
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
        ICON_COLOR="$PLAYER_PLAY_ICON_COLOR"
        ICON="$PLAYER_PLAY"
        echo -e "$full_lable" > "${LAST_PLAYING_LABEL_FILE}"
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
echo "status: $STATUS" > /tmp/logs.txt
if [[ "$STATUS" == "paused" ]]; then
    sketchybar --set "$NAME" drawing=on
    ICON="$PLAYER_PAUSE"
    ICON_COLOR="$PLAYER_PAUSE_ICON_COLOR"
    LABEL=$LAST_PLAYING_LABEL
    update_music_item
else
    # No music playing - hide the item completely
    ICON_COLOR="$PLAYER_STOP_ICON_COLOR"
    ICON="$PLAYER_STOP"
    sketchybar --set "$NAME" drawing=off
fi
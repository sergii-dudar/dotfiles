# dwlblocks-async Usage

## Overview

dwlblocks-async is an asynchronous status bar block generator for dwl. It outputs status text to stdout, which dwl reads via stdin pipe (`dwlblocks | dwl`).

## Configuration

Edit `config.h`:

```c
// Block definition: X(icon, command, interval, signal)
#define BLOCKS(X) \
    X("", "/path/to/script", 10, 1) \
    X("", "/path/to/other", 0,  2)
```

| Field | Description |
|-------|-------------|
| `icon` | Static prefix text (can be empty) |
| `cmd` | Script/command to execute |
| `interval` | Auto-refresh interval in seconds (0 = signal-only) |
| `signal` | Signal number for manual refresh (1-31) |

### Options

```c
#define CLICKABLE_BLOCKS 1    // Enable click support (requires dwl statuscmd)
#define DELIMITER ""          // Text between blocks
#define LEADING_DELIMITER 0   // Prepend delimiter before first block
#define TRAILING_DELIMITER 0  // Append delimiter after last block
#define MAX_BLOCK_OUTPUT_LENGTH 200  // Max chars per block
```

## Refreshing Blocks

Manually refresh a specific block from any script or terminal:

```sh
# Refresh block with signal N:
kill -$(( 34 + N )) $(pidof dwlblocks)

# Examples:
kill -35 $(pidof dwlblocks)   # signal 1 (e.g., date)
kill -37 $(pidof dwlblocks)   # signal 3 (e.g., volume)
kill -44 $(pidof dwlblocks)   # signal 10 (e.g., media)
```

`SIGRTMIN` = 34 on Linux, so signal N = `kill -(34 + N)`.

## Clickable Blocks

> **Requires patched dwl.** Vanilla dwl does not support per-block click detection.
> The `statuscmd` implementation in dwl parses signal bytes from the status text,
> tracks block pixel boundaries during rendering, and uses `sigqueue()` to send
> the button number back to dwlblocks. This is implemented in the custom dwl build
> at `/home/serhii/myforks/dwl` (not available as a standalone patch).

When `CLICKABLE_BLOCKS 1` is set, dwlblocks prepends a signal byte before each block's output. dwl uses this to identify which block was clicked and sends `sigqueue(SIGRTMIN+signal, button)` back to dwlblocks.

dwlblocks then re-executes the block script with `BLOCK_BUTTON` environment variable set.

### Button Values

| Action | `$BLOCK_BUTTON` |
|--------|----------------|
| Left click | 1 |
| Middle click | 2 |
| Right click | 3 |
| Scroll up | 4 |
| Scroll down | 5 |
| Shift + Left click | 6 |

### Example Script

```bash
#!/bin/bash

# Display volume
vol=$(pactl get-sink-volume @DEFAULT_SINK@ | grep -oP '\d+%' | head -1)
muted=$(pactl get-sink-mute @DEFAULT_SINK@ | grep -oP 'yes|no')

case "$BLOCK_BUTTON" in
    1) pactl set-sink-mute @DEFAULT_SINK@ toggle ;;
    2) setsid --fork foot -e pulsemixer ;;
    3) setsid --fork pavucontrol ;;
    4) pactl set-sink-volume @DEFAULT_SINK@ +5% ;;
    5) pactl set-sink-volume @DEFAULT_SINK@ -5% ;;
    6) setsid --fork foot -e nvim "$0" ;;
esac

if [ "$muted" = "yes" ]; then
    echo "^fg(ff6c6b) 婢 ^fg()MUTE"
else
    echo "^fg(98be65) ^fg()${vol}"
fi
```

## Markup Support

Block output supports dwl bar markup:

- `^fg(RRGGBB)` — set foreground color
- `^bg(RRGGBB)` — set background color
- `^fg()` / `^bg()` — reset to default
- `^^` — literal `^`

## Building

```sh
make clean && make
```

## Running

```sh
dwlblocks | dwl
```

Or in your session startup script. dwlblocks must be piped directly to dwl's stdin.

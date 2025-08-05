#!/bin/bash

INTERFACE="en0"

read initial_rx initial_tx < <(netstat -ibn | awk -v iface="$INTERFACE" '$1 == iface && $3 == "<Link#14>" {print $7, $10}')
sleep 1
read final_rx final_tx < <(netstat -ibn | awk -v iface="$INTERFACE" '$1 == iface && $3 == "<Link#14>" {print $7, $10}')

DOWN=$((final_rx - initial_rx)) # Bytes per second
UP=$((final_tx - initial_tx))   # Bytes per second

human_readable() {
    local bytes=$1
    case $bytes in
    # Greater than or equal to 1 GB
    [1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]*)
        printf "%.2f GB/s\n" "$(bc -l <<<"$bytes/1073741824")"
        ;;
    # Greater than or equal to 1 MB
    [1-9][0-9][0-9][0-9][0-9][0-9]*)
        printf "%.2f MB/s\n" "$(bc -l <<<"$bytes/1048576")"
        ;;
    # Greater than or equal to 1 KB
    [1-9][0-9][0-9][0-9]*)
        printf "%.2f KB/s\n" "$(bc -l <<<"$bytes/1024")"
        ;;
    # Default case (less than 1 KB)
    *)
        echo "$bytes B/s"
        ;;
    esac
}

sketchybar --set net.down label="$(human_readable $DOWN)" \
    --set net.up label="$(human_readable $UP)"

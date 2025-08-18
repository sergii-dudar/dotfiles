#!/usr/bin/env bash

APP_NAME=$1

#SCREEN_NUMBERS=$(system_profiler SPDisplaysDataType | rg "Resolution:" | wc -l | awk '{print $1}')
SCREEN_NUMBERS=$(system_profiler SPDisplaysDataType | rg --count "Resolution:")

if [ "$SCREEN_NUMBERS" = 1 ]; then
    # Single 4k monitor suport
    osascript -l JavaScript -e '
var frontAppName = "'"$APP_NAME"'";
var frontApp = Application(frontAppName);
var topWindow = frontApp.windows[0];

var windowBounds = topWindow.bounds();

var desktopBounds = Application("Finder").desktop.window().bounds();

windowBounds.width = desktopBounds.width * 0.7;
windowBounds.height = desktopBounds.height * 0.8;

windowBounds.x = (desktopBounds.width - windowBounds.width) / 2 + desktopBounds.x;
windowBounds.y = (desktopBounds.height - windowBounds.height) / 2 + desktopBounds.y;

topWindow.bounds = windowBounds;
    '
else
    osascript -l JavaScript -e '
var frontAppName = "'"$APP_NAME"'";
var frontApp = Application(frontAppName);
var topWindow = frontApp.windows[0];

var windowBounds = topWindow.bounds();

var desktopBounds = Application("Finder").desktop.window().bounds();

var desktopWidth = desktopBounds.width - 1440; // remove second vert monitor width to make correct position on first monitor.
var desktopHeight = desktopBounds.height;

windowBounds.width = desktopWidth * 0.8;
windowBounds.height = desktopHeight * 0.5;

windowBounds.x = (desktopHeight - windowBounds.width) / 2 + desktopBounds.x;
windowBounds.y = (desktopHeight - windowBounds.height) / 2 + desktopBounds.y;

topWindow.bounds = windowBounds;
    '

fi

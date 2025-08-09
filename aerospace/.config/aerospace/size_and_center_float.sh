#!/usr/bin/env bash

APP_NAME=$1

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

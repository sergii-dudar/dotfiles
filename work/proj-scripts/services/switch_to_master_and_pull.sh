#!/usr/bin/env bash

# Iterate over directories
for dir in */; do
    # Navigate into the project directory
    cd "$dir"

    # Check if the directory is a Git repository
    if [ -d ".git" ]; then
        echo "Switching to 'master' branch and pulling latest changes in '$dir'..."

        # Switch to the 'develop' branch
        # git checkout master
        git checkout main

        # Pull the latest changes from the 'develop' branch
        git pull #origin master

        # Print a newline for better output formatting
        echo ""
    else
        echo "'$dir' is not a Git repository. Skipping..."
    fi

    # Navigate back to the parent directory
    cd ..
done

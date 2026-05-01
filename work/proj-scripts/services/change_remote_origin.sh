#!/usr/bin/env bash

# Iterate over directories
for dir in */; do
    # Navigate into the project directory
    cd "$dir"

    # Check if the directory is a Git repository
    if [ -d ".git" ]; then
        current_remote=$(git remote get-url origin)
        new_remote=${current_remote/git@code.rbi.tech:raiffeisen/git@code.rbi.tech:RBUA}
        git remote set-url origin "$new_remote"
        echo "Changed origin from '$current_remote' to '$new_remote'"
    else
        echo "'$dir' is not a Git repository. Skipping..."
    fi

    # Navigate back to the parent directory
    cd ..
done

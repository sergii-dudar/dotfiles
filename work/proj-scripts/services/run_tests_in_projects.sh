#!/usr/bin/env bash

# Iterate over directories
for dir in */; do
    if [ "$dir" = 'ua-payments-payment-prevalidation/' ]; then
        FIXED_PROJ="FIXED"
    fi
    if [ ! -v FIXED_PROJ ]; then
        echo "skip as already processd"
        continue
    fi

    # Navigate into the project directory
    cd "$dir"
    echo "----------- inside $dir --  $PWD"

    # Check if the directory is a Git repository
    if [ -d ".git" ] && [ -f "pom.xml" ]; then

        echo "compiling $PWD"
        mvn clean verify -U || { echo "failed to verify $PWD" ; exit 1 ; }
        echo "$PWD successfully compiled"
    else
        echo "$PWD is not a Git repository. Skipping..."
    fi

    # Navigate back to the parent directory
    cd ..
    echo "----------- returned to parent $PWD"
done

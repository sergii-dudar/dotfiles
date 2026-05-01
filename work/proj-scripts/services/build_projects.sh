#!/usr/bin/env bash

# Iterate over directories
for dir in */; do
    # Navigate into the project directory
    cd "$dir"
    echo "----------- inside $PWD"

#    if [ "$dir" = 'ua-payments-payment-transmaster-connector/' ]; then
#      FIXED_PROJ="FIXED"
#    fi
#    if [ ! -v FIXED_PROJ ]; then
#      echo "skip as already processd"
#      continue
#    fi

    # Check if the directory is a Git repository
    if [ -d ".git" ] && [ -f "pom.xml" ]; then

        echo "compiling $PWD"
        mvn clean compile -U || { echo "failed to compile $PWD" ; exit 1 ; }
        echo "$PWD successfully compiled"
    else
        echo "$PWD is not a Git repository. Skipping..."
    fi

    # Navigate back to the parent directory
    cd ..
    echo "----------- returned to parent $PWD"
done

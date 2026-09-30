#!/usr/bin/env bash

# Switch every git repository in the current directory to the target branch
# and pull the latest changes. Repositories are processed in parallel.
#
# Usage:
#   ./switch_to_master_and_pull.sh
#   MAX_JOBS=16 ./switch_to_master_and_pull.sh
#   BRANCH=master ./switch_to_master_and_pull.sh

set -u

if ((BASH_VERSINFO[0] < 5 || (BASH_VERSINFO[0] == 5 && BASH_VERSINFO[1] < 1))); then
    echo "bash >= 5.1 is required for 'wait -n -p' (found $BASH_VERSION)" >&2
    exit 1
fi

branch="${BRANCH:-main}"
max_jobs="${MAX_JOBS:-8}"

# Never let a background git job block on a credential prompt or an editor.
export GIT_TERMINAL_PROMPT=0
export GIT_MERGE_AUTOEDIT=no

work_dir=$(mktemp -d) || exit 1
trap 'rm -rf "$work_dir"' EXIT

declare -A job_dir=()
failed=()

update_repo() {
    local dir="$1"

    cd "$dir" || exit 1
    git checkout "$branch" && git pull
}

print_result() {
    local dir="$1"
    local status="$2"

    echo "=== $dir"
    cat "$work_dir/${dir%/}.log"
    if ((status != 0)); then
        echo "!!! FAILED in '$dir' (exit code $status)"
        failed+=("$dir")
    fi
    echo ""
}

# Wait for one background job to finish and print its captured output.
# Only the parent process writes to the terminal, so blocks never interleave.
reap_one() {
    local pid=""
    local status=0

    wait -n -p pid || status=$?
    [[ -n "$pid" ]] || return 1

    print_result "${job_dir[$pid]}" "$status"
    unset "job_dir[$pid]"
}

for dir in */; do
    if [ ! -d "$dir/.git" ]; then
        echo "'$dir' is not a Git repository. Skipping..."
        continue
    fi

    if ((${#job_dir[@]} >= max_jobs)); then
        reap_one
    fi

    echo "Switching to '$branch' and pulling in '$dir'..."
    update_repo "$dir" </dev/null >"$work_dir/${dir%/}.log" 2>&1 &
    job_dir[$!]="$dir"
done

while ((${#job_dir[@]} > 0)); do
    reap_one || break
done

if ((${#failed[@]} > 0)); then
    echo "Failed repositories (${#failed[@]}):"
    printf '  - %s\n' "${failed[@]}"
    exit 1
fi

echo "All repositories are on '$branch' and up to date."

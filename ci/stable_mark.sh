#!/bin/bash
#
# Stolen from Travis via soca-science
# original script:
# https://github.com/JCSDA-internal/soca-science/blob/develop/.github/travisci/stable_mark.sh
#
set -e

# figure out what git hash is associated with each repo in the bundle.
# Note that there are several places where this repo could exist.
bundle_dir=$1
bundle_repos="oops vader saber ioda ufo iodaconv ioda-data ufo-data saber-data \
              fms fv3 femps fv3-jedi-lm fv3-jedi fv3-jedi-data gsibec \
              gsw mom6 soca"
for r in $bundle_repos; do

    echo ""
    echo "Finding hash tag for $r..."
    hash="none"

    # check the repo source ( for uncached repos, i.e. the main test repo)
    if [[ "$hash" == "none" ]]; then
        echo -n "searching src.. "
        src_dir=$bundle_dir/$r
        [[ -d $src_dir ]] \
            && cd $src_dir \
            && hash=$(git rev-parse HEAD || echo "none")
        [[ "$hash" == "none" ]] && echo "NOT found" || echo "FOUND"
    fi

    # if a git hash was found, update the bundle with a tagged version
    echo "git_hash: $hash"
    if [[ $hash != "none" ]]; then
        hash=${hash:0:7}
        echo "changing $r to $hash in $bundle_dir/CMakeLists.txt"
        sed -i "s/\(.*PROJECT \+$r .*\)\(BRANCH\|TAG\) *\([a-zA-Z0-9\/\_\.\-]*\)\(.*\)/\1TAG $hash\4/g" $bundle_dir/CMakeLists.txt
    fi
done

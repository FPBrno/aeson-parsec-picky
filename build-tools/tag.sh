#!/usr/bin/env bash
#
# How it is supposed to be used:
#
# * make changes (and commit)
# * bump version (change version number) (no commit?) (if so, why not take one parameter with version number?)
# * run this script
#
# This script modify cabal file (tag for "this" source repository).
# with current "version" value...
set -eu

declare -r project=aeson-parsec-picky
declare -r cabalfile=${project}.cabal

function die() {
    echo "${@}"
    exit 1
}

function main() {
    if [ ${#} -ne 0 ]; then
        die 'Usage: tag.sh'
    fi

    if ! [ -f "${cabalfile}" ]; then
        die "Script needs to be run in directory where is ${cabalfile}"
    fi

    declare -r last_version=$( sed -n '/^source-repository this$/,/^$/{s/^\s\+tag:\s*v//p}' "${cabalfile}" )
    declare -r current_version=$( sed -n 's/^version:\s*//p' "${cabalfile}" )

    if [[ ${current_version} == ${last_version} ]]; then
        die -e "ERROR: Version have not changed since the last releas\nConsider version bump."
    fi

    sed -i '/^source-repository this$/,/^$/{s/\(tag:\s*\).*/\1v'"${current_version}"'/}' "${cabalfile}"
    git commit -m "Release version ${current_version}" "${cabalfile}"
    git tag "v${current_version}"
    cabal sdist
}

main "${@}"

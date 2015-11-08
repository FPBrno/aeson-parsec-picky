set -eu

declare -r project=aeson-parsec-picky
declare -r cabalfile=${project}.cabal

if [ ${#} -ne 1 ]; then
    echo 'Usage: tag.sh <next-version>'
    exit 1
fi

if ! [ -f "${cabalfile}" ]; then
    echo "Script need to be run in directory where is ${cabalfile}"
    exit 1
fi

declare -r new_version=${1}
declare -r current_version=$( sed -n 's/^version:\s*//p' "${cabalfile}" )

sed -i '/^source-repository this$/,${s/\(tag:\s*\).*/\1v'"${current_version}"'/}' "${cabalfile}"

cabal sdist

sed -i '{s/^\(version:\s*\).*/\1'"${new_version}"'/}' "${cabalfile}"
git commit -m "Release version ${current_version} and bump latest to ${new_version}" "${cabalfile}"
git tag "v${current_version}"

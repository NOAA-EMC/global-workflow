datedir=${DATAROOT}/emc_ecflow_pdy.date.$$.$RANDOM
mkdir -p "$datedir"
pushd "$datedir"
setpdy.sh
. ./PDY
set -u
export CDATE="$PDY$cyc"
set +u
popd
rm -rf "$datedir"


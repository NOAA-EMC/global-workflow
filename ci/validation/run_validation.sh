#!/bin/bash
#set -eu

# ==============================================================================
usage() {
  set +x
  echo
  echo "Usage: $0 -d <directory> -o <output> -h"
  echo
  echo "  -d  Run build and validation for clone in <directory>"
  echo "  -o  Path to output message detailing results of CI tests"
  echo "  -h  display this message and quit"
  echo
  exit 1
}

# ==============================================================================
while getopts "d:o:h" opt; do
  case $opt in
    d)
      repodir=$OPTARG
      ;;
    o)
      outfile=$OPTARG
      ;;
    h|\?|:)
      usage
      ;;
  esac
done
# ==============================================================================
# start output file
echo "Automated GDASApp Validation Testing Results:" > $outfile
echo "Machine: ${TARGET}" >> $outfile
echo '```' >> $outfile
echo "Start: $(date) on $(hostname)" >> $outfile
echo "---------------------------------------------------" >> $outfile
# ==============================================================================
# run build script
cd $repodir
module purge
export BUILD_JOBS=8
rm -rf log.build
./build.sh -a -t $TARGET &>> log.build
build_status=$?
if [ $build_status -eq 0 ]; then
  echo "Build:                                 *SUCCESS*" >> $outfile
  echo "Build: Completed at $(date)" >> $outfile
else
  echo "Build:                                  *FAILED*" >> $outfile
  echo "Build: Failed at $(date)" >> $outfile
  echo "Build: see output at $repodir/log.build" >> $outfile
  echo '```' >> $outfile
  exit $build_status
fi
# ==============================================================================
# run ctests
cd $repodir/build
module use $GDAS_MODULE_USE
module load GDAS/$TARGET
echo "---------------------------------------------------" >> $outfile
rm -rf log.ctest
ctest -R gdasapp --output-on-failure &>> log.ctest
ctest_status=$?
npassed=$(cat log.ctest | grep "tests passed")
if [ $ctest_status -eq 0 ]; then
  echo "Tests:                                 *SUCCESS*" >> $outfile
  echo "Tests: Completed at $(date)" >> $outfile
  echo "Tests: $npassed" >> $outfile
else
  echo "Tests:                                  *Failed*" >> $outfile
  echo "Tests: Failed at $(date)" >> $outfile
  echo "Tests: $npassed" >> $outfile
  cat log.ctest | grep "(Failed)" >> $outfile
  echo "Tests: see output at $repodir/build/log.ctest" >> $outfile
  echo '```' >> $outfile
fi
# ==============================================================================
# run validation scripts
# ---------------------------
# UFO validation with GeoVaLs
cd $repodir/ush/ufoeval
export GDASApp=$repodir
export machine=$TARGET
export STMP=$repodir/../
commit=$(git rev-parse --short HEAD)
echo "Commit hash ${commit} tested on $(date)" >> $repodir/../ufo_geovals_results.txt
./test_yamls.sh >> $repodir/../ufo_geovals_results.txt

cat $repodir/../ufo_geovals_results.txt >> $outfile
# ---------------------------
# create HTML table of status
python $repodir/ci/validation/gen_ufo_geoval_table.py --oblist $repodir/ci/validation/oblist_gfsv16p3.txt --results $repodir/../ufo_geovals_results.txt --output $repodir/../status_geovals.html 
# ---------------------------
# scp HTML table of status
scp -r $repodir/../status_geovals.html cmartin@emcrzdm.ncep.noaa.gov:/home/www/emc/htdocs/data_assimilation/JEDI/GDAS/UFO/acceptance/status_geovals.html
# ---------------------------
# send email of status if necessary
grep "Fails" $repodir/../ufo_geovals_results.txt
if [ $? -eq 0 ]; then
  # one of the tests failed, that is bad!
  PEOPLE="Cory.R.Martin@noaa.gov Andrew.Collard@noaa.gov Emily.Liu@noaa.gov"
  SUBJECT="FAILURE of one or more GDAS UFO validation tests"
  BODY=$repodir/../ufo_geovals_email.txt
  cat > $BODY << EOF
One or more of the GDAS UFO validation tests failed earlier today. See information below.

EOF
  cat $repodir/../ufo_geovals_results.txt >> $BODY
  mail -r "Darth Vader - NOAA Affiliate <darth.vader@noaa.gov>" -s "$SUBJECT" "$PEOPLE" < $BODY
fi

echo '```' >> $outfile


set -x

# Set experiment name and analysis date

exp=$jobname


# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export NMEM_ENKF=20
export NVARS=6

# Set runtime and save directories
tmpdir=$tmpdir/tmp${JCAP}_enkf/${exp}
savdir=$savdir/out${JCAP}_enkf/${exp}

# Specify EnKf fixed field and data directories.
export DATA=$tmpdir

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=120
   export resol=1
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "126" ]]; then
   export LONA=256
   export LATA=128
   export DELTIM=600
   export resol=2
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
export adate=$global_enkf_T62_adate
gdate=`$ndate -06 $global_enkf_T62_adate`
yyg=`echo $gdate | cut -c1-8`
hhg=`echo $gdate | cut -c9-10`
yya=`echo $global_enkf_T62_adate | cut -c1-8`
hha=`echo $global_enkf_T62_adate | cut -c9-10`

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*


# Make enkf namelist

. $scripts/regression_nl_update.sh

export NAM_ENKF="$SETUP_enkf"


. $scripts/regression_namelists.sh

cat << EOF > enkf.nml

$global_enkf_T62_namelist

EOF

# Set fixed files
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   scaninfo = text file with scan angle information
#   ozinfo   = text file with information about assimilation of ozone data
#   convinfo = text file with information about assimilation of conventional data
#   hybens_locinfo = text file with localization informztion

satangl=$fixgsi/global_satangbias.txt
scaninfo=$fixgsi/global_scaninfo.txt
satinfo=$fixgsi/global_satinfo.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
ozinfo=$fixgsi/global_ozinfo.txt
hybens_locinfo=$fixgsi/global_hybens_locinfo.l64.txt


# Copy executable and fixed files to $tmpdir
if [[ $exp = $global_enkf_T62_updat_exp1 ]]; then
   $ncp $enkfexec_updat ./enkf.x
elif [[ $exp = $global_enkf_T62_updat_exp2 ]]; then
   $ncp $enkfexec_updat ./enkf.x
elif [[ $exp = $global_enkf_T62_contrl_exp1 ]]; then
   $ncp $enkfexec_contrl ./enkf.x
elif [[ $exp = $global_enkf_T62_contrl_exp2 ]]; then
   $ncp $enkfexec_contrl ./enkf.x
fi

$ncp $satangl  ./satbias_angle
$ncp $scaninfo ./scaninfo
$ncp $satinfo  ./satinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $hybens_locinfo ./hybens_locinfo


# Copy ensemble data to $tmpdir
list="cnvstat oznstat radstat"
for type in $list; do
   $ncp $global_enkf_T62_datobs/${type}_${adate}_ensmean ./${type}_ensmean
   tar -xvf ${type}_ensmean
done
imem=1
while [[ $imem -le $NMEM_ENKF ]]; do
   member="_mem"`printf %03i $imem`
   list="cnvstat oznstat radstat"
   for type in $list; do
      $ncp $global_enkf_T62_datobs/${type}_${adate}${member} ./${type}${member}
      tar -xvf ${type}${member}
   done
   sigens=$global_enkf_T62_datges/sfg_${gdate}_fhr06s${member}
   $ncp $sigens sfg_${global_enkf_T62_adate}_fhr06${member}
   (( imem = $imem + 1 ))
done


# Copy bias correction, atmospheric and surface files
if [ "$machine" = "Zeus" -o "$machine" = "Theia" ]; then
   $ncp $global_enkf_T62_datges/biascr.gdas.$gdate.orig     ./satbias_in
   $ncp $global_enkf_T62_datges/satang.gdas.$gdate.orig     ./satbias_ang.in
else
   $ncp $global_enkf_T62_datges/biascr_int_${gdate}_ensmean ./satbias_in
   $ncp $global_enkf_T62_datges/satang.gdas.$gdate          ./satbias_ang.in
fi

$ncp $global_enkf_T62_datges/sfg_${gdate}_fhr06_ensmean ./sfg_${global_enkf_T62_adate}_fhr06_ensmean
$ncp $global_enkf_T62_datges/bfg_${gdate}_fhr06_ensmean ./bfg_${global_enkf_T62_adate}_fhr06_ensmean


# Run enkf under Parallel Operating Environment (poe) on NCEP IBM
if [ "$machine" = "Zeus" -o "$machine" = "Theia" ]; then
   cd $tmpdir/
   echo "run enkf now"

   export MPI_DISPLAY_SETTINGS=YES
   export MPI_STATS=YES
   export MPI_STATS_FILE=mpi_tmp.out

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export OMP_NUM_THREADS=1

#  module load intel
#  module load mpt
   echo "JOB ID : $PBS_JOBID"
   eval "$launcher -v -np $PBS_NP $tmpdir/enkf.x > stdout"

elif [[ "$machine" = "WCOSS" ]]; then

   mpirun.lsf $tmpdir/enkf.x < enkf.nml > stdout

fi

rc=$?

exit


set -x

# Set experiment name and analysis date
exp=$jobname

#exp=$exp1_rtma_updat
#adate=$adate_regional_rtma_binary

# Set path/file for gsi executable
#gsiexec=$updat

# Set resoltion and other dependent parameters
#export JCAP=62
export LEVS=60
export JCAP_B=$JCAP
export DELTIM=1200

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_rtma/${exp}
savdir=$savdir/outreg/rtma/${exp}

# Specify GSI fixed field and data directories.

#datobs=$datobs_rtma/$adate
#datges=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -12 $rtma_adate`
cya=`echo $rtma_adate | cut -c9-10`
cyg=`echo $gdate | cut -c9-10`
prefixa=nam.t${cya}z
prefixg=na12snmm.t${cyg}z
suffix=tm00.bufr_d

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
	# Copy co2 files to $tmpdir
	co2dir=${CO2DIR:-$fixgsi}
	yyyy=$(echo ${CDATE:-$rtma_adate}|cut -c1-4)
	rm ./global_co2_data.txt
		co2=$co2dir/global_co2.gcmscl_$yyyy.txt
		if [ -s $co2 ] ; then
			$ncp $co2 ./global_co2_data.txt
		fi
	if [ ! -s ./global_co2_data.txt ] ; then
		echo "\./global_co2_data.txt" not created
		exit 1
   fi
fi

# Make gsi namelist

. $scripts/regression_nl_update.sh

SETUP="$SETUP_update"
GRIDOPTS="$GRIDOPTS_update"
BKGVERR="$BKGVERR_update"
ANBKGERR="$ANBKERR_update"
JCOPTS="$JCOPTS_update"
STRONGOPTS="$STRONGOPTS_update"
OBSQC="$OBSQC_update"
OBSINPUT="$OBSINPUT_update"
SUPERRAD="$SUPERRAD_update"
SINGLEOB="$SINGLEOB_update"

. $scripts/regression_namelists.sh
cat << EOF > gsiparm.anl

$RTMA_namelist

EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   uselist  =
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   reject   =
#   slmask   =
#   flt*     =

anavinfo=$fixgsi/anavinfo_rtma_gust_vis_7vars
berror=$fixgsi/$endianness/new_rtma_regional_nmm_berror.f77.gcv
errtable=$fixgsi/new_rtma_nam_errtable.r3dv
convinfo=$fixgsi/new_rtma_regional_convinfo.txt
mesonetuselist=$fixgsi/new_rtma_mesonet_uselist.txt
mesonet_stnuselist=$fixgsi/new_rtma_ruc2_wind-uselist-noMETAR.dat
wbinuselist=$fixgsi/new_rtma_wbinuselist
if [[ "$endianness" = "Little_Endian" ]]; then
   slmask=/scratch2/portfolios/NCEPDEV/meso/save/Manuel.Pondeca/aor_project/conusext_2.5km/hresext_rtma/fix.rtma/rtma_cohresext_slmask.dat_nolakes_le
   terrain=/scratch2/portfolios/NCEPDEV/meso/save/Manuel.Pondeca/aor_project/conusext_2.5km/hresext_rtma/fix.rtma/rtma_cohresext_terrain.dat_le
elif [[ "$endianness" = "Big_Endian" ]]; then
   slmask=$fixgsi/new_rtma_conus_slmask.dat
   terrain=$fixgsi/new_rtma_conus_terrain.dat
fi
bufrtable=$fixgsi/rtma_prepobs_prep.bufrtable

t_rejectlist=$fixgsi/new_rtma_t_rejectlist
p_rejectlist=$fixgsi/new_rtma_p_rejectlist
q_rejectlist=$fixgsi/new_rtma_q_rejectlist
w_rejectlist=$fixgsi/new_rtma_w_rejectlist

if [[ "$endianness" = "Little_Endian" ]]; then
   random_flips=/scratch2/portfolios/NCEPDEV/meso/save/Manuel.Pondeca/folks/for_patrick/15Aug2012/hresext_rtma/fix.rtma/fixgsi_200609/normalization/random_flips_le
elif [[ "$endianness" = "Big_Endian" ]]; then
   random_flips=$fixgsi/new_rtma_random_flips
fi

flt_chi=$fixgsi/$endianness/new_rtma_fltnorm.dat_chi
flt_ist=$fixgsi/$endianness/new_rtma_fltnorm.dat_ist
flt_ps=$fixgsi/$endianness/new_rtma_fltnorm.dat_ps
flt_lst=$fixgsi/$endianness/new_rtma_fltnorm.dat_lst
flt_oz=$fixgsi/$endianness/new_rtma_fltnorm.dat_oz
flt_pseudorh=$fixgsi/$endianness/new_rtma_fltnorm.dat_pseudorh
flt_psi=$fixgsi/$endianness/new_rtma_fltnorm.dat_psi
flt_qw=$fixgsi/$endianness/new_rtma_fltnorm.dat_qw
flt_sst=$fixgsi/$endianness/new_rtma_fltnorm.dat_sst
flt_t=$fixgsi/$endianness/new_rtma_fltnorm.dat_t
flt_gust=$fixgsi/$endianness/new_rtma_fltnorm.dat_gust
flt_vis=$fixgsi/$endianness/new_rtma_fltnorm.dat_vis

prmcard=$fixgsi/new_rtma_parmcard_input

# Copy executable and fixed files to $tmpdir
if [[ $exp = $rtma_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $rtma_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $rtma_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $rtma_contrl_exp2 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

$ncp $anavinfo           ./anavinfo
$ncp $berror             ./berror_stats
$ncp $convinfo           ./convinfo
$ncp $errtable           ./errtable
$ncp $mesonetuselist     ./mesonetuselist
$ncp $mesonet_stnuselist ./mesonet_stnuselist
$ncp $wbinuselist        ./wbinuselist
$ncp $slmask             ./rtma_slmask.dat
$ncp $terrain            ./rtma_terrain.dat
$ncp $bufrtable          ./prepobs_prep.bufrtable

$ncp $t_rejectlist       ./t_rejectlist
$ncp $p_rejectlist       ./p_rejectlist
$ncp $q_rejectlist       ./q_rejectlist
$ncp $w_rejectlist       ./w_rejectlist

$ncp $random_flips        ./random_flips

$ncp $flt_chi            ./fltnorm.dat_chi
$ncp $flt_ist            ./fltnorm.dat_ist
$ncp $flt_ps             ./fltnorm.dat_ps
$ncp $flt_lst            ./fltnorm.dat_lst
$ncp $flt_oz             ./fltnorm.dat_oz
$ncp $flt_pseudorh       ./fltnorm.dat_pseudorh
$ncp $flt_psi            ./fltnorm.dat_psi
$ncp $flt_qw             ./fltnorm.dat_qw
$ncp $flt_sst            ./fltnorm.dat_sst
$ncp $flt_t              ./fltnorm.dat_t
$ncp $flt_gust           ./fltnorm.dat_gust
$ncp $flt_vis            ./fltnorm.dat_vis

$ncp $prmcard            ./parmcard_input

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $fixcrtm/${file}.SpcCoeff.bin ./
    $ncp $fixcrtm/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $rtma_obs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr
$ncp $rtma_obs/rtma.t${cya}z.satwnd.tm00.bufr_d ./satwndbufr


# Copy first guess
$ncp $rtma_ges/rtma.t${cya}z.2dvar_input   ./wrf_inout
cp wrf_inout wrf_ges

if [[ "$machine" = "Zeus" ]]; then

   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"

elif [[ "$machine" = "WCOSS" ]]; then

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
   mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${rtma_adate}
$ncp wrf_inout       $savdir/wrfanl.${rtma_adate}
$ncp siganl          $savdir/siganl.${rtma_adate}
$ncp sigf06          $savdir/sigf06.${rtma_adate}
$ncp bckg_dxdy.dat   $savdir/bckg_dxdy.${rtma_adate}.dat
$ncp bckg_qsat.dat   $savdir/bckg_qsat.${rtma_adate}.dat
$ncp bckg_psfc.dat   $savdir/bckg_psfc.${rtma_adate}.dat
$ncp bckgvar.dat_psi $savdir/bckgvar_psi.${rtma_adate}.dat
$ncp bckgvar.dat_chi $savdir/bckgvar_chi.${rtma_adate}.dat
$ncp bckgvar.dat_ps  $savdir/bckgvar_ps.${rtma_adate}.dat
$ncp bckgvar.dat_t   $savdir/bckgvar_t0.${rtma_adate}.dat
$ncp bckgvar.dat_pseudorh $savdir/bckgvar_pseudorh.${rtma_adate}.dat
$ncp bckgvar.dat_gust $savdir/bckgvar.dat_gust.${rtma_adate}.dat
$ncp bckgvar.dat_vis  $savdir/bckgvar.dat_vis.${rtma_adate}.dat
$ncp tobs_allcv_groups  $savdir/tobs_allcv_groups
$ncp qobs_allcv_groups  $savdir/qobs_allcv_groups
$ncp psobs_allcv_groups $savdir/psobs_allcv_groups
$ncp uvobs_allcv_groups $savdir/uvobs_allcv_groups


# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

# Collect diagnostic files for obs types (groups) below
   listall="conv"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${rtma_adate}
         compress diag_${type}_${string}.${rtma_adate}
         $ncp diag_${type}_${string}.${rtma_adate}.Z $savdir/
      fi
   done
done

exit
# Standalone script used to pass namelist updates to the regression tests.

# First, generate new variable to hole the first 6 characters of the experiment.

global_test = cut c1-6 $exp

if [[ $gsiexec = $updat ]]; then
   export SETUP_update="lrun_subdirs=.true."
else
   export SETUP_update=""
fi
export GRIDOPTS_update=""
export BKGVERR_update=""
export ANBKGERR_update=""
export JCOPTS_update=""
if [[ $global_test = "global" ]]; then
   if [[ $gsiexec = $updat ]]; then
      export STRONGOPTS_update="tlnmc_option=1"
   else
      export STRONGOPTS_update="hybens_inmc_option=1,jcstrong_option=2,jcstrong=.true."
   fi
fi
export OBSQC_update=""
export OBSINPUT_update=""
export SUPERRAD_update=""
export SINGLEOB_update=""

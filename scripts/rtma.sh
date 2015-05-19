
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

if [ "$debug" = ".false." ]; then
   . $scripts/regression_namelists.sh
else
   . $scripts/regression_namelists_db.sh
fi
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

anavinfo=$fixgsi/anavinfo_rtma
berror=$fixgsi/$endianness/rtma_regional_nmm_berror.f77.gcv
errtable=$fixgsi/rtma_errtable.r3dv
convinfo=$fixgsi/rtma_convinfo.txt
mesonetuselist=$fixgsi/rtma_mesonet_uselist.txt
mesonet_stnuselist=$fixgsi/rtma_ruc2_wind-uselist-noMETAR.dat
wbinuselist=$fixgsi/rtma_wbinuselist
slmask=$fixgsi/$endianness/rtma_conus_slmask.dat
terrain=$fixgsi/$endianness/rtma_conus_terrain.dat
bufrtable=$fixgsi/rtma_prepobs_prep.bufrtable

t_rejectlist=$fixgsi/rtma_t_rejectlist
p_rejectlist=$fixgsi/rtma_p_rejectlist
q_rejectlist=$fixgsi/rtma_q_rejectlist
w_rejectlist=$fixgsi/rtma_w_rejectlist
t_day_rejectlist=$fixgsi/rtma_t_day_rejectlist
t_night_rejectlist=$fixgsi/rtma_t_night_rejectlist
q_day_rejectlist=$fixgsi/rtma_q_day_rejectlist
q_night_rejectlist=$fixgsi/rtma_q_night_rejectlist

if [[ "$endianness" = "Little_Endian" ]]; then
   random_flips=/scratch2/portfolios/NCEPDEV/meso/save/Manuel.Pondeca/folks/for_patrick/15Aug2012/hresext_rtma/fix.rtma/fixgsi_200609/normalization/random_flips_le
elif [[ "$endianness" = "Big_Endian" ]]; then
   random_flips=$fixgsi/rtma_random_flips
fi

flt_psi=$fixgsi/$endianness/rtma_fltnorm.dat_psi
flt_chi=$fixgsi/$endianness/rtma_fltnorm.dat_chi
flt_ps=$fixgsi/$endianness/rtma_fltnorm.dat_ps
flt_pseudorh=$fixgsi/$endianness/rtma_fltnorm.dat_pseudorh
flt_t=$fixgsi/$endianness/rtma_fltnorm.dat_t
flt_gust=$fixgsi/$endianness/rtma_fltnorm.dat_gust
flt_vis=$fixgsi/$endianness/rtma_fltnorm.dat_vis
flt_sfwter=$fixgsi/$endianness/rtma_fltnorm.dat_sfwter
flt_vpwter=$fixgsi/$endianness/rtma_fltnorm.dat_vpwter
flt_pswter=$fixgsi/$endianness/rtma_fltnorm.dat_pswter
flt_qwter=$fixgsi/$endianness/rtma_fltnorm.dat_qwter
flt_twter=$fixgsi/$endianness/rtma_fltnorm.dat_twter
flt_gustwter=$fixgsi/$endianness/rtma_fltnorm.dat_gustwter
flt_wspd10m=$fixgsi/$endianness/rtma_fltnorm.dat_wspd10m
flt_wspd10mwter=$fixgsi/$endianness/rtma_fltnorm.dat_wspd10mwter
flt_td2m=$fixgsi/$endianness/rtma_fltnorm.dat_td2m
flt_td2mwter=$fixgsi/$endianness/rtma_fltnorm.dat_td2mwter
flt_mitm=$fixgsi/$endianness/rtma_fltnorm.dat_mitm
flt_mitmwter=$fixgsi/$endianness/rtma_fltnorm.dat_mitmwter
flt_mxtm=$fixgsi/$endianness/rtma_fltnorm.dat_mxtm
flt_mxtmwter=$fixgsi/$endianness/rtma_fltnorm.dat_mxtmwter
flt_pmsl=$fixgsi/$endianness/rtma_fltnorm.dat_pmsl
flt_howv=$fixgsi/$endianness/rtma_fltnorm.dat_howv
flt_tcamt=$fixgsi/$endianness/rtma_fltnorm.dat_tcamt

prmcard=$fixgsi/rtma_parmcard_input

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
$ncp $t_day_rejectlist   ./t_day_rejectlist
$ncp $t_night_rejectlist ./t_night_rejectlist
$ncp $q_day_rejectlist   ./q_day_rejectlist
$ncp $q_night_rejectlist ./q_night_rejectlist

$ncp $random_flips        ./random_flips

$ncp $flt_psi            ./fltnorm.dat_psi
$ncp $flt_chi            ./fltnorm.dat_chi
$ncp $flt_ps             ./fltnorm.dat_ps
$ncp $flt_pseudorh       ./fltnorm.dat_pseudorh
$ncp $flt_t              ./fltnorm.dat_t
$ncp $flt_gust           ./fltnorm.dat_gust
$ncp $flt_vis            ./fltnorm.dat_vis
$ncp $flt_sfwter         ./fltnorm.dat_sfwter
$ncp $flt_vpwter         ./fltnorm.dat_vpwter
$ncp $flt_pswter         ./fltnorm.dat_pswter
$ncp $flt_qwter          ./fltnorm.dat_qwter
$ncp $flt_twter          ./fltnorm.dat_twter
$ncp $flt_gustwter       ./fltnorm.dat_gustwter
$ncp $flt_wspd10m        ./fltnorm.dat_wspd10m
$ncp $flt_wspd10mwter    ./fltnorm.dat_wspd10mwter
$ncp $flt_td2m           ./fltnorm.dat_td2m
$ncp $flt_td2mwter       ./fltnorm.dat_td2mwter
$ncp $flt_mitm           ./fltnorm.dat_mitm
$ncp $flt_mitmwter       ./fltnorm.dat_mitmwter
$ncp $flt_mxtm           ./fltnorm.dat_mxtm
$ncp $flt_mxtmwter       ./fltnorm.dat_mxtmwter
$ncp $flt_pmsl           ./fltnorm.dat_pmsl
$ncp $flt_howv           ./fltnorm.dat_howv
$ncp $flt_tcamt          ./fltnorm.dat_tcamt

$ncp $prmcard            ./parmcard_input

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $fixcrtm/${file}.SpcCoeff.bin ./
    $ncp $fixcrtm/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $rtma_obs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr
$ncp $rtma_obs/rtma.t${cya}z.satwnd.tm00.bufr_d ./satwndbufr
$ncp $rtma_obs/rtma.t${cya}z.goessky.tm00.bufr_d ./goessky


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

   export MP_USE_BULK_XFER=yes

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
   mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* >         $savdir/stdout.anl.${rtma_adate}
$ncp wrf_inout               $savdir/wrfanl.${rtma_adate}
$ncp siganl                  $savdir/siganl.${rtma_adate}
$ncp sigf06                  $savdir/sigf06.${rtma_adate}
$ncp bckg_dxdy.dat           $savdir/bckg_dxdy.${rtma_adate}.dat
$ncp bckg_qsat.dat           $savdir/bckg_qsat.${rtma_adate}.dat
$ncp bckg_psfc.dat           $savdir/bckg_psfc.${rtma_adate}.dat
$ncp bckgvar.dat_psi         $savdir/bckgvar_psi.${rtma_adate}.dat
$ncp bckgvar.dat_chi         $savdir/bckgvar_chi.${rtma_adate}.dat
$ncp bckgvar.dat_ps          $savdir/bckgvar_ps.${rtma_adate}.dat
$ncp bckgvar.dat_t           $savdir/bckgvar_t0.${rtma_adate}.dat
$ncp bckgvar.dat_pseudorh    $savdir/bckgvar_pseudorh.${rtma_adate}.dat
$ncp bckgvar.dat_gust        $savdir/bckgvar.dat_gust.${rtma_adate}.dat
$ncp bckgvar.dat_vis         $savdir/bckgvar.dat_vis.${rtma_adate}.dat
$ncp bckgvar.dat_td2m        $savdir/bckgvar.dat_td2m.${rtma_adate}.dat
$ncp bckgvar.dat_wspd10m     $savdir/bckgvar.dat_wspd10m.${rtma_adate}.dat
$ncp bckgvar.dat_mitm        $savdir/bckgvar.dat_mitm.${rtma_adate}.dat
$ncp bckgvar.dat_mxtm        $savdir/bckgvar.dat_mxtm.${rtma_adate}.dat
$ncp bckgvar.dat_pmsl        $savdir/bckgvar.dat_pmsl.${rtma_adate}.dat
$ncp bckgvar.dat_howv        $savdir/bckgvar.dat_howv.${rtma_adate}.dat
$ncp bckgvar.dat_tcamt       $savdir/bckgvar.dat_tcamt.${rtma_adate}.dat
$ncp bckgvar.dat_sfwter      $savdir/bckgvar.dat_sfwter.${rtma_adate}.dat
$ncp bckgvar.dat_vpwter      $savdir/bckgvar.dat_vpwter.${rtma_adate}.dat
$ncp bckgvar.dat_twter       $savdir/bckgvar.dat_twter.${rtma_adate}.dat
$ncp bckgvar.dat_qwter       $savdir/bckgvar.dat_qwter.${rtma_adate}.dat
$ncp bckgvar.dat_gustwter    $savdir/bckgvar.dat_gustwter.${rtma_adate}.dat
$ncp bckgvar.dat_pswter      $savdir/bckgvar.dat_pswter.${rtma_adate}.dat
$ncp bckgvar.dat_mitmwter    $savdir/bckgvar.dat_mitmwter.${rtma_adate}.dat
$ncp bckgvar.dat_mxtmwter    $savdir/bckgvar.dat_mxtmwter.${rtma_adate}.dat
$ncp bckgvar.dat_wspd10mwter $savdir/bckgvar.dat_wspd10mwter.${rtma_adate}.dat
$ncp bckgvar.dat_td2mwter    $savdir/bckgvar.dat_td2mwter.${rtma_adate}.dat
$ncp tobs_allcv_groups       $savdir/tobs_allcv_groups
$ncp qobs_allcv_groups       $savdir/qobs_allcv_groups
$ncp psobs_allcv_groups      $savdir/psobs_allcv_groups
$ncp uvobs_allcv_groups      $savdir/uvobs_allcv_groups
$ncp gustobs_allcv_groups    $savdir/gustobs_allcv_groups
$ncp visobs_allcv_groups     $savdir/visobs_allcv_groups 
$ncp wspd10mobs_allcv_groups $savdir/wspd10mobs_allcv
$ncp td2mobs_allcv_groups    $savdir/td2mobs_allcv_groups
$ncp mitmobs_allcv_groups    $savdir/mitmobs_allcv_groups
$ncp mxtmobs_allcv_groups    $savdir/mxtmobs_allcv_groups
$ncp pmslobs_allcv_groups    $savdir/pmslobs_allcv_groups
$ncp howvobs_allcv_groups    $savdir/howvobs_allcv_groups
$ncp tcamtobs_allcv_groups   $savdir/tcamtobs_allcv_groups

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

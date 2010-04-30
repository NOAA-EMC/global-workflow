#!/bin/sh

#@ job_name=regression_debug_test
#@ error=RTMA_debug_test.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ node_resources = ConsumableMemory(110 GB)
#@ parallel_threads = 1
#@ task_affinity = core(1)
#@ bulkxfer=yes
#@ class=dev
#@ group=dev
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 1:00:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ restart=no
#@ queue

. regression_var.sh

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_SYNC_QP=yes

# Set environment variables for no threads
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite

# Set experiment name and analysis date
exp=$exp1_rtma_sub_1node
adate=$adate_regional

# Set path/file for gsi executable
gsiexec=$subversion

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${rtma}/${exp}
savdir=$ptmp_loc/outreg/${rtma}/${exp}

# Specify GSI fixed field and data directories.

datobs=$datobs_rtma/$adate
datges=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -12 $adate`
cya=`echo $adate | cut -c9-10`
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

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
[ -n "$(echo $LOADL_STEP_NAME|grep update)" ] && ICO2=2
SETUP=" igfsco2=$ICO2, "
if [ $ICO2 -gt 0 ] ; then
	# Copy co2 files to $tmpdir
	co2dir=${CO2DIR:-$fix_file}
	yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
	rm ./global_co2_data.txt
	while [ $yyyy -ge 1957 ] ;do
		co2=$co2dir/global_co2historicaldata_$yyyy.txt
		if [ -s $co2 ] ; then
			$ncp $co2 ./global_co2_data.txt
		break
		fi
		((yyyy-=1))
	done
	if [ ! -s ./global_co2_data.txt ] ; then
		echo "\./global_co2_data.txt" not created
		exit 1
   fi
fi
. $scripts/regression_namelists_db.sh
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

anavinfo=$fix_file/anavinfo_rtma
#berror=$fix_file/new_rtma_regional_nmm_berror.f77
berror=$fix_file/new_rtma_regional_nmm_berror.f77.gcv
errtable=$fix_file/new_rtma_nam_errtable.r3dv
convinfo=$fix_file/new_rtma_regional_convinfo.txt
mesonetuselist=$fix_file/new_rtma_mesonet_uselist.txt
mesonet_stnuselist=$fix_file/new_rtma_ruc2_wind-uselist-noMETAR.dat
slmask=$fix_file/new_rtma_conus_slmask.dat
terrain=$fix_file/new_rtma_conus_terrain.dat
bufrtable=$fix_file/rtma_prepobs_prep.bufrtable

t_rejectlist=$fix_file/new_rtma_t_rejectlist
p_rejectlist=$fix_file/new_rtma_p_rejectlist
q_rejectlist=$fix_file/new_rtma_q_rejectlist
w_rejectlist=$fix_file/new_rtma_w_rejectlist

random_flips=$fix_file/new_rtma_random_flips

flt_chi=$fix_file/new_rtma_fltnorm.dat_chi
flt_ist=$fix_file/new_rtma_fltnorm.dat_ist
flt_ps=$fix_file/new_rtma_fltnorm.dat_ps
flt_lst=$fix_file/new_rtma_fltnorm.dat_lst
flt_oz=$fix_file/new_rtma_fltnorm.dat_oz
flt_pseudorh=$fix_file/new_rtma_fltnorm.dat_pseudorh
flt_psi=$fix_file/new_rtma_fltnorm.dat_psi
flt_qw=$fix_file/new_rtma_fltnorm.dat_qw
flt_sst=$fix_file/new_rtma_fltnorm.dat_sst
flt_t=$fix_file/new_rtma_fltnorm.dat_t

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo           ./anavinfo
$ncp $berror             ./berror_stats
$ncp $convinfo           ./convinfo
$ncp $errtable           ./errtable
$ncp $mesonetuselist     ./mesonetuselist
$ncp $mesonet_stnuselist ./mesonet_stnuselist
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

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $crtm_coef/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $crtm_coef/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/rtma.t${cya}z.prepbufr.tm00 ./prepbufr

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
##poe hpmcount $tmpdir/gsi.x < gsiparm.anl > stdout
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" = "0" ]]; then
   cd $regression_vfydir
   {
    echo
    echo 'RTMA debug test has passed'
   } >> $rtma_regression
fi

exit

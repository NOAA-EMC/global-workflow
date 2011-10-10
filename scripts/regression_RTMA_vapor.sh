#!/bin/sh

#@ error=$(job_name).$(step_name).e$(jobid)
#@ job_type=parallel
#@ class=jcsda
#@ group=jcsda
#@ account_no = JCSDA008-RES

#@ job_name=regression_test
#@ step_name=gsi_rtma_updat
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=10
#@ task_affinity = core(1)
#@ parallel_threads = 1 
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:10:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ restart=no
#@ queue

#@ step_name=gsi_rtma_updat2
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=20
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:10:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ restart=no
#@ dependency = (gsi_rtma_updat==0)
#@ queue

#@ step_name=gsi_rtma_cntrl
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=10
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:10:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ restart=no
#@ dependency = (gsi_rtma_updat2==0)
#@ queue

#@ step_name=gsi_rtma_cntrl2
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=20
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:10:00
#@ startdate = 10/27/05 20:00
#@ notification=error
#@ restart=no
#@ dependency = (gsi_rtma_cntrl==0)
#@ queue

#@ step_name=rtma_regression
#@ job_type=serial
#@ task_affinity = cpu(1)
#@ node_usage = shared
#@ node_resources = ConsumableMemory(2000 MB)
#@ wall_clock_limit = 0:10:00
#@ notification=error
#@ restart=no
#@ dependency=(gsi_rtma_cntrl2==0)
#@ queue

. ./regression_var.sh

case $LOADL_STEP_NAME in
  gsi_rtma_updat)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for threading and stacksize
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
exp=$exp1_rtma_updat
adate=$adate_regional_rtma_binary

# Set path/file for gsi executable
gsiexec=$updat

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

anavinfo=$fix_file/anavinfo_rtma
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
$ncp $datobs/rtma.t${cya}z.satwnd.tm00.bufr_d ./satwnd

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_rtma_updat' has failed to run to completion, with an error code of '$rc''
   } >> $rtma_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sigf03          $savdir/sigf03.${adate}
$ncp bckg_dxdy.dat   $savdir/bckg_dxdy.${adate}.dat
$ncp bckg_qsat.dat   $savdir/bckg_qsat.${adate}.dat
$ncp bckg_psfc.dat   $savdir/bckg_psfc.${adate}.dat
$ncp bckgvar.dat_psi $savdir/bckgvar_psi.${adate}.dat
$ncp bckgvar.dat_chi $savdir/bckgvar_chi.${adate}.dat
$ncp bckgvar.dat_ps  $savdir/bckgvar_ps.${adate}.dat
$ncp bckgvar.dat_t   $savdir/bckgvar_t0.${adate}.dat
$ncp bckgvar.dat_pseudorh $savdir/bckgvar_pseudorh.${adate}.dat


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
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

exit ;;

  gsi_rtma_updat2)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for threading and stacksize
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
exp=$exp2_rtma_updat
adate=$adate_regional_rtma_binary

# Set path/file for gsi executable
gsiexec=$updat

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

anavinfo=$fix_file/anavinfo_rtma
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

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

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
$ncp $datobs/rtma.t${cya}z.satwnd.tm00.bufr_d ./satwnd

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp2_rtma_updat' has failed to run to completion, with an error code of '$rc''
   } >> $rtma_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sigf03          $savdir/sigf03.${adate}
$ncp bckg_dxdy.dat   $savdir/bckg_dxdy.${adate}.dat
$ncp bckg_qsat.dat   $savdir/bckg_qsat.${adate}.dat
$ncp bckg_psfc.dat   $savdir/bckg_psfc.${adate}.dat
$ncp bckgvar.dat_psi $savdir/bckgvar_psi.${adate}.dat
$ncp bckgvar.dat_chi $savdir/bckgvar_chi.${adate}.dat
$ncp bckgvar.dat_ps  $savdir/bckgvar_ps.${adate}.dat
$ncp bckgvar.dat_t   $savdir/bckgvar_t0.${adate}.dat
$ncp bckgvar.dat_pseudorh $savdir/bckgvar_pseudorh.${adate}.dat


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
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

exit ;;

  gsi_rtma_cntrl)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for threading and stacksize
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
exp=$exp1_rtma_cntrl
adate=$adate_regional_rtma_binary

# Set path/file for gsi executable
gsiexec=$cntrl

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

anavinfo=$fix_file/anavinfo_rtma
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

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

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
$ncp $datobs/rtma.t${cya}z.satwnd.tm00.bufr_d ./satwnd

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_rtma_cntrl' has failed to run to completion, with an error code of '$rc''
   } >> $rtma_regression
   $step_name==$rc
   exit
fi

mkdir $noscrub/tmpreg_${rtma}
mkdir $control_RTMA
cp -rp stdout $control_RTMA
cp -rp fort.220 $control_RTMA
cp -rp siganl $control_RTMA

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sigf03          $savdir/sigf03.${adate}
$ncp bckg_dxdy.dat   $savdir/bckg_dxdy.${adate}.dat
$ncp bckg_qsat.dat   $savdir/bckg_qsat.${adate}.dat
$ncp bckg_psfc.dat   $savdir/bckg_psfc.${adate}.dat
$ncp bckgvar.dat_psi $savdir/bckgvar_psi.${adate}.dat
$ncp bckgvar.dat_chi $savdir/bckgvar_chi.${adate}.dat
$ncp bckgvar.dat_ps  $savdir/bckgvar_ps.${adate}.dat
$ncp bckgvar.dat_t   $savdir/bckgvar_t0.${adate}.dat
$ncp bckgvar.dat_pseudorh $savdir/bckgvar_pseudorh.${adate}.dat


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
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

exit ;;

  gsi_rtma_cntrl2)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for threading and stacksize
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"

# Recommended MPI environment variable setttings from IBM
# (Appendix E, HPC Clusters Using InfiniBand on IBM Power Systems Servers)
export LAPI_DEBUG_ENABLE_AFFINITY=YES
export MP_FIFO_MTU=4K
export MP_SYNC_QP=YES
export MP_SHM_ATTACH_THRESH=500000 # default is better sometimes
export MP_EUIDEVELOP=min
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=64k
export MP_RC_MAX_QP=8192
export LAPI_DEBUG_RC_DREG_THRESHOLD=1000000
export LAPI_DEBUG_QP_NOTIFICATION=no
export LAPI_DEBUG_RC_INIT_SETUP=yes

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes
export MP_INFOLEVEL=1

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set experiment name and analysis date
exp=$exp2_rtma_cntrl
adate=$adate_regional_rtma_binary

# Set path/file for gsi executable
gsiexec=$cntrl

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

anavinfo=$fix_file/anavinfo_rtma
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

$ncp $flt_chi       ./fltnorm.dat_chi
$ncp $flt_ist       ./fltnorm.dat_ist
$ncp $flt_ps        ./fltnorm.dat_ps
$ncp $flt_lst       ./fltnorm.dat_lst
$ncp $flt_oz        ./fltnorm.dat_oz
$ncp $flt_pseudorh  ./fltnorm.dat_pseudorh
$ncp $flt_psi       ./fltnorm.dat_psi
$ncp $flt_qw        ./fltnorm.dat_qw
$ncp $flt_sst       ./fltnorm.dat_sst
$ncp $flt_t         ./fltnorm.dat_t

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
$ncp $datobs/rtma.t${cya}z.satwnd.tm00.bufr_d ./satwnd

# Copy first guess
$ncp $datges/rtma.t${cya}z.2dvar_input   ./wrf_inout

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp2_rtma_cntrl' has failed to run to completion, with an error code of '$rc''
   } >> $rtma_regression
   $step_name==$rc
   exit
fi

mkdir $noscrub/tmpreg_${rtma}
mkdir $control_RTMA2
cp -rp stdout $control_RTMA2
cp -rp fort.220 $control_RTMA2
cp -rp siganl $control_RTMA2

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sigf03          $savdir/sigf03.${adate}
$ncp bckg_dxdy.dat   $savdir/bckg_dxdy.${adate}.dat
$ncp bckg_qsat.dat   $savdir/bckg_qsat.${adate}.dat
$ncp bckg_psfc.dat   $savdir/bckg_psfc.${adate}.dat
$ncp bckgvar.dat_psi $savdir/bckgvar_psi.${adate}.dat
$ncp bckgvar.dat_chi $savdir/bckgvar_chi.${adate}.dat
$ncp bckgvar.dat_ps  $savdir/bckgvar_ps.${adate}.dat
$ncp bckgvar.dat_t   $savdir/bckgvar_t0.${adate}.dat
$ncp bckgvar.dat_pseudorh $savdir/bckgvar_pseudorh.${adate}.dat


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
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

exit ;;

  rtma_regression)

set -ax

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the control run

exp1=$exp1_rtma_updat
exp2=$exp1_rtma_cntrl
exp3=$exp2_rtma_updat

# Choose global, regional, or RTMA
input=tmpreg_${rtma}

# Name output file
output=$rtma_regression

# Give location of analysis results, and choose location for regression output
savdir=$ptmp_loc/$input
vfydir=$regression_vfydir

ncp=/bin/cp

# Name and create temporary directory
tmpdir=$ptmp_loc/${compare}/$input/${exp1}_vs_${exp2}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Other required constants for regression testing
maxtime=1200
# Dew/Mist=26 GB/16 tasks per node
##maxmem=$((1500000*1))
# Vapor=110 GB/48 tasks per node
##maxmem=$((2300000*1))
# Cirrus=110 GB/32 tasks per node
maxmem=$((3400000*1))

# Copy stdout and fort.220 files 
# from $savdir to $tmpdir
list="$exp1 $exp2 $exp3"
for exp in $list; do
   $ncp $savdir/$exp/stdout ./stdout.$exp
   $ncp $savdir/$exp/fort.220 ./fort.220.$exp
   $ncp $savdir/$exp/siganl ./siganl.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep 'a,b' fort.220.$exp > penalty.$exp.txt
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.exp1.txt with penalty.exp2.txt)
diff penalty.$exp1.txt penalty.$exp2.txt > penalty.${exp1}-${exp2}.txt
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt

# Give location of additional output files for scalability testing
exp1_scale=$exp2_rtma_updat
exp2_scale=$exp2_rtma_cntrl

# Copy stdout for additional scalability testing
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   $ncp $savdir/$exp_scale/stdout ./stdout.$exp_scale
done

# Grep out run time from stdout file
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   grep 'The total amount of wall time' stdout.$exp_scale > runtime.$exp_scale.txt
   grep 'The maximum resident set size' stdout.$exp_scale > memory.$exp_scale.txt
done

# Important values used to calculate timethresh and memthresh below
# Values below can be fine tuned to make the regression more or less aggressive
# Currently using a value of 10%

timedif=10
memdiff=10
scaledif=2

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from control as avg values here

time2=$(awk '{ print $8 }' runtime.$exp2.txt)
time1=$(awk '{ print $8 }' runtime.$exp1.txt)
mem=$(awk '{ print $8 }' memory.$exp2.txt)

timethresh=$((time2 / timedif + time2))
memthresh=$((mem / memdiff + mem))

# Fill time variables with scalability data

time_scale1=$(awk '{ print $8 }' runtime.$exp1_scale.txt)
time_scale2=$(awk '{ print $8 }' runtime.$exp2_scale.txt)

timethresh2=$((time_scale2 / timedif + time_scale2))

# Now, figure out difference in time between two runs

scale1=$((time1 - time_scale1))
scale2=$((time2 - time_scale2))

# Calculate maximum allowable deviation for scalability

scale1thresh=$((scale1 / scaledif + scale1))

# Begin applying threshold tests
# First, wall time (both maximum allowable time and max/min allowable deviation)

{

# This part is for the maximum allowable time (operationally)

  if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $maxtime ]]; then
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable operational time of '$maxtime' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the maximum allowable operational time of '$maxtime' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# This part is for deviation of wall time for timethresh

{

  if [[ $(awk '{ print $8 }' runtime.$exp1.txt) -gt $timethresh ]]; then
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds.  This has exceeded maximum allowable threshold time of '$timethresh' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1' is '$(awk '{ print $8 }' runtime.$exp1.txt)' seconds and is within the allowable threshold time of '$timethresh' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# This part is for deviation of wall time for timethresh2

{

  if [[ $(awk '{ print $8 }' runtime.$exp1_scale.txt) -gt $timethresh2 ]]; then
    echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds.  This has exceeded maximum allowable threshold time of '$timethresh2' seconds,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The runtime for '$exp1_scale' is '$(awk '{ print $8 }' runtime.$exp1_scale.txt)' seconds and is within the allowable threshold time of '$timethresh2' seconds,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, maximum residence set size (both harware limitation and percent difference)
# First, hardware limitation

{

  if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $maxmem ]]; then
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable hardware memory limit of '$maxmem' KBs,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable hardware memory limit of '$maxmem' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, maximum residence set size

{

  if [[ $(awk '{ print $8 }' memory.$exp1.txt) -gt $memthresh ]]; then
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs.  This has exceeded maximum allowable memory of '$memthresh' KBs,'
    echo 'resulting in failure of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable memory of '$memthresh' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, reproducibility between exp1 and exp2

{

if [[ $(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp2}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp2}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are nonreproducible,'
   echo 'thus the regression test has failed for '${exp1}' and '${exp2}' analyses with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp2}.txt)' lines different.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp2

{

if cmp -s siganl.${exp1} siganl.${exp2}
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

# Next, reproducibility between exp1 and exp3

{

if [[ $(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are nonreproducible,'
   echo 'thus the regression test has failed for '${exp1}' and '${exp3}' analyses with '$(grep -c 'penalty,grad ,a,b' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp3

{

if cmp -s siganl.${exp1} siganl.${exp3} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

# Finally, scalability

{

if [[ $scale1thresh -ge $scale2 ]]; then
   echo 'The case has passed the scalability regression test.'
   echo 'The slope for the update ('$scale1thresh' seconds per node) is greater than or equal to that for the control ('$scale2' seconds per node).'
else
   echo 'The case has failed the scalability test.'
   echo 'The slope for the update ('$scale1thresh' seconds per node) is less than that for the control ('$scale2' seconds per node).'
fi

} >> $output

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

cd $scripts
rm -f regression_test.gsi_rtma_updat.e*
rm -f regression_test.gsi_rtma_updat2.e*
rm -f regression_test.gsi_rtma_cntrl.e*
rm -f regression_test.gsi_rtma_cntrl2.e*
rm -f regression_test.rtma_regression.e*

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit

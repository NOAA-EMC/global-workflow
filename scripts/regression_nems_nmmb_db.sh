#!/bin/sh
#
#  test nems nmmb regional option for gsi
#
#@ job_name=regression_debug_test
#@ error= nems_nmmb_debug_test.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 4
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ class=dev
#@ group=dev
#@ account_no = RDAS-MTN
#@ wall_clock_limit = 1:00:00
#@ notification=error
#@ restart=no
#@ queue

. regression_var.sh

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
export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite


# Set analysis date
adate=$adate_regional_nems_nmmb

# Set experiment name
exp=$exp1_nems_nmmb_updat

# Set path/file for gsi executable
gsiexec=$updat

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
export LEVS=60
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${nems_nmmb}/${exp}
savdir=$ptmp_loc/outreg/${nems_nmmb}/${exp}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

## Given the analysis date, compute the date from which the
## first guess comes.  Extract cycle and set prefix and suffix
## for guess and observation data files
#sdate=`echo $adate |cut -c1-8`
#odate=`$ndate +12 $adate`
#hha=`echo $adate | cut -c9-10`
#hho=`echo $odate | cut -c9-10`
#prefixo=ndas.t${hho}z
#prefixa=ndas.t${hha}z
#suffix=tm12.bufr_d

datobs=$datobs_nems_nmmb
datges=$datobs

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
. $scripts/regression_namelists_db.sh
cat << EOF > gsiparm.anl

$nems_nmmb_namelist

EOF

anavinfo=$fix_file/anavinfo_nems_nmmb
berror=$fix_file/nam_glb_berror.f77.gcv
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
scaninfo=$fix_file/global_scaninfo.txt
satangl=$fix_file/nam_global_satangbias.txt
atmsbeamdat=$fix_file/atms_beamwidth.txt
pcpinfo=$fix_file/nam_global_pcpinfo.txt
ozinfo=$fix_file/nam_global_ozinfo.txt
errtable=$fix_file/nam_errtable.r3dv
convinfo=$fix_file/nam_regional_convinfo.txt
mesonetuselist=$fix_file/nam_mesonet_uselist.txt
stnuselist=$fix_file/nam_mesonet_stnuselist.txt

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $atmsbeamdat  ./atms_beamwidth.txt
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist
$ncp $stnuselist ./mesonet_stnuselist

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
$ncp $datobs/ndas.t12z.prepbufr.tm12      ./prepbufr
$ncp $datobs/ndas.t12z.satwnd.tm12.bufr_d ./satwnd
$ncp $datobs/ndas.t12z.1bhrs3.tm12.bufr_d ./hirs3bufr
$ncp $datobs/ndas.t12z.1bhrs4.tm12.bufr_d ./hirs4bufr
$ncp $datobs/ndas.t12z.1bamua.tm12.bufr_d ./amsuabufr
$ncp $datobs/ndas.t12z.1bamub.tm12.bufr_d ./amsubbufr
$ncp $datobs/ndas.t12z.1bmhs.tm12.bufr_d  ./mhsbufr
$ncp $datobs/ndas.t12z.goesfv.tm12.bufr_d ./gsnd1bufr
$ncp $datobs/ndas.t12z.airsev.tm12.bufr_d ./airsbufr
$ncp $datobs/ndas.t12z.radwnd.tm12.bufr_d ./radarbufr
$ncp $datobs/gdas1.t00z.osbuv8.tm00.bufr_d ./sbuvbufr

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datges/ndas.t06z.satang.tm09 ./satbias_angle
$ncp $datges/ndas.t06z.satbias.tm09 ./satbias_in

$ncp $datges/nmm_b_history_nemsio.012 wrf_inout
$ncp $datges/nmm_b_history_nemsio.012.ctl wrf_inout.ctl
$ncp $datges/gdas1.t00z.sgesprep  ./gfs_sigf03
$ncp wrf_inout wrf_ges
$ncp wrf_inout.ctl wrf_ges.ctl

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" = "0" ]]; then
   cd $regression_vfydir
   {
    echo
    echo 'nems_nmmb debug test has passed'
   } >> $nems_nmmb_regression
fi

exit

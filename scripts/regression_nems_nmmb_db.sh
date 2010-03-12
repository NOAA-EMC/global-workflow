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
#@ node_resources = ConsumableMemory(110GB)
#@ class=dev
#@ group=dev
#@ account_no = RDAS-MTN
#@ wall_clock_limit = 3:00:00
#@ notification=error
#@ restart=no
#@ queue

. regression_var.sh

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_PULSE=0
##export MP_BULK_MIN_MSG_SIZE=10k
##export MP_USE_BULK_XFER=yes

# Set environment variables for threads
export AIXTHREAD_GUARDPAGES=4
export AIXTHREAD_MUTEX_DEBUG=OFF
export AIXTHREAD_RWLOCK_DEBUG=OFF
export AIXTHREAD_COND_DEBUG=OFF
export AIXTHREAD_MNRATIO=1:1
export AIXTHREAD_SCOPE=S
export XLSMPOPTS="parthds=1:stack=128000000"
##export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

# Variables for debugging (don't always need)
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite

# Set analysis date
adate=$adate_regional_nems_nmmb

# Set experiment name
exp=$exp1_nems_nmmb_sub_2node

# Set path/file for gsi executable
gsiexec=$subversion

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
###suffix=tm00.bufr_d
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
. $scripts/regression_namelists_db.sh
cat << EOF > gsiparm.anl

$nems_nmmb_namelist

EOF

berror=$fix_file/nam_glb_berror.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
satangl=$fix_file/nam_global_satangbias.txt
pcpinfo=$fix_file/nam_global_pcpinfo.txt
ozinfo=$fix_file/nam_global_ozinfo.txt
errtable=$fix_file/nam_errtable.r3dv
convinfo=$fix_file/nam_regional_convinfo.txt
mesonetuselist=$fix_file/nam_mesonet_uselist.txt

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $berror   ./berror_stats
$ncp $emiscoef ./EmisCoeff.bin
$ncp $aercoef  ./AerosolCoeff.bin
$ncp $cldcoef  ./CloudCoeff.bin
$ncp $satangl  ./satbias_angle
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

# Copy CRTM coefficient files based on entries in satinfo file
nsatsen=`cat $satinfo | wc -l`
isatsen=1
while [[ $isatsen -le $nsatsen ]]; do
   flag=`head -n $isatsen $satinfo | tail -1 | cut -c1-1`
   if [[ "$flag" != "!" ]]; then
      satsen=`head -n $isatsen $satinfo | tail -1 | cut -f 2 -d" "`
      spccoeff=${satsen}.SpcCoeff.bin
      if  [[ ! -s $spccoeff ]]; then
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/ndas.t12z.prepbufr.tm12      ./prepbufr
$ncp $datobs/ndas.t12z.1bhrs3.tm12.bufr_d ./hirs3bufr
$ncp $datobs/ndas.t12z.1bhrs4.tm12.bufr_d ./hirs4bufr
$ncp $datobs/ndas.t12z.1bamua.tm12.bufr_d ./amsuabufr
$ncp $datobs/ndas.t12z.1bamub.tm12.bufr_d ./amsubbufr
$ncp $datobs/ndas.t12z.1bmhs.tm12.bufr_d  ./mhsbufr
$ncp $datobs/ndas.t12z.goesfv.tm12.bufr_d ./gsnd1bufr
$ncp $datobs/ndas.t12z.airsev.tm12.bufr_d ./airsbufr
$ncp $datobs/ndas.t12z.radwnd.tm12.bufr_d ./radarbufr

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datges/ndas.t06z.satang.tm09 ./satbias_angle
$ncp $datges/ndas.t06z.satbias.tm09 ./satbias_in

$ncp $datges/nmm_b_history_nemsio.012 wrf_inout
$ncp $datges/nmm_b_history_nemsio.012.ctl wrf_inout.ctl
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

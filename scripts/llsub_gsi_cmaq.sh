#!/bin/sh

#@ job_type=parallel
#@ class=dev
#@ group=dev
#@ account_no = RDAS-MTN

#@ job_name=cmaq
#@ error=$(job_name).e$(jobid)
#@ network.MPI=sn_all,shared,us
#@ total_tasks=16
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ resources = ConsumableMemory (2 GB)
#@ wall_clock_limit = 0:10:00
#@ notification=error
#@ restart=no
#@ queue

. regression_var.sh

exp=cmaq_test.$gps_dtype
hour=12

tmpdir=$ptmp_loc/tmpreg_${cmaq}/${exp}
savdir=$ptmp_loc/out/${cmaq}/${exp}

datobs=$datobs_cmaq_binary
datges=$datobs

cmaq_ges=${datges}/cmaq2gsi_20100901_${hour}0000.bin
anowbufr=${datobs}/hourly.20100901/aqm.t${hour}z.anowpm.pb.tm216
anavinfo=${fix_file}/anavinfo_cmaq_binary
berror=$fix_file/$endianness/cmaq_pm2_5_reg_berror_${hour}z.bin

cmaq_input='cmaq_input.bin'
cmaq_output='cmaq_output.bin'
chem_increment='chem_increment.bin'

gsiexec=$updat

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
export MP_SHARED_MEMORY=yes

# Set environment variables for no threads
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

# Set analysis date
adate=$adate_regional_cmaq_binary

# Set guess/analysis (i/o) file format.  Two
# option are available:  binary or netcdf
io_format=binary
#io_format=netcdf

if [[ "$io_format" = "binary" ]]; then
   NETCDF=.false.
   FORMAT=binary
elif [[ "$io_format" = "netcdf" ]]; then
   NETCDF=.true.
   FORMAT=netcdf
else
   echo "***ERRROR*** INVALID io_format = $io_format"
   exit
fi

# Set path/file for gsi executable

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
if [[ "$io_format" = "binary" ]]; then
   export LEVS=45
elif [[ "$io_format" = "netcdf" ]]; then
   export LEVS=45
fi
export DELTIM=1200


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
sdate=`echo $adate |cut -c1-8`
odate=`$ndate +6 $adate`
hha=`echo $adate | cut -c9-10`
hho=`echo $odate | cut -c9-10`
prefixo=ndas.t${hho}z
prefixa=ndas.t${hha}z
suffix=tm06.bufr_d

rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Make gsi namelist

. $scripts/regression_namelists.sh

cat << EOF > gsiparm.anl

$cmaq_binary_namelist

EOF

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)


emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
satangl=$fix_file/nam_global_satangbias.txt
pcpinfo=$fix_file/nam_global_pcpinfo.txt
ozinfo=$fix_file/nam_global_ozinfo.txt
errtable=$fix_file/nam_errtable.r3dv
convinfo=$fix_file/nam_regional_convinfo.txt
mesonetuselist=$fix_file/nam_mesonet_uselist.txt

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

# Copy executable and fixed files to $tmpdir
$ncp $gsiexec ./gsi.x

$ncp $anavinfo ./anavinfo
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
$ncp $anowbufr ./anowbufr

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

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

$ncp $cmaq_ges ./${cmaq_input}

$ncp $datobs/${prefixo}.satbias.tm06      ./satbias_in
$ncp $datobs/${prefixo}.satang.tm06        ./satbias_angle
#$ncp $datobs/${prefixo}.prepbufr.tm06   ./prepbufr
#$ncp $datobs/${prefixo}.1bhrs3.$suffix  ./hirs3bufr
#$ncp $datobs/${prefixo}.1bhrs4.$suffix  ./hirs4bufr
#$ncp $datobs/${prefixo}.1bamua.$suffix  ./amsuabufr
#$ncp $datobs/${prefixo}.1bamub.$suffix  ./amsubbufr
#$ncp $datobs/${prefixo}.1bmhs.$suffix   ./mhsbufr
#$ncp $datobs/${prefixo}.goesfv.$suffix  ./gsnd1bufr
#$ncp $datobs/${prefixo}.airsev.$suffix  ./airsbufr
##$ncp $datobs/${prefixo}.radwnd.$suffix  ./radarbufr
#if [[ "$io_format" = "netcdf" ]]; then
#   $ncp $datobs/${prefixo}.nexrad.$suffix  ./l2rwbufr
#fi

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (cmaq_input.bin)
#


# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   {
    echo "'cmaq test' has failed to run to completion, with an error code of '$rc'"
   } >> ${scripts}/qslogs/error.txt
   exit
fi

if [ ! -r ${savdir} ]
    then
    mkdir -p $savdir
fi

$ncp $cmaq_output       $savdir

if [ -r ${chem_increment} ]
    then
    $ncp $chem_increment    $savdir
fi

exit


$ncp cmaq_output       $savdir/cmaq_output.${adate}
$ncp -rp stdout $savdir
$ncp -rp fort.220 $savdir
$ncp -rp siganl $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp cmaq_output       $savdir/cmaq_output.${adate}
$ncp satbias_out     $savdir/biascr.${adate}

# If desired, copy guess file to unique filename in $savdir
$ncp cmaq_input         $savdir/cmaq_ges.${adate}

exit

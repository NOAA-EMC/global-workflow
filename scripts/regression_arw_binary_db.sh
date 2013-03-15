#!/bin/sh

#@ job_name=regression_debug_test
#@ error=arw_binary_debug_test.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ class=dev
#@ group=dev
#@ account_no = RDAS-T2O
#@ wall_clock_limit = 0:30:00
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
adate=$adate_regional_arw_binary

# Set guess/analysis (i/o) file format.  Two
# option are available:  binary or netcdf
io_format=binary
##io_format=netcdf

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

# Set experiment name
exp=$exp1_arw_binary_updat

# Set path/file for gsi executable
gsiexec=$updat

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
if [[ "$io_format" = "binary" ]]; then
   export LEVS=50
elif [[ "$io_format" = "netcdf" ]]; then
   export LEVS=30
fi
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${arw_binary}/${exp}
savdir=$ptmp_loc/outreg/${arw_binary}/${exp}

# Specify GSI fixed field and data directories.


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
prefixo=ndas.t18z
prefixa=ndas.t18z
suffix=tm06.bufr_d

datobs=$datobs_arw_binary/$adate
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

$arw_binary_namelist

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
#   atmsbeamdat  =  data required for atms spatial averaging
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/anavinfo_arw_binary
if [[ "$io_format" = "binary" ]]; then
   berror=$fix_file/$endianness/nam_glb_berror.f77.gcv
elif [[ "$io_format" = "netcdf" ]]; then
   berror=$fix_file/$endianness/nam_glb_berror.f77.gcv
fi
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
$ncp $atmsbeamdat  ./atms_beamwidth.txt
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $crtm_coef/SpcCoeff/Big_Endian/${file}.SpcCoeff.bin ./
   $ncp $crtm_coef/TauCoeff/Big_Endian/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefixo}.prepbufr.tm06   ./prepbufr
$ncp $datobs/${prefixo}.satwnd.$suffix  ./satwnd
$ncp $datobs/${prefixo}.1bhrs3.$suffix  ./hirs3bufr
$ncp $datobs/${prefixo}.1bhrs4.$suffix  ./hirs4bufr
$ncp $datobs/${prefixo}.1bamua.$suffix  ./amsuabufr
$ncp $datobs/${prefixo}.1bamub.$suffix  ./amsubbufr
$ncp $datobs/${prefixo}.1bmhs.$suffix   ./mhsbufr
$ncp $datobs/${prefixo}.goesfv.$suffix  ./gsnd1bufr
$ncp $datobs/${prefixo}.airsev.$suffix  ./airsbufr
$ncp $datobs/${prefixo}.radwnd.$suffix  ./radarbufr
if [[ "$io_format" = "netcdf" ]]; then
   $ncp $datobs/${prefixo}.nexrad.$suffix  ./l2rwbufr
fi

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datobs/${prefixa}.satbias.tm06      ./satbias_in
$ncp $datobs/${prefixa}.satang.tm06        ./satbias_angle
if [[ "$io_format" = "binary" ]]; then
   $ncp $datges/wrfinput_d01_2010-07-24_12:00:00       ./wrf_inout
elif [[ "$io_format" = "netcdf" ]]; then
   $ncp $datges/wrfinput_d01_arw_netcdf       ./wrf_inout
fi
cp wrf_inout wrf_ges

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" = "0" ]]; then
   cd $regression_vfydir
   {
    echo
    echo 'arw_binary debug test has passed'
   } >> $arw_binary_regression
fi

exit

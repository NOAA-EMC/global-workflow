#!/bin/sh

#@ error=$(job_name).e$(jobid)
#@ job_type=parallel
#@ class=dev
#@ group=dev
#@ account_no = RDAS-MTN

#@ job_name=nmm_binary_gsi
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:15:00
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
##export XLFRTEOPTS="buffering=disable_all"
##export MP_COREFILE_FORMAT=lite


# Set analysis date
adate=$adate_regional_nmm_binary

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
exp=$exp1_nmm_binary_cntrl

# Set path/file for gsi executable
gsiexec=$cntrl

# Set resoltion and other dependent parameters
export JCAP=62
export LEVS=60
export JCAP_B=62
if [[ "$io_format" = "binary" ]]; then
   export LEVS=60
elif [[ "$io_format" = "netcdf" ]]; then
   export LEVS=45
fi
export DELTIM=1200

# Set runtime and save directories
tmpdir=$ptmp_loc/tmpreg_${nmm_binary}/${exp}
savdir=$ptmp_loc/outreg/${nmm_binary}/${exp}

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
odate=`$ndate +12 $adate`
hha=`echo $adate | cut -c9-10`
hho=`echo $odate | cut -c9-10`
prefixo=ndas.t${hho}z
prefixa=ndas.t${hha}z
suffix=tm12.bufr_d

datobs=$datobs_nmm_binary/$adate
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
. $scripts/regression_namelists.sh
cat << EOF > gsiparm.anl

$nmm_binary_namelist

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

anavinfo=$fix_file/anavinfo_ndas_binary
if [[ "$io_format" = "binary" ]]; then
   berror=$fix_file/$endianness/nam_nmmstat_na.gcv
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
convinfo=$fix_file/nam_regional_convinfo_reg_test.txt
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
$ncp $datobs/${prefixo}.prepbufr.tm12   ./prepbufr
$ncp $datobs/${prefixo}.satwnd.$suffix   ./satwndbufr
$ncp $datobs/${prefixo}.gpsro.$suffix   ./gpsrobufr
$ncp $datobs/${prefixo}.1bhrs3.$suffix  ./hirs3bufr
$ncp $datobs/${prefixo}.1bhrs4.$suffix  ./hirs4bufr
$ncp $datobs/${prefixo}.1bamua.$suffix  ./amsuabufr
$ncp $datobs/${prefixo}.1bamub.$suffix  ./amsubbufr
$ncp $datobs/${prefixo}.1bmhs.$suffix   ./mhsbufr
$ncp $datobs/${prefixo}.goesfv.$suffix  ./gsnd1bufr
$ncp $datobs/${prefixo}.airsev.$suffix  ./airsbufr
$ncp $datobs/${prefixo}.radwnd.$suffix  ./radarbufr
$ncp $datobs/${prefixo}.nexrad.$suffix  ./l2rwbufr

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $datobs/${prefixo}.satbias.tm12      ./satbias_in
$ncp $datobs/${prefixo}.satang.tm12       ./satbias_angle
if [[ "$io_format" = "binary" ]]; then
   $ncp $datges/${prefixo}.wrfinput_d01.init  ./wrf_inout
elif [[ "$io_format" = "netcdf" ]]; then
   $ncp $datges/wrfinput_d01_nmm_netcdf       ./wrf_inout
fi
cp wrf_inout wrf_ges

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_nmm_binary_updat' has failed to run to completion, with an error code of '$rc''
   } >> $nmm_binary_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp wrf_inout       $savdir/wrfanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${adate}

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
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g10 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

exit

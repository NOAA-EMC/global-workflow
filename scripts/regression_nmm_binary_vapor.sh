#!/bin/sh

#@ error=$(job_name).$(step_name).e$(jobid)
#@ job_type=parallel
#@ class=jcsda
#@ group=jcsda
#@ account_no = JCSDA008-RES

#@ job_name=regression_test
#@ step_name=gsi_nmm_binary_update
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources = ConsumableCpus(1) ConsumableMemory(3000 MB)
#@ wall_clock_limit = 0:15:00
#@ notification=error
#@ restart=no
#@ queue

#@ step_name=gsi_nmm_binary_update2
#@ network.MPI=sn_all,shared,us
#@ node = 4
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ node_resources = ConsumableCpus(2) ConsumableMemory(3000 MB)
#@ wall_clock_limit = 0:15:00
#@ notification=error
#@ restart=no
#@ dependency=(gsi_nmm_binary_update==0)
#@ queue

#@ step_name=gsi_nmm_binary_benchmark
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ node_resources = ConsumableCpus(1) ConsumableMemory(3000 MB)
#@ wall_clock_limit = 0:15:00
#@ notification=error
#@ restart=no
#@ dependency=(gsi_nmm_binary_update2==0)
#@ queue

#@ step_name=gsi_nmm_binary_benchmark2
#@ network.MPI=sn_all,shared,us
#@ node = 4
#@ node_usage=not_shared
#@ tasks_per_node=16
#@ node_resources = ConsumableCpus(2) ConsumableMemory(3000 MB)
#@ wall_clock_limit = 0:15:00
#@ notification=error
#@ restart=no
#@ dependency=(gsi_nmm_binary_benchmark==0)
#@ queue

#@ step_name=nmm_binary_regression
#@ job_type=serial
#@ node_resources = ConsumableCpus(1) ConsumableMemory(2000 MB)
#@ class = 1
#@ node_usage = shared
#@ wall_clock_limit = 00:10:00
#@ notification=error
#@ restart=no
#@ dependency=(gsi_nmm_binary_benchmark2==0)
#@ queue

. ./regression_var.sh

case $LOADL_STEP_NAME in
  gsi_nmm_binary_update)

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
exp=$exp1_nmm_binary_sub_2node

# Set path/file for gsi executable
gsiexec=$subversion

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

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix

##fixgsi=/nwprod/fix

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
##suffix=tm00.bufr_d
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
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/anavinfo_ndas_binary
if [[ "$io_format" = "binary" ]]; then
## berror=$fix_file/nam_nmmstat_na
   berror=$fix_file/nam_nmmstat_na.gcv
elif [[ "$io_format" = "netcdf" ]]; then
## berror=$fix_file/nam_glb_berror.f77
   berror=$fix_file/nam_glb_berror.f77.gcv
fi
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
satangl=$fix_file/nam_global_satangbias.txt
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
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

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
$ncp $datobs/${prefixo}.prepbufr.tm12   ./prepbufr
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
$ncp $datobs/${prefixo}.satang.tm12        ./satbias_angle
##$ncp $datges/${prefixa}.wrfrst_d01.tm12    ./wrf_inout
if [[ "$io_format" = "binary" ]]; then
   $ncp $datges/${prefixo}.wrfinput_d01.init    ./wrf_inout
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
    echo ''$exp1_nmm_binary_sub_2node' has failed to run to completion, with an error code of '$rc''
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

exit ;;

  gsi_nmm_binary_update2)

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
##export XLSMPOPTS="parthds=1:stack=128000000"
export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

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
exp=$exp2_nmm_binary_sub_3node

# Set path/file for gsi executable
gsiexec=$subversion

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

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix

##fixgsi=/nwprod/fix

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
##suffix=tm00.bufr_d
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
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/anavinfo_ndas_binary
if [[ "$io_format" = "binary" ]]; then
## berror=$fix_file/nam_nmmstat_na
   berror=$fix_file/nam_nmmstat_na.gcv
elif [[ "$io_format" = "netcdf" ]]; then
## berror=$fix_file/nam_glb_berror.f77
   berror=$fix_file/nam_glb_berror.f77.gcv
fi
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
satangl=$fix_file/nam_global_satangbias.txt
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
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

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
$ncp $datobs/${prefixo}.prepbufr.tm12   ./prepbufr
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
$ncp $datobs/${prefixo}.satang.tm12        ./satbias_angle
##$ncp $datges/${prefixa}.wrfrst_d01.tm12    ./wrf_inout
if [[ "$io_format" = "binary" ]]; then
   $ncp $datges/${prefixo}.wrfinput_d01.init    ./wrf_inout
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
    echo ''$exp2_nmm_binary_sub_3node' has failed to run to completion, with an error code of '$rc''
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

exit ;;

  gsi_nmm_binary_benchmark)

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
export PARALLEL_threads=1

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

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
exp=$exp1_nmm_binary_bench_2node

# Set path/file for gsi executable
gsiexec=$benchmark
##gsiexec=/global/save/wx20ml/q1fy10_new/global_gsi

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

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix

##fixgsi=/nwprod/fix

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
##suffix=tm00.bufr_d
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
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/anavinfo_ndas_binary
if [[ "$io_format" = "binary" ]]; then
## berror=$fix_file/nam_nmmstat_na
   berror=$fix_file/nam_nmmstat_na.gcv
elif [[ "$io_format" = "netcdf" ]]; then
## berror=$fix_file/nam_glb_berror.f77
   berror=$fix_file/nam_glb_berror.f77.gcv
fi
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
satangl=$fix_file/nam_global_satangbias.txt
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
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

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
$ncp $datobs/${prefixo}.prepbufr.tm12   ./prepbufr
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
$ncp $datobs/${prefixo}.satang.tm12        ./satbias_angle
##$ncp $datges/${prefixa}.wrfrst_d01.tm12    ./wrf_inout
if [[ "$io_format" = "binary" ]]; then
   $ncp $datges/${prefixo}.wrfinput_d01.init    ./wrf_inout
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
    echo ''$exp1_nmm_binary_bench_2node' has failed to run to completion, with an error code of '$rc''
   } >> $nmm_binary_regression
   $step_name==$rc
   exit
fi

mkdir $noscrub/tmpreg_${nmm_binary}
mkdir $control_nmm_binary
cp -rp stdout $control_nmm_binary
cp -rp fort.220 $control_nmm_binary
cp -rp wrf_inout $control_nmm_binary

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

exit ;;

  gsi_nmm_binary_benchmark2)

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
##export XLSMPOPTS="parthds=1:stack=128000000"
export XLSMPOPTS="parthds=2:stack=128000000"

# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

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
exp=$exp2_nmm_binary_bench_3node

# Set path/file for gsi executable
gsiexec=$benchmark

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

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix

##fixgsi=/nwprod/fix

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
##suffix=tm00.bufr_d
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
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/anavinfo_ndas_binary
if [[ "$io_format" = "binary" ]]; then
## berror=$fix_file/nam_nmmstat_na
   berror=$fix_file/nam_nmmstat_na.gcv
elif [[ "$io_format" = "netcdf" ]]; then
## berror=$fix_file/nam_glb_berror.f77
   berror=$fix_file/nam_glb_berror.f77.gcv
fi
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/nam_regional_satinfo.txt
satangl=$fix_file/nam_global_satangbias.txt
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
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $mesonetuselist ./mesonetuselist

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
$ncp $datobs/${prefixo}.prepbufr.tm12   ./prepbufr
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
$ncp $datobs/${prefixo}.satang.tm12        ./satbias_angle
##$ncp $datges/${prefixa}.wrfrst_d01.tm12    ./wrf_inout
if [[ "$io_format" = "binary" ]]; then
   $ncp $datges/${prefixo}.wrfinput_d01.init    ./wrf_inout
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
    echo ''$exp2_nmm_binary_bench_3node' has failed to run to completion, with an error code of '$rc''
   } >> $nmm_binary_regression
   $step_name==$rc
   exit
fi

mkdir $noscrub/tmpreg_${nmm_binary}
mkdir $control_nmm_binary2
cp -rp stdout $control_nmm_binary2
cp -rp fort.220 $control_nmm_binary2
cp -rp wrf_inout $control_nmm_binary2

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

exit ;;

  nmm_binary_regression)

set -ax

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the benchmark run

exp1=$exp1_nmm_binary_sub_2node
exp2=$exp1_nmm_binary_bench_2node
exp3=$exp2_nmm_binary_sub_3node

# Choose global, regional, or RTMA
input=tmpreg_${nmm_binary}

# Name output file
output=$nmm_binary_regression

# Give location of analysis results, and choose location for regression output
savdir=$ptmp_loc/$input
vfydir=$regression_vfydir

ncp=/bin/cp

# Name and create temporary directory
tmpdir=$ptmp_loc/$compare/$input/${exp1}_vs_${exp2}
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
   $ncp $savdir/$exp/wrf_inout ./wrf_inout.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3"
for exp in $list; do
   grep 'a,b' fort.220.$exp > penalty.$exp.txt
   grep 'The total amount of wall time' stdout.$exp > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.1node.txt with penalty.10node.txt)
diff penalty.$exp1.txt penalty.$exp2.txt > penalty.${exp1}-${exp2}.txt
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt

# Give location of additional output files for scalability testing
# (i.e., output from increased number of nodes)

exp1_scale=$exp2_nmm_binary_sub_3node
exp2_scale=$exp2_nmm_binary_bench_3node

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

timedif=8
memdiff=10
scaledif=8

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from benchmark as avg values here

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

# This part is for deviation of wall time for 1 node

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

# This part is for deviation of wall time for 2 node

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

# Next, maximum residence set size (both hardware limitation and percent difference)
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

# Next, reproducibility between a 1 node and 1 node experiment

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

# Next, check reproducibility of results between a 1 node branch and 1 node trunk experiment

{

if cmp -s wrf_inout.${exp1} wrf_inout.${exp2} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

# Finally, scalability

{

if [[ $scale1thresh -ge $scale2 ]]; then
   echo 'The case has passed the scalability regression test.'
   echo 'The slope for the branch ('$scale1thresh' seconds per node) is greater than or equal to that for the benchmark ('$scale2' seconds per node).'
else
   echo 'The case has failed the scalability test.'
   echo 'The slope for the branch ('$scale1thresh' seconds per node) is less than that for the benchmark ('$scale2' seconds per node).'
fi

} >> $output

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

cd $scripts
rm -f regression_test.gsi_nmm_binary_update.e*
rm -f regression_test.gsi_nmm_binary_update2.e*
rm -f regression_test.gsi_nmm_binary_benchmark.e*
rm -f regression_test.gsi_nmm_binary_benchmark2.e*
rm -f regression_test.nmm_binary_regression.e*

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit

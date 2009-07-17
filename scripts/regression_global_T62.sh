#!/bin/sh

#@ job_name=regression_test
#@ step_name=gsi_global_update
#@ error=gsi_global_update.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:20:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ queue

#@ step_name=gsi_global_update2
#@ error=gsi_global_update2.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:20:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_update==0)
#@ queue

#@ step_name=gsi_global_benchmark
#@ error=gsi_global_benchmark.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:20:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_update2==0)
#@ queue

#@ step_name=gsi_global_benchmark2
#@ error=gsi_global_benchmark2.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 2
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class= dev
#@ group= dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 0:20:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_benchmark==0)
#@ queue

#@ step_name=global_regression
#@ error=global_regression.e$(jobid)
#@ job_type=serial
#@ resources = consumablecpus(1) consumablememory(2000 MB)
#@ class= dev
#@ group= dev
#@ wall_clock_limit = 00:10:00
#@ account_no = GDAS-MTN
#@ notification=error
#@ dependency=(gsi_global_benchmark2==0)
#@ queue

. regression_var.sh

case $LOADL_STEP_NAME in
  gsi_global_update)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
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

# Set experiment name and analysis date
adate=$adate_global
exp=$exp1_global_sub_1node
##exp=gmao_gsi7.t62.subversion.1node

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
. $scripts/regression_namelists.sh

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl

$global_T62_namelist

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
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

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
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_global_sub_1node' has failed to run to completion, with an error code of '$rc''
   } >> $global_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}
$ncp sfcf06          $savdir/sfcf06.${gdate}
$ncp sigf06          $savdir/sigf06.${gdate}

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

echo "Time before diagnostic loop is `date` "
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "

exit ;;

  gsi_global_update2)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
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

# Set experiment name and analysis date
adate=$adate_global
exp=$exp2_global_sub_2node
##exp=gmao_gsi7.t62.subversion.2node

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
. $scripts/regression_namelists.sh

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl

$global_T62_namelist

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
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

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
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp2_global_sub_2node' has failed to run to completion, with an error code of '$rc''
   } >> $global_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}
$ncp sfcf06          $savdir/sfcf06.${gdate}
$ncp sigf06          $savdir/sigf06.${gdate}

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

echo "Time before diagnostic loop is `date` "
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "

exit ;;

  gsi_global_benchmark)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
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

# Set experiment name and analysis date
adate=$adate_global
exp=$exp1_global_bench_1node
##exp=q1fy10.t62.2node.orig_mpi_io

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$benchmark
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/versions/global_gsi.fd.old_mpio.May07_0819/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/global_gsi.fd/global_gsi
##gsiexec=/global/save/wx20ml/q1fy10_new/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
. $scripts/regression_namelists.sh

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl

$global_T62_namelist

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
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

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
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_global_bench_1node' has failed to run to completion, with an error code of '$rc''
   } >> $global_regression
   $step_name==$rc
   exit
fi

mkdir $noscrub/tmp${global}
mkdir $control_global_T62
cp -rp stdout $control_global_T62
cp -rp fort.220 $control_global_T62
cp -rp siganl $control_global_T62

# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}
$ncp sfcf06          $savdir/sfcf06.${gdate}
$ncp sigf06          $savdir/sigf06.${gdate}

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

echo "Time before diagnostic loop is `date` "
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "

exit ;;

  gsi_global_benchmark2)

set -x

# Set environment variables for NCEP IBM
export MEMORY_AFFINITY=MCM
##export BIND_TASKS=yes
export MP_PULSE=0
export MP_SHARED_MEMORY=yes
export MP_BULK_MIN_MSG_SIZE=10k
export MP_USE_BULK_XFER=yes

# Set environment variables for no threads
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

# Set experiment name and analysis date
adate=$adate_global
exp=$exp2_global_bench_2node
##exp=q1fy10.t62.2node.modified_mpi_io

# Set path/file for gsi executable
basedir=/global/save
gsipath=$basedir/wx20rt/gsi_anl
gsiexec=$benchmark
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/versions/global_gsi.fd.old_mpio.May07_0819/global_gsi
##gsiexec=/global/save/wx20rt/2jif/Q1FY10_DA/sorc/global_gsi.fd/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix
##fixcrtm=/global/save/wx20rt/2jif/Q1FY10_DA/fix/crtm_gfsgsi
##fixjif09=/global/save/wx20rt/2jif/Q1FY09_DA/fix
##fixjif10=/global/save/wx20rt/2jif/Q1FY10_DA/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "62" ]]; then
   export LONA=192
   export LATA=94
   export DELTIM=1200
   export resol=2
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLAT=$((${LATA}+2))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`
dumpobs=gdas
dumpges=gdas
datobs=$datobs_global/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist
SETUP=""
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
. $scripts/regression_namelists.sh

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl

$global_T62_namelist

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
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$fix_file/crtm_gfsgsi/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$fix_file/crtm_gfsgsi/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$fix_file/crtm_gfsgsi/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo.txt
errtable=$fix_file/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fix_file/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fix_file/bufrtab.012

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
         $ncp $fix_file/crtm_gfsgsi/SpcCoeff/Big_Endian/$spccoeff ./
         $ncp $fix_file/crtm_gfsgsi/TauCoeff/Big_Endian/${satsen}.TauCoeff.bin ./
      fi
   fi
   isatsen=` expr $isatsen + 1 `
done

# Copy observational data to $tmpdir
$ncp $datobs/${prefix_obs}.prepbufr           ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}    ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}   ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}   ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}   ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}   ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}   ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}   ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}   ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}   ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}   ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}    ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}    ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}   ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}   ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}    ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}    ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}    ./ssmisbufr

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias              ./satbias_in
$ncp $datges/${prefix_tbc}.satang             ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03               ./sfcf03
$ncp $datges/${prefix_sfc}.bf06               ./sfcf06
$ncp $datges/${prefix_sfc}.bf09               ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep           ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep           ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep           ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp2_global_bench_2node' has failed to run to completion, with an error code of '$rc''
   } >> $global_regression
   $step_name==$rc
   exit
fi

mkdir $noscrub/tmp${global}
mkdir $control_global_T622
cp -rp stdout $control_global_T622
cp -rp fort.220 $control_global_T622
cp -rp siganl $control_global_T622

# Save output
mkdir -p $savdir

cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}
$ncp sfcf06          $savdir/sfcf06.${gdate}
$ncp sigf06          $savdir/sigf06.${gdate}

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

echo "Time before diagnostic loop is `date` "
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "

exit ;;

  global_regression)

set -ax

JCAP=62

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the benchmark run

exp1=$exp1_global_sub_1node
exp2=$exp1_global_bench_1node
exp3=$exp2_global_sub_2node

# Choose global, regional, or RTMA
input=tmp${global}

# Name output file
output=$global_regression

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
   $ncp $savdir/$exp/siganl ./siganl.$exp
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

exp1_scale=$exp2_global_sub_2node
exp2_scale=$exp2_global_bench_2node

# Copy stdout for additional scalability testing
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   $ncp $savdir/$exp_scale/stdout ./stdout.$exp_scale
done

# Grep out run time from stdout file
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   grep 'The total amount of wall time' stdout.$exp_scale > runtime.$exp_scale.txt
done

# Important values used to calculate timethresh and memthresh below
# Values below can be fine tuned to make the regression more or less aggressive
# Currently using a value of 10%

timedif=10
memdiff=8
scaledif=4

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

# Now, figure out difference in time between two runs

scale1=$((time1 - time_scale1))
scale2=$((time2 - time_scale2))

# Calculate maximum allowable deviation for scalability

timethresh2=$((time_scale2 / timedif + time_scale2))

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

if cmp -s siganl.${exp1} siganl.${exp2} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp2}') are reproducible'
   echo 'since the corresponding results are identical.'
   echo
fi

} >> $output

# Next, reproducibility between a 1 node and 2 node experiment

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

# Next, check reproducibility of results between a 1 node branch and 2 node trunk experiment

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

if [[ $scale1 -ge $scale2 ]]; then
   echo 'The case has passed the scalability regression test.'
   echo 'The slope for the branch ('$scale1' seconds per node) is greater than or equal to that for the benchmark ('$scale2' seconds per node).'
else
   echo 'The case has failed the scalability test.'
   echo 'The slope for the branch ('$scale1' seconds per node) is less than that for the benchmark ('$scale2' seconds per node).'
fi

} >> $output

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

cd $scripts
rm -f gsi_global_update.e*
rm -f gsi_global_update2.e*
rm -f gsi_global_benchmark.e*
rm -f gsi_global_benchmark2.e*
rm -f global_regression.e*

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit

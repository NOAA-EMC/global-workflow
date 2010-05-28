#!/bin/sh

#@ job_name=regression_debug_test
#@ error=global_lanczos_debug_test.e$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node = 4
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity=core(1)
#@ node_resources=ConsumableMemory(110 GB)
#@ class=dev
#@ group=dev
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 3:00:00
#@ startdate = 09/27/06 05:00
#@ restart=no
#@ notification=error
#@ queue

. regression_var.sh

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
exp=$exp1_global_lanczos_sub_1node
##exp=gmao_gsi7.t62.subversion.1node

# Set path/file for gsi executable
gsiexec=$subversion
##gsiexec=/global/save/wx20ml/regional/src/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global_lanczos}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}
##tmpdir=/ptmp/wx20ml/tmp${JCAP}_sigmap/${exp}
##savdir=/ptmp/wx20ml/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.
##fixgsi=/nwprod/fix

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
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
datobs=$datobs_global_lanczos/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
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
GRIDOPTS=""
BKGVERR=""
ANBKGERR=""
JCOPTS=""
STRONGOPTS=""
OBSQC=""
OBSINPUT=""
SUPERRAD=""
SINGLEOB=""
. $scripts/regression_namelists_db.sh

##!   l4dvar=.false.,nhr_assimilation=6,nhr_obsbin=6,
##!   lsqrtb=.true.,lcongrad=.false.,ltlint=.true.,
##!   idmodel=.true.,lwrtinc=.false.,

cat << EOF > gsiparm.anl

$global_lanczos_T62_namelist

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

anavinfo=$fix_file/anavinfo_62_sigmap
#berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77
berror=$fix_file/global_berror.l${LEVS}y${NLAT}.f77.gcv
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo_reg_test.txt
satangl=$fix_file/global_satangbias.txt
pcpinfo=$fix_file/global_pcpinfo.txt
ozinfo=$fix_file/global_ozinfo.txt
convinfo=$fix_file/global_convinfo_reg_test.txt
errtable=$fix_file/prepobs_errtable.global

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
$ncp $datobs/${prefix_obs}.prepbufr                ./prepbufr
$ncp $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$ncp $datobs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
$ncp $datobs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$ncp $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$ncp $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$ncp $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
$ncp $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$ncp $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$ncp $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}         ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}         ./ssmisbufr
$ncp $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncp $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncp $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncp $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncp $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncp $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias                   ./satbias_in
$ncp $datges/${prefix_tbc}.satang                  ./satbias_angle

$ncp $datges/${prefix_sfc}.bf03                    ./sfcf03
$ncp $datges/${prefix_sfc}.bf06                    ./sfcf06
$ncp $datges/${prefix_sfc}.bf09                    ./sfcf09

$ncp $datobs/${prefix_atm}.sgm3prep                ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep                ./sigf06
$ncp $datobs/${prefix_atm}.sgp3prep                ./sigf09

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl > stdout
rc=$?

if [[ "$rc" = "0" ]]; then
   cd $regression_vfydir
   {
    echo
    echo 'global lanczos debug test has passed'
   } >> $global_lanczos_regression
fi

exit

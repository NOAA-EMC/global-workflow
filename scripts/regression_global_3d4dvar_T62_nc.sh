#!/bin/sh

#@ error=$(job_name).$(step_name).e$(jobid)
#@ job_type=parallel
#@ class=dev
#@ group=dev
#@ account_no = GDAS-T2O

#@ job_name=regression_test
#@ step_name=gsi_global_3d4dvar_updat
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:10:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ restart=no
#@ queue

#@ step_name=gsi_global_3d4dvar_updat2
#@ network.MPI=sn_all,shared,us
#@ node = 1
#@ node_usage=not_shared
#@ tasks_per_node=32
#@ task_affinity = core(1)
#@ parallel_threads = 1
#@ node_resources = ConsumableMemory (110 GB)
#@ wall_clock_limit = 0:10:00
#@ startdate = 09/27/06 05:00
#@ notification=error
#@ dependency=(gsi_global_3d4dvar_updat==0)
#@ restart=no
#@ queue

#@ step_name=global_3d4dvar_regression
#@ job_type=serial
#@ task_affinity = cpu(1)
#@ node_usage = shared
#@ node_resources = ConsumableMemory(2000 MB)
#@ wall_clock_limit = 0:10:00
#@ notification=error
#@ dependency=(gsi_global_3d4dvar_updat2==0)
#@ restart=no
#@ queue

. regression_var.sh

case $LOADL_STEP_NAME in
  gsi_global_3d4dvar_updat)

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
adate=$adate_global
exp=$exp1_global_3d4dvar_updat

# Set path/file for gsi executable
gsiexec=$updat

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global_3d4dvar}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.


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
datobs=$datobs_global_3d4dvar/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-2}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./global_co2_data.txt
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                $ncp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
#CH4 file decision
ICH4=${ICH4:-2}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        ch4dir=${CH4DIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                $ncp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
   fi
fi
IN2O=${IN2O:-2}
if [ $IN2O -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        n2odir=${N2ODIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                $ncp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
   fi
fi
ICO=${ICO:-2}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $tmpdir
        codir=${CODIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                $ncp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
                exit 1
   fi
fi

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

# Set variables for requested minimization (pcgsoi or lanczos)
JCOPTS="ljcpdry=.false.,"
OBSQC="noiqc=.false.,"
SETUPmin="miter=1,niter(1)=50,niter_no_qc(1)=500,"
SETUPlan=""
export minimization=${minimization:-"pcgsoi"}
if [ "$minimization" = "lanczos" ]; then
   SETUPlan="lsqrtb=.true.,lcongrad=.true.,ltlint=.true.,ladtest=.true.,lgrtest=.false.,"
   HYBENS_GLOBAL=".false."
fi

# Create namelist for 3dvar run
SETUPobs=""
SETUP="$SETUPmin $SETUPlan $SETUPobs"
. $scripts/regression_namelists.sh
rm gsiparm.anl.3dvar
cat << EOF > gsiparm.anl.3dvar

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
#   atmsbeamdat  =  data required for atms spatial averaging
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/global_anavinfo.l64.txt
berror=$fix_file/$endianness/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo_reg_test.txt
scaninfo=$fix_file/global_scaninfo.txt
satangl=$fix_file/global_satangbias.txt
atmsbeamdat=$fix_file/atms_beamwidth.txt
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
$ncp $atmsbeamdat  ./atms_beamwidth.txt
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Adjust data usage flags in convinfo file.
rm new
cp convinfo old
mv convinfo convinfo_original
sed 's/sst      180    0   -1     3.0/sst      180    0    1     3.0/' < old > new
mv new old
sed 's/uv       243   56    1     3.0/uv       243   56   -1     3.0/' < old > new
mv new old
sed 's/uv       253   56    1     3.0/uv       253   56   -1     3.0/' < old > new
mv new convinfo

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
$ncp $datobs/${prefix_obs}.satwnd.${suffix}         ./satwnd
$ncp $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
##$ncp $datobs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
##$ncp $datobs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
##$ncp $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
##$ncp $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
##$ncp $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
##$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
##$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
##$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
##$ncp $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
##$ncp $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
##$ncp $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
##$ncp $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
##$ncp $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
##$ncp $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
##$ncp $datobs/${prefix_obs}.amsre.${suffix}         ./amsrebufr
##$ncp $datobs/${prefix_obs}.ssmis.${suffix}         ./ssmisbufr
##$ncp $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
##$ncp $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
##$ncp $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
##$ncp $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
##$ncp $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncp $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias                   ./satbias_in
$ncp $datges/${prefix_tbc}.satang                  ./satbias_angle

##$ncp $datges/${prefix_sfc}.bf03                    ./sfcf03
$ncp $datges/${prefix_sfc}.bf06                    ./sfcf06
##$ncp $datges/${prefix_sfc}.bf09                    ./sfcf09

##$ncp $datobs/${prefix_atm}.sgm3prep                ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep                ./sigf06
##$ncp $datobs/${prefix_atm}.sgp3prep                ./sigf09

# Run gsi 3dvar under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl.3dvar > stdout
rc=$?
if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_global_3d4dvar_updat' has FAILED to run 3dvar to completion, with an error code of '$rc''
   } >> $global_3d4dvar_regression
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
loops="01 03"
for loop in $loops; do
case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10"
   for type in $listall; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

exit ;;

  gsi_global_3d4dvar_updat2)

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
adate=$adate_global
exp=$exp2_global_3d4dvar_updat

# Set path/file for gsi executable
gsiexec=$updat

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global_3d4dvar}/${exp}
savdir=$ptmp_loc/out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.


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
datobs=$datobs_global_3d4dvar/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

# CO2 namelist and file decisions
ICO2=${ICO2:-2}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./global_co2_data.txt
        co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        while [ ! -s $co2 ] ; do
                ((yyyy-=1))
                co2=$co2dir/global_co2.gcmscl_$yyyy.txt
        done
        if [ -s $co2 ] ; then
                $ncp $co2 ./global_co2_data.txt
        fi
        if [ ! -s ./global_co2_data.txt ] ; then
                echo "\./global_co2_data.txt" not created
                exit 1
   fi
fi
#CH4 file decision
ICH4=${ICH4:-2}
if [ $ICH4 -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        ch4dir=${CH4DIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./ch4globaldata.txt
        ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        while [ ! -s $ch4 ] ; do
                ((yyyy-=1))
                ch4=$ch4dir/global_ch4_esrlctm_$yyyy.txt
        done
        if [ -s $ch4 ] ; then
                $ncp $ch4 ./ch4globaldata.txt
        fi
        if [ ! -s ./ch4globaldata.txt ] ; then
                echo "\./ch4globaldata.txt" not created
                exit 1
   fi
fi
IN2O=${IN2O:-2}
if [ $IN2O -gt 0 ] ; then
#        # Copy ch4 files to $tmpdir
        n2odir=${N2ODIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./n2oglobaldata.txt
        n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        while [ ! -s $n2o ] ; do
                ((yyyy-=1))
                n2o=$n2odir/global_n2o_esrlctm_$yyyy.txt
        done
        if [ -s $n2o ] ; then
                $ncp $n2o ./n2oglobaldata.txt
        fi
        if [ ! -s ./n2oglobaldata.txt ] ; then
                echo "\./n2oglobaldata.txt" not created
                exit 1
   fi
fi
ICO=${ICO:-2}
if [ $ICO -gt 0 ] ; then
#        # Copy CO files to $tmpdir
        codir=${CODIR:-$fix_file}
        yyyy=$(echo ${CDATE:-$adate}|cut -c1-4)
        rm ./coglobaldata.txt
        co=$codir/global_co_esrlctm_$yyyy.txt
        while [ ! -s $co ] ; do
                ((yyyy-=1))
                co=$codir/global_co_esrlctm_$yyyy.txt
        done
        if [ -s $co ] ; then
                $ncp $co ./coglobaldata.txt
        fi
        if [ ! -s ./coglobaldata.txt ] ; then
                echo "\./coglobaldata.txt" not created
                exit 1
   fi
fi

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

# Set variables for requested minimization (pcgsoi or lanczos)
JCOPTS="ljcpdry=.false.,"
OBSQC="noiqc=.false.,"
SETUPmin="miter=1,niter(1)=50,niter_no_qc(1)=500,"
SETUPlan=""
export minimization=${minimization:-"pcgsoi"}
if [ "$minimization" = "lanczos" ]; then
   SETUPlan="lsqrtb=.true.,lcongrad=.true.,ltlint=.true.,ladtest=.true.,lgrtest=.false.,"
   HYBENS_GLOBAL=".false."
fi

# Create namelist for observer run
export nhr_obsbin=${nhr_obsbin:-1}
SETUPobs="l4dvar=.true.,jiterstart=1,lobserver=.true.,iwrtinc=1,nhr_assimilation=6,nhr_obsbin=$nhr_obsbin,"
SETUP="$SETUPmin $SETUPlan $SETUPobs"
. $scripts/regression_namelists.sh
rm gsiparm.anl.obsvr
cat << EOF > gsiparm.anl.obsvr

$global_T62_namelist

EOF

# Create namelist for identity model 4dvar run
SETUP4dv="l4dvar=.true.,jiterstart=1,nhr_assimilation=6,nhr_obsbin=$nhr_obsbin,idmodel=.true.,iwrtinc=1,lanczosave=.true.,"
SETUP="$SETUPmin $SETUPlan $SETUP4dv"
. $scripts/regression_namelists.sh
rm gsiparm.anl.4dvar
cat << EOF > gsiparm.anl.4dvar

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
#   atmsbeamdat  =  data required for atms spatial averaging
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fix_file/global_anavinfo.l64.txt
berror=$fix_file/$endianness/global_berror.l${LEVS}y${NLAT}.f77
emiscoef=$crtm_coef/EmisCoeff/Big_Endian/EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff/Big_Endian/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff/Big_Endian/CloudCoeff.bin
satinfo=$fix_file/global_satinfo_reg_test.txt
scaninfo=$fix_file/global_scaninfo.txt
satangl=$fix_file/global_satangbias.txt
atmsbeamdat=$fix_file/atms_beamwidth.txt
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
$ncp $atmsbeamdat  ./atms_beamwidth.txt
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Adjust data usage flags in convinfo file.
rm new
cp convinfo old
mv convinfo convinfo_original
sed 's/sst      180    0   -1     3.0/sst      180    0    1     3.0/' < old > new
mv new old
sed 's/uv       243   56    1     3.0/uv       243   56   -1     3.0/' < old > new
mv new old
sed 's/uv       253   56    1     3.0/uv       253   56   -1     3.0/' < old > new
mv new convinfo

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
$ncp $datobs/${prefix_obs}.satwnd.${suffix}         ./satwnd
$ncp $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
##$ncp $datobs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
##$ncp $datobs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncp $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
##$ncp $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
##$ncp $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
##$ncp $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
##$ncp $datobs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
##$ncp $datobs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
##$ncp $datobs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
##$ncp $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
##$ncp $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
##$ncp $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
##$ncp $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
##$ncp $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
##$ncp $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
##$ncp $datobs/${prefix_obs}.amsre.${suffix}         ./amsrebufr
##$ncp $datobs/${prefix_obs}.ssmis.${suffix}         ./ssmisbufr
##$ncp $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
##$ncp $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
##$ncp $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
##$ncp $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
##$ncp $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncp $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl

# Copy bias correction, atmospheric and surface files
$ncp $datges/${prefix_tbc}.abias                   ./satbias_in
$ncp $datges/${prefix_tbc}.satang                  ./satbias_angle

##$ncp $datges/${prefix_sfc}.bf03                    ./sfcf03
$ncp $datges/${prefix_sfc}.bf06                    ./sfcf06
##$ncp $datges/${prefix_sfc}.bf09                    ./sfcf09

##$ncp $datobs/${prefix_atm}.sgm3prep                ./sigf03
$ncp $datobs/${prefix_atm}.sgesprep                ./sigf06
##$ncp $datobs/${prefix_atm}.sgp3prep                ./sigf09

# Run gsi observer under Parallel Operating Environment (poe) on NCEP IBM
poe $tmpdir/gsi.x < gsiparm.anl.obsvr > stdout.obsvr
rc=$?
if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp2_global_3d4dvar_updat' has FAILED to run observer to completion, with an error code of '$rc''
   } >> $global_3d4dvar_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir
cat stdout.obsvr fort.2* > $savdir/stdout.anl.${adate}.obsvr
$ncp sfcf06          $savdir/sfcf06.${gdate}
$ncp sigf06          $savdir/sigf06.${gdate}

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
loops="01 03"
for loop in $loops; do
case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10"
   for type in $listall; do
      count=`ls ${tmpdir}/dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${adate}
         compress diag_${type}_${string}.${adate}
         $ncp diag_${type}_${string}.${adate}.Z $savdir/
      fi
   done
done

# Run gsi identity model 4dvar under Parallel Operating Environment (poe) on NCEP IBM
rm -f siganl sfcanl.gsi satbias_out fort.2*
rm -rf dir.0*
poe $tmpdir/gsi.x < gsiparm.anl.4dvar > stdout
rc=$?
if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp2_global_3d4dvar_updat' has FAILED to run id4dvar to completion, with an error code of '$rc''
   } >> $global_3d4dvar_regression
   $step_name==$rc
   exit
fi

# Save output
mkdir -p $savdir
cat stdout fort.2* > $savdir/stdout.anl.${adate}
$ncp siganl          $savdir/siganl.${adate}
$ncp sfcanl.gsi      $savdir/sfcanl.${adate}
$ncp satbias_out     $savdir/biascr.${adate}

exit ;;

  global_3d4dvar_regression)

set -ax

JCAP=62

# Choose the results that you wish to test.
# Here, exp1 is the run using the latest modified version of the code
# and exp2 is the control run

exp1=$exp1_global_3d4dvar_updat  #3dvar, update
exp2=$exp2_global_3d4dvar_updat  #4dvar, update
exp3=$exp1_global_3d4dvar_cntrl  #3dvar, control
exp4=$exp2_global_3d4dvar_cntrl  #4dvar, control

# Choose global, regional, or RTMA
input=tmp${global_3d4dvar}

# Name output file
output=$global_3d4dvar_regression

# Give location of analysis results, and choose location for regression output
savdir=$ptmp_loc/$input
savdir2=$noscrub/$input
vfydir=$regression_vfydir

ncp=/bin/cp

# Name and create temporary directory
tmpdir=$ptmp_loc/$compare/$input/${exp1}_vs_${exp3}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

# Other required constants for regression testing
maxtime=240
# Dew/Mist=26 GB/16 tasks per node
##maxmem=$((1500000*1))
# Vapor=110 GB/48 tasks per node
##maxmem=$((2300000*1))
# Cirrus=110 GB/32 tasks per node
maxmem=$((3400000*1))

# Copy stdout and fort.220 files 
# from $savdir to $tmpdir
list="$exp1 $exp2"
for exp in $list; do
   $ncp $savdir/$exp/stdout          ./stdout.$exp
   $ncp $savdir/$exp/fort.220        ./fort.220.$exp
   $ncp $savdir/$exp/siganl          ./siganl.$exp
done
list="$exp3 $exp4"
for exp in $list; do
   $ncp $savdir2/$exp/stdout          ./stdout.$exp
   $ncp $savdir2/$exp/fort.220        ./fort.220.$exp
   $ncp $savdir2/$exp/siganl          ./siganl.$exp
done

# Grep out penalty/gradient information, run time, and maximum resident memory from stdout file
list="$exp1 $exp2 $exp3 $exp4"
for exp in $list; do
   grep 'grepcost J,Jb' stdout.$exp                  > penalty.$exp.txt
   grep 'The total amount of wall time' stdout.$exp  > runtime.$exp.txt
   grep 'The maximum resident set size' stdout.$exp  > memory.$exp.txt
done

# Difference the 2 files (i.e., penalty.exp1.txt with penalty.exp2.txt)
diff penalty.$exp1.txt penalty.$exp2.txt > penalty.${exp1}-${exp2}.txt
diff penalty.$exp1.txt penalty.$exp3.txt > penalty.${exp1}-${exp3}.txt
diff penalty.$exp1.txt penalty.$exp4.txt > penalty.${exp1}-${exp4}.txt
diff penalty.$exp2.txt penalty.$exp4.txt > penalty.${exp2}-${exp4}.txt

# Give location of additional output files for scalability testing
exp1_scale=$exp2_global_3d4dvar_updat
exp2_scale=$exp2_global_3d4dvar_cntrl

# Copy stdout for additional scalability testing
list="$exp1_scale"
for exp_scale in $list; do
   $ncp $savdir/$exp_scale/stdout  ./stdout.$exp_scale
done
list="$exp2_scale"
for exp_scale in $list; do
   $ncp $savdir2/$exp_scale/stdout  ./stdout.$exp_scale
done

# Grep out run time from stdout file
list="$exp1_scale $exp2_scale"
for exp_scale in $list; do
   grep 'The total amount of wall time'  stdout.$exp_scale > runtime.$exp_scale.txt
   grep 'The maximum resident set size'  stdout.$exp_scale > memory.$exp_scale.txt
done

# Important values used to calculate timethresh and memthresh below
# Values below can be fine tuned to make the regression more or less aggressive
# Currently using a value of 10%

timedif=1
memdiff=8
scaledif=4

# timethresh = avgtime*timedif+avgtime
# memthresh = avgmem*memdiff+avgmem
# Note: using wall time/maximum residence memory from control as avg values here

time2=$(awk '{ print $8 }' runtime.$exp4.txt)
time1=$(awk '{ print $8 }' runtime.$exp2.txt)
mem=$(awk '{ print $8 }' memory.$exp1.txt)

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
    echo 'resulting in FAILURE of the regression test.'
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
    echo 'resulting in FAILURE of the regression test.'
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
    echo 'resulting in FAILURE of the regression test.'
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
    echo 'resulting in FAILURE of the regression test.'
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
    echo 'resulting in FAILURE of the regression test.'
    echo
  else
    echo 'The memory for '$exp1' is '$(awk '{ print $8 }' memory.$exp1.txt)' KBs and is within the maximum allowable memory of '$memthresh' KBs,'
    echo 'continuing with regression test.'
    echo
  fi

} >> $output

# Next, reproducibility between exp1 and exp3

{

if [[ $(grep -c 'grepcost J,Jb' penalty.${exp1}-${exp3}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are REPRODUCIBLE'
   echo 'since the corresponding penalties and gradients are IDENTICAL with '$(grep -c 'grepcost J,Jb' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are NOT REPRODUCIBLE,'
   echo 'thus the regression test has FAILED for '${exp1}' and '${exp3}' analyses with '$(grep -c 'grepcost J,Jb' penalty.${exp1}-${exp3}.txt)' lines different.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp1 and exp3

{

if cmp -s siganl.${exp1} siganl.${exp3} 
then
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are REPRODUCIBLE'
   echo 'since the corresponding siganl files are IDENTICAL.'
   echo
else
   echo 'The results between the two runs ('${exp1}' and '${exp3}') are NOT REPRODUCIBLE'
   echo 'since the corresponding siganl files DIFFER.'
   echo
fi

} >> $output

# Next, reproducibility between exp2 and exp4

{

if [[ $(grep -c 'grepcost J,Jb' penalty.${exp2}-${exp4}.txt) = 0 ]]; then
   echo 'The results between the two runs ('${exp2}' and '${exp4}') are REPRODUCIBLE'
   echo 'since the corresponding penalties and gradients are identical with '$(grep -c 'grepcost J,Jb' penalty.${exp2}-${exp4}.txt)' lines different.'
   echo
else
   echo 'The results between the two runs are NOT REPRODUCIBLE,'
   echo 'thus the regression test has FAILED for '${exp2}' and '${exp4}' analyses with '$(grep -c 'grepcost J,Jb' penalty.${exp2}-${exp4}.txt)' lines different.'
   echo
fi

} >> $output

# Next, check reproducibility of results between exp2 and exp4

{

if cmp -s siganl.${exp2} siganl.${exp4} 
then
   echo 'The results between the two runs ('${exp2}' and '${exp4}') are REPRODUCIBLE'
   echo 'since the corresponding siganl files are IDENTICAL.'
   echo
else 
   echo 'The results between the two runs ('${exp2}' and '${exp4}') are NOT REPRODUCIBLE'
   echo 'since the corresponding siganl files DIFFER.'
   echo
fi

} >> $output

# Finally, scalability

{

if [[ $scale1thresh -ge $scale2 ]]; then
   echo 'The case has passed the scalability regression test.'
   echo 'The slope for the update ('$scale1thresh' seconds per node) is greater than or equal to that for the control ('$scale2' seconds per node).'
else
   echo 'The case has FAILED the scalability test.'
   echo 'The slope for the update ('$scale1thresh' seconds per node) is less than that for the control ('$scale2' seconds per node).'
fi

} >> $output

# Copy select results to $savdir
mkdir -p $vfydir

$ncp $output                        $vfydir/

cd $scripts
rm -f regression_test.gsi_global_3d4dvar_updat.e*
rm -f regression_test.gsi_global_3d4dvar_updat2.e*
rm -f regression_test.gsi_global_3d4dvar_cntrl.e*
rm -f regression_test.gsi_global_3d4dvar_cntrl2.e*
rm -f regression_test.global_3d4dvar_regression.e*

exit ;;

  *) echo "Nothing to do for $LOADL_STEP_NAME"

esac

exit

#!/bin/sh

#@ job_name=regression_4dvar_debug_test
#@ error=global_4dvar_debug_test.e$(jobid)
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
#@ account_no = GDAS-T2O
#@ wall_clock_limit = 1:00:00
#@ startdate = 09/27/06 05:00
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


# Set experiment name and analysis date
adate=$adate_global
exp=$exp1_global_4dvar_updat

# Set path/file for gsi executable
gsiexec=$updat

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$ptmp_loc/tmp${global_4dvar}/${exp}
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
datobs=$datobs_global_4dvar/$adate
datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
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
ICH4=${ICH4:-0}
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
IN2O=${IN2O:-0}
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
ICO=${ICO:-0}
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
SETUPmin="miter=1,niter(1)=10,niter_no_qc(1)=500,"
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
. $scripts/regression_namelists_db.sh
rm gsiparm.anl.obsvr
cat << EOF > gsiparm.anl.obsvr

$global_T62_namelist

EOF

# Create namelist for identity model 4dvar run
SETUP4dv="l4dvar=.true.,jiterstart=1,nhr_assimilation=6,nhr_obsbin=$nhr_obsbin,idmodel=.true.,iwrtinc=1,lanczosave=.true.,"
SETUP="$SETUPmin $SETUPlan $SETUP4dv"
. $scripts/regression_namelists_db.sh
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

# Extract subset of data types to process from convinfo
cp convinfo convinfo_original
rm -rf type*
grep "   112   " convinfo > type112
grep "   120   " convinfo > type120
grep "   121   " convinfo > type121
grep "   130   " convinfo > type130
grep "   131   " convinfo > type131
grep "   132   " convinfo > type132
grep "   180   " convinfo > type180
grep "   182   " convinfo > type182
grep "   220   " convinfo > type220
grep "   221   " convinfo > type221
grep "   230   " convinfo > type230
grep "   231   " convinfo > type231
grep "   232   " convinfo > type232
grep "   242   " convinfo > type242
grep "   252   " convinfo > type252
grep "   280   " convinfo > type280
grep "   282   " convinfo > type282
grep "   004   " convinfo > type004
grep "   722   " convinfo > type722
grep "   741   " convinfo > type741

sed 's/sst      180    0   -1     3.0/sst      180    0    1     3.0/' < type180 > type180_new
mv type180_new type180

cat type* > convinfo_subset
mv convinfo_subset convinfo

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
$ncp $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$ncp $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$ncp $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$ncp $datobs/${prefix_obs}.amsre.${suffix}         ./amsrebufr
$ncp $datobs/${prefix_obs}.ssmis.${suffix}         ./ssmisbufr
$ncp $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncp $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncp $datobs/${prefix_obs}.mlsbufr.${suffix}        ./mlsbufr
$ncp $datobs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncp $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncp $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
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
    echo ''$exp1_global_4dvar_updat' has FAILED to run observer to completion, with an error code of '$rc''
   } >> $global_4dvar_regression
   exit
else
   cd $regression_vfydir
   {
    echo
    echo 'global_4dvar observer debug test has passed'
   } >> $global_4dvar_regression
fi

# Run gsi identity model 4dvar under Parallel Operating Environment (poe) on NCEP IBM
cd $tmpdir
rm -f siganl sfcanl.gsi satbias_out fort.2*
rm -rf dir.0*
poe $tmpdir/gsi.x < gsiparm.anl.4dvar > stdout
rc=$?
if [[ "$rc" != "0" ]]; then
   cd $regression_vfydir
   {
    echo ''$exp1_global_4dvar_updat' has FAILED to run id4dvar to completion, with an error code of '$rc''
   } >> $global_4dvar_regression
   exit
else
   cd $regression_vfydir
   {
    echo
    echo 'global_4dvar minimization debug test has passed'
   } >> $global_4dvar_regression
fi

exit


set -x

# Set experiment name and analysis date
if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#gsiexec=$updat

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
#export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$tmpdir/4dvar_tmp${JCAP}/${exp}
savdir=$savdir/4dvar_out${JCAP}/sigmap/${exp}

# Specify GSI fixed field and data directories.


# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
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
gdate=`$ndate -06 $global_4dvar_T62_adate`
hha=`echo $global_4dvar_T62_adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $global_4dvar_T62_adate | cut -c1-8`
gdate0=`echo $gdate | cut -c1-8`

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
        yyyy=$(echo ${CDATE:-$global_4dvar_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_4dvar_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_4dvar_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_4dvar_T62_adate}|cut -c1-4)
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

. $scripts/regression_nl_update.sh

GRIDOPTS="$GRIDOPTS_update"
BKGVERR="$BKGVERR_update"
ANBKGERR="$ANBKERR_update"
JCOPTS="$JCOPTS_update"
STRONGOPTS="$STRONGOPTS_update"
OBSQC="$OBSQC_update"
OBSINPUT="$OBSINPUT_update"
SUPERRAD="$SUPERRAD_update"
SINGLEOB="$SINGLEOB_update"

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
SETUP="$SETUPmin $SETUPlan $SETUPobs $SETUP_update"
. $scripts/regression_namelists.sh
rm gsiparm.anl
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
#   atmsbeamdat  =  data required for atms spatial averaging
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (optional)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fixgsi/global_anavinfo.l64.txt
berror=$fixgsi/$endianness/global_berror.l${LEVS}y${NLAT}.f77
emiscoef_IRwater=$fixcrtm/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$fixcrtm/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$fixcrtm/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$fixcrtm/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$fixcrtm/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$fixcrtm/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$fixcrtm/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$fixcrtm/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$fixcrtm/FASTEM5.MWwater.EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff.bin
satinfo=$fixgsi/global_satinfo_reg_test.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
atmsbeamdat=$fixgsi/atms_beamwidth.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
errtable=$fixgsi/prepobs_errtable.global

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp = $global_4dvar_T62_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_4dvar_T62_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_4dvar_T62_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $global_4dvar_T62_contrl_exp2 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $emiscoef_IRwater ./Nalli.IRwater.EmisCoeff.bin
$ncp $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
$ncp $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
$ncp $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
$ncp $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
$ncp $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
$ncp $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
$ncp $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
$ncp $emiscoef_MWwater ./FASTEM5.MWwater.EmisCoeff.bin
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
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $crtm_coef/${file}.SpcCoeff.bin ./
   $ncp $crtm_coef/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $global_4dvar_T62_obs/${prefix_obs}.prepbufr                ./prepbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.satwnd.${suffix}        ./satwndbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.amsre.${suffix}         ./amsrebufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.ssmis.${suffix}         ./ssmisbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.mlsbufr.${suffix}       ./mlsbufr
$ncp $global_4dvar_T62_obs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncp $global_4dvar_T62_obs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncp $global_4dvar_T62_obs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncp $global_4dvar_T62_obs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl

# Copy bias correction, atmospheric and surface files
$ncp $global_4dvar_T62_ges/${prefix_tbc}.abias                   ./satbias_in
$ncp $global_4dvar_T62_ges/${prefix_tbc}.satang                  ./satbias_angle

if [[ "$endianness" = "Big_Endian" ]]; then
   ##$ncp $global_4dvar_T62_ges/${prefix_sfc}.bf03               ./sfcf03
   $ncp $global_4dvar_T62_ges/${prefix_sfc}.bf06                 ./sfcf06
   ##$ncp $global_4dvar_T62_ges/${prefix_sfc}.bf09               ./sfcf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ##$ncp $global_4dvar_T62_ges/${prefix_sfc}.bf03.le            ./sfcf03
   $ncp $global_4dvar_T62_ges/${prefix_sfc}.bf06.le              ./sfcf06
   ##$ncp $global_4dvar_T62_ges/${prefix_sfc}.bf09.le            ./sfcf09
fi

if [[ "$endianness" = "Big_Endian" ]]; then
   ##$ncp $global_4dvar_T62_obs/${prefix_atm}.sgm3prep           ./sigf03
   $ncp $global_4dvar_T62_obs/${prefix_atm}.sgesprep             ./sigf06
   ##$ncp $global_4dvar_T62_obs/${prefix_atm}.sgp3prep           ./sigf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ##$ncp $global_4dvar_T62_obs/${prefix_atm}.sgm3prep.le        ./sigf03
   $ncp $global_4dvar_T62_obs/${prefix_atm}.sgesprep.le          ./sigf06
   ##$ncp $global_4dvar_T62_obs/${prefix_atm}.sgp3prep.le        ./sigf09
fi

# Run gsi observer under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$arch" = "Linux" ]]; then

   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout.obsvr"

elif [[ "$arch" = "AIX" ]]; then

   poe $tmpdir/gsi.x < gsiparm.anl > stdout.obsvr

fi

# Run gsi identity model 4dvar under Parallel Operating Environment (poe) on NCEP IBM
rm -f siganl sfcanl.gsi satbias_out fort.2*
rm -rf dir.0*

# Create namelist for identity model 4dvar run
SETUP4dv="l4dvar=.true.,jiterstart=1,nhr_assimilation=6,nhr_obsbin=$nhr_obsbin,idmodel=.true.,iwrtinc=1,lanczosave=.true.,"
SETUP="$SETUPmin $SETUPlan $SETUP4dv $SETUP_update"
. $scripts/regression_namelists.sh
rm gsiparm.anl
cat << EOF > gsiparm.anl

$global_T62_namelist

EOF

if [[ "$arch" = "Linux" ]]; then

   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   module load intel
   module load mpt

   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"

elif [[ "$arch" = "AIX" ]]; then

   poe $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

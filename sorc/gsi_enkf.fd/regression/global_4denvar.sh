set -x

# Set experiment name and analysis date

exp=$jobname

# Set the JCAP resolution which you want.
export JCAP=48
export LEVS=127
export JCAP_B=96

# Set ensemble size
export NMEM_ENKF=10

# Set runtime directories
tmpdir=$tmpdir/$tmpregdir/${exp}

# Specify GSI fixed field and data directories.
fixcrtm=${fixcrtm:-$CRTM_FIX}


# Set variables used in script
UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp
nln="/bin/ln -fs"


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "96" ]]; then
   export LONA=384
   export LATA=192
   export DELTIM=1200
elif [[ "$JCAP" = "48" ]]; then
   export LONA=192
   export LATA=96
   export DELTIM=1200
else
   echo "INVALID JCAP = $JCAP"
   exit
fi
export NLON=$LONA
export NLAT=$((${LATA}+2))


# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`date +%Y%m%d%H -d "${global_adate:0:8} ${global_adate:8:2} - 6 hours"`
PDYa=`echo $global_adate | cut -c1-8`
cyca=`echo $global_adate | cut -c9-10`
PDYg=`echo $gdate | cut -c1-8`
cycg=`echo $gdate | cut -c9-10`

dumpobs=gdas
prefix_obs=${dumpobs}.t${cyca}z
prefix_ges=gdas.t${cycg}z
prefix_ens=enkfgdas.t${cycg}z
suffix=tm00.bufr_d

dumpges=gdas
COMROOTgfs=$casesdir/gfs/prod
datobs=$COMROOTgfs/$dumpobs.$PDYa/${cyca}/obs
dathis=$COMROOTgfs/$dumpges.$PDYg/${cycg}/model/atmos/history
datanl=$COMROOTgfs/gdas.$PDYg/${cycg}/analysis/atmos
datens=$COMROOTgfs/enkfgdas.$PDYg/${cycg}


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

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

if [ "$debug" = ".false." ]; then
   . $scripts/regression_namelists.sh global_4denvar
else
   . $scripts/regression_namelists_db.sh global_4denvar
fi

cat << EOF > gsiparm.anl

$gsi_namelist

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
#   aeroinfo = text file with information about assimilation of aerosol data

anavinfo=$fixgsi/global_anavinfo.l${LEVS}.txt
berror=$fixgsi/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77
locinfo=$fixgsi/global_hybens_info.l${LEVS}.txt
satinfo=$fixgsi/global_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/global_satangbias.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
convinfo=$fixgsi/global_convinfo.txt
vqcdat=$fixgsi/vqctp001.dat
insituinfo=$fixgsi/global_insituinfo.txt
errtable=$fixgsi/prepobs_errtable.global
aeroinfo=$fixgsi/global_aeroinfo.txt
atmsbeaminfo=$fixgsi/atms_beamwidth.txt
cloudyinfo=$fixgsi/cloudy_radiance_info.txt
cris_clddet=$fixgsi/CRIS_CLDDET.NL
iasi_clddet=$fixgsi/IASI_CLDDET.NL

emiscoef_IRwater=$fixcrtm/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$fixcrtm/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$fixcrtm/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$fixcrtm/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$fixcrtm/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$fixcrtm/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$fixcrtm/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$fixcrtm/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$fixcrtm/FASTEM6.MWwater.EmisCoeff.bin
aercoef=$fixcrtm/AerosolCoeff.bin
cldcoef=$fixcrtm/CloudCoeff.bin
#cldcoef=$fixcrtm/CloudCoeff.GFDLFV3.-109z-1.bin   # use with crtm/2.4.0

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $gsiexec_updat  ./gsi.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

$ncp $anavinfo ./anavinfo
$ncp $berror   ./berror_stats
$ncp $locinfo  ./hybens_info
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $vqcdat   ./vqctp001.dat
$ncp $insituinfo ./insituinfo
$ncp $errtable ./errtable
$ncp $aeroinfo ./aeroinfo
$ncp $atmsbeaminfo ./atms_beamwidth.txt
$ncp $cloudyinfo   ./cloudy_radiance_info.txt
$ncp $cris_clddet ./CRIS_CLDDET.NL
$ncp $iasi_clddet ./IASI_CLDDET.NL

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

#If using correlated error, get the covariance files
if grep -q "Rcov" $anavinfo ;
then
  if ls ${fixgsi}/Rcov* 1> /dev/null 2>&1;
  then
    $ncp ${fixgsi}/Rcov* $tmpdir

#   Correlated error utlizes mkl lapack.  Found it necesary to fix the
#   number of mkl threads to ensure reproducible results independent
#   of the job configuration.
    export MKL_NUM_THREADS=1

  else
    echo "Warning: Satellite error covariance files are missing."
    echo "Check for the required Rcov files in " $ANAVINFO
    exit 1
  fi
fi

# Copy CRTM coefficient files based on entries in satinfo file
export CRTM_PATH="./crtm_coeffs/"
mkdir -p ${CRTM_PATH}
for file in `awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq` ;do
   $nln $fixcrtm/${file}.SpcCoeff.bin ${CRTM_PATH}/${file}.SpcCoeff.bin
   $nln $fixcrtm/${file}.TauCoeff.bin ${CRTM_PATH}/${file}.TauCoeff.bin
done
$nln $fixcrtm/amsua_metop-a_v2.SpcCoeff.bin ${CRTM_PATH}/amsua_metop-a_v2.SpcCoeff.bin

$nln $emiscoef_IRwater  ${CRTM_PATH}Nalli.IRwater.EmisCoeff.bin
$nln $emiscoef_IRice    ${CRTM_PATH}NPOESS.IRice.EmisCoeff.bin
$nln $emiscoef_IRsnow   ${CRTM_PATH}NPOESS.IRsnow.EmisCoeff.bin
$nln $emiscoef_IRland   ${CRTM_PATH}NPOESS.IRland.EmisCoeff.bin
$nln $emiscoef_VISice   ${CRTM_PATH}NPOESS.VISice.EmisCoeff.bin
$nln $emiscoef_VISland  ${CRTM_PATH}NPOESS.VISland.EmisCoeff.bin
$nln $emiscoef_VISsnow  ${CRTM_PATH}NPOESS.VISsnow.EmisCoeff.bin
$nln $emiscoef_VISwater ${CRTM_PATH}NPOESS.VISwater.EmisCoeff.bin
$nln $emiscoef_MWwater  ${CRTM_PATH}FASTEM6.MWwater.EmisCoeff.bin
$nln $aercoef           ${CRTM_PATH}AerosolCoeff.bin
$nln $cldcoef           ${CRTM_PATH}CloudCoeff.bin

# Copy observational data
$nln $datobs/${prefix_obs}.prepbufr                ./prepbufr
$nln $datobs/${prefix_obs}.prepbufr.acft_profiles  ./prepbufr_profl
$nln $datobs/${prefix_obs}.nsstbufr                ./nsstbufr
$nln $datobs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl
$nln $datobs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$nln $datobs/${prefix_obs}.satwnd.${suffix}        ./satwndbufr
$nln $datobs/${prefix_obs}.hdob.${suffix}          ./hdobbufr

$nln $datobs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$nln $datobs/${prefix_obs}.gome.${suffix}          ./gomebufr
$nln $datobs/${prefix_obs}.omi.${suffix}           ./omibufr
$nln $datobs/${prefix_obs}.mls.${suffix}           ./mlsbufr
$nln $datobs/${prefix_obs}.ompsn8.${suffix}        ./ompsnpbufr
$nln $datobs/${prefix_obs}.ompst8.${suffix}        ./ompstcbufr
$nln $datobs/${prefix_obs}.ompslp.${suffix}        ./ompslpbufr

$nln $datobs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$nln $datobs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$nln $datobs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$nln $datobs/${prefix_obs}.saphir.${suffix}        ./saphirbufr
$nln $datobs/${prefix_obs}.avcsam.${suffix}        ./avhambufr
$nln $datobs/${prefix_obs}.avcspm.${suffix}        ./avhpmbufr
$nln $datobs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$nln $datobs/${prefix_obs}.esiasi.${suffix}        ./iasibufrears
$nln $datobs/${prefix_obs}.iasidb.${suffix}        ./iasibufr_db
$nln $datobs/${prefix_obs}.crisf4.${suffix}        ./crisfsbufr
$nln $datobs/${prefix_obs}.escrsf.${suffix}        ./crisfsbufrears
$nln $datobs/${prefix_obs}.crsfdb.${suffix}        ./crisfsbufr_db
$nln $datobs/${prefix_obs}.ahicsr.${suffix}        ./ahibufr
$nln $datobs/${prefix_obs}.gsrcsr.${suffix}        ./abibufr
$nln $datobs/${prefix_obs}.sstvcw.${suffix}        ./sstviirs

$nln $datobs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$nln $datobs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$nln $datobs/${prefix_obs}.gmi1cr.${suffix}        ./gmibufr
$nln $datobs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$nln $datobs/${prefix_obs}.ssmisu.${suffix}        ./ssmisbufr
$nln $datobs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$nln $datobs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$nln $datobs/${prefix_obs}.amuadb.${suffix}        ./amsuabufr_db
$nln $datobs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$nln $datobs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$nln $datobs/${prefix_obs}.amubdb.${suffix}        ./amsubbufr_db
$nln $datobs/${prefix_obs}.atms.${suffix}          ./atmsbufr
$nln $datobs/${prefix_obs}.atmsdb.${suffix}        ./atmsbufr_db
$nln $datobs/${prefix_obs}.esatms.${suffix}        ./atmsbufrears

# Do not process
## $nln $datobs/${prefix_obs}.amsre.${suffix}      ./amsrebufr
## $nln $datobs/${prefix_obs}.amsr2.tm00.bufr_d    ./amsr2bufr

# Copy bias correction, atmospheric and surface files
$nln $datanl/${prefix_ges}.abias                      ./satbias_in
$nln $datanl/${prefix_ges}.abias_pc                   ./satbias_pc
$nln $datanl/${prefix_ges}.abias_air                  ./aircftbias_in
$nln $datanl/${prefix_ges}.radstat                    ./radstat.gdas

$nln $dathis/${prefix_ges}.sfcf003.nc         ./sfcf03
$nln $dathis/${prefix_ges}.sfcf004.nc         ./sfcf04
$nln $dathis/${prefix_ges}.sfcf005.nc         ./sfcf05
$nln $dathis/${prefix_ges}.sfcf006.nc         ./sfcf06
$nln $dathis/${prefix_ges}.sfcf007.nc         ./sfcf07
$nln $dathis/${prefix_ges}.sfcf008.nc         ./sfcf08
$nln $dathis/${prefix_ges}.sfcf009.nc         ./sfcf09

$nln $dathis/${prefix_ges}.atmf003.nc         ./sigf03
$nln $dathis/${prefix_ges}.atmf004.nc         ./sigf04
$nln $dathis/${prefix_ges}.atmf005.nc         ./sigf05
$nln $dathis/${prefix_ges}.atmf006.nc         ./sigf06
$nln $dathis/${prefix_ges}.atmf007.nc         ./sigf07
$nln $dathis/${prefix_ges}.atmf008.nc         ./sigf08
$nln $dathis/${prefix_ges}.atmf009.nc         ./sigf09

$nln $datens/ensstat/model/atmos/history/${prefix_ens}.sfcf006.ensmean.nc         ./sfcf06_anlgrid

export ENS_PATH='./ensemble_data/'
mkdir -p ${ENS_PATH}
flist="03 04 05 06 07 08 09"
for fh in $flist; do
    sigens=${prefix_ens}.atmf0${fh}.nc
    imem=1
    while [[ $imem -le $NMEM_ENKF ]]; do
	member="mem"`printf %03i $imem`
	$nln $datens/$member/model/atmos/history/$sigens ${ENS_PATH}sigf${fh}_ens_${member}
	(( imem = $imem + 1 ))
    done
done

 
listdiag=`tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges`
for type in $listdiag; do
   diag_file=`echo $type | cut -d',' -f1`
   fname=`echo $diag_file | cut -d'.' -f1`
   date=`echo $diag_file | cut -d'.' -f2`
   $UNCOMPRESS $diag_file
   fnameanl=$(echo $fname|sed 's/_ges//g')
   mv $fname.$date $fnameanl
done

# Run GSI
cd $tmpdir
echo "run gsi now"
eval "$APRUN $tmpdir/gsi.x < gsiparm.anl > stdout 2>&1"
rc=$?

exit $rc

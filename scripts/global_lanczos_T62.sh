
set -x

# Set experiment name and analysis date
if [[ "$arch" = "Linux" ]]; then
   exp=$jobname
elif [[ "$arch" = "AIX" ]]; then
   exp=$LOADL_JOB_NAME
fi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
#export JCAP=62
export LEVS=64
export JCAP_B=62

# Set runtime and save directories
tmpdir=$tmpdir/lanczos_tmp${JCAP}/${exp}
savdir=$savdir/lanczos_out${JCAP}/sigmap/${exp}

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
gdate=`$ndate -06 $global_lanczos_T62_adate`
hha=`echo $global_lanczos_T62_adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
prefixg=gdas1.t${hhg}z
suffix=tm00.bufr_d

adate0=`echo $global_lanczos_T62_adate | cut -c1-8`
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
        yyyy=$(echo ${CDATE:-$global_lanczos_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_lanczos_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_lanczos_T62_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_lanczos_T62_adate}|cut -c1-4)
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

SETUP="$SETUP_update"
GRIDOPTS="$GRIDOPTS_update"
BKGVERR="$BKGVERR_update"
ANBKGERR="$ANBKERR_update"
JCOPTS="$JCOPTS_update"
STRONGOPTS="$STRONGOPTS_update"
OBSQC="$OBSQC_update"
OBSINPUT="$OBSINPUT_update"
SUPERRAD="$SUPERRAD_update"
SINGLEOB="$SINGLEOB_update"

. $scripts/regression_namelists.sh

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
if [[ $exp = $global_lanczos_T62_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_lanczos_T62_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_lanczos_T62_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $global_lanczos_T62_contrl_exp2 ]]; then
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

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   $ncp $crtm_coef/${file}.SpcCoeff.bin ./
   $ncp $crtm_coef/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $global_lanczos_T62_obs/${prefix_obs}.prepbufr                ./prepbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.satwnd.${suffix}        ./satwndbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.gpsro.${suffix}         ./gpsrobufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.spssmi.${suffix}        ./ssmirrbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.sptrmm.${suffix}        ./tmirrbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.osbuv8.${suffix}        ./sbuvbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.goesfv.${suffix}        ./gsnd1bufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bamua.${suffix}        ./amsuabufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bamub.${suffix}        ./amsubbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bhrs2.${suffix}        ./hirs2bufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bhrs3.${suffix}        ./hirs3bufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bhrs4.${suffix}        ./hirs4bufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bmhs.${suffix}         ./mhsbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.1bmsu.${suffix}         ./msubufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.airsev.${suffix}        ./airsbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.sevcsr.${suffix}        ./seviribufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.mtiasi.${suffix}        ./iasibufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.ssmit.${suffix}         ./ssmitbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.amsre.${suffix}         ./amsrebufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.ssmis.${suffix}         ./ssmisbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.gome.${suffix}          ./gomebufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.omi.${suffix}           ./omibufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.mlsbufr.${suffix}       ./mlsbufr
$ncp $global_lanczos_T62_obs/${prefix_obs}.eshrs3.${suffix}        ./hirs3bufrears
$ncp $global_lanczos_T62_obs/${prefix_obs}.esamua.${suffix}        ./amsuabufrears
$ncp $global_lanczos_T62_obs/${prefix_obs}.esamub.${suffix}        ./amsubbufrears
$ncp $global_lanczos_T62_obs/${prefix_obs}.syndata.tcvitals.tm00   ./tcvitl

# Copy bias correction, atmospheric and surface files
$ncp $global_lanczos_T62_ges/${prefix_tbc}.abias                   ./satbias_in
$ncp $global_lanczos_T62_ges/${prefix_tbc}.satang                  ./satbias_angle

if [[ "$endianness" = "Big_Endian" ]]; then
   $ncp $global_lanczos_T62_ges/${prefix_sfc}.bf03                 ./sfcf03
   $ncp $global_lanczos_T62_ges/${prefix_sfc}.bf06                 ./sfcf06
   $ncp $global_lanczos_T62_ges/${prefix_sfc}.bf09                 ./sfcf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   $ncp $global_lanczos_T62_ges/${prefix_sfc}.bf03.le              ./sfcf03
   $ncp $global_lanczos_T62_ges/${prefix_sfc}.bf06.le              ./sfcf06
   $ncp $global_lanczos_T62_ges/${prefix_sfc}.bf09.le              ./sfcf09
fi

if [[ "$endianness" = "Big_Endian" ]]; then
   $ncp $global_lanczos_T62_obs/${prefix_atm}.sgm3prep             ./sigf03
   $ncp $global_lanczos_T62_obs/${prefix_atm}.sgesprep             ./sigf06
   $ncp $global_lanczos_T62_obs/${prefix_atm}.sgp3prep             ./sigf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   $ncp $global_lanczos_T62_obs/${prefix_atm}.sgm3prep.le          ./sigf03
   $ncp $global_lanczos_T62_obs/${prefix_atm}.sgesprep.le          ./sigf06
   $ncp $global_lanczos_T62_obs/${prefix_atm}.sgp3prep.le          ./sigf09
fi

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
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
echo "Time after diagnostic loop is `date` "

exit

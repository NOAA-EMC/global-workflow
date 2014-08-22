
set -x

# Set experiment name and analysis date

exp=$jobname

# Set path/file for gsi executable
#basedir=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken
#gsiexec=$gsiexec
#gsiexec=$basedir/EXP-port/src/global_gsi


# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
#export JCAP=62
export LEVS=64
export JCAP_B=$JCAP

# Set runtime and save directories
tmpdir=$tmpdir/tmp${JCAP}/${exp}
#tmpdir=/scratch2/portfolios/NCEPDEV/ptmp/Michael.Lueken/tmp${JCAP}/${exp}
savdir=$savdir/out${JCAP}/${exp}
#savdir=/scratch2/portfolios/NCEPDEV/ptmp/Michael.Lueken/out${JCAP}/${exp}

# Specify GSI fixed field and data directories.
#fixgsi=$fixgsi
#fixgsi=$basedir/EXP-port/fix
#fixcrtm=$fixcrtm
#fixcrtm=$basedir/nwprod/lib/sorc/CRTM_REL-2.1.3/Big_Endian

#datobs=$datobs
#datobs=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/sigmap/$adate
#datges=$datges
#datges=/scratch1/portfolios/NCEPDEV/da/noscrub/Michael.Lueken/CASES/sigmap/$adate

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
ncp=/bin/cp


# Given the requested resolution, set dependent resolution parameters
if [[ "$JCAP" = "574" ]]; then
   export LONA=1152
   export LATA=576
   export DELTIM=120
   export resol=1
elif [[ "$JCAP" = "382" ]]; then
   export LONA=768
   export LATA=384
   export DELTIM=180
   export resol=1
elif [[ "$JCAP" = "126" ]]; then
   export LONA=256
   export LATA=128
   export DELTIM=600
   export resol=2
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
gdate=`$ndate -06 $global_T62_adate`
hha=`echo $global_T62_adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefix_obs=gdas1.t${hha}z.
prefix_prep=$prefix_obs
prefix_tbc=gdas1.t${hhg}z
prefix_sfc=gdas${resol}.t${hhg}z
prefix_atm=gdas${resol}.t${hha}z
suffix=tm00.bufr_d


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*


# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
        # Copy co2 files to $tmpdir
        co2dir=${CO2DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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
        ch4dir=${CH4DIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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
        n2odir=${N2ODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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
        codir=${CODIR:-$fixgsi}
        yyyy=$(echo ${CDATE:-$global_T62_adate}|cut -c1-4)
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

# Make gsi namelist

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
satangl=$fixgsi/global_satangbias.txt

satinfo=$fixgsi/global_satinfo.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
anavinfo=$fixgsi/global_anavinfo.l64.txt
ozinfo=$fixgsi/global_ozinfo.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
hybens_locinfo=$fixgsi/global_hybens_locinfo.l64.txt
errtable=$fixgsi/prepobs_errtable.global
atmsbeaminfo=$fixgsi/atms_beamwidth.txt

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp = $global_T62_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_T62_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_T62_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $global_T62_contrl_exp2 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

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
$ncp $satinfo  ./satinfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $anavinfo ./anavinfo
$ncp $hybens_locinfo ./hybens_locinfo
$ncp $atmsbeaminfo ./atms_beamwidth.txt

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $fixcrtm/${file}.SpcCoeff.bin ./
    $ncp $fixcrtm/${file}.TauCoeff.bin ./
done


# Copy observational data to $tmpdir
ln -s -f $global_T62_obs/${prefix_obs}prepbufr           ./prepbufr
ln -s -f $global_T62_obs/${prefix_obs}satwnd.${suffix}   ./satwndbufr
ln -s -f $global_T62_obs/${prefix_obs}gpsro.${suffix}    ./gpsrobufr
ln -s -f $global_T62_obs/${prefix_obs}spssmi.${suffix}   ./ssmirrbufr
ln -s -f $global_T62_obs/${prefix_obs}sptrmm.${suffix}   ./tmirrbufr
ln -s -f $global_T62_obs/${prefix_obs}gome.${suffix}     ./gomebufr
ln -s -f $global_T62_obs/${prefix_obs}omi.${suffix}      ./omibufr
ln -s -f $global_T62_obs/${prefix_obs}mls.${suffix}      ./mlsbufr
ln -s -f $global_T62_obs/${prefix_obs}osbuv8.${suffix}   ./sbuvbufr
ln -s -f $global_T62_obs/${prefix_obs}goesfv.${suffix}   ./gsnd1bufr
ln -s -f $global_T62_obs/${prefix_obs}1bamua.${suffix}   ./amsuabufr
ln -s -f $global_T62_obs/${prefix_obs}1bamub.${suffix}   ./amsubbufr
ln -s -f $global_T62_obs/${prefix_obs}1bhrs2.${suffix}   ./hirs2bufr
ln -s -f $global_T62_obs/${prefix_obs}1bhrs3.${suffix}   ./hirs3bufr
ln -s -f $global_T62_obs/${prefix_obs}1bhrs4.${suffix}   ./hirs4bufr
ln -s -f $global_T62_obs/${prefix_obs}1bmhs.${suffix}    ./mhsbufr
ln -s -f $global_T62_obs/${prefix_obs}1bmsu.${suffix}    ./msubufr
ln -s -f $global_T62_obs/${prefix_obs}airsev.${suffix}   ./airsbufr
ln -s -f $global_T62_obs/${prefix_obs}sevcsr.${suffix}   ./seviribufr
ln -s -f $global_T62_obs/${prefix_obs}mtiasi.${suffix}   ./iasibufr
ln -s -f $global_T62_obs/${prefix_obs}esamua.${suffix}   ./amsuabufrears
ln -s -f $global_T62_obs/${prefix_obs}esamub.${suffix}   ./amsubbufrears
ln -s -f $global_T62_obs/${prefix_obs}eshrs3.${suffix}   ./hirs3bufrears
ln -s -f $global_T62_obs/${prefix_obs}ssmit.${suffix}    ./ssmitbufr
ln -s -f $global_T62_obs/${prefix_obs}amsre.${suffix}    ./amsrebufr
ln -s -f $global_T62_obs/${prefix_obs}ssmis.${suffix}    ./ssmisbufr
ln -s -f $global_T62_obs/${prefix_obs}atms.${suffix}     ./atmsbufr
ln -s -f $global_T62_obs/${prefix_obs}cris.${suffix}     ./crisbufr
ln -s -f $global_T62_obs/${prefix_obs}syndata.tcvitals.tm00 ./tcvitl


# Copy bias correction, atmospheric and surface files
ln -s -f $global_T62_ges/${prefix_tbc}.abias              ./satbias_in
ln -s -f $global_T62_ges/${prefix_tbc}.abias_pc           ./satbias_pc
ln -s -f $global_T62_ges/${prefix_tbc}.satang             ./satbias_angle
ln -s -f $global_T62_ges/${prefix_tbc}.radstat            ./radstat.gdas

if [[ "$endianness" = "Big_Endian" ]]; then
   ln -s -f $global_T62_ges/${prefix_sfc}.bf03            ./sfcf03
   ln -s -f $global_T62_ges/${prefix_sfc}.bf06            ./sfcf06
   ln -s -f $global_T62_ges/${prefix_sfc}.bf09            ./sfcf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ln -s -f $global_T62_ges/${prefix_sfc}.bf03.le         ./sfcf03
   ln -s -f $global_T62_ges/${prefix_sfc}.bf06.le         ./sfcf06
   ln -s -f $global_T62_ges/${prefix_sfc}.bf09.le         ./sfcf09
fi

if [[ "$endianness" = "Big_Endian" ]]; then
   ln -s -f $global_T62_obs/${prefix_atm}.sgm3prep        ./sigf03
   ln -s -f $global_T62_obs/${prefix_atm}.sgesprep        ./sigf06
   ln -s -f $global_T62_obs/${prefix_atm}.sgp3prep        ./sigf09
elif [[ "$endianness" = "Little_Endian" ]]; then
   ln -s -f $global_T62_obs/${prefix_atm}.sgm3prep.le     ./sigf03
   ln -s -f $global_T62_obs/${prefix_atm}.sgesprep.le     ./sigf06
   ln -s -f $global_T62_obs/${prefix_atm}.sgp3prep.le     ./sigf09
fi

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$machine" = "Zeus" ]]; then

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

elif [[ "$machine" = "WCOSS" ]]; then

   mpirun.lsf $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit




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
cd $tmpdir
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
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${global_T62_adate}
         compress diag_${type}_${string}.${global_T62_adate}
         $ncp diag_${type}_${string}.${global_T62_adate}.Z $savdir/
      fi
   done
done
echo "Time after diagnostic loop is `date` "



# If requested, clean up $tmpdir
if [[ "$CLEAN" = "YES" ]];then
   if [[ $rc -eq 0 ]];then
      rm -rf $tmpdir
      cd $tmpdir
      cd ../
      rmdir $tmpdir
   fi
fi


# End of script
exit

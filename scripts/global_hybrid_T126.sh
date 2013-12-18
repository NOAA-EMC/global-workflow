
set -x

# Set experiment name and analysis date

if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#basedir=/scratch1/portfolios/NCEPDEV/da/save/Daryl.Kleist
#gsipath=$basedir/gsi/
#gsiexec=$gsipath/trunk/src/global_gsi

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
export JCAP=126
export LEVS=64
export JCAP_B=126
export JCAP_EN=62

# Set runtime and save directories
tmpdir=$tmpdir/tmp${JCAP}/${exp}
#tmpdir=/scratch2/portfolios/NCEPDEV/ptmp/Daryl.Kleist/tmp${JCAP}/${exp}
savdir=$savdir/out${JCAP}/${exp}
#savdir=/scratch2/portfolios/NCEPDEV/ptmp/Daryl.Kleist/out${JCAP}/${exp}

# Specify GSI fixed field and data directories.
#fixgsi=$gsipath/trunk/fix
#fixcrtm=$gsipath/EXP-port410/lib/CRTM_REL-2.1.3/fix

#datobs=/scratch1/portfolios/NCEPDEV/da/noscrub/Daryl.Kleist/CASES/$adate/obs
#datges=/scratch1/portfolios/NCEPDEV/da/noscrub/Daryl.Kleist/CASES/$adate/ges
#datens=/scratch1/portfolios/NCEPDEV/da/noscrub/Daryl.Kleist/CASES/$adate/ens

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/scratch1/portfolios/NCEPDEV/da/save/Daryl.Kleist/nwprod/util/exec/ndate
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
gdate=`$ndate -06 $global_hybrid_T126_adate`
yyg=`echo $gdate | cut -c1-8`
hhg=`echo $gdate | cut -c9-10`
yya=`echo $global_hybrid_T126_adate | cut -c1-8`
hha=`echo $global_hybrid_T126_adate | cut -c9-10`

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
        yyyy=$(echo ${CDATE:-$global_hybrid_T126_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_hybrid_T126_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_hybrid_T126_adate}|cut -c1-4)
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
        yyyy=$(echo ${CDATE:-$global_hybrid_T126_adate}|cut -c1-4)
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

cat << EOF > gsiparm.anl

$global_hybrid_T126_namelist

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

berror=$fixgsi/Big_Endian/global_berror.l${LEVS}y${NLAT}.f77

emiscoef_IRwater=$crtm_coef/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=$crtm_coef/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=$crtm_coef/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=$crtm_coef/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=$crtm_coef/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=$crtm_coef/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=$crtm_coef/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=$crtm_coef/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=$crtm_coef/FASTEM5.MWwater.EmisCoeff.bin
aercoef=$crtm_coef/AerosolCoeff.bin
cldcoef=$crtm_coef/CloudCoeff.bin
satangl=$fixgsi/global_satangbias.txt

satinfo=$fixgsi/global_satinfo.txt
convinfo=$fixgsi/global_convinfo_reg_test.txt
anavinfo=$fixgsi/global_anavinfo.l64.txt
ozinfo=$fixgsi/global_ozinfo.txt
pcpinfo=$fixgsi/global_pcpinfo.txt
errtable=$fixgsi/prepobs_errtable.global
hybens_locinfo=$fixgsi/global_hybens_locinfo.l64.txt

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp = $global_hybrid_T126_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_hybrid_T126_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $global_hybrid_T126_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $global_hybrid_T126_contrl_exp2 ]]; then
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

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $crtm_coef/${file}.SpcCoeff.bin ./
    $ncp $crtm_coef/${file}.TauCoeff.bin ./
done


# Copy observational data to $tmpdir
$ncp $global_hybrid_T126_datobs/prepqc.gdas.$global_hybrid_T126_adate   ./prepbufr
$ncp $global_hybrid_T126_datobs/satwnd.gdas.$global_hybrid_T126_adate   ./satwndbufr
$ncp $global_hybrid_T126_datobs/gpsro.gdas.$global_hybrid_T126_adate    ./gpsrobufr
$ncp $global_hybrid_T126_datobs/sptrmm.gdas.$global_hybrid_T126_adate   ./tmirrbufr
$ncp $global_hybrid_T126_datobs/osbuv8.gdas.$global_hybrid_T126_adate   ./sbuvbufr
$ncp $global_hybrid_T126_datobs/gome.gdas.$global_hybrid_T126_adate     ./gomebufr
$ncp $global_hybrid_T126_datobs/omi.gdas.$global_hybrid_T126_adate      ./omibufr
$ncp $global_hybrid_T126_datobs/tcvitl.gdas.$global_hybrid_T126_adate   ./tcvitl
$ncp $global_hybrid_T126_datobs/goesfv.gdas.$global_hybrid_T126_adate   ./gsnd1bufr
$ncp $global_hybrid_T126_datobs/1bamua.gdas.$global_hybrid_T126_adate   ./amsuabufr
$ncp $global_hybrid_T126_datobs/1bamub.gdas.$global_hybrid_T126_adate   ./amsubbufr
$ncp $global_hybrid_T126_datobs/1bhrs3.gdas.$global_hybrid_T126_adate   ./hirs3bufr
$ncp $global_hybrid_T126_datobs/1bhrs4.gdas.$global_hybrid_T126_adate   ./hirs4bufr
$ncp $global_hybrid_T126_datobs/airsev.gdas.$global_hybrid_T126_adate   ./airsbufr
$ncp $global_hybrid_T126_datobs/mtiasi.gdas.$global_hybrid_T126_adate   ./iasibufr
$ncp $global_hybrid_T126_datobs/esamua.gdas.$global_hybrid_T126_adate   ./amsuabufrears
$ncp $global_hybrid_T126_datobs/esamub.gdas.$global_hybrid_T126_adate   ./amsubbufrears
$ncp $global_hybrid_T126_datobs/eshrs3.gdas.$global_hybrid_T126_adate   ./hirs3bufrears

# Copy bias correction, atmospheric and surface files
$ncp $global_hybrid_T126_datges/biascr.gdas.$gdate   ./satbias_in
$ncp $global_hybrid_T126_datges/satang.gdas.$gdate   ./satbias_angle

$ncp $global_hybrid_T126_datges/sfcf03.gdas.$gdate.t${JCAP}  ./sfcf03
$ncp $global_hybrid_T126_datges/sfcf06.gdas.$gdate.t${JCAP}  ./sfcf06
$ncp $global_hybrid_T126_datges/sfcf09.gdas.$gdate.t${JCAP}  ./sfcf09

$ncp $global_hybrid_T126_datges/siggm3.gdas.$global_hybrid_T126_adate.t${JCAP}  ./sigf03
$ncp $global_hybrid_T126_datges/sigges.gdas.$global_hybrid_T126_adate.t${JCAP}  ./sigf06
$ncp $global_hybrid_T126_datges/siggp3.gdas.$global_hybrid_T126_adate.t${JCAP}  ./sigf09

list="001 002 003 004 005 006 007 008 009 010 011 012 013 014 015 016 017 018 019 020"

for file in $list; do
   ln -s $global_hybrid_T126_datges/sigf06s_${gdate}_mem${file}_t${JCAP_EN} ./sigf06_ens_mem${file}
done

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$arch" = "Linux" ]]; then
   cd $tmpdir/
   echo "run gsi now"

   export MPI_DISPLAY_SETTINGS=YES
   export MPI_STATS=YES
   export MPI_STATS_FILE=mpi_tmp.out

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   export OMP_NUM_THREADS=1

   module load intel
   module load mpt
   echo "JOB ID : $PBS_JOBID"
   eval "mpiexec_mpt -v -np $PBS_NP $tmpdir/gsi.x > stdout"

elif [[ "$arch" = "AIX" ]]; then

   poe $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

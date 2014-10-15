
set -x

# Set analysis date
adate=$hwrf_nmm_adate

io_format=netcdf

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

# Set experiment name and analysis date

exp=$jobname

# Set path/file for gsi executable
#gsiexec=$gsiexec

# Set the JCAP resolution which you want.
# All resolutions use LEVS=64
#export JCAP=62
export LEVS=60
export JCAP_B=$JCAP
export DELTIM=1200

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_hwrf_nmm_d2/${exp}
savdir=$savdir/outreg_hwrf_nmm_d2/${exp}

# Specify GSI fixed field and data directories.
#fixgsi=$fixgsi

#datobs=$datobs

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/scratch1/portfolios/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
ncp=/bin/cp
lnsf='ln -sf'

NX2=166
NY2=336
export NLAT=$(( NY2 - 1 ))
export NLON=$(( NX2 - 1 ))

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
gdate=`$ndate -06 $adate`
hha=`echo $adate | cut -c9-10`
hhg=`echo $gdate | cut -c9-10`
prefixa=gfs.t${hha}z
prefixo=gdas1.t${hha}z
suffix=tm00.bufr_d


# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir
rm -rf core*

# Make gsi namelist

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

# parameters for radiance data assimilation
export SETUP="upd_pred=0,"

export USE_GFS_STRATOSPHERE=".true."
export USE_GFS_OZONE=".true."
export REGIONAL_OZONE=".true."

. $scripts/regression_namelists.sh

cat << EOF > gsiparm.anl

$hwrf_nmm_d2_namelist

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

anavinfo=$fixgsi/anavinfo_hwrf_d2
berror=$fixgsi/$endianness/nam_glb_berror.f77.gcv
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
satinfo=$fixgsi/hwrf_satinfo.txt
atmsbeaminfo=$fixgsi/atms_beamwidth.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/nam_global_satangbias.txt
pcpinfo=$fixgsi/nam_global_pcpinfo.txt
ozinfo=$fixgsi/global_ozinfo.txt
errtable=$fixgsi/hwrf_nam_errtable.r3dv
convinfo=$fixgsi/hwrf_convinfo.txt

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ $exp = $hwrf_nmm_d2_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $hwrf_nmm_d2_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ $exp = $hwrf_nmm_d2_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ $exp = $hwrf_nmm_d2_contrl_exp2 ]]; then
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
$ncp $satinfo  ./satinfo
$ncp $scaninfo ./scaninfo
$ncp $pcpinfo  ./pcpinfo
$ncp $ozinfo   ./ozinfo
$ncp $convinfo ./convinfo
$ncp $errtable ./errtable
$ncp $atmsbeaminfo ./atms_beamwidth.txt

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $fixcrtm/${file}.SpcCoeff.bin ./
    $ncp $fixcrtm/${file}.TauCoeff.bin ./
done


# Copy observational data to $tmpdir
$lnsf $hwrf_nmm_obs/${prefixa}.prepbufr           ./prepbufr
$lnsf $hwrf_nmm_obs/${prefixa}.satwnd.${suffix}   ./satwndbufr
$lnsf $hwrf_nmm_obs/${prefixa}.gpsro.${suffix}    ./gpsrobufr
#$lnsf $hwrf_nmm_obs/${prefixa}.spssmi.${suffix}   ./ssmirrbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.sptrmm.${suffix}   ./tmirrbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.gome.${suffix}     ./gomebufr
#$lnsf $hwrf_nmm_obs/${prefixa}.omi.${suffix}      ./omibufr
#$lnsf $hwrf_nmm_obs/${prefixa}.mls.${suffix}      ./mlsbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.osbuv8.${suffix}   ./sbuvbufr
$lnsf $hwrf_nmm_obs/${prefixa}.goesfv.${suffix}   ./gsnd1bufr
$lnsf $hwrf_nmm_obs/${prefixa}.1bamua.${suffix}   ./amsuabufr
#$lnsf $hwrf_nmm_obs/${prefixa}.1bamub.${suffix}   ./amsubbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.1bhrs2.${suffix}   ./hirs2bufr
#$lnsf $hwrf_nmm_obs/${prefixa}.1bhrs3.${suffix}   ./hirs3bufr
$lnsf $hwrf_nmm_obs/${prefixa}.1bhrs4.${suffix}   ./hirs4bufr
$lnsf $hwrf_nmm_obs/${prefixa}.1bmhs.${suffix}    ./mhsbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.1bmsu.${suffix}    ./msubufr
$lnsf $hwrf_nmm_obs/${prefixa}.airsev.${suffix}   ./airsbufr
$lnsf $hwrf_nmm_obs/${prefixa}.sevcsr.${suffix}   ./seviribufr
$lnsf $hwrf_nmm_obs/${prefixa}.mtiasi.${suffix}   ./iasibufr
$lnsf $hwrf_nmm_obs/${prefixa}.esamua.${suffix}   ./amsuabufrears
$lnsf $hwrf_nmm_obs/${prefixa}.esamub.${suffix}   ./amsubbufrears
$lnsf $hwrf_nmm_obs/${prefixa}.eshrs3.${suffix}   ./hirs3bufrears
#$lnsf $hwrf_nmm_obs/${prefixa}.ssmit.${suffix}    ./ssmitbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.amsre.${suffix}    ./amsrebufr
#$lnsf $hwrf_nmm_obs/${prefixa}.ssmis.${suffix}    ./ssmisbufr
$lnsf $hwrf_nmm_obs/${prefixa}.atms.${suffix}     ./atmsbufr
$lnsf $hwrf_nmm_obs/${prefixa}.cris.${suffix}     ./crisbufr
#$lnsf $hwrf_nmm_obs/${prefixa}.syndata.tcvitals.tm00 ./tcvitl
$lnsf $hwrf_nmm_obs/${prefixo}.tldplr.${suffix}  ./tldplrbufr


# Copy bias correction, atmospheric and surface files
$lnsf $hwrf_nmm_obs/gdas1.t${hhg}z.abias             ./satbias_in
$lnsf $hwrf_nmm_obs/gdas1.t${hhg}z.satang            ./satbias_angle

$ncp $hwrf_nmm_ges/wrfghost_d02_03      ./wrf_inou3
$ncp $hwrf_nmm_ges/wrfghost_d02_06      ./wrf_inout
$ncp $hwrf_nmm_ges/wrfghost_d02_09      ./wrf_inou9 

$ncp $hwrf_nmm_ges/gdas1.t${hhg}z.sf03  ./gfs_sigf03
$ncp $hwrf_nmm_ges/gdas1.t${hhg}z.sf06  ./gfs_sigf06
$ncp $hwrf_nmm_ges/gdas1.t${hhg}z.sf09  ./gfs_sigf09

# Copy ensemble forecast files for hybrid analysis
export ENSEMBLE_SIZE_REGIONAL=80
>filelist
n=1
while [[ $n -le ${ENSEMBLE_SIZE_REGIONAL} ]]; do
  $lnsf $hwrf_nmm_ges/$( printf sfg_${gdate}_fhr06s_mem%03d $n )  \
        ./$( printf sfg_${gdate}_fhr06s_mem%03d $n )
  ls ./$( printf sfg_${gdate}_fhr06s_mem%03d $n ) >> filelist
  n=$((n + 1))
done

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

   export MP_USE_BULK_XFER=yes
   export MP_BULK_MIN_MSG_SIZE=64k

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

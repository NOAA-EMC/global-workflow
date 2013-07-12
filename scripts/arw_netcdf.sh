
set -x

# Set analysis date
#adate=$adate_regional

# Set guess/analysis (i/o) file format.  Two
# option are available:  binary or netcdf
##io_format=binary
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

# Set experiment name

if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#gsiexec=$updat

# Set resoltion and other dependent parameters
#export JCAP=62
export LEVS=60
export JCAP_B=$JCAP
if [[ "$io_format" = "binary" ]]; then
   export LEVS=50
elif [[ "$io_format" = "netcdf" ]]; then
   export LEVS=30
fi
export DELTIM=1200

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_arw_netcdf/${exp}
savdir=$savdir/outreg/arw_netcdf/${exp}

# Specify GSI fixed field and data directories.


# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

# Given the analysis date, compute the date from which the
# first guess comes.  Extract cycle and set prefix and suffix
# for guess and observation data files
sdate=`echo $arw_netcdf_adate |cut -c1-8`
odate=`$ndate +6 $arw_netcdf_adate`
hha=`echo $arw_netcdf_adate | cut -c9-10`
hho=`echo $odate | cut -c9-10`
prefixo=ndas.t${hho}z
prefixa=ndas.t${hha}z
suffix=tm06.bufr_d

#datobs=$datobs_arw_netcdf/$adate_regional_arw_netcdf
#datges=$datobs

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir
rm -rf core*

# CO2 namelist and file decisions
ICO2=${ICO2:-0}
if [ $ICO2 -gt 0 ] ; then
	# Copy co2 files to $tmpdir
	co2dir=${CO2DIR:-$fixgsi}
	yyyy=$(echo ${CDATE:-$arw_netcdf_adate}|cut -c1-4)
	rm ./global_co2_data.txt
		co2=$co2dir/global_co2.gcmscl_$yyyy.txt
		if [ -s $co2 ] ; then
			$ncp $co2 ./global_co2_data.txt
		fi
	if [ ! -s ./global_co2_data.txt ] ; then
		echo "\./global_co2_data.txt" not created
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

$arw_netcdf_namelist

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
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=$fixgsi/anavinfo_arw_netcdf
if [[ "$io_format" = "binary" ]]; then
   berror=$fixgsi/$endianness/nam_glb_berror.f77.gcv
elif [[ "$io_format" = "netcdf" ]]; then
   berror=$fixgsi/$endianness/nam_glb_berror.f77.gcv
fi
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
satinfo=$fixgsi/nam_regional_satinfo.txt
scaninfo=$fixgsi/global_scaninfo.txt
satangl=$fixgsi/nam_global_satangbias.txt
atmsbeamdat=$fixgsi/atms_beamwidth.txt
pcpinfo=$fixgsi/nam_global_pcpinfo.txt
ozinfo=$fixgsi/nam_global_ozinfo.txt
errtable=$fixgsi/nam_errtable.r3dv
convinfo=$fixgsi/nam_regional_convinfo.txt
mesonetuselist=$fixgsi/nam_mesonet_uselist.txt

# Only need this file for single obs test
bufrtable=$fixgsi/prepobs_prep.bufrtable

# Only need this file for sst retrieval
bftab_sst=$fixgsi/bufrtab.012

# Copy executable and fixed files to $tmpdir
if [[ "$exp" = $arw_netcdf_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $arw_netcdf_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $arw_netcdf_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ "$exp" = $arw_netcdf_contrl_exp2 ]]; then
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
$ncp $mesonetuselist ./mesonetuselist

$ncp $bufrtable ./prepobs_prep.bufrtable
$ncp $bftab_sst ./bftab_sstphr

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $crtm_coef/${file}.SpcCoeff.bin ./
    $ncp $crtm_coef/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $arw_netcdf_obs/${prefixo}.prepbufr.tm06   ./prepbufr
$ncp $arw_netcdf_obs/${prefixo}.satwnd.$suffix  ./satwnd
$ncp $arw_netcdf_obs/${prefixo}.1bhrs3.$suffix  ./hirs3bufr
$ncp $arw_netcdf_obs/${prefixo}.1bhrs4.$suffix  ./hirs4bufr
$ncp $arw_netcdf_obs/${prefixo}.1bamua.$suffix  ./amsuabufr
$ncp $arw_netcdf_obs/${prefixo}.1bamub.$suffix  ./amsubbufr
$ncp $arw_netcdf_obs/${prefixo}.1bmhs.$suffix   ./mhsbufr
$ncp $arw_netcdf_obs/${prefixo}.goesfv.$suffix  ./gsnd1bufr
$ncp $arw_netcdf_obs/${prefixo}.airsev.$suffix  ./airsbufr
$ncp $arw_netcdf_obs/${prefixo}.radwnd.$suffix  ./radarbufr
if [[ "$io_format" = "netcdf" ]]; then
   $ncp $arw_netcdf_obs/${prefixo}.nexrad.$suffix  ./l2rwbufr
fi

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $arw_netcdf_obs/${prefixo}.satbias.tm06      ./satbias_in
$ncp $arw_netcdf_obs/${prefixo}.satang.tm06        ./satbias_angle
if [[ "$io_format" = "binary" ]]; then
   $ncp $arw_netcdf_ges/wrfinput_d01_arw_binary        ./wrf_inout
elif [[ "$io_format" = "netcdf" ]]; then
   $ncp $arw_netcdf_ges/wrfout_d01_2008-05-11_12:00:00 ./wrf_inout
fi
cp wrf_inout wrf_ges

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

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
   poe $tmpdir/gsi.x < gsiparm.anl > stdout

fi

rc=$?

exit

# Save output
mkdir -p $savdir
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${arw_netcdf_adate}
$ncp wrf_inout       $savdir/wrfanl.${arw_netcdf_adate}
$ncp satbias_out     $savdir/biascr.${arw_netcdf_adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${arw_netcdf_adate}

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

cd $tmpdir
loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

# Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g10 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${arw_netcdf_adate}
         compress diag_${type}_${string}.${arw_netcdf_adate}
         $ncp diag_${type}_${string}.${arw_netcdf_adate}.Z $savdir/
      fi
   done
done

exit


set -x

# Set analysis date
#adate=$adate_regional_nems_nmmb

# Set experiment name

if [[ "$arch" = "Linux" ]]; then

   exp=$jobname

elif [[ "$arch" = "AIX" ]]; then

   exp=$LOADL_JOB_NAME

fi

# Set path/file for gsi executable
#gsiexec=$cntrl

# Set resoltion and other dependent parameters
#export JCAP=62
export LEVS=60
export JCAP_B=$JCAP
export LEVS=60
export DELTIM=1200

# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_nems_nmmb/${exp}
savdir=$savdir/outreg_nems_nmmb/${exp}

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ndate is a date manipulation utility
#   ncp is cp replacement, currently keep as /bin/cp

CLEAN=NO
#ndate=/nwprod/util/exec/ndate
ncp=/bin/cp

## Given the analysis date, compute the date from which the
## first guess comes.  Extract cycle and set prefix and suffix
## for guess and observation data files
#sdate=`echo $adate |cut -c1-8`
#odate=`$ndate +12 $adate`
#hha=`echo $adate | cut -c9-10`
#hho=`echo $odate | cut -c9-10`
#prefixo=ndas.t${hho}z
#prefixa=ndas.t${hha}z
#suffix=tm12.bufr_d

#datobs=$datobs
#datges=$datges

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
        yyyy=$(echo ${CDATE:-$nmmb_nems_adate}|cut -c1-4)
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

$nems_nmmb_namelist

EOF

anavinfo=$fixgsi/anavinfo_nems_nmmb
berror=$fixgsi/$endianness/nam_glb_berror.f77.gcv
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
stnuselist=$fixgsi/nam_mesonet_stnuselist.txt

# Copy executable and fixed files to $tmpdir
if [[ "$exp" = $nmmb_nems_updat_exp1 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $nmmb_nems_updat_exp2 ]]; then
   $ncp $gsiexec_updat ./gsi.x
elif [[ "$exp" = $nmmb_nems_contrl_exp1 ]]; then
   $ncp $gsiexec_contrl ./gsi.x
elif [[ "$exp" = $nmmb_nems_contrl_exp2 ]]; then
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
$ncp $stnuselist ./mesonet_stnuselist   

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
    $ncp $crtm_coef/${file}.SpcCoeff.bin ./
    $ncp $crtm_coef/${file}.TauCoeff.bin ./
done

# Copy observational data to $tmpdir
$ncp $nmmb_nems_obs/ndas.t12z.prepbufr.tm12      ./prepbufr
$ncp $nmmb_nems_obs/ndas.t12z.satwnd.tm12.bufr_d ./satwndbufr
$ncp $nmmb_nems_obs/ndas.t12z.1bhrs3.tm12.bufr_d ./hirs3bufr
$ncp $nmmb_nems_obs/ndas.t12z.1bhrs4.tm12.bufr_d ./hirs4bufr
$ncp $nmmb_nems_obs/ndas.t12z.1bamua.tm12.bufr_d ./amsuabufr
$ncp $nmmb_nems_obs/ndas.t12z.1bamub.tm12.bufr_d ./amsubbufr
$ncp $nmmb_nems_obs/ndas.t12z.1bmhs.tm12.bufr_d  ./mhsbufr
$ncp $nmmb_nems_obs/ndas.t12z.goesfv.tm12.bufr_d ./gsnd1bufr
$ncp $nmmb_nems_obs/ndas.t12z.airsev.tm12.bufr_d ./airsbufr
$ncp $nmmb_nems_obs/ndas.t12z.radwnd.tm12.bufr_d ./radarbufr
$ncp $nmmb_nems_obs/gdas1.t00z.osbuv8.tm00.bufr_d ./sbuvbufr
$ncp $nmmb_nems_obs/gdas1.t00z.mls.tm00.bufr_d ./mlsbufr

# Copy bias correction, sigma, and surface files
#
#  *** NOTE:  The regional gsi analysis is written to (over)
#             the input guess field file (wrf_inout)
#
$ncp $nmmb_nems_ges/ndas.t06z.satang.tm09 ./satbias_angle
$ncp $nmmb_nems_ges/ndas.t06z.satbias.tm09 ./satbias_in

$ncp $nmmb_nems_ges/nmm_b_history_nemsio.012 ./wrf_inout
$ncp $nmmb_nems_ges/nmm_b_history_nemsio.012.ctl ./wrf_inout.ctl
if [[ "$endianness" = "Big_Endian" ]]; then
   $ncp $nmmb_nems_ges/gdas1.t00z.sgesprep     ./gfs_sigf03
elif [[ "$endianness" = "Little_Endian" ]]; then
   $ncp $nmmb_nems_ges/gdas1.t00z.sgesprep.le  ./gfs_sigf03
fi
$ncp wrf_inout wrf_ges
$ncp wrf_inout.ctl wrf_ges.ctl

# Run gsi under Parallel Operating Environment (poe) on NCEP IBM
if [[ "$arch" = "Linux" ]]; then
   cd $tmpdir/
   echo "run gsi now"

   export MPI_BUFS_PER_PROC=256
   export MPI_BUFS_PER_HOST=256
   export MPI_GROUP_MAX=256
   #export OMP_NUM_THREADS=1

   #export module="/usr/bin/modulecmd sh"

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
chgrp rstprod $savdir
chmod 750 $savdir

cat stdout fort.2* > $savdir/stdout.anl.${nmmb_nems_adate}
$ncp wrf_inout       $savdir/wrfanl.${nmmb_nems_adate}
$ncp satbias_out     $savdir/biascr.${nmmb_nems_adate}

# If desired, copy guess file to unique filename in $savdir
$ncp wrf_ges         $savdir/wrfges.${nmmb_nems_adate}

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
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g10 sndr_g12 sndr_g08_prep sndr_g10_prep sndr_g12_prep sndrd1_g08 sndrd2_g08 sndrd3_g08 sndrd4_g08 sndrd1_g10 sndrd2_g10 sndrd3_g10 sndrd4_g10 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g10 imgr_g12 pcp_ssmi_dmsp
pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 amsua_n18 mhs_n18 amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a"
   for type in $listall; do
      count=`ls dir.*/${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         cat dir.*/${type}_${loop}* > diag_${type}_${string}.${nmmb_nems_adate}
         compress diag_${type}_${string}.${nmmb_nems_adate}
         $ncp diag_${type}_${string}.${nmmb_nems_adate}.Z $savdir/
      fi
   done
done

exit


set -x

# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp
#
# Set experiment name
#
exp=$jobname

#-----------------------------------------------------------------------
#
# Extract from ADATE the starting year, month, day, and hour of the
# forecast.  These are needed below for various operations.
#
#-----------------------------------------------------------------------
#

adate=${rrfs_enkf_adate}
YYYYMMDDHH=$(date +%Y%m%d%H -d "${adate:0:8} ${adate:8:2}")
JJJ=$(date +%j -d "${adate:0:8} ${adate:8:2}")

YYYY=${YYYYMMDDHH:0:4}
MM=${YYYYMMDDHH:4:2}
DD=${YYYYMMDDHH:6:2}
HH=${YYYYMMDDHH:8:2}
YYYYMMDD=${YYYYMMDDHH:0:8}

#
#-----------------------------------------------------------------------
#
# go to working directory and save directory.
# define fix and background path
#
#-----------------------------------------------------------------------
# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_rrfs_enkf_conv/${exp}
savdir=$savdir/outreg_rrfs_enkf_conv/${exp}

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chmod 750 $tmpdir
cd $tmpdir

fixcrtm=${fixcrtm:-$CRTM_FIX}

cp ${rrfs_3denvar_rdasens_ges}/fv3_coupler.res    coupler.res
cp ${rrfs_3denvar_rdasens_ges}/fv3_akbk           fv3sar_tile1_akbk.nc
cp ${rrfs_3denvar_rdasens_ges}/fv3_grid_spec      fv3sar_tile1_grid_spec.nc

#
#-----------------------------------------------------------------------
#
# Loop through the members, link the background and copy over
#  observer output (diag*ges*) files to the running directory
#
#-----------------------------------------------------------------------
#
ob_type="conv"
DO_ENS_RADDA="false"
nens=${nens:-5}
netcdf_diag=".true."
for imem in  $(seq 1 $nens) ensmean; do

  if [ "${imem}" = "ensmean" ]; then
    memchar="ensmean"
    memcharv0="ensmean"
    restart_prefix=""
  else
    memchar="mem"$(printf %04i $imem)
    memcharv0="mem"$(printf %03i $imem)
    restart_prefix="${YYYYMMDD}.${HH}0000."
  fi
  slash_ensmem_subdir=$memchar
  bkpath=${rrfs_enkf_ges}/${slash_ensmem_subdir}/fcst_fv3lam/RESTART
  observer_nwges_dir="${rrfs_enkf_diag}/${slash_ensmem_subdir}/observer_gsi"

  cp  ${bkpath}/${restart_prefix}fv_core.res.tile1.nc      fv3sar_tile1_${memcharv0}_dynvars
  cp  ${bkpath}/${restart_prefix}fv_tracer.res.tile1.nc    fv3sar_tile1_${memcharv0}_tracer
  cp  ${bkpath}/${restart_prefix}sfc_data.nc               fv3sar_tile1_${memcharv0}_sfcdata
  cp  ${bkpath}/${restart_prefix}phy_data.nc               fv3sar_tile1_${memcharv0}_phyvar

  #
#-----------------------------------------------------------------------
#
# Copy observer outputs (diag*ges*) to the working directory
#
#-----------------------------------------------------------------------
#
  if [ "${netcdf_diag}" = ".true." ] ; then
    # Note, listall_rad is copied from exrrfs_run_analysis.sh
    listall_rad="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsua_n18 amsua_n19 amsua_metop-a amsua_metop-b amsua_metop-c amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a mhs_n18 mhs_n19 mhs_metop-a mhs_metop-b mhs_metop-c amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a iasi_metop-b iasi_metop-c seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp atms_npp ssmis_f17 cris-fsr_npp cris-fsr_n20 atms_n20 abi_g16"


    if [ "${ob_type}" = "conv" ]; then
      list_ob_type="conv_ps conv_q conv_t conv_uv conv_pw conv_rw conv_sst"

      if [ "${DO_ENS_RADDA}" = "TRUE" ]; then
        list_ob_type="$list_ob_type $listall_rad"
      fi
    fi

    if [ "${ob_type}" = "radardbz" ]; then
      if [ ${DO_GLM_FED_DA} == "TRUE" ]; then
        list_ob_type="conv_dbz conv_fed"
      else
        list_ob_type="conv_dbz"
      fi
    fi
    for sub_ob_type in ${list_ob_type} ; do
      diagfile0=${observer_nwges_dir}/diag_${sub_ob_type}_ges.${YYYYMMDDHH}.nc4.gz
      if [ -s $diagfile0 ]; then
        diagfile=$(basename  $diagfile0)
        cp  $diagfile0  $diagfile
        gzip -d $diagfile && rm -f $diagfile
        ncfile0=$(basename -s .gz $diagfile)
        ncfile=$(basename -s .nc4 $ncfile0)
        mv $ncfile0 ${ncfile}_${memcharv0}.nc4
      fi
    done
  else
    for diagfile0 in $(ls  ${observer_nwges_dir}/diag*${ob_type}*ges* ) ; do
      if [ -s $diagfile0 ]; then
         diagfile=$(basename  $diagfile0)
         cp  $diagfile0   diag_conv_ges.$memcharv0
      fi
    done
  fi
done

#
#-----------------------------------------------------------------------
#
# Set GSI fix files
#
#----------------------------------------------------------------------
#
found_ob_type=0

CONVINFO=${fixgsi}/convinfo.rrfs

if [ "${ob_type}" = "conv" ]; then
  ANAVINFO=${fixgsi}/anavinfo.rrfs
  found_ob_type=1
fi
if [ "${ob_type}" = "radardbz" ]; then
  ANAVINFO=${fixgsi}/anavinfo.enkf.rrfs_dbz
  CORRLENGTH="18"
  LNSIGCUTOFF="0.5"
  found_ob_type=1
fi
if [ ${found_ob_type} == 0 ]; then
  err_exit "Unknown observation type: ${ob_type}"
fi
stdout_name=stdout.${ob_type}
stderr_name=stderr.${ob_type}

SATINFO=${fixgsi}/global_satinfo.txt
OZINFO=${fixgsi}/global_ozinfo.txt

cp ${ANAVINFO} anavinfo
cp $SATINFO    satinfo
cp $CONVINFO   convinfo
cp $OZINFO     ozinfo

#
#-----------------------------------------------------------------------
#
# Get nlons (NX_RES), nlats (NY_RES) and nlevs
#
#-----------------------------------------------------------------------
#
NX_RES=$(ncdump -h fv3sar_tile1_grid_spec.nc | grep "grid_xt =" | cut -f3 -d" " )
NY_RES=$(ncdump -h fv3sar_tile1_grid_spec.nc | grep "grid_yt =" | cut -f3 -d" " )
nlevs=$(ncdump -h fv3sar_tile1_mem001_tracer | grep "zaxis_1 =" | cut -f3 -d" " )
#
#----------------------------------------------------------------------
#
# Set namelist parameters for EnKF
#
#----------------------------------------------------------------------
#
EnKFTracerVars=${EnKFTracerVar:-"sphum,o3mr"}
ldo_enscalc_option=${ldo_enscalc_option:-0}

# Make gsi namelist

. $scripts/regression_namelists.sh rrfs_enkf_conv

#

cat << EOF > enkf.nml

$gsi_namelist

EOF

#
#-----------------------------------------------------------------------
#
# Run the EnKF
#
#-----------------------------------------------------------------------
#
# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $enkfexec_updat  ./enkf.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $enkfexec_contrl ./enkf.x
fi

# Run enkf
cd $tmpdir
echo "run rrfs enkf now"
eval "$APRUN $tmpdir/enkf.x < enkf.nml > stdout 2>&1"
rc=$?
exit $rc

#! /usr/bin/env bash

################################################################################
# exglobal_land_analysis_run.sh
#
# This script runs a global land letkfoi analysis with FV3-JEDI.
# It assumes the runtime directory has already been staged with the appropriate
# input files and YAML configuration (by the initialize script) before execution.
#
################################################################################
#  Utilities
RES=${CASE:1}

#  Directories.
export gdasapp_dir=${gdasapp_dir:-"${HOMEgfs}/sorc/gdas.cd/"}
BKGDIR=${DATA}/bkg
ANLDIR=${DATA}/anl

#  Filenames
export CREATEENS=${CREATEENS:-"${HOMEgfs}/sorc/gdas.cd/ush/land/letkf_create_ens.py"}
export IMS_IODA=${IMS_IODA:-"${HOMEgfs}/sorc/gdas.cd/build/bin/imsfv3_scf2ioda.py"}
export CALCFIMS=${CALCFIMS:-"${HOMEgfs}/sorc/gdas.cd/build/bin/calcfIMS.exe"}
export ADDJEDIINC=${ADDJEDIINC:-"${HOMEgfs}/sorc/gdas.cd/build/bin/apply_incr.exe"}

export OROGPATH="${HOMEgfs}/fix/orog/C${RES}"
export OROGTYPE="C${RES}_oro_data"
export FRACGRID=${FRACGRID:-"NO"}
FILEDATE=${PDY}.${cyc}0000
DOY=$(date +%j -d "${PDY} + 1 day")
YYYY=${PDY:0:4}
################################################################################
# Create ensemble member
WORKDIR=${BKGDIR}
BERR_STD=30  # background error std for LETKFOI

if [[ ${FRACGRID} == "YES" ]]; then
    SNOWDEPTHVAR="snodl"
else
    SNOWDEPTHVAR="snwdph"
fi

# FOR LETKFOI, CREATE THE PSEUDO-ENSEMBLE
cd "${WORKDIR}" || exit 99
nmem_land=2
ntiles=6
for ens in $(seq -f %03g 1 ${nmem_land}); do
    if [[ -e "${WORKDIR}/mem${ens}" ]]; then
            rm -rf "${WORKDIR}/mem${ens}"
    fi
    mkdir -p "${WORKDIR}/mem${ens}"
    for tile in $(seq 1 ${ntiles}); do
        ${NCP} "${WORKDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"  "${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc"
    done
    ${NCP} "${WORKDIR}/${FILEDATE}.coupler.res" "${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res"
done

echo 'do_landDA: calling create ensemble'

python "${CREATEENS}" "${FILEDATE}" "${SNOWDEPTHVAR}" "${BERR_STD}" "${WORKDIR}"

for ens in $(seq -f %03g 1 ${nmem_land}); do
    mkdir -p "${WORKDIR}/mem${ens}/RESTART"
    for tile in $(seq 1 ${ntiles}); do
      ${NMV} "${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}/mem${ens}/RESTART/"
    done
    ${NMV} "${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res" "${WORKDIR}/mem${ens}/RESTART/"
done
################################################################################
# IMS proc
if [[ ${cyc} == "18" ]]; then
  WORKDIR=${DATA}/obs
  cd "${WORKDIR}" || exit 99
  if [[ -e fims.nml ]]; then
    rm fims.nml
  fi

cat >> fims.nml << EOF
 &fIMS_nml
  idim=${RES}, jdim=${RES},
  otype=${OROGTYPE},
  jdate=${YYYY}${DOY},
  yyyymmddhh=${PDY}.${cyc},
  imsformat=2,
  imsversion=1.3,
  IMS_OBS_PATH="${WORKDIR}/",
  IMS_IND_PATH="${WORKDIR}/"
  /
EOF

  # stage restarts
  for tile in $(seq 1 ${ntiles}); do
    if [[ ! -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
      ${NCP} "${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}"
    fi
  done

  # stage observations
  ${NCP} "${COMIN_OBS}/gdas.t${cyc}z.ims${YYYY}${DOY}_4km_v1.3.nc" "${WORKDIR}/ims${YYYY}${DOY}_4km_v1.3.nc"
  ${NCP} "${COMIN_OBS}/gdas.t${cyc}z.IMS4km_to_FV3_mapping.C${RES}_oro_data.nc" "${WORKDIR}/IMS4km_to_FV3_mapping.C${RES}_oro_data.nc"

  ${CALCFIMS}

##  export PYTHONPATH=${PYTHONPATH}:${gdasapp_dir}/build/lib/python${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}/pyioda/
  export PYTHONPATH=${PYTHONPATH}:${gdasapp_dir}/iodaconv/src/:${gdasapp_dir}/build/lib/python3.7/pyioda/

  echo "PYTHONPATH: ${PYTHONPATH}"
  echo 'do_landDA: calling ioda converter'
  python "${IMS_IODA}" -i "IMSscf.${PDY}.${OROGTYPE}.nc" -o "ioda.IMSscf.${PDY}.${OROGTYPE}.nc"

  if [[ -e "ioda.IMSscf.${PDY}.C${RES}_oro_data.nc"  ]]; then
    ${NCP} "ioda.IMSscf.${PDY}.C${RES}_oro_data.nc" "gdas.t${cyc}z.ims_snow_${PDY}${cyc}.nc4"
  fi

fi
################################################################################
# run executable
export pgm=${JEDIEXE}
. prep_step
${APRUN_LANDANL} "${DATA}/fv3jedi_letkf.x" "${DATA}/${CDUMP}.t${cyc}z.letkfoi.yaml" 1>&1 2>&2
################################################################################
# add jedi increment
WORKDIR=${ANLDIR}
cd "${WORKDIR}" || exit 99

if [[ -e apply_incr_nml ]]; then
  rm apply_incr_nml
fi

cat << EOF > apply_incr_nml
&noahmp_snow
 date_str=${PDY}
 hour_str=${cyc}
 res=${RES}
 frac_grid=${FRACGRID}
 orog_path="${OROGPATH}"
 otype="${OROGTYPE}"
/
EOF

for tile in $(seq 1 ${ntiles}); do
# stage restarts
  if [[ ! -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
    ${NCP} "${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"  "${WORKDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"
  fi

# change names of the increments
  if [[ ! -e ${FILEDATE}.xainc.sfc_data.tile${tile}.nc ]]; then
    ${NCP} "landinc.${FILEDATE}.sfc_data.tile${tile}.nc" "${FILEDATE}.xainc.sfc_data.tile${tile}.nc"
  fi
done

echo 'do_landDA: calling apply snow increment'

# (n=6) -> this is fixed, at one task per tile (with minor code change, could run on a single proc).
${APRUN_LANDANL} "${ADDJEDIINC}" "${WORKDIR}/apply_incr.log" 1>&1 2>&2

export err=$?; err_chk
exit "${err}"


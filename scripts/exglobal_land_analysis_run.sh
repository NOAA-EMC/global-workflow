#!/bin/bash
################################################################################
# exglobal_land_analysis_run.sh
#
# This script runs a global land letkfoi analysis with FV3-JEDI.
# It assumes the runtime directory has already been staged with the appropriate
# input files and YAML configuration (by the initialize script) before execution.
#
################################################################################
#  Utilities
CASE=${CASE:-C96}
RES=$(echo "${CASE}" | cut -c 2-)
export gdasapp_dir=${gdasapp_dir:-"${HOMEgfs}/sorc/gdas.cd/"}
export CREATEENS=${CREATEENS:-"${HOMEgfs}/sorc/gdas.cd/ush/land/letkf_create_ens.py"}
export IMS_IODA=${IMS_IODA:-"${HOMEgfs}/sorc/gdas.cd/build/bin/imsfv3_scf2ioda.py"}
export CALCFIMS=${CALCFIMS:-"${HOMEgfs}/sorc/gdas.cd/build/bin/calcfIMS.exe"}
export ADDJEDIINC=${ADDJEDIINC:-"${HOMEgfs}/sorc/gdas.cd/build/bin/apply_incr.exe"}
#export TPATH="/scratch2/NCEPDEV/land/data/fix/C${RES}/"
#export TPATH=${FIXgfs:-"${HOMEgfs}/fix/orog/C${RES}"}
export TPATH="${HOMEgfs}/fix/orog/C${RES}"
export TSTUB="C${RES}_oro_data"
export GFSv17=${GFSv17:-"NO"}
BKGDIR=${DATA}/bkg
ANLDIR=${DATA}/anl
FILEDATE=${PDY}.${cyc}0000
DOY=$(date +%j -d "${PDY} + 1 day")
YYYY=$(echo "${PDY}" | cut -c1-4)
################################################################################
# Create ensemble member
WORKDIR=${BKGDIR}
B=30  # background error std for LETKFOI

if [[ ${GFSv17} == "YES" ]]; then
    SNOWDEPTHVAR="snodl"
else
    SNOWDEPTHVAR="snwdph"
fi

echo "DONG:GFSv17: $GFSv17"
echo "DONG:SNOWDEPTHVAR: $SNOWDEPTHVAR"
# FOR LETKFOI, CREATE THE PSEUDO-ENSEMBLE
cd "${WORKDIR}" || exit
for ens in 001 002
do
    if [[ -e "${WORKDIR}/mem${ens}" ]]; then
            rm -rf "${WORKDIR}/mem${ens}"
    fi
    mkdir -p "${WORKDIR}/mem${ens}"
    for tile in 1 2 3 4 5 6
    do
        cp "${WORKDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"  "${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc"
    done
    cp "${WORKDIR}/${FILEDATE}.coupler.res" "${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res"
done

echo 'do_landDA: calling create ensemble'

python "${CREATEENS}" "${FILEDATE}" "${SNOWDEPTHVAR}" "${B}" "${WORKDIR}"

for ens in 001 002
do
    mkdir -p "${WORKDIR}/mem${ens}/RESTART"
    for tile in 1 2 3 4 5 6
    do
      mv "${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}/mem${ens}/RESTART/"
    done
    mv "${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res" "${WORKDIR}/mem${ens}/RESTART/"
done
################################################################################
# IMS proc
if [[ ${cyc} == "18" ]]; then
  WORKDIR=${DATA}/obs
  cd "${WORKDIR}" || exit
  if [[ -e fims.nml ]]; then
    rm fims.nml
  fi

cat >> fims.nml << EOF
 &fIMS_nml
  idim=${RES}, jdim=${RES},
  otype=${TSTUB},
  jdate=${YYYY}${DOY},
  yyyymmddhh=${PDY}.${cyc},
  imsformat=2,
  imsversion=1.3,
  IMS_OBS_PATH="${WORKDIR}/",
  IMS_IND_PATH="${WORKDIR}/"
  /
EOF

  # stage restarts
  for tile in 1 2 3 4 5 6
  do
    if [[ ! -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
      cp "${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}"
    fi
  done

  # stage observations
  cp "${COMIN_OBS}/gdas.t${cyc}z.ims${YYYY}${DOY}_4km_v1.3.nc" "${WORKDIR}/ims${YYYY}${DOY}_4km_v1.3.nc"
  cp "${COMIN_OBS}/gdas.t${cyc}z.IMS4km_to_FV3_mapping.C${RES}_oro_data.nc" "${WORKDIR}/IMS4km_to_FV3_mapping.C${RES}_oro_data.nc"

  ulimit -Ss unlimited
  ${CALCFIMS}

##  export PYTHONPATH=${PYTHONPATH}:${gdasapp_dir}/build/lib/python${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}/pyioda/
  export PYTHONPATH=${PYTHONPATH}:${gdasapp_dir}/iodaconv/src/:${gdasapp_dir}/build/lib/python3.7/pyioda/

  echo "PYTHONPATH: ${PYTHONPATH}"
  echo 'do_landDA: calling ioda converter'
  python "${IMS_IODA}" -i "IMSscf.${PDY}.${TSTUB}.nc" -o "ioda.IMSscf.${PDY}.${TSTUB}.nc"

  if [[ -e "ioda.IMSscf.${PDY}.C${RES}_oro_data.nc"  ]]; then
    cp "ioda.IMSscf.${PDY}.C${RES}_oro_data.nc" "gdas.t${cyc}z.ims_snow_${PDY}${cyc}.nc4"
  fi

fi
################################################################################
# run executable
export pgm=${JEDIVAREXE}
. prep_step
${APRUN_LANDANL} "${DATA}/fv3jedi_letkf.x" "${DATA}/${CDUMP}.t${cyc}z.landoi.yaml" 1>&1 2>&2
################################################################################
# add jedi increment
WORKDIR=${ANLDIR}
cd "${WORKDIR}" || exit

if [[ -e apply_incr_nml ]]; then
  rm apply_incr_nml
fi

cat << EOF > apply_incr_nml
&noahmp_snow
 date_str=${PDY}
 hour_str=${cyc}
 res=${RES}
 frac_grid=${GFSv17}
 orog_path="${TPATH}"
 otype="${TSTUB}"
/
EOF

# stage restarts
for tile in 1 2 3 4 5 6
do
  if [[ ! -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
    cp "${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}"
  fi
done

# change names of the increments
for tile in 1 2 3 4 5 6
do
  if [[ ! -e ${FILEDATE}.xainc.sfc_data.tile${tile}.nc ]]; then
    cp landinc.${FILEDATE}.sfc_data.tile${tile}.nc ${FILEDATE}.xainc.sfc_data.tile${tile}.nc
  fi
done

echo 'do_landDA: calling apply snow increment'

# (n=6) -> this is fixed, at one task per tile (with minor code change, could run on a single proc).
${APRUN_LANDANL} "${ADDJEDIINC}" "${WORKDIR}/apply_incr.log" 1>&1 2>&2

# change names of the analysis
for tile in 1 2 3 4 5 6
do
  if [[ -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
    mv ${FILEDATE}.sfc_data.tile${tile}.nc ${FILEDATE}.sfcanl_data.tile${tile}.nc
  fi
done

export err=$?; err_chk
exit "${err}"


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
export ADDJEDIINC=${ADDJEDIINC:-"${HOMEgfs}/sorc/gdas.cd/build/bin/apply_incr.exe"}

export OROGPATH="${HOMEgfs}/fix/orog/C${RES}"
export OROGTYPE="C${RES}_oro_data"
export FRACGRID=${FRACGRID:-"NO"}
FILEDATE=${PDY}.${cyc}0000
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
for ens in $(seq -f %03g 1 "${nmem_land}"); do
    if [[ -e "${WORKDIR}/mem${ens}" ]]; then
            rm -rf "${WORKDIR}/mem${ens}"
    fi
    mkdir -p "${WORKDIR}/mem${ens}"
    for tile in $(seq 1 "${ntiles}"); do
        ${NCP} "${WORKDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"  "${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc"
    done
    ${NCP} "${WORKDIR}/${FILEDATE}.coupler.res" "${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res"
done

echo 'do_landDA: calling create ensemble'

python "${CREATEENS}" "${FILEDATE}" "${SNOWDEPTHVAR}" "${BERR_STD}" "${WORKDIR}"

for ens in $(seq -f %03g 1 "${nmem_land}"); do
    mkdir -p "${WORKDIR}/mem${ens}/RESTART"
    for tile in $(seq 1 "${ntiles}"); do
      ${NMV} "${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}/mem${ens}/RESTART/"
    done
    ${NMV} "${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res" "${WORKDIR}/mem${ens}/RESTART/"
done
################################################################################
################################################################################
# run executable
export pgm=${JEDIEXE}
. prep_step
${APRUN_LANDANL} "${DATA}/fv3jedi_letkf.x" "${DATA}/${CDUMP}.t${cyc}z.letkfoi.yaml" 1>&1 2>&2

cd "${ANLDIR}" || exit 99
# change names of the increments
for tile in $(seq 1 "${ntiles}"); do
  if [[ -e "landinc.${FILEDATE}.sfc_data.tile${tile}.nc" ]]; then
    ${NMV} "landinc.${FILEDATE}.sfc_data.tile${tile}.nc" "${FILEDATE}.xainc.sfc_data.tile${tile}.nc"
  fi
done
if [[ -e "landinc.${FILEDATE}.coupler.res" ]]; then
  ${NMV} "landinc.${FILEDATE}.coupler.res" "${FILEDATE}.xainc.coupler.res"
fi
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

# stage restarts
for tile in $(seq 1 "${ntiles}"); do
  if [[ ! -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
    ${NCP} "${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"  "${WORKDIR}/${FILEDATE}.sfc_data.tile${tile}.nc"
  fi
done

echo 'do_landDA: calling apply snow increment'

# (n=6) -> this is fixed, at one task per tile (with minor code change, could run on a single proc).
${APRUN_LANDANL} "${ADDJEDIINC}" "${WORKDIR}/apply_incr.log" 1>&1 2>&2

export err=$?; err_chk
exit "${err}"


#! /usr/bin/env bash

################################################################################
# exglobal_land_analysis_run.sh
#
# This script runs a global land letkfoi analysis with FV3-JEDI.
# It assumes the runtime directory has already been staged with the appropriate
# input files and YAML configuration (by the initialize script) before execution.
#
################################################################################
#  Filenames
CREATEENS="${HOMEgfs}/ush/letkf_create_ens.py"

FRACGRID="NO"
FILEDATE=${PDY}.${cyc}0000
################################################################################
# Create ensemble member
BERR_STD=30  # background error std for LETKFOI

if [[ ${FRACGRID} == "YES" ]]; then
    SNOWDEPTHVAR="snodl"
else
    SNOWDEPTHVAR="snwdph"
fi

# FOR LETKFOI, CREATE THE PSEUDO-ENSEMBLE
cd "${DATA}/bkg" || exit 99
nmem_land=2
ntiles=6
for ens in $(seq -f %03g 1 "${nmem_land}"); do
    if [[ -e "${DATA}/bkg/mem${ens}" ]]; then
            rm -rf "${DATA}/bkg/mem${ens}"
    fi
    mkdir -p "${DATA}/bkg/mem${ens}"
    for tile in $(seq 1 "${ntiles}"); do
        ${NCP} "${DATA}/bkg/${FILEDATE}.sfc_data.tile${tile}.nc"  "${DATA}/bkg/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc"
    done
    ${NCP} "${DATA}/bkg/${FILEDATE}.coupler.res" "${DATA}/bkg/mem${ens}/${FILEDATE}.coupler.res"
done

echo 'do_landDA: calling create ensemble'

python "${CREATEENS}" "${FILEDATE}" "${SNOWDEPTHVAR}" "${BERR_STD}" "${DATA}/bkg"

for ens in $(seq -f %03g 1 "${nmem_land}"); do
    mkdir -p "${DATA}/bkg/mem${ens}/RESTART"
    for tile in $(seq 1 "${ntiles}"); do
      ${NMV} "${DATA}/bkg/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc" "${DATA}/bkg/mem${ens}/RESTART/"
    done
    ${NMV} "${DATA}/bkg/mem${ens}/${FILEDATE}.coupler.res" "${DATA}/bkg/mem${ens}/RESTART/"
done
################################################################################
################################################################################
# run executable
export pgm=${JEDIEXE}
. prep_step
${APRUN_LANDANL} "${DATA}/fv3jedi_letkf.x" "${DATA}/${CDUMP}.t${cyc}z.letkfoi.yaml" 1>&1 2>&2

cd "${DATA}/anl" || exit 99
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
################################################################################

export err=$?; err_chk
exit "${err}"


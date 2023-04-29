#! /usr/bin/env bash

################################################################################
# exglobal_land_analysis_prep.sh
#
# This script processes the IMS input ascii/netcdf files onto the UFS model grid.
# Inputs: IMS ascii/netcdf file, IMS index file (is model resolution specific) 
#         and model restart files.
# Outputs: file with i) IMS-derived snow cover fraction over land on UFS model 
#         grid, and ii) snow depth derived form the IMS snow cover fraction, 
#         using an inversion of the noah model snow depletion curve.
#
################################################################################
#  Utilities
RES=${CASE:1}

#  Directories.
export gdasapp_dir=${gdasapp_dir:-"${HOMEgfs}/sorc/gdas.cd/"}
BKGDIR=${DATA}/bkg

#  Filenames
export IMS_IODA=${IMS_IODA:-"${HOMEgfs}/sorc/gdas.cd/build/bin/imsfv3_scf2ioda.py"}
export CALCFIMS=${CALCFIMS:-"${HOMEgfs}/sorc/gdas.cd/build/bin/calcfIMS.exe"}

export OROGTYPE="C${RES}_oro_data"
FILEDATE=${PDY}.${cyc}0000
DOY=$(date +%j -d "${PDY} + 1 day")
YYYY=${PDY:0:4}
ntiles=6
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
  for tile in $(seq 1 "${ntiles}"); do
    if [[ ! -e ${FILEDATE}.sfc_data.tile${tile}.nc ]]; then
      ${NCP} "${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc" "${WORKDIR}"
    fi
  done

  # stage observations
  ${NCP} "${COM_OBS}/gdas.t${cyc}z.ims${YYYY}${DOY}_4km_v1.3.nc" "${WORKDIR}/ims${YYYY}${DOY}_4km_v1.3.nc"
  ${NCP} "${COM_OBS}/gdas.t${cyc}z.IMS4km_to_FV3_mapping.C${RES}_oro_data.nc" "${WORKDIR}/IMS4km_to_FV3_mapping.C${RES}_oro_data.nc"

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

export err=$?; err_chk
exit "${err}"


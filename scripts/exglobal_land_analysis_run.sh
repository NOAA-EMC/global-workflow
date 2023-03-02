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
RES=$(echo $CASE | cut -c 2-)
export CREATEENS=${CREATEENS:-"${HOMEgfs}/sorc/gdas.cd/ush/land/letkf_create_ens.py"}
export IMS_IODA=${IMS_IODA:-"${HOMEgfs}/sorc/gdas.cd/build/bin/imsfv3_scf2ioda.py"}
export CALCFIMS=${CALCFIMS:-"${HOMEgfs}/sorc/gdas.cd/build/bin/calcfIMS.exe"}
#export TPATH="/scratch2/NCEPDEV/land/data/fix/C${RES}/"
export TPATH=${FIXgfs:-"${HOMEgfs}/fix/orog/C${RES}"}
export TSTUB="C${RES}_oro_data"
export GFSv17=${GFSv17:-"NO"}
BKGDIR=${DATA}/bkg
FILEDATE=${PDY}.${cyc}0000
DOY=$(date +%j -d "${PDY} + 1 day")
YYYY=`echo ${PDY} | cut -c1-4`
################################################################################
# Create ensemble member
WORKDIR=${BKGDIR}
B=30  # background error std for LETKFOI

if [ $GFSv17 == "YES" ]; then
    SNOWDEPTHVAR="snodl"
else
    SNOWDEPTHVAR="snwdph"
fi

# FOR LETKFOI, CREATE THE PSEUDO-ENSEMBLE
cd $WORKDIR
for ens in 001 002
do
    if [ -e $WORKDIR/mem${ens} ]; then
            rm -rf $WORKDIR/mem${ens}
    fi
    mkdir -p $WORKDIR/mem${ens}
    for tile in 1 2 3 4 5 6
    do
        cp ${WORKDIR}/${FILEDATE}.sfc_data.tile${tile}.nc  ${WORKDIR}/mem${ens}/${FILEDATE}.sfc_data.tile${tile}.nc
    done
    cp ${WORKDIR}/${FILEDATE}.coupler.res ${WORKDIR}/mem${ens}/${FILEDATE}.coupler.res
done

echo 'do_landDA: calling create ensemble'

python ${CREATEENS} $FILEDATE $SNOWDEPTHVAR $B $WORKDIR

for ens in 001 002
do
    mkdir -p $WORKDIR/mem${ens}/RESTART
    mv $WORKDIR/mem${ens}/${FILEDATE}.* $WORKDIR/mem${ens}/RESTART/
    cp ${WORKDIR}/${FILEDATE}.coupler.res $WORKDIR/mem${ens}/RESTART/
done
################################################################################
# IMS proc
if [[ ${cyc} == "18" ]]; then
  WORKDIR=${DATA}/obs
  cd $WORKDIR
  if [[ -e fims.nml ]]; then
    rm fims.nml
  fi

cat >> fims.nml << EOF
 &fIMS_nml
  idim=$RES, jdim=$RES,
  otype=${TSTUB},
  jdate=$YYYY${DOY},
  yyyymmddhh=${PDY}.18,
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
      cp ${BKGDIR}/${FILEDATE}.sfc_data.tile${tile}.nc .
    fi
  done

  ulimit -Ss unlimited
  ${CALCFIMS}

  export PYTHONPATH=$PYTHONPATH:${project_source_dir}/build/lib/python${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR}/pyioda/

  echo 'do_landDA: calling ioda converter'
  python ${IMS_IODA} -i IMSscf.${PDY}.${TSTUB}.nc -o ioda.IMSscf.${PDY}.${TSTUB}.nc
fi
################################################################################
# run executable
export pgm=${JEDIVAREXE}
. prep_step
${APRUN_LANDANL} "${DATA}/fv3jedi_letkf.x" "${DATA}/${CDUMP}.t${cyc}z.landoi.yaml" 1>&1 2>&2
export err=$?; err_chk
exit "${err}"

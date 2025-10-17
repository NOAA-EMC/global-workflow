#! /usr/bin/env bash

########################################################
#  save fit and horiz files for all analysis cycles
########################################################

export FIT_DIR=${FIT_DIR:-${COMOUT}/fits}
export HORZ_DIR=${HORZ_DIR:-${COMOUT}/horiz}
export fh1=06
export fh2=00
#
#dir=${FIT_DIR}/${EXP}
dir="${FIT_DIR}"
mkdir -p "${dir}"
cd "${dir}" || exit 8
cpreq "${COMOUT}/f${fh1}.raob.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh2}.raob.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh1}.sfc.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh2}.sfc.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh1}.acar.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh2}.acar.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh1}.acft.${PDY}${cyc}" .
cpreq "${COMOUT}/f${fh2}.acft.${PDY}${cyc}" .

export typ=anl
#dir=${HORZ_DIR}/${EXP}/${typ}
dir="${HORZ_DIR}/${typ}"
mkdir -p "${dir}"
cd "${dir}" || exit 8
cpreq "${COMOUT}/adpupa.mand.${typ}.${PDY}${cyc}" "adpupa.mand.${PDY}${cyc}"
cpreq "${COMOUT}/adpsfc.${typ}.${PDY}${cyc}" "adpsfc.${PDY}${cyc}"
cpreq "${COMOUT}/sfcshp.${typ}.${PDY}${cyc}" "sfcshp.${PDY}${cyc}"
cpreq "${COMOUT}/aircar.${typ}.${PDY}${cyc}" "aircar.${PDY}${cyc}"
cpreq "${COMOUT}/aircft.${typ}.${PDY}${cyc}" "aircft.${PDY}${cyc}"
#########################################################################
#  save fit and horiz files for forecasts verifying at 00Z and 12Z cycles
#########################################################################
if [[ "${cyc}" == "00" || "${cyc}" == "12" ]]; then
    if [[ "${cyc}" == "00" ]]; then
        export fh1=24
        export fh2=48
    fi
    if [[ "${cyc}" == "12" ]]; then
        export fh1=12
        export fh2=36
    fi
    #dir=${FIT_DIR}/${EXP}
    dir="${FIT_DIR}"
    mkdir -p "${dir}"
    cd "${dir}" || exit 8
    cpreq "${COMOUT}/f${fh1}.raob.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh2}.raob.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh1}.sfc.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh2}.sfc.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh1}.acar.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh2}.acar.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh1}.acft.${PDY}${cyc}" .
    cpreq "${COMOUT}/f${fh2}.acft.${PDY}${cyc}" .
    export typ=fcs
    #dir=${HORZ_DIR}/${EXP}/${typ}
    dir="${HORZ_DIR}/${typ}"
    mkdir -p "${dir}"
    cd "${dir}" || exit 8
    cpreq "${COMOUT}/adpupa.mand.${typ}.${PDY}${cyc}" "adpupa.mand.${PDY}${cyc}"
    cpreq "${COMOUT}/adpsfc.${typ}.${PDY}${cyc}" "adpsfc.${PDY}${cyc}"
    cpreq "${COMOUT}/sfcshp.${typ}.${PDY}${cyc}" "sfcshp.${PDY}${cyc}"
    cpreq "${COMOUT}/aircar.${typ}.${PDY}${cyc}" "aircar.${PDY}${cyc}"
    cpreq "${COMOUT}/aircft.${typ}.${PDY}${cyc}" "aircft.${PDY}${cyc}"
fi
#########################################################################

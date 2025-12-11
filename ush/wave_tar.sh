#! /usr/bin/env bash

###############################################################################
#                                                                             #
# This script tars the sectral or bulletin files into a single file and       #
# puts it into /com. This is a separate script to enable it to be run in      #
# parallel using poe. It also tars the spectral and bulletin files of the     #
# old grids that are generated for backward compatibility                     #
#                                                                             #
# Remarks :                                                                   #
# - Shell script variables controling time, directories etc. are set in the   #
#   mother script.                                                            #
# - This script runs in the work directory designated in the mother script.   #
#   Under this directory it geneates a work directory TAR_$type_$ID which is  #
#   removed if this script exits normally.                                    #
# - See section 0.c for variables that need to be set.                        #
#                                                                             #
# Origination: Hendrik Tolman                                March 13, 2007   #
# Update log                                                                  #
# Nov2019 JHAlves - Merging wave scripts to global workflow                   #
# 2020-06-10  J-Henrique Alves Ported R&D machine Hera
#                                                                             #
###############################################################################
#
# --------------------------------------------------------------------------- #
# 0.  Preparations

# 0.a Basic modes of operation

cd "${DATA}" || exit 1
echo "Making TAR FILE"
export err

# 0.b Check if type set

if [[ "$#" -lt '3' ]]; then
    msg='FATAL ERROR: VARIABLES IN ww3_tar.sh NOT SET'
    err=1
    err_exit "${msg}"
else
    ID=$1
    type=$2
    nb=$3
fi

cat << EOF

+--------------------------------+
!         Make tar file          |
+--------------------------------+
   ID              : $1
   Type            : $2
   Number of files : $3
EOF

filext=${type}
if [[ "${type}" == "ibp" ]]; then
    filext='spec'
fi
if [[ "${type}" == "ibpbull" ]]; then
    filext='bull'
fi
if [[ "${type}" == "ibpcbull" ]]; then
    filext='cbull'
fi

rm -rf "TAR_${filext}_${ID}"
mkdir "TAR_${filext}_${ID}"
# this directory is used only for error capturing

# 0.c Define directories and the search path.
#     The tested variables should be exported by the postprocessor script.

if [[ -z "${COMOUT_WAVE_STATION+x}" || -z "${SENDDBN+x}" || -z "${STA_DIR+x}" ]]; then
    msg='FATAL ERROR: EXPORTED VARIABLES IN ww3_tar.sh NOT SET'
    err=2
    err_exit "${msg}"
fi

# --------------------------------------------------------------------------- #
# 2.  Generate tar file (spectral files are compressed)

printf "\n   Making tar file ..."

countMAX=5
tardone='no'
sleep_interval=10

while [[ "${tardone}" == "no" ]]; do
    # shellcheck disable=SC2312
    nf=$(find . -maxdepth 1 -type f -name "*.${filext}" -printf '.' | wc -c)
    nbm2=$((nb - 2))
    if [[ ${nf} -ge ${nbm2} ]]; then

        tar -cf "${ID}.${type}.tar" ./*."${filext}"
        err=$?

        if [[ ${err} -ne 0 ]]; then
            msg='FATAL ERROR: TAR CREATION FAILED *** '
            err=3
            err_exit "${msg}"
        fi

        filename="${ID}.${type}.tar"
        if ! wait_for_file "${filename}" "${sleep_interval}" "${countMAX}"; then
            msg="FATAL ERROR: File ${filename} not found after waiting $((sleep_interval * (countMAX + 1))) secs"
            err=3
            err_exit "${msg}"
        fi

        if [[ -f "${ID}.${type}.tar" ]]; then
            tardone='yes'
        fi
    fi

done

if [[ "${tardone}" == 'no' ]]; then
    msg='FATAL ERROR: TAR CREATION FAILED *** '
    err=3
    err_exit "${msg}"
fi

if [[ "${type}" == 'spec' ]]; then
    if [[ -s "${ID}.${type}.tar" ]]; then
        file_name="${ID}.${type}.tar.gz"
        /usr/bin/gzip -c "${ID}.${type}.tar" > "${file_name}"
        err=$?

        if [[ ${err} -ne 0 ]]; then
            msg='FATAL ERROR: SPECTRAL TAR COMPRESSION FAILED *** '
            err=4
            err_exit
        fi
    fi
else
    file_name="${ID}.${type}.tar"
fi

# --------------------------------------------------------------------------- #
# 3.  Move data to /com

echo "   Moving tar file ${file_name} to ${COMOUT_WAVE_STATION} ..."

cpfs "${file_name}" "${COMOUT_WAVE_STATION}/."

err=$?

if [[ ${err} -ne 0 ]]; then
    msg='FATAL ERROR: TAR COPY FAILED *** '
    export err=4
    err_exit "${msg}"
fi

if [[ "${SENDDBN}" == "YES" ]]; then
    echo "   Alerting TAR file as ${COMOUT_WAVE_STATION}/${file_name}"
    "${DBNROOT}/bin/dbn_alert MODEL" "${RUN^^}_WAVE_TAR" "${job}" \
        "${COMOUT_WAVE_STATION}/${file_name}"
fi

# --------------------------------------------------------------------------- #
# 4.  Final clean up

cd "${DATA}" || exit 1

if [[ "${KEEPDATA:-NO}" == "NO" ]]; then
    set -v
    rm -rf "${STA_DIR:?}/${type}"
    set +v
fi

# End of ww3_tar.sh ----------------------------------------------------- #

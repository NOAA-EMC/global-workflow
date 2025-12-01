#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_diag.sh
# Script description:  Creates diagnostic files after GSI analysis is performed
#
# Author: Cory Martin      Org: NCEP/EMC     Date: 2020-03-03
#
# Abstract: This script creates GSI diagnostic files after GSI exits successfully
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

#  Set environment.

# Base variables
GDUMP="${GDUMP:-gdas}"

# Utilities
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-${USHgfs}/getncdimlen}
export CATEXEC=${CATEXEC:-${ncdiag_ROOT:-${gsi_ncdiag_ROOT}}/bin/ncdiag_cat_serial.x}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUNCFP=${APRUNCFP:-""}

# Diagnostic files options
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

# Analysis files
export APREFIX=${APREFIX:-""}
RADSTAT=${RADSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}radstat.tar}
PCPSTAT=${PCPSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}pcpstat}
CNVSTAT=${CNVSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}cnvstat.tar}
OZNSTAT=${OZNSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}oznstat.tar}

# Remove stat file if file already exists
rm -f "${RADSTAT}" "${PCPSTAT}" "${CNVSTAT}" "${OZNSTAT}"

# Obs diag
GENDIAG=${GENDIAG:-"YES"}
DIAG_SUFFIX=${DIAG_SUFFIX:-""}
if [[ "${netcdf_diag}" == ".true." ]]; then
    DIAG_SUFFIX="${DIAG_SUFFIX}.nc4"
fi
DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
COMPRESS_SUFFIX=${COMPRESS_SUFFIX:-""}
if [[ ${DIAG_COMPRESS} == "YES" ]]; then
    COMPRESS_SUFFIX="${COMPRESS_SUFFIX}.gz"
fi
DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
DIAG_DIR=${DIAG_DIR:-${COMOUT_ATMOS_ANALYSIS}/gsidiags}
REMOVE_DIAG_DIR=${REMOVE_DIAG_DIR:-"NO"}

# Set script / GSI control parameters
lrun_subdirs=${lrun_subdirs:-".true."}

################################################################################
# If requested, generate diagnostic files
if [[ "${GENDIAG}" == "YES" ]]; then
    if [[ "${lrun_subdirs}" == ".true." ]]; then
        for pe in "${DIAG_DIR}/"dir.*; do
            pedir="$(basename -- "${pe}")"
            ${NLN} "${pe}" "${DATA}/${pedir}"
        done
    else
        err_exit "lrun_subdirs must be true.  Abort job"
    fi

    # Set up lists and variables for various types of diagnostic files.
    diagtype[0]="conv conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_tcp conv_uv conv_spd"
    diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
    if [[ "${USE_BUILD_GSINFO}" == "YES" ]]; then
        diagtype[2]=$(cat "${BUILD_GSINFO_DIR}/ozinfo/satellites")
        diagtype[3]=$(cat "${BUILD_GSINFO_DIR}/satinfo/satellites")
    else
        diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls30_aura ompsnp_npp ompstc8_npp  ompstc8_n20 ompsnp_n20 ompstc8_n21 ompsnp_n21 ompslp_npp gome_metop-c"
        diagtype[3]="msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f15 amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_f16 ssmis_f17 ssmis_f18 ssmis_f19 ssmis_f20 iasi_metop-a amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp cris-fsr_npp cris-fsr_n20 atms_npp atms_n20 amsua_metop-b mhs_metop-b iasi_metop-b avhrr_metop-b avhrr_n18 avhrr_n19 avhrr_metop-a amsr2_gcom-w1 gmi_gpm saphir_meghat ahi_himawari8 abi_g16 abi_g17 amsua_metop-c mhs_metop-c iasi_metop-c avhrr_metop-c viirs-m_npp viirs-m_j1 abi_g18 ahi_himawari9 viirs-m_j2 cris-fsr_n21 atms_n21 abi_g19"
    fi

    diaglist[0]=listcnv
    diaglist[1]=listpcp
    diaglist[2]=listozn
    diaglist[3]=listrad

    diagfile[0]="${CNVSTAT}"
    diagfile[1]="${PCPSTAT}"
    diagfile[2]="${OZNSTAT}"
    diagfile[3]="${RADSTAT}"

    # Set diagnostic file prefix based on lrun_subdirs variable
    if [[ "${lrun_subdirs}" == ".true." ]]; then
        prefix="dir.*/"
    else
        prefix="pe*"
    fi

    rm -f "${DATA}/diag.sh" "${DATA}/mp_diag.sh"
    cat > "${DATA}/diag.sh" << EOFdiag
#!/bin/sh
lrun_subdirs=\$1
binary_diag=\$2
type=\$3
loop=\$4
string=\$5
PDY=\$6
cyc=\$7
DIAG_COMPRESS=\$8
DIAG_SUFFIX=\$9
if [[ "\${lrun_subdirs}" == ".true." ]]; then
    prefix="dir.*/"
else
    prefix="pe*"
fi
count=\$(find -L ./ -path "./\${prefix}\${type}_\${loop}*" -type f -printf '.' | wc -c)
file="diag_\${type}_\${string}.\${PDY}\${cyc}\${DIAG_SUFFIX}"
if [[ "\${binary_diag}" == ".true." ]] || [[ "\${count}" -eq 1 ]]; then
    cat \${prefix}\${type}_\${loop}* > "\${file}"
else    
    ${CATEXEC} -o "\${file}" \${prefix}\${type}_\${loop}*
fi
if [[ "\${DIAG_COMPRESS}" == "YES" ]]; then
    ${COMPRESS} "\${file}"
fi
EOFdiag
    chmod 755 "${DATA}/diag.sh"

    # Collect diagnostic files as a function of loop and type.
    # Loop over first and last outer loops to generate innovation
    # diagnostic files for indicated observation types (groups)
    #
    # NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
    #        loop 03 will contain innovations with respect to
    #        the analysis.  Creation of o-a innovation files
    #        is triggered by write_diag(3)=.true.  The setting
    #        write_diag(1)=.true. turns on creation of o-g
    #        innovation files.

    loops="01 03"
    for loop in ${loops}; do
        case ${loop} in
            01) string=ges ;;
            03) string=anl ;;
            *) string=${loop} ;;
        esac
        # shellcheck disable=SC2312
        echo "$(date) START loop ${string}" >&2
        for ((n = 0; n < ${#diagtype[@]}; n++)); do
            for type in ${diagtype[n]}; do
                # shellcheck disable=SC2312
                count=$(find -L ./ -path "./${prefix}${type}_${loop}*" -type f -printf '.' | wc -c)
                if [[ "${count}" -gt 0 ]]; then
                    echo "${DATA}/diag.sh ${lrun_subdirs} ${binary_diag} ${type} ${loop} ${string} ${PDY} ${cyc} ${DIAG_COMPRESS} ${DIAG_SUFFIX}" | tee -a "${DATA}/mp_diag.sh"
                    echo "diag_${type}_${string}.${PDY}${cyc}${DIAG_SUFFIX}${COMPRESS_SUFFIX}" >> "${diaglist[n]}"
                fi
            done
        done
        # shellcheck disable=SC2312
        echo "$(date) END loop ${string}" >&2
    done

    # We should already be in $DATA, but extra cd to be sure.
    cd "${DATA}" || exit 1

    export err=0
    if [[ -s "${DATA}/mp_diag.sh" ]]; then
        chmod 755 "${DATA}/mp_diag.sh"
        "${USHgfs}/run_mpmd.sh" "${DATA}/mp_diag.sh" && true
        # Delay err exit to avoid rstprod leakage
        err=$?
    fi

    # Restrict diagnostic files containing rstprod data
    if [[ "${CHGRP_RSTPROD}" == "YES" ]]; then
        rlist="conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_uv saphir"
        for rtype in ${rlist}; do
            for rfile in *"${rtype}"*; do
                if [[ -s "${rfile}" ]]; then
                    ${CHGRP_CMD} "${rfile}"
                fi
            done
        done
    fi

    # Now exit if compression failed
    if [[ ${err} -ne 0 ]]; then
        err_exit "Failed to compress one or more observation diagnostic files!"
    fi

    # If requested, create diagnostic file tarballs
    if [[ "${DIAG_TARBALL}" == "YES" ]]; then
        # shellcheck disable=SC2312
        echo "$(date) START tar diagnostic files" >&2
        for ((n = 0; n < ${#diagtype[@]}; n++)); do
            TAROPTS="-uvf"
            if [[ ! -s "${diagfile[n]}" ]]; then
                TAROPTS="-cvf"
            fi
            if [[ -s "${diaglist[n]}" ]]; then
                tar "${TAROPTS}" "${diagfile[n]}" -T "${diaglist[n]}"
                export err=$?
                if [[ ${err} -ne 0 ]]; then
                    # Delay err_exit to avoid rstprod leakage
                    err_msg="Unable to create ${diagfile[n]}!"
                    break
                fi
            fi
        done

        # Restrict CNVSTAT
        chmod 750 "${CNVSTAT}"
        if [[ "${CHGRP_RSTPROD}" == "YES" ]]; then
            ${CHGRP_CMD} "${CNVSTAT}"
        fi

        # Restrict RADSTAT
        if [[ -s "${RADSTAT}" ]]; then
            chmod 750 "${RADSTAT}"
            if [[ "${CHGRP_RSTPROD}" == "YES" ]]; then
                ${CHGRP_CMD} "${RADSTAT}"
            fi
        fi

        # Now exit if there was an error
        if [[ ${err} -ne 0 ]]; then
            err_exit "${err_msg}"
        fi

        # shellcheck disable=SC2312
        echo "$(date) END tar diagnostic files" >&2
    fi
fi # End diagnostic file generation block - if [[ "${GENDIAG}" == "YES" ]]

################################################################################
# Postprocessing
# Remove $DIAG_DIR
if [[ "${REMOVE_DIAG_DIR}" == "YES" ]]; then
    rm -rf "${DIAG_DIR}"
fi

exit 0

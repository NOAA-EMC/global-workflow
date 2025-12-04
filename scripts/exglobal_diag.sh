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
cd "${DATA}" || exit 8

# Base variables

# Utilities
CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
CATEXEC=${CATEXEC:-${ncdiag_ROOT:-${gsi_ncdiag_ROOT}}/bin/ncdiag_cat_serial.x}
COMPRESS=${COMPRESS:-gzip}

# Analysis files
APREFIX=${APREFIX:-"${RUN}.t${cyc}z."}
CNVSTAT=${CNVSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}cnvstat.tar}
PCPSTAT=${PCPSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}pcpstat}
OZNSTAT=${OZNSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}oznstat.tar}
RADSTAT=${RADSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}radstat.tar}

# Remove stat file if file already exists
rm -f "${RADSTAT}" "${PCPSTAT}" "${CNVSTAT}" "${OZNSTAT}"

# Obs diag
GENDIAG=${GENDIAG:-"YES"}
GSIDIAG=${GSIDIAG:-"${COMIN_ATMOS_ANALYSIS}/${APREFIX}gsidiags${DIAG_SUFFIX:-}.tar"}
USE_BUILD_GSINFO=${USE_BUILD_GSINFO:-"NO"}
DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
if [[ "${DIAG_COMPRESS:-}" == "YES" ]]; then
    COMPRESS_SUFFIX=".gz"
fi
DIAG_TARBALL=${DIAG_TARBALL:-"YES"}

# Set script / GSI control parameters

################################################################################
if [[ "${GENDIAG}" != "YES" ]]; then
    echo "INFO: GENDIAG set to NO.  Skipping diagnostic file generation."
    exit 0
fi

################################################################################
# Copy gsidiags.tar file from COMIN to DATA and untar
cpreq "${GSIDIAG}" ./gsidiags.tar
tar -xvf gsidiags.tar
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "Unable to unpack gsidiags.tar file!"
fi

# Set up lists and variables for various types of diagnostic files.
ntype=3

diagtype[0]="conv conv_gps conv_ps conv_pw conv_q conv_sst conv_t conv_tcp conv_uv conv_spd"
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
if [[ ${USE_BUILD_GSINFO} == "YES" ]]; then
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

numfile[0]=0
numfile[1]=0
numfile[2]=0
numfile[3]=0

rm -f "${DATA}/diag.sh"
cat > "${DATA}/diag.sh" << EOF
#!/bin/bash
set -x

type=\$1
loop=\$2
string=\$3
count=\$4
suffix=\$5

# Match files with this prefix
diag_files=dir.*/\${type}_\${loop}

# Name of combined diagnostic file from matched files
out_diag_file=diag_\${type}_\${string}.${PDY}${cyc}\${suffix}

# Combine diagnostic files
if [[ \${count} -gt 1 ]]; then
    ${CATEXEC} -o \${out_diag_file} \${diag_files}*
else
    cat \${diag_files}* > "\${out_diag_file}"
fi

# Compress diagnostic file if requested
if [[ "${DIAG_COMPRESS:-}" == "YES" ]]; then
    ${COMPRESS} "\${out_diag_file}"
fi

exit 0
EOF
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

rm -f cmdfile
touch cmdfile
loops="01 03"
for loop in ${loops}; do
    case ${loop} in
        01) string=ges ;;
        03) string=anl ;;
        *) string=${loop} ;;
    esac
    echo "START loop ${string}"
    n=-1
    while [[ ${n} -lt ${ntype} ]]; do
        n=$((n + 1))
        for type in ${diagtype[n]}; do
            #shellcheck disable=SC2012,SC2312
            count=$(ls dir.*/"${type}_${loop}"* 2> /dev/null | wc -l)
            if [[ ${count} -eq 0 ]]; then
                continue
            fi
            echo "${DATA}/diag.sh ${type} ${loop} ${string} ${count} ${DIAG_SUFFIX:-}.nc4" >> cmdfile
            echo "diag_${type}_${string}.${PDY}${cyc}${DIAG_SUFFIX:-}.nc4${COMPRESS_SUFFIX:-}" >> "${diaglist[n]}"
            numfile[n]=$((numfile[n] + 1))
        done
    done
    echo "END loop ${string}"
done

ncmd=$(wc -l < cmdfile)
if [[ ${ncmd} -eq 0 ]]; then
    echo "WARNING: No diagnostic files found to process!"
    exit 0
fi

# MPMD can only be executed on a single node,
# so break up cmdfile into parts of tasks_per_node size files
# and run them sequentially
split -l "${tasks_per_node}" ./cmdfile cmdfile_part_
cmdfile_parts=$(ls cmdfile_part_*)
for partfile in ${cmdfile_parts}; do
    "${USHgfs}/run_mpmd.sh" "${partfile}" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "Failed to create one or more observation diagnostic files for ${partfile}!"
    fi
done

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

# If requested, create diagnostic file tarballs
if [[ "${DIAG_TARBALL}" == "YES" ]]; then
    echo "START tar diagnostic files"
    n=-1
    while [[ ${n} -lt ${ntype} ]]; do
        n=$((n + 1))
        TAROPTS="-uvf"
        if [[ ! -s "${diagfile[n]}" ]]; then
            TAROPTS="-cvf"
        fi
        if [[ ${numfile[n]} -gt 0 ]]; then
            tar "${TAROPTS}" "${diagfile[n]}" -T "${diaglist[n]}"
            export err=$?
            if [[ ${err} -ne 0 ]]; then
                err_exit "Unable to create ${diagfile[n]}!"
            fi
        fi
    done
    echo "END tar diagnostic files"

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
fi

################################################################################
# Postprocessing

exit 0

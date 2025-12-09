#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exgdas_enkf_update.sh
# Script description:  Make global_enkf update
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script runs the global_enkf update
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
################################################################################

# Directories.
cd "${DATA}" || exit 1

# Utilities
NCLEN=${NCLEN:-${USHgfs}/getncdimlen}
APRUN_ENKF=${APRUN_ENKF:-${APRUN:-""}}
NTHREADS_ENKF=${NTHREADS_ENKF:-${NTHREADS:-1}}

# Executables
ENKFEXEC=${ENKFEXEC:-${EXECgfs}/enkf.x}

APREFIX=${APREFIX:-${RUN}.t${cyc}z.}
GPREFIX=${GPREFIX:-${RUN}.t${GDATE:8:2}z.}

# GBIASe=${GBIASe:-${APREFIX}abias_int.ensmean.txt}  # TODO: remove (see comment and TODO below) Also, this is not a "G"BIAS (See the name, it has APREFIX in its name)
CNVSTAT="${APREFIX}cnvstat_ensmean.tar"
OZNSTAT="${APREFIX}oznstat_ensmean.tar"
RADSTAT="${APREFIX}radstat_ensmean.tar"

# Namelist parameters
if [[ "${USE_CORRELATED_OBERRS:-}" == "YES" ]]; then
    use_correlated_oberrs=".true."
fi
corrlength=${corrlength:-1250}
lnsigcutoff=${lnsigcutoff:-2.5}
analpertwt=${analpertwt:-0.85}
cnvw_option=${cnvw_option:-".false."}
IAUFHRS_ENKF=${IAUFHRS_ENKF:-"6,"}
NMEM_ENS_MAX=${NMEM_ENS:-80}
if [[ "${RUN}" == "enkfgfs" ]]; then
    DO_CALC_INCREMENT=${DO_CALC_INCREMENT_ENKF_GFS:-"NO"}
    NMEM_ENS=${NMEM_ENS_GFS:-30}
    ec_offset=${NMEM_ENS_GFS_OFFSET:-20}
    mem_offset=$((ec_offset * cyc / 6))
else
    DO_CALC_INCREMENT=${DO_CALC_INCREMENT:-"NO"}
    NMEM_ENS=${NMEM_ENS:-80}
    mem_offset=0
fi
INCREMENTS_TO_ZERO=${INCREMENTS_TO_ZERO:-"'NONE'"}
DO_GSISOILDA=${DO_GSISOILDA:-"NO"}
hofx_2m_sfcfile=${hofx_2m_sfcfile:-".false."}

################################################################################

ATMGES_ENSMEAN="${COMIN_ATMOS_HISTORY_STAT_PREV}/${GPREFIX}ensmean.atm.f006.nc"
LONB_ENKF=${LONB_ENKF:-$(${NCLEN} "${ATMGES_ENSMEAN}" grid_xt)} # get LONB_ENKF
LATB_ENKF=${LATB_ENKF:-$(${NCLEN} "${ATMGES_ENSMEAN}" grid_yt)} # get LATB_ENFK
LEVS_ENKF=${LEVS_ENKF:-$(${NCLEN} "${ATMGES_ENSMEAN}" pfull)}   # get LEVS_ENFK
WRITE_INCR_ZERO="incvars_to_zero= ${INCREMENTS_TO_ZERO},"
if [[ "${DO_CALC_INCREMENT}" == "YES" ]]; then
    write_fv3_incr=".false."
else
    write_fv3_incr=".true."
fi
LATA_ENKF=${LATA_ENKF:-${LATB_ENKF}}
LONA_ENKF=${LONA_ENKF:-${LONB_ENKF}}
SATANGL=${SATANGL:-${FIXgfs}/gsi/global_satangbias.txt}
SATINFO=${SATINFO:-${FIXgfs}/gsi/global_satinfo.txt}
CONVINFO=${CONVINFO:-${FIXgfs}/gsi/global_convinfo.txt}
OZINFO=${OZINFO:-${FIXgfs}/gsi/global_ozinfo.txt}
SCANINFO=${SCANINFO:-${FIXgfs}/gsi/global_scaninfo.txt}
HYBENSINFO=${HYBENSINFO:-${FIXgfs}/gsi/global_hybens_info.l${LEVS_ENKF}.txt}
ANAVINFO=${ANAVINFO:-${FIXgfs}/gsi/global_anavinfo.l${LEVS_ENKF}.txt}
VLOCALEIG=${VLOCALEIG:-${FIXgfs}/gsi/vlocal_eig_l${LEVS_ENKF}.dat}
ENKF_SUFFIX="s"
if [[ "${SMOOTH_ENKF:-YES}" == "NO" ]]; then
    ENKF_SUFFIX=""
fi

################################################################################
# Fixed files
cpreq "${SATANGL}" satbias_angle
cpreq "${SCANINFO}" scaninfo
cpreq "${HYBENSINFO}" hybens_info
cpreq "${ANAVINFO}" anavinfo
cpreq "${VLOCALEIG}" vlocal_eig.dat
if [[ "${SATINFO}" == "generate" ]]; then
    "${USHgfs}/create_gsi_info.sh" sat "${PDY}${cyc}" "${DATA}"
else
    cpreq "${SATINFO}" satinfo
fi
if [[ "${CONVINFO}" == "generate" ]]; then
    "${USHgfs}/create_gsi_info.sh" conv "${PDY}${cyc}" "${DATA}" "${USE_2M_OBS}"
else
    cpreq "${CONVINFO}" convinfo
fi
if [[ "${OZINFO}" == "generate" ]]; then
    "${USHgfs}/create_gsi_info.sh" oz "${PDY}${cyc}" "${DATA}"
else
    cpreq "${OZINFO}" ozinfo
fi

# Bias correction coefficients based on the ensemble mean
#${NLN} "${COMIN_ATMOS_ANALYSIS_STAT}/${GBIASe}" "satbias_in"  # This file does not exist when test was run # TODO: remove

################################################################################
# Ensemble guess, observational data and analyses/increments

flist="${CNVSTAT} ${OZNSTAT} ${RADSTAT}"
for ftype in ${flist}; do
    fname="${COMIN_ATMOS_ANALYSIS_STAT}/${ftype}"
    tar -xvf "${fname}"
done

nfhrs="${IAUFHRS_ENKF//,/ }"
for imem in $(seq 1 "${NMEM_ENS}"); do
    smem=$((imem + mem_offset))
    if [[ ${smem} -gt ${NMEM_ENS_MAX} ]]; then
        smem=$((smem - NMEM_ENS_MAX))
    fi
    gmemchar="mem"$(printf "%03i" "${smem}")
    memchar="mem"$(printf "%03i" "${imem}")

    MEMDIR=${gmemchar} RUN=${GDUMP} YMD=${GDATE:0:8} HH=${GDATE:8:2} declare_from_tmpl -x \
        COMIN_ATMOS_HISTORY_MEM_PREV:COM_ATMOS_HISTORY_TMPL

    MEMDIR=${memchar} YMD=${PDY} HH=${cyc} declare_from_tmpl -x \
        COMOUT_ATMOS_ANALYSIS_MEM:COM_ATMOS_ANALYSIS_TMPL

    mkdir -p "${COMOUT_ATMOS_ANALYSIS_MEM}"

    for FHR in ${nfhrs}; do
        ${NLN} "${COMIN_ATMOS_HISTORY_MEM_PREV}/${GPREFIX}atm.f00${FHR}${ENKF_SUFFIX}.nc" \
            "sfg_${PDY}${cyc}_fhr0${FHR}_${memchar}"
        if [[ "${hofx_2m_sfcfile}" == ".true." ]]; then
            ${NLN} "${COMIN_ATMOS_HISTORY_MEM_PREV}/${GPREFIX}sfc.f00${FHR}${ENKF_SUFFIX}.nc" \
                "bfg_${PDY}${cyc}_fhr0${FHR}_${memchar}"
        fi
        if [[ "${cnvw_option}" == ".true." ]]; then
            ${NLN} "${COMIN_ATMOS_HISTORY_MEM_PREV}/${GPREFIX}sfc.f00${FHR}.nc" \
                "sfgsfc_${PDY}${cyc}_fhr0${FHR}_${memchar}"
        fi

        if [[ "${DO_CALC_INCREMENT}" == "YES" ]]; then
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX}analysis.atm.a00${FHR}.nc" \
                "sanl_${PDY}${cyc}_fhr0${FHR}_${memchar}"
        else
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX}increment.atm.i00${FHR}.nc" \
                "incr_${PDY}${cyc}_fhr0${FHR}_${memchar}"
        fi

        if [[ "${DO_GSISOILDA}" == "YES" ]]; then
            ${NLN} "${COMOUT_ATMOS_ANALYSIS_MEM}/${APREFIX}increment.sfc.i00${FHR}.nc" \
                "sfcincr_${PDY}${cyc}_fhr0${FHR}_${memchar}"
        fi
    done
done

# Ensemble mean guess
for FHR in ${nfhrs}; do
    ${NLN} "${COMIN_ATMOS_HISTORY_STAT_PREV}/${GPREFIX}ensmean.atm.f00${FHR}.nc" \
        "sfg_${PDY}${cyc}_fhr0${FHR}_ensmean"
    if [[ "${cnvw_option}" == ".true." ]]; then
        ${NLN} "${COMIN_ATMOS_HISTORY_STAT_PREV}/${GPREFIX}ensmean.sfc.f00${FHR}.nc" \
            "sfgsfc_${PDY}${cyc}_fhr0${FHR}_ensmean"
    fi
    if [[ "${DO_GSISOILDA}" == "YES" ]]; then
        ${NLN} "${COMIN_ATMOS_HISTORY_STAT_PREV}/${GPREFIX}ensmean.sfc.f00${FHR}.nc" \
            "bfg_${PDY}${cyc}_fhr0${FHR}_ensmean"
        ${NLN} "${COMIN_ATMOS_ANALYSIS_STAT}/${APREFIX}ensmean_increment.sfc.i00${FHR}.nc" \
            "sfcincr_${PDY}${cyc}_fhr0${FHR}_ensmean"
    fi
done

################################################################################
# Create global_enkf namelist
cat > enkf.nml << EOFnml
&nam_enkf
   datestring="${PDY}${cyc}",datapath="${DATA}/",
   analpertwtnh=${analpertwt},analpertwtsh=${analpertwt},analpertwttr=${analpertwt},
   covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.false.,iassim_order=0,
   corrlengthnh=${corrlength},corrlengthsh=${corrlength},corrlengthtr=${corrlength},
   lnsigcutoffnh=${lnsigcutoff},lnsigcutoffsh=${lnsigcutoff},lnsigcutofftr=${lnsigcutoff},
   lnsigcutoffpsnh=${lnsigcutoff},lnsigcutoffpssh=${lnsigcutoff},lnsigcutoffpstr=${lnsigcutoff},
   lnsigcutoffsatnh=${lnsigcutoff},lnsigcutoffsatsh=${lnsigcutoff},lnsigcutoffsattr=${lnsigcutoff},
   obtimelnh=1.e30,obtimelsh=1.e30,obtimeltr=1.e30,
   saterrfact=1.0,numiter=0,
   sprd_tol=1.e30,paoverpb_thresh=0.98,
   nlons=${LONA_ENKF},nlats=${LATA_ENKF},nlevs=${LEVS_ENKF},nanals=${NMEM_ENS},
   deterministic=.true.,sortinc=.true.,lupd_satbiasc=.false.,
   reducedgrid=${reducedgrid:-.false.},readin_localization=${readin_localization_enkf:-.true.},
   use_gfs_nemsio=.false.,use_gfs_ncio=.true.,imp_physics=${imp_physics:-99},lupp=${lupp:-.true.},
   univaroz=.false.,adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,
   letkf_flag=${letkf_flag:-.false.},nobsl_max=${nobsl_max:-10000},denkf=${denkf:-.false.},getkf=${getkf:-.false.},
   nhr_anal=${IAUFHRS_ENKF},nhr_state=${IAUFHRS_ENKF},
   lobsdiag_forenkf=.true.,taperanalperts=${taperanalperts:-.false.},
   write_spread_diag=${write_spread_diag:-".false."},
   modelspace_vloc=${modelspace_vloc:-.false.},
   use_correlated_oberrs=${use_correlated_oberrs:-.false.},
   netcdf_diag=.true.,cnvw_option=${cnvw_option},
   paranc=${paranc:-.true.},write_fv3_incr=${write_fv3_incr},
   ${WRITE_INCR_ZERO}
   ${NAM_ENKF:-}
/
&satobs_enkf
   sattypes_rad(1) = 'amsua_n15',     dsis(1) = 'amsua_n15',
   sattypes_rad(2) = 'amsua_n18',     dsis(2) = 'amsua_n18',
   sattypes_rad(3) = 'amsua_n19',     dsis(3) = 'amsua_n19',
   sattypes_rad(4) = 'amsub_n16',     dsis(4) = 'amsub_n16',
   sattypes_rad(5) = 'amsub_n17',     dsis(5) = 'amsub_n17',
   sattypes_rad(6) = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
   sattypes_rad(7) = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
   sattypes_rad(8) = 'airs_aqua',     dsis(8) = 'airs_aqua',
   sattypes_rad(9) = 'hirs3_n17',     dsis(9) = 'hirs3_n17',
   sattypes_rad(10)= 'hirs4_n19',     dsis(10)= 'hirs4_n19',
   sattypes_rad(11)= 'hirs4_metop-a', dsis(11)= 'hirs4_metop-a',
   sattypes_rad(12)= 'mhs_n18',       dsis(12)= 'mhs_n18',
   sattypes_rad(13)= 'mhs_n19',       dsis(13)= 'mhs_n19',
   sattypes_rad(14)= 'mhs_metop-a',   dsis(14)= 'mhs_metop-a',
   sattypes_rad(15)= 'goes_img_g11',  dsis(15)= 'imgr_g11',
   sattypes_rad(16)= 'goes_img_g12',  dsis(16)= 'imgr_g12',
   sattypes_rad(17)= 'goes_img_g13',  dsis(17)= 'imgr_g13',
   sattypes_rad(18)= 'goes_img_g14',  dsis(18)= 'imgr_g14',
   sattypes_rad(19)= 'goes_img_g15',  dsis(19)= 'imgr_g15',
   sattypes_rad(20)= 'avhrr_n18',     dsis(20)= 'avhrr3_n18',
   sattypes_rad(21)= 'avhrr_metop-a', dsis(21)= 'avhrr3_metop-a',
   sattypes_rad(22)= 'avhrr_n19',     dsis(22)= 'avhrr3_n19',
   sattypes_rad(23)= 'amsre_aqua',    dsis(23)= 'amsre_aqua',
   sattypes_rad(24)= 'ssmis_f16',     dsis(24)= 'ssmis_f16',
   sattypes_rad(25)= 'ssmis_f17',     dsis(25)= 'ssmis_f17',
   sattypes_rad(26)= 'ssmis_f18',     dsis(26)= 'ssmis_f18',
   sattypes_rad(27)= 'ssmis_f19',     dsis(27)= 'ssmis_f19',
   sattypes_rad(28)= 'ssmis_f20',     dsis(28)= 'ssmis_f20',
   sattypes_rad(29)= 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
   sattypes_rad(30)= 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
   sattypes_rad(31)= 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
   sattypes_rad(32)= 'sndrd4_g11',    dsis(32)= 'sndrD4_g11',
   sattypes_rad(33)= 'sndrd1_g12',    dsis(33)= 'sndrD1_g12',
   sattypes_rad(34)= 'sndrd2_g12',    dsis(34)= 'sndrD2_g12',
   sattypes_rad(35)= 'sndrd3_g12',    dsis(35)= 'sndrD3_g12',
   sattypes_rad(36)= 'sndrd4_g12',    dsis(36)= 'sndrD4_g12',
   sattypes_rad(37)= 'sndrd1_g13',    dsis(37)= 'sndrD1_g13',
   sattypes_rad(38)= 'sndrd2_g13',    dsis(38)= 'sndrD2_g13',
   sattypes_rad(39)= 'sndrd3_g13',    dsis(39)= 'sndrD3_g13',
   sattypes_rad(40)= 'sndrd4_g13',    dsis(40)= 'sndrD4_g13',
   sattypes_rad(41)= 'sndrd1_g14',    dsis(41)= 'sndrD1_g14',
   sattypes_rad(42)= 'sndrd2_g14',    dsis(42)= 'sndrD2_g14',
   sattypes_rad(43)= 'sndrd3_g14',    dsis(43)= 'sndrD3_g14',
   sattypes_rad(44)= 'sndrd4_g14',    dsis(44)= 'sndrD4_g14',
   sattypes_rad(45)= 'sndrd1_g15',    dsis(45)= 'sndrD1_g15',
   sattypes_rad(46)= 'sndrd2_g15',    dsis(46)= 'sndrD2_g15',
   sattypes_rad(47)= 'sndrd3_g15',    dsis(47)= 'sndrD3_g15',
   sattypes_rad(48)= 'sndrd4_g15',    dsis(48)= 'sndrD4_g15',
   sattypes_rad(49)= 'iasi_metop-a',  dsis(49)= 'iasi_metop-a',
   sattypes_rad(50)= 'seviri_m08',    dsis(50)= 'seviri_m08',
   sattypes_rad(51)= 'seviri_m09',    dsis(51)= 'seviri_m09',
   sattypes_rad(52)= 'seviri_m10',    dsis(52)= 'seviri_m10',
   sattypes_rad(53)= 'seviri_m11',    dsis(53)= 'seviri_m11',
   sattypes_rad(54)= 'amsua_metop-b', dsis(54)= 'amsua_metop-b',
   sattypes_rad(55)= 'hirs4_metop-b', dsis(55)= 'hirs4_metop-b',
   sattypes_rad(56)= 'mhs_metop-b',   dsis(56)= 'mhs_metop-b',
   sattypes_rad(57)= 'iasi_metop-b',  dsis(57)= 'iasi_metop-b',
   sattypes_rad(58)= 'avhrr_metop-b', dsis(58)= 'avhrr3_metop-b',
   sattypes_rad(59)= 'atms_npp',      dsis(59)= 'atms_npp',
   sattypes_rad(60)= 'atms_n20',      dsis(60)= 'atms_n20',
   sattypes_rad(61)= 'cris_npp',      dsis(61)= 'cris_npp',
   sattypes_rad(62)= 'cris-fsr_npp',  dsis(62)= 'cris-fsr_npp',
   sattypes_rad(63)= 'cris-fsr_n20',  dsis(63)= 'cris-fsr_n20',
   sattypes_rad(64)= 'gmi_gpm',       dsis(64)= 'gmi_gpm',
   sattypes_rad(65)= 'saphir_meghat', dsis(65)= 'saphir_meghat',
   sattypes_rad(66)= 'amsua_metop-c', dsis(66)= 'amsua_metop-c',
   sattypes_rad(67)= 'mhs_metop-c',   dsis(67)= 'mhs_metop-c',
   sattypes_rad(68)= 'ahi_himawari8', dsis(68)= 'ahi_himawari8',
   sattypes_rad(69)= 'abi_g16',       dsis(69)= 'abi_g16',
   sattypes_rad(70)= 'abi_g17',       dsis(70)= 'abi_g17',
   sattypes_rad(71)= 'iasi_metop-c',  dsis(71)= 'iasi_metop-c',
   sattypes_rad(72)= 'viirs-m_npp',   dsis(72)= 'viirs-m_npp',
   sattypes_rad(73)= 'viirs-m_j1',    dsis(73)= 'viirs-m_j1',
   sattypes_rad(74)= 'avhrr_metop-c', dsis(74)= 'avhrr3_metop-c',
   sattypes_rad(75)= 'abi_g18',       dsis(75)= 'abi_g18',
   sattypes_rad(76)= 'ahi_himawari9', dsis(76)= 'ahi_himawari9',
   sattypes_rad(77)= 'viirs-m_j2',    dsis(77)= 'viirs-m_j2',
   sattypes_rad(78)= 'atms_n21',      dsis(78)= 'atms_n21',
   sattypes_rad(79)= 'cris-fsr_n21',  dsis(79)= 'cris-fsr_n21',
   sattypes_rad(80)= 'abi_g19',       dsis(80)= 'abi_g19',
   ${SATOBS_ENKF:-}
/
&ozobs_enkf
   sattypes_oz(1) = 'sbuv2_n16',
   sattypes_oz(2) = 'sbuv2_n17',
   sattypes_oz(3) = 'sbuv2_n18',
   sattypes_oz(4) = 'sbuv2_n19',
   sattypes_oz(5) = 'omi_aura',
   sattypes_oz(6) = 'gome_metop-a',
   sattypes_oz(7) = 'gome_metop-b',
   sattypes_oz(8) = 'mls30_aura',
   sattypes_oz(9) = 'ompsnp_npp',
   sattypes_oz(10) = 'ompstc8_npp',
   sattypes_oz(11) = 'ompstc8_n20',
   sattypes_oz(12) = 'ompsnp_n20',
   sattypes_oz(13) = 'ompslp_npp',
   sattypes_oz(14) = 'ompstc8_n21',
   sattypes_oz(15) = 'ompsnp_n21',
   sattypes_oz(16) = 'gome_metop-c',
   ${OZOBS_ENKF:-}
/
EOFnml

################################################################################
# Run enkf update

export OMP_NUM_THREADS=${NTHREADS_ENKF}
export pgm=${ENKFEXEC}
source prep_step

cpreq "${ENKFEXEC}" "${DATA}"
${APRUN_ENKF} "${DATA}/$(basename "${ENKFEXEC}")" 1> stdout 2> stderr && true
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "Failed to run the EnKF!"
fi

# Cat runtime output files.
cat stdout stderr > enkfstat.txt
cpfs enkfstat.txt "${COMOUT_ATMOS_ANALYSIS_STAT}/${APREFIX}enkfstat.txt"

################################################################################
#  Postprocessing

exit 0

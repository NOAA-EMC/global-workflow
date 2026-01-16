#! /usr/bin/env bash

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exglobal_atmos_analysis.sh
# Script description:  Makes a global model upper air analysis with GSI
#
# Author: Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-02
#
# Abstract: This script makes a global model analysis using the GSI
#
# $Id$
#
# Attributes:
#   Language: POSIX shell
#
#################################################################################

#  Set environment.

#  Directories.
# shellcheck disable=SC2153
cd "${DATA}" || exit 1

# Base variables
rCDUMP=${rCDUMP:-"gdas"}
GDUMP=${GDUMP:-"gdas"}

# Derived base variables
# shellcheck disable=SC2153
GDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${assim_freq} hours")
export GDATE
BDATE=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - 3 hours")
export bPDY=${BDATE:0:8}
export bcyc=${BDATE:8:2}

# Utilities
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-${USHgfs}/getncdimlen}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUN_GSI=${APRUN_GSI:-${APRUN:-""}}
NTHREADS_GSI=${NTHREADS_GSI:-${NTHREADS:-1}}

# Microphysics in the model; 99:ZC, 11:GFDLMP
export imp_physics=${imp_physics:-99}
lupp=${lupp:-".true."}
cnvw_option=${cnvw_option:-".false."}

# Observation usage options
cao_check=${cao_check:-".true."}
ta2tb=${ta2tb:-".true."}
optconv=${optconv:-0.06}
AIRS_CADS=${AIRS_CADS:-".false."}
IASI_CADS=${IASI_CADS:-".false."}
CRIS_CADS=${CRIS_CADS:-".false."}

# Diagnostic files options
lobsdiag_forenkf=${lobsdiag_forenkf:-".false."}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6,"}

# Dependent Scripts and Executables
GSIEXEC=${GSIEXEC:-${EXECgfs}/gsi.x}
export NTHREADS_CALCINC=${NTHREADS_CALCINC:-1}
export APRUN_CALCINC=${APRUN_CALCINC:-${APRUN:-""}}
export APRUN_CALCANL=${APRUN_CALCANL:-${APRUN:-""}}
export APRUN_CHGRES=${APRUN_CALCANL:-${APRUN:-""}}
export CALCINCEXEC=${CALCINCEXEC:-${EXECgfs}/calc_increment_ens.x}
export CALCINCNCEXEC=${CALCINCNCEXEC:-${EXECgfs}/calc_increment_ens_ncio.x}
export CALCANLEXEC=${CALCANLEXEC:-${EXECgfs}/calc_analysis.x}
export CHGRESNCEXEC=${CHGRESNCEXEC:-${EXECgfs}/enkf_chgres_recenter_nc.x}
export CHGRESINCEXEC=${CHGRESINCEXEC:-${EXECgfs}/interp_inc.x}
CHGRESEXEC=${CHGRESEXEC:-${EXECgfs}/enkf_chgres_recenter.x}
export NTHREADS_CHGRES=${NTHREADS_CHGRES:-24}
CALCINCPY=${CALCINCPY:-${USHgfs}/calcinc_gfs.py}

export hofx_2m_sfcfile=${hofx_2m_sfcfile:-".false."}
export ignore_2mQM=${ignore_2mQM:-".false."}

# Observations
OPREFIX=${OPREFIX:-""}
OSUFFIX=${OSUFFIX:-""}
PREPQC=${PREPQC:-${COMIN_OBS}/${OPREFIX}prepbufr${OSUFFIX}}
PREPQCPF=${PREPQCPF:-${COMIN_OBS}/${OPREFIX}prepbufr.acft_profiles${OSUFFIX}}
NSSTBF=${NSSTBF:-${COMIN_OBS}/${OPREFIX}nsstbufr${OSUFFIX}}
SATWND=${SATWND:-${COMIN_OBS}/${OPREFIX}satwnd.tm00.bufr_d${OSUFFIX}}
OSCATBF=${OSCATBF:-${COMIN_OBS}/${OPREFIX}oscatw.tm00.bufr_d${OSUFFIX}}
RAPIDSCATBF=${RAPIDSCATBF:-${COMIN_OBS}/${OPREFIX}rapidscatw.tm00.bufr_d${OSUFFIX}}
GSNDBF=${GSNDBF:-${COMIN_OBS}/${OPREFIX}goesnd.tm00.bufr_d${OSUFFIX}}
GSNDBF1=${GSNDBF1:-${COMIN_OBS}/${OPREFIX}goesfv.tm00.bufr_d${OSUFFIX}}
B1HRS2=${B1HRS2:-${COMIN_OBS}/${OPREFIX}1bhrs2.tm00.bufr_d${OSUFFIX}}
B1MSU=${B1MSU:-${COMIN_OBS}/${OPREFIX}1bmsu.tm00.bufr_d${OSUFFIX}}
B1HRS3=${B1HRS3:-${COMIN_OBS}/${OPREFIX}1bhrs3.tm00.bufr_d${OSUFFIX}}
B1HRS4=${B1HRS4:-${COMIN_OBS}/${OPREFIX}1bhrs4.tm00.bufr_d${OSUFFIX}}
B1AMUA=${B1AMUA:-${COMIN_OBS}/${OPREFIX}1bamua.tm00.bufr_d${OSUFFIX}}
B1AMUB=${B1AMUB:-${COMIN_OBS}/${OPREFIX}1bamub.tm00.bufr_d${OSUFFIX}}
AQUAAMUA=${AQUAAMUA:-${COMIN_OBS}/${OPREFIX}aquaamua.tm00.bufr_d${OSUFFIX}}
B1MHS=${B1MHS:-${COMIN_OBS}/${OPREFIX}1bmhs.tm00.bufr_d${OSUFFIX}}
ESHRS3=${ESHRS3:-${COMIN_OBS}/${OPREFIX}eshrs3.tm00.bufr_d${OSUFFIX}}
ESAMUA=${ESAMUA:-${COMIN_OBS}/${OPREFIX}esamua.tm00.bufr_d${OSUFFIX}}
ESAMUB=${ESAMUB:-${COMIN_OBS}/${OPREFIX}esamub.tm00.bufr_d${OSUFFIX}}
ESMHS=${ESMHS:-${COMIN_OBS}/${OPREFIX}esmhs.tm00.bufr_d${OSUFFIX}}
HRS3DB=${HRS3DB:-${COMIN_OBS}/${OPREFIX}hrs3db.tm00.bufr_d${OSUFFIX}}
AMUADB=${AMUADB:-${COMIN_OBS}/${OPREFIX}amuadb.tm00.bufr_d${OSUFFIX}}
AMUBDB=${AMUBDB:-${COMIN_OBS}/${OPREFIX}amubdb.tm00.bufr_d${OSUFFIX}}
MHSDB=${MHSDB:-${COMIN_OBS}/${OPREFIX}mhsdb.tm00.bufr_d${OSUFFIX}}
AIRSBF=${AIRSBF:-${COMIN_OBS}/${OPREFIX}airsev.tm00.bufr_d${OSUFFIX}}
IASIBF=${IASIBF:-${COMIN_OBS}/${OPREFIX}mtiasi.tm00.bufr_d${OSUFFIX}}
ESIASI=${ESIASI:-${COMIN_OBS}/${OPREFIX}esiasi.tm00.bufr_d${OSUFFIX}}
IASIDB=${IASIDB:-${COMIN_OBS}/${OPREFIX}iasidb.tm00.bufr_d${OSUFFIX}}
AMSREBF=${AMSREBF:-${COMIN_OBS}/${OPREFIX}amsre.tm00.bufr_d${OSUFFIX}}
AMSR2BF=${AMSR2BF:-${COMIN_OBS}/${OPREFIX}amsr2.tm00.bufr_d${OSUFFIX}}
GMI1CRBF=${GMI1CRBF:-${COMIN_OBS}/${OPREFIX}gmi1cr.tm00.bufr_d${OSUFFIX}}
SAPHIRBF=${SAPHIRBF:-${COMIN_OBS}/${OPREFIX}saphir.tm00.bufr_d${OSUFFIX}}
SEVIRIBF=${SEVIRIBF:-${COMIN_OBS}/${OPREFIX}sevcsr.tm00.bufr_d${OSUFFIX}}
AHIBF=${AHIBF:-${COMIN_OBS}/${OPREFIX}ahicsr.tm00.bufr_d${OSUFFIX}}
SSTVIIRS=${SSTVIIRS:-${COMIN_OBS}/${OPREFIX}sstvcw.tm00.bufr_d${OSUFFIX}}
ABIBF=${ABIBF:-${COMIN_OBS}/${OPREFIX}gsrcsr.tm00.bufr_d${OSUFFIX}}
CRISBF=${CRISBF:-${COMIN_OBS}/${OPREFIX}cris.tm00.bufr_d${OSUFFIX}}
ESCRIS=${ESCRIS:-${COMIN_OBS}/${OPREFIX}escris.tm00.bufr_d${OSUFFIX}}
CRISDB=${CRISDB:-${COMIN_OBS}/${OPREFIX}crisdb.tm00.bufr_d${OSUFFIX}}
CRISFSBF=${CRISFSBF:-${COMIN_OBS}/${OPREFIX}crisf4.tm00.bufr_d${OSUFFIX}}
ESCRISFS=${ESCRISFS:-${COMIN_OBS}/${OPREFIX}escrsf.tm00.bufr_d${OSUFFIX}}
CRISFSDB=${CRISFSDB:-${COMIN_OBS}/${OPREFIX}crsfdb.tm00.bufr_d${OSUFFIX}}
ATMSBF=${ATMSBF:-${COMIN_OBS}/${OPREFIX}atms.tm00.bufr_d${OSUFFIX}}
ESATMS=${ESATMS:-${COMIN_OBS}/${OPREFIX}esatms.tm00.bufr_d${OSUFFIX}}
ATMSDB=${ATMSDB:-${COMIN_OBS}/${OPREFIX}atmsdb.tm00.bufr_d${OSUFFIX}}
SSMITBF=${SSMITBF:-${COMIN_OBS}/${OPREFIX}ssmit.tm00.bufr_d${OSUFFIX}}
SSMISBF=${SSMISBF:-${COMIN_OBS}/${OPREFIX}ssmisu.tm00.bufr_d${OSUFFIX}}
SBUVBF=${SBUVBF:-${COMIN_OBS}/${OPREFIX}osbuv8.tm00.bufr_d${OSUFFIX}}
OMPSNPBF=${OMPSNPBF:-${COMIN_OBS}/${OPREFIX}ompsn8.tm00.bufr_d${OSUFFIX}}
OMPSTCBF=${OMPSTCBF:-${COMIN_OBS}/${OPREFIX}ompst8.tm00.bufr_d${OSUFFIX}}
OMPSLPBF=${OMPSLPBF:-${COMIN_OBS}/${OPREFIX}ompslp.tm00.bufr_d${OSUFFIX}}
GOMEBF=${GOMEBF:-${COMIN_OBS}/${OPREFIX}gome.tm00.bufr_d${OSUFFIX}}
OMIBF=${OMIBF:-${COMIN_OBS}/${OPREFIX}omi.tm00.bufr_d${OSUFFIX}}
MLSBF=${MLSBF:-${COMIN_OBS}/${OPREFIX}mls.tm00.bufr_d${OSUFFIX}}
SMIPCP=${SMIPCP:-${COMIN_OBS}/${OPREFIX}spssmi.tm00.bufr_d${OSUFFIX}}
TMIPCP=${TMIPCP:-${COMIN_OBS}/${OPREFIX}sptrmm.tm00.bufr_d${OSUFFIX}}
GPSROBF=${GPSROBF:-${COMIN_OBS}/${OPREFIX}gpsro.tm00.bufr_d${OSUFFIX}}
TCVITL=${TCVITL:-${COMIN_OBS}/${OPREFIX}syndata.tcvitals.tm00}
B1AVHAM=${B1AVHAM:-${COMIN_OBS}/${OPREFIX}avcsam.tm00.bufr_d${OSUFFIX}}
B1AVHPM=${B1AVHPM:-${COMIN_OBS}/${OPREFIX}avcspm.tm00.bufr_d${OSUFFIX}}
HDOB=${HDOB:-${COMIN_OBS}/${OPREFIX}hdob.tm00.bufr_d${OSUFFIX}}
OMIEFFNC=${OMIEFFNC:-${COMIN_OBS}/OMIeff-adj.${PDY}_${cyc}z.nc}
OMPSNMEFFNC=${OMPSNMEFFNC:-${COMIN_OBS}/OMPSNM.${PDY}_${cyc}z.nc}
OMPSNPNC=${OMPSNPNC:-${COMIN_OBS}/OMPSNP.${PDY}_${cyc}z.nc}
OMPSLPNC=${OMPSLPNC:-${COMIN_OBS}/OMPS-LPoz-Vis.${PDY}_${cyc}z.nc}
MLS55NC=${MLS55NC:-${COMIN_OBS}/MLS-v5.0-oz.${PDY}_${cyc}z.nc}
SAILDRONE=${SAILDRONE:-${COMIN_OBS}/${OPREFIX}saldrn.tm00.bufr_d${OSUFFIX}}
GSBBF=${GSBBF:-${COMIN_OBS}/${OPREFIX}gsbprf.tm00.bufr_d${OSUFFIX}}

# Guess files
GPREFIX=${GPREFIX:-""}
GSUFFIX=${GSUFFIX:-""}
SFCG03=${SFCG03:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f003.nc}
SFCG04=${SFCG04:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f004.nc}
SFCG05=${SFCG05:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f005.nc}
SFCGES=${SFCGES:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f006.nc}
SFCG07=${SFCG07:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f007.nc}
SFCG08=${SFCG08:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f008.nc}
SFCG09=${SFCG09:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}sfc.f009.nc}
ATMG03=${ATMG03:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f003.nc}
ATMG04=${ATMG04:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f004.nc}
ATMG05=${ATMG05:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f005.nc}
ATMGES=${ATMGES:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f006.nc}
ATMG07=${ATMG07:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f007.nc}
ATMG08=${ATMG08:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f008.nc}
ATMG09=${ATMG09:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}${GSUFFIX}atm.f009.nc}
GBIAS=${GBIAS:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}abias.txt}
GBIASPC=${GBIASPC:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}abias_pc.txt}
GBIASAIR=${GBIASAIR:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}abias_air.txt}
GRADSTAT=${GRADSTAT:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}radstat.tar}

# Analysis files
export APREFIX=${APREFIX:-""}
SFCANL=${SFCANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.sfc.a006.nc}
ATMANL=${ATMANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a006.nc}
ABIAS=${ABIAS:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias.txt}
ABIASPC=${ABIASPC:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias_pc.txt}
ABIASAIR=${ABIASAIR:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias_air.txt}
ABIASe=${ABIASe:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias_int.txt}
GSISTAT=${GSISTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}gsistat.txt}

# Increment files
ATMINC=${ATMINC:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i006.nc}
DTFANL=${DTFANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.dtf.i006.nc}

# Obs diag
RUN_SELECT=${RUN_SELECT:-"NO"}
USE_SELECT=${USE_SELECT:-"NO"}
USE_RADSTAT=${USE_RADSTAT:-"YES"}
SELECT_OBS=${SELECT_OBS:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}obsinput.tar}
GENDIAG=${GENDIAG:-"YES"}

# Set script / GSI control parameters
DOHYBVAR=${DOHYBVAR:-"NO"}
NMEM_ENS=${NMEM_ENS:-0}
export DONST=${DONST:-"NO"}
NST_GSI=${NST_GSI:-0}
NSTINFO=${NSTINFO:-0}
ZSEA1=${ZSEA1:-0}
ZSEA2=${ZSEA2:-0}
FAC_DTL=${FAC_DTL:-1}
FAC_TSL=${FAC_TSL:-1}
TZR_QC=${TZR_QC:-1}
USE_READIN_ANL_SFCMASK=${USE_READIN_ANL_SFCMASK:-.false.}
SMOOTH_ENKF=${SMOOTH_ENKF:-"YES"}
export DOIAU=${DOIAU:-"NO"}
DO_CALC_INCREMENT=${DO_CALC_INCREMENT:-"NO"}
DO_CALC_ANALYSIS=${DO_CALC_ANALYSIS:-"NO"}
export INCREMENTS_TO_ZERO=${INCREMENTS_TO_ZERO:-"'NONE'"}
USE_CORRELATED_OBERRS=${USE_CORRELATED_OBERRS:-"YES"}

# Get header information from Guess files
LONB=${LONB:-$(${NCLEN} "${ATMGES}" grid_xt)} # get LONB
LATB=${LATB:-$(${NCLEN} "${ATMGES}" grid_yt)} # get LATB
LEVS=${LEVS:-$(${NCLEN} "${ATMGES}" pfull)}   # get LEVS
JCAP=${JCAP:--9999}                           # there is no jcap in these files
if [[ "${JCAP}" -eq -9999 && "${LATB}" -ne -9999 ]]; then
    JCAP=$((LATB - 2))
fi

if [[ "${LONB}" -eq -9999 || "${LATB}" -eq -9999 || "${LEVS}" -eq -9999 || "${JCAP}" -eq -9999 ]]; then
    exit 9
fi

# Get header information from Ensemble Guess files
if [[ "${DOHYBVAR}" == "YES" ]]; then
    SFCGES_ENSMEAN=${SFCGES_ENSMEAN:-${COMIN_ATMOS_HISTORY_ENS_PREV}/${GPREFIX_ENS}ensmean.sfc.f006.nc}
    export ATMGES_ENSMEAN=${ATMGES_ENSMEAN:-${COMIN_ATMOS_HISTORY_ENS_PREV}/${GPREFIX_ENS}ensmean.atm.f006.nc}
    LONB_ENKF=${LONB_ENKF:-$(${NCLEN} "${ATMGES_ENSMEAN}" grid_xt)} # get LONB_ENKF
    LATB_ENKF=${LATB_ENKF:-$(${NCLEN} "${ATMGES_ENSMEAN}" grid_yt)} # get LATB_ENFK
    LEVS_ENKF=${LEVS_ENKF:-$(${NCLEN} "${ATMGES_ENSMEAN}" pfull)}   # get LATB_ENFK
    JCAP_ENKF=${JCAP_ENKF:--9999}                                   # again, no jcap in the netcdf files
    NLON_ENKF=${NLON_ENKF:-${LONB_ENKF}}
    NLAT_ENKF=${NLAT_ENKF:-$((LATB_ENKF + 2))}
    if [[ "${JCAP_ENKF}" -eq -9999 && "${LATB_ENKF}" -ne -9999 ]]; then
        JCAP_ENKF=$((LATB_ENKF - 2))
    fi
    if [[ "${LONB_ENKF}" -eq -9999 || "${LATB_ENKF}" -eq -9999 || "${LEVS_ENKF}" -eq -9999 || "${JCAP_ENKF}" -eq -9999 ]]; then
        exit 9
    fi
else
    LONB_ENKF=0 # just for if statement later
fi

# Get dimension information based on CASE
res=${CASE_HIST:1}
JCAP_CASE=$((res * 2 - 2))
LATB_CASE=$((res * 2))
LONB_CASE=$((res * 4))
export JCAP_CASE LATB_CASE LONB_CASE

# Set analysis resolution information
if [[ "${DOHYBVAR}" == "YES" ]]; then
    JCAP_A=${JCAP_A:-${JCAP_ENKF:-${JCAP}}}
    LONA=${LONA:-${LONB_ENKF:-${LONB}}}
    LATA=${LATA:-${LATB_ENKF:-${LATB}}}
else
    JCAP_A=${JCAP_A:-${JCAP}}
    LONA=${LONA:-${LONB}}
    LATA=${LATA:-${LATB}}
fi
NLON_A=${NLON_A:-${LONA}}
NLAT_A=${NLAT_A:-$((LATA + 2))}

DELTIM=${DELTIM:-$((3600 / (JCAP_A / 20)))}

# determine if writing or calculating increment
if [[ "${DO_CALC_INCREMENT}" == "YES" ]]; then
    write_fv3_increment=".false."
else
    write_fv3_increment=".true."
    WRITE_INCR_ZERO="incvars_to_zero= ${INCREMENTS_TO_ZERO},"
    WRITE_ZERO_STRAT="incvars_zero_strat= ${INCVARS_ZERO_STRAT},"
    WRITE_STRAT_EFOLD="incvars_efold= ${INCVARS_EFOLD},"
fi

# GSI Fix files
BERROR=${BERROR:-${FIXgfs}/gsi/Big_Endian/global_berror.l${LEVS}y${NLAT_A}.f77}
SATANGL=${SATANGL:-${FIXgfs}/gsi/global_satangbias.txt}
SATINFO=${SATINFO:-${FIXgfs}/gsi/global_satinfo.txt}
RADCLOUDINFO=${RADCLOUDINFO:-${FIXgfs}/gsi/cloudy_radiance_info.txt}
ATMSFILTER=${ATMSFILTER:-${FIXgfs}/gsi/atms_beamwidth.txt}
ANAVINFO=${ANAVINFO:-${FIXgfs}/gsi/global_anavinfo.l${LEVS}.txt}
CONVINFO=${CONVINFO:-${FIXgfs}/gsi/global_convinfo.txt}
vqcdat=${vqcdat:-${FIXgfs}/gsi/vqctp001.dat}
INSITUINFO=${INSITUINFO:-${FIXgfs}/gsi/global_insituinfo.txt}
OZINFO=${OZINFO:-${FIXgfs}/gsi/global_ozinfo.txt}
PCPINFO=${PCPINFO:-${FIXgfs}/gsi/global_pcpinfo.txt}
AEROINFO=${AEROINFO:-${FIXgfs}/gsi/global_aeroinfo.txt}
SCANINFO=${SCANINFO:-${FIXgfs}/gsi/global_scaninfo.txt}
HYBENSINFO=${HYBENSINFO:-${FIXgfs}/gsi/global_hybens_info.l${LEVS}.txt}
OBERROR=${OBERROR:-${FIXgfs}/gsi/prepobs_errtable.global}
OBS_INPUT=${OBS_INPUT:-${BUILD_GSINFO_DIR}/obs_input/obs_input_ops.txt}
HIRS_FIX=${HIRS_FIX:-${CRTM_FIX}}
BLACKLST=${BLACKLST:-${FIXgfs}/gsi/rejectlist_global.txt}

# GSI namelist
SETUP=${SETUP:-""}
GRIDOPTS=${GRIDOPTS:-""}
BKGVERR=${BKGVERR:-""}
ANBKGERR=${ANBKGERR:-""}
JCOPTS=${JCOPTS:-""}
STRONGOPTS=${STRONGOPTS:-""}
OBSQC=${OBSQC:-""}
OBSINPUT=${OBSINPUT:-""}
SUPERRAD=${SUPERRAD:-""}
SINGLEOB=${SINGLEOB:-""}
LAGDATA=${LAGDATA:-""}
HYBRID_ENSEMBLE=${HYBRID_ENSEMBLE:-""}
RAPIDREFRESH_CLDSURF=${RAPIDREFRESH_CLDSURF:-""}
CHEM=${CHEM:-""}
NST=${NST:-""}

# GSI Namelist parameters
if [[ "${DOHYBVAR}" == "YES" ]]; then
    l_hyb_ens=.true.
    export l4densvar=${l4densvar:-".false."}
    export lwrite4danl=${lwrite4danl:-".false."}
else
    l_hyb_ens=.false.
    export l4densvar=.false.
    export lwrite4danl=.false.
fi

# Set 4D-EnVar specific variables
if [[ "${DOHYBVAR}" == "YES" && "${l4densvar}" == ".true." && "${lwrite4danl}" == ".true." ]]; then
    ATMA03=${ATMA03:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a003.nc}
    ATMI03=${ATMI03:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i003.nc}
    ATMA04=${ATMA04:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a004.nc}
    ATMI04=${ATMI04:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i004.nc}
    ATMA05=${ATMA05:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a005.nc}
    ATMI05=${ATMI05:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i005.nc}
    ATMA07=${ATMA07:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a007.nc}
    ATMI07=${ATMI07:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i007.nc}
    ATMA08=${ATMA08:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a008.nc}
    ATMI08=${ATMI08:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i008.nc}
    ATMA09=${ATMA09:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}analysis.atm.a009.nc}
    ATMI09=${ATMI09:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.atm.i009.nc}
fi

##############################################################
# Fixed files
${NLN} "${BERROR}" berror_stats
${NLN} "${SATANGL}" satbias_angle
if [[ "${SATINFO}" == "generate" ]]; then
    # shellcheck disable=SC2153
    "${USHgfs}/create_gsi_info.sh" sat "${PDY}${cyc}" "${DATA}"
else
    ${NLN} "${SATINFO}" satinfo
fi
${NLN} "${RADCLOUDINFO}" cloudy_radiance_info.txt
${NLN} "${ATMSFILTER}" atms_beamwidth.txt
${NLN} "${ANAVINFO}" anavinfo
if [[ "${CONVINFO}" == "generate" ]]; then
    "${USHgfs}/create_gsi_info.sh" conv "${PDY}${cyc}" "${DATA}" "${USE_2M_OBS}"
else
    ${NLN} "${CONVINFO}" convinfo
fi
${NLN} "${vqcdat}" vqctp001.dat
${NLN} "${INSITUINFO}" insituinfo
if [[ "${OZINFO}" == "generate" ]]; then
    "${USHgfs}/create_gsi_info.sh" oz "${PDY}${cyc}" "${DATA}"
else
    ${NLN} "${OZINFO}" ozinfo
fi
${NLN} "${PCPINFO}" pcpinfo
${NLN} "${AEROINFO}" aeroinfo
${NLN} "${SCANINFO}" scaninfo
${NLN} "${HYBENSINFO}" hybens_info
${NLN} "${OBERROR}" errtable
${NLN} "${BLACKLST}" blacklist

${NLN} "${FIXgfs}/gsi/AIRS_CLDDET.NL" AIRS_CLDDET.NL
${NLN} "${FIXgfs}/gsi/CRIS_CLDDET.NL" CRIS_CLDDET.NL
${NLN} "${FIXgfs}/gsi/IASI_CLDDET.NL" IASI_CLDDET.NL

#If using correlated error, link to the covariance files
if [[ "${USE_CORRELATED_OBERRS}" == "YES" ]]; then
    if grep -q "Rcov" "${ANAVINFO}"; then
        mapfile -t covfile_array < <(find "${FIXgfs}/gsi/" -name "Rcov*")
        if ((${#covfile_array[@]} > 0)); then
            for covfile in "${covfile_array[@]}"; do
                covfile_base=$(basename "${covfile}")
                # shellcheck disable=SC2153
                ${NLN} "${covfile}" "${DATA}/${covfile_base}"
            done
            echo "using correlated obs error"
        else
            export err=1
            err_exit "FATAL ERROR: Satellite error covariance files (Rcov) are missing. Check for the required Rcov files in ${ANAVINFO}"
        fi
    else
        export err=1
        err_exit "FATAL ERROR: Satellite error covariance info missing in ${ANAVINFO}"
    fi

    # Correlated error utlizes mkl lapack.  Found it necesary to fix the
    # number of mkl threads to ensure reproducible results independent
    # of the job configuration.
    export MKL_NUM_THREADS=1

else
    echo "not using correlated obs error"
fi

##############################################################
# CRTM Spectral and Transmittance coefficients
mkdir -p crtm_coeffs
for file in $(awk '{if($1!~"!"){print $1}}' satinfo | sort | uniq); do
    if [[ ${file:0:4} == "hirs" ]]; then
        ${NLN} "${HIRS_FIX}/${file}.SpcCoeff.bin" "./crtm_coeffs/${file}.SpcCoeff.bin"
    else
        ${NLN} "${CRTM_FIX}/${file}.SpcCoeff.bin" "./crtm_coeffs/${file}.SpcCoeff.bin"
    fi
    ${NLN} "${CRTM_FIX}/${file}.TauCoeff.bin" "./crtm_coeffs/${file}.TauCoeff.bin"
done
${NLN} "${CRTM_FIX}/amsua_metop-a_v2.SpcCoeff.bin" "./crtm_coeffs/amsua_metop-a_v2.SpcCoeff.bin"

${NLN} "${CRTM_FIX}/Nalli.IRwater.EmisCoeff.bin" "./crtm_coeffs/Nalli.IRwater.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.IRice.EmisCoeff.bin" "./crtm_coeffs/NPOESS.IRice.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.IRland.EmisCoeff.bin" "./crtm_coeffs/NPOESS.IRland.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.IRsnow.EmisCoeff.bin" "./crtm_coeffs/NPOESS.IRsnow.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.VISice.EmisCoeff.bin" "./crtm_coeffs/NPOESS.VISice.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.VISland.EmisCoeff.bin" "./crtm_coeffs/NPOESS.VISland.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.VISsnow.EmisCoeff.bin" "./crtm_coeffs/NPOESS.VISsnow.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/NPOESS.VISwater.EmisCoeff.bin" "./crtm_coeffs/NPOESS.VISwater.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/FASTEM6.MWwater.EmisCoeff.bin" "./crtm_coeffs/FASTEM6.MWwater.EmisCoeff.bin"
${NLN} "${CRTM_FIX}/AerosolCoeff.bin" "./crtm_coeffs/AerosolCoeff.bin"

case "${imp_physics}" in
    8)
        echo "Using CRTM Thompson cloud optical table"
        ${NLN} "${CRTM_FIX}/CloudCoeff.Thompson08.-109z-1.bin" ./crtm_coeffs/CloudCoeff.bin
        ;;
    11)
        echo "Using CRTM GFDL cloud optical table"
        ${NLN} "${CRTM_FIX}/CloudCoeff.GFDLFV3.-109z-1.bin" ./crtm_coeffs/CloudCoeff.bin
        ;;
    *)
        echo "FATAL ERROR: INVALID imp_physics = ${imp_physics}"
        export err=1
        err_exit "No valid CRTM cloud optical table found for imp_physics = ${imp_physics}"
        ;;
esac

##############################################################
# Observational data
${NLN} "${PREPQC}" prepbufr
${NLN} "${PREPQCPF}" prepbufr_profl
${NLN} "${SATWND}" satwndbufr
${NLN} "${OSCATBF}" oscatbufr
${NLN} "${RAPIDSCATBF}" rapidscatbufr
${NLN} "${GSNDBF}" gsndrbufr
${NLN} "${GSNDBF1}" gsnd1bufr
${NLN} "${B1MSU}" msubufr
${NLN} "${B1AMUA}" amsuabufr
${NLN} "${B1AMUB}" amsubbufr
${NLN} "${B1MHS}" mhsbufr
${NLN} "${B1HRS2}" hirs2bufr
${NLN} "${B1HRS3}" hirs3bufr
${NLN} "${B1HRS4}" hirs4bufr
${NLN} "${ESAMUA}" amsuabufrears
${NLN} "${ESAMUB}" amsubbufrears
#${NLN}  "${ESMHS}"           mhsbufrears
${NLN} "${AMUADB}" amsuabufr_db
${NLN} "${AMUBDB}" amsubbufr_db
#${NLN}  "${MHSDB}"           mhsbufr_db
${NLN} "${SBUVBF}" sbuvbufr
${NLN} "${OMPSNPBF}" ompsnpbufr
${NLN} "${OMPSLPBF}" ompslpbufr
${NLN} "${OMPSTCBF}" ompstcbufr
${NLN} "${GOMEBF}" gomebufr
${NLN} "${OMIBF}" omibufr
${NLN} "${MLSBF}" mlsbufr
${NLN} "${SMIPCP}" ssmirrbufr
${NLN} "${TMIPCP}" tmirrbufr
${NLN} "${AIRSBF}" airsbufr
${NLN} "${IASIBF}" iasibufr
${NLN} "${ESIASI}" iasibufrears
${NLN} "${IASIDB}" iasibufr_db
${NLN} "${AMSREBF}" amsrebufr
${NLN} "${AMSR2BF}" amsr2bufr
${NLN} "${GMI1CRBF}" gmibufr
${NLN} "${SAPHIRBF}" saphirbufr
${NLN} "${SEVIRIBF}" seviribufr
${NLN} "${CRISBF}" crisbufr
${NLN} "${ESCRIS}" crisbufrears
${NLN} "${CRISDB}" crisbufr_db
${NLN} "${CRISFSBF}" crisfsbufr
${NLN} "${ESCRISFS}" crisfsbufrears
${NLN} "${CRISFSDB}" crisfsbufr_db
${NLN} "${ATMSBF}" atmsbufr
${NLN} "${ESATMS}" atmsbufrears
${NLN} "${ATMSDB}" atmsbufr_db
${NLN} "${SSMITBF}" ssmitbufr
${NLN} "${SSMISBF}" ssmisbufr
${NLN} "${GPSROBF}" gpsrobufr
${NLN} "${TCVITL}" tcvitl
${NLN} "${B1AVHAM}" avhambufr
${NLN} "${B1AVHPM}" avhpmbufr
${NLN} "${AHIBF}" ahibufr
${NLN} "${ABIBF}" abibufr
${NLN} "${HDOB}" hdobbufr
${NLN} "${SSTVIIRS}" sstviirs
${NLN} "${SAILDRONE}" sdbufr
${NLN} "${GSBBF}" wbbufr

# NASA ozone (netcdf) from NNJA
${NLN} "${OMIEFFNC}" omieffnc
${NLN} "${OMPSNMEFFNC}" ompsnmeffnc
${NLN} "${OMPSNPNC}" ompsnpnc
${NLN} "${OMPSLPNC}" ompslpnc
${NLN} "${MLS55NC}" mls55nc
# NASA airs aqua amsua (bufr) from NNJA
${NLN} "${AQUAAMUA}" aquabufr

if [[ "${DONST}" == "YES" ]]; then
    ${NLN} "${NSSTBF}" nsstbufr
fi

##############################################################
# Required bias guess files
cpreq "${GBIAS}" satbias_in
cpreq "${GBIASPC}" satbias_pc
cpreq "${GBIASAIR}" aircftbias_in

##############################################################
# Required model guess files
${NLN} "${ATMG03}" sigf03
${NLN} "${ATMGES}" sigf06
${NLN} "${ATMG09}" sigf09

${NLN} "${SFCG03}" sfcf03
${NLN} "${SFCGES}" sfcf06
${NLN} "${SFCG09}" sfcf09

if [[ -f "${ATMG04}" ]]; then
    ${NLN} "${ATMG04}" sigf04
fi
if [[ -f "${ATMG05}" ]]; then
    ${NLN} "${ATMG05}" sigf05
fi
if [[ -f "${ATMG07}" ]]; then
    ${NLN} "${ATMG07}" sigf07
fi
if [[ -f "${ATMG08}" ]]; then
    ${NLN} "${ATMG08}" sigf08
fi

if [[ -f "${SFCG04}" ]]; then
    ${NLN} "${SFCG04}" sfcf04
fi
if [[ -f "${SFCG05}" ]]; then
    ${NLN} "${SFCG05}" sfcf05
fi
if [[ -f "${SFCG07}" ]]; then
    ${NLN} "${SFCG07}" sfcf07
fi
if [[ -f "${SFCG08}" ]]; then
    ${NLN} "${SFCG08}" sfcf08
fi

if [[ "${DOHYBVAR}" == "YES" ]]; then

    # Link ensemble members
    mkdir -p ensemble_data

    ENKF_SUFFIX="smooth"
    if [[ "${SMOOTH_ENKF}" == "NO" ]]; then
        ENKF_SUFFIX=""
    fi

    fhrs="06"
    if [[ "${l4densvar}" == ".true." ]]; then
        fhrs=$(seq -s ' ' -f '%02g' 3 9)
        nhr_obsbin=1
    fi

    for imem in $(seq 1 "${NMEM_ENS}"); do
        memchar="mem$(printf %03i "${imem}")"
        MEMDIR=${memchar} RUN=${GDUMP_ENS} YMD=${gPDY} HH=${gcyc} declare_from_tmpl \
            COMIN_ATMOS_HISTORY:COM_ATMOS_HISTORY_TMPL

        for fhr in ${fhrs}; do
            ${NLN} "${COMIN_ATMOS_HISTORY}/${GPREFIX_ENS}${ENKF_SUFFIX}atm.f0${fhr}.nc" "./ensemble_data/sigf${fhr}_ens_${memchar}"
            if [[ "${cnvw_option}" == ".true." ]]; then
                ${NLN} "${COMIN_ATMOS_HISTORY}/${GPREFIX_ENS}sfc.f0${fhr}.nc" "./ensemble_data/sfcf${fhr}_ens_${memchar}"
            fi
        done
    done

fi

##############################################################
# Handle inconsistent surface mask between background, ensemble and analysis grids
# This needs re-visiting in the context of NSST; especially references to JCAP*
if [[ "${JCAP}" -ne "${JCAP_A}" ]]; then
    if [[ "${DOHYBVAR}" == "YES" && "${JCAP_A}" == "${JCAP_ENKF}" ]]; then
        if [[ -e "${SFCGES_ENSMEAN}" ]]; then
            USE_READIN_ANL_SFCMASK=.true.
            ${NLN} "${SFCGES_ENSMEAN}" sfcf06_anlgrid
        else
            echo "Warning: Inconsistent sfc mask between analysis and ensemble grids, GSI will interpolate"
        fi
    else
        echo "Warning: Inconsistent sfc mask between analysis and background grids, GSI will interpolate"
    fi
fi

##############################################################
# Output files
${NLN} "${ATMANL}" siganl
${NLN} "${ATMINC}" siginc.nc
if [[ "${DOHYBVAR}" == "YES" && "${l4densvar}" == ".true." && "${lwrite4danl}" == ".true." ]]; then
    ${NLN} "${ATMA03}" siga03
    ${NLN} "${ATMI03}" sigi03.nc
    ${NLN} "${ATMA04}" siga04
    ${NLN} "${ATMI04}" sigi04.nc
    ${NLN} "${ATMA05}" siga05
    ${NLN} "${ATMI05}" sigi05.nc
    ${NLN} "${ATMA07}" siga07
    ${NLN} "${ATMI07}" sigi07.nc
    ${NLN} "${ATMA08}" siga08
    ${NLN} "${ATMI08}" sigi08.nc
    ${NLN} "${ATMA09}" siga09
    ${NLN} "${ATMI09}" sigi09.nc
fi
${NLN} "${ABIAS}" satbias_out
${NLN} "${ABIASPC}" satbias_pc.out
${NLN} "${ABIASAIR}" aircftbias_out

if [[ "${DONST}" == "YES" ]]; then
    ${NLN} "${DTFANL}" dtfanl
fi

# If requested, link (and if tarred, de-tar obsinput.tar) into obs_input.* files
if [[ "${USE_SELECT}" == "YES" ]]; then
    rm -f obs_input.*
    nl=$(file "${SELECT_OBS}" | cut -d: -f2 | grep -c tar)
    if [[ ${nl} -eq 1 ]]; then
        rm -f obsinput.tar
        ${NLN} "${SELECT_OBS}" obsinput.tar
        tar -xvf obsinput.tar
        rm -f obsinput.tar
    else
        for filetop in "${SELECT_OBS}/"obs_input.*; do
            fileloc=$(basename "${filetop}")
            ${NLN} "${filetop}" "${fileloc}"
        done
    fi
fi

##############################################################
# If requested, copy and de-tar guess radstat file
if [[ "${USE_RADSTAT}" == "YES" ]]; then
    rm -f "${DATA}/unzip_diag.sh"
    cat > "${DATA}/unzip_diag.sh" << EOF
#!/bin/bash
diag_file=\$1
diag_suffix=\$2
fname=\$(echo \$diag_file | cut -d'.' -f1)
fdate=\$(echo \$diag_file | cut -d'.' -f2)
${UNCOMPRESS} \$diag_file
fnameges=\$(echo \$fname | sed 's/_ges//g')
${NMV} \$fname.\$fdate\$diag_suffix \$fnameges
EOF
    chmod 755 "${DATA}/unzip_diag.sh"

    rm -f "${DATA}/cmdfile"
    cpreq "${GRADSTAT}" radstat.tar
    tar -xvf radstat.tar
    listdiag=$(find ./ -maxdepth 1 -path "./diag_*_ges.*" -type f)
    for type in ${listdiag}; do
        diag_file=$(basename "${type}")
        echo "${DATA}/unzip_diag.sh ${diag_file} ${DIAG_SUFFIX:-}.nc4" >> "${DATA}/cmdfile"
    done

    "${USHgfs}/run_mpmd.sh" "${DATA}/cmdfile" && true
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "Failed to unzip rad diag file!"
    fi
fi # if [[ $USE_RADSTAT == "YES" ]

##############################################################
# GSI Namelist options
if [[ "${DOHYBVAR}" == "YES" ]]; then
    HYBRID_ENSEMBLE="n_ens=${NMEM_ENS},jcap_ens=${JCAP_ENKF},nlat_ens=${NLAT_ENKF},nlon_ens=${NLON_ENKF},jcap_ens_test=${JCAP_ENKF},${HYBRID_ENSEMBLE}"
    if [[ "${l4densvar}" == ".true." ]]; then
        SETUP="niter(1)=50,niter(2)=150,niter_no_qc(1)=25,niter_no_qc(2)=0,thin4d=.true.,ens_nstarthr=3,gmi_method=4,l4densvar=${l4densvar},lwrite4danl=${lwrite4danl},${SETUP}"
        JCOPTS="ljc4tlevs=.true.,${JCOPTS}"
        STRONGOPTS="tlnmc_option=3,${STRONGOPTS}"
        OBSQC="c_varqc=0.04,${OBSQC}"
    fi
fi

if [[ "${DONST}" == "YES" ]]; then
    NST="nstinfo=${NSTINFO},fac_dtl=${FAC_DTL},fac_tsl=${FAC_TSL},zsea1=${ZSEA1},zsea2=${ZSEA2},${NST}"
fi

OBS_INPUT_TABLE=$(cat "${OBS_INPUT}")

##############################################################
# Create global_gsi namelist
cat > gsiparm.anl << EOF
&SETUP
  miter=2,
  niter(1)=100,niter(2)=100,
  niter_no_qc(1)=50,niter_no_qc(2)=0,
  write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
  qoption=2,
  gencode=${IGEN:-0},deltim=${DELTIM},
  factqmin=0.5,factqmax=0.0002,
  iguess=-1,
  tzr_qc=${TZR_QC},
  oneobtest=.false.,retrieval=.false.,l_foto=.false.,
  use_pbl=.false.,use_compress=.true.,nsig_ext=45,gpstop=50.,commgpstop=45.,commgpserrinf=1.0,
  use_gfs_nemsio=.false.,use_gfs_ncio=.true.,sfcnst_comb=.true.,
  use_readin_anl_sfcmask=${USE_READIN_ANL_SFCMASK},
  lrun_subdirs=.true.,
  crtm_coeffs_path='./crtm_coeffs/',
  newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
  diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,nhr_obsbin=${nhr_obsbin:-3},
  cwoption=3,imp_physics=${imp_physics},lupp=${lupp},cnvw_option=${cnvw_option},cao_check=${cao_check},
  netcdf_diag=.true.,binary_diag=.false.,
  lobsdiag_forenkf=${lobsdiag_forenkf},
  write_fv3_incr=${write_fv3_increment},
  nhr_anal=${IAUFHRS},
  ta2tb=${ta2tb},optconv=${optconv},
  ${WRITE_INCR_ZERO}
  ${WRITE_ZERO_STRAT}
  ${WRITE_STRAT_EFOLD}
  ${SETUP}
/
&GRIDOPTS
  JCAP_B=${JCAP},JCAP=${JCAP_A},NLAT=${NLAT_A},NLON=${NLON_A},nsig=${LEVS},
  regional=.false.,nlayers(63)=3,nlayers(64)=6,
  ${GRIDOPTS}
/
&BKGERR
  vs=0.7,
  hzscl=1.7,0.8,0.5,
  hswgt=0.45,0.3,0.25,
  bw=0.0,norsp=4,
  bkgv_flowdep=.true.,bkgv_rewgtfct=1.5,
  bkgv_write=.false.,
  cwcoveqqcov=.false.,
  ${BKGVERR}
/
&ANBKGERR
  anisotropic=.false.,
  ${ANBKGERR}
/
&JCOPTS
  ljcdfi=.false.,alphajc=0.0,ljcpdry=.true.,bamp_jcpdry=5.0e7,
  ${JCOPTS}
/
&STRONGOPTS
  tlnmc_option=2,nstrong=1,nvmodes_keep=8,period_max=6.,period_width=1.5,
  ${STRONGOPTS}
/
&OBSQC
  dfact=0.75,dfact1=3.0,noiqc=.true.,oberrflg=.false.,c_varqc=0.02,
  use_poq7=.true.,qc_noirjaco3_pole=.false.,vqc=.false.,nvqc=.true.,
  aircraft_t_bc=.true.,biaspredt=1.0e5,upd_aircraft=.true.,cleanup_tail=.true.,
  tcp_width=70.0,tcp_ermax=7.35,airs_cads=${AIRS_CADS},cris_cads=${CRIS_CADS},
  iasi_cads=${IASI_CADS},blacklst=.true.,
  ${OBSQC}
/
&OBS_INPUT
  dmesh(1)=145.0,dmesh(2)=150.0,dmesh(3)=100.0,dmesh(4)=50.0,time_window_max=3.0,
  hofx_2m_sfcfile=${hofx_2m_sfcfile},ignore_2mQM=${ignore_2mQM},
  ${OBSINPUT}
/
OBS_INPUT::
${OBS_INPUT_TABLE}
::
&SUPEROB_RADAR
  ${SUPERRAD}
/
&LAG_DATA
  ${LAGDATA}
/
&HYBRID_ENSEMBLE
  l_hyb_ens=${l_hyb_ens},
  generate_ens=.false.,
  beta_s0=0.125,readin_beta=.false.,
  s_ens_h=1000.0,300.0,150.0,685.0,219.2,s_ens_v=-0.5,-0.5,-0.5,0.0,0.0,
  readin_localization=.false.,global_spectral_filter_sd=.false.,
  r_ensloccov4scl=1,nsclgrp=3,naensloc=5,
  aniso_a_en=.false.,oz_univ_static=.false.,uv_hyb_ens=.true.,
  ensemble_path='./ensemble_data/',
  ens_fast_read=.true.,
  ${HYBRID_ENSEMBLE}
/
&RAPIDREFRESH_CLDSURF
  dfi_radar_latent_heat_time_period=30.0,
  ${RAPIDREFRESH_CLDSURF}
/
&CHEM
  ${CHEM}
/
&SINGLEOB_TEST
  maginnov=0.1,magoberr=0.1,oneob_type='t',
  oblat=45.,oblon=180.,obpres=1000.,obdattim=${PDY}${cyc},
  obhourset=0.,
  ${SINGLEOB}
/
&NST
  nst_gsi=${NST_GSI},
  ${NST}
/
EOF
cat gsiparm.anl

##############################################################
#  Run gsi analysis

export OMP_NUM_THREADS=${NTHREADS_GSI}
export pgm=${GSIEXEC}
source prep_step

cpreq "${GSIEXEC}" "${DATA}"
${APRUN_GSI} "${DATA}/$(basename "${GSIEXEC}")" 1>&1 2>&2
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "Failed to run the GSI analysis!"
fi

##############################################################
# If full analysis field written, calculate analysis increment
# here before releasing FV3 forecast
if [[ "${DO_CALC_INCREMENT}" == "YES" ]]; then
    ${CALCINCPY}
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "Failed to calculate the analysis increment!"
    fi
fi

##############################################################
# For eupd
if [[ -s satbias_out.int ]]; then
    cpfs satbias_out.int "${ABIASe}"
else
    cpfs satbias_in "${ABIASe}"
fi

# Cat runtime output files.
cat fort.2* > "${GSISTAT}"

# If requested, create obsinput tarball from obs_input.* files
if [[ ${RUN_SELECT} == "YES" ]]; then
    echo "$(date) START tar obs_input" >&2
    if [[ -s obsinput.tar ]]; then
        rm -f obsinput.tar
    fi
    ${NLN} "${SELECT_OBS}" obsinput.tar
    ${CHGRP_CMD} obs_input.*
    tar -cvf obsinput.tar obs_input.*
    chmod 750 "${SELECT_OBS}"
    ${CHGRP_CMD} "${SELECT_OBS}"
    rm -f obsinput.tar
    echo "$(date) END tar obs_input" >&2
fi

################################################################################
# Send alerts
if [[ "${SENDDBN}" == "YES" ]]; then
    if [[ "${RUN}" == "gfs" ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL GFS_abias "${job}" "${ABIAS}"
    fi
fi

################################################################################
# Postprocessing
cd "${DATA}" || exit 1

##############################################################
# Add this statement to release the forecast job once the
# atmopsheric analysis and updated surface RESTARTS are
# available.  Do not release forecast when RUN=enkf
##############################################################
if [[ "${SENDECF}" == "YES" && "${RUN}" != "enkf" ]]; then
    ecflow_client --event release_fcst
fi

# Diagnostic files
# if requested, GSI diagnostic file directories for use later
if [[ "${GENDIAG}" == "YES" ]]; then
    tar -cvf gsidiags.tar dir.????
    export err=$?
    if [[ ${err} -ne 0 ]]; then
        err_exit "Failed to tar GSI diagnostic directories!"
    fi
    cpfs gsidiags.tar "${COMOUT_ATMOS_ANALYSIS}/${APREFIX}gsidiags${DIAG_SUFFIX:-}.tar"
fi

echo "${rCDUMP} ${PDY}${cyc} atminc done at $(date)" > "${COMOUT_ATMOS_ANALYSIS}/${APREFIX}increment.done.txt"

################################################################################

exit "${err}"

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

source "${USHgfs}/preamble.sh"

#  Directories.
pwd=$(pwd)

# Base variables
CDATE=${CDATE:-"2001010100"}
rCDUMP=${rCDUMP:-"gdas"}
GDUMP=${GDUMP:-"gdas"}

# Derived base variables
GDATE=$(${NDATE} -${assim_freq} ${CDATE})
BDATE=$(${NDATE} -3 ${CDATE})
PDY=$(echo ${CDATE} | cut -c1-8)
cyc=$(echo ${CDATE} | cut -c9-10)
bPDY=$(echo ${BDATE} | cut -c1-8)
bcyc=$(echo ${BDATE} | cut -c9-10)

# Utilities
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export NCLEN=${NCLEN:-${USHgfs}/getncdimlen}
COMPRESS=${COMPRESS:-gzip}
UNCOMPRESS=${UNCOMPRESS:-gunzip}
APRUNCFP=${APRUNCFP:-""}
APRUN_GSI=${APRUN_GSI:-${APRUN:-""}}
NTHREADS_GSI=${NTHREADS_GSI:-${NTHREADS:-1}}

# Microphysics in the model; 99:ZC, 11:GFDLMP
export imp_physics=${imp_physics:-99}
lupp=${lupp:-".true."}
cnvw_option=${cnvw_option:-".false."}

# Observation usage options
cao_check=${cao_check:-".true."}
ta2tb=${ta2tb:-".true."}

# Diagnostic files options
lobsdiag_forenkf=${lobsdiag_forenkf:-".false."}
netcdf_diag=${netcdf_diag:-".true."}
binary_diag=${binary_diag:-".false."}

# IAU
DOIAU=${DOIAU:-"NO"}
export IAUFHRS=${IAUFHRS:-"6"}

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

# OPS flags
RUN=${RUN:-""}
SENDECF=${SENDECF:-"NO"}
SENDDBN=${SENDDBN:-"NO"}
RUN_GETGES=${RUN_GETGES:-"NO"}
GETGESSH=${GETGESSH:-"getges.sh"}
export gesenvir=${gesenvir:-${envir}}
 
export hofx_2m_sfcfile=${hofx_2m_sfcfile:-".false."}

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
#B1HRS2=${B1HRS2:-${COMIN_OBS}/${OPREFIX}1bhrs2.tm00.bufr_d${OSUFFIX}} # HIRS temporarily disabled due to CRTM versioning issues
B1MSU=${B1MSU:-${COMIN_OBS}/${OPREFIX}1bmsu.tm00.bufr_d${OSUFFIX}}
#B1HRS3=${B1HRS3:-${COMIN_OBS}/${OPREFIX}1bhrs3.tm00.bufr_d${OSUFFIX}} # HIRS temporarily disabled due to CRTM versioning issues
#B1HRS4=${B1HRS4:-${COMIN_OBS}/${OPREFIX}1bhrs4.tm00.bufr_d${OSUFFIX}} # HIRS temporarily disabled due to CRTM versioning issues
B1AMUA=${B1AMUA:-${COMIN_OBS}/${OPREFIX}1bamua.tm00.bufr_d${OSUFFIX}}
B1AMUB=${B1AMUB:-${COMIN_OBS}/${OPREFIX}1bamub.tm00.bufr_d${OSUFFIX}}
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
GMI1CRBF=${GMI1CRBF:-${COMIN_OBS}/${OPREFIX}gmi1cr.tm00.bufr_d${OSUFFIX}} # GMI temporarily disabled due to array overflow.
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

# Guess files
GPREFIX=${GPREFIX:-""}
GSUFFIX=${GSUFFIX:-".nc"}
SFCG03=${SFCG03:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf003${GSUFFIX}}
SFCG04=${SFCG04:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf004${GSUFFIX}}
SFCG05=${SFCG05:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf005${GSUFFIX}}
SFCGES=${SFCGES:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf006${GSUFFIX}}
SFCG07=${SFCG07:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf007${GSUFFIX}}
SFCG08=${SFCG08:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf008${GSUFFIX}}
SFCG09=${SFCG09:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}sfcf009${GSUFFIX}}
ATMG03=${ATMG03:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf003${GSUFFIX}}
ATMG04=${ATMG04:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf004${GSUFFIX}}
ATMG05=${ATMG05:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf005${GSUFFIX}}
ATMGES=${ATMGES:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf006${GSUFFIX}}
ATMG07=${ATMG07:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf007${GSUFFIX}}
ATMG08=${ATMG08:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf008${GSUFFIX}}
ATMG09=${ATMG09:-${COMIN_ATMOS_HISTORY_PREV}/${GPREFIX}atmf009${GSUFFIX}}
GBIAS=${GBIAS:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}abias}
GBIASPC=${GBIASPC:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}abias_pc}
GBIASAIR=${GBIASAIR:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}abias_air}
GRADSTAT=${GRADSTAT:-${COMIN_ATMOS_ANALYSIS_PREV}/${GPREFIX}radstat}

# Analysis files
export APREFIX=${APREFIX:-""}
SFCANL=${SFCANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}sfcanl.nc}
DTFANL=${DTFANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}dtfanl.nc}
ATMANL=${ATMANL:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmanl.nc}
ABIAS=${ABIAS:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias}
ABIASPC=${ABIASPC:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias_pc}
ABIASAIR=${ABIASAIR:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias_air}
ABIASe=${ABIASe:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}abias_int}
RADSTAT=${RADSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}radstat}
GSISTAT=${GSISTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}gsistat}
PCPSTAT=${PCPSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}pcpstat}
CNVSTAT=${CNVSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}cnvstat}
OZNSTAT=${OZNSTAT:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}oznstat}

# Increment files
ATMINC=${ATMINC:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atminc.nc}

# Obs diag
RUN_SELECT=${RUN_SELECT:-"NO"}
USE_SELECT=${USE_SELECT:-"NO"}
USE_RADSTAT=${USE_RADSTAT:-"YES"}
SELECT_OBS=${SELECT_OBS:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}obsinput}
GENDIAG=${GENDIAG:-"YES"}
DIAG_SUFFIX=${DIAG_SUFFIX:-""}
if [ ${netcdf_diag} = ".true." ] ; then
   DIAG_SUFFIX="${DIAG_SUFFIX}.nc4"
fi
DIAG_COMPRESS=${DIAG_COMPRESS:-"YES"}
DIAG_TARBALL=${DIAG_TARBALL:-"YES"}
USE_CFP=${USE_CFP:-"NO"}
CFP_MP=${CFP_MP:-"NO"}
nm=""
if [ ${CFP_MP} = "YES" ]; then
    nm=0
fi
DIAG_DIR=${DIAG_DIR:-${COMOUT_ATMOS_ANALYSIS}/gsidiags}

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
LONB=${LONB:-$(${NCLEN} ${ATMGES} grid_xt)} # get LONB
LATB=${LATB:-$(${NCLEN} ${ATMGES} grid_yt)} # get LATB
LEVS=${LEVS:-$(${NCLEN} ${ATMGES} pfull)} # get LEVS
JCAP=${JCAP:--9999} # there is no jcap in these files
[ ${JCAP} -eq -9999 -a ${LATB} -ne -9999 ] && JCAP=$((LATB-2))
[ ${LONB} -eq -9999 -o ${LATB} -eq -9999 -o ${LEVS} -eq -9999 -o ${JCAP} -eq -9999 ] && exit -9999

# Get header information from Ensemble Guess files
if [ ${DOHYBVAR} = "YES" ]; then
   SFCGES_ENSMEAN=${SFCGES_ENSMEAN:-${COMIN_ATMOS_HISTORY_ENS_PREV}/${GPREFIX_ENS}sfcf006.ensmean.nc}
   export ATMGES_ENSMEAN=${ATMGES_ENSMEAN:-${COMIN_ATMOS_HISTORY_ENS_PREV}/${GPREFIX_ENS}atmf006.ensmean.nc}
   LONB_ENKF=${LONB_ENKF:-$(${NCLEN} ${ATMGES_ENSMEAN} grid_xt)} # get LONB_ENKF
   LATB_ENKF=${LATB_ENKF:-$(${NCLEN} ${ATMGES_ENSMEAN} grid_yt)} # get LATB_ENFK
   LEVS_ENKF=${LEVS_ENKF:-$(${NCLEN} ${ATMGES_ENSMEAN} pfull)} # get LATB_ENFK
   JCAP_ENKF=${JCAP_ENKF:--9999} # again, no jcap in the netcdf files
   NLON_ENKF=${NLON_ENKF:-${LONB_ENKF}}
   NLAT_ENKF=${NLAT_ENKF:-$((${LATB_ENKF}+2))}
   [ ${JCAP_ENKF} -eq -9999 -a ${LATB_ENKF} -ne -9999 ] && JCAP_ENKF=$((LATB_ENKF-2))
   [ ${LONB_ENKF} -eq -9999 -o ${LATB_ENKF} -eq -9999 -o ${LEVS_ENKF} -eq -9999 -o ${JCAP_ENKF} -eq -9999 ] && exit -9999
else
   LONB_ENKF=0 # just for if statement later
fi

# Get dimension information based on CASE
res=$(echo ${CASE} | cut -c2-)
JCAP_CASE=$((res*2-2))
LATB_CASE=$((res*2))
LONB_CASE=$((res*4))

# Set analysis resolution information
if [ ${DOHYBVAR} = "YES" ]; then
   JCAP_A=${JCAP_A:-${JCAP_ENKF:-${JCAP}}}
   LONA=${LONA:-${LONB_ENKF:-${LONB}}}
   LATA=${LATA:-${LATB_ENKF:-${LATB}}}
else
   JCAP_A=${JCAP_A:-${JCAP}}
   LONA=${LONA:-${LONB}}
   LATA=${LATA:-${LATB}}
fi
NLON_A=${NLON_A:-${LONA}}
NLAT_A=${NLAT_A:-$((${LATA}+2))}

DELTIM=${DELTIM:-$((3600/(${JCAP_A}/20)))}

# determine if writing or calculating increment
if [ ${DO_CALC_INCREMENT} = "YES" ]; then
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

#uGSI Namelist parameters
lrun_subdirs=${lrun_subdirs:-".true."}
if [ ${DOHYBVAR} = "YES" ]; then
   l_hyb_ens=.true.
   export l4densvar=${l4densvar:-".false."}
   export lwrite4danl=${lwrite4danl:-".false."}
else
   l_hyb_ens=.false.
   export l4densvar=.false.
   export lwrite4danl=.false.
fi

# Set 4D-EnVar specific variables
if [ ${DOHYBVAR} = "YES" -a ${l4densvar} = ".true." -a ${lwrite4danl} = ".true." ]; then
   ATMA03=${ATMA03:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma003.nc}
   ATMI03=${ATMI03:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi003.nc}
   ATMA04=${ATMA04:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma004.nc}
   ATMI04=${ATMI04:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi004.nc}
   ATMA05=${ATMA05:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma005.nc}
   ATMI05=${ATMI05:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi005.nc}
   ATMA07=${ATMA07:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma007.nc}
   ATMI07=${ATMI07:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi007.nc}
   ATMA08=${ATMA08:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma008.nc}
   ATMI08=${ATMI08:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi008.nc}
   ATMA09=${ATMA09:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atma009.nc}
   ATMI09=${ATMI09:-${COMOUT_ATMOS_ANALYSIS}/${APREFIX}atmi009.nc}
fi

################################################################################
#  Preprocessing
mkdata=NO
if [ ! -d ${DATA} ]; then
   mkdata=YES
   mkdir -p ${DATA}
fi

cd ${DATA} || exit 99

##############################################################
# Fixed files
${NLN} ${BERROR}       berror_stats
${NLN} ${SATANGL}      satbias_angle
${NLN} ${SATINFO}      satinfo
${NLN} ${RADCLOUDINFO} cloudy_radiance_info.txt
${NLN} ${ATMSFILTER}   atms_beamwidth.txt
${NLN} ${ANAVINFO}     anavinfo
${NLN} ${CONVINFO}     convinfo
${NLN} ${vqcdat}       vqctp001.dat
${NLN} ${INSITUINFO}   insituinfo
${NLN} ${OZINFO}       ozinfo
${NLN} ${PCPINFO}      pcpinfo
${NLN} ${AEROINFO}     aeroinfo
${NLN} ${SCANINFO}     scaninfo
${NLN} ${HYBENSINFO}   hybens_info
${NLN} ${OBERROR}      errtable

${NLN} ${FIXgfs}/gsi/AIRS_CLDDET.NL   AIRS_CLDDET.NL
${NLN} ${FIXgfs}/gsi/CRIS_CLDDET.NL   CRIS_CLDDET.NL
${NLN} ${FIXgfs}/gsi/IASI_CLDDET.NL   IASI_CLDDET.NL

#If using correlated error, link to the covariance files
if [ ${USE_CORRELATED_OBERRS} == "YES" ];  then
  if grep -q "Rcov" ${ANAVINFO} ;  then
     # shellcheck disable=SC2312
     mapfile -t covfile_array < <(find "${FIXgfs}/gsi/" -name "Rcov*")
     if (( ${#covfile_array[@]} > 0 )); then
       for covfile in "${covfile_array[@]}"; do
         covfile_base=$(basename "${covfile}")
         ${NLN} "${covfile}" "${DATA}/${covfile_base}"
       done
       echo "using correlated obs error"
     else
       echo "FATAL ERROR: Satellite error covariance files (Rcov) are missing."
       echo "Check for the required Rcov files in " ${ANAVINFO}
       exit 1
     fi
  else
     echo "FATAL ERROR: Satellite error covariance info missing in " ${ANAVINFO}
     exit 1
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
   ${NLN} ${CRTM_FIX}/${file}.SpcCoeff.bin ./crtm_coeffs/${file}.SpcCoeff.bin
   ${NLN} ${CRTM_FIX}/${file}.TauCoeff.bin ./crtm_coeffs/${file}.TauCoeff.bin
done
${NLN} ${CRTM_FIX}/amsua_metop-a_v2.SpcCoeff.bin ./crtm_coeffs/amsua_metop-a_v2.SpcCoeff.bin

${NLN} ${CRTM_FIX}/Nalli.IRwater.EmisCoeff.bin   ./crtm_coeffs/Nalli.IRwater.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.IRice.EmisCoeff.bin    ./crtm_coeffs/NPOESS.IRice.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.IRland.EmisCoeff.bin   ./crtm_coeffs/NPOESS.IRland.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.IRsnow.EmisCoeff.bin   ./crtm_coeffs/NPOESS.IRsnow.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.VISice.EmisCoeff.bin   ./crtm_coeffs/NPOESS.VISice.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.VISland.EmisCoeff.bin  ./crtm_coeffs/NPOESS.VISland.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.VISsnow.EmisCoeff.bin  ./crtm_coeffs/NPOESS.VISsnow.EmisCoeff.bin
${NLN} ${CRTM_FIX}/NPOESS.VISwater.EmisCoeff.bin ./crtm_coeffs/NPOESS.VISwater.EmisCoeff.bin
${NLN} ${CRTM_FIX}/FASTEM6.MWwater.EmisCoeff.bin ./crtm_coeffs/FASTEM6.MWwater.EmisCoeff.bin
${NLN} ${CRTM_FIX}/AerosolCoeff.bin              ./crtm_coeffs/AerosolCoeff.bin
if (( imp_physics == 8 )); then
   echo "using CRTM Thompson cloud optical table"
   ${NLN} "${CRTM_FIX}/CloudCoeff.Thompson08.-109z-1.bin" ./crtm_coeffs/CloudCoeff.bin
elif (( imp_physics == 11 )); then
   echo "using CRTM GFDL cloud optical table"
   ${NLN} "${CRTM_FIX}/CloudCoeff.GFDLFV3.-109z-1.bin" ./crtm_coeffs/CloudCoeff.bin
else
   echo "INVALID imp_physics = ${imp_physics}"
   echo "FATAL ERROR: No valid CRTM cloud optical table found for imp_physics =  ${imp_physics}"
   exit 1
fi


##############################################################
# Observational data
${NLN} ${PREPQC}           prepbufr
${NLN} ${PREPQCPF}         prepbufr_profl
${NLN} ${SATWND}           satwndbufr
${NLN} ${OSCATBF}          oscatbufr
${NLN} ${RAPIDSCATBF}      rapidscatbufr
${NLN} ${GSNDBF}           gsndrbufr
${NLN} ${GSNDBF1}          gsnd1bufr
${NLN} ${B1MSU}            msubufr
${NLN} ${B1AMUA}           amsuabufr
${NLN} ${B1AMUB}           amsubbufr
${NLN} ${B1MHS}            mhsbufr
${NLN} ${ESAMUA}           amsuabufrears
${NLN} ${ESAMUB}           amsubbufrears
#$NLN $ESMHS            mhsbufrears
${NLN} ${AMUADB}           amsuabufr_db
${NLN} ${AMUBDB}           amsubbufr_db
#$NLN $MHSDB            mhsbufr_db
${NLN} ${SBUVBF}           sbuvbufr
${NLN} ${OMPSNPBF}         ompsnpbufr
${NLN} ${OMPSLPBF}         ompslpbufr
${NLN} ${OMPSTCBF}         ompstcbufr
${NLN} ${GOMEBF}           gomebufr
${NLN} ${OMIBF}            omibufr
${NLN} ${MLSBF}            mlsbufr
${NLN} ${SMIPCP}           ssmirrbufr
${NLN} ${TMIPCP}           tmirrbufr
${NLN} ${AIRSBF}           airsbufr
${NLN} ${IASIBF}           iasibufr
${NLN} ${ESIASI}           iasibufrears
${NLN} ${IASIDB}           iasibufr_db
${NLN} ${AMSREBF}          amsrebufr
${NLN} ${AMSR2BF}          amsr2bufr
#${NLN} ${GMI1CRBF}         gmibufr # GMI temporarily disabled due to array overflow.
${NLN} ${SAPHIRBF}         saphirbufr
${NLN} ${SEVIRIBF}         seviribufr
${NLN} ${CRISBF}           crisbufr
${NLN} ${ESCRIS}           crisbufrears
${NLN} ${CRISDB}           crisbufr_db
${NLN} ${CRISFSBF}         crisfsbufr
${NLN} ${ESCRISFS}         crisfsbufrears
${NLN} ${CRISFSDB}         crisfsbufr_db
${NLN} ${ATMSBF}           atmsbufr
${NLN} ${ESATMS}           atmsbufrears
${NLN} ${ATMSDB}           atmsbufr_db
${NLN} ${SSMITBF}          ssmitbufr
${NLN} ${SSMISBF}          ssmisbufr
${NLN} ${GPSROBF}          gpsrobufr
${NLN} ${TCVITL}           tcvitl
${NLN} ${B1AVHAM}          avhambufr
${NLN} ${B1AVHPM}          avhpmbufr
${NLN} ${AHIBF}            ahibufr
${NLN} ${ABIBF}            abibufr
${NLN} ${HDOB}             hdobbufr
${NLN} ${SSTVIIRS}         sstviirs

[[ ${DONST} = "YES" ]] && ${NLN} ${NSSTBF} nsstbufr

##############################################################
# Required bias guess files
${NLN} ${GBIAS}    satbias_in
${NLN} ${GBIASPC}  satbias_pc
${NLN} ${GBIASAIR} aircftbias_in
${NLN} ${GRADSTAT} radstat.gdas

##############################################################
# Required model guess files
${NLN} ${ATMG03} sigf03
${NLN} ${ATMGES} sigf06
${NLN} ${ATMG09} sigf09

${NLN} ${SFCG03} sfcf03
${NLN} ${SFCGES} sfcf06
${NLN} ${SFCG09} sfcf09

[[ -f ${ATMG04} ]] && ${NLN} ${ATMG04} sigf04
[[ -f ${ATMG05} ]] && ${NLN} ${ATMG05} sigf05
[[ -f ${ATMG07} ]] && ${NLN} ${ATMG07} sigf07
[[ -f ${ATMG08} ]] && ${NLN} ${ATMG08} sigf08

[[ -f ${SFCG04} ]] && ${NLN} ${SFCG04} sfcf04
[[ -f ${SFCG05} ]] && ${NLN} ${SFCG05} sfcf05
[[ -f ${SFCG07} ]] && ${NLN} ${SFCG07} sfcf07
[[ -f ${SFCG08} ]] && ${NLN} ${SFCG08} sfcf08

if [ ${DOHYBVAR} = "YES" ]; then

   # Link ensemble members
   mkdir -p ensemble_data

   ENKF_SUFFIX="s"
   [[ ${SMOOTH_ENKF} = "NO" ]] && ENKF_SUFFIX=""

   fhrs="06"
   if [ ${l4densvar} = ".true." ]; then
      fhrs="03 04 05 06 07 08 09"
      nhr_obsbin=1
   fi

   for imem in $(seq 1 ${NMEM_ENS}); do
      memchar="mem$(printf %03i "${imem}")"
      MEMDIR=${memchar} RUN=${GDUMP_ENS} YMD=${gPDY} HH=${gcyc} declare_from_tmpl \
	      COMIN_ATMOS_HISTORY:COM_ATMOS_HISTORY_TMPL

      for fhr in ${fhrs}; do
         ${NLN} ${COMIN_ATMOS_HISTORY}/${GPREFIX_ENS}atmf0${fhr}${ENKF_SUFFIX}.nc ./ensemble_data/sigf${fhr}_ens_${memchar}
         if [ ${cnvw_option} = ".true." ]; then
            ${NLN} ${COMIN_ATMOS_HISTORY}/${GPREFIX_ENS}sfcf0${fhr}.nc ./ensemble_data/sfcf${fhr}_ens_${memchar}
         fi
      done
   done

fi

##############################################################
# Handle inconsistent surface mask between background, ensemble and analysis grids
# This needs re-visiting in the context of NSST; especially references to JCAP*
if [ ${JCAP} -ne ${JCAP_A} ]; then
   if [ ${DOHYBVAR} = "YES" -a ${JCAP_A} = ${JCAP_ENKF} ]; then
      if [ -e ${SFCGES_ENSMEAN} ]; then
         USE_READIN_ANL_SFCMASK=.true.
         ${NLN} ${SFCGES_ENSMEAN} sfcf06_anlgrid
      else
         echo "Warning: Inconsistent sfc mask between analysis and ensemble grids, GSI will interpolate"
      fi
    else
      echo "Warning: Inconsistent sfc mask between analysis and background grids, GSI will interpolate"
   fi
fi

##############################################################
# Diagnostic files
# if requested, link GSI diagnostic file directories for use later
if [ ${GENDIAG} = "YES" ] ; then
   if [ ${lrun_subdirs} = ".true." ] ; then
      if [ -d ${DIAG_DIR} ]; then
         rm -rf ${DIAG_DIR}
      fi
      ntasks_m1="$((ntasks-1))"
      for pe in $(seq 0 ${ntasks_m1}); do
        pedir="dir."$(printf %04i ${pe})
        mkdir -p ${DIAG_DIR}/${pedir}
        ${NLN} ${DIAG_DIR}/${pedir} ${pedir}
      done
   else
      err_exit "FATAL ERROR: lrun_subdirs must be true. lrun_subdirs=${lrun_subdirs}"
   fi
fi

##############################################################
# Output files
${NLN} ${ATMANL} siganl
${NLN} ${ATMINC} siginc.nc
if [ ${DOHYBVAR} = "YES" -a ${l4densvar} = ".true." -a ${lwrite4danl} = ".true." ]; then
   ${NLN} ${ATMA03}   siga03
   ${NLN} ${ATMI03}   sigi03.nc
   ${NLN} ${ATMA04}   siga04
   ${NLN} ${ATMI04}   sigi04.nc
   ${NLN} ${ATMA05}   siga05
   ${NLN} ${ATMI05}   sigi05.nc
   ${NLN} ${ATMA07}   siga07
   ${NLN} ${ATMI07}   sigi07.nc
   ${NLN} ${ATMA08}   siga08
   ${NLN} ${ATMI08}   sigi08.nc
   ${NLN} ${ATMA09}   siga09
   ${NLN} ${ATMI09}   sigi09.nc
fi
${NLN} ${ABIAS}    satbias_out
${NLN} ${ABIASPC}  satbias_pc.out
${NLN} ${ABIASAIR} aircftbias_out

if [ ${DONST} = "YES" ]; then
   ${NLN} ${DTFANL} dtfanl
fi

# If requested, link (and if tarred, de-tar obsinput.tar) into obs_input.* files
if [ ${USE_SELECT} = "YES" ]; then
   rm obs_input.*
   nl=$(file ${SELECT_OBS} | cut -d: -f2 | grep tar | wc -l)
   if [ ${nl} -eq 1 ]; then
      rm obsinput.tar
      ${NLN} ${SELECT_OBS} obsinput.tar
      tar -xvf obsinput.tar
      rm obsinput.tar
   else
      for filetop in $(ls ${SELECT_OBS}/obs_input.*); do
         fileloc=$(basename ${filetop})
         ${NLN} ${filetop} ${fileloc}
      done
   fi
fi

##############################################################
# If requested, copy and de-tar guess radstat file
if [ ${USE_RADSTAT} = "YES" ]; then
   if [ ${USE_CFP} = "YES" ]; then
     [[ -f ${DATA}/unzip.sh ]] && rm ${DATA}/unzip.sh
     [[ -f ${DATA}/mp_unzip.sh ]] && rm ${DATA}/mp_unzip.sh
     cat > ${DATA}/unzip.sh << EOFunzip
#!/bin/sh
   diag_file=\$1
   diag_suffix=\$2
   fname=\$(echo \$diag_file | cut -d'.' -f1)
   fdate=\$(echo \$diag_file | cut -d'.' -f2)
   ${UNCOMPRESS} \$diag_file
   fnameges=\$(echo \$fname | sed 's/_ges//g')
   ${NMV} \$fname.\$fdate\$diag_suffix \$fnameges
EOFunzip
     chmod 755 ${DATA}/unzip.sh
   fi

   listdiag=$(tar xvf radstat.gdas | cut -d' ' -f2 | grep _ges)
   for type in ${listdiag}; do
      diag_file=$(echo ${type} | cut -d',' -f1)
      if [ ${USE_CFP} = "YES" ] ; then
         echo "${nm} ${DATA}/unzip.sh ${diag_file} ${DIAG_SUFFIX}" | tee -a ${DATA}/mp_unzip.sh
         if [ ${CFP_MP:-"NO"} = "YES" ]; then
           nm=$((nm+1))
         fi
      else
         fname=$(echo ${diag_file} | cut -d'.' -f1)
         date=$(echo ${diag_file} | cut -d'.' -f2)
         ${UNCOMPRESS} ${diag_file}
         fnameges=$(echo ${fname}|sed 's/_ges//g')
         ${NMV} ${fname}.${date}${DIAG_SUFFIX} ${fnameges}
      fi
   done

   if [ ${USE_CFP} = "YES" ] ; then
      chmod 755 ${DATA}/mp_unzip.sh
      ncmd=$(cat ${DATA}/mp_unzip.sh | wc -l)
      if [ ${ncmd} -gt 0 ]; then
         ncmd_max=$((ncmd < max_tasks_per_node ? ncmd : max_tasks_per_node))
         APRUNCFP_UNZIP=$(eval echo ${APRUNCFP})
         ${APRUNCFP_UNZIP} ${DATA}/mp_unzip.sh
         export err=$?; err_chk
      fi
   fi
fi # if [ $USE_RADSTAT = "YES" ]

##############################################################
# GSI Namelist options
if [ ${DOHYBVAR} = "YES" ]; then
   HYBRID_ENSEMBLE="n_ens=${NMEM_ENS},jcap_ens=${JCAP_ENKF},nlat_ens=${NLAT_ENKF},nlon_ens=${NLON_ENKF},jcap_ens_test=${JCAP_ENKF},${HYBRID_ENSEMBLE}"
   if [ ${l4densvar} = ".true." ]; then
      SETUP="niter(1)=50,niter(2)=150,niter_no_qc(1)=25,niter_no_qc(2)=0,thin4d=.true.,ens_nstarthr=3,l4densvar=${l4densvar},lwrite4danl=${lwrite4danl},${SETUP}"
      JCOPTS="ljc4tlevs=.true.,${JCOPTS}"
      STRONGOPTS="tlnmc_option=3,${STRONGOPTS}"
      OBSQC="c_varqc=0.04,${OBSQC}"
   fi
fi

if [ ${DONST} = "YES" ]; then
   NST="nstinfo=${NSTINFO},fac_dtl=${FAC_DTL},fac_tsl=${FAC_TSL},zsea1=${ZSEA1},zsea2=${ZSEA2},${NST}"
fi

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
  lrun_subdirs=${lrun_subdirs},
  crtm_coeffs_path='./crtm_coeffs/',
  newpc4pred=.true.,adp_anglebc=.true.,angord=4,passive_bc=.true.,use_edges=.false.,
  diag_precon=.true.,step_start=1.e-3,emiss_bc=.true.,nhr_obsbin=${nhr_obsbin:-3},
  cwoption=3,imp_physics=${imp_physics},lupp=${lupp},cnvw_option=${cnvw_option},cao_check=${cao_check},
  netcdf_diag=${netcdf_diag},binary_diag=${binary_diag},
  lobsdiag_forenkf=${lobsdiag_forenkf},
  write_fv3_incr=${write_fv3_increment},
  nhr_anal=${IAUFHRS},
  ta2tb=${ta2tb},
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
  use_poq7=.true.,qc_noirjaco3_pole=.true.,vqc=.false.,nvqc=.true.,
  aircraft_t_bc=.true.,biaspredt=1.0e5,upd_aircraft=.true.,cleanup_tail=.true.,
  tcp_width=70.0,tcp_ermax=7.35,airs_cads=${AIRS_CADS},cris_cads=${CRIS_CADS},
  iasi_cads=${IASI_CADS},
  ${OBSQC}
/
&OBS_INPUT
  dmesh(1)=145.0,dmesh(2)=150.0,dmesh(3)=100.0,dmesh(4)=50.0,time_window_max=3.0,
  hofx_2m_sfcfile=${hofx_2m_sfcfile},
  ${OBSINPUT}
/
OBS_INPUT::
!  dfile          dtype       dplat       dsis                dval    dthin dsfcalc
   prepbufr       ps          null        ps                  0.0     0     0
   prepbufr       t           null        t                   0.0     0     0
   prepbufr_profl t           null        t                   0.0     0     0
   hdobbufr       t           null        t                   0.0     0     0
   prepbufr       q           null        q                   0.0     0     0
   prepbufr_profl q           null        q                   0.0     0     0
   hdobbufr       q           null        q                   0.0     0     0
   prepbufr       pw          null        pw                  0.0     0     0
   prepbufr       uv          null        uv                  0.0     0     0
   prepbufr_profl uv          null        uv                  0.0     0     0
   satwndbufr     uv          null        uv                  0.0     0     0
   hdobbufr       uv          null        uv                  0.0     0     0
   prepbufr       spd         null        spd                 0.0     0     0
   hdobbufr       spd         null        spd                 0.0     0     0
   prepbufr       dw          null        dw                  0.0     0     0
   radarbufr      rw          null        rw                  0.0     0     0
   nsstbufr       sst         nsst        sst                 0.0     0     0
   gpsrobufr      gps_bnd     null        gps                 0.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp        pcp_ssmi            0.0    -1     0
   tmirrbufr      pcp_tmi     trmm        pcp_tmi             0.0    -1     0
   sbuvbufr       sbuv2       n16         sbuv8_n16           0.0     0     0
   sbuvbufr       sbuv2       n17         sbuv8_n17           0.0     0     0
   sbuvbufr       sbuv2       n18         sbuv8_n18           0.0     0     0
   gimgrbufr      goes_img    g11         imgr_g11            0.0     1     0
   gimgrbufr      goes_img    g12         imgr_g12            0.0     1     0
   airsbufr       airs        aqua        airs_aqua           0.0     1     1
   amsuabufr      amsua       n15         amsua_n15           0.0     1     1
   amsuabufr      amsua       n18         amsua_n18           0.0     1     1
   amsuabufr      amsua       metop-a     amsua_metop-a       0.0     1     1
   airsbufr       amsua       aqua        amsua_aqua          0.0     1     1
   amsubbufr      amsub       n17         amsub_n17           0.0     1     1
   mhsbufr        mhs         n18         mhs_n18             0.0     1     1
   mhsbufr        mhs         metop-a     mhs_metop-a         0.0     1     1
   ssmitbufr      ssmi        f15         ssmi_f15            0.0     1     0
   amsrebufr      amsre_low   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_mid   aqua        amsre_aqua          0.0     1     0
   amsrebufr      amsre_hig   aqua        amsre_aqua          0.0     1     0
   ssmisbufr      ssmis       f16         ssmis_f16           0.0     1     0
   ssmisbufr      ssmis       f17         ssmis_f17           0.0     1     0
   ssmisbufr      ssmis       f18         ssmis_f18           0.0     1     0
   gsnd1bufr      sndrd1      g12         sndrD1_g12          0.0     1     0
   gsnd1bufr      sndrd2      g12         sndrD2_g12          0.0     1     0
   gsnd1bufr      sndrd3      g12         sndrD3_g12          0.0     1     0
   gsnd1bufr      sndrd4      g12         sndrD4_g12          0.0     1     0
   gsnd1bufr      sndrd1      g11         sndrD1_g11          0.0     1     0
   gsnd1bufr      sndrd2      g11         sndrD2_g11          0.0     1     0
   gsnd1bufr      sndrd3      g11         sndrD3_g11          0.0     1     0
   gsnd1bufr      sndrd4      g11         sndrD4_g11          0.0     1     0
   gsnd1bufr      sndrd1      g13         sndrD1_g13          0.0     1     0
   gsnd1bufr      sndrd2      g13         sndrD2_g13          0.0     1     0
   gsnd1bufr      sndrd3      g13         sndrD3_g13          0.0     1     0
   gsnd1bufr      sndrd4      g13         sndrD4_g13          0.0     1     0
   iasibufr       iasi        metop-a     iasi_metop-a        0.0     1     1
   gomebufr       gome        metop-a     gome_metop-a        0.0     2     0
   omibufr        omi         aura        omi_aura            0.0     2     0
   sbuvbufr       sbuv2       n19         sbuv8_n19           0.0     0     0
   amsuabufr      amsua       n19         amsua_n19           0.0     1     1
   mhsbufr        mhs         n19         mhs_n19             0.0     1     1
   tcvitl         tcp         null        tcp                 0.0     0     0
   seviribufr     seviri      m08         seviri_m08          0.0     1     0
   seviribufr     seviri      m09         seviri_m09          0.0     1     0
   seviribufr     seviri      m10         seviri_m10          0.0     1     0
   seviribufr     seviri      m11         seviri_m11          0.0     1     0
   amsuabufr      amsua       metop-b     amsua_metop-b       0.0     1     1
   mhsbufr        mhs         metop-b     mhs_metop-b         0.0     1     1
   iasibufr       iasi        metop-b     iasi_metop-b        0.0     1     1
   gomebufr       gome        metop-b     gome_metop-b        0.0     2     0
   atmsbufr       atms        npp         atms_npp            0.0     1     1
   atmsbufr       atms        n20         atms_n20            0.0     1     1
   atmsbufr       atms        n21         atms_n21            0.0     1     1
   crisbufr       cris        npp         cris_npp            0.0     1     0
   crisfsbufr     cris-fsr    npp         cris-fsr_npp        0.0     1     0
   crisfsbufr     cris-fsr    n20         cris-fsr_n20        0.0     1     0
   crisfsbufr     cris-fsr    n21         cris-fsr_n21        0.0     1     0
   gsnd1bufr      sndrd1      g14         sndrD1_g14          0.0     1     0
   gsnd1bufr      sndrd2      g14         sndrD2_g14          0.0     1     0
   gsnd1bufr      sndrd3      g14         sndrD3_g14          0.0     1     0
   gsnd1bufr      sndrd4      g14         sndrD4_g14          0.0     1     0
   gsnd1bufr      sndrd1      g15         sndrD1_g15          0.0     1     0
   gsnd1bufr      sndrd2      g15         sndrD2_g15          0.0     1     0
   gsnd1bufr      sndrd3      g15         sndrD3_g15          0.0     1     0
   gsnd1bufr      sndrd4      g15         sndrD4_g15          0.0     1     0
   oscatbufr      uv          null        uv                  0.0     0     0
   mlsbufr        mls30       aura        mls30_aura          0.0     0     0
   avhambufr      avhrr       metop-a     avhrr3_metop-a      0.0     4     0
   avhpmbufr      avhrr       n18         avhrr3_n18          0.0     4     0
   avhambufr      avhrr       metop-b     avhrr3_metop-b      0.0     4     0
   avhambufr      avhrr       metop-c     avhrr3_metop-c      0.0     4     0
   avhpmbufr      avhrr       n19         avhrr3_n19          0.0     4     0
   amsr2bufr      amsr2       gcom-w1     amsr2_gcom-w1       0.0     3     0
   gmibufr        gmi         gpm         gmi_gpm             0.0     1     0
   saphirbufr     saphir      meghat      saphir_meghat       0.0     3     0
   ahibufr        ahi         himawari8   ahi_himawari8       0.0     1     0
   abibufr        abi         g16         abi_g16             0.0     1     0
   abibufr        abi         g17         abi_g17             0.0     1     0
   abibufr        abi         g18         abi_g18             0.0     1     0
   rapidscatbufr  uv          null        uv                  0.0     0     0
   ompsnpbufr     ompsnp      npp         ompsnp_npp          0.0     0     0
   ompslpbufr     ompslp      npp         ompslp_npp          0.0     0     0
   ompstcbufr     ompstc8     npp         ompstc8_npp         0.0     2     0
   ompsnpbufr     ompsnp      n20         ompsnp_n20          0.0     0     0
   ompstcbufr     ompstc8     n20         ompstc8_n20         0.0     2     0
   amsuabufr      amsua       metop-c     amsua_metop-c       0.0     1     1
   mhsbufr        mhs         metop-c     mhs_metop-c         0.0     1     1
   iasibufr       iasi        metop-c     iasi_metop-c        0.0     1     1
   sstviirs       viirs-m     npp         viirs-m_npp         0.0     4     0
   sstviirs       viirs-m     j1          viirs-m_j1          0.0     4     0
   ahibufr        ahi         himawari9   ahi_himawari9       0.0     1     0
   sstviirs       viirs-m     j2          viirs-m_j2          0.0     4     0
   ompsnpbufr     ompsnp      n21         ompsnp_n21          0.0     0     0
   ompstcbufr     ompstc8     n21         ompstc8_n21         0.0     2     0
   gomebufr       gome        metop-c     gome_metop-c        0.0     2     0
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
  oblat=45.,oblon=180.,obpres=1000.,obdattim=${CDATE},
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
. prep_step

${NCP} ${GSIEXEC} ${DATA}
${APRUN_GSI} ${DATA}/$(basename ${GSIEXEC}) 1>&1 2>&2
export err=$?; err_chk


##############################################################
# If full analysis field written, calculate analysis increment
# here before releasing FV3 forecast
if [ ${DO_CALC_INCREMENT} = "YES" ]; then
  ${CALCINCPY}
  export err=$?; err_chk
fi


##############################################################
# For eupd
if [ -s satbias_out.int ]; then
   ${NCP} satbias_out.int ${ABIASe}
else
   ${NCP} satbias_in ${ABIASe}
fi

# Cat runtime output files.
cat fort.2* > ${GSISTAT}

# If requested, create obsinput tarball from obs_input.* files
if [ ${RUN_SELECT} = "YES" ]; then
  echo $(date) START tar obs_input >&2
  [[ -s obsinput.tar ]] && rm obsinput.tar
  ${NLN} ${SELECT_OBS} obsinput.tar
  ${CHGRP_CMD} obs_input.*
  tar -cvf obsinput.tar obs_input.*
  chmod 750 ${SELECT_OBS}
  ${CHGRP_CMD} ${SELECT_OBS}
  rm obsinput.tar
  echo $(date) END tar obs_input >&2
fi

################################################################################
# Send alerts
if [ ${SENDDBN} = "YES" ]; then
    if [ ${RUN} = "gfs" ]; then
       ${DBNROOT}/bin/dbn_alert MODEL GFS_abias ${job} ${ABIAS}
    fi
fi

################################################################################
# Postprocessing
cd ${pwd}
[[ ${mkdata} = "YES" ]] && rm -rf ${DATA}

##############################################################
# Add this statement to release the forecast job once the
# atmopsheric analysis and updated surface RESTARTS are
# available.  Do not release forecast when RUN=enkf
##############################################################
if [ ${SENDECF} = "YES" -a "${RUN}" != "enkf" ]; then
   ecflow_client --event release_fcst
fi
echo "${rCDUMP} ${CDATE} atminc done at $(date)" > ${COMOUT_ATMOS_ANALYSIS}/${APREFIX}loginc.txt

################################################################################

exit ${err}

################################################################################

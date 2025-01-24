set -x
# Set variables used in script
#   CLEAN up $tmpdir when finished (YES=remove, NO=leave alone)
#   ncp is cp replacement, currently keep as /bin/cp

UNCOMPRESS=gunzip
CLEAN=NO
ncp=/bin/cp
nln="/bin/ln -sf"

# HAFS test cases set up
RUN_FGAT=YES  # use FGAT or not
RUN_ENSDA=NO
l4densvar=.true.
nhr_obsbin=3
l_both_fv3sar_gfs_ens=.false.
n_ens_gfs=5
n_ens_fv3sar=5

#
# Set experiment name
#
exp=$jobname

#-----------------------------------------------------------------------
#
# Extract from ADATE the starting year, month, day, and hour of the
# forecast.  These are needed below for various operations.
#
#-----------------------------------------------------------------------
#
adate=${hafs_envar_adate}
YYYYMMDDHH=$(date +%Y%m%d%H -d "${adate:0:8} ${adate:8:2}")
JJJ=$(date +%j -d "${adate:0:8} ${adate:8:2}")

YYYY=${YYYYMMDDHH:0:4}
MM=${YYYYMMDDHH:4:2}
DD=${YYYYMMDDHH:6:2}
HH=${YYYYMMDDHH:8:2}
YYYYMMDD=${YYYYMMDDHH:0:8}
PDY=${YYYYMMDD}
cyc=${HH}
# prior date and hour
adateprior=`date +%Y%m%d%H -d "${adate:0:8} ${adate:8:2} - 6 hours"`

ymdprior=$(echo ${adateprior} | cut -c1-8)
hhprior=$(echo ${adateprior} | cut -c9-10)

CDATEtm03=`date +%Y%m%d%H -d "${adate:0:8} ${adate:8:2} - 3 hours"`
ymdtm03=$(echo ${CDATEtm03} | cut -c1-8)
hhtm03=$(echo ${CDATEtm03} | cut -c9-10)

CDATEtp03=`date +%Y%m%d%H -d "${adate:0:8} ${adate:8:2} + 3 hours"`
ymdtp03=$(echo ${CDATEtp03} | cut -c1-8)
hhtp03=$(echo ${CDATEtp03} | cut -c9-10)

#
#-----------------------------------------------------------------------
#
# go to working directory and save directory.
# define fix and background path
#
#-----------------------------------------------------------------------
# Set runtime and save directories
tmpdir=$tmpdir/tmpreg_hafs_4denvar_glbens/${exp}
savdir=$savdir/outreg_hafs_4denvar_glbens/${exp}

# Set up $tmpdir
rm -rf $tmpdir
mkdir -p $tmpdir
chgrp rstprod $tmpdir
chmod 750 $tmpdir
cd $tmpdir

bkpath=${hafs_envar_ges}
fixcrtm=${fixcrtm:-$CRTM_FIX}

################################################################
##### input data and fix directory #######################
inputdata=${hafs_envar_ges}
COMINgdas=${hafs_envar_ens}
COMINobs=${hafs_envar_obs}
COMINgfs=${hafs_envar_obs}
WORKhafs=${hafs_envar_obs}
##########################################################

# use FGAT or not
if [ ${RUN_FGAT} = "YES" ]; then
  ln -sf ${inputdata}/coupler.res_03 .
  ln -sf ${inputdata}/fv3_akbk_03 .
  ln -sf ${inputdata}/fv3_sfcdata_03 .
  ln -sf ${inputdata}/fv3_srfwnd_03 .
  ln -sf ${inputdata}/fv3_dynvars_03 .
  ln -sf ${inputdata}/fv3_tracer_03 .

  ln -sf ${inputdata}/coupler.res_09 .
  ln -sf ${inputdata}/fv3_akbk_09 .
  ln -sf ${inputdata}/fv3_sfcdata_09 .
  ln -sf ${inputdata}/fv3_srfwnd_09 .
  ln -sf ${inputdata}/fv3_dynvars_09 .
  ln -sf ${inputdata}/fv3_tracer_09 .
fi

# copy background and grib configuration files
cp ${bkpath}/${YYYYMMDD}.${HH}0000.coupler.res ./coupler.res
cp ${bkpath}/${YYYYMMDD}.${HH}0000.fv_core.res.nc ./fv3_akbk
cp ${bkpath}/${YYYYMMDD}.${HH}0000.sfc_data.nc ./fv3_sfcdata
cp ${bkpath}/${YYYYMMDD}.${HH}0000.fv_srf_wnd.res.tile1.nc ./fv3_srfwnd
cp ${bkpath}/${YYYYMMDD}.${HH}0000.fv_core.res.tile1.nc ./fv3_dynvars
cp ${bkpath}/${YYYYMMDD}.${HH}0000.fv_tracer.res.tile1.nc ./fv3_tracer

cp ${bkpath}/oro_data.nc ./fv3_oro_data
cp ${bkpath}/atmos_static.nc ./fv3_atmos_static
cp ${bkpath}/grid_spec.nc ./fv3_grid_spec

# create ensemble member file list
if [ ${RUN_ENSDA} != "YES" ] || [ $l_both_fv3sar_gfs_ens = .true. ]; then
# Link gdas ensemble members
  mkdir -p ensemble_data
  GSUFFIX=${GSUFFIX:-.nc}
  if [ ${l4densvar:-.false.} = ".true." ]; then
    fhrs="03 06 09"
  else
    fhrs="06"
  fi
  for fhh in $fhrs; do
  rm -f filelist${fhh}
  for mem in $(seq -f '%03g' 1 ${n_ens_gfs}); do
    if [ -s ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}s${GSUFFIX:-.nc} ]; then
      ${nln} ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}s${GSUFFIX:-.nc} ./ensemble_data/enkfgdas.${ymdprior}${hhprior}.atmf0${fhh}_ens_${mem}
    elif [ -s ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nc} ]; then
      ${nln} ${COMINgdas}/enkfgdas.${ymdprior}/${hhprior}/atmos/mem${mem}/gdas.t${hhprior}z.atmf0${fhh}${GSUFFIX:-.nc} ./ensemble_data/enkfgdas.${ymdprior}${hhprior}.atmf0${fhh}_ens_${mem}
    fi
    echo "./ensemble_data/enkfgdas.${ymdprior}${hhprior}.atmf0${fhh}_ens_${mem}" >> filelist${fhh}
  done
  done
fi

if [ ${RUN_ENSDA} = "YES" ]; then
  for mem in $(seq -f '%03g' 1 ${n_ens_fv3sar})
  do
    RESTARTens=${inputdata}
    fhh="06"
    ln -sf ${inputdata}/fv3SAR${fhh}_ens_mem${mem}-coupler.res .
    ln -sf ${inputdata}/fv3SAR${fhh}_ens_mem${mem}-fv3_akbk .
    ln -sf ${inputdata}/fv3SAR${fhh}_ens_mem${mem}-fv3_sfcdata .
    ln -sf ${inputdata}/fv3SAR${fhh}_ens_mem${mem}-fv3_srfwnd .
    ln -sf ${inputdata}/fv3SAR${fhh}_ens_mem${mem}-fv3_dynvars .
    ln -sf ${inputdata}/fv3SAR${fhh}_ens_mem${mem}-fv3_tracer .
    if [ ! -s ./fv3_ens_grid_spec ]; then
      ln -sf ${RESTARTens}/grid_spec.nc ./fv3_ens_grid_spec
    fi
    if [ ${l4densvar:-.false.} = ".true." ]; then
      export ENS_NSTARTHR=3
      fhh="03"
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-coupler.res .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_akbk .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_sfcdata .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_srfwnd .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_dynvars .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_tracer .
      fhh="09"
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-coupler.res .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_akbk .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_sfcdata .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_srfwnd .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_dynvars .
      ${nln} ${RESTARTens}/fv3SAR${fhh}_ens_mem${mem}-fv3_tracer .
    fi
  done
fi
if [ ${RUN_ENSDA} != "YES" ]; then
  export N_ENS=${n_ens_gfs}
  export BETA_S0=${BETA_S0:-0.2}
  export GRID_RATIO_ENS=1
  export REGIONAL_ENSEMBLE_OPTION=1
elif [ ${RUN_ENSDA} = "YES" ]; then
  if [ $l_both_fv3sar_gfs_ens = .false. ]; then
    export N_ENS=${n_ens_fv3sar}
    export BETA_S0=${BETA_S0:-0.0}
    export GRID_RATIO_ENS=2
    export grid_ratio_fv3_regional=2
    export REGIONAL_ENSEMBLE_OPTION=5
  elif [ $l_both_fv3sar_gfs_ens = .true. ]; then
    export N_ENS=$((${n_ens_gfs} + ${n_ens_fv3sar}))
    export BETA_S0=${BETA_S0:-0.0}
    export GRID_RATIO_ENS=2
    export grid_ratio_fv3_regional=2
    export REGIONAL_ENSEMBLE_OPTION=5
  fi
fi

#-----------------------------------------------------------------------
#
# link observation files
# copy observation files to working directory
#
#-----------------------------------------------------------------------

# Link GFS/GDAS input and observation files
COMIN_OBS=${COMIN_OBS:-${COMINobs}}
COMIN_GFS=${COMIN_GFS:-${COMINgfs}}

OPREFIX=${OPREFIX:-"gfs.t${cyc}z."}
OSUFFIX=${OSUFFIX:-""}
PREPQC=${PREPQC:-${COMIN_OBS}/${OPREFIX}prepbufr${OSUFFIX}}
PREPQCPF=${PREPQCPF:-${COMIN_OBS}/${OPREFIX}prepbufr.acft_profiles${OSUFFIX}}
NSSTBF=${NSSTBF:-${COMIN_OBS}/${OPREFIX}nsstbufr${OSUFFIX}}
SATWND=${SATWND:-${COMIN_OBS}/${OPREFIX}satwnd.tm00.bufr_d${OSUFFIX}}
SATWHR=${SATWHR:-${COMIN_OBS}/${OPREFIX}satwhr.tm00.bufr_d${OSUFFIX}}
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
ESATMS=${ESATMS:-${COMIN_OBS}/${OPREFIX}esatms.tm00.bufr_d${OSUFFIX}}
ATMSDB=${ATMSDB:-${COMIN_OBS}/${OPREFIX}atmsdb.tm00.bufr_d${OSUFFIX}}
SSMITBF=${SSMITBF:-${COMIN_OBS}/${OPREFIX}ssmit.tm00.bufr_d${OSUFFIX}}
SSMISBF=${SSMISBF:-${COMIN_OBS}/${OPREFIX}ssmisu.tm00.bufr_d${OSUFFIX}}
SBUVBF=${SBUVBF:-${COMIN_OBS}/${OPREFIX}osbuv8.tm00.bufr_d${OSUFFIX}}
OMPSNPBF=${OMPSNPBF:-${COMIN_OBS}/${OPREFIX}ompsn8.tm00.bufr_d${OSUFFIX}}
OMPSTCBF=${OMPSTCBF:-${COMIN_OBS}/${OPREFIX}ompst8.tm00.bufr_d${OSUFFIX}}
GOMEBF=${GOMEBF:-${COMIN_OBS}/${OPREFIX}gome.tm00.bufr_d${OSUFFIX}}
OMIBF=${OMIBF:-${COMIN_OBS}/${OPREFIX}omi.tm00.bufr_d${OSUFFIX}}
MLSBF=${MLSBF:-${COMIN_OBS}/${OPREFIX}mls.tm00.bufr_d${OSUFFIX}}
OMPSLPBF=${OMPSLPBF:-${COMIN_OBS}/${OPREFIX}ompslp.tm00.bufr_d${OSUFFIX}}
SMIPCP=${SMIPCP:-${COMIN_OBS}/${OPREFIX}spssmi.tm00.bufr_d${OSUFFIX}}
TMIPCP=${TMIPCP:-${COMIN_OBS}/${OPREFIX}sptrmm.tm00.bufr_d${OSUFFIX}}
if [[ ${use_bufr_nr:-no} = "no" ]]; then
  GPSROBF=${GPSROBF:-${COMIN_OBS}/${OPREFIX}gpsro.tm00.bufr_d${OSUFFIX}}
else
  GPSROBF=${GPSROBF:-${COMIN_OBS}/${OPREFIX}gpsro.tm00.bufr_d.nr}
fi
TCVITL=${TCVITL:-${COMIN_GFS}/${OPREFIX}syndata.tcvitals.tm00}
B1AVHAM=${B1AVHAM:-${COMIN_OBS}/${OPREFIX}avcsam.tm00.bufr_d${OSUFFIX}}
B1AVHPM=${B1AVHPM:-${COMIN_OBS}/${OPREFIX}avcspm.tm00.bufr_d${OSUFFIX}}

# Observational data
if [[ ${use_bufr_nr:-no} = "no" ]] && [ -s $PREPQC ]; then
  $ncp -Lp $PREPQC     prepbufr
else
  touch prepbufr
fi
ln -sf $SATWND           satwndbufr
ln -sf $SATWHR           satwhrbufr
ln -sf $GSNDBF1          gsnd1bufr
ln -sf $B1AMUA           amsuabufr
ln -sf $B1MHS            mhsbufr
ln -sf $ESAMUA           amsuabufrears
ln -sf $SBUVBF           sbuvbufr
ln -sf $OMPSNPBF         ompsnpbufr
ln -sf $OMPSTCBF         ompstcbufr
ln -sf $GOMEBF           gomebufr
ln -sf $OMIBF            omibufr
ln -sf $MLSBF            mlsbufr
ln -sf $AIRSBF           airsbufr
ln -sf $IASIBF           iasibufr
ln -sf $ESIASI           iasibufrears
ln -sf $IASIDB           iasibufr_db
ln -sf $AMSR2BF          amsr2bufr
ln -sf $GMI1CRBF         gmibufr
ln -sf $SAPHIRBF         saphirbufr
ln -sf $SEVIRIBF         seviribufr
ln -sf $CRISBF           crisbufr
ln -sf $ESCRIS           crisbufrears
ln -sf $CRISDB           crisbufr_db
ln -sf $CRISFSBF         crisfsbufr
ln -sf $ESCRISFS         crisfsbufrears
ln -sf $CRISFSDB         crisfsbufr_db
ln -sf $ATMSBF           atmsbufr
ln -sf $ESATMS           atmsbufrears
ln -sf $ATMSDB           atmsbufr_db
ln -sf $SSMISBF          ssmisbufr
ln -sf $GPSROBF          gpsrobufr
ln -sf $TCVITL           tcvitl
ln -sf $B1AVHAM          avhambufr
ln -sf $B1AVHPM          avhpmbufr

if [[ ${use_bufr_nr:-no} = "yes" ]]; then

  if [ -s ${PREPQC}.nr ]; then
    $ncp -L ${PREPQC}.nr    prepbufr
  fi
  ln -sf ${SAPHIRBF}.nr  saphirbufr

fi
# HAFS specific observations
INTCOMobs=${WORKhafs}/obs_prep
# Use updated prepbufr if exists
if [ -s ${INTCOMobs}/hafs.t${cyc}z.prepbufr ]; then
  ln -s ${INTCOMobs}/hafs.t${cyc}z.prepbufr prepbufr
fi
# cat tempdrop.prepbufr with drifting correction into prepbufr
if [ -s ${INTCOMobs}/hafs.t${cyc}z.tempdrop.prepbufr ]; then
  cat ${INTCOMobs}/hafs.t${cyc}z.tempdrop.prepbufr >> prepbufr
fi
if [ -s ${INTCOMobs}/hafs.t${cyc}z.tldplr.tm00.bufr_d ]; then
  ln -s ${INTCOMobs}/hafs.t${cyc}z.tldplr.tm00.bufr_d tldplrbufr
fi
if [ -s ${INTCOMobs}/hafs.t${cyc}z.hdob.tm00.bufr_d ]; then
  ln -s ${INTCOMobs}/hafs.t${cyc}z.hdob.tm00.bufr_d hdobbufr
fi
if [ -s ${INTCOMobs}/hafs.t${cyc}z.nexrad.tm00.bufr_d ]; then
  ln -s ${INTCOMobs}/hafs.t${cyc}z.nexrad.tm00.bufr_d l2rwbufr
fi


#
#-----------------------------------------------------------------------
#
# Create links to fix files in the FIXgsi directory.
#
#-----------------------------------------------------------------------

ln -sf ${inputdata}/berror_stats .
ln -sf ${inputdata}/satinfo .
ln -sf ${inputdata}/atms_beamwidth.txt .
ln -sf ${inputdata}/anavinfo .
ln -sf ${inputdata}/convinfo .
ln -sf ${inputdata}/ozinfo .
ln -sf ${inputdata}/pcpinfo .
ln -sf ${inputdata}/scaninfo .
ln -sf ${inputdata}/errtable .
ln -sf ${inputdata}/prepobs_prep.bufrtable .
ln -sf ${inputdata}/bftab_sstphr .

#-----------------------------------------------------------------------
#
# CRTM Spectral and Transmittance coefficients
#
#-----------------------------------------------------------------------
emiscoef_IRwater=${fixcrtm}/Nalli.IRwater.EmisCoeff.bin
emiscoef_IRice=${fixcrtm}/NPOESS.IRice.EmisCoeff.bin
emiscoef_IRland=${fixcrtm}/NPOESS.IRland.EmisCoeff.bin
emiscoef_IRsnow=${fixcrtm}/NPOESS.IRsnow.EmisCoeff.bin
emiscoef_VISice=${fixcrtm}/NPOESS.VISice.EmisCoeff.bin
emiscoef_VISland=${fixcrtm}/NPOESS.VISland.EmisCoeff.bin
emiscoef_VISsnow=${fixcrtm}/NPOESS.VISsnow.EmisCoeff.bin
emiscoef_VISwater=${fixcrtm}/NPOESS.VISwater.EmisCoeff.bin
emiscoef_MWwater=${fixcrtm}/FASTEM6.MWwater.EmisCoeff.bin
aercoef=${fixcrtm}/AerosolCoeff.bin
cldcoef=${fixcrtm}/CloudCoeff.bin

ln -sf ${emiscoef_IRwater} Nalli.IRwater.EmisCoeff.bin
ln -sf $emiscoef_IRice ./NPOESS.IRice.EmisCoeff.bin
ln -sf $emiscoef_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
ln -sf $emiscoef_IRland ./NPOESS.IRland.EmisCoeff.bin
ln -sf $emiscoef_VISice ./NPOESS.VISice.EmisCoeff.bin
ln -sf $emiscoef_VISland ./NPOESS.VISland.EmisCoeff.bin
ln -sf $emiscoef_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
ln -sf $emiscoef_VISwater ./NPOESS.VISwater.EmisCoeff.bin
ln -sf $emiscoef_MWwater ./FASTEM6.MWwater.EmisCoeff.bin
ln -sf $aercoef  ./AerosolCoeff.bin
ln -sf $cldcoef  ./CloudCoeff.bin


# Copy CRTM coefficient files based on entries in satinfo file
for file in $(awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq) ;do
   ln -sf ${fixcrtm}/${file}.SpcCoeff.bin ./
   ln -sf ${fixcrtm}/${file}.TauCoeff.bin ./
done

# Read from previous cycles for satbias predictors (no online satbias) 
PASSIVE_BC=.false.
UPD_PRED=0
ln -sf ${COMINgdas}/gdas.t${hhprior}z.abias           satbias_in
ln -sf ${COMINgdas}/gdas.t${hhprior}z.abias_pc        satbias_pc

#-----------------------------------------------------------------------
#
# Build the GSI namelist on-the-fly
#
#-----------------------------------------------------------------------
#

. $scripts/regression_nl_update.sh

SETUP="$SETUP_update"
GRIDOPTS="$GRIDOPTS_update"
BKGVERR="$BKGVERR_update"
ANBKGERR="$ANBKERR_update"
JCOPTS="$JCOPTS_update"
STRONGOPTS="$STRONGOPTS_update"
OBSQC="$OBSQC_update"
OBSINPUT="$OBSINPUT_update"
SUPERRAD="$SUPERRAD_update"
HYBRID_ENSEMBLE='ensemble_path="",'
SINGLEOB="$SINGLEOB_update"

if [ "$debug" = ".false." ]; then
   . $scripts/regression_namelists.sh hafs_envar
else
   . $scripts/regression_namelists_db.sh hafs_envar
fi

cat << EOF > gsiparm.anl

$gsi_namelist

EOF

# Copy executable and fixed files to $tmpdir
if [[ $exp == *"updat"* ]]; then
   $ncp $gsiexec_updat  ./gsi.x
elif [[ $exp == *"contrl"* ]]; then
   $ncp $gsiexec_contrl ./gsi.x
fi

# Run GSI
cd $tmpdir
echo "run gsi now"
eval "$APRUN $tmpdir/gsi.x > stdout 2>&1"
rc=$?
exit $rc




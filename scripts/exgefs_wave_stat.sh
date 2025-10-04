#!/bin/bash
#                                                                       
################################################################################
#                                                                              #
# exgefs_wave_stats.sh - Compute unified statistics for global wave ensemble   #
#                                                                              #        
# Packs ensemble mean, spread and probabilities in grib2 format.               #
#                                                                              #             
# Requirements:                                                                #    
# - WGRIB2 with IPOLATES library                                               #              
#                                                                              #              
# Origination:                                                                 #
# - Unreported Waves Group Developer, Feb 2008                                 #               
#                                                                              #          
# Changes:                                                                     #           
# - expanded parameter list including partitioned data                         #
#   (list in parameter array) (JH Alves, Jan 2014)                             #                 #                                                                              #
# - introduced wave ensemble bulletin following spectral bulletin format       #
#   (JH Alves, Jan 2014)                                                       #    
# - introduced two USH scripts for post proc                                   #
#   - wave_ens_stats.sh : generate unified stats files (mean, spread, prob)    #
#   - wave_ens_bull.sh : generates wave ensemble bulletin files                #
#   (JH Alves, Jan 2014)                                                       #
# - mpiserial for parallel processing (JH Alves, Jan 2014)                     #
# - Changes to wave_ens_stats (fortran) for paralellism: code now              #
#    computes separately stats type (mean, spread or prob) and prob            #
#    level (JH Alves, Jan 2014)                                                #
#                                                                              #
# Update log since 2014                                                        #
# Nov2019 JHAlves - Transitioning to GEFS workflow                             #
# Dec2019 JHAlves RPadilla - Merging wave scripts to global workflow           #
# Jan2025 SBanihash - Adding this script to the global workflow for gefsv13    #
# Oct2025 SBanihash - Updating script to follow new implementation standards   #
################################################################################
#
cat << EOF

    *************************************
    ********** WAVE STAT SCRIPT *********
    *************************************

Starting at : $(date)
-------------

EOF

#
# 0.a System-specific settings
#
nens=${NMEM_ENS:?Parameter NMEM_ENS required for ensemble statistics}
nmembn=$(( nens + 1 ))
#
# Initialize the array
membn_array=()

# Populate the array (the seq output is split by whitespace and added to the array)
for i in $(seq -f "%03g" 0 "$nens"); do
    membn_array+=("$i")
done

#
# 0.b Define model grid
#
source "${USHgfs}/wave_domain_grid.sh"
process_grdID "${wavepostGRD}"

# Script will run only if pre-defined NTASKS
#     The actual work is distributed over these tasks.
#
#
if [[ -z "${NTASKS}" ]]; then
  export err=1
  err_exit "Requires NTASKS to be set"
fi

#
# 0.c Time management
#
fhr3=$(printf %03i ${FORECAST_HOUR})
valid_time=$(date -u -d "${PDY} ${cyc} + ${FORECAST_HOUR} hours" "+%Y%m%d%H")
ymdh_init=$(date -u -d "${valid_time:0:8} ${valid_time:8:2} - ${WAVHINDH} hours" "+%Y%m%d%H")

fcmdnow=cmdfile.${fhr3}

mkdir -p "output_${ymdh_init}" # Use -p to avoid errors if it already exists
cd "output_${ymdh_init}" || exit 1

rm -f ${fcmdnow}
touch "${fcmdnow}"

# 0.d Parameter selection and deployment of arrays
#
ASWELL=(SWELL1 SWELL2 SWELL3) # Indices of HS from partitions
ASWPER=(SWPER1 SWPER2 SWPER3) # Indices of PERIODS from partitions
#  (should be same as ASWELL)
ASWDIR=(SWDIR1 SWDIR2 SWDIR3) # Indices of PERIODS from partitions
export arrpar=(HTSGW PERPW ICEC IMWF MWSPER DIRPW WVHGT WVPER WVDIR WWSDIR WIND WDIR "${ASWELL[@]}" "${ASWDIR[@]}" "${ASWPER[@]}")

nparam=$(echo "${arrpar[@]}" | wc -w)

export nparam

#
# 1. Get Input files for current script 
#

#
# 1.a Link grib2 data for all members
#
ngrib=0
inc=$FHOUT_HF_WAV
ftype="mem"
      
ngrib=$(( ngrib + 1 ))
for me in ${membn}; do
  ENSTAG=${ftype}${me}
  cpfile=${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/gridded/${grdNAME}/${RUN}.${cycle}.${grdNAME}.f${fhr3}.grib2
  if [ -s "${cpfile}" ] ; then
    ln -s  "$cpfile"  "./${RUN}.${cycle}.${ENSTAG}.${grdNAME}.f${fhr3}.grib2"
  else
    export err=2
    err_exit "No ${cpfile} copied."
  fi
done

# Prepare separate data files to reduce copy load to tmp directories
# 
# 2.a Command file set-up
rm -f cmdfile cmdfile.$

iparam=1

# Number of expected extracted files if nparam * nmembn
while [[ "${iparam}" -le "${nparam}" ]]
do
  nip=${arrpar[$((iparam - 1))]}
  echo ${nip}
  prepar=${nip%?} #Part prefix (assumes 1 digit index)
  paridx=${nip: -1}
  npart=0
  case ${prepar} in
    HTSG)   nnip=${nip} ; snip=hs ;;
    PERP)   nnip=${nip} ; snip=tp ;;
    ICE)   nnip=${nip} ; snip=ice ;;
    IMW)   nnip=${nip} ; snip=tm ;;
    MWSPE)   nnip=${nip} ; snip=tz ;;
    DIRP)   nnip=${nip} ; snip=pdir ;;
    WVHG)  nnip=${nip} ; snip=wshs ;;
    WVPE)  nnip=${nip} ; snip=wstp ;;
    WVDI)  nnip=${nip} ; snip=wsdir ;;
    WWSDI)  nnip=${nip} ; snip=wwdir ;;
    WIN)    nnip=${nip} ; snip=wnd ;;
    WDI)    nnip=${nip} ; snip=wnddir ;;
    SWELL)  nnip=${nip} ; snip=hswell ; npart=1 ;;
    SWDIR)  nnip=${nip} ; snip=dswell ; npart=1 ;;
    SWPER)  nnip=${nip} ; snip=tswell ; npart=1 ;;
    *)       nnip= ;;
  esac

  inc=${FHOUT_HF_WAV}
  if [[ "${iparam}" -eq 3  || "${iparam}" -eq 4  ||  "${iparam}" -eq 5  || \
      "${iparam}" -eq 10  || "${iparam}" -eq 15  ||  "${iparam}" -eq 16 || \
      "${iparam}" -eq 17  || "${iparam}" -eq 18  ||  "${iparam}" -eq 21 ]]
  then
     echo "Parameter ${snip} not yet available for stats"
  else
    for me in ${membn}; do
      ENSTAG=${ftype}${me}
      infile=${RUN}.${cycle}.${ENSTAG}.${grdNAME}.f${fhr3}.grib2
      outfile=${nnip}_${me}.t${cyc}z.${grdNAME}.f${fhr3}.grib2
      wgfileout=wgrib_${nnip}_${me}.out
      if [ "${npart}" = "0" ]; then
        echo " $WGRIB2 -match ${nip} -match surface ${infile} -grib ${outfile} 2>&1 |tee ${wgfileout}" >> "${fcmdnow}"
      else
        echo " $WGRIB2 -match ${prepar} -match \"${paridx} in sequence\" ${infile} -grib ${outfile} 2>&1 | tee ${wgfileout}" >> "${fcmdnow}"
      fi
    done    #for members
  fi
  iparam=$(( iparam + 1 ))
done    #for parameters
# END all loops

# 2.c Execute poe or serial command files

echo " INFO: Generating ${nmembn} hourly to ${FHMAX_WAV}h wave ensembles stats files "
"${HOMEgfs}/ush/run_mpmd.sh" ${fcmdnow} && true
export err=$?
if [[ ${err} -ne 0 ]]; then
  err_exit "run_mpmd.sh failed!"
fi


#
# 2. Generate ensemble mean, spread and probability files
# 
# 2.b Populate command files with stats wave_ens_stats.sh calls
#
rm -f cmdmfile cmdfile.$ cmdmprog
rm -f "${fcmdnow}"
iparam=1
while [[ "${iparam}" -le "${nparam}" ]]
do
  nip=${arrpar[$((iparam - 1))]}
  if [[ "${iparam}" -eq 3  || "${iparam}" -eq 4  ||  "${iparam}" -eq 5  || \
      "${iparam}" -eq 10  ||  "${iparam}" -eq 15  ||  "${iparam}" -eq 16  || \
      "${iparam}" -eq 17  ||  "${iparam}" -eq 18  ||  "${iparam}" -eq 21 ]]
  then
    echo " Parameter $nip not yet available in grib2 library "
  else
# Line for doing per parameter, per time stamp
    echo "nip ngrib FORECAST_HOUR: ${nip}, ${ngrib}, ${FORECAST_HOUR}"
    echo " ${HOMEgfs}/ush/wave_ens_stat.sh ${nip} ${ngrib} ${FORECAST_HOUR} 1 ${grdNAME} " >> cmdfile
  fi
  iparam=$(( iparam + 1))
done

# 2.c Execute poe or serial command files

echo " INFO: Generating ${nmembn} hourly to ${FHMAX_WAV}h wave ensembles stats files "

"${HOMEgfs}/ush/run_mpmd.sh" cmdfile
export err=$?
if [[ ${err} -ne 0 ]]; then
  err_exit "run_mpmd.sh failed!"
fi

# Regroup all outputs in parameter/stats files
# Regrouping has to be sequential per parameter, per hour


iparam=1

while [[ "${iparam}" -le "${nparam}" ]]
do
  nip=${arrpar[$((iparam - 1))]}
  case ${nip} in
    HTSGW)   stypes='mean spread prob' ; snip=hs ;;
    PERPW)   stypes='mean spread prob' ; snip=tp ;;
    ICEC)    stypes='mean spread prob' ; snip=ice ;;
    DIRPW)   stypes='mean spread ' ; snip=pdir ;;
    IMWF)    stypes='mean spread prob' ; snip=tm ;;
    MWSP)    stypes='mean spread prob' ; snip=tz ;;
    WVHGT)   stypes='mean spread prob' ; snip=wshs ;;
    WVPER)   stypes='mean spread prob' ; snip=wstp ;;
    WVDIR)   stypes='mean spread' ; snip=wsdir ;;
    WWSDIR)  stypes='mean spread' ; snip=wwdir ;;
    WIND)    stypes='mean spread prob' ; snip=wnd ;;
    WDIR)    stypes='mean spread' ; snip=wnddir ;;
    SWELL1)  stypes='mean spread prob' ; snip=hswell1 ;;
    SWELL2)  stypes='mean spread prob' ; snip=hswell2 ;;
    SWELL3)  stypes='mean spread prob' ; snip=hswell3 ;;
    SWDIR1)  stypes='mean spread' ; snip=dswell1 ;;
    SWDIR2)  stypes='mean spread' ; snip=dswell2 ;;
    SWDIR3)  stypes='mean spread' ; snip=dswell3 ;;
    SWPER1)  stypes='mean spread prob' ; snip=tswell1 ;;
    SWPER2)  stypes='mean spread prob' ; snip=tswell2 ;;
    SWPER3)  stypes='mean spread prob' ; snip=tswell3 ;;
    *)       nnip= ;;
  esac

  par_dir=tmp_${nip}
  
  if [[ "${iparam}" -eq 3  || "${iparam}" -eq 4  ||  "${iparam}" -eq 5  || \
      "${iparam}" -eq 10  ||  "${iparam}" -eq 15  ||  "${iparam}" -eq 16  || \
      "${iparam}" -eq 17  ||  "${iparam}" -eq 18  ||  "${iparam}" -eq 21 ]]
  then
    echo " Parameter $nip not yet available in grib2 library "
  else
# 2.e Cleanup base parameter files per member
    rm -f ${nip}_??.t${cyc}z.grib2

    for stype in ${stypes}; do
      ingrib=${snip}_${stype}.${fhr3}.grib2
      outgrib=${RUN}.t${cyc}z.${stype}.${grdNAME}.f${fhr3}.grib2
      echo "$WGRIB2  ./${par_dir}/${valid_time}/${ingrib} -append -grib ./${outgrib} " >> ${stype}.ncmdfile

    done

  fi
  iparam=$((iparam + 1))
  echo "IPARAM: ${iparam}"
done

chmod 744 mean.ncmdfile
chmod 744 spread.ncmdfile
chmod 744 prob.ncmdfile

echo " INFO: Generating ${nmembn} hourly to ${FHMAX_WAV}h wave ensembles stats files "

for stype in ${stypes}; do
  "./${stype}.ncmdfile"
  export err=$?
  if [[ ${err} -ne 0 ]]; then
    err_exit "${stype}.ncmdfile failed!"
  fi
done

# 3 Check if buoy input files exist and copy
# #
#
buoyfile=wave_${NET}.buoys
if [ -s ${PARMgfs}/wave/${buoyfile} ] ; then
  cp  ${PARMgfs}/wave/${buoyfile} buoy_file.data
  echo " ${PARMgfs}/wave/${buoyfile} copied to buoy_file.data."
else
  export err=2
  err_exit "No ${PARMgfs}/wave/${buoyfile} copied."
fi

											      
# 3.a Buoy locations file massaging

sed '/\$/d' buoy_file.data | sed '/STOPSTRING/d ' > buoy.file

nbuoys=$(wc -l < buoy.file)

# 3.b Command file set-up
rm -f cmdfile cmdfile.$

ibuoy=1

# 3.c Create bundled grib2 file with all parameters


cat ${RUN}.t${cyc}z.{mean,prob,spread}.${grdNAME}.f${fhr3}.grib2 | $WGRIB2 - -match "(HTSGW|PERPW|WIND)" -grib gribfile > gribfile.out 2>&1

if [ -s gribfile ]
then
  echo "   Gribfile for bulletins created"
else
  export err=7
  err_exit "No gribfile created for ${TYPE}, no bulls"
fi

rm -f ${fcmdnow}
touch ${fcmdnow}

# 3.d Loop through buoys and populate cmdfiles with calls to wave_ens_bull.sh
ifile=0
while [ ${ibuoy} -le ${nbuoys} ]
do
  bline=`sed ''$ibuoy'!d' buoy.file`
  blat=`echo ${bline} | awk '{print $2}'`
  blon=`echo ${bline} | awk '{print $1}'`
  bnom=`echo ${bline} | awk '{print $3}' | sed "s/'//g"`

  echo "${HOMEgfs}/ush/wave_ens_bull.sh ${blon} ${blat} ${bnom} ${FORECAST_HOUR} 2>&1 | tee  bull_${bnom}.out" >> ${fcmdnow}

  (( ibuoy = ibuoy + 1 ))
  (( ifile = ifile + 1 ))

done

# 3.e Execute poe or serial cmdfile
echo " Generating bulletins and ts files for ${nbuoys} locations."

if [ ${CFP_MP:-"NO"} = "NO" ]; then
  nfile=0
  ifile=0
  iline=1
  ifirst='yes'
  nlines=$( wc -l ${fcmdnow} | awk '{print $1}' )
  while [ ${iline} -le ${nlines} ]; do
    line=$( sed -n ''$iline'p' ${fcmdnow} )
    if [ -z "$line" ]; then
      break
    else
      if [ "$ifirst" = 'yes' ]; then
	echo "#!/bin/sh" > "cmdmfile.$nfile"
        echo " ${DATA}/output_${ymdh_init}/cmdmfile.${nfile}" >> cmdmprog
	chmod 744 "cmdmfile.${nfile}"
      fi
      echo $line >> "cmdmfile.${nfile}"
      nfile=$(( nfile + 1 ))
      if [ "${nfile}" -eq "${NTASKS}" ]; then
        nfile=0
        ifirst='no'
      fi
      iline=$(( iline + 1 ))
    fi
  done
fi

echo "   Executing the wave_ens_bull scripts at : $(date)"
  
ncmds=$(wc -l < cmdmprog)
if [[ ${NTASKS} -lt ${ncmds} ]]; then
 if [[ "${USE_CFP:-}" = "YES" ]]; then
   echo "WARNING: Not enough processors for MPMD, '${NTASKS} < ${ncmd}', running in serial mode"
   export USE_CFP="NO"
 fi
fi
"${USHgfs}/run_mpmd.sh" "cmdmprog"
export err=$?
if [[ ${err} -ne 0 ]]; then
  err_exit "run_mpmd.sh failed!"
fi

#Assign COMOUT_STATION
MEMDIR="ensstat"  YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_STATION_ENS:COM_WAVE_STATION_TMPL

ibuoy=1
# 3.f Check for errors
while (( ibuoy <= nbuoys ))
do

  bline=`sed ''$ibuoy'!d' buoy.file`
  blat=`echo ${bline} | awk '{print $2}'`
  blon=`echo ${bline} | awk '{print $1}'`
  bnom=`echo ${bline} | awk '{print $3}' | sed "s/'//g"`

  if [ ! -s ${RUN}.${bnom}.f${fhr3}.bull ]
  then
    export err=9
    err_exit "ABNORMAL EXIT: ERR in generating bulettin file,  No ${RUN}.${bnom}.bull file created"
  else
    echo -e "\n Bulletin file ${RUN}.${bnom}.${fhr3}.bull generated succesfully.\n"
    rm -f bull_${bnom}.out
  fi
  ibuoy=$(( ibuoy + 1 ))
done

tar cf ${RUN}.t${cyc}z.f${fhr3}.bull_tar ${RUN}.*.f*.bull
rm -f ${RUN}.*.bull
tar cf ${RUN}.t${cyc}z.f${fhr3}.station_tar ${RUN}.*.f*.ts
rm -f ${RUN}.*.ts

# 4.a Output all grib2 parameter files to COMOUT

MEMDIR="ensstat" GRID=${wavepostGRD} YMD=${PDY} HH=${cyc} declare_from_tmpl COMOUT_WAVE_GRID_ENS:COM_WAVE_GRID_TMPL


for stype in mean spread prob
do
  fcopy=${RUN}.t${cyc}z.${stype}.${grdNAME}.f${fhr3}.grib2
  if [[ -s ${fcopy} ]]
  then
    echo "   Copying ${fcopy} to ensstat and ALERT if SENDDBN=YES"
    cp -f ${fcopy}  "${COMOUT_WAVE_GRID_ENS}"
# 2.g Alert DBN
    if [[ "$SENDDBN" = 'YES' ]]
    then
      MODCOM=$(echo ${NET}_${COMPONENT} | tr '[a-z]' '[A-Z]')
      $DBNROOT/bin/dbn_alert MODEL ${MODCOM}_GB2 $job ${ROTDIR}/${RUN}.${PDY}/${cyc}/${ENSTAG}/products/wave/gridded/${fcopy}
    fi
  else
    export err=6
    err_exit "ERROR: ${modIE} fcst ${date} ${cycle}: ${fcopy} not fouund."
  fi
done


# 4.b Output all station and bull tars to COMOUT (TO DO: this should go somewhere else)
#
bcopy_station=${RUN}.t${cyc}z.f${fhr3}.station_tar
bcopy_bull=${RUN}.t${cyc}z.f${fhr3}.bull_tar
if [[ -s "$bcopy_station" ]] && [[ -s "$bcopy_bull" ]]; then
  echo "   Copying tar files to ensstat"
  cp -f ${bcopy_station}  "${COMOUT_WAVE_STATION_ENS}"
  cp -f ${bcopy_bull}  "${COMOUT_WAVE_STATION_ENS}"
else
  export err=6
  err_exit "${modIE} fcst ${date} ${cycle}: ${bcopy_station} and ${bcopy_bull} not fouund."
fi

echo "$job completed normally"
#
echo "Ending at : `date`"
#
# END
#

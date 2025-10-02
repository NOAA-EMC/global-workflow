#!/bin/bash
#                                                                       
################################################################################
#
# UNIX Script Documentation Block
# Script name:         wave_ens_bull.sh
# Script description:  Create buoy bulletin for NCEP Global Wave Ensemble
#
# Author:   Jose-Henrique Alves Org: NCEP/EMC      Date: 2014-01-16
# Abstract: Creates bulletin for NCEP Global Wave Ensemble using grib2 data.
#           Values at buoy locations are extracted using wgrib2 bi-linear 
#           interpolation (-new_grid) and requires IPOLATES lib.
#
# Script history log:
# 2019-05-06  J-Henrique Alves First Version.
# 2019-11-02  J-Henrique Alves Ported to global-workflow.
# 2025-03-01  S-Banihashemi modified script to be used in gefsv13 implementation
#
################################################################################
#
# 0.  Preparations
# 0.a Basic modes of operation
#
  seton='-xa'
  setoff='+xa'
  set $seton

  echo -e '\n         ******************************************'
  echo      '         *** WAVE ENSEMBLE BUOY BULLETIN SCRIPT ***'
  echo -e   '         ******************************************\n'
  echo " Starting at : `date`"
#
# 0.b External dependencies and paths
#
  scripname=wave_ens_bull.sh
#
# 0.b Date and time stuff
#
  export YMD=$PDY
  export YMDH=${PDY}${cyc}
  export tcycz=t${cyc}z
#
# 0.c Buoy location parameters (from stdin)
#
  blon=$1
  blat=$2 
  bnom=$3 
  bfhr=$4
  
#
# 0.d Plumbing
#
  BULLdir=${bnom}_bull
  rm -rf $BULLdir
  mkdir -p $BULLdir
  err=$?
  if [ "$err" != '0' ]
  then
    set +x
    echo ' '
    echo '******************************************************* '
    echo "  FATAL ERROR: NOT ABLE TO CREATE TEMP DIR ${BULLdir} "
    echo '******************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "FATAL ERROR in ${scripname}: Could not create temp directory"
    exit 1
  fi

  cd ${BULLdir}
#
# 0.e Output file names
#

  FH3=$(printf "%03d" $bfhr)
  bfil="${RUN}.${bnom}.f${FH3}.bull"
  tfil="${RUN}.${bnom}.f${FH3}.ts"
#
# 1. Prepare input data
#
# 1.a Interpolate from gribfile at model res to high resolution at buoy location 
# (wgrib2 + IPOLATES -> bi-linear)
#
  $WGRIB2 ../gribfile -new_grid_winds earth \
                     -new_grid_interpolation bilinear -new_grid latlon \
                     ${blon}:2:.01 ${blat}:2:.01 grbint.${bnom} \
                     2>&1 | tee  buoy_interp.out
#
  if ! [ -f grbint.${bnom} ]
  then
    set +x
    echo ' '
    echo '******************************************************* '
    echo "  FATAL ERROR: FAILED TO CREATE FILE grbint.${bnom}   "
    echo '******************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "FATAL ERROR creating grbint.${bnom} in ${scripname}"
    exit 2
  fi
#   
# 1.b Extract parameters at buoy locations from higher res interpolated file 
#
    valpdy=(`$WGRIB2 grbint.${bnom} -match HTSGW -match mean -vt \
          | sed 's/[,=]/ /g' | awk '{print $NF}' | cut -c1-8`)
    vald=(`$WGRIB2 grbint.${bnom} -match HTSGW -match mean -vt \
          | sed 's/[,=]/ /g' | awk '{print $NF}' | cut -c7-8`)
    valt=(`$WGRIB2 grbint.${bnom} -match HTSGW -match mean -vt \
          | sed 's/[,=]/ /g' | awk '{print $NF}' | cut -c9-10`)
    hsb=(`$WGRIB2 grbint.${bnom} -match HTSGW -match mean -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    hspb=(`$WGRIB2 grbint.${bnom} -match HTSGW -match spread \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    tpb=(`$WGRIB2 grbint.${bnom} -match PERPW -match mean -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    tspb=(`$WGRIB2 grbint.${bnom} -match PERPW -match spread \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    ub=(`$WGRIB2 grbint.${bnom} -match WIND -match mean -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    usb=(`$WGRIB2 grbint.${bnom} -match WIND -match spread -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p1b=(`$WGRIB2 grbint.${bnom} -match HTSGW -match 'prob >0.6' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p2b=(`$WGRIB2 grbint.${bnom} -match HTSGW -match 'prob >1' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p3b=(`$WGRIB2 grbint.${bnom} -match HTSGW -match 'prob >2' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p4b=(`$WGRIB2 grbint.${bnom} -match HTSGW -match 'prob >5.5' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p5b=(`$WGRIB2 grbint.${bnom} -match HTSGW -match 'prob >7' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p6b=(`$WGRIB2 grbint.${bnom} -match HTSGW -match 'prob >9' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
#
# Length of parameter vectors
#
    tlen=`echo ${hsb[@]} | wc -w`
#
# Check for error in reading parameters from interpolated file
#
  if [ ! $vald ] || [ ! valt ] || [ ! hsb ] || [ ! hspb ] || [ ! tpb ] || \
     [ ! tspb ] || [ ! ub ] || [ ! usb ] || [ ! p1b ] || [ ! p2b ] || \
     [ ! p3b ] || [ ! p4b ] || [ ! p5b ] || [ ! p6b ]
  then
    set +x
    echo ' '
    echo '******************************************************* '
    echo "  FATAL ERROR: FAILED TO READ PARAMS FROM grbint${bnom}  "
    echo '******************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "FATAL ERROR reading parameters from grbint.${bnom} in ${scripname}"
    exit 3
  fi
#
# Warning if any parameter has UNDEF value
#
  UNDF=9.999e+20
  UNDFCHK=`echo ${hsb[@]} ${hspb[@]} ${tpb[@]} ${tspb[@]} ${ub[@]} ${usb[@]} \
          ${p1b[@]} ${p2b[@]} ${p3b[@]} ${p4b[@]} ${p5b[@]} ${p6b[@]}`
  if [ `echo $UNDFCHK | grep $UNDF | cut -c1` ]
  then
    set +x
    echo ' '
    echo '******************************************************* '
    echo "  WARNING: PARAMETER IS UNDEFINED IN grbint.${bnom}      "
    echo '******************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "WARNING: parameter is UNDEFINED in grbint.${bnom} in ${scripname}"
  fi

#
# 2. Generate bulletin
#
    if [ "$bfhr" -eq "${FHMIN_WAV}" ]
    then
    	printf "\n Location : "$bnom"      ("$blat"N  "$blon"W)\n" > $bfil
    	printf " Model    : NCEP Global Wave Ensemble System \n" >> $bfil
    	printf " Cycle    : "$PDY" "$cycle" UTC\n" >> $bfil
    	printf "\n+-------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+\n" >> $bfil
    	printf   "| day   | Hs avg | Hs spr | Tp avg | Tp spr | U10avg | U10spr | P(Hs>) | P(Hs>) | P(Hs>) | P(Hs>) | P(Hs>) | P(Hs>) |\n" >> $bfil
    	printf   "|  hour |  (m)   |  (m)   |  (s)   |  (s)   |  (m/s) |  (m/s) |  1.00m |  2.00m | 3.00m  |  5.50m |  7.00m |  9.00m |\n" >> $bfil
    	printf   "+-------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+\n" >> $bfil
    fi
    for (( it=1; it<=$tlen; it++ ))
    do
	    tdum=$(printf "%d" $((10#${valt[$it-1]})))
	    ddum=$(printf "%d" $((10#${vald[$it-1]})))
	    printf '| %02d %02d' "$ddum" "$tdum" >> $bfil
	    printf ' | %5.2f ' \
	   $(echo "${hsb[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
	   $(echo "${hspb[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
	   $(echo "${tpb[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${tspb[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${ub[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${usb[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
           $(echo "${p1b[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${p2b[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${p3b[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${p4b[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${p5b[$it-1]:0:4}" | awk '{printf "%.5f", $1}') \
	   $(echo "${p6b[$it-1]:0:4}" | awk '{printf "%.5f", $1}') >> $bfil
           printf ' |\n' >> $bfil
    done

    if [ "$bfhr" -eq "${FHMAX_WAV}" ]
    then
	    printf   "+-------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+\n" >> $bfil
	    printf "                                                               Hs  : Significant wave height\n" >> $bfil
	    printf "                                                               Tp  : Peak period\n" >> $bfil
	    printf "                                                               U10 : Wind speed at a height of 10m above the surface\n" >> $bfil
	    printf "                                                               avg : Average of ensemble members\n" >> $bfil
	    printf "                                                               spr : Spread (standard deviation) of ensemble members\n" >> $bfil
	    printf "                                                               P(Hs >): Probability of Hs exceeding given threshold\n" >> $bfil
	    printf " NOAA/NWS/NCEP Marine Modeling and Analysis Branch, $PDY" >> $bfil
    fi
#
# 2.b Create time series output
#
    if [ "$bfhr" -eq "${FHMIN_WAV}" ] 
    then
	    printf   " date   hour Hs avg Hs spr Tp avg Tp spr U10avg U10spr \n" >> $tfil
	    printf   "               (m)    (m)    (s)    (s)  (m/s)  (m/s)  \n" >> $tfil
	    printf   " ----------------------------------------------------- \n" >> $tfil
    fi 
  for (( it=1; it<=$tlen; it++ ))
  do
	 tdum=$(printf "%d" $((10#${valt[$it-1]})))
         printf ' %8.8i %02d' ${valpdy[$it-1]} "$tdum" >> $tfil
         printf ' %5.2f ' \
         $(echo "${hsb[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
	 $(echo "${hspb[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
         $(echo "${tpb[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
         $(echo "${tspb[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
         $(echo "${ub[$it-1]:0:4}" | awk '{printf "%.5f", $1}')\
         $(echo "${usb[$it-1]:0:4}" | awk '{printf "%.5f", $1}') >> $tfil
          printf '\n' >> $tfil
  done
#
# 2.c Check for errors in creating bulletin file
#
  if [ -f ${bfil} ] && [ -f ${tfil} ]
  then
    echo -e "\n  bulletin and ts-file created for location ${bnom}.\n"
  else
    set +x
    echo ' '
    echo '******************************************************* '
    echo '*** FATAL ERROR: BULL/TS FILES AT ${bnom} NOT FOUND ***'
    echo '******************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    echo "FATAL ERROR : BULL/TS FILES NOT FOUND"
    exit 4
  fi
#
# 3. Copy and Cleanup
#
    mv -f ${bfil} ../.
    mv -f ${tfil} ../.
    rm -rf ${bnom}_bull

# End of buoy bulletin script

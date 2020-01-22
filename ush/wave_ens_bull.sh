#!/bin/bash
#                                                                       
################################################################################
#                                                                       
# wave_ens_bull.sh - Create buoy bulletin for NCEP Global Wave Ensemble
#                                                                       
# Format follows general idea of regular bulletins created from point output. 
# Uses grib2 gridded field data. Values at buoy locations are extracted  
# using wgrib2 bi-linear interpolation (-new_grid) and requires IPOLATES lib  
# to be installed. Slicing and dicing of data into bulletin structure is made 
# using bash built-ins (sed, awk, printf etc).                               
#                                                                           
# Requirements:                                                             
# - wgrib2 with IPOLATES library                                            
#                                                                           
# Origination: 
# - Jose-Henrique Alves, Jan 2014                               
#                                                                       
# Changes:                                                              
# -                                                                       
#                                                                       
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
  export wgrib2=$utilexec/wgrib2
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
  bfil=$4
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
    ../postmsg "$jlogfile" "FATAL ERROR in ${scripname}: Could not create temp directory"
    exit 1
  fi

  cd ${BULLdir}
#
# 0.e Output file names
#
  bfil="${MDC}.${bnom}.bull"
  tfil="${MDC}.${bnom}.ts"
#
# 1. Prepare input data
#
# 1.a Interpolate from gribfile at model res to high resolution at buoy location 
# (wgrib2 + IPOLATES -> bi-linear)
#
  $utilexec/wgrib2 ../gribfile -new_grid_winds earth \
                     -new_grid_interpolation bilinear -new_grid latlon \
                     ${blon}:2:.01 ${blat}:2:.01 grbint.${bnom} \
                     1> buoy_interp.out 2>&1
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
    ../postmsg "$jlogfile" "FATAL ERROR creating grbint.${bnom} in ${scripname}"
    exit 2
  fi
#   
# 1.b Extract parameters at buoy locations from higher res interpolated file 
#
    valpdy=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match mean -vt \
          | sed 's/[,=]/ /g' | awk '{print $NF}' | cut -c1-8`)
    vald=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match mean -vt \
          | sed 's/[,=]/ /g' | awk '{print $NF}' | cut -c7-8`)
    valt=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match mean -vt \
          | sed 's/[,=]/ /g' | awk '{print $NF}' | cut -c9-10`)
    hsb=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match mean -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    hspb=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match spread \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    tpb=(`$utilexec/wgrib2 grbint.${bnom} -match PERPW -match mean -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    tspb=(`$utilexec/wgrib2 grbint.${bnom} -match PERPW -match spread \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    ub=(`$utilexec/wgrib2 grbint.${bnom} -match WIND -match mean -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    usb=(`$utilexec/wgrib2 grbint.${bnom} -match WIND -match spread -lon \
          ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p1b=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match 'prob >0.6' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p2b=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match 'prob >1' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p3b=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match 'prob >2' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p4b=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match 'prob >5.5' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p5b=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match 'prob >7' \
          -lon ${blon} ${blat} | sed 's/[,=]/ /g' | awk '{print $NF}'`)
    p6b=(`$utilexec/wgrib2 grbint.${bnom} -match HTSGW -match 'prob >9' \
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
    ../postmsg "$jlogfile" "FATAL ERROR reading parameters from grbint.${bnom} in ${scripname}"
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
    ../postmsg "$jlogfile" "WARNING: parameter is UNDEFINED in grbint.${bnom} in ${scripname}"
  fi
#
# 2. Generate bulletin
#
    printf "\n Location : "$bnom"      ("$blat"N  "$blon"W)\n" > $bfil
    printf " Model    : NCEP Global Wave Ensemble System (${MDC})\n" >> $bfil
    printf " Cycle    : "$PDY" "$cycle" UTC\n" >> $bfil
    printf "\n+-------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+\n" >> $bfil
    printf   "| day   | Hs avg | Hs spr | Tp avg | Tp spr | U10avg | U10spr | P(Hs>) | P(Hs>) | P(Hs>) | P(Hs>) | P(Hs>) | P(Hs>) |\n" >> $bfil
    printf   "|  hour |  (m)   |  (m)   |  (s)   |  (m)   |  (m/s) |  (m/s) |  1.00m |  2.00m | 3.00m  |  5.50m |  7.00m |  9.00m |\n" >> $bfil
    printf   "+-------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+\n" >> $bfil

    for (( it=1; it<=$tlen; it++ ))
    do
      tdum=`expr ${valt[$it-1]} / 1`
      ddum=`expr ${vald[$it-1]} / 1`
      printf '| %2.2i %2.2i' $ddum $tdum >> $bfil
      printf ' | %5.2f ' \
           ${hsb[$it-1]:0:4} \
           ${hspb[$it-1]:0:4} \
           ${tpb[$it-1]:0:4} \
           ${tspb[$it-1]:0:4} \
           ${ub[$it-1]:0:4} \
           ${usb[$it-1]:0:4} \
           ${p1b[$it-1]:0:4} \
           ${p2b[$it-1]:0:4} \
           ${p3b[$it-1]:0:4} \
           ${p4b[$it-1]:0:4} \
           ${p5b[$it-1]:0:4} \
           ${p6b[$it-1]:0:4} >> $bfil
      printf ' |\n' >> $bfil
    done

    printf   "+-------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+\n" >> $bfil
    printf "                                                               Hs  : Significant wave height\n" >> $bfil
    printf "                                                               Tp  : Peak period\n" >> $bfil
    printf "                                                               U10 : Wind speed at a height of 10m above the surface\n" >> $bfil
    printf "                                                               avg : Average of ensemble members\n" >> $bfil
    printf "                                                               spr : Spread (standard deviation) of ensemble members\n" >> $bfil
    printf "                                                               P(Hs >): Probability of Hs exceeding given threshold\n" >> $bfil
    printf " NOAA/NWS/NCEP Marine Modeling and Analysis Branch, $PDY" >> $bfil
#
# 2.b Create time series output
#
  printf   " date   hour Hs avg Hs spr Tp avg Tp spr U10avg U10spr \n" >> $tfil
  printf   "               (m)    (m)    (s)    (s)  (m/s)  (m/s)  \n" >> $tfil
  printf   " ----------------------------------------------------- \n" >> $tfil
  for (( it=1; it<=$tlen; it++ ))
  do
    tdum=`expr ${valt[$it-1]} / 1`
    printf ' %8.8i %2.2i' ${valpdy[$it-1]} $tdum >> $tfil
    printf ' %5.2f ' \
         ${hsb[$it-1]:0:4} \
         ${hspb[$it-1]:0:4} \
         ${tpb[$it-1]:0:4} \
         ${tspb[$it-1]:0:4} \
         ${ub[$it-1]:0:4} \
         ${usb[$it-1]:0:4} >> $tfil
    printf '\n' >> $tfil
  done
#
# 2.c Check for errors in creating bulletin file
#
  if [ -f ${bfil} ] && [ -f ${tfil} ]
  then
    echo -e "\n ${MDC} bulletin and ts-file created for location ${bnom}.\n"
  else
    set +x
    echo ' '
    echo '******************************************************* '
    echo '*** FATAL ERROR: BULL/TS FILES AT ${bnom} NOT FOUND ***'
    echo '******************************************************* '
    echo ' '
    [[ "$LOUD" = YES ]] && set -x
    ../postmsg "$jlogfile" "FATAL ERROR : BULL/TS FILES NOT FOUND"
    exit 4
  fi
#
# 3. Copy and Cleanup
#
    mv -f ${bfil} ../.
    mv -f ${tfil} ../.
    rm -rf ${bnom}_bull

# End of buoy bulletin script

#! /usr/bin/env bash
################################################################################
#   Script:    
#
source "${USHgfs}/preamble.sh"

fcnt=1
dcnt=1 
ensname=$1
subdata=$2

cd "${subdata}" || true

for outtype in "f2d" "f3d"; do

  if [[ "${outtype}" == "f2d" ]];then
    varlist=${varlist_2d}
  elif [[ "${outtype}" == "f3d" ]];then 
    varlist=${varlist_3d}
    varlist_d=${varlist_3d_d}
  fi

  outdirpre="${subdata}/${outtype}"
  if [[ ! -d "${outdirpre}" ]]; then mkdir -p "${outdirpre}"; fi 

  nh=${FHMIN}
  while [[ ${nh} -le ${FHMAX} ]];do
    fnh=$(printf "%3.3d" "${nh}")

    if [[ "${outtype}" == "f2d" ]];then
      if [[ ${nh} -le ${FHMAXHF} ]];then
        outres="0p25"
      else
        outres="0p50"
      fi
    elif [[ "${outtype}" == "f3d" ]];then
      outres="1p00"
    fi

    if [[ ${nh} -lt ${FHMAXHF} ]];then
      outfreq=$FHOUTHF
    else
      outfreq=$FHOUTLF
    fi                                      

    echo $ensname ==============
    if [[ "$outres" == "0p25" ]];then
      infile1=$COM_ATMOS_GRIB_0p25/gefs.${cycle}.pgrb2.$outres.f${fnh}
      infile2=$COM_ATMOS_GRIB_0p25/gefs.${cycle}.pgrb2b.$outres.f${fnh}
    elif [[ "$outres" == "0p50" ]];then
      infile1=$COM_ATMOS_GRIB_0p50/gefs.${cycle}.pgrb2.$outres.f${fnh}
      infile2=$COM_ATMOS_GRIB_0p50/gefs.${cycle}.pgrb2b.$outres.f${fnh}
    elif [[ "$outres" == "1p00" ]];then
      infile1=$COM_ATMOS_GRIB_1p00/gefs.${cycle}.pgrb2.$outres.f${fnh}
      infile2=$COM_ATMOS_GRIB_1p00/gefs.${cycle}.pgrb2b.$outres.f${fnh}
    fi
    oufile=$outdirpre/ge${ensname}.${cycle}.pgrb2.$outres.f${fnh}
    rm -f $oufile #remove outfile if it already exists before extraction
            
    if [ -f $infile1 ]; then #check if input file exists before extraction
      $WGRIB2 $infile1 | grep -F -f $varlist | $WGRIB2 -i $infile1 -append -grib $oufile>/dev/null
    else
      echo "WARNING: $infile1 does not exist."
    fi 

    if [ -f $infile2 ]; then #check if input file exists before extraction
      $WGRIB2 $infile2 | grep -F -f $varlist | $WGRIB2 -i $infile2 -append -grib $oufile>/dev/null
    else
      echo "WARNING: $infile2 does not exist."
    fi

    #Compute daily average for a subset of variables
    if [[ "$outtype" == "f3d" ]];then
      if ! (( $nh % 6 ));then
        outfile=$subdata/vartmp_raw_vari_ldy${dcnt}.${ensname}.grib2
        $WGRIB2 $infile1 | grep -F -f $varlist_d | $WGRIB2 -i $infile1 -append -grib $outfile>/dev/null
        $WGRIB2 $infile2 | grep -F -f $varlist_d | $WGRIB2 -i $infile2 -append -grib $outfile>/dev/null                                             
        if [[ $fcnt -eq 4 ]];then
          fnd=$(printf "%2.2d" ${dcnt})
          davg_file=$outdirpre/ge${ensname}.${cycle}.pgrb2.$outres.ldy${fnd}
          vcnt=1
          while read vari; do
            davgtmp=$subdata/ge${ensname}.${cycle}.tmp.pgrb2.$outres.ldy${fnd}.${vcnt}
            $WGRIB2 $outfile | grep "$vari" | $WGRIB2 -i $outfile -fcst_ave 6hr $davgtmp>/dev/null
            $WGRIB2 $davgtmp | $WGRIB2 -i $davgtmp -append -grib $davg_file>/dev/null
            rm -f $davgtmp
            vcnt=$(($vcnt + 1))
          done <$varlist_d #variable
          fcnt=1
          dcnt=$(($dcnt + 1))
        else
          fcnt=$(($fcnt + 1))
        fi #If at final lead hour of a given day
      fi #if lead hour is divisible by 6
    fi #if outtype == f3d
    nh=$(($nh + $outfreq))
  done #fhr

done #f2d,f3d

exit 0                                                                                                                                                                                        

#!/bin/bash
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# This script calls the stats program for each requested parameters in         +
# exwave_gwes_stats.sh                                                         +
#                                                                              +
# Jan, 2014                                                                    +
#                                                                              +
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 0.  Preparations
# 0.a Basic modes of operation
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
  seton='-xa'
  setoff='+xa'
  set $seton

  para=$1
  prepar=`echo $para | rev | cut -c2- | rev` #Part prefix (assumes 1 digit index)
  paridx=`echo $para | rev | cut -c-1`  #Part index (assumes 1 digit index)

# Number of grib records
  ngrib=$2

# Number of ensemble members
  nmembn=`echo ${membn} | wc -w`

# Forecast range
  fhour=$3

  mkdir -p tmp_${para}
  cd tmp_${para}

# 0.b Set general parameter settings

  scale='     '
  case $prepar in
    HTSG)   ascale=(0.60 1.00 2.00  3.00  4.00  5.50  7.00  9.00) ;
             scale=${ascale[@]}
             npart=0 ;
             nip='hs' ;
             nnip=${nip} ;
             parcode='10 0 3'  ;;
    PERP)   ascale=(5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0) ;
             scale=${ascale[@]}
             npart=0 ;
             nip='tp' ;
             nnip=${nip} ;
             parcode='10 0 11'  ;;
    DIRP)   ascale='0' ;
             scale=${ascale[@]}
             npart=0 ;
             nip='pdir' ;
             nnip=${nip} ;
             parcode='10 0 10'  ;;
    WIN)    ascale=(3.60 5.65 8.74 11.31 14.39 17.48 21.07 24.67) ;
             scale=${ascale[@]}
             npart=0 ;
             nip='wnd' ;
             nnip=${nip} ;
             parcode='0 2 1'  ;;
    WDI)    ascale='0' ;
             scale=${ascale[@]}
             npart=0 ;
             nip='wnddir' ;
             nnip=${nip} ;
             parcode='0 2 0'  ;;
    WVHG)   ascale=(0.60 1.00 2.00  3.00  4.00  5.50  7.00  9.00) ;
             scale=${ascale[@]}
             npart=0 ;
             nip='wshs' ;
             nnip=${nip} ;
             parcode='10 0 5'  ;;
    WVPE)   ascale=(5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0) ;
             scale=${ascale[@]}
             npart=0 ;
             nip='wstp' ;
             nnip=${nip} ;
             parcode='10 0 6'  ;;
    WVDI)   ascale='0' ;
             scale=${ascale[@]}
             npart=0 ;
             nip='wsdir' ;
             nnip=${nip} ;
             parcode='10 0 4'  ;;
    SWELL)  ascale=(0.60 1.00 2.00  3.00  4.00  5.50  7.00  9.00) ;
             scale=${ascale[@]}
             npart=1 ;
             nip='hswell' ;
             nnip="${nip}"$paridx ;
             parcode='10 0 8'  ;;
    SWPER)  ascale=(5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0) ;
             scale=${ascale[@]}
             npart=1 ;
             nip='tswell' ;
             nnip="${nip}"$paridx ;
             parcode='10 0 9'  ;;
    *)       ascale=${tsscale[@]} ; scale=${ascale[$plev]} ; parcode='   '  ;;
  esac

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
# 1. Compute mean, spread and probability of exceedence 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --+ + + ++
#
# set $seton

  rm -f mean.t${cyc}z.grib2 spread.t${cyc}z.grib2 probab.t${cyc}z.grib2

  nmemb=${nmembn}
  nmembm1=`expr ${nmemb} - 1`
#
# 1.a Create list of combined ensemble member numbers (starting from 00 = NCEP control run)
#
  memb=`seq -w 0 ${nmembm1}`
#
    valtime=`$NDATE ${fhour} ${YMDH}`

    mkdir -p ${valtime}
    cd ${valtime}

    if [ $fhour -eq 0 ] ; then
      ihr='anl'
      hhh='000'
    elif [ $fhour -lt 10 ] ; then
      ihr=${fhour}' hour'
      hhh='00'$fhour
    elif [ $fhour -lt 100 ] ; then
      ihr=${fhour}' hour'
      hhh='0'$fhour
    elif [ $fhour -ge 100 ] ; then
      ihr=${fhour}' hour'
      hhh=$fhour
    fi
#
    rm -f gwes_stats.inp data_* 
#
# 1.b Loop through members
    nme=0
#    while [ ${nme} -lt ${nmemb} ]
    for im in $membn
    do

      infile=../../${para}_${im}.t${cyc}z.grib2
      if [ "${im}" = "00" ]
      then

# 1.b.1 Generate input file for gwes_stats
        echo $YMDH $hhh $nnip $parcode  > gwes_stats.inp
        #echo $YMDH $ngrib $dtgh $nnip $parcode  > gwes_stats.inp
        echo ${nmemb}                 >> gwes_stats.inp
        echo $memb                    >> gwes_stats.inp
        echo ${scale[@]} | wc -w           >> gwes_stats.inp
        echo ${scale[@]}                   >> gwes_stats.inp

# 1.b.2 Get grid dimension for input grib file and pass to fortran code
        nlola=`$WGRIB2 ${infile} -grid -d 1 | sed 's/(/ /g' | sed 's/)/ /g' | sed '2!d' | awk '{print $3,$5}'`
        rdlon=`$WGRIB2 ${infile} -grid -d 1 | sed 's/(/ /g' | sed 's/)/ /g' | sed '4!d' | awk '{print $2,$4,$6}'`
        rdlat=`$WGRIB2 ${infile} -grid -d 1 | sed 's/(/ /g' | sed 's/)/ /g' | sed '3!d' | awk '{print $2,$4,$6}'`
        echo ${nlola}       >> gwes_stats.inp
        echo ${rdlon}       >> gwes_stats.inp
        echo ${rdlat}       >> gwes_stats.inp
      fi

# 1.b.3 Create binary file for input to gwes_stats FORTRAN executable
      $WGRIB2 $infile -vt -match ${valtime} -bin data_${im}
      ok1=$?

# 1.b.4 Check for errors
      if [ $ok1 -ne 0 ] ; then
        echo " *** ERROR : para=$para, im=$im, ok1=$ok1"
        exit
      fi
      echo data_$im       >> gwes_stats.inp

      nme=`expr ${nme} + 1`
      
    done

#
# 1.c Execute gwes_stats and create grib2 files
#
    rm -f mean_out spread_out probab_out test_out
#
    $EXECwave/gwes_stats  < gwes_stats.inp >>$pgmout 2>&1
#
# 1.d Check for errors and move output files to tagged grib2 parameter-hour files
   if [ ! -f mean_out ]
   then
     msg="ABNORMAL EXIT: ERR mean_out not gerenerated for ${nnip} $hhh."
     postmsg "$jlogfile" "$msg"
     set +x
     echo "--- mean_out not gerenerated for ${nnip} $hhh --- "
     [[ "$LOUD" = YES ]] && set -x
     echo "mean_out not gerenerated for ${nnip} $hhh" >> $wave_log
     err=1;export err;err_chk
   else
     mv -f mean_out    ${nnip}_mean.$hhh.grib2
   fi
   if [ ! -f spread_out ]
   then
     msg="ABNORMAL EXIT: ERR spread_out not gerenerated for ${nnip} $hhh."
     postmsg "$jlogfile" "$msg"
     set +x
     echo "--- spread_out not gerenerated for ${nnip} $hhh --- "
     [[ "$LOUD" = YES ]] && set -x
     echo "spread_out not gerenerated for ${nnip} $hhh" >> $wave_log
     err=1;export err;err_chk
   else
     mv -f spread_out  ${nnip}_spread.$hhh.grib2
   fi

   nscale=`echo ${ascale[@]} | wc -w`     
   if [ ${nscale} -gt 1 ]
   then
     if [ ! -f probab_out ]
     then
       msg="ABNORMAL EXIT: ERR probab_out not gerenerated for ${nnip} $hhh."
       postmsg "$jlogfile" "$msg"
       set +x
       echo "--- probab_out not gerenerated for ${nnip} $hhh --- "
       [[ "$LOUD" = YES ]] && set -x
       echo "probab_out not gerenerated for ${nnip} $hhh" >> $wave_log
       err=1;export err;err_chk
     else
       mv -f probab_out  ${nnip}_probab.$hhh.grib2
     fi
   fi

#
# 2. Cleanup
# Remove binary data files
  rm -f data_??

# 
# End of wave_gwes_stats.sh

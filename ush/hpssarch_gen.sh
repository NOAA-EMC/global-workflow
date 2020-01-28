#!/bin/ksh
set -x

###################################################
# Fanglin Yang, 20180318
# --create bunches of files to be archived to HPSS
###################################################


type=${1:-gfs}                ##gfs, gdas, enkfgdas or enkfggfs

CDATE=${CDATE:-2018010100}
PDY=$(echo $CDATE | cut -c 1-8)
cyc=$(echo $CDATE | cut -c 9-10)
OUTPUT_FILE=${OUTPUT_FILE:-"netcdf"}
OUTPUT_HISTORY=${OUTPUT_HISTORY:-".true."}
SUFFIX=${SUFFIX:-".nc"}
if [ $SUFFIX = ".nc" ]; then
  format="netcdf"
else
  format="nemsio"
fi

#-----------------------------------------------------
if [ $type = "gfs" ]; then
#-----------------------------------------------------
  FHMIN_GFS=${FHMIN_GFS:-0}
  FHMAX_GFS=${FHMAX_GFS:-384}
  FHOUT_GFS=${FHOUT_GFS:-3}
  FHMAX_HF_GFS=${FHMAX_HF_GFS:-120}
  FHOUT_HF_GFS=${FHOUT_HF_GFS:-1}


  rm -f gfsa.txt
  rm -f gfsb.txt
  rm -f gfs_pgrb2b.txt
  rm -f gfs_flux.txt
  rm -f gfs_${format}a.txt
  rm -f gfs_${format}b.txt
  rm -f gfs_restarta.txt
  touch gfsa.txt
  touch gfsb.txt
  touch gfs_pgrb2b.txt
  touch gfs_flux.txt
  touch gfs_${format}a.txt
  touch gfs_${format}b.txt
  touch gfs_restarta.txt

  dirpath="gfs.${PDY}/${cyc}/"
  dirname="./${dirpath}"

  head="gfs.t${cyc}z."

  #..................
  echo  "${dirname}${head}pgrb2b.0p25.anl                  " >>gfs_pgrb2b.txt
  echo  "${dirname}${head}pgrb2b.0p25.anl.idx              " >>gfs_pgrb2b.txt
  echo  "${dirname}${head}pgrb2b.0p50.anl                  " >>gfs_pgrb2b.txt
  echo  "${dirname}${head}pgrb2b.0p50.anl.idx              " >>gfs_pgrb2b.txt

  echo  "${dirname}${head}gsistat                          " >>gfsa.txt
  echo  "${dirname}${head}nsstbufr                         " >>gfsa.txt
  echo  "${dirname}${head}prepbufr                         " >>gfsa.txt
  echo  "${dirname}${head}prepbufr_pre-qc                  " >>gfsa.txt
  echo  "${dirname}${head}prepbufr.acft_profiles           " >>gfsa.txt
  echo  "${dirname}${head}pgrb2.0p25.anl                   " >>gfsa.txt
  echo  "${dirname}${head}pgrb2.0p25.anl.idx               " >>gfsa.txt
  echo  "${dirname}avno.t${cyc}z.cyclone.trackatcfunix     " >>gfsa.txt
  echo  "${dirname}avnop.t${cyc}z.cyclone.trackatcfunix    " >>gfsa.txt
  echo  "${dirname}trak.gfso.atcfunix.${PDY}${cyc}         " >>gfsa.txt
  echo  "${dirname}trak.gfso.atcfunix.altg.${PDY}${cyc}    " >>gfsa.txt
  echo  "${dirname}storms.gfso.atcf_gen.${PDY}${cyc}       " >>gfsa.txt
  echo  "${dirname}storms.gfso.atcf_gen.altg.${PDY}${cyc}  " >>gfsa.txt
  echo  "${dirname}nawips/gfs_${PDY}${cyc}.sfc             " >>gfsa.txt
  echo  "${dirname}nawips/gfs_${PDY}${cyc}.snd             " >>gfsa.txt
  echo  "${dirname}bufr.t${cyc}z                           " >>gfsa.txt
  echo  "./logs/${CDATE}/gfs*.log                          " >>gfsa.txt

  echo  "${dirname}${head}pgrb2.0p50.anl                   " >>gfsb.txt
  echo  "${dirname}${head}pgrb2.0p50.anl.idx               " >>gfsb.txt
  echo  "${dirname}${head}pgrb2.1p00.anl                   " >>gfsb.txt
  echo  "${dirname}${head}pgrb2.1p00.anl.idx               " >>gfsb.txt


  fh=0
  while [ $fh -le $FHMAX_GFS ]; do
    fhr=$(printf %03i $fh)
    echo  "${dirname}${head}pgrb2b.0p25.f${fhr}             " >>gfs_pgrb2b.txt
    echo  "${dirname}${head}pgrb2b.0p25.f${fhr}.idx         " >>gfs_pgrb2b.txt
    echo  "${dirname}${head}pgrb2b.0p50.f${fhr}             " >>gfs_pgrb2b.txt
    echo  "${dirname}${head}pgrb2b.0p50.f${fhr}.idx         " >>gfs_pgrb2b.txt

    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2           " >>gfs_flux.txt
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2.idx       " >>gfs_flux.txt

    echo  "${dirname}${head}pgrb2.0p25.f${fhr}              " >>gfsa.txt
    echo  "${dirname}${head}pgrb2.0p25.f${fhr}.idx          " >>gfsa.txt
    echo  "${dirname}${head}logf${fhr}.txt                  " >>gfsa.txt

    echo  "${dirname}${head}pgrb2.0p50.f${fhr}              " >>gfsb.txt
    echo  "${dirname}${head}pgrb2.0p50.f${fhr}.idx          " >>gfsb.txt
    echo  "${dirname}${head}pgrb2.1p00.f${fhr}              " >>gfsb.txt
    echo  "${dirname}${head}pgrb2.1p00.f${fhr}.idx          " >>gfsb.txt

    inc=$FHOUT_GFS
    if [ $FHMAX_HF_GFS -gt 0 -a $FHOUT_HF_GFS -gt 0 -a $fh -lt $FHMAX_HF_GFS ]; then
     inc=$FHOUT_HF_GFS
    fi

    fh=$((fh+inc))
  done


  #..................
  echo  "${dirname}${head}atmanl${SUFFIX}            " >>gfs_${format}a.txt
  echo  "${dirname}${head}sfcanl${SUFFIX}            " >>gfs_${format}a.txt
  echo  "${dirname}${head}atmi*.nc                   " >>gfs_${format}a.txt
  echo  "${dirname}${head}dtfanl.nc                  " >>gfs_${format}a.txt
  echo  "${dirname}${head}loginc.txt                 " >>gfs_${format}a.txt

  #..................
  if [ $OUTPUT_HISTORY = ".true." ]; then
  fh=0
  while [ $fh -le 36 ]; do
    fhr=$(printf %03i $fh)
    echo  "${dirname}${head}atmf${fhr}${SUFFIX}        " >>gfs_${format}b.txt
    echo  "${dirname}${head}sfcf${fhr}${SUFFIX}        " >>gfs_${format}b.txt
    fh=$((fh+6))
  done
  fi

  #..................
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile1.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile2.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile3.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile4.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile5.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile6.nc  " >>gfs_restarta.txt

#-----------------------------------------------------
fi   ##end of gfs
#-----------------------------------------------------



#-----------------------------------------------------
if [ $type = "gdas" ]; then
#-----------------------------------------------------

  rm -f gdas.txt
  rm -f gdas_restarta.txt
  rm -f gdas_restartb.txt
  touch gdas.txt
  touch gdas_restarta.txt
  touch gdas_restartb.txt

  dirpath="gdas.${PDY}/${cyc}/"
  dirname="./${dirpath}"
  head="gdas.t${cyc}z."

  #..................
  echo  "${dirname}${head}gsistat                    " >>gdas.txt
  echo  "${dirname}${head}pgrb2.0p25.anl             " >>gdas.txt
  echo  "${dirname}${head}pgrb2.0p25.anl.idx         " >>gdas.txt
  echo  "${dirname}${head}pgrb2.1p00.anl             " >>gdas.txt
  echo  "${dirname}${head}pgrb2.1p00.anl.idx         " >>gdas.txt
  echo  "${dirname}${head}atmanl${SUFFIX}            " >>gdas.txt
  echo  "${dirname}${head}sfcanl${SUFFIX}            " >>gdas.txt
  if [ -s $ROTDIR/${dirpath}${head}cnvstat ]; then
     echo  "${dirname}${head}cnvstat                 " >>gdas.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}oznstat ]; then
     echo  "${dirname}${head}oznstat                 " >>gdas.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}radstat ]; then
     echo  "${dirname}${head}radstat                 " >>gdas.txt
  fi
  for fstep in prep anal fcst vrfy radmon minmon oznmon; do
   if [ -s $ROTDIR/logs/${CDATE}/gdas${fstep}.log ]; then
     echo  "./logs/${CDATE}/gdas${fstep}.log         " >>gdas.txt
   fi
  done
  echo  "./logs/${CDATE}/gdaspost*.log               " >>gdas.txt

  fh=0
  while [ $fh -le 9 ]; do
    fhr=$(printf %03i $fh)
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2      " >>gdas.txt
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2.idx  " >>gdas.txt
    echo  "${dirname}${head}pgrb2.0p25.f${fhr}         " >>gdas.txt
    echo  "${dirname}${head}pgrb2.0p25.f${fhr}.idx     " >>gdas.txt
    echo  "${dirname}${head}pgrb2.1p00.f${fhr}         " >>gdas.txt
    echo  "${dirname}${head}pgrb2.1p00.f${fhr}.idx     " >>gdas.txt
    echo  "${dirname}${head}logf${fhr}.txt             " >>gdas.txt
    echo  "${dirname}${head}atmf${fhr}${SUFFIX}        " >>gdas.txt
    echo  "${dirname}${head}sfcf${fhr}${SUFFIX}        " >>gdas.txt
    fh=$((fh+3))
  done
  flist="001 002 004 005 007 008"
  for fhr in $flist; do
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2      " >>gdas.txt
    echo  "${dirname}${head}sfluxgrbf${fhr}.grib2.idx  " >>gdas.txt
  done
  


  #..................
  if [ -s $ROTDIR/${dirpath}${head}cnvstat ]; then
     echo  "${dirname}${head}cnvstat               " >>gdas_restarta.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}radstat ]; then
     echo  "${dirname}${head}radstat               " >>gdas_restarta.txt
  fi
  echo  "${dirname}${head}nsstbufr                 " >>gdas_restarta.txt
  echo  "${dirname}${head}prepbufr                 " >>gdas_restarta.txt
  echo  "${dirname}${head}prepbufr_pre-qc          " >>gdas_restarta.txt
  echo  "${dirname}${head}prepbufr.acft_profiles   " >>gdas_restarta.txt
  echo  "${dirname}${head}abias                    " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_air                " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_int                " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_pc                 " >>gdas_restarta.txt
  echo  "${dirname}${head}atmi*nc                  " >>gdas_restarta.txt
  echo  "${dirname}${head}dtfanl.nc                " >>gdas_restarta.txt
  echo  "${dirname}${head}loginc.txt               " >>gdas_restarta.txt

  echo  "${dirname}RESTART/*0000.sfcanl_data.tile1.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile2.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile3.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile4.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile5.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/*0000.sfcanl_data.tile6.nc  " >>gdas_restarta.txt


  #..................
  echo  "${dirname}RESTART " >>gdas_restartb.txt

#-----------------------------------------------------
fi   ##end of gdas
#-----------------------------------------------------


#-----------------------------------------------------
if [ $type = "enkfgdas" -o $type = "enkfgfs" ]; then
#-----------------------------------------------------

  IAUFHRS_ENKF=${IAUFHRS_ENKF:-6}
  lobsdiag_forenkf=${lobsdiag_forenkf:-".false."}
  nfhrs=`echo $IAUFHRS_ENKF | sed 's/,/ /g'`
  NMEM_ENKF=${NMEM_ENKF:-80}
  NMEM_EARCGRP=${NMEM_EARCGRP:-10}               ##number of ens memebers included in each tarball
  NTARS=$((NMEM_ENKF/NMEM_EARCGRP))
  [[ $NTARS -eq 0 ]] && NTARS=1
  [[ $((NTARS*NMEM_EARCGRP)) -lt $NMEM_ENKF ]] && NTARS=$((NTARS+1))
  NTARS2=$((NTARS/2))

  dirpath="enkf${CDUMP}.${PDY}/${cyc}/"
  dirname="./${dirpath}"
  head="${CDUMP}.t${cyc}z."

  #..................
  rm -f enkf${CDUMP}.txt
  touch enkf${CDUMP}.txt

  echo  "${dirname}${head}enkfstat                   " >>enkf${CDUMP}.txt
  echo  "${dirname}${head}gsistat.ensmean            " >>enkf${CDUMP}.txt
  if [ -s $ROTDIR/${dirpath}${head}cnvstat.ensmean ]; then
       echo  "${dirname}${head}cnvstat.ensmean       " >>enkf${CDUMP}.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}oznstat.ensmean ]; then
       echo  "${dirname}${head}oznstat.ensmean       " >>enkf${CDUMP}.txt
  fi
  if [ -s $ROTDIR/${dirpath}${head}radstat.ensmean ]; then
       echo  "${dirname}${head}radstat.ensmean       " >>enkf${CDUMP}.txt
  fi
  for FHR in $nfhrs; do  # loop over analysis times in window
     if [ $FHR -eq 6 ]; then
        echo  "${dirname}${head}atmanl.ensmean${SUFFIX}      " >>enkf${CDUMP}.txt
     else
        echo  "${dirname}${head}atma00${FHR}.ensmean${SUFFIX}      " >>enkf${CDUMP}.txt
     fi 
  done # loop over FHR
  for fstep in eobs eomg ecen eupd efcs epos ; do
   echo  "logs/${CDATE}/${CDUMP}${fstep}*.log        " >>enkf${CDUMP}.txt
  done


# Ensemble spread file only available with netcdf output
  fh=3
  while [ $fh -le 9 ]; do
      fhr=$(printf %03i $fh)
      echo  "${dirname}${head}atmf${fhr}.ensmean${SUFFIX}       " >>enkf${CDUMP}.txt
      if [ $OUTPUT_FILE = "netcdf" ]; then
	  echo  "${dirname}${head}atmf${fhr}.ensspread${SUFFIX}     " >>enkf${CDUMP}.txt
      fi
      fh=$((fh+3))
  done

  #...........................
  n=1
  while [ $n -le $NTARS ]; do
  #...........................

  rm -f enkf${CDUMP}_grp${n}.txt
  rm -f enkf${CDUMP}_restarta_grp${n}.txt
  rm -f enkf${CDUMP}_restartb_grp${n}.txt
  touch enkf${CDUMP}_grp${n}.txt
  touch enkf${CDUMP}_restarta_grp${n}.txt
  touch enkf${CDUMP}_restartb_grp${n}.txt

  m=1
  while [ $m -le $NMEM_EARCGRP ]; do
    nm=$(((n-1)*NMEM_EARCGRP+m))
    mem=$(printf %03i $nm)
    dirpath="enkf${CDUMP}.${PDY}/${cyc}/mem${mem}/"
    dirname="./${dirpath}"
    head="${CDUMP}.t${cyc}z."

    #---
    for FHR in $nfhrs; do  # loop over analysis times in window
      if [ $FHR -eq 6 ]; then
         if [ $n -le $NTARS2 ]; then
            echo "${dirname}${head}ratmanl${SUFFIX}      " >>enkf${CDUMP}_grp${n}.txt
         fi
         echo "${dirname}${head}atminc.nc            " >>enkf${CDUMP}_restarta_grp${n}.txt
      else
         if [ $n -le $NTARS2 ]; then
            echo "${dirname}${head}ratma00${FHR}${SUFFIX}      " >>enkf${CDUMP}_grp${n}.txt
         fi
         echo "${dirname}${head}atmi00${FHR}.nc            " >>enkf${CDUMP}_restarta_grp${n}.txt
      fi 
      echo "${dirname}${head}atmf00${FHR}${SUFFIX}       " >>enkf${CDUMP}_grp${n}.txt
    done # loop over FHR

    if [[ lobsdiag_forenkf = ".false." ]] ; then
       echo "${dirname}${head}gsistat              " >>enkf${CDUMP}_grp${n}.txt
       if [ -s $ROTDIR/${dirpath}${head}cnvstat ] ; then
          echo "${dirname}${head}cnvstat           " >>enkf${CDUMP}_grp${n}.txt
       fi
       if [ -s $ROTDIR/${dirpath}${head}radstat ]; then
          echo "${dirname}${head}radstat           " >>enkf${CDUMP}_restarta_grp${n}.txt
       fi
       if [ -s $ROTDIR/${dirpath}${head}cnvstat ]; then
          echo "${dirname}${head}cnvstat           " >>enkf${CDUMP}_restarta_grp${n}.txt
       fi
       echo "${dirname}${head}abias                " >>enkf${CDUMP}_restarta_grp${n}.txt
       echo "${dirname}${head}abias_air            " >>enkf${CDUMP}_restarta_grp${n}.txt
       echo "${dirname}${head}abias_int            " >>enkf${CDUMP}_restarta_grp${n}.txt
       echo "${dirname}${head}abias_pc             " >>enkf${CDUMP}_restarta_grp${n}.txt
    fi
    #---
    echo "${dirname}RESTART/*0000.sfcanl_data.tile1.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile2.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile3.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile4.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile5.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/*0000.sfcanl_data.tile6.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt

    #---
    echo "${dirname}RESTART                     " >>enkf${CDUMP}_restartb_grp${n}.txt

    m=$((m+1))
  done


  #...........................
  n=$((n+1))
  done
  #...........................


#-----------------------------------------------------
fi   ##end of enkfgdas or enkfgfs
#-----------------------------------------------------


exit 0


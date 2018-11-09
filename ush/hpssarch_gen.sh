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
  rm -f gfs_nemsioa.txt
  rm -f gfs_nemsiob.txt
  rm -f gfs_restarta.txt
  touch gfsa.txt
  touch gfsb.txt
  touch gfs_pgrb2b.txt
  touch gfs_flux.txt
  touch gfs_nemsioa.txt
  touch gfs_nemsiob.txt
  touch gfs_restarta.txt

  dirname="./gfs.${PDY}/${cyc}/"
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
  echo  "${dirname}avn.t${cyc}z.cyclone.trackatcfunix      " >>gfsa.txt
  echo  "${dirname}gfso.t${cyc}z.cyclone.trackatcfunix     " >>gfsa.txt
  echo  "${dirname}storms.gfso.atcf_gen.${PDY}${cyc}       " >>gfsa.txt
  echo  "${dirname}storms.gfso.atcf_gen.altg.${PDY}${cyc}  " >>gfsa.txt
  echo  "${dirname}trak.gfso.atcf_gen.${PDY}${cyc}         " >>gfsa.txt
  echo  "${dirname}trak.gfso.atcf_gen.altg.${PDY}${cyc}    " >>gfsa.txt
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
    echo  "${dirname}${head}logf${fhr}.nemsio               " >>gfsa.txt

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
  echo  "${dirname}${head}atmanl.nemsio              " >>gfs_nemsioa.txt
  echo  "${dirname}${head}sfcanl.nemsio              " >>gfs_nemsioa.txt
  echo  "${dirname}${head}atmf000.nemsio             " >>gfs_nemsioa.txt
  echo  "${dirname}${head}sfcf000.nemsio             " >>gfs_nemsioa.txt
  echo  "${dirname}${head}atminc.nc                  " >>gfs_nemsioa.txt
  echo  "${dirname}${head}dtfanl.nc                  " >>gfs_nemsioa.txt

  #..................
  fh=6
  while [ $fh -le 36 ]; do
    fhr=$(printf %03i $fh)
    echo  "${dirname}${head}atmf${fhr}.nemsio             " >>gfs_nemsiob.txt
    echo  "${dirname}${head}sfcf${fhr}.nemsio             " >>gfs_nemsiob.txt
    fh=$((fh+6))
  done

  #..................
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile1.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile2.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile3.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile4.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile5.nc  " >>gfs_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile6.nc  " >>gfs_restarta.txt

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

  dirname="./gdas.${PDY}/${cyc}/"
  head="gdas.t${cyc}z."

  #..................
  echo  "${dirname}${head}cnvstat                    " >>gdas.txt
  echo  "${dirname}${head}gsistat                    " >>gdas.txt
  echo  "${dirname}${head}oznstat                    " >>gdas.txt
  echo  "${dirname}${head}radstat                    " >>gdas.txt
  echo  "${dirname}${head}pgrb2.0p25.anl             " >>gdas.txt
  echo  "${dirname}${head}pgrb2.0p25.anl.idx         " >>gdas.txt
  echo  "${dirname}${head}pgrb2.1p00.anl             " >>gdas.txt
  echo  "${dirname}${head}pgrb2.1p00.anl.idx         " >>gdas.txt
  echo  "${dirname}${head}atmanl.nemsio              " >>gdas.txt
  echo  "${dirname}${head}sfcanl.nemsio              " >>gdas.txt
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
    echo  "${dirname}${head}logf${fhr}.nemsio          " >>gdas.txt
    echo  "${dirname}${head}atmf${fhr}.nemsio          " >>gdas.txt
    echo  "${dirname}${head}sfcf${fhr}.nemsio          " >>gdas.txt
    fh=$((fh+3))
  done


  #..................
  echo  "${dirname}${head}cnvstat                  " >>gdas_restarta.txt
  echo  "${dirname}${head}radstat                  " >>gdas_restarta.txt
  echo  "${dirname}${head}nsstbufr                 " >>gdas_restarta.txt
  echo  "${dirname}${head}prepbufr                 " >>gdas_restarta.txt
  echo  "${dirname}${head}prepbufr_pre-qc          " >>gdas_restarta.txt
  echo  "${dirname}${head}prepbufr.acft_profiles   " >>gdas_restarta.txt
  echo  "${dirname}${head}abias                    " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_air                " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_int                " >>gdas_restarta.txt
  echo  "${dirname}${head}abias_pc                 " >>gdas_restarta.txt
  echo  "${dirname}${head}atminc.nc                " >>gdas_restarta.txt
  echo  "${dirname}${head}dtfanl.nc                " >>gdas_restarta.txt

  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile1.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile2.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile3.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile4.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile5.nc  " >>gdas_restarta.txt
  echo  "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile6.nc  " >>gdas_restarta.txt

  #..................
  echo  "${dirname}RESTART " >>gdas_restartb.txt

#-----------------------------------------------------
fi   ##end of gdas
#-----------------------------------------------------


#-----------------------------------------------------
if [ $type = "enkfgdas" -o $type = "enkfgfs" ]; then
#-----------------------------------------------------

  NMEM_ENKF=${NMEM_ENKF:-80}
  NMEM_EARCGRP=${NMEM_EARCGRP:-10}               ##number of ens memebers included in each tarball
  NTARS=$((NMEM_ENKF/NMEM_EARCGRP))
  [[ $NTARS -eq 0 ]] && NTARS=1
  [[ $((NTARS*NMEM_EARCGRP)) -lt $NMEM_ENKF ]] && NTARS=$((NTARS+1))
  NTARS2=$((NTARS/2))

  dirname="./enkf${CDUMP}.${PDY}/${cyc}/"
  head="${CDUMP}.t${cyc}z."

  #..................
  rm -f enkf${CDUMP}.txt
  touch enkf${CDUMP}.txt

  echo  "${dirname}${head}cnvstat.ensmean            " >>enkf${CDUMP}.txt
  echo  "${dirname}${head}enkfstat                   " >>enkf${CDUMP}.txt
  echo  "${dirname}${head}gsistat.ensmean            " >>enkf${CDUMP}.txt
  echo  "${dirname}${head}oznstat.ensmean            " >>enkf${CDUMP}.txt
  echo  "${dirname}${head}radstat.ensmean            " >>enkf${CDUMP}.txt
  echo  "${dirname}${head}atmanl.ensmean.nemsio      " >>enkf${CDUMP}.txt
  for fstep in eobs eomg ecen eupd efcs epos ; do
   echo  "logs/${CDATE}/${CDUMP}${fstep}*.log        " >>enkf${CDUMP}.txt
  done

  fh=3
  while [ $fh -le 9 ]; do
    fhr=$(printf %03i $fh)
    echo  "${dirname}${head}atmf${fhr}.ensmean.nc4      " >>enkf${CDUMP}.txt
    echo  "${dirname}${head}atmf${fhr}.ensspread.nc4    " >>enkf${CDUMP}.txt
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
     dirname="./enkf${CDUMP}.${PDY}/${cyc}/mem${mem}/"
     head="${CDUMP}.t${cyc}z."

    #---
    if [ $n -le $NTARS2 ]; then
     echo "${dirname}${head}ratmanl.nemsio      " >>enkf${CDUMP}_grp${n}.txt
    fi
    echo "${dirname}${head}atmf006.nemsio       " >>enkf${CDUMP}_grp${n}.txt
    echo "${dirname}${head}cnvstat              " >>enkf${CDUMP}_grp${n}.txt
    echo "${dirname}${head}gsistat              " >>enkf${CDUMP}_grp${n}.txt


    #---
    echo "${dirname}${head}atminc.nc            " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}${head}abias                " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}${head}abias_air            " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}${head}abias_int            " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}${head}abias_pc             " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}${head}radstat              " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}${head}cnvstat              " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile1.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile2.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile3.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile4.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile5.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt
    echo "${dirname}RESTART/${PDY}.${cyc}0000.sfcanl_data.tile6.nc  " >>enkf${CDUMP}_restarta_grp${n}.txt

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


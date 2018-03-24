#!/bin/ksh
#set -x

###################################################
# Fanglin Yang, 20180318  
# --create bunches of files to be archived to HPSS
###################################################


type=${1:-gfs}                ##gfs, gdas, enkf.gdas or enkf.ggfs

CDATE=${CDATE:-2018010100}
ymd=`echo $CDATE |cut -c 1-8`
cyc=`echo $CDATE |cut -c 9-10`

#-----------------------------------------------------
if [ $type = "gfs" ]; then
#-----------------------------------------------------
FHMIN_GFS=${FHMIN_GFS:-0}
FHMAX_GFS=${FHMAX_GFS:-384}
FHOUT_GFS=${FHOUT_GFS:-3}
FHMAX_HF_GFS=${FHMAX_HF_GFS:-120}
FHOUT_HF_GFS=${FHOUT_HF_GFS:-1}


rm -f gfs.txt
rm -f gfs_pgrb2b.txt
rm -f gfs_flux.txt
rm -f gfs_nemsioa.txt
rm -f gfs_nemsiob.txt
>gfs.txt
>gfs_pgrb2b.txt
>gfs_flux.txt
>gfs_nemsioa.txt
>gfs_nemsiob.txt

dirname="./gfs.${ymd}/${cyc}/"
head="gfs.t${cyc}z."

#..................
echo  "${dirname}${head}pgrb2b.0p25.anl                  " >>gfs_pgrb2b.txt
echo  "${dirname}${head}pgrb2b.0p25.anl.idx              " >>gfs_pgrb2b.txt
echo  "${dirname}${head}nsstbufr                         " >>gfs.txt
echo  "${dirname}${head}prepbufr                         " >>gfs.txt
echo  "${dirname}${head}prepbufr_pre-qc                  " >>gfs.txt
echo  "${dirname}${head}prepbufr.acft_profiles           " >>gfs.txt
echo  "${dirname}${head}pgrb2.0p25.anl                   " >>gfs.txt
echo  "${dirname}${head}pgrb2.0p25.anl.idx               " >>gfs.txt
echo  "${dirname}${head}pgrb2.1p00.anl                   " >>gfs.txt
echo  "${dirname}${head}pgrb2.1p00.anl.idx               " >>gfs.txt
echo  "${dirname}avn.t${cyc}z.cyclone.trackatcfunix      " >>gfs.txt
echo  "${dirname}gfso.t${cyc}z.cyclone.trackatcfunix     " >>gfs.txt
echo  "${dirname}storms.gfso.atcf_gen.${ymd}${cyc}       " >>gfs.txt
echo  "${dirname}storms.gfso.atcf_gen.altg.${ymd}${cyc}  " >>gfs.txt
echo  "${dirname}trak.gfso.atcf_gen.${ymd}${cyc}         " >>gfs.txt
echo  "${dirname}trak.gfso.atcf_gen.altg.${ymd}${cyc}    " >>gfs.txt
echo  "${dirname}nawips/gfs_${ymd}${cyc}.sfc             " >>gfs.txt
echo  "${dirname}nawips/gfs_${ymd}${cyc}.snd             " >>gfs.txt
echo  "${dirname}bufr.t${cyc}z                           " >>gfs.txt

fh=0
while [ $fh -le $FHMAX_GFS ]; do
  fhr=$(printf %03i $fh)
  echo  "${dirname}${head}pgrb2b.0p25.f${fhr}             " >>gfs_pgrb2b.txt
  echo  "${dirname}${head}pgrb2b.0p25.f${fhr}.idx         " >>gfs_pgrb2b.txt

  echo  "${dirname}${head}sfluxgrbf${fhr}.grib2           " >>gfs_flux.txt
  echo  "${dirname}${head}sfluxgrbf${fhr}.grib2.idx       " >>gfs_flux.txt

  echo  "${dirname}${head}pgrb2.0p25.f${fhr}              " >>gfs.txt
  echo  "${dirname}${head}pgrb2.0p25.f${fhr}.idx          " >>gfs.txt
  echo  "${dirname}${head}pgrb2.1p00.f${fhr}              " >>gfs.txt
  echo  "${dirname}${head}pgrb2.1p00.f${fhr}.idx          " >>gfs.txt
  echo  "${dirname}${head}logf${fhr}.nemsio               " >>gfs.txt

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

#-----------------------------------------------------
fi   ##end of gfs
#-----------------------------------------------------



#-----------------------------------------------------
if [ $type = "gdas" ]; then
#-----------------------------------------------------

rm -f gdas.txt
rm -f gdas_restarta.txt
rm -f gdas_restartb.txt
>gdas.txt
>gdas_restarta.txt
>gdas_restartb.txt

dirname="./gdas.${ymd}/${cyc}/"
head="gdas.t${cyc}z."

#..................
echo  "${dirname}${head}pgrb2.0p25.anl             " >>gdas.txt
echo  "${dirname}${head}pgrb2.0p25.anl.idx         " >>gdas.txt
echo  "${dirname}${head}pgrb2.1p00.anl             " >>gdas.txt
echo  "${dirname}${head}pgrb2.1p00.anl.idx         " >>gdas.txt
echo  "${dirname}${head}atmanl.nemsio              " >>gdas.txt
echo  "${dirname}${head}sfcanl.nemsio              " >>gdas.txt
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
echo  "${dirname}${head}atmanl.ensres.nemsio     " >>gdas_restarta.txt
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

echo  "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile1.nc  " >>gdas_restarta.txt
echo  "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile2.nc  " >>gdas_restarta.txt
echo  "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile3.nc  " >>gdas_restarta.txt
echo  "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile4.nc  " >>gdas_restarta.txt
echo  "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile5.nc  " >>gdas_restarta.txt
echo  "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile6.nc  " >>gdas_restarta.txt

#..................
echo  "${dirname}RESTART " >>gdas_restartb.txt

#-----------------------------------------------------
fi   ##end of gdas
#-----------------------------------------------------


#-----------------------------------------------------
if [ $type = "enkf.gdas" -o  $type = "enkf.gfs" ]; then
#-----------------------------------------------------
[[ $type = "enkf.gdas" ]] && cdump="gdas"
[[ $type = "enkf.gfs" ]] && cdump="gfs"

NMEM_ENKF=${NMEM_ENKF:-80}
NMEM_EARCGRP=${NMEM_EARCGRP:-10}               ##number of ens memebers included in each tarball
NTARS=$((NMEM_ENKF/NMEM_EARCGRP))
[[ $NTARS -eq 0 ]] && NTARS=1
[[ $((NTARS*NMEM_EARCGRP)) -lt $NMEM_ENKF ]] && NTARS=$((NTARS+1))

dirname="./enkf.${cdump}.${ymd}/${cyc}/"
head="${cdump}.t${cyc}z."

#..................
rm -f enkf.${cdump}.txt
>enkf.${cdump}.txt

echo  "${dirname}${head}cnvstat.ensmean            " >>enkf.${cdump}.txt
echo  "${dirname}${head}enkfstat                   " >>enkf.${cdump}.txt
echo  "${dirname}${head}gsistat.ensmean            " >>enkf.${cdump}.txt
echo  "${dirname}${head}oznstat.ensmean            " >>enkf.${cdump}.txt
echo  "${dirname}${head}radstat.ensmean            " >>enkf.${cdump}.txt
echo  "${dirname}${head}atmanl.ensmean.nemsio      " >>enkf.${cdump}.txt
fh=3
while [ $fh -le 9 ]; do
  fhr=$(printf %03i $fh)
  echo  "${dirname}${head}atmf${fhr}.ensmean.nc4      " >>enkf.${cdump}.txt
  echo  "${dirname}${head}atmf${fhr}.ensspread.nc4    " >>enkf.${cdump}.txt
  fh=$((fh+3))
done

#...........................
n=1
while [ $n -le $NTARS ]; do
#...........................

rm -f enkf.${cdump}_grp${n}.txt
rm -f enkf.${cdump}_restarta_grp${n}.txt
rm -f enkf.${cdump}_restartb_grp${n}.txt
>enkf.${cdump}_grp${n}.txt
>enkf.${cdump}_restarta_grp${n}.txt
>enkf.${cdump}_restartb_grp${n}.txt

m=1
while [ $m -le $NMEM_EARCGRP ]; do
   nm=$(((n-1)*NMEM_EARCGRP+m))
   mem=$(printf %03i $nm)
   dirname="./enkf.${cdump}.${ymd}/${cyc}/mem${mem}/"
   head="${cdump}.t${cyc}z."

  #---
  echo "${dirname}${head}ratmanl.nemsio       " >>enkf.${cdump}_grp${n}.txt
  echo "${dirname}${head}atmf006.nemsio       " >>enkf.${cdump}_grp${n}.txt
  echo "${dirname}${head}cnvstat              " >>enkf.${cdump}_grp${n}.txt
  echo "${dirname}${head}gsistat              " >>enkf.${cdump}_grp${n}.txt


  #---
  echo "${dirname}${head}atminc.nc            " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}${head}abias                " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}${head}abias_air            " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}${head}abias_int            " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}${head}abias_pc             " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}${head}radstat              " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}${head}cnvstat              " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile1.nc  " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile2.nc  " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile3.nc  " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile4.nc  " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile5.nc  " >>enkf.${cdump}_restarta_grp${n}.txt
  echo "${dirname}RESTART/${ymd}.${cyc}0000.sfcanl_data.tile6.nc  " >>enkf.${cdump}_restarta_grp${n}.txt

  #---
  echo "${dirname}RESTART                     " >>enkf.${cdump}_restartb_grp${n}.txt

  m=$((m+1))
done


#...........................
n=$((n+1))
done
#...........................


#-----------------------------------------------------
fi   ##end of enkf.gdas or enkf.gfs
#-----------------------------------------------------


exit


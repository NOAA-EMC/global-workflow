#!/bin/sh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         global_getdump.sh
# Script description:  Copies a global dump
#
# Author:        Mark Iredell       Org: NP23         Date: 1999-07-15
#
# Abstract: This script copies a global dump.
#
# Script history log:
# 1999-05-01  Mark Iredell
# 2002-03-08  Mark Iredell  generalize suffixes
# 2005-01-24  Mark Iredell  transition to blue
# 2014-04-14  Kate Howard   reworked to handle files with period in name
#
# Usage:  global_getdump.sh CDATE CDUMP DFILES
#
#   Input script positional parameters:
#     1             Current date in YYYYMMDDHH form
#                   defaults to $CDATE; required
#     2             Current dump (gfs or fnl)
#                   defaults to $CDUMP, then to fnl
#     3             Dump file list
#                   defaults to $DFILES, then to all possible
#
#   Imported Shell Variables:
#     COMOUT        output directory
#                   (if nonexistent will be made)
#                   defaults to current working directory
#     PREOUT        Prefix to add to output filenames
#                   defaults to none
#     SUFOUT        Suffix to add to output filenames
#                   defaults to .$CDUMP.$CDATE
#     NCP           Copy command
#                   defaults to cp
#     VERBOSE       Verbose flag (YES or NO)
#                   defaults to NO
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
#
####

################################################################################
#  Set environment.

export CDATE=${1:-${CDATE:?}}
export CDUMP=${2:-${CDUMP:-gdas}}
nshift=$#
[[ $nshift -le 2 ]]||nshift=2
shift $nshift
export DFILES="${*:-${DFILES}}"
if [[ -z "$DFILES" ]]
then
   DFILES="$DFILES icegrb snogrb snogrb_t382 snogrb_t574 sstgrb tcvitl"
   DFILES="$DFILES stat01 adpupa proflr aircar aircft satwnd adpsfc sfcshp"
   DFILES="$DFILES sfcbog vadwnd goesnd spssmi sptrmm erscat qkswnd"
   DFILES="$DFILES osbuvb osbuv8 geoimr airs airswm ssmit"
   DFILES="$DFILES 1bmhs 1bmsu 1bhrs2 1bhrs3 1bhrs4 1bamua 1bamub"
   DFILES="$DFILES airsev goesfv gpsro gpsipw wdsatr wndsat rassda"
   DFILES="$DFILES avcsam avcspm gome mtiasi ascatt ascatw"
   DFILES="$DFILES esamua esamub eshrs3 esmhs omi amsre ssmisu"
   DFILES="$DFILES statup wdsatr wndsat"
   DFILES="$DFILES sevcsr atms mls bathy tesac trkob cris"
   DFILES="$DFILES rtg1 rtg2 seaice1 seaice2 NPRN NPRS ims2"
fi

export COMGFSTMP=${COMGFSTMP:-/com/gfs/prod/gfs.\$day/gfs.\$cyc}
export COMFNLTMP=${COMFNLTMP:-/com/gfs/prod/gdas.\$day/gdas1.\$cyc}
export HOST1=${HOST1:-$(hostname|cut -c1)}
export COMOUT=${COMOUT:-$(pwd)}
export PREOUT=${PREOUT:-""}
export SUFOUT=${SUFOUT:-".$CDUMP.$CDATE"}
export NCP=${NCP:-cp}
export VERBOSE=${VERBOSE:-"NO"}
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

################################################################################
#  Define old filenames

day=$(echo $CDATE|cut -c1-8)
cyc=t$(echo $CDATE|cut -c9-10)z
if [[ "$CDUMP" = gfs || "$CDUMP" = avn ]]
then
   eval com=$COMGFSTMP
else
   eval com=$COMFNLTMP
fi

# Input names

I_icegrb=$com.engicegrb
I_snogrb=$com.snogrb
I_snogrb_t382=/dcom/us007003/$(echo $CDATE | cut -c 1-8)/wgrbbul/snowdepth.t382.grb
I_snogrb_t574=$com.snogrb_t574
I_sstgrb=$com.sstgrb
I_tcvitl=$com.syndata.tcvitals.tm00
I_stat01=$com.status.tm00.bufr_d
I_adpupa=$com.adpupa.tm00.bufr_d
I_proflr=$com.proflr.tm00.bufr_d
I_aircar=$com.aircar.tm00.bufr_d
I_aircft=$com.aircft.tm00.bufr_d
I_satwnd=$com.satwnd.tm00.bufr_d
I_adpsfc=$com.adpsfc.tm00.bufr_d
I_sfcshp=$com.sfcshp.tm00.bufr_d
I_sfcbog=$com.sfcbog.tm00.bufr_d
I_vadwnd=$com.vadwnd.tm00.bufr_d
I_goesnd=$com.goesnd.tm00.bufr_d
I_spssmi=$com.spssmi.tm00.bufr_d
I_sptrmm=$com.sptrmm.tm00.bufr_d
I_erscat=$com.erscat.tm00.bufr_d
I_qkswnd=$com.qkswnd.tm00.bufr_d
I_stat02=$com.status.tm00.ieee_d
I_h1bn11=$com.h1bn11.tm00.ieee_d
I_m1bn11=$com.m1bn11.tm00.ieee_d
I_sbvn11=$com.sbvn11.tm00.ieee_d
I_m1bn12=$com.m1bn12.tm00.ieee_d
I_h1bn14=$com.h1bn14.tm00.ieee_d
I_m1bn14=$com.m1bn14.tm00.ieee_d
I_sbvn14=$com.sbvn14.tm00.ieee_d
I_h1bn15=$com.h1bn15.tm00.ieee_d
I_a1bn15=$com.a1bn15.tm00.ieee_d
I_b1bn15=$com.b1bn15.tm00.ieee_d
I_h1bn16=$com.h1bn16.tm00.ieee_d
I_a1bn16=$com.a1bn16.tm00.ieee_d
I_b1bn16=$com.b1bn16.tm00.ieee_d
I_sbvn16=$com.sbvn16.tm00.ieee_d
I_osbuvb=$com.osbuv.tm00.bufr_d
I_osbuv8=$com.osbuv8.tm00.bufr_d
I_geoimr=$com.geoimr.tm00.bufr_d
I_1bmsu=$com.1bmsu.tm00.bufr_d
I_1bhrs2=$com.1bhrs2.tm00.bufr_d
I_1bhrs3=$com.1bhrs3.tm00.bufr_d
I_1bamua=$com.1bamua.tm00.bufr_d
I_1bamub=$com.1bamub.tm00.bufr_d
I_airs=$com.airs.tm00.bufr_d
I_airswm=$com.airswm.tm00.bufr_d
I_ssmit=$com.ssmit.tm00.bufr_d
I_1bhrs4=$com.1bhrs4.tm00.bufr_d
I_1bmhs=$com.1bmhs.tm00.bufr_d
I_airsev=$com.airsev.tm00.bufr_d
I_goesfv=$com.goesfv.tm00.bufr_d
I_gpsro=$com.gpsro.tm00.bufr_d
I_gpsipw=$com.gpsipw.tm00.bufr_d
I_wdsatr=$com.wdsatr.tm00.bufr_d
I_wndsat=$com.wndsat.tm00.bufr_d
I_rassda=$com.rassda.tm00.bufr_d
I_avcsam=$com.avcsam.tm00.bufr_d
I_avcspm=$com.avcspm.tm00.bufr_d
I_gome=$com.gome.tm00.bufr_d
I_mtiasi=$com.mtiasi.tm00.bufr_d
I_ascatt=$com.ascatt.tm00.bufr_d
I_ascatw=$com.ascatw.tm00.bufr_d
I_esamua=$com.esamua.tm00.bufr_d
I_esamub=$com.esamub.tm00.bufr_d
I_eshrs3=$com.eshrs3.tm00.bufr_d
I_esmhs=$com.esmhs.tm00.bufr_d
I_omi=$com.omi.tm00.bufr_d
I_amsre=$com.amsre.tm00.bufr_d
I_ssmisu=$com.ssmisu.tm00.bufr_d
I_statup=$com.updated.status.tm00.bufr_d
I_sevcsr=$com.sevcsr.tm00.bufr_d
I_atms=$com.atms.tm00.bufr_d
I_mls=$com.mls.tm00.bufr_d
I_bathy=$com.bathy.tm00.bufr_d
I_tesac=$com.tesac.tm00.bufr_d
I_trkob=$com.trkob.tm00.bufr_d
I_cris=$com.cris.tm00.bufr_d
I_rtg1=$com.rtgssthr.grb
I_rtg2=$com.rtgssthr.grib2
I_seaice1=$com.seaice.5min.grb
I_seaice2=$com.seaice.5min.grib2
I_NPRN=$com.NPR.SNWN.SP.S1200.MESH16.grb
I_NPRS=$com.NPR.SNWS.SP.S1200.MESH16.grb
I_ims2=$com.imssnow96.grib2

I_alertf=$com.dump_alert_flag

# Output names

eval preout=$PREOUT
eval sufout=$SUFOUT

O_icegrb=$COMOUT/${preout}icegrb${sufout}
O_snogrb=$COMOUT/${preout}snogrb${sufout}
O_snogrb_t382=$COMOUT/${preout}snogrb_t382${sufout}
O_snogrb_t574=$COMOUT/${preout}snogrb_t574${sufout}
O_sstgrb=$COMOUT/${preout}sstgrb${sufout}
O_tcvitl=$COMOUT/${preout}tcvitl${sufout}
O_stat01=$COMOUT/${preout}stat01${sufout}
O_adpupa=$COMOUT/${preout}adpupa${sufout}
O_proflr=$COMOUT/${preout}proflr${sufout}
O_aircar=$COMOUT/${preout}aircar${sufout}
O_aircft=$COMOUT/${preout}aircft${sufout}
O_satwnd=$COMOUT/${preout}satwnd${sufout}
O_adpsfc=$COMOUT/${preout}adpsfc${sufout}
O_sfcshp=$COMOUT/${preout}sfcshp${sufout}
O_sfcbog=$COMOUT/${preout}sfcbog${sufout}
O_vadwnd=$COMOUT/${preout}vadwnd${sufout}
O_goesnd=$COMOUT/${preout}goesnd${sufout}
O_spssmi=$COMOUT/${preout}spssmi${sufout}
O_sptrmm=$COMOUT/${preout}sptrmm${sufout}
O_erscat=$COMOUT/${preout}erscat${sufout}
O_qkswnd=$COMOUT/${preout}qkswnd${sufout}
O_stat02=$COMOUT/${preout}stat02${sufout}
O_h1bn11=$COMOUT/${preout}h1bn11${sufout}
O_m1bn11=$COMOUT/${preout}m1bn11${sufout}
O_sbvn11=$COMOUT/${preout}sbvn11${sufout}
O_m1bn12=$COMOUT/${preout}m1bn12${sufout}
O_h1bn14=$COMOUT/${preout}h1bn14${sufout}
O_m1bn14=$COMOUT/${preout}m1bn14${sufout}
O_sbvn14=$COMOUT/${preout}sbvn14${sufout}
O_h1bn15=$COMOUT/${preout}h1bn15${sufout}
O_a1bn15=$COMOUT/${preout}a1bn15${sufout}
O_b1bn15=$COMOUT/${preout}b1bn15${sufout}
O_h1bn16=$COMOUT/${preout}h1bn16${sufout}
O_a1bn16=$COMOUT/${preout}a1bn16${sufout}
O_b1bn16=$COMOUT/${preout}b1bn16${sufout}
O_sbvn16=$COMOUT/${preout}sbvn16${sufout}
O_osbuvb=$COMOUT/${preout}osbuvb${sufout}
O_osbuv8=$COMOUT/${preout}osbuv8${sufout}
O_geoimr=$COMOUT/${preout}geoimr${sufout}
O_1bmsu=$COMOUT/${preout}1bmsu${sufout}
O_1bhrs2=$COMOUT/${preout}1bhrs2${sufout}
O_1bhrs3=$COMOUT/${preout}1bhrs3${sufout}
O_1bamua=$COMOUT/${preout}1bamua${sufout}
O_1bamub=$COMOUT/${preout}1bamub${sufout}
O_airs=$COMOUT/${preout}airs${sufout}
O_airswm=$COMOUT/${preout}airswm${sufout}
O_ssmit=$COMOUT/${preout}ssmit${sufout}
O_1bhrs4=$COMOUT/${preout}1bhrs4${sufout}
O_1bmhs=$COMOUT/${preout}1bmhs${sufout}
O_airsev=$COMOUT/${preout}airsev${sufout}
O_goesfv=$COMOUT/${preout}goesfv${sufout}
O_gpsro=$COMOUT/${preout}gpsro${sufout}
O_gpsipw=$COMOUT/${preout}gpsipw${sufout}
O_wdsatr=$COMOUT/${preout}wdsatr${sufout}
O_wndsat=$COMOUT/${preout}wndsat${sufout}
O_rassda=$COMOUT/${preout}rassda${sufout}
O_avcsam=$COMOUT/${preout}avcsam${sufout}
O_avcspm=$COMOUT/${preout}avcspm${sufout}
O_gome=$COMOUT/${preout}gome${sufout}
O_mtiasi=$COMOUT/${preout}mtiasi${sufout}
O_ascatt=$COMOUT/${preout}ascatt${sufout}
O_ascatw=$COMOUT/${preout}ascatw${sufout}
O_esamua=$COMOUT/${preout}esamua${sufout}
O_esamub=$COMOUT/${preout}esamub${sufout}
O_eshrs3=$COMOUT/${preout}eshrs3${sufout}
O_esmhs=$COMOUT/${preout}esmhs${sufout}
O_omi=$COMOUT/${preout}omi${sufout}
O_amsre=$COMOUT/${preout}amsre${sufout}
O_ssmisu=$COMOUT/${preout}ssmisu${sufout}
O_statup=$COMOUT/${preout}statup${sufout}
O_sevcsr=$COMOUT/${preout}sevcsr${sufout}
O_atms=$COMOUT/${preout}atms${sufout}
O_mls=$COMOUT/${preout}mls${sufout}
O_bathy=$COMOUT/${preout}bathy${sufout}
O_tesac=$COMOUT/${preout}tesac${sufout}
O_trkob=$COMOUT/${preout}trkob${sufout}
O_cris=$COMOUT/${preout}cris${sufout}
O_rtg1=$COMOUT/${preout}rtgssthr.grb${sufout}
O_rtg2=$COMOUT/${preout}rtgssthr.grib2${sufout}
O_seaice1=$COMOUT/${preout}seaice.5min.grb${sufout}
O_seaice2=$COMOUT/${preout}seaice.5min.grib2${sufout}
O_NPRN=$COMOUT/${preout}NPR.SNWN.SP.S1200.MESH16.grb${sufout}
O_NPRS=$COMOUT/${preout}NPR.SNWS.SP.S1200.MESH16.grb${sufout}
O_ims2=$COMOUT/${preout}imssnow96.grib2${sufout}

O_alertf=$COMOUT/${preout}alertf${sufout}

################################################################################
#  Copy to new filenames

cpy=""
cpn=""
for ft in $DFILES
do
   eval fto=\$I_$ft
   eval ftn=\$O_$ft
   $NCP -p $fto $ftn
   if [[ $? -eq 0 ]]
   then
      chmod 644 $ftn
      cpy="$cpy $ft"
   else
      cpn="$cpn $ft"
   fi
done
echo File types     copied: $cpy
echo File types NOT copied: $cpn

export err=0

################################################################################
#  Postprocessing
set +x
if [[ "$VERBOSE" = "YES" ]]
then
   echo $(date) EXITING $0 with return code $err >&2
fi
exit $err

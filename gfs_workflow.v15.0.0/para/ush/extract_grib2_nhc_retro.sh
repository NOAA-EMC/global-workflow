#!/bin/ksh
set -x
#
# extract a subset of grib2 data for National Hurricane Center
#

if [ $# -lt 10 ]; then
  set +x
  echo " Usage:  $0 COMIN NHCOUTPUT NHCSUBSET_LIST NHCSUBSET_PGB NHCSUBSET_FHOUT NHCSUBSET_FHMAX PGB_FORMAT CDATE CDUMP HRKSIG"
  echo " Eg: /ptmpd1/emc.glopara /global/noscrub/emc.glopara/nhc_subset "
  echo "     /global/save/emc.glopara/misc/subset4nhc_list.txt pgrbh 6 168 3 2014080600 gfs 120"
  echo " exiting"
  set -x
  exit 10
fi

NWPROD=${NWPROD:-/nwprod}
wgrib2=${WGRIB2:-$NWPROD/util/exec/wgrib2}

COMIN=${1:-${COMROT:-/ptmpd1/emc.glopara/prhw14}}
NHCOUTPUT=${2:-${NHCOUTPUT:-/global/noscrub/$LOGNAME/nhc_subset}}
NHCSUBSET_LIST=${3:-${NHCSUBSET_LIST:-/global/save/emc.glopara/misc/subset4nhc_list.txt}}
NHCSUBSET_PGB=${4:-${NHCSUBSET_PGB:-pgbq}}
NHCSUBSET_FHOUT=${5:-${NHCSUBSET_FHOUT:-6}}
NHCSUBSET_FHMAX=${6:-${NHCSUBSET_FHMAX:-180}}
PGB_FORMAT=${7:-${PGB_FORMAT:-2}}
CDATE=${8:-${CDATE:-2014080600}}
CDUMP=${9:-${CDUMP:-gfs}}
HRKSIG=${10:-${HRKSIG:-120}}    ;#clean files older than HRKSIG hours

if [ ! -d $COMIN ]; then
  set +x
  echo "ERROR:  $COMIN is not a directory.  Exiting..."
  set -x
  exit
fi

if [ ! -d $NHCOUTPUT ]; then mkdir -p  $NHCOUTPUT ; fi
cd $NHCOUTPUT  
if [ $? -ne 0 ]; then 
 echo " creating $NHCOUTPUT failed, exit"
 exit
fi

yyyymmddhh=$CDATE                        
hh=`echo $CDATE | cut -c 9-10`
NDATE=${NDATE:-NWPROD/util/exec/ndate}
CDATEM=`$NDATE -$HRKSIG $CDATE`
rm -f $NHCOUTPUT/*${CDATEM}*

for hour in $( seq 0 $NHCSUBSET_FHOUT $NHCSUBSET_FHMAX ) ; do
 FHR=$( printf %02d $hour )
 if [ $PGB_FORMAT -eq 2 ] ; then
   FHR=$( printf %02d $hour )
 elif [ $PGB_FORMAT -eq 3 ] ; then
   FHR=$( printf %03d $hour )
 fi
 #grib2_file=$COMIN/gfs.t${hh}z.pgrb2f${FHR}
 grib2_file=$COMIN/${NHCSUBSET_PGB}${FHR}.gfs.${yyyymmddhh}.grib2
 grib2_subset=${NHCOUTPUT}/${NHCSUBSET_PGB}${FHR}.gfs.${yyyymmddhh}_nhc.grib2
 if [ -s $grib2_subset ]; then rm -f $grib2_subset ; fi
 echo $grib2_file
 $wgrib2 -s $grib2_file | grep -F -f $NHCSUBSET_LIST | $wgrib2 -i -grib $grib2_subset $grib2_file
done

exit





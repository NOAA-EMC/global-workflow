#!/bin/ksh
set -x
# This script saves sigf and sfcf files to HWRF HPSS archive for running HWRF

export PSLOT=${PSLOT:-${1:-4dev}}
export CDATE=${CDATE:-${2:-2015062500}}
export COMROT=${COMROT:-/ptmpd3/$LOGNAME/pr$PSLOT}
##export FHMAX_HWRF=${FHMAX_HWRF:-126}
export FHMAX_HWRF=126
export FHOUT_HWRF=${FHOUT_HWRF:-6}

export HPSSTAR=${HPSSTAR:-/nwprod/util/ush/hpsstar}
export ATARDIR_HWRF=${ATARDIR_HWRF:-/2year/NCEPDEV/emc-hwrf/GFS-PR4DEV}
export hwrftar1=${hwrftar1:-pr${PSLOT}_${CDATE}gfs_hwrf_spectral_anl.tar}
export hwrftar2=${hwrftar2:-pr${PSLOT}_${CDATE}gfs_hwrf_ens06h.tar}
export hwrftar3=${hwrftar3:-pr${PSLOT}_${CDATE}gfs_hwrf_spectral.tar}
export hwrftar4=${hwrftar4:-pr${PSLOT}_${CDATE}gfs_hwrf_ens09h.tar}
$HPSSTAR mkd $ATARDIR_HWRF

cd $COMROT ||exit 8
list1=hpss_hwrf_list1$CDATE.txt
list2=hpss_hwrf_list2$CDATE.txt
list3=hpss_hwrf_list3$CDATE.txt
list4=hpss_hwrf_list4$CDATE.txt
if [ -s $list1 ]; then rm -f $list1 ; fi
if [ -s $list2 ]; then rm -f $list2 ; fi
if [ -s $list3 ]; then rm -f $list3 ; fi
if [ -s $list4 ]; then rm -f $list4 ; fi
>$list1                       
>$list2                       
>$list3                       
>$list4                       

echo siganl.gfs.$CDATE >>$list1          
echo sfcanl.gfs.$CDATE >>$list1               

CDATEM6=`/nwprod/util/exec/ndate -6 $CDATE`
typeset -Z3 ens
for ens in $( seq 1 80 ); do
#echo sfg_${CDATEM6}_fhr03s_mem${ens} >>$list3               
echo sfg_${CDATEM6}_fhr06s_mem${ens} >>$list2               
#echo sfg_${CDATEM6}_fhr09s_mem${ens} >>$list4               
done

echo sigf00.gfs.$CDATE >>$list1               

fh=06
while [ $fh -le $FHMAX_HWRF ]; do
 echo sigf$fh.gfs.$CDATE >>$list3               
 fh=`expr $fh + $FHOUT_HWRF `
 if [ $fh -lt 10 ]; then fh=0$fh ; fi
done

$HPSSTAR put $ATARDIR_HWRF/$hwrftar2  `cat $list2`               
$HPSSTAR put $ATARDIR_HWRF/$hwrftar1  `cat $list1`               
#$HPSSTAR put $ATARDIR_HWRF/$hwrftar3  `cat $list3`               
#$HPSSTAR put $ATARDIR_HWRF/$hwrftar4  `cat $list4`               
if [ $? -ne 0 ]; then exit 9 ; fi

exit 0


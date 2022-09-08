#!/bin/ksh
#########################################################
# This script generate gldas forcing from gdas prod sflux
# script history:
# 20190509 Jesse Meng - first version
# 20191008 Youlong Xia - modified
# 20191123 Fanglin Yang - restructured for global-workflow
#########################################################

export VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ]; then
   echo $(date) EXECUTING $0 $* >&2
   set -x
fi

bdate=$1
edate=$2

if [ $machine = "WCOSS_DELL_P3" ] || [ $machine = "WCOSS_C" ] || \
   [ $machine = "HERA" ] || [ $machine = "WCOSS2" ]; then
  touch ./cfile
fi

### COMINgdas = prod gdas sflux grib2
### RUNDIR = gldas forcing in grib2 format
### RUNDIR/force = gldas forcing in grib1 format
export COMPONENT=${COMPONENT:-atmos}
fpath=$RUNDIR
gpath=$RUNDIR/force
cycint=${assim_freq:-6}

# get gdas flux files to force gldas.
# CPC precipitation is from 12z to 12z. One more day of gdas data is 
# needed to disaggregate daily CPC precipitation values to hourly values
cdate=`$NDATE -12 $bdate`

iter=0

#-------------------------------
while [ $cdate -lt $edate ]; do
#-------------------------------
  ymd=`echo $cdate |cut -c 1-8`
  cyc=`echo $cdate |cut -c 9-10`
  [[ ! -d $fpath/gdas.${ymd} ]] && mkdir -p $fpath/gdas.${ymd}
  [[ ! -d $gpath/gdas.${ymd} ]] && mkdir -p $gpath/gdas.${ymd} 

f=1
while [ $f -le $cycint ]; do
  rflux=${COMINgdas}/gdas.$ymd/$cyc/$COMPONENT/gdas.t${cyc}z.sfluxgrbf00$f.grib2
  fflux=$fpath/gdas.$ymd/gdas.t${cyc}z.sfluxgrbf0$f.grib2
  gflux=$gpath/gdas.$ymd/gdas1.t${cyc}z.sfluxgrbf0$f
  if [ ! -s $rflux ];then
     echo "WARNING: GLDAS MISSING $rflux, WILL NOT RUN."
     exit 2
  fi
  rm -f $fflux $gflux
  touch $fflux $gflux

  fcsty=anl
  if [ $f -ge 1 ]; then fcsty=fcst; fi

  if [ $machine = "WCOSS_DELL_P3" ] || [ $machine = "WCOSS_C" ] || \
   [ $machine = "WCOSS2" ]; then
    echo "${USHgldas}/gldas_process_data.sh $rflux $fcsty $fflux $gflux $f" >> ./cfile
  elif [ $machine = "HERA" ]; then
    echo "$iter ${USHgldas}/gldas_process_data.sh $rflux $fcsty $fflux $gflux $f" >> ./cfile
  else
    ${USHgldas}/gldas_process_data.sh $rflux $fcsty $fflux $gflux $f
  fi

  iter=$((iter+1))
  f=$((f+1))
done

#-------------------------------
  cdate=`$NDATE +$cycint $cdate` 
done
#-------------------------------

if [ $machine = "WCOSS_DELL_P3" ] || [ $machine = "WCOSS_C" ] || \
   [ $machine = "HERA" ] || [ $machine = "WCOSS2" ]; then
  $APRUN_GLDAS_DATA_PROC ./cfile
fi

exit $?

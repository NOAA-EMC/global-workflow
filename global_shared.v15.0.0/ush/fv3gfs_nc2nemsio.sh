#!/bin/ksh 
set -x
#----------------------------------------------------------------------------
#--Fanglin Yang, October 2016: convert FV3 NetCDF files to NEMSIO format.
#    Note FV3 lat-lon grid is located at the center of each grid box,
#    starting from south to north and from east to west.
#    For example, for a 0.5-deg uniform grid, nlon=720, nlat=360
#    X(1,1)=[0.25E,89.75S], X(nlon,nlat)=[359.75E,89.75N]
#---------------------------------------------------------------------------

##. $MODULESHOME/init/sh 2>>/dev/null
##module load PrgEnv-intel 2>>/dev/null

export CDATE=${CDATE:-2016100300}
export CASE=${CASE:-C192}                 ;#C48 C96 C192 C384 C768 C1152 C3072
export GG=${master_grid:-0p25deg}         ;#1deg 0p5deg 0p25deg 0p125deg     
export FHMAX=${FHMAX:-240}             
export FHOUT=${FHOUT:-6}             
export FHZER=${FHZER:-6}                  ;#accumulation bucket in hours             
export NFCST=${NFCST:-$((FHMAX/FHOUT+1))} ;#number of forecatsts included in netCDF file
export fdiag=${fdiag:-none}               ;#specified forecast output hours

export PSLOT=${PSLOT:-fv3gfs}
export PTMP=${PTMP:-/gpfs/hps/ptmp}
export COMROT=${COMROT:-$PTMP/$LOGNAME/pr${PSLOT}}
export BASE_GSM=${BASE_GSM:-/gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/gfs/fv3gfs/global_shared.v15.0.0}
export NC2NEMSIOEXE=${NC2NEMSIOEXE:-$BASE_GSM/exec/fv3nc2nemsio.x}
#--------------------------------------------------
cd $COMROT ||exit 8
err=0
input_dir=$COMROT
output_dir=$COMROT

in_3d=${CASE}_${CDATE}.nggps3d.${GG}.nc
in_2d=${CASE}_${CDATE}.nggps2d.${GG}.nc
if [ ! -s $in_3d -o ! -s $in_2d ]; then
 echo "$in_3d and $in_2d are missing. exit"
 exit 1
fi

#--check if the output is from non-hydrostatic case
nhrun=$(ncdump -c $in_3d |grep nhpres)
nhcase=$?

if [ $fdiag = none ]; then
 fdiag=$( for (( num=1; num<=$NFCST; num++ )); do printf "%d," $(((num-1)*FHOUT)); done )
fi
#---------------------------------------------------
nt=0
for fhour in $(echo $fdiag | sed "s?,? ?g"); do
#---------------------------------------------------
   nt=$((nt+1))
   ifhour=$(printf "%09d" $fhour)              ;#convert to integer
   fhzh=$(( (ifhour/FHZER-1)*FHZER ))          ;#bucket accumulation starting hour
   if [ $fhzh -lt 0 ]; then fhzh=0; fi
   outfile=${CASE}_nemsio${GG}
   $NC2NEMSIOEXE $CDATE $nt $fhzh $fhour $input_dir $in_2d $in_3d $output_dir $outfile $nhcase
   err=$?
done

echo $(date) EXITING $0 with return code $err >&2
exit $err

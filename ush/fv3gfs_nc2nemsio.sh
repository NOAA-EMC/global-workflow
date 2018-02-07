#!/bin/ksh
set -x
#----------------------------------------------------------------------------
#--Fanglin Yang, October 2016: convert FV3 NetCDF files to NEMSIO format.
#    Note FV3 lat-lon grid is located at the center of each grid box,
#    starting from south to north and from east to west.
#    For example, for a 0.5-deg uniform grid, nlon=720, nlat=360
#    X(1,1)=[0.25E,89.75S], X(nlon,nlat)=[359.75E,89.75N]
#---------------------------------------------------------------------------

export CDATE=${CDATE:-"2016100300"}
export GG=${master_grid:-"0p25deg"}         # 1deg 0p5deg 0p25deg 0p125deg
export FHZER=${FHZER:-6}                    # accumulation bucket in hours
export fdiag=${fdiag:-"none"}               # specified forecast output hours

pwd=$(pwd)
export DATA=${DATA:-$pwd}
export NWPROD=${NWPROD:-$pwd}
export HOMEgfs=${HOMEgfs:-$NWPROD}
export NC2NEMSIOEXE=${NC2NEMSIOEXE:-$HOMEgfs/exec/fv3nc2nemsio.x}

cycn=`echo $CDATE | cut -c 9-10`
export TCYC=${TCYC:-".t${cycn}z."}
export CDUMP=${CDUMP:-gfs}

export PREFIX=${PREFIX:-${CDUMP}${TCYC}}
export SUFFIX=${SUFFIX:-".nemsio"}

#--------------------------------------------------
cd $DATA || exit 8

input_dir=$DATA
output_dir=$DATA

in_3d=${PREFIX}nggps3d.${GG}.nc
in_2d=${PREFIX}nggps2d.${GG}.nc
if [ ! -s $in_3d -o ! -s $in_2d ]; then
  echo "$in_3d and $in_2d are missing. exit"
  exit 1
fi

#--check if the output is from non-hydrostatic case
nhrun=$(ncdump -c $in_3d | grep nhpres)
nhcase=$?

# If no information on the time interval is given, deduce from the netCDF file
[[ $fdiag = "none" ]] && fdiag=$(ncks -H -s "%g " -C -v time $in_3d)

#---------------------------------------------------
nt=0
err=0
for fhour in $(echo $fdiag | sed "s/,/ /g"); do
   nt=$((nt+1))
   ifhour=$(printf "%09d" $fhour)              # convert to integer
   fhzh=$(( (ifhour/FHZER-1)*FHZER ))          # bucket accumulation starting hour
   [[ $fhzh -lt 0 ]] && fhzh=0

   fhr=$(printf "%03d" $fhour)
   outfile=${PREFIX}atmf${fhr}${SUFFIX}

   $NC2NEMSIOEXE $CDATE $nt $fhzh $fhour $input_dir $in_2d $in_3d $output_dir $outfile $nhcase
   rc=$?
   ((err+=rc))

   [[ ! -f $outfile ]] && ((err+=1))

done

#---------------------------------------------------
echo $(date) EXITING $0 with return code $err >&2
exit $err

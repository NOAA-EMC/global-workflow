#!/bin/sh 
#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/log.nc2nemsio
#BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/log.nc2nemsio
#BSUB -J nc2nemsio
#BSUB -q dev
#BSUB -M 3072
#BSUN -n 1
##BSUB -x
#BSUB -W 10:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang
#BSUB -extsched 'CRAYLINUX[]'
set -ax

#----------------------------------------------------------------------------
#--Fanglin Yang, October 2016: convert FV3 NetCDF files to NEMSIO format.
#    Note FV3 lat-lon grid is located at the center of each grid box,
#    starting from south to north and from east to west.
#    For example, for a 0.5-deg uniform grid, nlon=720, nlat=360
#    X(1,1)=[0.25E,89.75S], X(nlon,nlat)=[359.75E,89.75N]
#---------------------------------------------------------------------------

. $MODULESHOME/init/sh
module load PrgEnv-intel

export CDATE=${CDATE:-2016100300}
export CASE=${CASE:-C768}         ;#C48 C96 C192 C384 C768 C1152 C3072
export NFCST=${NFCST:-40}         ;#number of forecatsts included in netCDF file
export GG=${GG:-0p25deg}          ;#1deg 0p5deg 0p25deg 0p125deg     

export PTMP=${PTMP:-/gpfs/hps/ptmp}
export input_dir=${input_dir:-$PTMP/$LOGNAME/fv3/archive}
export output_dir=${output_dir:-$PTMP/$LOGNAME/fv3/nemsio}
export COMROT=${COMROT:-$PTMP/$LOGNAME/fv3/${CASE}_${CDATE}}

export home_dir=${home_dir:-/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS}
export script_dir=$home_dir/ush
export nc2nemsio=$home_dir/exec/fv3nc2nemsio.x   
#--------------------------------------------------
if [ ! -s $input_dir ]; then mkdir -p $input_dir ;fi
if [ ! -s $output_dir ]; then mkdir -p $output_dir ;fi
if [ ! -s $COMROT ]; then mkdir -p $COMROT ;fi

export NODES=1
#export max_core=${max_core:-24}
#export thread=${thread:-1}
#export npe_node=$((max_core/thread))
#export npes=$((NODES*npe_node))
#export APRUN="aprun -n $npes -N $npe_node -j 1 -d $thread -cc depth"
export APRUN="aprun -n 1 -N 1 -j 1 -d 1 -cc depth"

#--------------------------------------------------
cd $COMROT ||exit 8

in_3d=${CASE}_${CDATE}.nggps3d_4xdaily.${GG}.nc
in_2d=${CASE}_${CDATE}.nggps2d.${GG}.nc

nt=1
while [ $nt -le $NFCST ];do
   outfile=${CASE}_nemsio${GG}
   $nc2nemsio $CDATE $nt $input_dir $in_2d $in_3d $output_dir $outfile
   nt=$((nt+1))       
done

echo "complete fv3nc2nemsio for $CASE $CDATE"
exit


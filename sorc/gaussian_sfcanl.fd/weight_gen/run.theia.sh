#!/bin/sh

#------------------------------------------------------------------------
# Run the "RegridWeightGen" step on Theia to create interpolation
# weight files to transform from cubed-sphere tiles to global
# gaussian.
#
# First, create the 'scrip' files for the gaussian grids.  Two
# grids are created - the normal gaussian grid, and one with
# two extra rows at the N/S poles.  The program to create the
# script files is in ./scrip.fd.  To compile, type 'make.sh'.
# Then, run the RegridWeightGen step to create the interpolation
# weight files.
#------------------------------------------------------------------------

#PBS -l nodes=1:ppn=1
#PBS -l walltime=0:30:00
#PBS -A fv3-cpu
#PBS -q debug
#PBS -N fv3_wgtgen
#PBS -o ./log
#PBS -e ./log

set -x

CRES=C48     # run with one mpi task
#CRES=C96     # run with one mpi task
#CRES=C128    # run with one mpi task
#CRES=C192    # run with one mpi task
#CRES=C384    # run with one mpi task
#CRES=C768    # run with 4 mpi tasks
#CRES=C1152   # run with 8 mpi tasks
#CRES=C3072   # run on two nodes, 8 tasks per node

WORK=/scratch3/NCEPDEV/stmp1/$LOGNAME/weight_gen
rm -fr $WORK
mkdir -p $WORK
cd $WORK

source /apps/lmod/lmod/init/sh
module purge
module load intel/15.1.133
module load impi/5.0.1.035
module use /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/soft/modulefiles
module load esmf/7.1.0r
module load netcdf/4.3.0
module load hdf5/1.8.14

#------------------------------------------------------------------------
# The RegridWeightGen program.
#------------------------------------------------------------------------

RWG=/scratch4/NCEPDEV/nems/noscrub/emc.nemspara/soft/esmf/7.1.0r/bin/ESMF_RegridWeightGen

#------------------------------------------------------------------------
# Path to the 'mosaic' and 'grid' files for each cubed-sphere 
# resolution.
#------------------------------------------------------------------------

FIX_DIR=/scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/fix/fix_fv3_gmted2010/$CRES

#------------------------------------------------------------------------
# Create 'scrip' files for two gaussian grids.  One normal grid
# and one with two extra rows at the N/S poles.
#------------------------------------------------------------------------

${PBS_O_WORKDIR}/scrip.fd/scrip.exe $CRES

if [[ $? -ne 0 ]]; then
  echo "ERROR CREATING SCRIP FILE"
  exit 2
fi

#------------------------------------------------------------------------
# Create weight files.
#------------------------------------------------------------------------

case $CRES in
  "C48" )
    LONB="192"
    LATB="94"
    LATB2="96" 
    ;;
  "C96" )
    LONB="384"
    LATB="192"
    LATB2="194" 
    ;;
  "C128" )
    LONB="512"
    LATB="256"
    LATB2="258" 
    ;;
  "C192" )
    LONB="768"
    LATB="384"
    LATB2="386" 
    ;;
  "C384" )
    LONB="1536"
    LATB="768"
    LATB2="770" 
    ;;
  "C768" )
    LONB="3072"
    LATB="1536"
    LATB2="1538" 
    ;;
  "C1152" )
    LONB="4608"
    LATB="2304"
    LATB2="2306" 
    ;;
  "C3072" )
    LONB="12288"
    LATB="6144"
    LATB2="6146" 
    ;;
  * )
    echo "GRID NOT SUPPORTED"
    exit 3 
    ;;
esac

np=$PBS_NP

mpirun -np $np $RWG -d ./gaussian.${LONB}.${LATB}.nc  -s $FIX_DIR/${CRES}_mosaic.nc \
      -w fv3_SCRIP_${CRES}_GRIDSPEC_lon${LONB}_lat${LATB}.gaussian.neareststod.nc \
      -m neareststod --64bit_offset --tilefile_path $FIX_DIR

mpirun -np $np $RWG -d ./gaussian.${LONB}.${LATB}.nc  -s $FIX_DIR/${CRES}_mosaic.nc \
      -w fv3_SCRIP_${CRES}_GRIDSPEC_lon${LONB}_lat${LATB}.gaussian.bilinear.nc \
      -m bilinear --64bit_offset --tilefile_path $FIX_DIR

mpirun -np $np $RWG -d ./gaussian.${LONB}.${LATB2}.nc  -s $FIX_DIR/${CRES}_mosaic.nc \
      -w fv3_SCRIP_${CRES}_GRIDSPEC_lon${LONB}_lat${LATB2}.gaussian.neareststod.nc \
      -m neareststod --64bit_offset --tilefile_path $FIX_DIR

#------------------------------------------------------------------------
# Could not get this C3072 bilinear option to work.  This grid is
# so big we are pushing the limits of the utility.
#------------------------------------------------------------------------

if [[ $CRES == "C3072" ]]; then
 exit 0
fi

mpirun -np $np $RWG -d ./gaussian.${LONB}.${LATB2}.nc  -s $FIX_DIR/${CRES}_mosaic.nc \
      -w fv3_SCRIP_${CRES}_GRIDSPEC_lon${LONB}_lat${LATB2}.gaussian.bilinear.nc \
      -m bilinear --64bit_offset --tilefile_path $FIX_DIR

exit

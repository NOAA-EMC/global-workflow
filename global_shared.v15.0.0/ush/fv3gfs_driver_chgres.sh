#!/bin/sh
#----WCOSS_CRAY JOBCARD
#BSUB -L /bin/sh
#BSUB -P FV3GFS-T2O
#BSUB -oo log.chgres
#BSUB -eo log.chgres
#BSUB -J chgres_fv3
#BSUB -q devonprod
#BSUB -W 06:00
#BSUB -M 1024
#BSUB -extsched 'CRAYLINUX[]'

#----WCOSS JOBCARD
##BSUB -L /bin/sh
##BSUB -P FV3GFS-T2O
##BSUB -oo log.chgres
##BSUB -eo log.chgres
##BSUB -J chgres_fv3
##BSUB -q devonprod
##BSUB -x
##BSUB -a openmp
##BSUB -n 24
##BSUB -R span[ptile=24]
#----THEIA JOBCARD
##PBS -l nodes=1:ppn=24
##PBS -l walltime=0:12:00
##PBS -A glbss
##PBS -N chgres_fv3
##PBS -o log.chres
##PBS -e log.chres


set -ax
#-------------------------------------------------------------------------------------------------
# Makes ICs on fv3 globally uniform cubed-sphere grid using operational GFS initial conditions.
# Fanglin Yang, 09/30/2016
#  This script is created based on the C-shell scripts fv3_gfs_preproc/IC_scripts/DRIVER_CHGRES.csh
#  and submit_chgres.csh provided by GFDL.  APRUN and environment variables are added to run on 
#  WCOSS CRAY.  Directory and file names are standaridized to follow NCEP global model convention.
#  This script calls fv3gfs_chgres.sh.
# Fanglin Yang and George Gayno, 02/08/2017
#  Modified to use the new CHGRES George Gayno developed.
# Fanglin Yang 03/08/2017
#  Generalized and streamlined the script and enabled to run on multiple platforms.
#-------------------------------------------------------------------------------------------------

export machine=${machine:-WCOSS_C}
export NODES=1
export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS_CH:-24}

if [ $machine = WCOSS_C ]; then 
 . $MODULESHOME/init/sh 2>>/dev/null
 module load PrgEnv-intel 2>>/dev/null
 export KMP_AFFINITY=disabled
 export APRUNC="aprun -n 1 -N 1 -j 1 -d $OMP_NUM_THREADS_CH -cc depth"
elif [ $machine = WCOSS ]; then 
 . /usrx/local/Modules/default/init/sh 2>>/dev/null
 module load ics/12.1 NetCDF/4.2/serial 2>>/dev/null
 export FIX_FV3=${FIX_FV3:-/gpfs/hps/emc/global/noscrub/emc.glopara/svn/fv3gfs/fix_fv3}
elif [ $machine = THEIA ]; then 
 module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
 module load netcdf/4.3.0 hdf5/1.8.14 2>>/dev/null
 export FIX_FV3=${FIX_FV3:-/scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/fix_fv3}
else 
 echo "$machine not supported, exit"
 exit
fi

export BASE_GSM=${BASE_GSM:-/gpfs/hps/ptmp/emc.glopara/EXP-cyc/global_shared.v15.0.0}                        
export script_dir=$BASE_GSM/ush

export CASE=${CASE:-C96}                     # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072         
export CDATE=${CDATE:-${cdate:-2017030800}}  # format yyyymmddhh yyyymmddhh ... 

export out_dir=${out_dir:-/gpfs/hps/ptmp/$LOGNAME/FV3IC/ICs}
export tmp_dir=${tmp_dir:-/gpfs/hps/ptmp/$LOGNAME/FV3IC/chgres_${CASE}_${CDATE}}                                       
if [ ! -s $out_dir ]; then mkdir -p $out_dir; fi           
if [ ! -s $tmp_dir ]; then mkdir -p $tmp_dir; fi
export VERBOSE=YES
#---------------------------------------------------------
#---------------------------------------------------------

export gtype=uniform	          # grid type = uniform, stretch, or nested

if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
  export name=${CASE}
  export ntiles=6
elif [ $gtype = stretch ]; then
  export stetch_fac=       	                 # Stretching factor for the grid
  export rn=`expr $stetch_fac \* 10 `
  export name=${CASE}r${rn}       		 # identifier based on refined location (same as grid)
  export ntiles=6
  echo "creating stretched ICs"
elif [ $gtype = nest ]; then 
  export stetch_fac=  	                         # Stretching factor for the grid
  export rn=`expr $stetch_fac \* 10 `
  export refine_ratio=   	                 # Specify the refinement ratio for nest grid
  export name=${CASE}r${rn}n${refine_ratio}      # identifier based on nest location (same as grid)
  export ntiles=7
  echo "creating nested ICs"
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, or nest"
fi

#---------------------------------------------------------------

  echo "processing " $CDATE
  export ymd=`echo $CDATE |cut -c 1-8`
  export gfs_dir=${inidir:-$COMROOTp2/gfs/prod/gfs.$ymd}  #directory that contains GFS input file(s)
  export outdir=$out_dir/${name}_${CDATE}
  mkdir -p $outdir

  export COMOUT=$outdir
  export DATA=$tmp_dir
  export APRUN=$APRUNC

cyc=`echo $CDATE|cut -c 9-10`
if [ -s ${gfs_dir}/siganl.gfs.$CDATE ]; then
  export ATMANL=$gfs_dir/siganl.gfs.$CDATE
  export SFCANL=$gfs_dir/sfcanl.gfs.$CDATE
else
  export ATMANL=$gfs_dir/gfs.t${cyc}z.sanl 
  export SFCANL=$gfs_dir/gfs.t${cyc}z.sfcanl 
fi


  # make atmospheric and surface data
  $script_dir/fv3gfs_chgres.sh

exit


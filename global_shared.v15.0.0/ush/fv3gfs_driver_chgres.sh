#!/bin/sh
#BSUB -L /bin/sh
#BSUB -P FV3GFS-T2O
#BSUB -oo log.chgres
#BSUB -eo log.chgres
#BSUB -J chgres_fv3
#BSUB -q dev
#BSUB -M 1024
##BSUB -x
#BSUB -W 06:00
#BSUB -extsched 'CRAYLINUX[]'
set -ax

#-------------------------------------------------------------------------------------------------
# Makes ICs on fv3 globally uniform cubed-sphere grid using operational GFS initial conditions.
# Fanglin Yang, 09/30/2016
#  This script is created based on the C-shell scripts fv3_gfs_preproc/IC_scripts/DRIVER_CHGRES.csh
#  and submit_chgres.csh provided by GFDL.  APRUN and environment variables are added to run on 
#  WCOSS CRAY.  Directory and file names are standaridized to follow NCEP global model convention.
#  This script calls fv3gfs_chgres.sh.
# Fanglin Yang and George Gayno, 02/08/2017
#  Further modified to use the new CHGRES George Gayno developed.
#-------------------------------------------------------------------------------------------------

. $MODULESHOME/init/sh 2>>/dev/null
module load PrgEnv-intel 2>>/dev/null
export NODES=1
export KMP_AFFINITY=disabled
export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS_CH:-24}
#export HUGETLB_MORECORE=yes
export APRUNC="aprun -n 1 -N 1 -j 1 -d $OMP_NUM_THREADS_CH -cc depth"
export USER=$LOGNAME                 # your username 
export VERBOSE=YES

export res=${res:-768}               # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072         
export date=${cdate:-2016120100}     # format yyyymmddhh yyyymmddhh ... 


export BASE_GSM=${BASE_GSM:-/gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs/global_shared.v15.0.0}
export script_dir=$BASE_GSM/ush
export fix_fv3_dir=$BASE_GSM/fix/C${res}
export fix_gsm_dir=$BASE_GSM/fix/fix_am     
export exec_dir=$BASE_GSM/exec         

export out_dir=${out_dir:-/gpfs/hps/ptmp/$LOGNAME/FV3IC/ICs}
export tmp_dir=${tmp_dir:-/gpfs/hps/ptmp/$LOGNAME/FV3IC/chgres}                                       
if [ ! -s $out_dir ]; then mkdir -p $out_dir; fi           
if [ ! -s $tmp_dir ]; then mkdir -p $tmp_dir; fi           
#---------------------------------------------------------
#---------------------------------------------------------

export gtype=uniform	          # grid type = uniform, stretch, or nested

if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
  export name=C${res}
  export ntiles=6
elif [ $gtype = stretch ]; then
  export stetch_fac=       	                 # Stretching factor for the grid
  export rn=`expr $stetch_fac \* 10 `
  export name=C${res}r${rn}       		 # identifier based on refined location (same as grid)
  export ntiles=6
  echo "creating stretched ICs"
elif [ $gtype = nest ]; then 
  export stetch_fac=  	                         # Stretching factor for the grid
  export rn=`expr $stetch_fac \* 10 `
  export refine_ratio=   	                 # Specify the refinement ratio for nest grid
  export name=C${res}r${rn}n${refine_ratio}      # identifier based on nest location (same as grid)
  export ntiles=7
  echo "creating nested ICs"
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, or nest"
fi

#---------------------------------------------------------------

  echo "processing " $date
  export ymd=`echo $date |cut -c 1-8`
  export cyc=`echo $date |cut -c 9-10`
  export gfs_dir=${inidir:-$COMROOTp2/gfs/prod/gfs.$ymd}  #directory that contains GFS input file(s)
  export grid_dir=$fix_fv3_dir
  export outdir=$out_dir/${name}_${date}
  mkdir -p $outdir

  # make atmospheric data
  export TMPDIR=$tmp_dir/gfs
  if [ -s $TMPDIR ]; then rm -rf $TMPDIR ; fi
  mkdir -p $TMPDIR;  cd $TMPDIR || exit 8
  $script_dir/fv3gfs_chgres.sh GFS $res $grid_dir $outdir $date $gfs_dir $gtype 

  # make surface data
  tile=1
  while  [ $tile -le $ntiles ]; do
    export TMPDIR=$tmp_dir/sfc_tile$tile
    if [ -s $TMPDIR ]; then rm -rf $TMPDIR ; fi
    mkdir -p $TMPDIR;  cd $TMPDIR || exit 8
     $script_dir/fv3gfs_chgres.sh SFC $res $tile $grid_dir $outdir $date $gfs_dir 
    tile=$((tile+1))
  done


exit


#!/bin/sh
#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/log.chgres
#BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/log.chgres
#BSUB -J chgres_fv3
#BSUB -q dev
#BSUB -M 2400
#BSUB -x
#BSUB -W 06:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang
#BSUB -extsched 'CRAYLINUX[]'
set -ax

#-------------------------------------------------------------------------------------------------
# Makes ICs on fv3 globally uniform cubed-sphere grid using operational GFS initial conditions.
# Fanglin Yang, 09/30/2016
#  This script is created based on the C-shell scripts fv3_gfs_preproc/IC_scripts/DRIVER_CHGRES.csh
#  and submit_chgres.csh provided by GFDL.  APRUN and environment variables are added to run on 
#  WCOSS CRAY.  Directory and file names are standaridized to follow NCEP global model convention.
#  This script calls run_chgres.sh.
#-------------------------------------------------------------------------------------------------

 . $MODULESHOME/init/sh
module load PrgEnv-intel
export NODES=1
export OMP_NUM_THREADS_CH=24
export HUGETLB_MORECORE=yes
export APRUNC="aprun -n 1 -N 1 -j 1 -d 24 -cc depth"

export USER=$LOGNAME              # your username 
export res=3072    	          # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072         
export gtype=uniform	          # grid type = uniform, stretch, or nested

export home_dir=/gpfs/hps/emc/global/noscrub/Fanglin.Yang/NGGPS
export script_dir=$home_dir/ush
export fix_fv3_dir=$home_dir/fix/C${res}
export fix_gsm_dir=$NWROOTp2/global_shared.v13.0.3/fix/fix_am     
export exec_dir=$home_dir/exec         

export out_dir=/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS/ICs
export tmp_dir=/gpfs/hps/ptmp/$LOGNAME/fv3_chgres                                       
mkdir -p $out_dir $tmp_dir

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
export dates=2016101900           # format yyyymmddhh yyyymmddhh ... 

for date in $dates ; do
  echo "processing " $date
  export ymd=`echo $date |cut -c 1-8`
  export cyc=`echo $date |cut -c 9-10`
  export gfs_dir=$COMROOTp2/gfs/prod/gfs.$ymd                # directory that contains raw GFS input file(s)
  export grid_dir=$fix_fv3_dir
  export outdir=$out_dir/${name}_${date}
  mkdir -p $outdir

  # make atmospheric data
  export TMPDIR=$tmp_dir/${name}_${date}/gfs
  if [ -s $TMPDIR ]; then rm -rf $TMPDIR ; fi
  mkdir -p $TMPDIR;  cd $TMPDIR || exit 8
  $script_dir/run_chgres.sh GFS $res $grid_dir $outdir $date $gfs_dir $gtype 

  # make surface data
  tile=1
  while  [ $tile -le $ntiles ]; do
    export TMPDIR=$tmp_dir/${name}_${date}/sfc_tile$tile
    if [ -s $TMPDIR ]; then rm -rf $TMPDIR ; fi
    mkdir -p $TMPDIR;  cd $TMPDIR || exit 8
     $script_dir/run_chgres.sh SFC $res $tile $grid_dir $outdir $date $gfs_dir 
    tile=$((tile+1))
  done

done


exit


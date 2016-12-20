#!/bin/sh
#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/log.grid
#BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/log.grid
#BSUB -J grid_fv3
#BSUB -q dev
#BSUB -M 2400
#BSUB -x
#BSUB -W 10:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang
#BSUB -extsched 'CRAYLINUX[]'
set -ax

#-----------------------------
# Makes FV3 cubed-sphere grid
#-----------------------------

. $MODULESHOME/init/sh
module load PrgEnv-intel
export NODES=1
export OMP_NUM_THREADS_CH=24
export APRUN="aprun -n 1 -N 1 -j 1 -d 24 -cc depth"
#----------------------------------------------------------------

export USER=$LOGNAME 
export res=768	 	   # resolution of tile: 48, 96, 192, 384, 768, 1152, 3072
export gtype=uniform	   # grid type: uniform, stretch, or nest

#----------------------------------------------------------------
export home_dir=/gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs/global_shared.v15.0.0
export workflow_dir=/gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs/gfs_workflow.v15.0.0/para
export script_dir=$home_dir/ush
export topo=$workflow_dir/util/fix
export exec_dir=$home_dir/exec

export out_dir=$home_dir/fix/C${res}
#export out_dir=/gpfs/hps/ptmp/$LOGNAME/fv3_grid/fix/C${res}
export TMPDIR=/gpfs/hps/ptmp/$LOGNAME/fv3_grid
mkdir -p $out_dir $TMPDIR
cd $TMPDIR ||exit 8

#----------------------------------------------------------------
if [ $gtype = uniform ];  then
  echo "creating uniform ICs"
elif [ $gtype = stretch ]; then
  export stetch_fac=  	 # Stretching factor for the grid
  export target_lon= 	 # center longitude of the highest resolution tile
  export target_lat= 	 # center latitude of the highest resolution tile
  export title=		 # identifier based on refined location
  echo "creating stretched grid"
elif [ $gtype = nest ]; then
  export stetch_fac=  	 # Stretching factor for the grid
  export target_lon=   	 # center longitude of the highest resolution tile
  export target_lat= 	 # center latitude of the highest resolution tile 
  export refine_ratio= 	 # Specify the refinement ratio for nest grid
  export istart_nest= 	 # Specify the starting i-direction index of nest grid in parent tile supergrid(Fortran index)
  export jstart_nest= 	 # Specify the starting j-direction index of nest grid in parent tile supergrid(Fortran index)
  export iend_nest= 	 # Specify the ending i-direction index of nest grid in parent tile supergrid(Fortran index)
  export jend_nest= 	 # Specify the ending j-direction index of nest grid in parent tile supergrid(Fortran index)
  export halo=  	 # halo size to be used in the atmosphere cubic sphere model. It only needs to be specified when --nest_grid is set
  export title=		 # identifier based on nest location
  echo "creating nested grid"
else
  echo "Error: please specify grid type with 'gtype' as uniform, stretch, or nest"
fi

#----------------------------------------------------------------
#filter_topo parameters. C192->50km, C384->25km, C768->13km, C1152->8.5km, C3072->3.2km
if [ $res -eq 48 ]; then 
 export cd4=0.12;  export max_slope=0.12; export n_del2_weak=4;   export peak_fac=1.1  
elif [ $res -eq 96 ]; then 
 export cd4=0.12;  export max_slope=0.12; export n_del2_weak=8;   export peak_fac=1.1  
elif [ $res -eq 192 ]; then 
 export cd4=0.15;  export max_slope=0.12; export n_del2_weak=12;  export peak_fac=1.05  
elif [ $res -eq 384 ]; then 
 export cd4=0.15;  export max_slope=0.12; export n_del2_weak=12;  export peak_fac=1.0  
elif [ $res -eq 768 ]; then 
 export cd4=0.15;  export max_slope=0.12; export n_del2_weak=16;   export peak_fac=1.0  
elif [ $res -eq 1152 ]; then 
 export cd4=0.15;  export max_slope=0.16; export n_del2_weak=20;   export peak_fac=1.0  
elif [ $res -eq 3072 ]; then 
 export cd4=0.15;  export max_slope=0.30; export n_del2_weak=24;   export peak_fac=1.0  
else
 echo "grid C$res not supported, exit"
 exit
fi

#----------------------------------------------------------------
# Make grid and orography

if [ $gtype = uniform ];  then
  export ntiles=6
  export name=C${res}
  export grid_dir=$TMPDIR/$name/grid
  export orog_dir=$TMPDIR/$name/orog
  export filter_dir=$TMPDIR/$name/filter_topo
  rm -rf $TMPDIR/$name                  
  mkdir -p $grid_dir $orog_dir $filter_dir

  echo 
  echo "............ execute fv3gfs_make_grid.sh ................."
  $script_dir/fv3gfs_make_grid.sh $res $grid_dir $script_dir

  for tile in 1 2 3 4 5 6 ; do
    echo 
    echo "............ execute fv3gfs_make_orog.sh for tile $tile .................."
    $script_dir/fv3gfs_make_orog.sh $res $tile $grid_dir $orog_dir $script_dir $topo $TMPDIR
  done

  echo 
  echo "............ execute fv3gfs_filter_topo.sh .............."
  $script_dir/filter_topo.sh $res $grid_dir $orog_dir $filter_dir $cd4 $peak_fac $max_slope $n_del2_weak $script_dir 
  echo "Grid and orography files are now prepared for uniform grid"

elif [ $gtype = stretch ]; then
  export ntiles=6
  export rn=`expr $stetch_fac \* 10 `          
  export rn=`echo $rn | cut -c1-2`
  export name=C${res}r${rn}_${title}
  export grid_dir=$TMPDIR/${name}/grid
  export orog_dir=$TMPDIR/$name/orog
  export filter_dir=$TMPDIR/${name}/filter_topo
  rm -rf $TMPDIR/$name                  
  mkdir -p $grid_dir $orog_dir $filter_dir

  echo 
  echo "............ execute fv3gfs_make_grid.sh ................."
  $script_dir/fv3gfs_make_grid.sh $res $grid_dir $stetch_fac $target_lon $target_lat $script_dir

  for tile in 1 2 3 4 5 6 ; do
    echo 
    echo "............ execute fv3gfs_make_orog.sh for tile $tile .................."
    $script_dir/fv3gfs_make_orog.sh $res $tile $grid_dir $orog_dir $script_dir $topo $TMPDIR
  done

  echo 
  echo "............ execute filter_topo.sh .............."
  $script_dir/fv3gfs_filter_topo.sh $res $grid_dir $orog_dir $filter_dir $cd4 $peak_fac $max_slope $n_del2_weak $script_dir 
  echo "Grid and orography files are now prepared for stretched grid"

elif [ $gtype = nest ]; then
  export ntiles=7
  export rn=`expr $stetch_fac \* 10 `          
  export rn=`echo $rn | cut -c1-2`
  export name=C${res}r${rn}n${refine_ratio}_${title}
  export grid_dir=$TMPDIR/${name}/grid
  export orog_dir=$TMPDIR/$name/orog
  export filter_dir=$orog_dir   # nested grid topography will be filtered online
  rm -rf $TMPDIR/$name                  
  mkdir -p $grid_dir $orog_dir $filter_dir

  echo 
  echo "............ execute fv3gfs_make_grid.sh ................."
  $script_dir/fv3gfs_make_grid.sh $res $grid_dir $stetch_fac $target_lon $target_lat $refine_ratio $istart_nest $jstart_nest $iend_nest $jend_nest $halo $script_dir

  for tile in 1 2 3 4 5 6 7; do
    echo 
    echo "............ execute fv3gfs_make_orog.sh for tile $tile .................."
    $script_dir/fv3gfs_make_orog.sh $res $tile $grid_dir $orog_dir $script_dir $topo $TMPDIR
  done
  echo "Grid and orography files are now preparedd for nested grid"
fi
#----------------------------------------------------------------



echo 
echo "--------------------------------------------------"

tile=1
while [ $tile -le $ntiles ]; do
 cp $filter_dir/oro.C${res}.tile${tile}.nc $out_dir/C${res}_oro_data.tile${tile}.nc
 cp $grid_dir/C${res}_grid.tile${tile}.nc  $out_dir/C${res}_grid.tile${tile}.nc
 tile=`expr $tile + 1 `
done
#cp $filter_dir/C${res}_mosaic.nc $out_dir/grid_spec.nc
cp $filter_dir/C${res}_mosaic.nc $out_dir/C${res}_mosaic.nc

exit









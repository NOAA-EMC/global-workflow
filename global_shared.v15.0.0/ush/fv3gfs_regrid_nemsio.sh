#!/bin/ksh
#BSUB -L /bin/sh
#BSUB -P FV3GFS-T2O
#BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/prtest/log.regrid
#BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/prtest/log.regrid
#BSUB -J regrid_fv3 
#BSUB -q dev
#BSUB -M 1024
#BSUB -x
#BSUB -W 10:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang/prtest
#BSUB -extsched 'CRAYLINUX[]'
set -ax

#---------------------------------------------------------------------------------------
#-- Remap FV3GFS forecasts on six tiles in netCDF to a global array on Gaussian grid 
#   and write out output in nemsio.  Regrid_nemsio.fd is provided by Jeff Whitaker at ESRL.
#-- Fanglin Yang, January 2017
#---------------------------------------------------------------------------------------
##. $MODULESHOME/init/sh 2>>/dev/null
##module load PrgEnv-intel cray-mpich 2>>/dev/null

export CDATE=${CDATE:-2017011500}
export CASE=${CASE:-C768}                 ;#C48 C96 C192 C384 C768 C1152 C3072
export GG=gaussian                        ;#gaussian or regular lat-lon     
export NLAT=${NLAT:-1536}
export NLON=${NLON:-3072}
export truncation=${JCAP:-$((`echo $CASE|cut -c 2-`*2-2))}
export out2dname=${out2dname:-fln$CDATE.$CDUMP}
export out3dname=${out3dname:-gfn$CDATE.$CDUMP}
export debug_regrid=${debug_regrid:-T}

export PSLOT=${PSLOT:-test}
export PTMP=${PTMP:-/gpfs/hps/ptmp}
export COMROT=${COMROT:-$PTMP/$LOGNAME/pr${PSLOT}}                  
export BASE_GSM=${BASE_GSM:-/gpfs/hps/emc/global/noscrub/$LOGNAME/svn/fv3gfs/trunk_r86472/global_shared.v15.0.0}
export REGRID_NEMSIO=${REGRID_NEMSIO:-$BASE_GSM/exec/regrid_nemsio}
export FIX_AM=${FIX_AM:-$BASE_GSM/fix/fix_am}

#--------------------------------------------------
#-- ESMF regrid weights and output variable table
export weight_bilinear=${weight_bilinear:-$BASE_GSM/fix/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${NLON}_lat${NLAT}.${GG}.bilinear.nc}
export weight_neareststod=${weight_neareststod:-$BASE_GSM/fix/$CASE/fv3_SCRIP_${CASE}_GRIDSPEC_lon${NLON}_lat${NLAT}.${GG}.neareststod.nc}
export variable_table=${variable_table:-$BASE_GSM/parm/parm_fv3diag/variable_table.txt}

export npe_regrid=${npe_regrid:-${LEVS:-64}}
export pe_node=${pe_node:-24}
export thread_regrid=${thread_regrid:-1}
export npe_node_regrid=$((pe_node/thread_regrid))
export xnodes=$(echo $npe_regrid $thread_regrid $pe_node|awk '{printf "%i", $1*$2/$3}')
export NODES=3
export APRUN_LOC="aprun -n $npe_regrid -N $npe_node_regrid -j 1 -d $thread_regrid -cc depth"
export APRUN_REGRID=${APRUN_REGRID:-$APRUN_LOC}

#--------------------------------------------------
cd $COMROT ||exit 8
err=0

rm -f regrid-nemsio.input
cat >regrid-nemsio.input <<EOF
&share
debug=$debug_regrid,nlons=$NLON,nlats=$NLAT,ntrunc=$truncation,
datapathout2d='$out2dname',
datapathout3d='$out3dname',
analysis_filename='${CDATE}0000.fv3_history.tile1.nc','${CDATE}0000.fv3_history.tile2.nc','${CDATE}0000.fv3_history.tile3.nc','${CDATE}0000.fv3_history.tile4.nc','${CDATE}0000.fv3_history.tile5.nc','${CDATE}0000.fv3_history.tile6.nc',
analysis_filename2d='${CDATE}0000.fv3_history2d.tile1.nc','${CDATE}0000.fv3_history2d.tile2.nc','${CDATE}0000.fv3_history2d.tile3.nc','${CDATE}0000.fv3_history2d.tile4.nc','${CDATE}0000.fv3_history2d.tile5.nc','${CDATE}0000.fv3_history2d.tile6.nc',
forecast_timestamp='${CDATE}'
variable_table='$variable_table'
nemsio_opt3d='bin4'
nemsio_opt2d='bin4'
/
&interpio
esmf_bilinear_filename='$weight_bilinear'
esmf_neareststod_filename='$weight_neareststod'
gfs_hyblevs_filename='${FIX_AM}/global_hyblev.l${LEVS}.txt'
/
EOF

$APRUN_REGRID $REGRID_NEMSIO
err=$?

echo $(date) EXITING $0 with return code $err >&2
exit $err


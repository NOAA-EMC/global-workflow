#!/bin/bash

###############################################################
# Source relevant configs
configs="base"
for config in $configs; do
    . $EXPDIR/config.${config}
    status=$?
    [[ $status -ne 0 ]] && exit $status
done

# initialize
AERO_DIR=${HOMEgfs}/sorc/aeroconv
module purge
module load intel/18.0.5.274
module load hdf5/1.10.5
module load netcdf/4.7.0
module load grib_api/1.26.1
module load contrib
module load anaconda/2.3.0
module load nco/4.7.0
module load ncl/6.5.0
export LD_PRELOAD=$AERO_DIR/thirdparty/lib/libjpeg.so
export PYTHONPATH=$AERO_DIR/thirdparty/lib/python2.7/site-packages:$PYTHONPATH

module use -a /scratch1/BMC/gmtb/software/modulefiles/intel-18.0.5.274/impi-2018.0.4
module load cdo/1.7.2

echo
echo "FIXfv3 = $FIXfv3"
echo "CDATE = $CDATE"
echo "CDUMP = $CDUMP"
echo "CASE = $CASE"
echo "AERO_DIR = $AERO_DIR"
echo "FV3ICS_DIR = $FV3ICS_DIR"
echo

# Temporary runtime directory
export DATA="$DATAROOT/aerofv3ic$$"
[[ -d $DATA ]] && rm -rf $DATA
mkdir -p $DATA/INPUT
cd $DATA

export OUTDIR="$ICSDIR/$CDATE/$CDUMP/$CASE/INPUT"

# link files
ln -sf ${FV3ICS_DIR}/gfs_ctrl.nc INPUT       
ln -sf ${FV3ICS_DIR}/gfs_data.tile1.nc INPUT 
ln -sf ${FV3ICS_DIR}/gfs_data.tile2.nc INPUT 
ln -sf ${FV3ICS_DIR}/gfs_data.tile3.nc INPUT 
ln -sf ${FV3ICS_DIR}/gfs_data.tile4.nc INPUT 
ln -sf ${FV3ICS_DIR}/gfs_data.tile5.nc INPUT 
ln -sf ${FV3ICS_DIR}/gfs_data.tile6.nc INPUT 
ln -sf ${FIXfv3}/${CASE}/${CASE}_grid_spec.tile1.nc INPUT/grid_spec.tile1.nc
ln -sf ${FIXfv3}/${CASE}/${CASE}_grid_spec.tile2.nc INPUT/grid_spec.tile2.nc
ln -sf ${FIXfv3}/${CASE}/${CASE}_grid_spec.tile3.nc INPUT/grid_spec.tile3.nc
ln -sf ${FIXfv3}/${CASE}/${CASE}_grid_spec.tile4.nc INPUT/grid_spec.tile4.nc
ln -sf ${FIXfv3}/${CASE}/${CASE}_grid_spec.tile5.nc INPUT/grid_spec.tile5.nc
ln -sf ${FIXfv3}/${CASE}/${CASE}_grid_spec.tile6.nc INPUT/grid_spec.tile6.nc
ln -sf ${AERO_DIR}/INPUT/QNWFA_QNIFA_SIGMA_MONTHLY.dat.nc INPUT

cp ${AERO_DIR}/int2nc_to_nggps_ic_* ./ 

yyyymmdd=`echo $CDATE | cut -c1-8`
./int2nc_to_nggps_ic_batch.sh $yyyymmdd

# Move output data
echo "copying updated files to $FV3ICS_DIR...."
cp -p $DATA/OUTPUT/gfs*nc $FV3ICS_DIR

###############################################################
# Force Exit out cleanly
if [ ${KEEPDATA:-"NO"} = "NO" ] ; then rm -rf $DATA ; fi
exit 0

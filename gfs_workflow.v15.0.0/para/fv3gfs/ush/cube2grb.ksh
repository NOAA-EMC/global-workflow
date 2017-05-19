#!/bin/ksh -x
###############################################################
# < next few lines under version control, D O  N O T  E D I T >
# $Date$
# $Revision$
# $Author$
# $Id$
###############################################################

###############################################################
# Standalone script to convert cube-sphere output to grib2
# Author: Rahul Mahajan  Org: NCEP/EMC  Date: April 12, 2017
###############################################################

###############################################################
# Variables that change often
# Data directory, date, resolution ...
# CASE   : case resolution
# LEVS   : no. of vertical levels (layers+1)
# CDATE  : datestring that the forecast was initialized
# COMROT : directory containing the netCDF tile files

# The netCDF files must have the following naming convention (shown for tile 1):
# ${CDATE}0000.atmos_4xdaily.tile1.nc
# ${CDATE}0000.atmos_static.tile1.nc
# ${CDATE}0000.grid_spec.tile1.nc
# ${CDATE}0000.nggps2d.tile1.nc
# ${CDATE}0000.nggps3d.tile1.nc
# These files were created using the following diag_table:
# $SVN_FV3GFS/trunk/global_shared.v15.0.0/parm/parm_fv3diag/diag_table
###############################################################

export CASE="C192"
export LEVS=64
export CDATE="2016100118"
export COMROT="/scratch3/NCEPDEV/stmp2/Rahul.Mahajan/ROTDIRS/FV3e1/gfs.20161001/18"

export machine="THEIA"

# Define appropriate job card based on machine
# Resources below are sufficient for C192 on Theia and WCOSS-Cray
export npe_post=72
export npe_node_post=12
export memory_post="3072M"

###############################################################
# The following section sets up the runtime environment,
# paths to scripts and executables required for running a
# standalone post on the forecast files
# Variables that change less often
# Builds, scripts, executables, etc ...
###############################################################

# Load modules specific on WCOSS-Cray
if [ $machine = "WCOSS_C" ]; then

    . $MODULESHOME/init/ksh >> /dev/null 2>&1
    module load PrgEnv-intel >> /dev/null 2>&1
    module load cray-mpich >> /dev/null 2>&1
    module load hpss >> /dev/null 2>&1
    module load g2tmpl-intel/1.4.0 >> /dev/null 2>&1

fi

# GLOBAL static environment parameters
if [ $machine = "THEIA" ]; then

    export NWPROD="/scratch4/NCEPDEV/global/save/glopara/nwpara"
    export STMP="/scratch3/NCEPDEV/stmp1/$USER"
    export BASE_SVN="/scratch4/NCEPDEV/global/save/Rahul.Mahajan"

elif [ $machine = "WCOSS_C" ]; then

    export NWPROD="/gpfs/hps/nco/ops/nwprod"
    export STMP="/gpfs/hps/stmp/$USER"
    export BASE_SVN="/gpfs/hps/emc/global/noscrub/Rahul.Mahajan/svn"

fi

# Utilities needed in the post scripts
if [ $machine = "THEIA" ]; then

    export NDATE="$NWPROD/util/exec/ndate"
    export NHOUR="$NWPROD/util/exec/nhour"
    export WGRIB="$NWPROD/util/exec/wgrib"
    export WGRIB2="$NWPROD/util/exec/wgrib2"
    export COPYGB="$NWPROD/util/exec/copygb"
    export COPYGB2="$NWPROD/util/exec/copygb2"
    export GRBINDEX="$NWPROD/util/exec/grbindex"
    export GRB2INDEX="$NWPROD/util/exec/grb2index"
    export GRBINDEX2="$NWPROD/util/exec/grb2index"
    export CNVGRIB="/apps/cnvgrib/1.4.0/bin/cnvgrib"

elif [ $machine = "WCOSS_C" ]; then

    module load grib_util/1.0.3 >> /dev/null 2>&1
    module load prod_util/1.0.7 >> /dev/null 2>&1

fi

# Post requires grib2 table
if [ $machine = "THEIA" ]; then
    export POSTGRB2TBL="/scratch3/NCEPDEV/nwprod/lib/sorc/g2tmpl/params_grib2_tbl_new"
elif [ $machine = "WCOSS_C" ]; then
    export POSTGRB2TBL="/gpfs/hps/nco/ops/nwprod/lib/g2tmpl/v1.3.0/src/params_grib2_tbl_new"
fi

# Build paths relative to $BASE_SVN
export BASE_WORKFLOW="$BASE_SVN/fv3gfs/branches/Rahul.Mahajan/EXP-cyc/gfs_workflow.v15.0.0/para"
export BASE_GSM="$BASE_SVN/fv3gfs/branches/Rahul.Mahajan/EXP-cyc/global_shared.v15.0.0"
export BASE_POST="$BASE_SVN/fv3gfs/branches/Rahul.Mahajan/EXP-fv3gfspost/post"

# CONVENIENT utility scripts and other environment parameters
export NCP="/bin/cp -p"
export NMV="/bin/mv"
export NLN="/bin/ln -sf"
export VERBOSE="YES"
export KEEPDATA="YES"
export NCO_NAMING_CONV="YES"

# Remap cube-sphere tiles to global LatLon (using fregrid)
export REMAPSH="$BASE_GSM/ush/fv3gfs_remap.sh"
export master_grid="0p5deg" # 1deg 0p5deg 0p25deg 0p125deg etc
export npe_remap=$npe_post
export nth_remap=2

# Global LatLon NetCDF to nemsio
export NC2NEMSIOSH="$BASE_GSM/ush/fv3gfs_nc2nemsio.sh"

# Convert nemsio files to grib files using post job
# Post driver job that calls global_nceppost.sh and downstream jobs
export POSTJJOBSH="$BASE_WORKFLOW/jobs/JGFS_POST.sh"

# Produce master grib2 file
export POSTGPSH="$BASE_POST/ush/global_nceppost.sh"
export POSTGPEXEC="$BASE_POST/exec/ncep_post"
export npe_postgp=$npe_post
export nth_postgp=1

# Downstream grib files at different resolutions, some in Grib1 format
export GFS_DOWNSTREAM="YES"
export GFSDOWNSH="$BASE_WORKFLOW/ush/gfs_downstream_nems.sh"
export GFSDWNSH="$BASE_WORKFLOW/ush/gfs_dwn_nems.sh"
export downset=1
export npe_dwn=$npe_post
export nth_dwn=2

###############################################################
# All definitions are supplied above this block
# Now do the work, abort if any step fails
# Workflow does not really change frequently
###############################################################

# Source machine runtime environment
. $BASE_WORKFLOW/fv3gfs/env/$machine.env post
status=$?
[[ $status -ne 0 ]] && exit $status

res=`echo $CASE | cut -c2-`
export JCAP=`echo "$res * 2 - 2" | bc`
export LONB=`echo "$res * 4" | bc`
export LATB=`expr "$res * 2" | bc`

export CDUMP="gdas"
export pgmout="/dev/null" # exgfs_nceppost.sh fails without this

# Remap 6-tile output to global array in NetCDF latlon
$REMAPSH
status=$?
[[ $status -ne 0 ]] && exit $status

# Convert NetCDF to nemsio
$NC2NEMSIOSH
status=$?
[[ $status -ne 0 ]] && exit $status

# Run post job to create forecast grib files
export DATA=$STMP/post$$
$POSTJJOBSH
status=$?
[[ $status -ne 0 ]] && exit $status

exit 0

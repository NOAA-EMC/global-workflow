#!/bin/sh 
#BSUB -L /bin/sh
#BSUB -P GFS-T2O
#BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/fv3/log.remap
#BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/fv3/log.remap
#BSUB -J remap_fv3
#BSUB -q dev
#BSUB -M 3072
#BSUN -n 1
##BSUB -x
#BSUB -W 10:00
#BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang
#BSUB -extsched 'CRAYLINUX[]'
set -ax

#-------------------------------------
for CASE in C48 C96 C192 C384 C768 ;do
#-------------------------------------

#export CASE=C768
export CDATE=2016100300

#--global lat-lon array size
#export GG=1deg;    export nlon=360 ;  export nlat=180
#export GG=0p5deg;  export nlon=720 ;  export nlat=360
export GG=0p25deg; export nlon=1440 ; export nlat=720

#--------------------------------------------------
 . $MODULESHOME/init/sh
module load PrgEnv-intel
export NODES=1

export PTMP=/gpfs/hps/ptmp
export expdir=$PTMP/$LOGNAME/fv3/${CASE}_${CDATE}
export arcdir=$PTMP/$LOGNAME/fv3/archive
mkdir -p $arcdir

#--------------------------------------------------
export home_dir=/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS
export script_dir=$home_dir/ush
export exec_dir=$home_dir/exec
export fix_fv3_dir=$home_dir/fix
export fregrid=$home_dir/exec/fregrid
export TMPDIR=/gpfs/hps/ptmp/$LOGNAME/fv3_weight

export grid_loc=$fix_fv3_dir/$CASE/${CASE}_mosaic.nc
export weight_file=$fix_fv3_dir/$CASE/remap_weights_${CASE}_${GG}.nc

#export APRUN="aprun -n 1 -N 1 -j 1 -d 24 -cc depth"
export APRUN="aprun -n 1 -N 1 -j 1 -d 1 -cc depth"
#--------------------------------------------------
cd $expdir ||exit 8

export atmos_4xdaily_s="slp, vort850, vort200, tm, t1000, t850, t700, t500, t200, t100, t50, \
             h1000, h850, h700, h500, h200, h100, \
             w850, w500, w200, q1000, q850, q700, q500, \
             rh1000, rh850, rh700, rh500, omg1000, omg850, omg700, omg500,u50"
export atmos_4xdaily_u="us, u1000, u850, u700, u500, u200, u100"
export atmos_4xdaily_v="vs, v1000, v850, v700, v500, v200, v100 "

export nggps2d_s="CPRATsfc, PRATEsfc, HGTsfc, HPBLsfc, LHTFLsfc, SHTFLsfc, SOILM, SPFH2m, TMP2m, TMPsfc, SNODsfc"
export nggps2d_u="UGRD10m"
export nggps2d_v="VGRD10m"

export nggps3d_4xdaily_s="ucomp, vcomp, temp, delp, sphum, o3mr, clwmr, nhpres, w, delz"
export nggps3d_4xdaily_u=""
export nggps3d_4xdaily_v=""
#--------------------------------------------------


#for type in atmos_4xdaily atmos_daily atmos_sos nggps2d nggps3d_4xdaily ; do
for type in atmos_4xdaily nggps2d nggps3d_4xdaily ; do

export in_file="${CDATE}0000.${type}"
export out_file=${CASE}_${CDATE}.${type}.${GG}.nc
if [ -s $arcdir/$out_file ]; then rm -f $arcdir/$out_file ; fi
export fld=$(eval echo \${${type}_s})
export u_fld=$(eval echo \${${type}_u})
export v_fld=$(eval echo \${${type}_v})


if [ "$u_fld" = "" -o "$v_fld" = "" ]; then
 $APRUN $fregrid --input_dir $expdir \
                --input_file $in_file \
                --output_dir $arcdir \
                --output_file $out_file \
                --input_mosaic $grid_loc \
                --scalar_field "$fld" \
                --interp_method conserve_order1 \
                --remap_file $weight_file \
                --nlon $nlon --nlat $nlat
else
 $APRUN $fregrid --input_dir $expdir \
                --input_file $in_file \
                --output_dir $arcdir \
                --output_file $out_file \
                --input_mosaic $grid_loc \
                --scalar_field "$fld" \
                --u_field "$u_fld" \
                --v_field "$v_fld" \
                --interp_method conserve_order1 \
                --remap_file $weight_file \
                --nlon $nlon --nlat $nlat
fi

done

#----------------
done  #CASES
#----------------

exit

##--interp_method :conserve_order1, conserve_order2, bilinear

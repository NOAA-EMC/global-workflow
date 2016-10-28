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

 . $MODULESHOME/init/sh
module load PrgEnv-intel
export PTMP=/gpfs/hps/ptmp
export arcdir=$PTMP/$LOGNAME/fv3/archive
mkdir -p $arcdir

export CDATE=2016100300
export CASE=C192      ;#C48 C96 C192 C384 C768 C1152 C3072


export max_core=24
export NODES=10
export thread=2
if [ $CASE = C3072 ]; then export thread=4; fi
export npe_node=$((max_core/thread))
export npes=$((NODES*npe_node))

#export APRUN="aprun -n 1 -N 1 -j 1 -d 24 -cc depth"
export APRUN="aprun -n $npes -N $npe_node -j 1 -d $thread -cc depth"


#----------------------------------------------------------
for GG in 1deg 0p5deg 0p25deg 0p125deg; do
#----------------------------------------------------------
if [ $GG = 1deg    ];  then  export nlon=360 ;  export nlat=180 ;fi
if [ $GG = 0p5deg  ];  then  export nlon=720 ;  export nlat=360 ;fi
if [ $GG = 0p25deg ];  then  export nlon=1440 ; export nlat=720 ;fi
if [ $GG = 0p125deg ]; then  export nlon=2880 ; export nlat=1440 ;fi


#--------------------------------------------------
export expdir=$PTMP/$LOGNAME/fv3/${CASE}_${CDATE}
export home_dir=/gpfs/hps/emc/global/noscrub/$LOGNAME/NGGPS
export script_dir=$home_dir/ush
export exec_dir=$home_dir/exec
export fix_fv3_dir=$home_dir/fix
export fregrid=$home_dir/exec/fregrid_parallel
export TMPDIR=/gpfs/hps/ptmp/$LOGNAME/fv3_weight

export grid_loc=$fix_fv3_dir/$CASE/${CASE}_mosaic.nc
export weight_file=$fix_fv3_dir/$CASE/remap_weights_${CASE}_${GG}.nc

#--------------------------------------------------
cd $expdir ||exit 8

export atmos_4xdaily_s="slp, vort850, vort200, tm, \
             t1000, t850, t700, t500, t200, t100, t50, t10,\
             h1000, h850, h700, h500, h200, h100, h50, h10,\
             w850, w700, w500, w200, \
             q1000, q850, q700, q500, q200, q100, q50, q10,\
             rh1000, rh850, rh700, rh500, rh200, \
             omg1000, omg850, omg700, omg500, omg200, omg100, omg50, omg10, \
             us, u1000, u850, u700, u500, u200, u100, u50, u10, \
             vs, v1000, v850, v700, v500, v200, v100, v50, v10"

export nggps2d_s="ALBDOsfc, CPRATsfc, PRATEsfc, DLWRFsfc, ULWRFsfc, DSWRFsfc,\
         USWRFsfc, DSWRFtoa, USWRFtoa, ULWRFtoa, GFLUXsfc, HGTsfc,\
         HPBLsfc, ICECsfc, SLMSKsfc, LHTFLsfc, SHTFLsfc, PRESsfc,\
         PWATclm, SOILM, SOILW1, SOILW2, SOILW3, SOILW4, SPFH2m,\
         TCDCclm, TCDChcl, TCDClcl, TCDCmcl,\
         SOILT1, SOILT2, SOILT3, SOILT4,\
         TMP2m, TMPsfc, UGWDsfc, VGWDsfc, UFLXsfc, VFLXsfc,\
         WEASDsfc, SNODsfc, ZORLsfc, VFRACsfc, F10Msfc,\
         VTYPEsfc, STYPEsfc, UGRD10m, VGRD10m"

export nggps3d_4xdaily_s="ucomp, vcomp, temp, delp, sphum, o3mr, clwmr, nhpres, w, delz"
#--------------------------------------------------


for type in atmos_4xdaily nggps2d nggps3d_4xdaily ; do

export in_file="${CDATE}0000.${type}"
export out_file=${CASE}_${CDATE}.${type}.${GG}.nc
if [ -s $arcdir/$out_file ]; then rm -f $arcdir/$out_file ; fi
export fld=$(eval echo \${${type}_s})


 $APRUN $fregrid --input_dir $expdir \
                --input_file $in_file \
                --output_dir $arcdir \
                --output_file $out_file \
                --input_mosaic $grid_loc \
                --scalar_field "$fld" \
                --interp_method conserve_order1 \
                --remap_file $weight_file \
                --nlon $nlon --nlat $nlat
done

#----------------
done  #lat-lon grid
#----------------

exit

##--interp_method :conserve_order1, conserve_order2, bilinear

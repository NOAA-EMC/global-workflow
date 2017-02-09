#!/bin/ksh
###BSUB -L /bin/sh
###BSUB -P GFS-T2O
###BSUB -oo /gpfs/hps/ptmp/Fanglin.Yang/log.remap
###BSUB -eo /gpfs/hps/ptmp/Fanglin.Yang/log.remap
###BSUB -J remap_fv3
###BSUB -q dev
###BSUB -M 1536
####BSUB -x
###BSUB -W 10:00
###BSUB -cwd /gpfs/hps/ptmp/Fanglin.Yang
###BSUB -extsched 'CRAYLINUX[]'
set -ax

#--------------------------------------
#-- remap FV3 6 tiles to global array
#-- Fanglin Yang, October 2016
#--------------------------------------
##. $MODULESHOME/init/sh 2>>/dev/null
##module load PrgEnv-intel 2>>/dev/null

export CDATE=${CDATE:-2016100300}
export CASE=${CASE:-C192}                 ;#C48 C96 C192 C384 C768 C1152 C3072
export GG=${master_grid:-0p25deg}         ;#1deg 0p5deg 0p25deg 0p125deg     

export PSLOT=${PSLOT:-fv3gfs}
export PTMP=${PTMP:-/gpfs/hps/ptmp}
export COMROT=${COMROT:-$PTMP/$LOGNAME/pr${PSLOT}}                  
export BASE_GSM=${BASE_GSM:-/gpfs/hps/emc/global/noscrub/Fanglin.Yang/svn/gfs/fv3gfs/global_shared.v15.0.0}
export REMAPEXE=${REMAPEXE:-$BASE_GSM/exec/fregrid_parallel}

#--------------------------------------------------
export grid_loc=$BASE_GSM/fix/$CASE/${CASE}_mosaic.nc
export weight_file=$BASE_GSM/fix/$CASE/remap_weights_${CASE}_${GG}.nc

export NODES=${NODES:-3}
export pe_node=${pe_node:-24}
export thread_remap=${thread_remap:-2}
export npe_node_remap=$((pe_node/thread_remap))
export npe_remap=$((NODES*npe_node_remap))
export APRUN_LOC="aprun -n $npe_remap -N $npe_node_remap -j 1 -d $thread_remap -cc depth"
export APRUN_REMAP=${APRUN_REMAP:-$APRUN_LOC}

if [ $GG = 1deg    ];  then  export nlon=360 ;  export nlat=180 ;fi
if [ $GG = 0p5deg  ];  then  export nlon=720 ;  export nlat=360 ;fi
if [ $GG = 0p25deg ];  then  export nlon=1440 ; export nlat=720 ;fi
if [ $GG = 0p125deg ]; then  export nlon=2880 ; export nlat=1440 ;fi
#--------------------------------------------------
cd $COMROT ||exit 8
err=0

#--for non-hydrostatic case
export atmos_4xdaily_nh="slp, vort850, vort200,\
        us, u1000, u850, u700, u500, u200, u100, u50, u10,\
        vs, v1000, v850, v700, v500, v200, v100, v50, v10,\
        tm, t1000, t850, t700, t500, t200, t100, t50, t10,\
        h1000, h850, h700, h500, h200, h100, h50, h10,\
        q1000, q850, q700, q500, q200, q100, q50, q10,\
        rh1000, rh850, rh700, rh500, rh200,\
        omg1000, omg850, omg700, omg500, omg200, omg100, omg50, omg10,\
        w700,w850,w500, w200" 

#--for hydrostatic case
export atmos_4xdaily_hy="slp, vort850, vort200,\
        us, u1000, u850, u700, u500, u200, u100, u50, u10,\
        vs, v1000, v850, v700, v500, v200, v100, v50, v10,\
        tm, t1000, t850, t700, t500, t200, t100, t50, t10,\
        h1000, h850, h700, h500, h200, h100, h50, h10,\
        q1000, q850, q700, q500, q200, q100, q50, q10,\
        rh1000, rh850, rh700, rh500, rh200,\
        omg1000, omg850, omg700, omg500, omg200, omg100, omg50, omg10,\
        w700"                     

export nggps2d_nh="ALBDOsfc, CPRATsfc, PRATEsfc, DLWRFsfc, ULWRFsfc,\
        DSWRFsfc, USWRFsfc, DSWRFtoa, USWRFtoa, ULWRFtoa,\
        GFLUXsfc, HGTsfc, HPBLsfc, ICECsfc, SLMSKsfc,\
        LHTFLsfc, SHTFLsfc, PRESsfc, PWATclm, SOILM,\
        SOILW1, SOILW2, SOILW3, SOILW4, SPFH2m,\
        TCDCclm, TCDChcl, TCDClcl, TCDCmcl,\
        SOILT1, SOILT2, SOILT3, SOILT4,\
        TMP2m, TMPsfc, UGWDsfc, VGWDsfc, UFLXsfc,\
        VFLXsfc, UGRD10m, VGRD10m, WEASDsfc, SNODsfc,\
        ZORLsfc, VFRACsfc, F10Msfc, VTYPEsfc, STYPEsfc"
export nggps2d_hy="$nggps2d_nh"


export nggps3d_nh="ucomp, vcomp, temp, delp, sphum, o3mr, clwmr, nhpres, w, delz"     #for non-hydrostatic case
export nggps3d_hy="ucomp, vcomp, temp, delp, sphum, o3mr, clwmr, hypres"              #for hydrostatic case

nhrun=$(ncdump -c ${CDATE}0000.nggps3d.tile4.nc |grep nhpres) 
testout=$?
#--------------------------------------------------

for type in atmos_4xdaily nggps2d nggps3d ; do

export in_file="${CDATE}0000.${type}"
export out_file=${CASE}_${CDATE}.${type}.${GG}.nc
if [ -s $COMROT/$out_file ]; then rm -f $COMROT/$out_file ; fi
if [ $testout -eq 0 ]; then
 export fld=$(eval echo \${${type}_nh})
else
 export fld=$(eval echo \${${type}_hy})
fi


 $APRUN_REMAP $REMAPEXE --input_dir $COMROT \
                --input_file $in_file \
                --output_dir $COMROT \
                --output_file $out_file \
                --input_mosaic $grid_loc \
                --scalar_field "$fld" \
                --interp_method conserve_order1 \
                --remap_file $weight_file \
                --nlon $nlon --nlat $nlat
 err=$?
done

echo $(date) EXITING $0 with return code $err >&2
exit $err


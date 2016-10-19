#!/bin/ksh 
set -ax

export type=$1   # GFS or SFC

if [ $type == GFS ]; then
  if [ $# -ne 7 ]; then
     echo "when type is GFS, number of arguments must be 7"
     echo "usage: $0 GFS resolution grid_dir output_dir date gfs_dir grid_type "
     exit 1
  fi
  export res=$2  
  export griddir=$3
  export outdir=$4  
  export d=$5
  export gfs_dir=$6
  export gtype=$7

elif [ $type = SFC ]; then
  if [ $# -ne 7 ]; then
     echo "when type is SFC, number of arguments must be 7"
     echo "usage: $0 SFC resolution tile grid_dir output_dir date gfs_dir "
     exit 1
  fi
  export res=$2  
  export tile=$3   
  export griddir=$4
  export outdir=$5 
  export d=$6
  export gfs_dir=$7

else
  echo "first arugment (type) must be GFS or SFC"
  exit 1
fi

if [ $type == GFS ]; then
  if [ $gtype = stretch ]; then
    export LONB=3600
    export LATB=1800 
  else
    lon=$((res*4))
    lat=$((res*2))
    export LONB=$lon
    export LATB=$lat
    echo "LONB = $LONB, LATB = $LATB"  
  fi
else
  export LONB=$res
  export LATB=$res
fi

export JCAP=$(($LATB-2))
export LEVS=64
export IDVC=2
export OUTTYP=1
export IDRT=4
export LANDICE_OPT=2
export CLIMO_FIELDS_OPT=3
export NTRAC=3
export IDSL=1
export LSOIL=0
export IVSSFC=0
if [ $type = "GFS" ]; then
  export CHGRESVARS="use_ufo=.true.,IALB=0,ntrac=3,idvc=2,idvt=21,idsl=1,IDVM=1,make_sfc=.false."
else
  export CHGRESVARS="use_ufo=.true.,IALB=0,ntrac=3,idvc=2,idvt=21,idsl=1,IDVM=1,make_sfc=.true."
fi

export executable=$exec_dir/global_chgres

if [ ! -s $executable ]; then
  echo "executable does not exist"
  exit 1 
fi

export FIXGLOBAL=${fix_gsm_dir:-$NWROOTp2/global_shared.v13.0.3/fix/fix_am}
export workdir=${TMPDIR:-/ptmp/$LOGNAME/fv3_chgres}
cd $workdir ||exit 8

cp $executable .
export day=`echo $d | cut -c1-8`
export hr=`echo $d | cut -c9-10`

#ln -sf ${gfs_dir}/siganl.gdas.${day}.${hr} chgres.inp.sig
#ln -sf ${gfs_dir}/sfcanl.gdas.${day}.${hr} chgres.inp.sfc

ln -sf ${gfs_dir}/gfs.t${hr}z.sanl chgres.inp.sig
ln -sf ${gfs_dir}/gfs.t${hr}z.sfcanl chgres.inp.sfc
ln -sf $FIXGLOBAL/global_hyblev.l64.txt chgres.inp.siglevel
ln -sf $FIXGLOBAL/global_o3clim.txt chgres.inp.o3clim

export FNGLAC=$FIXGLOBAL/global_glacier.2x2.grb
export FNMXIC=${FIXGLOBAL}/global_maxice.2x2.grb
export FNTSFC=${FIXGLOBAL}/cfs_oi2sst1x1monclim19822001.grb
export FNSNOC=${FIXGLOBAL}/global_snoclim.1.875.grb
export FNZORC=sib
export FNALBC=${FIXGLOBAL}/global_albedo4.1x1.grb
export FNAISC=${FIXGLOBAL}/cfs_ice1x1monclim19822001.grb
export FNTG3C=${FIXGLOBAL}/global_tg3clim.2.6x1.5.grb
export FNVEGC=${FIXGLOBAL}/global_vegfrac.0.144.decpercent.grb
export FNVETC=${FIXGLOBAL}/global_vegtype.1x1.grb
export FNSOTC=${FIXGLOBAL}/global_soiltype.1x1.grb
export FNSMCC=${FIXGLOBAL}/global_soilmcpc.1x1.grb
export FNVMNC=${FIXGLOBAL}/global_shdmin.0.144x0.144.grb
export FNVMXC=${FIXGLOBAL}/global_shdmax.0.144x0.144.grb
export FNSLPC=${FIXGLOBAL}/global_slope.1x1.grb
export FNABSC=${FIXGLOBAL}/global_snoalb.1x1.grb
export FNMSKH=${FIXGLOBAL}/seaice_newland.grb


if [ $type = "GFS" ]; then
  export OUTGRID="${griddir}/C${res}_grid"
  export OUTOROG=""
else
  export OUTGRID="${griddir}/C${res}_grid.tile$tile.nc"
  export OUTOROG="${griddir}/C${res}_oro_data.tile$tile.nc"
fi
export ntiles=${ntiles:-6}

if [ $LANDICE_OPT -eq 3 -o $LANDICE_OPT -eq 4 ]; then
  export LANDICE=.false.
else
  export LANDICE=.true.
fi

cat << EOF > fort.35
 &NAMSFC
  FNGLAC='${FNGLAC}'
  FNMXIC='${FNMXIC}'
  FNTSFC='${FNTSFC}'
  FNSNOC='${FNSNOC}'
  FNZORC='${FNZORC}'
  FNALBC='${FNALBC}'
  FNAISC='${FNAISC}'
  FNTG3C='${FNTG3C}'
  FNVEGC='${FNVEGC}'
  FNVETC='${FNVETC}'
  FNSOTC='${FNSOTC}'
  FNSMCC='${FNSMCC}'
  FNVMNC='${FNVMNC}'
  FNVMXC='${FNVMXC}'
  FNSLPC='${FNSLPC}'
  FNABSC='${FNABSC}'
  FNMSKH='${FNMSKH}'
  FNTSFA=''
  FNACNA=''
  FNSNOA=''
  LDEBUG=.false.
  LANDICE=$LANDICE
/
EOF

cat << EOF > fort.81
 &soil_parameters
  soil_src_input = "zobler"
  smclow_input  = 0.5
  smchigh_input = 6.0
  smcmax_input= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                0.404, 0.439, 0.421
  beta_input  =   4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_input  =   0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_input = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
  soil_src_output = "zobler"
  smclow_output  = 0.5
  smchigh_output = 6.0
  smcmax_output= 0.421, 0.464, 0.468, 0.434, 0.406, 0.465,
                 0.404, 0.439, 0.421
  beta_output  =  4.26,  8.72, 11.55,  4.74, 10.73,  8.17,
                  6.77,  5.25,  4.26
  psis_output  =  0.040, 0.620, 0.470, 0.140, 0.100, 0.260,
                  0.140, 0.360, 0.040
  satdk_output = 1.41e-5, 0.20e-5, 0.10e-5, 0.52e-5, 0.72e-5,
                 0.25e-5, 0.45e-5, 0.34e-5, 1.41e-5
 /
 &veg_parameters
  veg_src_input = "sib"
  veg_src_output = "sib"
  salp_output= -999.
  snup_output= -999.
 /
 &options
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT}
  LANDICE_OPT=${LANDICE_OPT}
 /
EOF
 $APRUNC global_chgres <<EOF '1>&1' '2>&2'
  &NAMCHG JCAP=$JCAP, LEVS=$LEVS, LONB=$LONB, LATB=$LATB, 
           NTRAC=$NTRAC, IDVC=$IDVC, IDSL=$IDSL,
           LSOIL=$LSOIL, IVSSFC=$IVSSFC, OUTTYP=$OUTTYP, IDRT=$IDRT, $CHGRESVARS, 
           ntiles=$ntiles, OUTGRID="$OUTGRID", OUTOROG="$OUTOROG"
 /
EOF

if [ $? -ne 0 ];  then
   echo "ERROR in running $executable "
   exit 1
else
# copy data to output directory
   if [ $type = GFS ]; then
      tile=1
      while [ $tile -le $ntiles ]; do
         mv gfs_data.tile$tile.nc $outdir/gfs_data.tile$tile.nc
         tile=$((tile+1))
      done
      mv gfs_ctrl.nc $outdir/gfs_ctrl.nc 
      mv gfs_data.nc  $outdir
   else
      mv out.sfc.nc $outdir/sfc_data.tile$tile.nc
      # sfc_ctrl.nc file are the same for all the tiles. so only need to copy 1
      if [ $tile = 1 ]; then
       mv sfc_ctrl.nc $outdir/sfc_ctrl.nc
      fi
   fi
   echo "successfully running $executable "
   echo 0
fi

exit

#!/bin/ksh
################################################################################
# UNIX Script Documentation Block
# Script name:         exglobal_fcst_nemsfv3gfs.sh.ecf
# Script description:  Runs a global FV3GFS model forecast
#
# Author:   Fanglin Yang       Org: NCEP/EMC       Date: 2016-11-15
# Abstract: This script runs a single GFS forecast with FV3 dynamical core.
#           This script is created based on a C-shell script that GFDL wrote
#           for the NGGPS Phase-II Dycore Comparison Project.
#
# Script history log:
# 2016-11-15  Fanglin Yang   First Version.
# 2017-02-09  Rahul Mahajan  Added warm start and restructured the code.
# 2017-03-10  Fanglin Yang   Updated for running forecast on Cray.
# 2017-03-24  Fanglin Yang   Updated to use NEMS FV3GFS with IPD4
# 2017-05-24  Rahul Mahajan  Updated for cycling with NEMS FV3GFS
# 2017-09-13  Fanglin Yang   Updated for using GFDL MP and Write Component
# 2019-03-05  Rahul Mahajan  Implemented IAU
# 2019-03-21  Fanglin Yang   Add restart capability for running gfs fcst from a break point.
# 2019-12-12  Henrique Alves Added wave model blocks for coupled run
#
# $Id$
#
# Attributes:
#   Language: Portable Operating System Interface (POSIX) Shell
#   Machine: WCOSS-CRAY, Theia
################################################################################

#  Set environment.
VERBOSE=${VERBOSE:-"YES"}
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXECUTING $0 $* >&2
  set -x
fi

machine=${machine:-"WCOSS_C"}
machine=$(echo $machine | tr '[a-z]' '[A-Z]')

# Cycling and forecast hour specific parameters 
CASE=${CASE:-C768}
CDATE=${CDATE:-2017032500}
CDUMP=${CDUMP:-gdas}
FHMIN=${FHMIN:-0}
FHMAX=${FHMAX:-9}
FHOUT=${FHOUT:-3}
FHZER=${FHZER:-6}
FHCYC=${FHCYC:-24}
FHMAX_HF=${FHMAX_HF:-0}
FHOUT_HF=${FHOUT_HF:-1}
NSOUT=${NSOUT:-"-1"}
FDIAG=$FHOUT
if [ $FHMAX_HF -gt 0 -a $FHOUT_HF -gt 0 ]; then FDIAG=$FHOUT_HF; fi
WRITE_DOPOST=${WRITE_DOPOST:-".false."}
restart_interval=${restart_interval:-0}
rst_invt1=`echo $restart_interval |cut -d " " -f 1`

PDY=$(echo $CDATE | cut -c1-8)
cyc=$(echo $CDATE | cut -c9-10)

# Directories.
pwd=$(pwd)
NWPROD=${NWPROD:-${NWROOT:-$pwd}}
HOMEgfs=${HOMEgfs:-$NWPROD}
FIX_DIR=${FIX_DIR:-$HOMEgfs/fix}
FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}
FIXfv3=${FIXfv3:-$FIX_DIR/fix_fv3_gmted2010}
DATA=${DATA:-$pwd/fv3tmp$$}    # temporary running directory
ROTDIR=${ROTDIR:-$pwd}         # rotating archive directory
ICSDIR=${ICSDIR:-$pwd}         # cold start initial conditions
DMPDIR=${DMPDIR:-$pwd}         # global dumps for seaice, snow and sst analysis

# Model resolution specific parameters
DELTIM=${DELTIM:-225}
layout_x=${layout_x:-8}
layout_y=${layout_y:-16}
LEVS=${LEVS:-65}

# Utilities
NCP=${NCP:-"/bin/cp -p"}
NLN=${NLN:-"/bin/ln -sf"}
NMV=${NMV:-"/bin/mv"}
SEND=${SEND:-"YES"}   #move final result to rotating directory
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
KEEPDATA=${KEEPDATA:-"NO"}

# Other options
MEMBER=${MEMBER:-"-1"} # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER
ENS_NUM=${ENS_NUM:-1}  # Single executable runs multiple members (e.g. GEFS)

# IAU options
DOIAU=${DOIAU:-"NO"}
IAUFHRS=${IAUFHRS:-0}
IAU_DELTHRS=${IAU_DELTHRS:-0}
IAU_OFFSET=${IAU_OFFSET:-0}

# Model specific stuff
FCSTEXECDIR=${FCSTEXECDIR:-$HOMEgfs/sorc/fv3gfs.fd/NEMS/exe}
FCSTEXEC=${FCSTEXEC:-fv3_gfs.x}
PARM_FV3DIAG=${PARM_FV3DIAG:-$HOMEgfs/parm/parm_fv3diag}
PARM_POST=${PARM_POST:-$HOMEgfs/parm/post}

# Wave coupling parameter defaults to false
cplwav=${cplwav:-.false.}

# Model config options
APRUN_FV3=${APRUN_FV3:-${APRUN_FCST:-${APRUN:-""}}}
NTHREADS_FV3=${NTHREADS_FV3:-${NTHREADS_FCST:-${nth_fv3:-1}}}
cores_per_node=${cores_per_node:-${npe_node_max:-24}}
ntiles=${ntiles:-6}
NTASKS_FV3=${NTASKS_FV3:-$npe_fv3}

TYPE=${TYPE:-"nh"}                  # choices:  nh, hydro
MONO=${MONO:-"non-mono"}            # choices:  mono, non-mono

QUILTING=${QUILTING:-".true."}
OUTPUT_GRID=${OUTPUT_GRID:-"gaussian_grid"}
OUTPUT_FILE=${OUTPUT_FILE:-"nemsio"}
WRITE_NEMSIOFLIP=${WRITE_NEMSIOFLIP:-".true."}
WRITE_FSYNCFLAG=${WRITE_FSYNCFLAG:-".true."}
affix="nemsio"
[[ "$OUTPUT_FILE" = "netcdf" ]] && affix="nc"

rCDUMP=${rCDUMP:-$CDUMP}

#------------------------------------------------------------------
# setup the runtime environment
if [ $machine = "WCOSS_C" ] ; then
  HUGEPAGES=${HUGEPAGES:-hugepages4M}
  . $MODULESHOME/init/sh 2>/dev/null
  module load iobuf craype-$HUGEPAGES 2>/dev/null
  export MPICH_GNI_COLL_OPT_OFF=${MPICH_GNI_COLL_OPT_OFF:-MPI_Alltoallv}
  export MKL_CBWR=AVX2
  export WRTIOBUF=${WRTIOBUF:-"4M"}
  export NC_BLKSZ=${NC_BLKSZ:-"4M"}
  export IOBUF_PARAMS="*nemsio:verbose:size=${WRTIOBUF},*:verbose:size=${NC_BLKSZ}"
fi

#-------------------------------------------------------
if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
mkdata=NO
if [ ! -d $DATA ]; then
   mkdata=YES
   mkdir -p $DATA
fi
cd $DATA || exit 8
mkdir -p $DATA/INPUT
if [ $CDUMP = "gfs" -a $rst_invt1 -gt 0 ]; then
    RSTDIR_TMP=${RSTDIR:-$ROTDIR}/${CDUMP}.${PDY}/${cyc}/RERUN_RESTART
    if [ ! -d $RSTDIR_TMP ]; then mkdir -p $RSTDIR_TMP ; fi
    $NLN $RSTDIR_TMP RESTART
else
    mkdir -p $DATA/RESTART
fi

#-------------------------------------------------------
# determine if restart IC exists to continue from a previous forecast
RERUN="NO"
filecount=$(find $RSTDIR_TMP -type f | wc -l) 
if [ $CDUMP = "gfs" -a $rst_invt1 -gt 0 -a $FHMAX -gt $rst_invt1 -a $filecount -gt 10 ]; then
    SDATE=$($NDATE +$FHMAX $CDATE)
    EDATE=$($NDATE +$rst_invt1 $CDATE)
    while [ $SDATE -gt $EDATE ]; do
        PDYS=$(echo $SDATE | cut -c1-8)
        cycs=$(echo $SDATE | cut -c9-10)
        flag1=$RSTDIR_TMP/${PDYS}.${cycs}0000.coupler.res
        flag2=$RSTDIR_TMP/coupler.res
        if [ -s $flag1 ]; then
            mv $flag1 ${flag1}.old
            if [ -s $flag2 ]; then mv $flag2 ${flag2}.old ;fi
            RERUN="YES"
            CDATE_RST=$($NDATE -$rst_invt1 $SDATE)
            break
        fi 
        SDATE=$($NDATE -$rst_invt1 $SDATE)
    done
fi

#-------------------------------------------------------
# member directory
if [ $MEMBER -lt 0 ]; then
  prefix=$CDUMP
  rprefix=$rCDUMP
  memchar=""
else
  prefix=enkf$CDUMP
  rprefix=enkf$rCDUMP
  memchar=mem$(printf %03i $MEMBER)
fi
memdir=$ROTDIR/${prefix}.$PDY/$cyc/$memchar
if [ ! -d $memdir ]; then mkdir -p $memdir; fi

GDATE=$($NDATE -$assim_freq $CDATE)
gPDY=$(echo $GDATE | cut -c1-8)
gcyc=$(echo $GDATE | cut -c9-10)
gmemdir=$ROTDIR/${rprefix}.$gPDY/$gcyc/$memchar
sCDATE=$($NDATE -3 $CDATE)

if [[ "$DOIAU" = "YES" ]]; then
  sCDATE=$($NDATE -3 $CDATE)
  sPDY=$(echo $sCDATE | cut -c1-8)
  scyc=$(echo $sCDATE | cut -c9-10)
  tPDY=$gPDY
  tcyc=$gcyc
else
  sCDATE=$CDATE
  sPDY=$PDY
  scyc=$cyc
  tPDY=$sPDY
  tcyc=$cyc
fi

#-------------------------------------------------------
# initial conditions
warm_start=${warm_start:-".false."}
read_increment=${read_increment:-".false."}
res_latlon_dynamics="''"

# Determine if this is a warm start or cold start
if [ -f $gmemdir/RESTART/${sPDY}.${scyc}0000.coupler.res ]; then
  export warm_start=".true."
fi

# turn IAU off for cold start
DOIAU_coldstart=${DOIAU_coldstart:-"NO"}
if [ $DOIAU = "YES" -a $warm_start = ".false." ] || [ $DOIAU_coldstart = "YES" -a $warm_start = ".true." ]; then
  export DOIAU="NO"
  echo "turning off IAU"
  DOIAU_coldstart="YES"
  IAU_OFFSET=0
  sCDATE=$CDATE
  sPDY=$PDY
  scyc=$cyc
  tPDY=$sPDY
  tcyc=$cyc
fi

#-------------------------------------------------------
if [ $warm_start = ".true." -o $RERUN = "YES" ]; then
#-------------------------------------------------------
#.............................
  if [ $RERUN = "NO" ]; then
#.............................

  # Link all (except sfc_data) restart files from $gmemdir
  for file in $(ls $gmemdir/RESTART/${sPDY}.${scyc}0000.*.nc); do
    file2=$(echo $(basename $file))
    file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
    fsuf=$(echo $file2 | cut -d. -f1)
    if [ $fsuf != "sfc_data" ]; then
       $NLN $file $DATA/INPUT/$file2
    fi
  done

  # Link sfcanl_data restart files from $memdir
  for file in $(ls $memdir/RESTART/${sPDY}.${scyc}0000.*.nc); do
    file2=$(echo $(basename $file))
    file2=$(echo $file2 | cut -d. -f3-) # remove the date from file
    fsufanl=$(echo $file2 | cut -d. -f1)
    if [ $fsufanl = "sfcanl_data" ]; then
      file2=$(echo $file2 | sed -e "s/sfcanl_data/sfc_data/g")
      $NLN $file $DATA/INPUT/$file2
    fi
  done

  # Need a coupler.res when doing IAU
  if [ $DOIAU = "YES" ]; then
    rm -f $DATA/INPUT/coupler.res
    cat >> $DATA/INPUT/coupler.res << EOF
     2        (Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)
  ${gPDY:0:4}  ${gPDY:4:2}  ${gPDY:6:2}  ${gcyc}     0     0        Model start time:   year, month, day, hour, minute, second
  ${sPDY:0:4}  ${sPDY:4:2}  ${sPDY:6:2}  ${scyc}     0     0        Current model time: year, month, day, hour, minute, second
EOF
  fi

  # Link increments
  if [ $DOIAU = "YES" ]; then
    for i in $(echo $IAUFHRS | sed "s/,/ /g" | rev); do
      incfhr=$(printf %03i $i)
      if [ $incfhr = "006" ]; then
        increment_file=$memdir/${CDUMP}.t${cyc}z.atminc.nc
      else
        increment_file=$memdir/${CDUMP}.t${cyc}z.atmi${incfhr}.nc
      fi
      if [ ! -f $increment_file ]; then
        echo "ERROR: DOIAU = $DOIAU, but missing increment file for fhr $incfhr at $increment_file"
        echo "Abort!"
        exit 1
      fi
      $NLN $increment_file $DATA/INPUT/fv_increment$i.nc
      IAU_INC_FILES="'fv_increment$i.nc',$IAU_INC_FILES"
    done
    read_increment=".false."
    res_latlon_dynamics=""
  else
    increment_file=$memdir/${CDUMP}.t${cyc}z.atminc.nc
    if [ -f $increment_file ]; then
      $NLN $increment_file $DATA/INPUT/fv_increment.nc
      read_increment=".true."
      res_latlon_dynamics="fv_increment.nc"
    fi
  fi

#.............................
  else  ##RERUN                         

    export warm_start=".true."
    PDYT=$(echo $CDATE_RST | cut -c1-8)
    cyct=$(echo $CDATE_RST | cut -c9-10)
    for file in $(ls $RSTDIR_TMP/${PDYT}.${cyct}0000.*); do
      file2=$(echo $(basename $file))
      file2=$(echo $file2 | cut -d. -f3-) 
      $NLN $file $DATA/INPUT/$file2
    done

  fi
#.............................

else ## cold start                            

  for file in $(ls $memdir/INPUT/*.nc); do
    file2=$(echo $(basename $file))
    fsuf=$(echo $file2 | cut -c1-3)
    if [ $fsuf = "gfs" -o $fsuf = "sfc" ]; then
      $NLN $file $DATA/INPUT/$file2
    fi
  done

#-------------------------------------------------------
fi 
#-------------------------------------------------------

nfiles=$(ls -1 $DATA/INPUT/* | wc -l)
if [ $nfiles -le 0 ]; then
  echo "Initial conditions must exist in $DATA/INPUT, ABORT!"
  msg="Initial conditions must exist in $DATA/INPUT, ABORT!"
  postmsg "$jlogfile" "$msg"
  exit 1
fi

# If doing IAU, change forecast hours
if [[ "$DOIAU" = "YES" ]]; then
  FHMAX=$((FHMAX+6))
  if [ $FHMAX_HF -gt 0 ]; then
     FHMAX_HF=$((FHMAX_HF+6))
  fi
fi

#--------------------------------------------------------------------------
# Grid and orography data
for n in $(seq 1 $ntiles); do
  $NLN $FIXfv3/$CASE/${CASE}_grid.tile${n}.nc     $DATA/INPUT/${CASE}_grid.tile${n}.nc
  $NLN $FIXfv3/$CASE/${CASE}_oro_data.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
done
$NLN $FIXfv3/$CASE/${CASE}_mosaic.nc  $DATA/INPUT/grid_spec.nc

# GFS standard input data
IAER=${IAER:-111}
ICO2=${ICO2:-2}

if [ ${new_o3forc:-YES} = YES ]; then
    O3FORC=ozprdlos_2015_new_sbuvO3_tclm15_nuchem.f77
else
    O3FORC=global_o3prdlos.f77
fi
H2OFORC=${H2OFORC:-"global_h2o_pltc.f77"}
$NLN $FIX_AM/${O3FORC}                         $DATA/global_o3prdlos.f77
$NLN $FIX_AM/${H2OFORC}                        $DATA/global_h2oprdlos.f77
$NLN $FIX_AM/global_solarconstant_noaa_an.txt  $DATA/solarconstant_noaa_an.txt
$NLN $FIX_AM/global_sfc_emissivity_idx.txt     $DATA/sfc_emissivity_idx.txt

$NLN $FIX_AM/global_co2historicaldata_glob.txt $DATA/co2historicaldata_glob.txt
$NLN $FIX_AM/co2monthlycyc.txt                 $DATA/co2monthlycyc.txt
if [ $ICO2 -gt 0 ]; then
  for file in $(ls $FIX_AM/fix_co2_proj/global_co2historicaldata*) ; do
    $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
  done
fi

$NLN $FIX_AM/global_climaeropac_global.txt     $DATA/aerosol.dat
if [ $IAER -gt 0 ] ; then
  for file in $(ls $FIX_AM/global_volcanic_aerosols*) ; do
    $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
  done
fi

#### Copy over WW3 inputs
# At this time only test gfs but this change need to be tested on gdas, enkf, and gfs
if [ $cplwav = ".true." ]; then
# Link WW3 files
  for file in $(ls $COMINWW3/${MDC}.${PDY}/${cyc}/rundata/rmp_src_to_dst_conserv_*) ; do
    $NLN $file $DATA/
  done
  $NLN $COMINWW3/${MDC}.${PDY}/${cyc}/rundata/ww3_multi.${MDC}${WAV_MEMBER}.${cycle}.inp $DATA/ww3_multi.inp
        # Check for expected wave grids for this run
  array=($curID $iceID $wndID $uoutpGRD $waveGRD $sbsGRD $postGRD $interpGRD)
  grdALL=`printf "%s\n" "${array[@]}" | sort -u | tr '\n' ' '`
  for wavGRD in ${grdALL}; do
    # Wave IC (restart) file must exist for warm start on this cycle, if not wave model starts from flat ocean
    $NLN $COMINWW3/${MDC}.${PDY}/${cyc}/restart/${MDC}${WAV_MEMBER}.restart.${wavGRD}.${PDY}${cyc} $DATA/restart.${wavGRD}
    $NLN $COMINWW3/${MDC}.${PDY}/${cyc}/rundata/${MDC}.mod_def.$wavGRD $DATA/mod_def.$wavGRD

    # Link wave IC for the next cycle
    # Wave IC (restart) interval controlled by gfs_cyc parameter (default: 4 cyc/day gfs_cyc=4)
    gfs_cyc=${gfs_cyc:-4}
    gfs_cych=`expr 24 / ${gfs_cyc}`
    WRDATE=`$NDATE ${gfs_cych} $CDATE`
    WRPDY=`echo $WRDATE | cut -c1-8`
    WRcyc=`echo $WRDATE | cut -c9-10`
    WRDIR=$COMOUTWW3/${MDC}.${WRPDY}/${WRcyc}/restart
    [[ -d $WRDIR ]] || mkdir -p $WRDIR
    $NLN ${WRDIR}/${MDC}${WAV_MEMBER}.restart.${wavGRD}.${WRDATE} $DATA/restart001.${wavGRD}
  done
  if [ "$WW3ICEINP" = "YES" ]; then
    $NLN $COMINWW3/${MDC}.${PDY}/${cyc}/rundata/${MDC}.${iceID}.${cycle}.ice $DATA/ice.${iceID}
  fi
  if [ "$WW3CURINP" = "YES" ]; then
    $NLN $COMINWW3/${MDC}.${PDY}/${cyc}/rundata/${MDC}.${curID}.${cycle}.cur $DATA/current.${curID}
  fi
# Link output files
  cd $DATA
  datwave=$COMOUTWW3/${MDC}.${PDY}/${cyc}/rundata/
  wavprfx=${MDC}${WAV_MEMBER}
  eval $NLN $datwave/${wavprfx}.log.mww3.${PDY}${cyc} log.mww3
  eval $NLN $datwave/${wavprfx}.log.${wavGRD}.${PDY}${cyc} log.${grdID}
# Loop for gridded output (uses FHINC)
  fhr=$FHMIN
  while [ $fhr -le $FHMAX ]; do
    YMDH=`$NDATE $fhr $CDATE`
    YMD=$(echo $YMDH | cut -c1-8)
    HMS="$(echo $YMDH | cut -c9-10)0000"
      for wavGRD in ${waveGRD} ; do
        eval $NLN $datwave/${wavprfx}.out_grd.${wavGRD}.${YMD}.${HMS} ${YMD}.${HMS}.out_grd.${wavGRD}
      done
      FHINC=$FHOUT
      if [ $FHMAX_HF -gt 0 -a $FHOUT_HF -gt 0 -a $fhr -lt $FHMAX_HF ]; then
        FHINC=$FHOUT_HF
      fi
    fhr=$((fhr+FHINC))
  done
# Loop for point output (uses DTPNT)
  fhr=$FHMIN
  while [ $fhr -le $FHMAX ]; do
    YMDH=`$NDATE $fhr $CDATE`
    YMD=$(echo $YMDH | cut -c1-8)
    HMS="$(echo $YMDH | cut -c9-10)0000"
      eval $NLN $datwave/${wavprfx}.out_pnt.${uoutpGRD}.${YMD}.${HMS} ${YMD}.${HMS}.out_pnt.${uoutpGRD}
      FHINC=$FHINCWAVP
    fhr=$((fhr+FHINC))
  done
fi

# inline post fix files
if [ $WRITE_DOPOST = ".true." ]; then
    $NLN $PARM_POST/post_tag_gfs${LEVS}             $DATA/itag               
    $NLN $PARM_POST/postxconfig-NT-GFS-TWO.txt      $DATA/postxconfig-NT.txt 
    $NLN $PARM_POST/postxconfig-NT-GFS-F00-TWO.txt  $DATA/postxconfig-NT_FH00.txt
    $NLN $PARM_POST/params_grib2_tbl_new            $DATA/params_grib2_tbl_new
fi
#------------------------------------------------------------------

# changeable parameters
# dycore definitions
res=$(echo $CASE |cut -c2-5)
resp=$((res+1))
npx=$resp
npy=$resp
npz=$((LEVS-1))
io_layout="1,1"
#ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

# spectral truncation and regular grid resolution based on FV3 resolution
JCAP_CASE=$((2*res-2))
LONB_CASE=$((4*res))
LATB_CASE=$((2*res))

JCAP=${JCAP:-$JCAP_CASE}
LONB=${LONB:-$LONB_CASE}
LATB=${LATB:-$LATB_CASE}

LONB_IMO=${LONB_IMO:-$LONB_CASE}
LATB_JMO=${LATB_JMO:-$LATB_CASE}

# Fix files
FNGLAC=${FNGLAC:-"$FIX_AM/global_glacier.2x2.grb"}
FNMXIC=${FNMXIC:-"$FIX_AM/global_maxice.2x2.grb"}
FNTSFC=${FNTSFC:-"$FIX_AM/RTGSST.1982.2012.monthly.clim.grb"}
FNSNOC=${FNSNOC:-"$FIX_AM/global_snoclim.1.875.grb"}
FNZORC=${FNZORC:-"igbp"}
FNALBC2=${FNALBC2:-"$FIX_AM/global_albedo4.1x1.grb"}
FNAISC=${FNAISC:-"$FIX_AM/CFSR.SEAICE.1982.2012.monthly.clim.grb"}
FNTG3C=${FNTG3C:-"$FIX_AM/global_tg3clim.2.6x1.5.grb"}
FNVEGC=${FNVEGC:-"$FIX_AM/global_vegfrac.0.144.decpercent.grb"}
FNMSKH=${FNMSKH:-"$FIX_AM/global_slmask.t1534.3072.1536.grb"}
FNVMNC=${FNVMNC:-"$FIX_AM/global_shdmin.0.144x0.144.grb"}
FNVMXC=${FNVMXC:-"$FIX_AM/global_shdmax.0.144x0.144.grb"}
FNSLPC=${FNSLPC:-"$FIX_AM/global_slope.1x1.grb"}
FNALBC=${FNALBC:-"$FIX_AM/global_snowfree_albedo.bosu.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNVETC=${FNVETC:-"$FIX_AM/global_vegtype.igbp.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNSOTC=${FNSOTC:-"$FIX_AM/global_soiltype.statsgo.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNABSC=${FNABSC:-"$FIX_AM/global_mxsnoalb.uariz.t${JCAP}.${LONB}.${LATB}.rg.grb"}
FNSMCC=${FNSMCC:-"$FIX_AM/global_soilmgldas.statsgo.t${JCAP}.${LONB}.${LATB}.grb"}

# If the appropriate resolution fix file is not present, use the highest resolution available (T1534)
[[ ! -f $FNALBC ]] && FNALBC="$FIX_AM/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb"
[[ ! -f $FNVETC ]] && FNVETC="$FIX_AM/global_vegtype.igbp.t1534.3072.1536.rg.grb"
[[ ! -f $FNSOTC ]] && FNSOTC="$FIX_AM/global_soiltype.statsgo.t1534.3072.1536.rg.grb"
[[ ! -f $FNABSC ]] && FNABSC="$FIX_AM/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb"
[[ ! -f $FNSMCC ]] && FNSMCC="$FIX_AM/global_soilmgldas.statsgo.t1534.3072.1536.grb"

# NSST Options
# nstf_name contains the NSST related parameters
# nstf_name(1) : NST_MODEL (NSST Model) : 0 = OFF, 1 = ON but uncoupled, 2 = ON and coupled
# nstf_name(2) : NST_SPINUP : 0 = OFF, 1 = ON,
# nstf_name(3) : NST_RESV (Reserved, NSST Analysis) : 0 = OFF, 1 = ON
# nstf_name(4) : ZSEA1 (in mm) : 0
# nstf_name(5) : ZSEA2 (in mm) : 0
# nst_anl      : .true. or .false., NSST analysis over lake
NST_MODEL=${NST_MODEL:-0}
NST_SPINUP=${NST_SPINUP:-0}
NST_RESV=${NST_RESV-0}
ZSEA1=${ZSEA1:-0}
ZSEA2=${ZSEA2:-0}
nstf_name=${nstf_name:-"$NST_MODEL,$NST_SPINUP,$NST_RESV,$ZSEA1,$ZSEA2"}
nst_anl=${nst_anl:-".false."}


# blocking factor used for threading and general physics performance
#nyblocks=`expr \( $npy - 1 \) \/ $layout_y `
#nxblocks=`expr \( $npx - 1 \) \/ $layout_x \/ 32`
#if [ $nxblocks -le 0 ]; then nxblocks=1 ; fi
blocksize=${blocksize:-32}

# the pre-conditioning of the solution
# =0 implies no pre-conditioning
# >0 means new adiabatic pre-conditioning
# <0 means older adiabatic pre-conditioning
na_init=${na_init:-1}
[[ $warm_start = ".true." ]] && na_init=0

# variables for controlling initialization of NCEP/NGGPS ICs
filtered_terrain=${filtered_terrain:-".true."}
gfs_dwinds=${gfs_dwinds:-".true."}

# various debug options
no_dycore=${no_dycore:-".false."}
dycore_only=${adiabatic:-".false."}
chksum_debug=${chksum_debug:-".false."}
print_freq=${print_freq:-6}

if [ ${TYPE} = "nh" ]; then # non-hydrostatic options

  hydrostatic=".false."
  phys_hydrostatic=".false."     # enable heating in hydrostatic balance in non-hydrostatic simulation
  use_hydro_pressure=".false."   # use hydrostatic pressure for physics
  if [ $warm_start = ".true." ]; then
    make_nh=".false."              # restarts contain non-hydrostatic state
  else
    make_nh=".true."               # re-initialize non-hydrostatic state
  fi

else # hydrostatic options

  hydrostatic=".true."
  phys_hydrostatic=".false."     # ignored when hydrostatic = T
  use_hydro_pressure=".false."   # ignored when hydrostatic = T
  make_nh=".false."              # running in hydrostatic mode

fi

# Conserve total energy as heat globally
consv_te=${consv_te:-1.} # range 0.-1., 1. will restore energy to orig. val. before physics

# time step parameters in FV3
k_split=${k_split:-2}
n_split=${n_split:-6}

if [ $(echo $MONO | cut -c-4) = "mono" ];  then # monotonic options

  d_con=${d_con_mono:-"0."}
  do_vort_damp=".false."
  if [ ${TYPE} = "nh" ]; then # non-hydrostatic
    hord_mt=${hord_mt_nh_mono:-"10"}
    hord_xx=${hord_xx_nh_mono:-"10"}
  else # hydrostatic
    hord_mt=${hord_mt_hydro_mono:-"10"}
    hord_xx=${hord_xx_hydro_mono:-"10"}
  fi

else # non-monotonic options

  d_con=${d_con_nonmono:-"1."}
  do_vort_damp=".true."
  if [ ${TYPE} = "nh" ]; then # non-hydrostatic
    hord_mt=${hord_mt_nh_nonmono:-"5"}
    hord_xx=${hord_xx_nh_nonmono:-"5"}
  else # hydrostatic
    hord_mt=${hord_mt_hydro_nonmono:-"10"}
    hord_xx=${hord_xx_hydro_nonmono:-"10"}
  fi

fi

if [ $(echo $MONO | cut -c-4) != "mono" -a $TYPE = "nh" ]; then
  vtdm4=${vtdm4_nh_nonmono:-"0.06"}
else
  vtdm4=${vtdm4:-"0.05"}
fi

if [ $warm_start = ".true." ]; then # warm start from restart file

  nggps_ic=".false."
  ncep_ic=".false."
  external_ic=".false."
  mountain=".true."
  if [ $read_increment = ".true." ]; then # add increment on the fly to the restarts
    res_latlon_dynamics="fv_increment.nc"
  else
    res_latlon_dynamics='""'
  fi

else # CHGRES'd GFS analyses

  nggps_ic=${nggps_ic:-".true."}
  ncep_ic=${ncep_ic:-".false."}
  external_ic=".true."
  mountain=".false."
  read_increment=".false."
  res_latlon_dynamics='""'

fi

# Stochastic Physics Options
if [ ${SET_STP_SEED:-"YES"} = "YES" ]; then
  ISEED_SKEB=$((CDATE*1000 + MEMBER*10 + 1))
  ISEED_SHUM=$((CDATE*1000 + MEMBER*10 + 2))
  ISEED_SPPT=$((CDATE*1000 + MEMBER*10 + 3))
else
  ISEED=${ISEED:-0}
fi
DO_SKEB=${DO_SKEB:-"NO"}
DO_SPPT=${DO_SPPT:-"NO"}
DO_SHUM=${DO_SHUM:-"NO"}
JCAP_STP=${JCAP_STP:-$JCAP_CASE}
LONB_STP=${LONB_STP:-$LONB_CASE}
LATB_STP=${LATB_STP:-$LATB_CASE}

if [ $DO_SKEB = "YES" ]; then
    do_skeb=".true."
fi
if [ $DO_SHUM = "YES" ]; then
    do_shum=".true."
fi
if [ $DO_SPPT = "YES" ]; then
    do_sppt=".true."
fi

# copy over the tables
DIAG_TABLE=${DIAG_TABLE:-$PARM_FV3DIAG/diag_table}
DATA_TABLE=${DATA_TABLE:-$PARM_FV3DIAG/data_table}
FIELD_TABLE=${FIELD_TABLE:-$PARM_FV3DIAG/field_table}

# build the diag_table with the experiment name and date stamp
if [ $DOIAU = "YES" ]; then
cat > diag_table << EOF
FV3 Forecast
${gPDY:0:4} ${gPDY:4:2} ${gPDY:6:2} ${gcyc} 0 0
EOF
cat $DIAG_TABLE >> diag_table
else
cat > diag_table << EOF
FV3 Forecast
${sPDY:0:4} ${sPDY:4:2} ${sPDY:6:2} ${scyc} 0 0
EOF
cat $DIAG_TABLE >> diag_table
fi

$NCP $DATA_TABLE  data_table
$NCP $FIELD_TABLE field_table

#------------------------------------------------------------------
rm -f nems.configure
cat > nems.configure <<EOF
EARTH_component_list: ATM
ATM_model:            fv3
runSeq::
  ATM
::
EOF

#### ww3 version of nems.configure
if [ $cplwav = ".true." ]; then

# Switch on cpl flag
  cpl=.true.

NTASKS_FV3m1=$((NTASKS_FV3-1))
atm_petlist_bounds=" 0 $((NTASKS_FV3-1))"
wav_petlist_bounds=" $((NTASKS_FV3)) $((NTASKS_FV3m1+npe_wav))"
###  atm_petlist_bounds=" 0   1511"
###  atm_petlist_bounds=$atm_petlist_bounds
###  wav_petlist_bounds="1512 1691"
###  wav_petlist_bounds=$wav_petlist_bounds
  coupling_interval_sec=${coupling_interval_sec:-1800}
  rm -f nems.configure
cat > nems.configure <<EOF
EARTH_component_list: ATM WAV
EARTH_attributes::
  Verbosity = 0
::

ATM_model:                      fv3
ATM_petlist_bounds:             ${atm_petlist_bounds}
ATM_attributes::
  Verbosity = 0
  DumpFields = false
::

WAV_model:                      ww3
WAV_petlist_bounds:             ${wav_petlist_bounds}
WAV_attributes::
  Verbosity = 0
::

runSeq::
  @${coupling_interval_sec}
    ATM -> WAV
    ATM
    WAV
  @
::
EOF
fi

# Set NTASKS_CFG to reflect cplwav
NTASKS_CFG=$NTASKS_FV3
if [ $cplwav = ".true." ]; then
  NTASKS_CFG=$((NTASKS_FV3 + npe_wav))
fi

rm -f model_configure
cat > model_configure <<EOF
total_member:            $ENS_NUM
print_esmf:              ${print_esmf:-.true.}
PE_MEMBER01:             $NTASKS_CFG
start_year:              ${tPDY:0:4}
start_month:             ${tPDY:4:2}
start_day:               ${tPDY:6:2}
start_hour:              ${tcyc}
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
RUN_CONTINUE:            ${RUN_CONTINUE:-".false."}
ENS_SPS:                 ${ENS_SPS:-".false."}

dt_atmos:                $DELTIM
output_1st_tstep_rst:    .false.
calendar:                ${calendar:-'julian'}
cpl:                     ${cpl:-".false."}
memuse_verbose:          ${memuse_verbose:-".false."}
atmos_nthreads:          $NTHREADS_FV3
use_hyper_thread:        ${hyperthread:-".false."}
ncores_per_node:         $cores_per_node
restart_interval:        $restart_interval

quilting:                $QUILTING
write_groups:            ${WRITE_GROUP:-1}
write_tasks_per_group:   ${WRTTASK_PER_GROUP:-24}
output_history:          ${OUTPUT_HISTORY:-".true."}
write_dopost:            ${WRITE_DOPOST:-".false."}
num_files:               ${NUM_FILES:-2}
filename_base:           'atm' 'sfc'
output_grid:             $OUTPUT_GRID
output_file:             $OUTPUT_FILE
output_1st_tstep_rst:    ${output_1st_tstep_rst:-".false."}
ideflate:                ${ideflate:-1}
nbits:                   ${nbits:-14}
write_nemsioflip:        $WRITE_NEMSIOFLIP
write_fsyncflag:         $WRITE_FSYNCFLAG
imo:                     $LONB_IMO
jmo:                     $LATB_JMO

nfhout:                  $FHOUT
nfhmax_hf:               $FHMAX_HF
nfhout_hf:               $FHOUT_HF
nsout:                   $NSOUT
iau_offset:              ${IAU_OFFSET}
EOF

#&coupler_nml
#  months = ${months:-0}
#  days = ${days:-$((FHMAX/24))}
#  hours = ${hours:-$((FHMAX-24*(FHMAX/24)))}
#  dt_atmos = $DELTIM
#  dt_ocean = $DELTIM
#  current_date = $curr_date
#  calendar = 'julian'
#  memuse_verbose = .false.
#  atmos_nthreads = $NTHREADS_FV3
#  use_hyper_thread = ${hyperthread:-".false."}
#  ncores_per_node = $cores_per_node
#  restart_secs = $restart_secs
#  $coupler_nml
#/

cat > input.nml <<EOF
&amip_interp_nml
  interp_oi_sst = .true.
  use_ncep_sst = .true.
  use_ncep_ice = .false.
  no_anom_sst = .false.
  data_set = 'reynolds_oi'
  date_out_of_range = 'climo'
  $amip_interp_nml
/

&atmos_model_nml
  blocksize = $blocksize
  chksum_debug = $chksum_debug
  dycore_only = $dycore_only
  fdiag = $FDIAG
  fhmax = $FHMAX
  fhout = $FHOUT
  fhmaxhf = $FHMAX_HF
  fhouthf = $FHOUT_HF
  $atmos_model_nml
/

&diag_manager_nml
  prepend_date = .false.
  $diag_manager_nml
/

&fms_io_nml
  checksum_required = .false.
  max_files_r = 100
  max_files_w = 100
  $fms_io_nml
/
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
&mpp_io_nml
shuffle=${shuffle:-1}
deflate_level=${deflate_level:-1}
/
EOF
fi

cat >> input.nml << EOF
&fms_nml
  clock_grain = 'ROUTINE'
  domains_stack_size = ${domains_stack_size:-3000000}
  print_memory_usage = ${print_memory_usage:-".false."}
  $fms_nml
/

&fv_core_nml
  layout = $layout_x,$layout_y
  io_layout = $io_layout
  npx = $npx
  npy = $npy
  ntiles = $ntiles
  npz = $npz
  grid_type = -1
  make_nh = $make_nh
  fv_debug = ${fv_debug:-".false."}
  range_warn = ${range_warn:-".false."}
  reset_eta = .false.
  n_sponge = ${n_sponge:-"10"}
  nudge_qv = ${nudge_qv:-".true."}
  nudge_dz = ${nudge_dz:-".false."}
  tau = ${tau:-10.}
  rf_cutoff = ${rf_cutoff:-"7.5e2"}
  d2_bg_k1 = ${d2_bg_k1:-"0.15"}
  d2_bg_k2 = ${d2_bg_k2:-"0.02"}
  kord_tm = ${kord_tm:-"-9"}
  kord_mt = ${kord_mt:-"9"}
  kord_wz = ${kord_wz:-"9"}
  kord_tr = ${kord_tr:-"9"}
  hydrostatic = $hydrostatic
  phys_hydrostatic = $phys_hydrostatic
  use_hydro_pressure = $use_hydro_pressure
  beta = 0.
  a_imp = 1.
  p_fac = 0.1
  k_split = $k_split
  n_split = $n_split
  nwat = ${nwat:-2}
  na_init = $na_init
  d_ext = 0.
  dnats = ${dnats:-0}
  fv_sg_adj = ${fv_sg_adj:-"450"}
  d2_bg = 0.
  nord = ${nord:-3}
  dddmp = ${dddmp:-0.2}
  d4_bg = ${d4_bg:-0.15}
  vtdm4 = $vtdm4
  delt_max = ${delt_max:-"0.002"}
  ke_bg = 0.
  do_vort_damp = $do_vort_damp
  external_ic = $external_ic
  external_eta = ${external_eta:-.true.}
  gfs_phil = ${gfs_phil:-".false."}
  nggps_ic = $nggps_ic
  mountain = $mountain
  ncep_ic = $ncep_ic
  d_con = $d_con
  hord_mt = $hord_mt
  hord_vt = $hord_xx
  hord_tm = $hord_xx
  hord_dp = -$hord_xx
  hord_tr = ${hord_tr:-"8"}
  adjust_dry_mass = ${adjust_dry_mass:-".false."}
  consv_te = $consv_te
  do_sat_adj = ${do_sat_adj:-".false."}
  consv_am = .false.
  fill = .true.
  dwind_2d = .false.
  print_freq = $print_freq
  warm_start = $warm_start
  no_dycore = $no_dycore
  z_tracer = .true.
  agrid_vel_rst = ${agrid_vel_rst:-".true."}
  read_increment = $read_increment
  res_latlon_dynamics = $res_latlon_dynamics
  $fv_core_nml
/
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
&cires_ugwp_nml
       knob_ugwp_solver  = ${knob_ugwp_solver:-2}
       knob_ugwp_source  = ${knob_ugwp_source:-1,1,0,0}
       knob_ugwp_wvspec  = ${knob_ugwp_wvspec:-1,25,25,25}
       knob_ugwp_azdir   = ${knob_ugwp_azdir:-2,4,4,4}
       knob_ugwp_stoch   = ${knob_ugwp_stoch:-0,0,0,0}
       knob_ugwp_effac   = ${knob_ugwp_effac:-1,1,1,1}
       knob_ugwp_doaxyz  = ${knob_ugwp_doaxyz:-1}
       knob_ugwp_doheat  = ${knob_ugwp_doheat:-1}
       knob_ugwp_dokdis  = ${knob_ugwp_dokdis:-1}
       knob_ugwp_ndx4lh  = ${knob_ugwp_ndx4lh:-1}
       knob_ugwp_version = ${knob_ugwp_version:-0}
       launch_level      = ${launch_level:-54}                   
/
EOF
fi

cat >> input.nml << EOF
&external_ic_nml
  filtered_terrain = $filtered_terrain
  levp = $LEVS
  gfs_dwinds = $gfs_dwinds
  checker_tr = .false.
  nt_checker = 0
  $external_ic_nml
/

&gfs_physics_nml
  fhzero       = $FHZER
  h2o_phys     = ${h2o_phys:-".true."}
  ldiag3d      = ${ldiag3d:-".false."}
  fhcyc        = $FHCYC
  use_ufo      = ${use_ufo:-".true."}
  pre_rad      = ${pre_rad:-".false."}
  ncld         = ${ncld:-1}
  imp_physics  = ${imp_physics:-"99"}
  pdfcld       = ${pdfcld:-".false."}
  fhswr        = ${FHSWR:-"3600."}
  fhlwr        = ${FHLWR:-"3600."}
  ialb         = ${IALB:-"1"}
  iems         = ${IEMS:-"1"}
  iaer         = $IAER
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
  icliq_sw     = ${icliq_sw:-"2"}
  iovr_lw      = ${iovr_lw:-"3"}
  iovr_sw      = ${iovr_sw:-"3"}
EOF
fi

cat >> input.nml << EOF
  ico2         = $ICO2
  isubc_sw     = ${isubc_sw:-"2"}
  isubc_lw     = ${isubc_lw:-"2"}
  isol         = ${ISOL:-"2"}
  lwhtr        = ${lwhtr:-".true."}
  swhtr        = ${swhtr:-".true."}
  cnvgwd       = ${cnvgwd:-".true."}
  shal_cnv     = ${shal_cnv:-".true."}
  cal_pre      = ${cal_pre:-".true."}
  redrag       = ${redrag:-".true."}
  dspheat      = ${dspheat:-".true."}
EOF

if [ "${NET}" = "gens" ]; then
cat >> input.nml << EOF
  hybedmf      = ${hybedmf:-".true."}
EOF
elif [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
  hybedmf      = ${hybedmf:-".false."}
  satmedmf     = ${satmedmf-".true."}
  isatmedmf    = ${isatmedmf-"1"}
  lheatstrg    = ${lheatstrg-".true."}
EOF
fi

cat >> input.nml << EOF
  random_clds  = ${random_clds:-".true."}
  trans_trac   = ${trans_trac:-".true."}
  cnvcld       = ${cnvcld:-".true."}
  imfshalcnv   = ${imfshalcnv:-"2"}
  imfdeepcnv   = ${imfdeepcnv:-"2"}
  cdmbgwd      = ${cdmbgwd:-"3.5,0.25"}
  prslrd0      = ${prslrd0:-"0."}
  ivegsrc      = ${ivegsrc:-"1"}
  isot         = ${isot:-"1"}
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
  lsoil        = ${lsoil:-"4"}
  lsm          = ${lsm:-"2"}
  iopt_dveg    = ${iopt_dveg:-"1"}
  iopt_crs     = ${iopt_crs:-"1"}
  iopt_btr     = ${iopt_btr:-"1"}
  iopt_run     = ${iopt_run:-"1"}
  iopt_sfc     = ${iopt_sfc:-"1"}
  iopt_frz     = ${iopt_frz:-"1"}
  iopt_inf     = ${iopt_inf:-"1"}
  iopt_rad     = ${iopt_rad:-"1"}
  iopt_alb     = ${iopt_alb:-"2"}
  iopt_snf     = ${iopt_snf:-"4"}
  iopt_tbot    = ${iopt_tbot:-"2"}
  iopt_stc     = ${iopt_stc:-"1"}
EOF
fi

cat >> input.nml << EOF
  debug        = ${gfs_phys_debug:-".false."}
  nstf_name    = $nstf_name
  nst_anl      = $nst_anl
  psautco      = ${psautco:-"0.0008,0.0005"}
  prautco      = ${prautco:-"0.00015,0.00015"}
  lgfdlmprad   = ${lgfdlmprad:-".false."}
  effr_in      = ${effr_in:-".false."}
  cplwav       = ${cplwav:-".false."}
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
  ldiag_ugwp   = ${ldiag_ugwp:-".false."}
  do_ugwp      = ${do_ugwp:-".true."}
  do_tofd      = ${do_tofd:-".true."}
  do_sppt      = ${do_sppt:-".false."}
  do_shum      = ${do_shum:-".false."}
  do_skeb      = ${do_skeb:-".false."}
EOF
fi

# Add namelist for IAU
if [ $DOIAU = "YES" ]; then
  cat >> input.nml << EOF
  iaufhrs      = ${IAUFHRS}
  iau_delthrs  = ${IAU_DELTHRS}
  iau_inc_files= ${IAU_INC_FILES}
EOF
fi

cat >> input.nml <<EOF
  $gfs_physics_nml
/
EOF

echo "" >> input.nml

cat >> input.nml <<EOF
&gfdl_cloud_microphysics_nml
  sedi_transport = .true.
  do_sedi_heat = .false.
  rad_snow = .true.
  rad_graupel = .true.
  rad_rain = .true.
  const_vi = .F.
  const_vs = .F.
  const_vg = .F.
  const_vr = .F.
  vi_max = 1.
  vs_max = 2.
  vg_max = 12.
  vr_max = 12.
  qi_lim = 1.
  prog_ccn = .false.
  do_qa = .true.
  fast_sat_adj = .true.
  tau_l2v = 225.
  tau_v2l = 150.
  tau_g2v = 900.
  rthresh = 10.e-6  ! This is a key parameter for cloud water
  dw_land  = 0.16
  dw_ocean = 0.10
  ql_gen = 1.0e-3
  ql_mlt = 1.0e-3
  qi0_crt = 8.0E-5
  qs0_crt = 1.0e-3
  tau_i2s = 1000.
  c_psaci = 0.05
  c_pgacs = 0.01
  rh_inc = 0.30
  rh_inr = 0.30
  rh_ins = 0.30
  ccn_l = 300.
  ccn_o = 100.
  c_paut = 0.5
  c_cracw = 0.8
  use_ppm = .false.
  use_ccn = .true.
  mono_prof = .true.
  z_slope_liq  = .true.
  z_slope_ice  = .true.
  de_ice = .false.
  fix_negative = .true.
  icloud_f = 1
  mp_time = 150.
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
  reiflag = ${reiflag:-"2"}
EOF
fi

cat >> input.nml << EOF
  $gfdl_cloud_microphysics_nml
/

&interpolator_nml
  interp_method = 'conserve_great_circle'
  $interpolator_nml
/

&namsfc
  FNGLAC   = '${FNGLAC}'
  FNMXIC   = '${FNMXIC}'
  FNTSFC   = '${FNTSFC}'
  FNSNOC   = '${FNSNOC}'
  FNZORC   = '${FNZORC}'
  FNALBC   = '${FNALBC}'
  FNALBC2  = '${FNALBC2}'
  FNAISC   = '${FNAISC}'
  FNTG3C   = '${FNTG3C}'
  FNVEGC   = '${FNVEGC}'
  FNVETC   = '${FNVETC}'
  FNSOTC   = '${FNSOTC}'
  FNSMCC   = '${FNSMCC}'
  FNMSKH   = '${FNMSKH}'
  FNTSFA   = '${FNTSFA}'
  FNACNA   = '${FNACNA}'
  FNSNOA   = '${FNSNOA}'
  FNVMNC   = '${FNVMNC}'
  FNVMXC   = '${FNVMXC}'
  FNSLPC   = '${FNSLPC}'
  FNABSC   = '${FNABSC}'
  LDEBUG = ${LDEBUG:-".false."}
  FSMCL(2) = ${FSMCL2:-99999}
  FSMCL(3) = ${FSMCL3:-99999}
  FSMCL(4) = ${FSMCL4:-99999}
EOF

if [ "${NET}" != "gens" ]; then
cat >> input.nml << EOF
 LANDICE  = ${landice:-".true."}
EOF
fi

cat >> input.nml << EOF
  FTSFS = ${FTSFS:-90}
  FAISL = ${FAISL:-99999}
  FAISS = ${FAISS:-99999}
  FSNOL = ${FSNOL:-99999}
  FSNOS = ${FSNOS:-99999}
  FSICL = 99999
  FSICS = 99999
  FTSFL = 99999
  FVETL = 99999
  FSOTL = 99999
  FvmnL = 99999
  FvmxL = 99999
  FSLPL = 99999
  FABSL = 99999
  $namsfc_nml
/

&fv_grid_nml
  grid_file = 'INPUT/grid_spec.nc'
  $fv_grid_nml
/
EOF

# Add namelist for stochastic physics options
echo "" >> input.nml
if [ $MEMBER -gt 0 ]; then

    cat >> input.nml << EOF
&nam_stochy
  ntrunc = $JCAP_STP
  lon_s = $LONB_STP
  lat_s = $LATB_STP
EOF

  if [ $DO_SKEB = "YES" ]; then
    cat >> input.nml << EOF
  skeb = $SKEB
  iseed_skeb = ${ISEED_SKEB:-$ISEED}
  skeb_tau = ${SKEB_TAU:-"-999."}
  skeb_lscale = ${SKEB_LSCALE:-"-999."}
  skebnorm = ${SKEBNORM:-"1"}
  skeb_npass = ${SKEB_nPASS:-"30"}
  skeb_vdof = ${SKEB_VDOF:-"5"}
EOF
  fi

  if [ $DO_SHUM = "YES" ]; then
    cat >> input.nml << EOF
  shum = $SHUM
  iseed_shum = ${ISEED_SHUM:-$ISEED}
  shum_tau = ${SHUM_TAU:-"-999."}
  shum_lscale = ${SHUM_LSCALE:-"-999."}
EOF
  fi

  if [ $DO_SPPT = "YES" ]; then
    cat >> input.nml << EOF
  sppt = $SPPT
  iseed_sppt = ${ISEED_SPPT:-$ISEED}
  sppt_tau = ${SPPT_TAU:-"-999."}
  sppt_lscale = ${SPPT_LSCALE:-"-999."}
  sppt_logit = ${SPPT_LOGIT:-".true."}
  sppt_sfclimit = ${SPPT_SFCLIMIT:-".true."}
  use_zmtnblck = ${use_zmtnblck:-".true."}
EOF
  fi

  cat >> input.nml << EOF
  $nam_stochy_nml
/
EOF


    cat >> input.nml << EOF
&nam_sfcperts
  $nam_sfcperts_nml
/
EOF

else

  cat >> input.nml << EOF
&nam_stochy
/
&nam_sfcperts
/
EOF

fi


#------------------------------------------------------------------
# make symbolic links to write forecast files directly in memdir
cd $DATA
if [ $QUILTING = ".true." -a $OUTPUT_GRID = "gaussian_grid" ]; then
  fhr=$FHMIN
  while [ $fhr -le $FHMAX ]; do
    FH3=$(printf %03i $fhr)
    FH2=$(printf %02i $fhr)
    atmi=atmf${FH3}.$affix
    sfci=sfcf${FH3}.$affix
    logi=logf${FH3}
    pgbi=GFSPRS.GrbF${FH2}
    flxi=GFSFLX.GrbF${FH2}
    atmo=$memdir/${CDUMP}.t${cyc}z.atmf${FH3}.$affix
    sfco=$memdir/${CDUMP}.t${cyc}z.sfcf${FH3}.$affix
    logo=$memdir/${CDUMP}.t${cyc}z.logf${FH3}.txt
    pgbo=$memdir/${CDUMP}.t${cyc}z.master.grb2f${FH3}
    flxo=$memdir/${CDUMP}.t${cyc}z.sfluxgrbf${FH3}.grib2
    eval $NLN $atmo $atmi
    eval $NLN $sfco $sfci
    eval $NLN $logo $logi
    if [ $WRITE_DOPOST = ".true." ]; then
      eval $NLN $pgbo $pgbi
      eval $NLN $flxo $flxi
    fi
    FHINC=$FHOUT
    if [ $FHMAX_HF -gt 0 -a $FHOUT_HF -gt 0 -a $fhr -lt $FHMAX_HF ]; then
      FHINC=$FHOUT_HF
    fi
    fhr=$((fhr+FHINC))
  done
else
  for n in $(seq 1 $ntiles); do
    eval $NLN nggps2d.tile${n}.nc       $memdir/nggps2d.tile${n}.nc
    eval $NLN nggps3d.tile${n}.nc       $memdir/nggps3d.tile${n}.nc
    eval $NLN grid_spec.tile${n}.nc     $memdir/grid_spec.tile${n}.nc
    eval $NLN atmos_static.tile${n}.nc  $memdir/atmos_static.tile${n}.nc
    eval $NLN atmos_4xdaily.tile${n}.nc $memdir/atmos_4xdaily.tile${n}.nc
  done
fi

#------------------------------------------------------------------
# run the executable

$NCP $FCSTEXECDIR/$FCSTEXEC $DATA/.
export OMP_NUM_THREADS=$NTHREADS_FV3
$APRUN_FV3 $DATA/$FCSTEXEC 1>&1 2>&2
export ERR=$?
export err=$ERR
$ERRSCRIPT || exit $err

#------------------------------------------------------------------
if [ $SEND = "YES" ]; then

  # Copy gdas and enkf member restart files
  if [ $CDUMP = "gdas" -a $rst_invt1 -gt 0 ]; then
    cd $DATA/RESTART
    mkdir -p $memdir/RESTART

    for rst_int in $restart_interval ; do
     if [ $rst_int -ge 0 ]; then
      RDATE=$($NDATE +$rst_int $CDATE)
      rPDY=$(echo $RDATE | cut -c1-8)
      rcyc=$(echo $RDATE | cut -c9-10)
      for file in $(ls ${rPDY}.${rcyc}0000.*) ; do
        $NCP $file $memdir/RESTART/$file
      done
     fi
    done
    if [ $DOIAU = "YES" ] || [ $DOIAU_coldstart = "YES" ]; then
       # if IAU is on, save restart at start of IAU window
       rst_iau=$(( ${IAU_OFFSET} - (${IAU_DELTHRS}/2) ))
       if [ $rst_iau -lt 0 ];then
          rst_iau=$(( (${IAU_DELTHRS}) - ${IAU_OFFSET} ))
       fi
       RDATE=$($NDATE +$rst_iau $CDATE)
       rPDY=$(echo $RDATE | cut -c1-8)
       rcyc=$(echo $RDATE | cut -c9-10)
       for file in $(ls ${rPDY}.${rcyc}0000.*) ; do
          $NCP $file $memdir/RESTART/$file
       done
    fi

  fi

fi

#------------------------------------------------------------------
# Clean up before leaving
if [ $mkdata = "YES" ]; then rm -rf $DATA; fi

#------------------------------------------------------------------
set +x
if [ $VERBOSE = "YES" ] ; then
  echo $(date) EXITING $0 with return code $err >&2
fi
exit 0

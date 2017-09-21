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

# This should be in the script that calls this script, not here
machine=${machine:-"WCOSS_C"}
machine=$(echo $machine | tr '[a-z]' '[A-Z]')
if [ $machine = "WCOSS_C" ] ; then
  . $MODULESHOME/init/sh 2>/dev/null
  PRGENV=${PRGENV:-intel}
  HUGEPAGES=${HUGEPAGES:-hugepages4M}
  module  unload prod_util iobuf PrgEnv-$PRGENV craype-$HUGEPAGES 2>/dev/null
  module  load   prod_util iobuf PrgEnv-$PRGENV craype-$HUGEPAGES 2>/dev/null
  module  use /usrx/local/dev/modulefiles
  module  load ESMF-intel-haswell/7_0_0 2>/dev/null
  export IOBUF_PARAMS=${IOBUF_PARAMS:-'*:size=8M:verbose'}
  export MPICH_GNI_COLL_OPT_OFF=${MPICH_GNI_COLL_OPT_OFF:-MPI_Alltoallv}
  export MKL_CBWR=AVX2
elif [ $machine = "THEIA" ]; then
  . $MODULESHOME/init/sh 2>/dev/null
  module load esmf/7.0.0 2>/dev/null
fi

# Cycling and forecast hour specific parameters
CASE=${CASE:-C768}
CDATE=${CDATE:-2017032500}
CDUMP=${CDUMP:-gdas}
FHMIN=${FHMIN:-0}
FHMAX=${FHMAX:-9}
FHOUT=${FHOUT:-3}
FHZER=${FHZER:-6}
FHCYC=${FHCYC:-24}

# Directories.
pwd=$(pwd)
NWPROD=${NWPROD:-${NWROOT:-$pwd}}
BASE_GSM=${BASE_GSM:-$NWPROD}
FIX_DIR=${FIX_DIR:-$BASE_GSM/fix}
FIX_AM=${FIX_AM:-$FIX_DIR/fix_am}
FIX_FV3=${FIX_FV3:-$FIX_DIR/fix_fv3}
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
SEND=${SEND:-"YES"}   #move final result to rotating directory
ERRSCRIPT=${ERRSCRIPT:-'eval [[ $err = 0 ]]'}
NDATE=${NDATE:-$NWPROD/util/exec/ndate}

# Other options
MEMBER=${MEMBER:-"-1"} # -1: control, 0: ensemble mean, >0: ensemble member $MEMBER
ENS_NUM=${ENS_NUM:-1}  # Single executable runs multiple members (e.g. GEFS)

# Model specific stuff
FCSTEXECDIR=${FCSTEXECDIR:-$BASE_GSM/sorc/fv3gfs.fd/BUILD/bin}
FCSTEXEC=${FCSTEXEC:-fv3_gfs.x}
PARM_FV3DIAG=${PARM_FV3DIAG:-$BASE_GSM/parm/parm_fv3diag}

# Model config options
APRUN_FV3=${APRUN_FV3:-${APRUN_FCST:-${APRUN:-""}}}
NTHREADS_FV3=${NTHREADS_FV3:-${NTHREADS_FCST:-${nthreads:-${nth_f:-1}}}}
cores_per_node=${cores_per_node:-${npe_node_f:-24}}
ntiles=${ntiles:-6}
NTASKS_FV3=${NTASKS_FV3:-${tasks:-$((ntiles*layout_x*layout_y))}}

TYPE=${TYPE:-"nh"}                  # choices:  nh, hydro
MONO=${MONO:-"non-mono"}            # choices:  mono, non-mono

#-------------------------------------------------------
if [ ! -d $ROTDIR ]; then mkdir -p $ROTDIR; fi
if [ ! -d $DATA ]; then mkdir -p $DATA ;fi
mkdir -p $DATA/RESTART $DATA/INPUT
cd $DATA || exit 8

#-------------------------------------------------------
# member directory
if [ $MEMBER -lt 0 ]; then
  prefix=$CDUMP
  memchar=""
else
  prefix=enkf.$CDUMP
  memchar=mem`printf %03i $MEMBER`
fi
cymd=`echo $CDATE | cut -c1-8`
chh=`echo  $CDATE | cut -c9-10`
memdir=$ROTDIR/${prefix}.$cymd/$chh/$memchar
if [ ! -d $memdir ]; then mkdir -p $memdir; fi

#-------------------------------------------------------
# initial conditions
warm_start=${warm_start:-".false."}
read_increment=${read_increment:-".false."}
increment_file=${increment_file:-$memdir/${CDUMP}.t${chh}z.atminc.nc}
restart_interval=${restart_interval:-0}

if [ $warm_start = ".false." ]; then
  if [ -d $ICSDIR/$CDATE/$CDUMP/$CASE/INPUT ]; then
    $NCP $ICSDIR/$CDATE/$CDUMP/$CASE/INPUT/* $DATA/INPUT/.
  else
    for file in $memdir/INPUT/*.nc; do
      file2=$(echo $(basename $file))
      fsuf=`echo $file2 | cut -c1-3`
      if [ $fsuf = "gfs" -o $fsuf = "sfc" ]; then
        $NLN $file $DATA/INPUT/$file2
      fi
    done
  fi
else
  if [ ${restart_test:-"NO"} = "YES" ]; then
    # start from the end of last forecast run
    $NLN $memdir/RESTART/* $DATA/INPUT/.
  else
    # Handle .res.tile?.nc and .suf.tile?.nc files for DA cycling
    for file in $memdir/RESTART/${cymd}.${chh}0000.*.nc; do
      file2=$(echo $(basename $file))
      file2=`echo $file2 | cut -d. -f3-` # remove the date from file
      fres=`echo $file2 | cut -d. -f2`
      fsuf=`echo $file2 | cut -d. -f1 | cut -c1-3`
      if [ $fres = "res" -o $fsuf = "sfc" ]; then
        $NLN $file $DATA/INPUT/$file2
      fi
    done
    # Handle coupler.res file for DA cycling
    if [ ${USE_COUPLER_RES:-"YES"} = "YES" ]; then
      # In DA, this is not really a "true restart",
      # and the model start time is the analysis time
      # The alternative is to replace
      # model start time with current model time in coupler.res
      file=$memdir/RESTART/${cymd}.${chh}0000.coupler.res
      file2=$(echo $(basename $file))
      file2=`echo $file2 | cut -d. -f3-` # remove the date from file
      $NLN $file $DATA/INPUT/$file2
    fi
    if [ $read_increment = ".true." ]; then
      if [ -f $increment_file ]; then
        $NLN $increment_file $DATA/INPUT/fv3_increment.nc
      else
        read_increment=".false."
      fi
    fi
  fi
fi
nfiles=`ls -1 $DATA/INPUT/* | wc -l`
if [ $nfiles -le 0 ]; then
  echo "Initial conditions must exist in $DATA/INPUT, ABORT!"
  exit 1
fi

#--------------------------------------------------------------------------
# Grid and orography data
for n in `seq 1 $ntiles`; do
  $NLN $FIX_FV3/$CASE/${CASE}_grid.tile${n}.nc     $DATA/INPUT/${CASE}_grid.tile${n}.nc
  $NLN $FIX_FV3/$CASE/${CASE}_oro_data.tile${n}.nc $DATA/INPUT/oro_data.tile${n}.nc
done
$NLN $FIX_FV3/$CASE/${CASE}_mosaic.nc  $DATA/INPUT/grid_spec.nc

# GFS standard input data

IALB=${IALB:-1}
IEMS=${IEMS:-1}
ISOL=${ISOL:-2}
IAER=${IAER:-111}
ICO2=${ICO2:-2}

$NLN $FIX_AM/global_solarconstant_noaa_an.txt  $DATA/solarconstant_noaa_an.txt
$NLN $FIX_AM/global_o3prdlos.f77               $DATA/INPUT/global_o3prdlos.f77
$NLN $FIX_AM/global_sfc_emissivity_idx.txt     $DATA/sfc_emissivity_idx.txt

$NLN $FIX_AM/global_co2historicaldata_glob.txt $DATA/co2historicaldata_glob.txt
$NLN $FIX_AM/co2monthlycyc.txt                 $DATA/co2monthlycyc.txt
if [ $ICO2 -gt 0 ]; then
  for file in `ls $FIX_AM/fix_co2_proj/global_co2historicaldata* ` ; do
    $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
  done
fi

$NLN $FIX_AM/global_climaeropac_global.txt     $DATA/aerosol.dat
if [ $IAER -gt 0 ] ; then
  for file in `ls $FIX_AM/global_volcanic_aerosols* ` ; do
    $NLN $file $DATA/$(echo $(basename $file) | sed -e "s/global_//g")
  done
fi

FNGLAC=${FNGLAC:-"$FIX_AM/global_glacier.2x2.grb"}
FNMXIC=${FNMXIC:-"$FIX_AM/global_maxice.2x2.grb"}
FNTSFC=${FNTSFC:-"$FIX_AM/RTGSST.1982.2012.monthly.clim.grb"}
FNSNOC=${FNSNOC:-"$FIX_AM/global_snoclim.1.875.grb"}
FNZORC=${FNZORC:-"igbp"}
FNALBC=${FNALBC:-"$FIX_AM/global_snowfree_albedo.bosu.t1534.3072.1536.rg.grb"}
FNALBC2=${FNALBC2:-"$FIX_AM/global_albedo4.1x1.grb"}
FNAISC=${FNAISC:-"$FIX_AM/CFSR.SEAICE.1982.2012.monthly.clim.grb"}
FNTG3C=${FNTG3C:-"$FIX_AM/global_tg3clim.2.6x1.5.grb"}
FNVEGC=${FNVEGC:-"$FIX_AM/global_vegfrac.0.144.decpercent.grb"}
FNVETC=${FNVETC:-"$FIX_AM/global_vegtype.igbp.t1534.3072.1536.rg.grb"}
FNSOTC=${FNSOTC:-"$FIX_AM/global_soiltype.statsgo.t1534.3072.1536.rg.grb"}
FNSMCC=${FNSMCC:-"$FIX_AM/global_soilmgldas.t1534.3072.1536.grb"}
FNMSKH=${FNMSKH:-"$FIX_AM/seaice_newland.grb"}
FNVMNC=${FNVMNC:-"$FIX_AM/global_shdmin.0.144x0.144.grb"}
FNVMXC=${FNVMXC:-"$FIX_AM/global_shdmax.0.144x0.144.grb"}
FNSLPC=${FNSLPC:-"$FIX_AM/global_slope.1x1.grb"}
FNABSC=${FNABSC:-"$FIX_AM/global_mxsnoalb.uariz.t1534.3072.1536.rg.grb"}

# Warm start and read increment, update surface variables for GDAS cycle only
# since we do not have SST, SNOW or ICE via global_cycle
if [ $CDUMP = "gdas" -a $warm_start = ".true." -a $read_increment = ".true." ]; then
  FNTSFA=${FNTSFA:-"$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.rtgssthr.grb"}
  FNACNA=${FNACNA:-"$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.seaice.5min.blend.grb"}
  FNSNOA=${FNSNOA:-"$DMPDIR/$CDATE/$CDUMP/${CDUMP}.t${chh}z.snogrb_t1534.3072.1536"}
  FTSFS=${FTSFS:-0}
  FAISS=${FAISS:-0}
  FAISL=${FAISL:-0}
  FSMCL2=${FSMCL2:-60}
  FSMCL3=${FSMCL3:-60}
  FSMCL4=${FSMCL4:-60}
  [[ $chh = 18 ]] && FSNOL=${FSNOL:-"-2"}
fi

# NSST Options
# nstf_name contains the NSST related parameters
# nstf_name(1) : 0 = NSSTM off, 1 = NSSTM on but uncoupled, 2 = NSSTM on and coupled
# nstf_name(2) : 0 = NSSTM spin up off, 1 = NSSTM spin up on,
# nstf_name(3) : 0 = NSSTM analysis off, 1 = NSST analysis on
# nstf_name(4) : zsea1 in mm
# nstf_name(5) : zsea2 in mm
# nst_anl      : .true. or .false., NSST analysis over lake
nstf_name=${nstf_name:-"0,0,0,0,0"}
nst_anl=${nst_anl:-".false."}

#------------------------------------------------------------------
# changeable parameters
# dycore definitions
res=`echo $CASE |cut -c2-5`
resp=`expr $res + 1 `
npx=$resp
npy=$resp
npz=`expr $LEVS - 1 `
io_layout="1,1"
#ncols=$(( (${npx}-1)*(${npy}-1)*3/2 ))

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
ncep_plevels=${ncep_plevels:-".true."}
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

if [ `echo ${MONO} | cut -c-4` = "mono" ];  then # monotonic options

  d_con=${d_con:-"0."}
  do_vort_damp=${do_vort_damp:-".false."}
  if [ ${TYPE} = "nh" ]; then # non-hydrostatic
    hord_mt=${hord_mt:-"10"}
    hord_xx=${hord_xx:-"10"}
  else # hydrostatic
    hord_mt=${hord_mt:-"10"}
    hord_xx=${hord_xx:-"10"}
  fi

else # non-monotonic options

  d_con=${d_con:-"1."}
  do_vort_damp=".true."
  if [ ${TYPE} = "nh" ]; then # non-hydrostatic
    hord_mt=${hord_mt:-"5"}
    hord_xx=${hord_xx:-"5"}
  else # hydrostatic
    hord_mt=${hord_mt:-"10"}
    hord_xx=${hord_xx:-"10"}
  fi

fi

if [ `echo ${MONO} | cut -c-4` != "mono" -a ${TYPE} = "nh" ]; then
  vtdm4=${vtdm4:-"0.03"}
else
  vtdm4=${vtdm4:-"0.05"}
fi

if [ $warm_start = ".true." ]; then # warm start from restart file

  nggps_ic=".false."
  ncep_ic=".false."
  external_ic=".false."
  mountain=".true."
  if [ $read_increment = ".true." ]; then # add increment on the fly to the restarts
    res_latlon_dynamics="fv3_increment.nc"
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

# build the date for curr_date and diag_table from CDATE
SYEAR=`echo $CDATE | cut -c1-4`
SMONTH=`echo $CDATE | cut -c5-6`
SDAY=`echo $CDATE | cut -c7-8`
SHOUR=`echo $CDATE | cut -c9-10`
curr_date="${SYEAR},${SMONTH},${SDAY},${SHOUR},0,0"
rsecs=$((restart_interval*3600))
restart_secs=${rsecs:-0}

# copy over the tables
DIAG_TABLE=${DIAG_TABLE:-$PARM_FV3DIAG/diag_table}
DATA_TABLE=${DATA_TABLE:-$PARM_FV3DIAG/data_table}
FIELD_TABLE=${FIELD_TABLE:-$PARM_FV3DIAG/field_table}

# build the diag_table with the experiment name and date stamp
cat > diag_table << EOF
FV3 Forecast
$SYEAR $SMONTH $SDAY $SHOUR 0 0
EOF
cat $DIAG_TABLE >> diag_table

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

rm -f model_configure
cat > model_configure <<EOF
total_member:            $ENS_NUM
PE_MEMBER01:             $NTASKS_FV3
start_year:              $SYEAR
start_month:             $SMONTH
start_day:               $SDAY
start_hour:              $SHOUR
start_minute:            0
start_second:            0
nhours_fcst:             $FHMAX
RUN_CONTINUE:            ${RUN_CONTINUE:-".false."}
ENS_SPS:                 ${ENS_SPS:-".false."}

dt_atmos:                $DELTIM
calendar:                ${calendar:-'julian'}
memuse_verbose:          ${memuse_verbose:-".false."}
atmos_nthreads:          $NTHREADS_FV3
use_hyper_thread:        ${hyperthread:-".false."}
ncores_per_node:         $cores_per_node
restart_interval:        $restart_interval
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

&fms_nml
  clock_grain = 'ROUTINE'
  domains_stack_size = ${domains_stack_size:-115200}
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
  n_sponge = ${n_sponge:-"24"}
  nudge_qv = ${nudge_qv:-".true."}
  tau = ${tau:-"5."}
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
  nwat = 2
  na_init = $na_init
  d_ext = 0.
  dnats = 0
  fv_sg_adj = ${fv_sg_adj:-"450"}
  d2_bg = 0.
  nord = ${nord:-"2"}
  dddmp = ${dddmp:-"0.1"}
  d4_bg = ${d4_bg:-"0.12"}
  vtdm4 = $vtdm4
  delt_max = ${delt_max:-"0.002"}
  ke_bg = 0.
  do_vort_damp = $do_vort_damp
  external_ic = $external_ic
  read_increment = $read_increment
  res_latlon_dynamics = $res_latlon_dynamics
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
  consv_am = .false.
  fill = .true.
  dwind_2d = .false.
  print_freq = $print_freq
  warm_start = $warm_start
  no_dycore = $no_dycore
  z_tracer = .true.
  agrid_vel_rst = ${agrid_vel_rst:-".true."}
  $fv_core_nml
/

&external_ic_nml
  filtered_terrain = $filtered_terrain
  ncep_plevels = $ncep_plevels
  levp = $LEVS
  gfs_dwinds = $gfs_dwinds
  checker_tr = .false.
  nt_checker = 0
  $external_ic_nml
/

##  ntoz        = ${ntoz:-2}
##  ntcw        = ${ntcw:-3}
&gfs_physics_nml
  fhzero      = $FHZER
  ldiag3d     = ${ldiag3d:-".false."}
  fhcyc       = $FHCYC
  use_ufo     = ${use_ufo:-".true."}
  pre_rad     = ${pre_rad:-".false."}
  ncld        = ${ncld:-1}
  zhao_mic    = ${zhao_mic:-".true."}
  pdfcld      = ${pdfcld:-".false."}
  fhswr       = ${FHSWR:-"3600."}
  fhlwr       = ${FHLWR:-"3600."}
  ialb        = $IALB
  iems        = $IEMS
  iaer        = $IAER
  ico2        = $ICO2
  isubc_sw    = ${isubc_sw:-"2"}
  isubc_lw    = ${isubc_lw:-"2"}
  isol        = $ISOL
  lwhtr       = ${lwhtr:-".true."}
  swhtr       = ${swhtr:-".true."}
  cnvgwd      = ${cnvgwd:-".true."}
  shal_cnv    = ${shal_cnv:-".true."}
  cal_pre     = ${cal_pre:-".true."}
  redrag      = ${redrag:-".true."}
  dspheat     = ${dspheat:-".true."}
  hybedmf     = ${hybedmf:-".true."}
  random_clds = ${random_clds:-".true."}
  trans_trac  = ${trans_trac:-".true."}
  cnvcld      = ${cnvcld:-".true."}
  imfshalcnv  = ${imfshalcnv:-"2"}
  imfdeepcnv  = ${imfdeepcnv:-"2"}
  cdmbgwd     = ${cdmbgwd:-"3.5,0.25"}
  prslrd0     = ${prslrd0:-"0."}
  ivegsrc     = ${ivegsrc:-"1"}
  isot        = ${isot:-"1"}
  debug       = ${gfs_phys_debug:-".false."}
  nstf_name   = $nstf_name
  nst_anl     = $nst_anl
  $gfs_physics_nml
/

&nggps_diag_nml
  fdiag = ${fdiag:-$FHOUT}
  $nggps_diag_nml
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
  ntrunc = ${JCAP:-$((`echo $CASE | cut -c 2-`*2-2))}
  lon_s = ${LONB:-$((`echo  $CASE | cut -c 2-`*4))}
  lat_s = ${LATB:-$((`echo  $CASE | cut -c 2-`*2))}
EOF

  if [ ${DO_SKEB:-".false."} = ".true." ]; then
    [[ ${SET_STP_SEED:-"NO"} = "YES" ]] && ISEED_SKEB=$((CDATE*1000 + MEMBER*10 + 1))
    cat >> input.nml << EOF
  skeb = $SKEB
  iseed_skeb = ${ISEED_SKEB:-${ISEED:-"0"}}
  skeb_tau = ${SKEB_TAU:-"-999."}
  skeb_lscale = ${SKEB_LSCALE:-"-999."}
  skebnorm = ${SKEBNORM:-"1"}
EOF
  fi

  if [ ${DO_SHUM:-".false."} = ".true." ]; then
    [[ ${SET_STP_SEED:-"NO"} = "YES" ]] && ISEED_SHUM=$((CDATE*1000 + MEMBER*10 + 2))
    cat >> input.nml << EOF
  shum = $SHUM
  iseed_shum = ${ISEED_SHUM:-${ISEED:-"0"}}
  shum_tau = ${SHUM_TAU:-"-999."}
  shum_lscale = ${SHUM_LSCALE:-"-999."}
EOF
  fi

  if [ ${DO_SPPT:-".false."} = ".true." ]; then
    [[ ${SET_STP_SEED:-"NO"} = "YES" ]] && ISEED_SPPT=$((CDATE*1000 + MEMBER*10 + 3))
    cat >> input.nml << EOF
  sppt = $SPPT
  iseed_sppt = ${ISEED_SPPT:-${ISEED:-"0"}}
  sppt_tau = ${SPPT_TAU:-"-999."}
  sppt_lscale = ${SPPT_LSCALE:-"-999."}
  sppt_logit = ${SPPT_LOGIT:-".true."}
  sppt_sfclimit = ${SPPT_SFCLIMIT:-".true."}
EOF
  fi

  cat >> input.nml << EOF
  $nam_stochy_nml
/
EOF

else

  cat >> input.nml << EOF
&nam_stochy
/
EOF

fi

#------------------------------------------------------------------
# setup the runtime environment and run the executable
cd $DATA
$NCP $FCSTEXECDIR/$FCSTEXEC $DATA/.
export OMP_NUM_THREADS=$NTHREADS_FV3
$APRUN_FV3 $DATA/$FCSTEXEC 1>&1 2>&2

export ERR=$?
export err=$ERR
$ERRSCRIPT || exit 2

#------------------------------------------------------------------
if [ $SEND = "YES" ]; then
  # Copy model output files
  cd $DATA
  for n in `seq 1 $ntiles`; do
    for file in *.tile${n}.nc; do
      $NCP $file $memdir/.
    done
  done

  # Copy model restart files
  # if restart_interval = 6
  #     Only save the first time in the interval (6) at the restart time directory
  #     This is a DA requirement
  #     If other restart times are needed, logic will be added later
  # else
  #     Save the all other restart times in the initial restart directory
  #     This logic was introduced by Fanglin
  cd $DATA/RESTART
  if [ $restart_interval -ne 0 -a $restart_interval -eq 6 ]; then
    # Only save the restart files at 6 hours in relevant directory as needed for DA
    RDATE=`$NDATE +$restart_interval $CDATE`
    rymd=`echo $RDATE | cut -c1-8`
    rhh=`echo  $RDATE | cut -c9-10`
    rmemdir=$ROTDIR/${prefix}.$rymd/$rhh/$memchar
    mkdir -p $rmemdir/RESTART
    for file in ${rymd}.${rhh}0000.* ; do
      $NCP $file $rmemdir/RESTART/$file
    done
  else
    # Save restart files at the end of the forecast in ROTDIR/RESTART directory
    mkdir -p $memdir/RESTART
    for file in `ls * |grep -v 0000`  ; do
      $NCP $file $memdir/RESTART/$file
    done
  fi
fi

#------------------------------------------------------------------
# Clean up before leaving
if [ ${KEEPDATA:-"NO"} = "NO" ]; then rm -rf $DATA; fi

#------------------------------------------------------------------
set +x
if [ "$VERBOSE" = "YES" ] ; then
  echo $(date) EXITING $0 with return code $err >&2
fi
exit $err

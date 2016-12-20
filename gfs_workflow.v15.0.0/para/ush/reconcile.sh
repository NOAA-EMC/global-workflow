#!/bin/ksh
set -ax
#
#   This reconcile file was created by Shrinivas Moorthi in March/April 2007
#   for GFS and subseqently updated for CFS
#   With this version forecasts can be made using two model resolutions.
#
CDATE=${CDATE:-2011020100}
CDATE_SKIP=${CDATE_SKIP:-0}
GROUP=${GROUP:-devonprod}
NWPROD=${NWPROD:-/nwprod}
if [ $NWPROD = /nwprod ] ; then
 PARMSUBDA=${PARMSUBDA:-parm}
 PARMSUBDP=${PARMSUBDP:-parm}
 FIXSUBDA=${FIXSUBDA:-fix}
 FIXSUBDP=${FIXSUBDP:-fix}
fi
scrubtyp=${scrubtyp:-noscrub}
NDATE=${NDATE:-$NWPROD/util/exec/ndate}
NHOUR=${NHOUR:-$NWPROD/util/exec/nhour}

RUN_ENVIR=${RUN_ENVIR:-dev}

USE_HPSS=${USE_HPSS:-YES}
#
# Depending on the machine define certain prefixes to the directory path
#
machine=${machine:-WCOSS}

if [ $machine = GAEA ] ; then

  rdir=/lustre/fs/scratch
  lfsdir=$rdir/$LOGNAME
  DMPDIR=${DMPDIR:-$rdir/shared/glopara}
  memory_node=${memory_node:-64000}
  USE_HPSS=NO

 elif [ $machine = THEIA ] ; then

  rdir=/scratch4/NCEPDEV/global
  PREPTMP=/scratch4/NCEPDEV
  PTMP=${PTMP:-/scratch4/NCEPDEV/stmp3}
  STMP=${STMP:-/scratch4/NCEPDEV/stmp4}
  lfsdir=$rdir/save/$LOGNAME
  DMPDIR=${DMPDIR:-$rdir/shared/glopara}
  memory_node=${memory_node:-60000}
  USE_HPSS=YES
  POE=NO
  pe_node=${pe_node:-24}

elif [ $machine = WCOSS -o $machine = WCOSS_C ] ; then

  rdir=${TOPDIR:-/global}
  lfsdir=$rdir/save/$LOGNAME
  DMPDIR=${DMPDIR:-$rdir/shared/glopara}
  memory_node=${memory_node:-28000}
  USE_HPSS=YES
  POE=NO
  PTMP=${PTMP:-/ptmpp1}
  STMP=${STMP:-/stmpp1}
  pe_node=${pe_node:-16}

fi

TOPDIR=${TOPDIR:-$rdir}
TOPDRG=${TOPDRG:-$rdir}
TOPDRC=${TOPDRC:-$rdir}
TOPDRA=${TOPDRA:-$rdir}
TOPDRM=${TOPDRM:-$rdir}
DISK_GLOB=${DISK_GLOB:-${TOPDRG:-$rdir}/save}
#DISK_CLIM=${DISK_CLIM:-${TOPDRC:-/climate}/save}
#DISK_ANAL=${DISK_ANAL:-${TOPDRA:-$rdir}/save}
#DISK_TRAK=${DISK_TRAK:-${TOPDRG:-$rdir}/save}
#DISK_MESO=${DISK_MESO:-${TOPDRM:-$rdir}/save}
ROTDIR=${ROTDIR:-$rdir}
DAYDIR=${DAYDIR:-$rdir}
COMDMP=${COMDMP:-'$DMPDIR/$CDATE/${CDUMP}x,$DMPDIR/$CDATE/$CDUMP'}
COMCOP=${COMCOP:-'$DMPDIR/$CDATE/$CDUMP'}
PTMP=${PTMP:-''}
STMP=${STMP:-''}

EXPDIR=${EXPDIR:-$TOPDIR/save/$LOGNAME/para_exp/pr$PSLOT}
mkdir -p $EXPDIR
#
BASEDIR=${BASEDIR:-$NWPROD}
SHDIR=${SHDIR:-$BASEDIR/bin}
JOBSDIR=${JOBSDIR:-$BASEDIR/jobs}
USHDIR=${USHDIR:-$BASEDIR/ush}
USHPREV=${USHPREV:-$USHDIR}
USHSYND=${USHSYND:-$USHDIR}
SCRDIR=${SCRDIR:-$BASEDIR/scripts}
#
PSLOT=${PSLOT:-x}
EDATE=${EDATE:-$CDATE}
CDUMP=${CDUMP:-gdas}
CUE2RUN=${CUE2RUN:-dev}
ACCOUNT=${ACCOUNT:-GFS-T2O}
USE_RESTART=${USE_RESTART:-NO}
CCPOST=${CCPOST:-NO}
NEMS=${NEMS:-YES}

# File format settings

export use_gfs_nemsio=${use_gfs_nemsio:-.false.}

export NEMSIO_IN=${NEMSIO_IN:-.true.}
export NEMSIO_OUT=${NEMSIO_OUT:-.true.}
export SIGIO_IN=${SIGIO_IN:-.false.}
export SIGIO_OUT=${SIGIO_OUT:-.false.}
export SFCIO_OUT=${SFCIO_OUT:-.false.}

if [ $NEMSIO_IN = .true. ]; then
  export SIGISUF='gfnanl'
  export SFCISUF='sfnanl'
  export NSTISUF='nsnanl'
  export FLXISUF='flnanl'
elif [ $SIGIO_IN = .true. ]; then
  export SIGISUF='siganl'
  export SFCISUF='sfcanl'
  export NSTISUF='nstanl'
  export FLXISUF='flxanl'
fi
export OCNISUF='ocnanl'
if [ $NEMSIO_OUT = .true. ]; then
  export SIGOSUF='gfn'
  export SFCOSUF='sfn'
  export NSTOSUF='nsn'
  export FLXOSUF='fln'
else
  export SIGOSUF='sig'
  export SFCOSUF='sfc'
  export NSTOSUF='nst'
  export FLXOSUF='flx'
fi 

# NEMS variables

export WRITE_DOPOST=${WRITE_DOPOST:-.false.}

# Adjust SIGISUF/SFCISUF if CSTEP=fcst2

if [ $CSTEP = fcst2 ]; then
  export SIGISUF=${SIGOSUF}f${fmax1}
  export SFCISUF=${SFCOSUF}f${fmax1}
  export NSTISUF=${NSTOSUF}f${fmax1}
fi

# Define Resolution - Defaults to T38264 Noah
JCAP=${JCAP:-574}
LEVS=${LEVS:-64}
LSOIL=${LSOIL:-4}
if [ $LSOIL -eq 2 ] ; then
  lsm=${lsm:-0}    # This will run OSU LSM option
fi
lsm=${lsm:-1}      # lsm=1 is for NOAH land model (=0 for OSU model)
                   # Some defaults are set if not defined
ENTHALPY=${ENTHALPY:-NO}
Apercent=${Apercent:-100}
if [ $ENTHALPY = YES ] ; then
 SFCPRESS_ID=${SFCPRESS_ID:-2}
 THERMODYN_ID=${THERMODYN_ID:-3}
 IDVM=${IDVM:-${THERMODYN_ID}${SFCPRESS_ID}}
 ivssig=${ivssig:-200509}
 nvcoord=3 ; LATCH=${LATCH:-8}
#***************************************************************
#                            Dry ,     H2O,     O3,         CLW
 CPIlist=${CPIlist:-" 1004.6,   1846.0, 820.2391,    0.0"}
 RIlist=${RIlist:-"   287.05,   461.50, 173.2247,    0.0"}

#***************************************************************
 TRACERVARS="RI=$RIlist,CPI=$CPIlist,"
fi
#
NOANAL=${NOANAL:-NO}
OPANAL_06=${OPANAL_06:-NO}
CKSH=${CKSH:-null}
#
# To run forecasts from existing analysis
#
if [[ $NOANAL = YES ]] ; then
  if [ $lsm -eq 0 ] ; then #  This section for input containing landice
    landice=4         # Input landice (Noah LSM), fcst with OSU LSM
  else
    landice=2         # Input landice and forecast with landice (Noah LSM)
  fi
#   This is for using the (old-prelandice) operational analysis
#   -----------------------------------------------------------
  if [ $OPANAL_06 = YES ] ; then
    if [[ $CDATE -lt 2006082200 ]] ; then
      if [ $lsm -eq 0 ] ; then
        landice=3      # Input no landice (Noah LSM), fcst with OSU LSM
      else
        landice=1      # Input no landice but landice in fcst (Noah LSM)
      fi
    fi
  fi
fi
#
if [ $CKSH = copy ] ; then
#   This is to start a new experiment from the (old) operational analysis
  if [ $OPANAL_06 = YES ] ; then
    if [[ $CDATE -lt 2006082200 ]] ; then
      if [ $lsm -eq 0 ] ; then
        landice=3      # Input no landice (Noah LSM), fcst with OSU LSM
      else
        landice=1      # Input no landice but landice in fcst (Noah LSM)
      fi
    else
      if [ $lsm -eq 0 ] ; then
        landice=4      # Input no landice (Noah LSM), fcst with OSU LSM
      else
        landice=2      # Input no landice but landice in fcst (Noah LSM)
      fi
    fi
  fi
fi
NCEPPOST=${NCEPPOST:-NO}
OUTTYP=${OUTTYP:-1}
if [ $CKSH = post -o $CKSH = d3dp -a $NCEPPOST = YES -a $OUTTYP -ne 3 ] ; then
 OUTTYP=${OUTTYP_NP:-1}
elif [ $CKSH = fcst -o $CKSH = ecen ] ; then
 OUTTYP=${OUTTYP_F:-$OUTTYP}
fi

landice=${landice:-2}
landice2=${landice2:-2}
#
#    Number of ensemble members
#
ENS_NUM=${ENS_NUM:-1}
#
###########################################################################
#
#   If ensemble numbers are > 1 the following variable need to be defined
#   ---------------------------------------------------------------------
#FM='_01'
#ENS_NUM=2
#MEMBER_NAMES='_01 _02'
#ENS_NUM=5
#MEMBER_NAMES='_01 _02 _03 _04 _05'
#ENS_NUM=11
#MEMBER_NAMES='_01 _02 _03 _04 _05 _06 _07 _08 _09 _10 _11'
###########################################################################
#
newoz_gsfc=${newoz_gsfc:-NO}
newozopr=${newozopr:-NO}
newoz_nrl=${newoz_nrl:-YES}
#
ivssfc=${ivssfc:-200509}
#ivssig=${ivssig:-198410}
#
#
FHLWR=${FHLWR:-3600}
FHSWR=${FHSWR:-3600}
fmon_1=${fmon_1:-0}
fmon_2=${fmon_2:-0}
if [ $fmon_1 -gt 0 ] ; then
 start_date=$CDATE
 YYYYSS=$(echo $start_date | cut -c1-4)
  MMSS=$(echo $start_date | cut -c5-6)
  YYYYEE=$YYYYSS
  MMEE=$((MMSS+fmon_1+1))
  while [[ $MMEE -gt 12 ]] ; do
    MMEE=$((MMEE-12))
    YYYYEE=$((YYYYEE+1))
  done
  if [[ $MMEE -lt 10 ]] ; then MMEE=0$MMEE; fi
  end_date=${YYYYEE}${MMEE}0100
  fmax1=$($NHOUR $end_date $start_date)
fi
#
LONG_FCST=${LONG_FCST:-NO}
#
fmax1=${fmax1:-09}
if [ $fmax1 -le 09 ] ; then
 nc=$(echo $fmax1 | wc -c)
 fmax1=0$(echo $fmax1 | cut -c $((nc-1))-$((nc-1)))
fi

gdas_fh=${gdas_fh:-999}  ; # defaults to 999 i.e. no long fcst in GDAS step
gdas_fh2=${gdas_fh2:-$gdas_fh}
fseg=${fseg:-1}
fseg_orig=$fseg
if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 -a $fmax1 -gt 9 ] ; then
 fseg=2
 fmax2=$fmax1
 #fmax1=12
 fmax1=09
 fres1=06
 io_2=${io_1:-360}
 jo_2=${jo_1:-181}
 io_1=${io_a:-720}
 jo_1=${jo_a:-361}
 fout1=01
 USE_RESTART=YES
 ntrn_2=2
 #fres1=$fmax1
 fres2=${fres2:-24}
 fbak2=$fmax1
 tasks2=${tasks2:-$tasks}
fi
#
#
fres1=${fres1:-24}
fout1=${fout1:-03}
fzer1=${fzer1:-06}
ntrn_1=${ntrn_1:-1}
inch_1=${inch_1:-$fmax1}
#
fres2=${fres2:-$fres1}
fout2=${fout2:-06}
fzer2=${fzer2:-06}
ntrn_2=${ntrn_2:-0}
fini2=${fini2:-${fmax1:-0}}
inch_2=${inch_2:-360}
#
fdfi_a=${fdfi_a:-00}
fdfi_1=${fdfi_1:-03}
fdfi_2=${fdfi_2:-00}
#
fout_a=${fout_a:-3}
fmax1=${fmax1:-180}
fseg=${fseg:-1}
if [ $fseg -eq 2 ] ; then
 fmax2=${fmax2:-384}
else
 fmax2=$fmax1
fi
fbak2=${fbak2:-$((fmax1-12))}
if [ $fbak2 -lt 0 ] ; then fbak2=0 ; fi

if [ $fbak2 -eq 0 -a $landice -eq 1 ] ; then landice2=$landice ; fi

fcyc=${fcyc:-24}
pefac=${pefac:-2}
#
tasksp_1=${tasksp_1:-16}
tasksp_2=${tasksp_2:-${tasksp_1:-16}}
tasksop_1=${tasksop_1:-12}
tasksop_2=${tasksop_2:-${tasksop_1:-12}}
#
if [ $fmon_2 -gt 0 ] ; then
 start_date=$($NDATE $fbak2 $CDATE)
 YYYYSS=$(echo $start_date | cut -c1-4)
  MMSS=$(echo $start_date | cut -c5-6)
  YYYYEE=$YYYYSS
  MMEE=$((MMSS+fmon_2+1))
  while [[ $MMEE -gt 12 ]] ; do
    MMEE=$((MMEE-12))
    YYYYEE=$((YYYYEE+1))
  done
  if [[ $MMEE -lt 10 ]] ; then MMEE=0$MMEE; fi
  end_date=${YYYYEE}${MMEE}0100
  fmax2=$($NHOUR $end_date $start_date)
fi
#
NEW_DAYFILE=${NEW_DAYFILE:-NO}
COUP_FCST=${COUP_FCST:-NO}
if [ $COUP_FCST = YES ] ; then
#dt_cpld=${dt_cpld:-3600}
#dt_ocean=${dt_ocean:-3600}
 dt_rstrt=${dt_rstrt:-10800}
 dt_rstrt_long=${dt_rstrt_long:-86400}
 dt_rstrt_long_1=${dt_rstrt_long_1:-${dt_rstrt_long:-86400}}
 dt_rstrt_long_2=${dt_rstrt_long_2:-${dt_rstrt_long:-86400}}
 pfac=${pfac:-10}
 if [ $fbak2 -lt $fmax1 ] ; then
   dt_rstrt_long=$(((fmax1-fbak2)*3600))
   dt_rstrt_long_2=$(((fmax1-fbak2)*3600))
 fi
fi
#
#             Output resolution - default values
#
io_1=${io_1:-720}
jo_1=${jo_1:-361}
ko_a=${ko_a:-47}
ko_1=${ko_1:-47}
kto_1=${kto_1:-0}
grid11_1=${grid11_1:-0}
grid25_1=${grid25_1:-0}
grid62_1=${grid62_1:-0}
ld3d_1=${ld3d_1:-.false.}            # For 3D diagnostics
lg3d_1=${lg3d_1:-.false.}            # For gocart
#                                      For OM
omres_1=${omres_1:-${omres:-05}}
nproco_1=${nproco_1:-${nproco:-60}}

io_2=${io_2:-$io_1}
jo_2=${jo_2:-$jo_1}
ko_2=${ko_2:-$ko_1}
kto_2=${kto_2:-$kto_1}
grid11_2=${grid11_2:-0}
grid25_2=${grid25_2:-0}
grid62_2=${grid62_2:-0}
ld3d_2=${ld3d_2:-$ld3d_1}
lg3d_2=${lg3d_2:-$lg3d_1}
#                                       For OM
omres_2=${omres_2:-$omres_1}
nproco_2=${nproco_2:-$nproco_1}

io_a=${io_a:-$io_1}
jo_a=${jo_a:-$jo_1}
ko_a=${ko_a:-$ko_1}

if [ $ko_1 -ne 26 ] ; then
 export polist_$ko_1=$(eval echo \${polist_$ko_1})
fi
if [ $fseg -eq 2 -a $ko_2 -ne $ko_1 ] ; then
 if [ $ko_2 -ne 26 ] ; then export polist_$ko_2=$(eval echo \${polist_$ko_2}) ;  fi
fi
#           For output on isentropic surfaces
#
if [ $kto_1 -ne 16  -a $kto_1 -ne 0 ] ; then
  thlist_$kto_1=$(eval echo \${thlist_$kto_1})
fi
if [ $fseg -eq 2 -a $kto_2 -ne $kto_1 ] ; then
  if [ $kto_2 -ne 16  -a $kto_2 -ne 0 ] ; then
    thlist_$kto_2=$(eval echo \${thlist_$kto_2})
 fi
fi
#
#NUM_THREADS=${NUM_THREADS:-1}
#
#   Number of cycles - if GSI can handle, then it can take values
#   ----------------    1, 2, 3, 4, 6, 8, 12.
#
cyc_start_hr=${cyc_start_hr:-0}
gdas_cyc=${gdas_cyc:-4}          ; # defaults to 4 cycles of GDAS
gfs_cyc=${gfs_cyc:-1}            ; # defaults to 1 cycle  of GFS
ldas_cyc=${ldas_cyc:-0}          ; # defaults to 0 ldas   cycle
odas_cyc=${odas_cyc:-$gdas_cyc}  ; # defaults to gdas_cyc
if [ $COUP_FCST = NO ] ; then odas_cyc=0 ; fi
gdas_fh=${gdas_fh:-999}  ; # defaults to 999 i.e. no long fcst in GDAS step
#export gdas_cyc
if [ $gdas_fh -lt 999 -a $fseg -lt 2 ] ; then
 fseg=2
fi
fcst_cyc=-1
#
start_mm_dd_hh=${start_mm_dd_hh:-010100}
if [[ $gdas_cyc -eq $((24/gdas_fh)) ]] ;then
 fcst_cyc=$(echo $CDATE | cut -c9-10)
elif [ $gdas_fh -lt 999 ] ; then
 set +x
 YYYY=$(echo $CDATE | cut -c1-4)
#MM=$(echo $CDATE | cut -c5-6)
#DD=$(echo $CDATE | cut -c7-8)
#next_year=$((YYYY+1))010100
 if [ ${SKIP_LEAP:-YES} = YES -a $(((YYYY/4)*4-YYYY)) -eq 0 ] ; then
   LEAP_SKIP=YES
 else
   LEAP_SKIP=NO
 fi
 fcst_date=${YYYY}$start_mm_dd_hh
#while [ $fcst_date -lt $next_year -a $fcst_cyc -eq -1 ] ; do
 while [ $fcst_date -le $CDATE -a $fcst_cyc -eq -1 ] ; do
  if [ $fcst_date -eq $CDATE ] ; then
    fcst_cyc=$(echo $fcst_date | cut -c9-10)
  fi
  fcst_date=$($NDATE $gdas_fh $fcst_date)
  if [ $LEAP_SKIP = YES -a $(((24/gdas_fh)*gdas_fh)) -ne 24 ] ; then
    MMN=$(echo $fcst_date | cut -c5-6)
    DDN=$(echo $fcst_date | cut -c7-8)
    if [ ${MMN}${DDN} = '0229' -o $MMN -gt 2 ] ; then 
      LEAP_SKIP=NO
      fcst_date=$($NDATE 24 $fcst_date)
    fi
  fi
 done
fi
set -x
echo 'fcst_cyc= '$fcst_cyc
#
if [ $gdas_cyc -lt $gfs_cyc ] ; then
 echo ' Number of GDAS cycle must >= number of GFS cycles'
 exit
fi

if [ $gdas_cyc -gt 0 ] ; then
 set -A all_gdas $gdas_cyc
 n=-1
 while [ $((n+=1)) -lt $gdas_cyc ] ; do
  hr=$((n*(24/gdas_cyc)+cyc_start_hr))
  if [ $hr -lt 10 ] ; then hr=0$hr ; fi
  all_gdas[n]=$hr
  all_gfs[n]=0
 done
fi
#
if [ $gfs_cyc -gt 0 ] ; then
 set -A all_gfs $gfs_cyc
 n=-1
 while [ $((n+=1)) -lt $gfs_cyc ] ; do
  hr=$((n*(24/gfs_cyc)+cyc_start_hr))
  if [ $hr -lt 10 ] ; then hr=0$hr ; fi
  all_gfs[n]=$hr
 done
fi
FDMP=${FDMP:-GFS}
if [ $gfs_cyc -le 0 ] ; then FDMP=GDAS ; fi
#
if [ $ldas_cyc -gt 0 ] ; then
 set -A all_ldas $ldas_cyc
 n=-1
 while [ $((n+=1)) -lt $ldas_cyc ] ; do
  hr=$((n*(24/ldas_cyc)+cyc_start_hr))
  if [ $hr -lt 10 ] ; then hr=0$hr ; fi
  all_ldas[n]=$hr
 done
fi
#
if [ $odas_cyc -gt 0 ] ; then
 set -A all_odas $odas_cyc
 n=-1
 while [ $((n+=1)) -lt $odas_cyc ] ; do
  hr=$((n*(24/odas_cyc)+cyc_start_hr))
  if [ $hr -lt 10 ] ; then hr=0$hr ; fi
  all_odas[n]=$hr
 done
fi
#
#  For monthly averaging - assumed to be done at 0100 of each month
#
MON_AVG=${MON_AVG:-YES}
if [ $MON_AVG = YES ] ; then
 MAVRG00GDAS=1
 MAVRG00GFS=1
fi

#                Specify the cycle for which the analysis is done (i.e. "1")
n=-1
while [ $((n+=1)) -lt $gdas_cyc ] ; do
 cyc=${all_gdas[n]}
 export MOANL${cyc}GDAS=0
 export MLANL${cyc}GDAS=0
 export MANAL${cyc}GFS=0
#                If NOANAL=YES, then no analysis - Forecasts only
 if [[ $NOANAL != YES ]] ; then
  ngfs=$((n*gfs_cyc/gdas_cyc))
  if [ $gfs_cyc -gt $ngfs ] ; then
   if [ $cyc -eq ${all_gfs[ngfs]} ] ; then
     export MANAL${cyc}GFS=1      ;# GFS analysis will be done at these cycles
   fi
  fi
  nlds=$((n*ldas_cyc/gdas_cyc))
  if [ $ldas_cyc -gt $nlds ] ; then
   if [ $cyc -eq ${all_ldas[nlds]} ] ; then
     export MLANL${cyc}GDAS=1      ;# LDAS will be done at these cycles
   fi
  fi
  nods=$((n*odas_cyc/gdas_cyc))
  if [ $odas_cyc -gt $nods ] ; then
   if [ $cyc -eq ${all_odas[nods]} ] ; then
     export MOANL${cyc}GDAS=1      ;# GODAS will be done at these GDAS cycles
    if [ $(eval echo \${MANAL${cyc}GFS}) = 1 ] ; then
      export MOANL${cyc}GFS=1      ;# GODAS will be done at these GFS cycles
    fi
   fi
  fi
 else
   export MANAL${cyc}GDAS=0
   export MLANL${cyc}GDAS=0
   export MOANL${cyc}GDAS=0
# CDFNL=''
  CDFNL=${CDFNL:-gfs}
# CDFNL=gfs
  VHR=00
  CYINC=${CYINC:-24}
  if [ $gdas_fh -ne 999 ] ; then CYINC=$gdas_fh ; fi
 fi
done

tasks_pe=${tasks_pe:-32}
tasks=${tasks:-$((((JCAP/pefac+tasks_pe/2)/tasks_pe)*tasks_pe))}
#
nth_f1=${nth_f1:-2}    # Defaults to     2 threads
nth_f2=${nth_f2:-1}    # Defaults to     1 thread
nth_f3=${nth_f3:-1}    # Defaults to     1 thread
#
idvc_f=${idvc_f:-2}    # Defaults to hybrid model forecast (GFS sela)
idvc_a=${idvc_a:-2}    # Defaulds to hybrid model forecast (GDAS sela)
#
#Apercent=${Apercent:-050} #   % theta in Generalized coord (henry)(idvc=3)
#
cha=${cha:-00} ; cma=${cma:-45} ; csa=${csa:-00}
ch1=${ch1:-03} ; cm1=${cm1:-00} ; cs1=${cs1:-00}
ch2=${ch2:-01} ; cm2=${cm2:-30} ; cs2=${cs2:-00}
#
q2run_1=${q2run_1:-$CUE2RUN}
q2run_2=${q2run_2:-$q2run_1}

#    Different resolutions

if [ $JCAP -eq 2046 ]    ; then  # Assumes semi-Lagrangian
  JCAP2=${JCAP2:-1148}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-1760}     ; LATA=${LATA:-880}
  LONB=${LONB:-4096}     ; LATB=${LATB:-2048}  ; DELTIM=${DELTIM:-450}
  DTPHYS=${DTPHYS:-$((DELTIM/2))}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-2304}  ; LATB2=${LATB2:-1152}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-600}
   DTPHYS2=${DTPHYS2:-$DELTIM2}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}
  if [ $DELTIM2 -lt $DELTIM ] ; then DELTIM2=$DELTIM ; fi
elif [ $JCAP -eq 1500 -o $JCAP -eq 1534 ]    ; then  # Assumes semi-Lagrangian
  JCAP2=${JCAP2:-878}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-1760}     ; LATA=${LATA:-880}
  LONB=${LONB:-3072}     ; LATB=${LATB:-1536}  ; DELTIM=${DELTIM:-450}
  DTPHYS=${DTPHYS:-$((DELTIM/2))}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-1760}  ; LATB2=${LATB2:-880}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-600}
   DTPHYS2=${DTPHYS2:-$DELTIM2}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}
  if [ $DELTIM2 -lt $DELTIM ] ; then DELTIM2=$DELTIM ; fi

elif [ $JCAP -eq 1148 ]    ; then  # Assumes semi-Lagrangian
  JCAP2=${JCAP2:-574}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
# LONA=${LONA:-1760}     ; LATA=${LATA:-880}
  LONA=${LONA:-1152}     ; LATA=${LATA:-576}
  LONB=${LONB:-2304}     ; LATB=${LATB:-1152}  ; DELTIM=${DELTIM:-600}
  DTPHYS=${DTPHYS:-$DELTIM}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-1152}  ; LATB2=${LATB2:-576}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-600}
   DTPHYS2=${DTPHYS2:-$DELTIM2}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}
  if [ $DELTIM2 -lt $DELTIM ] ; then DELTIM2=$DELTIM ; fi

elif [ $JCAP -eq 878 ]   ; then
  if [ $semilag = .true. ] ; then  # Semi-Lagrangian - linear grid
    JCAP2=${JCAP2:-574}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
#   LONA=${LONA:-1760}     ; LATA=${LATA:-880}
    LONA=${LONA:-1152}     ; LATA=${LATA:-576}
    LONB=${LONB:-1760}     ; LATB=${LATB:-880}  ; DELTIM=${DELTIM:-600}
    DTPHYS=${DTPHYS:-$DELTIM}
    if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
     JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
     LONB2=$LONB           ; LATB2=$LATB
     LEVS2=$LEVS           ; DELTIM2=$DELTIM
    else
     LONB2=${LONB2:-1152}  ; LATB2=${LATB2:-576}
     LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-600}
     DTPHYS2=${DTPHYS2:-$DELTIM2}
    fi
    NUMPROCANAL=${NUMPROCANAL:-128}
    if [ $DELTIM2 -lt $DELTIM ] ; then DELTIM2=$DELTIM ; fi
  else                           # Eulerian - quadreatic grid
   JCAP2=${JCAP2:-574}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
    LONA=${LONA:-1760}     ; LATA=${LATA:-880}
    LONB=${LONB:-2640}     ; LATB=${LATB:-1320}  ; DELTIM=${DELTIM:-100}
    if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
     JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
     LONB2=$LONB           ; LATB2=$LATB
     LEVS2=$LEVS           ; DELTIM2=$DELTIM
    else
     LONB2=${LONB2:-1760}  ; LATB2=${LATB2:-880}
     LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-180}
    fi
    NUMPROCANAL=${NUMPROCANAL:-128}
    if [ $DELTIM2 -lt $DELTIM ] ; then DELTIM2=$DELTIM ; fi
  fi

elif [ $JCAP -eq 670 ]     ; then
  JCAP2=${JCAP2:-382}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-1344}     ; LATA=${LATA:-672}
  LONB=${LONB:-1760}     ; LATB=${LATB:-880}  ; DELTIM=${DELTIM:-120}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-1152}  ; LATB2=${LATB2:-576}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-180}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}
elif [ $JCAP -eq 574 ]     ; then
  JCAP2=${JCAP2:-382}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-1152}     ; LATA=${LATA:-576}
  LONB=${LONB:-1760}     ; LATB=${LATB:-880}  ; DELTIM=${DELTIM:-120}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-1152}  ; LATB2=${LATB2:-576}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-180}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}

elif [ $JCAP -eq 510 ]     ; then
  JCAP2=${JCAP2:-382}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-768}      ; LATA=${LATA:-384}
  LONB=${LONB:-1536}     ; LATB=${LATB:-766}  ; DELTIM=${DELTIM:-120}  
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-768}   ; LATB2=${LATB2:-384}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-300}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}

elif [ $JCAP -eq 382 ]   ; then
  JCAP2=${JCAP2:-190}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-768}      ; LATA=${LATA:-384}
  LONB=${LONB:-1152}     ; LATB=${LATB:-576}  ; DELTIM=${DELTIM:-180}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-576}   ; LATB2=${LATB2:-288}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-360}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}

elif [ $JCAP -eq 254 ]   ; then
  JCAP2=${JCAP2:-170}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-768}      ; LATA=${LATA:-384}
  LONB=${LONB:-768}      ; LATB=${LATB:-384}  ; DELTIM=${DELTIM:-300}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-512}   ; LATB2=${LATB2:-256}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-450}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}

elif [ $JCAP -eq 190 ]   ; then
  JCAP2=${JCAP2:-126}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-576}      ; LATA=${LATA:-288}
  LONB=${LONB:-576}      ; LATB=${LATB:-288}  ; DELTIM=${DELTIM:-360}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-384}   ; LATB2=${LATB2:-190}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-600}
  fi
  NUMPROCANAL=${NUMPROCANAL:-128}

elif [ $JCAP -eq 170 ]   ; then
  JCAP2=${JCAP2:-126}    ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-512}      ; LATA=${LATA:-256}
  LONB=${LONB:-512}      ; LATB=${LATB:-256}  ; DELTIM=${DELTIM:-450}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-384}   ; LATB2=${LATB2:-190}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-600}
  fi
  NUMPROCANAL=${NUMPROCANAL:-64}

elif [ $JCAP -eq 126 ]   ; then
  JCAP2=${JCAP2:-62}     ; tasks2=${tasks2:-$((JCAP2/pefac))}
  LONA=${LONA:-384}      ; LATA=${LATA:-190}
  LONB=${LONB:-384}      ; LATB=${LATB:-190}
  LONB=${LONB:-384}      ; LATB=${LATB:-190}  ; DELTIM=${DELTIM:-600}
  if [ $gdas_fh -lt 999 -a $fseg_orig -lt 2 ] ; then
   JCAP2=$JCAP           ; tasks2=${tasks2:-${tasks:-$((JCAP/pefac))}}
   LONB2=$LONB           ; LATB2=$LATB
   LEVS2=$LEVS           ; DELTIM2=$DELTIM
  else
   LONB2=${LONB2:-192}   ; LATB2=${LATB2:-94}
   LEVS2=${LEVS2:-$LEVS} ; DELTIM2=${DELTIM2:-900}
  fi
  NUMPROCANAL=${NUMPROCANAL:-64}

elif [ $JCAP -eq 62 ]    ; then
  JCAP2=${JCAP2:-62}     ; tasks2=${tasks2:-$tasks}
  LONA=${LONA:-192}      ; LATA=${LATA:-94}
  LONB=${LONB:-192}      ; LATB=${LATB:-94}
  LONB2=${LONB2:-192}    ; LATB2=${LATB:-94}    ; LEVS2=${LEVS2:-$LEVS}
  DELTIM=${DELTIM:-900}  ; DELTIM2=${DELTIM2:-900}
  NUMPROCANAL=${NUMPROCANAL:-32}
fi
NUMPROCANALGDAS=$NUMPROCANAL       # number of tasks for GDAS anal (448)
NUMPROCANALGFS=$NUMPROCANAL        # number of tasks for GFS anal

DTPHYS=${DTPHYS:-$DELTIM}
DTPHYS2=${DTPHYS2:-$DELTIM}

#  Define new variables for analysis and prep step

JCAP_A=${JCAP_A:-$JCAP}
NLON_A=${NLON_A:-$LONA} ; NLAT_A=${NLAT_A:-$((LATA+2))}

export pgbf_gfs=${pgbf_gfs:-3}     #resolution of gfs pgb files: 193-0.25x0.25, 3-1x1, 4-0.5x0.5
export pgbf_gdas=${pgbf_gdas:-4}   #resolution of gdas pgb files
export pgbf_grid=$(eval echo \$pgbf_$CDUMP)
if [ $pgbf_grid -eq 193 ] ; then
 export flag_pgb=q
elif [ $pgbf_grid -eq 4 ] ; then
 export flag_pgb=h
elif [ $pgbf_grid -eq 3 ] ; then
 export flag_pgb=f
elif [ $pgbf_grid -eq 2 ] ; then
 export flag_pgb=l
fi
export flag_pgb=${flag_pgb:-q}
pgb_typ4prep=${pgb_typ4prep:-$flag_pgb}
if [ $pgb_typ4prep = m ] ; then
 LONA_P=${LONA_P:-$LONB} ; LATA_P=${LATA_P:-$LATB}
else
 LONA_P=${LONA_P:-$io_1} ; LATA_P=${LATA_P:-$jo_1}
fi

if [ $JCAP -ne $JCAP2 ] ; then
 if [ $fbak2 -eq 0 ] ; then fini2=0 ; fi
fi
#
n=-1
while [ $((n+=1)) -lt $gdas_cyc ] ; do
 cyc=${all_gdas[n]}
 ngfs=$((n*gfs_cyc/gdas_cyc))
#
#    First define default GDAS Forecast parameters
#
 if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
   export TIMELIMFCST${cyc}GDAS=${cha}:${cma}
   export TIMELIMPOST${cyc}GDAS=${cha}:${cma}
 else
   export TIMELIMFCST${cyc}GDAS=${cha}:${cma}:${csa}
   export TIMELIMPOST${cyc}GDAS=${cha}:${cma}:${csa}
 fi
 export NUMPROCFCST${cyc}GDAS=$tasks
 if [ $COUP_FCST = YES ] ; then
   export OMRESFCST${cyc}GDAS=$omres_1
   export NUMPROCOFCST${cyc}GDAS=$nproco_1
   export NUMPROCOCNP${cyc}GDAS=$tasksop_1
 fi
 export NUMPROCPOST${cyc}GDAS=$tasksp_1
 export FHDFIFCST${cyc}GDAS=$fdfi_a
 export FHOUTFCST${cyc}GDAS=$fout_a
 export IDVCFCST${cyc}GDAS=$idvc_a
 export NTHRFCST${cyc}GDAS=$nth_f1
 export LD3DFCST${cyc}GDAS=$ld3d_1
 export LG3DFCST${cyc}GDAS=$lg3d_1
 export GRID11FCST${cyc}GDAS=$grid11_1
 export GRID25FCST${cyc}GDAS=$grid25_1
 export GRID62FCST${cyc}GDAS=$grid62_1
 export LONBFCST${cyc}GDAS=$LONB
 export LATBFCST${cyc}GDAS=$LATB
 export LICEFCST${cyc}GDAS=$landice
 export IOPOST${cyc}GDAS=$io_a
 export JOPOST${cyc}GDAS=$jo_a
 export KOPOST${cyc}GDAS=$ko_a
 export KTOPOST${cyc}GDAS=$kto_1
 export ARCA${cyc}GDAS=$(eval echo \${ARCA${cyc}GDAS:-null})
#
#    Now define Forecast parameters for either GFS or GDAS long forecasts
#
# MFCST${cyc}GFS=0
 FDMP_GFS='NONE'
 if [ $gfs_cyc -gt $ngfs ] ; then
  echo ${all_gfs[ngfs]}
  if [ $cyc -eq ${all_gfs[ngfs]} ] ; then
    export MFCST${cyc}GFS=$fseg
   FDMP_GFS=GFS
  fi
 fi
 FDMP_GDAS='NONE'
 if [ $cyc -eq $fcst_cyc ] ; then
    export MFCST${cyc}GDAS=$fseg
   FDMP_GDAS=GDAS
 fi
 if [ $FDMP_GFS = GFS -a $FDMP_GDAS = GDAS ] ; then
  FDMP_ALL='GFS GDAS'
 elif [ $FDMP_GFS = GFS ] ; then
  FDMP_ALL=GFS
 elif [ $FDMP_GDAS = GDAS ] ; then
  FDMP_ALL=GDAS
 else
  FDMP_ALL=NONE
 fi
#                       For no forecast ; Analysis only
 NOFCST=${NOFCST:-NO}
 if [[ $NOFCST = YES ]] ; then
  FDMP_ALL='NONE'
  if [ $gfs_cyc -gt $ngfs ] ; then
    echo ${all_gfs[ngfs]}
    if [ $cyc -eq ${all_gfs[ngfs]} ] ; then
      export MFCST${cyc}GFS=0
    fi
  fi
 fi
 for FDMP in $FDMP_ALL ; do
  if [ $FDMP != NONE ] ; then
    export NUMPROCFCST${cyc}${FDMP}=$tasks,$tasks2
    export NUMPROCPOST${cyc}${FDMP}=$tasksp_1,$tasksp_2
    export JCAPFCST${cyc}${FDMP}=$JCAP,$JCAP2
    export IDVCFCST${cyc}${FDMP}=$idvc_f,$idvc_f
    export INCHFCST${cyc}${FDMP}=$inch_1,$inch_2
   if [ $COUP_FCST = YES ] ; then
     export OMRESFCST${cyc}${FDMP}=$omres_1,$omres_2
     export NUMPROCOFCST${cyc}${FDMP}=$nproco_1,$nproco_2
     export NUMPROCOCNP${cyc}${FDMP}=$tasksop_1,$tasksop_2
   fi
   export NTHRFCST${cyc}${FDMP}=$nth_f1,$nth_f2
   export LD3DFCST${cyc}${FDMP}=$ld3d_1,$ld3d_2
   export LG3DFCST${cyc}${FDMP}=$lg3d_1,$lg3d_2
   export Q2RUNFCST${cyc}${FDMP}=$q2run_1,$q2run_2
   export GRID11FCST${cyc}${FDMP}=$grid11_1,$grid11_2
   export GRID25FCST${cyc}${FDMP}=$grid25_1,$grid25_2
   export GRID62FCST${cyc}${FDMP}=$grid62_1,$grid62_2
   export LEVSFCST${cyc}${FDMP}=$LEVS,$LEVS2
   export LONBFCST${cyc}${FDMP}=$LONB,$LONB2
   export LATBFCST${cyc}${FDMP}=$LATB,$LATB2
   export NTRANFCST${cyc}${FDMP}=$ntrn_1,$ntrn_2
   export LICEFCST${cyc}${FDMP}=$landice,$landice2
   export FHBAKFCST${cyc}${FDMP}=00,$fbak2
   export FHDFIFCST${cyc}${FDMP}=$fdfi_1,$fdfi_2
   export FHCYCFCST${cyc}${FDMP}=$fcyc,$fcyc
   export FHINIFCST${cyc}${FDMP}=00,$fini2
   export FHMAXFCST${cyc}${FDMP}=$fmax1,$fmax2
   export FHOUTFCST${cyc}${FDMP}=$fout1,$fout2
   export FHZERFCST${cyc}${FDMP}=$fzer1,$fzer2
   export FHRESFCST${cyc}${FDMP}=$fres1,$fres2
   export DELTIMFCST${cyc}${FDMP}=$DELTIM,$DELTIM2
   export DTPHYSFCST${cyc}${FDMP}=$DTPHYS,$DTPHYS2
   export IOPOST${cyc}${FDMP}=$io_1,$io_2
   export JOPOST${cyc}${FDMP}=$jo_1,$jo_2
   export KOPOST${cyc}${FDMP}=$ko_1,$ko_2
   export KTOPOST${cyc}${FDMP}=$kto_1,$kto_2
#
   if [ $machine = WCOSS -o $machine = WCOSS_C ] ; then
     export TIMELIMFCST${cyc}${FDMP}=$ch1:$cm1,$ch2:$cm2
     export TIMELIMPOST${cyc}${FDMP}=$ch1:$cm1,$ch2:$cm2
   else
     export TIMELIMFCST${cyc}${FDMP}=$ch1:$cm1:$cs1,$ch2:$cm2:$cs2
     export TIMELIMPOST${cyc}${FDMP}=$ch1:$cm1:$cs1,$ch2:$cm2:$cs2
   fi
   export ARCA${cyc}GFS=$(eval echo \${ARCA${cyc}GFS:-null})
  fi
 done
done
#

CDFNL=${CDFNL:-gdas}
EDUMP=${EDUMP:-gdas}
CDUMPPREP=${CDUMPPREP:-gdas}
ESTEP=${ESTEP:-prep}
#
TIMELIMANAL=${TIMELIMANAL:-2:00}
#
DATATMP=${DATATMP:-'$PTMP/$LOGNAME/$PSLOT$CDATE$CDUMP$CSTEP'}
#
#COMDMP=${COMDMP:-'$DMPDIR/shared/dump/$CDATE/${CDUMP}x,$DMPDIR/shared/dump/$CDATE/$CDUMP'}
#COMCOP=${COMCOP:-'$DMPDIR/shared/dump/$CDATE/$CDUMP'}
#
COMROT=${COMROT:-$ROTDIR/$scrubtyp/$LOGNAME/pr$PSLOT}
COMDAY=${COMDAY:-$ROTDIR/$scrubtyp/$LOGNAME/pr$PSLOT}
ARCDIR=${ARCDIR:-$TOPDIR/$scrubtyp/$LOGNAME/archive/pr$PSLOT}
#
for cdm in $(eval echo $COMDMP|tr , ' ') ; do
echo ' cdm=',$cdm
 if [[ ! -s $cdm/sstgrb.$CDUMP.$CDATE ]] ; then
   sst_exists=${sst_exists:-NO}
 else
   sst_exists=YES
 fi
done

ARCHIVE=${ARCHIVE:-YES}
ARCH_TO_HPSS=${ARCH_TO_HPSS:-YES}
ARCH_TO_DISK=${ARCH_TO_DISK:-NO}
if [ $ARCH_TO_HPSS = YES -a $ARCH_TO_DISK = YES ]; then
 echo "Both ARCH_TO_HPSS and ARCH_TO_DISK are both set to YES. Only one can be turned on. Please fix."
 $PERR
 exit 1
fi
ATARDIR=${ATARDIR:-null}
ATARFILE=${ATARFILE:-$ADAY.tar}
if [[ $ARCH_TO_HPSS = YES && $USE_HPSS = YES ]] ; then
# HPSSTAR=${HPSSTAR:-$BASEDIR/ush/hpsstar}
  HTAR=${HTAR:-/apps/hpss/htar}
  HSI=${HSI:-/apps/hpss/hsi}
  echo 'sst_exists=' $sst_exists
  GET_DUMP=${GET_DUMP:-NO}
  if [[ $sst_exists = NO && $GET_DUMP = YES ]] ; then
    COMDMP=$PTMP/$usrdir/dump
    COMCOP=$PTMP/$usrdir/dump
    mkdir -p $COMDMP
    cd $COMDMP
    if [[ ! -s $COMDMP/sstgrb.$CDUMP.$CDATE ]] ; then
      /bin/rm $PTMP/$usrdir/dump/*
      YMD=$(echo $CDATE | cut -c1-8)
#     $HPSSTAR get /hpsspara/runhistory/glopara/dump/a${CDATE}$CDUMP.tar
      $HTAR get /hpsspara/runhistory/glopara/dump/a${CDATE}$CDUMP.tar
    fi
  fi
fi
#
RLIST=${RLIST:-$EXPDIR/pr${PSLOT}${MFCST00GFS}.rlist}
ALIST=${ALIST:-$EXPDIR/pr${PSLOT}.t${JCAP}.alist}
#
DOENKF=${DOENKF:-NO}
if [[ ! -s $RLIST ]] ; then
 echo '# Rlist is created in reconcile' > $RLIST
 PMKR=${PMKR:-$SHDIR/pmkr}
 $PMKR >>$RLIST
 append_rlist=${append_rlist:-$EXPDIR/append.rlist}
 if [ $DOENKF = YES ] ; then
  cat ${append_enkf_rlist:-$EXPDIR/append_enkf.rlist} >> $RLIST
 fi
 if [[ $ARCHIVE = YES ]] ; then cat $append_rlist >> $RLIST ; fi
 if [[ -s $ALIST ]] ; then cat $ALIST >> $RLIST ; fi
fi
enkf_ver=${enkf_ver:-v2.0.1}
#
#ATARDIR=${ATARDIR:-null}
#if [ $ATARDIR != null ] ; then
# if [[ $ARCH_TO_HPSS = YES && $USE_HPSS = YES ]] ; then
# $HPSSTAR mkd $ATARDIR
#  $HSI mkdir -p $ATARDIR
# fi
#fi
#ATARFILE=${ATARFILE:-/hpssuser/g01/$usrdir/pr${PSLOT}/\$ADAY.tar}
#
RUNLOG=${RUNLOG:-$EXPDIR/pr${PSLOT}.runlog}
NCP=${NCP:-$SHDIR/ncpx}
TRACKERSH=${TRACKERSH:-$USHDIR/global_tracker.sh}
#
#
# Verification switches. NOTE:  All verification is OFF by default
#                                                      ---
VRFYSCOR=${VRFYSCOR:-NO}
VRFYPRCP=${VRFYPRCP:-NO}
SAVEFITS=${SAVEFITS:-NO}
VRFYTRAK=${VRFYTRAK:-NO}
PLTSTORMS=${PLTSTORMS:-NO}
SEND2WEB=${SEND2WEB:-NO}
PRCP_PREFIX=${PRCP_PREFIX:-flx}

#
#                      SYSTEM CHANGES BELOW
#                      _____________________
#

#
#                      GENERAL (USER) CHANGES BELOW
#                      _____________________
#
DO2ANL=${DO2ANL:-NO}
PMKR=${PMKR:-$SHDIR/pmkr}

                                   # Set path to gsi directory

GSIFIXDIR=${GSIFIXDIR:-$BASE_GSI/fix}
gsi_ver=${gsi_ver:-v5.0.0}
PCPINFO=${PCPINFO:-$GSIFIXDIR/global_pcpinfo.txt}
PRVT=${PRVT:-$BASE_PREP/fix/prepobs_errtable.global}


# AM Fix files

FIXgsm=${FIXgsm:-$BASEDIR/fix/fix_am}
FIX_RAD=${FIX_RAD:-$FIXgsm}

SIGLEVEL1=${SIGLEVEL1:-$FIXgsm/global_siglevel.l${LEVS}.txt}
SIGLEVEL2=${SIGLEVEL2:-$FIXgsm/global_hyblev.l${LEVS}.txt}
if [ $Apercent -lt 100 ] ; then
  SIGLEVEL3=${SIGLEVEL3:-$FIXgsm/global_hyblev3.ipa$Apercent.txt}
else
  SIGLEVEL3=${SIGLEVEL3:-$FIXgsm/global_hyblev3.l${LEVS}.txt}
fi

#
#                     PREP STEP CHANGES BELOW
#                     _______________________
#
prep_global_ver=${prep_global_ver:-v2.1.0}
# Scripts and variables

##DUMPROCESSSH=${DUMPROCESSSH:-$BASEDIR/ush/get_opr_satwnd.sh}

PROCESS_TROPCY=${PROCESS_TROPCY:-NO}
DO_RELOCATE=${DO_RELOCATE:-YES}
RELOCATESH=${RELOCATESH:-$TROPCYDIR/ush/tropcy_relocate.sh}
DO_MAKEPREPBUFR=${DO_MAKEPREPBUFR:-YES}
MAKEPREPBUFRSH=${MAKEPREPBUFRSH:-$BASE_PREP/ush/prepobs_makeprepbufr.sh}
USHSYND=${USHSYND:-$NWPROD/ush}
BUFRLIST=${BUFRLIST:-"adpupa proflr aircar aircft satwnd adpsfc sfcshp vadwnd spssmi qkswnd wdsatr rassda gpsipw"}

# Fix files

PRPT=${PRPT:-$BASE_PREP/fix/prepobs_prep.bufrtable}
PRPC=${PRPC:-$BASE_PREP_GLOBAL/parm/prepobs_prepdata.$CDUMP.parm}

# Executables
RELOX=${RELOX:-$TROPCYDIR/sorc/relocate_mv_nvortex.fd/relocate_mv_nvortex}
PRPX=${PRPX:-$BASE_PREP/sorc/prepobs_prepdata.fd/prepobs_prepdata}
SYNDX=${SYNDX:-$BASE_PREP/sorc/syndat_syndata.fd/syndat_syndata}
PSTX=${PSTX:-$BASE_PREP_POST/sorc/global_postevents.fd/global_postevents}
AQCX=${AQCX:-$BASE_PREP/sorc/prepobs_prepacqc.fd/prepobs_prepacqc}
CQCX=${CQCX:-$BASE_PREP/sorc/prepobs_cqcbufr.fd/prepobs_cqcbufr}

#
#                    ANALYSIS STEP CHANGES BELOW
#                    ___________________________
#
# Scripts and variables
ANALYSISSH=${ANALYSISSH:-$BASE_GSI/scripts/exglobal_analysis.sh.ecf}
NLAT=${NLAT:-$((${LATA}+2))}

# Do not produce RAD, PCP, OZN, CNV diagnostic (STAT) files
# for GFS early analysis
SETUP=${SETUP:-""}
if [[ "${CDUMP}" = 'gfs' ]]; then
  SETUP="diag_rad=.false.,diag_pcp=.false.,diag_conv=.false.,diag_ozone=.false.,write_diag(3)=.false.,$SETUP"
fi

# GSI Fix files

BERROR=${BERROR:-$GSIFIXDIR/global_berror.l${LEVS}y${NLAT}.f77}
CONVINFO=${CONVINFO:-$GSIFIXDIR/global_convinfo.txt}
OZINFO=${OZINFO:-$GSIFIXDIR/global_ozinfo.txt}
PCPINFO=${PCPINFO:-$GSIFIXDIR/global_pcpinfo.txt}
SATINFO=${SATINFO:-$GSIFIXDIR/global_satinfo.txt}
SATANGL=${SATANGL:-$GSIFIXDIR/global_satangbias.txt}
RTMFIX=${RTMFIX:-$GSIFIXDIR/crtm_gfsgsi}
SET_BIASCR=${SET_BIASCR:-NO}
SET_BIASAT=${SET_BIASAT:-NO}

###########################################################################
# NOTE:  The logic below is designed to turn off the assimilation
#        of GOES 1x1 radiance data for the indicated period.
#        There were problems with GOES 1x1 data during this
#        period.
#
#  ***WARNING***
#  IT IS PREFERABLE TO REMOVE THE LOGIC BELOW ONCE THE PRS41H PASSES
#  THROUGH THE PERIOD INDICATED BELOW
#  ***WARNING***
#
# The prs41h is now (Sat Dec 29 08:40:19 EST 2007) working on the 2007102006
# gdas prep.  Therefore, the logic below is commented out.

##if [ $CDATE -gt 2007120512 -a $CDATE -lt 2007121818 ] ; then
##   SATINFO=$GSIJIF/fix/global_satinfo.txt.no_goes
##fi
###########################################################################

# Executables

GSIEXEC=${GSIEXEC:-$BASEDIR/exec/global_gsi}
       
IGEN=${IGEN:-82}
ANALSH=${ANALSH:-$JOBSDIR/anal.sh}
if [ $JCAP -ne 62 -a $JCAP -ne 126 -a $JCAP -ne 170 -a $JCAP -ne 254 -a $JCAP -ne 382  -a $JCAP -ne 574  -a $JCAP -ne 670 -a $JCAP -ne 878  -a $JCAP -ne 1148 -a $JCAP -ne 1534 -a $JCAP -ne 1500 -a $JCAP -ne 2046 ] ; then
 echo ' JCAP='$JCAP ' OPTION NOT AVAILABLE  WITH GSI AT THIS TIME!'
 exit
fi
#if [ $LEVS -ne 64 -a $LEVS -ne 91 ] ; then
# echo ' LEVS='$LEVS ' OPTION NOT AVAILABLE  WITH GSI AT THIS TIME!'
# exit
#fi
 
#
# FORECAST STEP CHANGES BELOW
#
#COPYCH=NO
AERODIR=${AERODIR:-$FIX_RAD}
RESDIR=${RESDIR:-$TOPDIR/$scrubtyp/$LOGNAME/pr$PSLOT/RESTART}
mkdir -p $RESDIR

if [ $COUP_FCST = YES ] ; then
 FORECASTSH=${FORECASTSH:-$SCRDIR/excfs_fcst.sh.sms}
else
 FORECASTSH=${FORECASTSH:-$SCRDIR/exglobal_fcst.sh.sms}
fi
FCSTEXECTMP=${FCSTEXECTMP:-$BASEDIR/exec/global_fcst}
EXEC_AMD=${EXEC_AMD:-$BASEDIR/exec}
AM_EXEC=${AM_EXEC:-$FCSTEXECTMP}

# POST STEP CHANGES
# Scripts and variables

PARMPOST=${PARMPOST:-$BASE_POST/parm}
post_ver=${post_ver:-v5.0.0}

# Check GDAS_GP flag to see if need to turn off NCEPPOST
# in GDAS post job
GDAS_GP=${GDAS_GP:-NO}
if [ $CDUMP = gdas  -a $GDAS_GP = YES ] ; then
  NCEPPOST=NO
fi

if [ $NCEPPOST = YES ] ; then
 OUTTYP=${OUTTYP:-1}
#POSTGPSH=${POSTGPSH_NP:-${POSTGPSH:-$BASE_POST/ush/global_nceppost.sh}}
 POSTGPEXEC=${POSTGPEXEC_NP:-${POSTGPEXEC:-$BASE_POST/src/ncep_post}}
else
 POSTGPSH=${POSTGPSH_GP:-${POSTGPSH:-/nwprod/ush/global_postgp.sh}}
 POSTGPEXEC=${POSTGPEXEC_GP:-${POSTGPEXEC:-$/nwprod/exec/global_postgp}}
fi

#  To generate PSI and CHI during post

GENPSICHI=${GENPSICHI:-NO}
GENPSICHIEXE=${GENPSICHIEXE:-$BASEDIR/exec/genpsiandchi}
OVERPARMEXEC=${OVERPARMEXEC:-$BASEDIR/exec/overparm_grib}


# VRFY STEP CHANGES
# Scripts and variables

POSTEVENTSSH=${POSTEVENTSSH:-$BASE_PREP_POST/ush/global_postevents.sh}
USHPREV=${USHPREV:-$PREPDIR/ush}

# Executables
PREX=${PREX:-$BASE_PREP/sorc/prepobs_prevents.fd/prepobs_prevents}


# ARCH STEP CHANGES
# Scripts and variables


#
#LDIAG3D=${LDIAG3D:-.false.}
ras=${ras:-.false.}
nsout=${nsout:-0}
tfiltc=${tfiltc:-0.85}
zhao_mic=${zhao_mic:-.true.}
liope=${liope:-.true.}
#
IEMS=${IEMS:-${iems:-0}}
ISOL=${ISOL:-${isol:-0}}
IAER=${IAER:-${iaer:-0}}
ICO2=${ICO2:-${ico2:-0}}
#
old_monin=${old_monin:-.true.}
ncw=${ncw:-'10,100'}
crtrh=${crtrh:-'0.85,0.85,0.85'}
# crtrh=${crtrh:-'0.98,0.98,0.98'}
flgmin=${flgmin:-0.220}
if [ $IEMS = 1 ] ; then EMISDIR=${EMISDIR:-$FIX_RAD} ; fi
 lsm=${lsm:-1}      ;# lsm=1 is for NOAH land model (=0 for OSU model)

 FCSTVARS=${FCSTVARS:-""}
# FCSTVARS="LDIAG3D=$LDIAG3D, ras=$ras, nsout=$nsout, lsm=$lsm,\
 FCSTVARS="ras=$ras, nsout=$nsout, lsm=$lsm,\
                 tfiltc=$tfiltc, liope=$liope, zhao_mic=$zhao_mic,  \
                 old_monin=$old_monin,\
                 ncw=$ncw,crtrh=$crtrh,flgmin=$flgmin,$FCSTVARS"
#                ncw=10,100,crtrh=0.98,0.98,0.98,flgmin=0.210,"
#             lsm=$lsm,gfsio_in=$gfsio_in,gfsio_out=$gfsio_out,adiab=.false.,\
#
# LANDICE_OPT=1          # Input no landice, output with landice
# LANDICE_OPT=2          # Both input and ouput with landice (default)
# LANDICE_OPT=3          # Both input and ouput without landice
# LANDICE_OPT=4          # Input file with landice & output file without landice
if [ $lsm -eq 0 ] ; then # all climo/static fields interpolated from input grid
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-1}
else                     # Interpolate veg type, soil type and slope type from input grid, all others from sfcsub.f
  CLIMO_FIELDS_OPT=${CLIMO_FIELDS_OPT:-2}
fi
#
#
#  UTILITY CHANGES (sighdr, chgres, sfchdr, etc)
#  Scripts and variables
#  global_sighdr and global_sfchdr changes
#
SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr}
SFCHDR=${SFCHDR:-$NWPROD/exec/global_sfchdr}
#
if [ $machine = THEIA ]; then
  export nemsioget=${nemsioget:-/home/glopara/bin/nemsio_util/nemsio_get}
elif [ $machine = WCOSS ]; then
  export nemsioget=${nemsioget:-/nwprod/ngac.v1.0.0/exec/nemsio_get}
elif [ $machine = WCOSS_C ]; then
  export nemsioget=${nemsioget:-$NWPROD/ngac.v1.0.0/exec/nemsio_get}
fi
#
#  global_cycle chgres changes
#
CHGRESEXEC=${CHGRESEXEC:-$BASEDIR/exec/global_chgres}
CYCLEXEC=${CYCLEXEC:-$BASEDIR/exec/global_cycle}
CHGRESSH=${CHGRESSH:-$USHDIR/global_chgres.sh}
npe_node_po=${npe_node_po:-12}
CHGRESTHREAD=${CHGRESTHREAD:-$npe_node_po}

#
FNTSFC=${FNTSFC:-$FIXgsm/cfs_oi2sst1x1monclim19822001.grb}
FNAISC=${FNAISC:-$FIXgsm/cfs_ice1x1monclim19822001.grb}
FNVEGC=${FNVEGC:-$FIXgsm/global_vegfrac.0.144.decpercent.grb}
FNVETC=${FNVETC:-$FIXgsm/global_vegtype.1x1.grb}
IALB=${IALB:-${ialb:-0}}
#
MTNDIR=${MTNDIR:-$FIXgsm}
#
#  Set snoid to "snod" if the new T382 snogrib file exists
#
#dumpx=$(eval echo $COMDMP|cut -f1 -d,)
#if [ -s $dumpx/snogrb_t382.$CDUMP.$CDATE ] ; then
#export FNSNOA=$dumpx/snogrb_t382.$CDUMP.$CDATE
# export snoid=snod
#fi
#dumpx=$(eval echo $COMDMP|cut -f2 -d,)
#if [ -s $dumpx/snogrb_t382.$CDUMP.$CDATE ] ; then
# export snoid=snod
#fi
#
#  For new Ozone Production/Destruction and Climatology
#
#if [ $newoz_gsfc = YES ] ; then
# O3FORC=${O3FORC:-$DISK_GLOB/wx23sm/O3P_L/ox_prdlos_head.ieee}
# O3CLIM=${O3CLIM:-$DISK_GLOB/wx23sm/O3P_L/ozone.clim}
#fi
if [ $newoz_nrl = YES ] ; then
 O3FORC=${O3FORC:-$FIXgsm/global_o3prdlos.f77}
 O3CLIM=${O3CLIM:-$FIXgsm/ozone.clim}
fi
#if [ $newozopr = YES ] ; then
# O3FORC=${O3FORC:-$DISK_GLOB/wx23sm/O3P_L/ox_prdlos_opr_head.ieee}
#fi
#
MODIS_ALB=${MODIS_ALB:-NO}
#     For Modis Albedo ##########################################
if [ $MODIS_ALB = YES ] ; then
  MODISDIR=${MODISDIR:-/global/save/wx20hw/data_modis}
  FNALBC=${FNALBC:-$MODISDIR/global_snowfree_albedo.t$JCAP.grb}
  FNABSC=${FNABSC:-$MODISDIR/global_mxsnoalb.t$JCAP.grb}
fi
#
#ARCA00GFS=${ARCA00GFS:-'/hpssuser/g01/$usrdir/pr${PSLOT}/$CDATE$CDUMP.tar'}
#ARCA00GDAS=${ARCA00GDAS:-'/hpssuser/g01/$usrdir/pr${PSLOT}/$CDATE$CDUMP.tar'}

FIT_DIR=${FIT_DIR:-$COMROT/fits}
HORZ_DIR=${HORZ_DIR:-$COMROT/horiz}
#
if [ $COUP_FCST = YES ] ; then
#
#                           For MOM4 OM and GODAS
#                           ---------------------
#
#     Default fix fields for OM
#
 FIX_OM=${FIX_OM:-$BASEDIR/fix/fix_om}
 FIX_OCN=${FIX_OCN:-$FIX_OM}
 PARM_OM=${PARM_OM:-$BASEDIR/parm/parm_om}
 SNOWNC=${SNOWNC:-$FIX_OCN/SNOW.nc}
 SSTICECLIM=${SSTICECLIM:-$FIX_OCN/sst_ice_clim.nc}
#
 if [[ $CKSH = oanl ]] ; then
  PARM_GODAS=${PARM_GODAS:-$BASEDIR/parm/parm_om/tbl_nml}
  diagtable=${diagtable:-$PARM_GODAS/diag_table}    # diagnositics table
  datatable=${datatable:-$PARM_GODAS/data_table}    # data override table
  fieldtable=${fieldtable:-$PARM_GODAS/field_table} # field table
# namelist=${namelist:-$PARM_GODAS/namelist_$omres} # namelist file
  Obs_dir=${Obs_dir:-$COMROT/GDS_Obs}
  SBC_dir=${SBC_dir:-$COMROT/GDS_SBC}
 else
  PARM_OM=${PARM_OM:-$BASEDIR/parm/parm_om}
  diagtable=${diagtable:-$PARM_OM/diag_table.hr}    # Hourly output
  diagtable_long=${diagtable_long:-$diagtable}      # Hourly output
  datatable=${datatable:-$PARM_OM/data_override}    # data override table
  fieldtable=${fieldtable:-$PARM_OM/field_table}    # field table
  namelist=${namelist:-$PARM_OM/namelist}           # namelist file
 fi
#
 oisst_clim=${oisst_clim:-$FIX_OM/oisst_clim.nc}
 r2ts_clim=${r2ts_clim:-FIX_OM/r2ts_clim.nc}
 sst_ice_clim=${sst_ice_clim:-$FIX_OM/sst_ice_clim.nc}

#                                     Ocean model directory and executable
#                                     ------------------------------------
 EXEC_OMD=${EXEC_OMD:-$BASEDIR/exec}
 OM_EXEC=${OM_EXEC:-EXEC_OMD/fms_mom4icie2.x}
#
#                                     Coupler directory and executable
#                                     --------------------------------
 EXEC_CD=${EXEC_CD:-$BASEDIR/exec}
 C_EXEC=${C_EXEC:-$EXEC_CD/mlc_cfs4_coupler}
#
#
#                                     GODAS and related executables
#                                     --------------------------------
 GODASEXEC=${GODASEXEC:-$BASEDIR/exec/fms_gdsSOLO.x}
 cmbDysPrf4=${cmbDysPrf4:-$BASEDIR/exec/cmbDysPrf4}
 cmbDysPrfs4=${cmbDysPrfs4:-$BASEDIR/exec/cmbDysPrfs4}
 mkEvNc4r=${mkEvNc4r:-$BASEDIR/exec/mkEvNc4r}
 GODASSH=${GODASSH:-$BASEDIR/ush/godasM4.sh}
#
 SNOWNC=$FIX_OCN/SNOW.nc
 SSTICECLIM=$sst_ice_clim
fi
#
# For GLDAS
if [[ $CKSH = lanl ]] ; then
 PARM_LM=${PARM_LM:-$BASEDIR/parm/parm_lm}
fi
#                                     --------------------------------
#                                     GLDAS (aka LIS) executable
#                                     --------------------------------
#LISSH=${LISSH:-$USHDIR/LIS.sh}
LISSH=${LISSH:-$SCRDIR/excfs_cdas_gldas.sh.sms}
LISEXEC=${LISEXEC:-$BASEDIR/exec/cfs_cdas_gldas_LIS}
#
#  Soil moisture nudging - defaults to no nudging with LDAS
if [ $ldas_cyc -gt 0 -o $CKSH = fcst ] ; then
 FSMCL2=${FSMCL2:-99999}
else
 FSMCL2=${FSMCL2:-60}
fi
#
mkdir -p $COMROT
mkdir -p $COMDAY
mkdir -p $ARCDIR
#
mkdir -p $FIT_DIR
mkdir -p $HORZ_DIR

group_name=${group_name:-rstprod}
permission=${permission:-755}
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
#$CHGRP_CMD $COMROT
#$CHGRP_CMD $COMDAY
$CHGRP_CMD $ARCDIR
$CHGRP_CMD $FIT_DIR
$CHGRP_CMD $HORZ_DIR

chmod $permission $COMROT
chmod $permission $COMDAY
chmod $permission $ARCDIR
chmod $permission $FIT_DIR
chmod $permission $HORZ_DIR
#
#  Do Task Geometry if necessary
#
if [ $machine = WCOSS ] ; then
 TASK_GEOMETRY=NONE
 if [ ${TASKGM:-NO} = YES ] ; then
  if [ $COUP_FCST = NO ] ; then
   #if [ $CKSH = fcst -a $liope = .true. -a $JCAP -ge 1500 ] ; then
   if [ $CKSH = fcst -a $JCAP -ge 1500 ] ; then
    cycle=$(echo $CDATE|cut -c9-10)
    cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
    CKND=$(echo $CSTEP|cut -c5-)
    nknd=${CKND:-1}
    tasks=$(eval echo \${NUMPROCFCST$cycle$cdump:-$tasks}|cut -f$nknd -d,)
    #JCAP=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd -d,)
    NUMTHRD=$(eval echo \${NTHRFCST$cycle$cdump:-$nth_f1}|cut -f$nknd -d,)
    npe_node=${npe_node_f:-24}
    tpn=$((npe_node/NUMTHRD))
    tpnm=4; if [ $tpnm -gt $tpn ]; then tpnm=$tpn ;fi
    tpnu=$((tpnm+tpnm+tpn+tpn))
    TASK_GEOMETRY=$(/usrx/local/bin/mktgs $tpnm/$tpnm $((tasks-tpnu))/$tpn $tpnm/$tpnm $tpn/$tpn $tpn/$tpn)
   fi
  else
#  TASK_GEOMETRY=$(/usrx/local/bin/mktgs $NPROCS_oc/$((npe_node/NUMTHRD)) $NPROCS_a/$((npe_node/NUMTHRD)))
   echo "Correct TASK_GEOMETRY for the coupled forecast needs to be defined"
  fi
 fi
fi

#!/bin/ksh
################################################################################
# This script runs the post processor.
# Usage: d3dp.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
#
export BASEDIR=${BASEDIR:-..}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export USHDIR=${USHDIR:-$BASEDIR/ush}
export EXECDIR=${EXECDIR:-$BASEDIR/exec}
export NWPROD=${NWPROD:-$BASEDIR}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NCP=${NCP:-cp}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export COPYGB=${COPYGB:-$NWPROD/util/exec/copygb}
export NCEPPOST=${NCEPPOST:-NO}
export in_o=${in_o:-0} # copygb interpolation option, defaults to 0 (bilinear)

if [ $NCEPPOST = YES ] ; then
 export D3DPOSTSH=${D3DPOSTSH:-$USHDIR/global_nceppost_d3d.sh}
 export POSTGPEXEC=${POSTGPEXEC:-$EXECDIR/ncep_post}
 export PARM_AM=${PARM_AM:-$BASEDIR/parm/parm_am}
 export OUTTYP=${OUTTYP:-1}
 export CHGRESEXEC=${CHGRESEXEC:-$EXECDIR/global_chgres}
 export SIGHDR=${SIGHDR:-$NWPROD/exec/global_sighdr}
 export USHGLOBAL=${USHGLOBAL:-$USHDIR}
else
 export D3DPOSTSH=${D3DPOSTSH:-$USHDIR/d3d_post.sh}
 export D3DPOSTEXEC=${D3DPOSTEXEC:-$EXECDIR/post_d3d}
fi

export COUP_FCST=${COUP_FCST:-YES}
export CCPOST=${CCPOSTD3D:-NO}
export FFPOST=${FFPOST:-YES}
export CDFNL=${CDFNL:-fnl}
export GDUMP=${GDUMP:-$CDFNL}
cycle=$(echo $CDATE|cut -c9-10)
cdump=$(echo $CDUMP|tr '[a-z]' '[A-Z]')
eval MANAL=\${MANAL$cycle$cdump:-1}
export MANAL
eval mfcst=\${MFCST$cycle$cdump:-1}
nknd=${CKND:-1}
export JCAP=$(eval echo \${JCAPFCST$cycle$cdump:-$JCAP}|cut -f$nknd -d,)
export FHINI=$(eval echo \${FHINIFCST$cycle$cdump:-0}|cut -f$nknd -d,)
export FHMAX=$(eval echo \${FHMAXFCST$cycle$cdump:-9}|cut -f$nknd -d,)
export FHOUT=$(eval echo \${FHOUTFCST$cycle$cdump:-3}|cut -f$nknd -d,)
export FHBAK=$(eval echo \${FHBAKFCST$cycle$cdump:-00}|cut -f$nknd -d,)
export GRID_ID25=$(eval echo \${GRID25FCST$cycle$cdump:-0}|cut -f$nknd -d,)
if [ $FHBAK -eq 0 ] ; then export FHINI=$FHBAK ; fi
export IO=$(eval echo \${IOPOST$cycle$cdump:-${IO:-360}}|cut -f$nknd -d,)
export JO=$(eval echo \${JOPOST$cycle$cdump:-${JO:-181}}|cut -f$nknd -d,)
export KO=$(eval echo \${KOPOST$cycle$cdump:-${KO:-26}}|cut -f$nknd -d,)
export FH_STRT_POST=${FH_STRT_POST:-0}
#
export IGEN_FCST=${IGEN_FCST:-96}
export VERBOSE=YES
export SIGIND=${SIGIND:-$COMROT}
export FLXIND=${FLXIND:-$COMROT}
export D3DIND=${D3DIND:-$COMROT}
export COMOUT=$COMROT
if [ $nknd -gt 1 -a $FHBAK -eq 0 ] ; then
 export SUFOUT=.${CDUMP}$nknd.$CDATE
else
 export SUFOUT=.${CDUMP}.$CDATE
fi
#                    To start post from the middle
if [ $FH_STRT_POST -gt 0 ] ; then
 FHINI=$FH_STRT_POST
elif [ -s $COMROT/FHREST.$CDUMP.$CDATE.$nknd ] ; then
 read FHINI < $COMROT/FHREST.$CDUMP.$CDATE.$nknd
fi
#
polist_47d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,725.,700.,675.,650.,625.,600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.,275.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_46d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,750.,700., 650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,40.,30.,20.,10.,7.,5.,4.,3.,2.,1.,0.7,0.5,0.4,0.3,0.2,0.1,0.07,0.05,0.04,0.03,0.02"
#
polist_31d="1000.,975.,950.,925.,900.,850.,800.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,200.,150.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_37d="1000.,975.,950.,925.,900.,875.,850.,825.,800.,775.,750.,700.,650.,600.,550.,500.,450.,400.,350.,300.,250.,225.,200.,175.,150.,125.,100.,70.,50.,30.,20.,10.,7.,5.,3.,2.,1."
#
polist_47=${polist_47:-$polist_47d}
polist_46=${polist_46:-$polist_46d}
polist_31=${polist_31:-$polist_31d}
polist_37=${polist_37:-$polist_37d}

#
if [[ $CCPOST = YES ]];then
  CSTEP0=$CSTEP
  eval CSTEP=fcst$CKND datafcst=$SIGIND
  CSTEP=$CSTEP0
  tsleep=$((JCAP/FHOUT/6*15+15))
  msleep=240
else
  datafcst=$SIGIND
fi
#
if [ $NCEPPOST = YES ] ; then
 export IDRT=${IDRT_NP:-0}         # defaults to lat/lon (for gfsio file)
 export LONB=${LONB_D3D:-360}      # defaults to 0.5 degree resolution
 export LATB=${LATB_D3D:-181}
 export GRID_IDD=${GRID_IDD:-3}    # defaults to lat/lon grid of 1.0 degree

 export  POSTGPVARS="KPO=$KO,PO=$(eval echo \${polist_$KO}),"
else
 LONB=$IO
 LATB=$JO
 export  D3DPOSTVARS="KPO=$KO,PO=$(eval echo \${polist_$KO}),"
fi
#
################################################################################
# Copy in restart and input files (disabled here)

#$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST

################################################################################
# Post forecast files

if [[ $nknd -le $mfcst ]];then
  if [[ 10#$FHINI -eq 0 ]] ; then
    FH=-$FHOUT
  else
    FH=$(((10#$FHINI/10#$FHOUT)*10#$FHOUT))
#   FH=$FHINI
  fi
  if [ $NCEPPOST = YES ] ; then
    export CTLFILE=${CTL_FCS_D3D:-$PARM_AM/am_cntrl.parm_d3d}
  fi
  until [[ $((FH=10#$FH+10#$FHOUT)) -gt $FHMAX ]];do [[ $FH -lt 10 ]]&&FH=0$FH
    export VDATE=$($NDATE $FH $CDATE)
    if [[ $CCPOST = YES ]];then
      export LOGINP=$datafcst/logf${FH}${SUFOUT}
      export SIGINP=$datafcst/${SIGOSUF}f${FH}${SUFOUT}
      export FLXINP=/dev/null
      if [ $NCEPPOST = YES ] ; then
        if [ $IDRT -eq 4 ] ; then
          export D3DINP=$datafcst/d3df${FH}${SUFOUT}
        elif [ $IDRT -eq 0 ] ; then
          rm d3dfile
          $COPYGB -g$GRID_IDD -i$in_o -x $datafcst/d3df${FH}${SUFOUT} d3dfile
          export D3DINP=d3dfile
        else
          echo 'INVALID IDRT has value  '$IDRT
          exit
        fi
      else
        export D3DINP=$datafcst/d3df${FH}${SUFOUT}
      fi
      
      nsleep=0
      until [[ -s $LOGINP || $((nsleep+=1)) -gt $msleep ]];do sleep $tsleep;done
      if [[ $nsleep -gt $msleep ]];then $PERR;exit 2;fi
    else
      export SIGINP=$SIGIND/${SIGOSUF}f${FH}${SUFOUT}
      export FLXINP=/dev/null
      if [ $NCEPPOST = YES ] ; then
        if [ $IDRT -eq 4 ] ; then
          export D3DINP=$D3DIND/d3df${FH}${SUFOUT}
        elif [ $IDRT -eq 0 ] ; then
          rm d3dfile
          $COPYGB -g$GRID_IDD -i$in_o -x $D3DIND/d3df${FH}${SUFOUT} d3dfile
          export D3DINP=d3dfile
        else
          echo 'INVALID IDRT has value  '$IDRT
          exit
        fi
      else
        export D3DINP=$D3DIND/d3df${FH}${SUFOUT}
      fi
    fi
    if [[ $FFPOST = YES || -s $SIGINP ]];then
      export IGEN=$IGEN_FCST
      export D3DOUT1=$COMOUT/diabf${FH}${SUFOUT}
      export D3DOUT=$COMOUT/diabh${FH}${SUFOUT}

      $D3DPOSTSH

      rc=$?
      if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

      if [ $NCEPPOST = YES ] ; then
        if [ $GRID_IDD -gt 3 ] ; then
          $COPYGB -g3 -i$in_o -x $D3DOUT $D3DOUT1
        else
          mv $D3DOUT $D3DOUT1
        fi
      elif [ $LONB -gt 360 ] ; then
        $COPYGB -g3 -i$in_o -x $D3DOUT $D3DOUT1
      else
        mv $D3DOUT $D3DOUT1
      fi
      if [ $GRID_ID25 -gt 0 ] ; then
        if [ $GRID_IDD -gt 3  -o $LONB -gt 360 ] ; then
          $COPYGB -g$GRID_ID25 -i$in_o -x $D3DOUT $COMOUT/diabl${FH}${SUFOUT}
        else
          $COPYGB -g$GRID_ID25 -i$in_o -x $D3DOUT1 $COMOUT/diabl${FH}${SUFOUT}
        fi
      fi
    fi
  done
fi
#
################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
$PEND

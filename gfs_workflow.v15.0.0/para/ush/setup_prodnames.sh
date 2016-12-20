#!/bin/ksh
# make links for global parallel files using prod names
# must run for each cycle... 
set -xa

if [ $# -lt 5 ]; then
  set +x
  echo " Usage:  $0 COMIN COMPROD CDATE CDUMP HRKCOM"
  echo " Eg:  /global/hires/glopara/pre13a /global/hires/glopara/prod 2008101700 gfs 120"
  echo " exiting"
  set -x
  exit 6
fi

DATA=${TMPDIR:-$STMP/$LOGNAME}/setprodnames
NWPROD=${NWPROD:-/nwprod}
[ -d $DATA ] || mkdir $DATA
cd $DATA


COMIN=${1:-$COMROT}
COMPROD=${2:-${COMPROD:-$COMIN/prod}}
CDATE=${3:-$CDATE}
CDUMP=${4:-gfs}
HRKCOM=${5:-${HRKCOM:-120}}    ;#clean files older than HRKCOM hours

if [ ! -d $COMIN ]; then
  set +x
  echo "ERROR:  $COMIN is not a directory.  Exiting..."
  set -x
  exit
fi


DDATE=`echo $CDATE | cut -c 1-8`
CYC=`echo $CDATE | cut -c 9-10`
export cycle=t${CYC}z
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
CDATEM=`$NDATE -$HRKCOM $CDATE`
DDATEM=`echo $CDATEM | cut -c 1-8`
cycm=`echo $CDATEM | cut -c9-10`

cd $COMIN

# Process GDAS and GFS
if [ $CDUMP = gdas -o $CDUMP = gfs ]; then
 case $CDUMP in
  gdas) NCO_DUMP=gdas;PRE=gdas;;
  gfs) NCO_DUMP=gfs;PRE=gfs;;
  *) echo UH-OH. $CDUMP;continue;;  # default
 esac

 COMOUT=$COMPROD/com2/gfs/para/$CDUMP.$DDATE
 [ -d $COMOUT ] || mkdir -p $COMOUT
 if [ $? -ne 0 ]; then
  set +x; echo error creating $COMOUT;set -x
  exit 7
 fi

 COMOUTM=$COMPROD/com2/gfs/para/$CDUMP.$DDATEM
 if [ -d $COMOUTM ]; then rm -r $COMOUTM/${PRE}.t${cycm}* ;fi         

 for file in *.$CDUMP.$CDATE; do

  echo $file

  base=${file%%.$CDUMP.$CDATE}
  echo $base

  [ -n "$FH" ] && unset FH
  [ -n "$NCO_BASE" ] && unset NCO_BASE

  FOUND=YES

  case $base in
   *.lr) continue;;
   splf*) continue;;
   tcinform_relocate)continue;;
   tcvitals_relocate)continue;;
   atcfunix) continue;;
   prepqc) NCO_BASE=prepbufr;;
   prepbufr.acft_profiles) NCO_BASE=prepbufr.acft_profiles;;
   nsstbufr) NCO_BASE=nsstbufr;;
   prepq*) continue;;
   cnvstat) NCO_BASE=cnvstat;;
   gsistat) NCO_BASE=gsistat;;
   oznstat) NCO_BASE=oznstat;;
   pcpstat) NCO_BASE=pcpstat;;
   radstat) NCO_BASE=radstat;;
   flxf*) FH=${base#flxf};NCO_BASE=sfluxgrbf$FH;;
   flnf*) FH=${base#flnf};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=flxf$FH.nemsio;;
   g3dc*) FH=${base#g3dc};NCO_BASE=gcgrbf$FH;;
   logf*) FH=${base#logf};NCO_BASE=logf$FH;;
   pgbqanl) NCO_BASE=pgrbqanl;;
   pgbhanl) NCO_BASE=pgrbhanl;;
   pgbanl) NCO_BASE=pgrbanl;;
   pgbqnl) NCO_BASE=pgrbqnl;;
   pgblanl) NCO_BASE=pgrblanl;;
   pgbq*) FH=${base#pgbq};NCO_BASE=pgrbq$FH;;
   pgbh*) FH=${base#pgbh};NCO_BASE=pgrbh$FH;;
   pgbf*) FH=${base#pgbf};NCO_BASE=pgrbf$FH;;
   pgbl*) FH=${base#pgbl};NCO_BASE=pgrbl$FH;;
   pgrbl*) FH=${base#pgrbl};NCO_BASE=pgrbf$FH.2p5deg;;
   sfcanl) NCO_BASE=sfcanl.nemsio;;
   sfnanl) NCO_BASE=sfcanl.nemsio;;
   nsnanl) NCO_BASE=nstanl.nemsio;;
   dtfanl) NCO_BASE=dtfanl.bin4;;
   sfcf*) FH=${base#sfcf};NCO_BASE=bf$FH;;
   sfnf*) FH=${base#sfnf};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=sfcf$FH.nemsio;;
   nsnf*) FH=${base#nsnf};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=nstf$FH.nemsio;;
   gfnanl) NCO_BASE=atmanl.nemsio;;
   sigf*) FH=${base#sigf};NCO_BASE=sf$FH;;
   gfnf*) FH=${base#gfnf};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf$FH.nemsio;;
   sigges) NCO_BASE=sgesprep;;
   siggm3) NCO_BASE=sgm3prep;;
   siggm2) NCO_BASE=sgm2prep;;
   siggm1) NCO_BASE=sgm1prep;;
   siggp1) NCO_BASE=sgp1prep;;
   siggp2) NCO_BASE=sgp2prep;;
   siggp3) NCO_BASE=sgp3prep;;
   gfnges) NCO_BASE=atmges.nemsio;;
   gfngm3) NCO_BASE=atmgm3.nemsio;;
   gfngm2) NCO_BASE=atmgm2.nemsio;;
   gfngm1) NCO_BASE=atmgm1.nemsio;;
   gfngp1) NCO_BASE=atmgp1.nemsio;;
   gfngp2) NCO_BASE=atmgp2.nemsio;;
   gfngp3) NCO_BASE=atmgp3.nemsio;;
   biascr) NCO_BASE=abias;;
   biascr_pc) NCO_BASE=abias_pc;;
   aircraft_t_bias) NCO_BASE=abias_air;;
   satang) NCO_BASE=satang;;
   *) FOUND=NO;;
  esac

  if [ $FOUND = YES ]; then
   if [ $CDUMP = gfs ]; then 
    if [ $base = pgrbqnl -o $base = pgrbq$FH ]; then
     if [ $FH = nl ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     elif [ $FH -le 240 ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     else
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     fi
    else
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
    fi
   fi
   if [ $CDUMP = gdas ]; then 
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
   fi

   if [ -s $COMIN/${file}.idx ] ; then
      ln -sf $COMIN/${file}.idx $COMOUT/$PRE.t${CYC}z.${NCO_BASE}.idx
   fi

  else
   echo "NOT FOUND:  $COMIN/$file"
  fi

 done # end for file

 for file in *.$CDUMP.$CDATE.grib2; do

  echo $file

  base=${file%%.$CDUMP.$CDATE.grib2}
  echo $base

  [ -n "$FH" ] && unset FH
  [ -n "$NCO_BASE" ] && unset NCO_BASE

  FOUND=YES

# Generate Gaussian grid master file

  case $base in
   pgrbqanl) NCO_BASE=pgrb2.0p25.anl;;
   pgrbhanl) NCO_BASE=pgrb2.0p50.anl;;
   pgrbanl) NCO_BASE=pgrb2.1p00.anl;;
   pgrblanl) NCO_BASE=pgrb2.2p50.anl;;
   pgbmnl) NCO_BASE=master.grb2anl;;
   pgrbbqanl) NCO_BASE=pgrb2b.0p25.anl;;
   pgrbbhanl) NCO_BASE=pgrb2b.0p50.anl;; 
   pgrbbfanl) NCO_BASE=pgrb2b.1p00.anl;;
   pgrbq*) FH=${base#pgrbq};NCO_BASE=pgrb2.0p25.f$FH;;
   pgrbh*) FH=${base#pgrbh};NCO_BASE=pgrb2.0p50.f$FH;;
   pgrbf*) FH=${base#pgrbf};NCO_BASE=pgrb2.1p00.f$FH;;
   pgrbl*) FH=${base#pgrbl};NCO_BASE=pgrb2.2p50.f$FH;;
   pgbm*) FH=${base#pgbm};NCO_BASE=master.grb2f$FH;;
   pgrbbq*) FH=${base#pgrbbq};NCO_BASE=pgrb2b.0p25.f$FH;;
   pgrbbh*) FH=${base#pgrbbh};NCO_BASE=pgrb2b.0p50.f$FH;;
   pgrbbf*) FH=${base#pgrbbf};NCO_BASE=pgrb2b.1p00.f$FH;;
   flxf*) FH=${base#flxf};NCO_BASE=sfluxgrbf$FH.grib2;;
   flnf*) FH=${base#flnf};NCO_BASE=sfluxgrbf$FH.grib2;;
   *) FOUND=NO;;
  esac

  if [ $FOUND = YES ]; then
   if [ $CDUMP = gfs ]; then 
    if [ $base = pgrbqnl -o $base = pgrbq$FH ]; then
     if [ $FH = nl ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     elif [ $FH -le 240 ]; then
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     else
       ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
     fi
    else
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
    fi
   fi
   if [ $CDUMP = gdas ]; then 
     ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
   fi

   if [ -s $COMIN/${file}.idx ] ; then
      ln -sf $COMIN/${file}.idx $COMOUT/$PRE.t${CYC}z.${NCO_BASE}.idx
   fi

  else
   echo "NOT FOUND:  $COMIN/$file"
  fi

 done # end for grib2 file

fi

# Create prod look-alike for select dump data
if [ $CDUMP = gdas -o $CDUMP = gfs ]; then
   COMDMP=$DMPDIR/$CDATE/$CDUMP
   COMDMPx=$DMPDIR/$CDATE/${CDUMP}x
   COMDMPy=$DMPDIR/$CDATE/${CDUMP}y
   COMDMPv=$DMPDIR/$CDATE/${CDUMP}v

fi



# Create prod look-alike for enkf
if [ $CDUMP = enkf ]; then

 PRE=gdas

 COMOUT=$COMPROD/com2/gfs/para/$CDUMP.$DDATE/$CYC
 [ -d $COMOUT ] || mkdir -p $COMOUT
 if [ $? -ne 0 ]; then
  set +x; echo error creating $COMOUT;set -x
  exit 7
 fi

 COMOUTM=$COMPROD/com2/gfs/para/$CDUMP.$DDATEM/$cycm
 if [ -d $COMOUTM ]; then rm -r $COMOUTM ;fi

 for file in *_${CDATE}*; do
  echo $file
  base=${file}
  echo $base
  [ -n "$FH" ] && unset FH
  [ -n "$NCO_BASE" ] && unset NCO_BASE
  FOUND=YES
  case $base in
   siganl*) MEM=${file#siganl_${CDATE}_mem};NCO_BASE=ratmanl.mem$MEM.nemsio;;
   sanl*ensmean) NCO_BASE=atmanl.ensmean.nemsio;;
   sanl*mem*) MEM=${file#sanl_${CDATE}_mem};NCO_BASE=atmanl.mem$MEM.nemsio;;
   sfcanl*ensmean) NCO_BASE=sfcanl.ensmean.nemsio;;
   sfcanl*mem*) MEM=${file#sfcanl_${CDATE}_mem};NCO_BASE=sfcanl.mem$MEM.nemsio;;
   sfcgcy*ensmean) NCO_BASE=gcyanl.ensmean.nemsio;;
   sfcgcy*mem*) MEM=${file#sfcgcy_${CDATE}_mem};NCO_BASE=gcyanl.mem$MEM.nemsio;;
   nstanl*mem*) MEM=${file#nstanl_${CDATE}_mem};NCO_BASE=nstanl.mem$MEM.nemsio;;
   sfg*ensmean) FH=${file#sfg_${CDATE}_fhr};FH=${FH%%_ensmean};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf$FH.ensmean.nemsio;;
   sfg*ensmean.nc4) FH=${file#sfg_${CDATE}_fhr};FH=${FH%%_ensmean.nc4};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf$FH.ensmean.nc4;;
   sfg*ensspread.nc4) FH=${file#sfg_${CDATE}_fhr};FH=${FH%%_ensspread.nc4};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf$FH.ensspread.nc4;;
   sfg*s_mem*) MEM=${file#*mem};FH=${file#sfg_${CDATE}_fhr};FH=${FH%%_mem*};FH=${FH%%s*};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf${FH}s.mem${MEM}.nemsio;;
   sfg*mem*) MEM=${file#*mem};FH=${file#sfg_${CDATE}_fhr};FH=${FH%%_mem*};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf${FH}.mem${MEM}.nemsio;;
   bfg*ensmean) FH=${file#bfg_${CDATE}_fhr};FH=${FH%%_ensmean};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=sfcf$FH.ensmean.nemsio;;
   bfg*mem*) MEM=${file#*mem};FH=${file#bfg_${CDATE}_fhr};FH=${FH%%_mem*};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=sfcf${FH}.mem${MEM}.nemsio;;
   nfg*ensmean) FH=${file#nfg_${CDATE}_fhr};FH=${FH%%_ensmean};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=nstf$FH.ensmean.nemsio;;
   nfg*mem*) MEM=${file#*mem};FH=${file#nfg_${CDATE}_fhr};FH=${FH%%_mem*};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=nstf${FH}.mem${MEM}.nemsio;;
   flg*mem*) MEM=${file#*mem};FH=${file#flg_${CDATE}_fhr};FH=${FH%%_mem*};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=flxf${FH}.mem${MEM}.nemsio;;
   obsinput*ensmean) NCO_BASE=obsinput.ensmean;;
   gsistat*ensmean) NCO_BASE=gsistat.ensmean;;
   gsistat*mem*) MEM=${file#gsistat_${CDATE}_mem};NCO_BASE=gsistat.mem$MEM;;
   cnvstat*ensmean) NCO_BASE=cnvstat.ensmean;;
   cnvstat*mem*) MEM=${file#cnvstat_${CDATE}_mem};NCO_BASE=cnvstat.mem$MEM;;
   oznstat*ensmean) NCO_BASE=oznstat.ensmean;;
   oznstat*mem*) MEM=${file#oznstat_${CDATE}_mem};NCO_BASE=oznstat.mem$MEM;;
   radstat*ensmean) NCO_BASE=radstat.ensmean;;
   radstat*mem*) MEM=${file#radstat_${CDATE}_mem};NCO_BASE=radstat.mem$MEM;;
   biascr_int*ensmean) NCO_BASE=biascr_int.ensmean;;
   enkfstat*) NCO_BASE=enkfstat;;
   transfer*) FOUND=NA;;
   ensstat*) FOUND=NA;;
   fcsstat*) FOUND=NA;;
   omgstat*) FOUND=NA;;
   config*) FOUND=NA;;
   *) FOUND=NO;;
  esac

  if [ $FOUND = YES ]; then
   ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
  else
   if [ $FOUND = NO ]; then
     ln -sf $COMIN/$file $COMOUT/$file
   fi
  fi
 done
fi

# Create nwges2 look-alike name
# Process GDAS and GFS
if [ $CDUMP = gdas -o $CDUMP = gfs ]; then
 case $CDUMP in
  gdas) NCO_DUMP=gdas;PRE=gdas;;
  gfs) NCO_DUMP=gfs;PRE=gfs;;
  *) echo UH-OH. $CDUMP;continue;;  # default
 esac

 COMOUT=$COMPROD/com2/gfs/nwges2/$CDUMP.$DDATE
 [ -d $COMOUT ] || mkdir -p $COMOUT
 if [ $? -ne 0 ]; then
  set +x; echo error creating $COMOUT;set -x
  exit 7
 fi

 COMOUTM=$COMPROD/com2/gfs/nwges2/$CDUMP.$DDATEM
 if [ -d $COMOUTM ]; then rm -r $COMOUTM/${PRE}.t${cycm}* ;fi

 for file in *.$CDUMP.$CDATE; do

  echo $file

  base=${file%%.$CDUMP.$CDATE}
  echo $base



  [ -n "$FH" ] && unset FH
  [ -n "$NCO_BASE" ] && unset NCO_BASE

  FOUND=YES

  case $base in
   sfnanl) NCO_BASE=sfcanl.nemsio;;
   gfnanl) NCO_BASE=atmanl.nemsio;;
   sfnf*) FH=${base#sfnf};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=sfcf$FH.nemsio;;
   gfnf*) FH=${base#gfnf};[ $FH -lt 100 ] && FH=0${FH};NCO_BASE=atmf$FH.nemsio;;
   biascr) NCO_BASE=abias;;
   biascr_pc) NCO_BASE=abias_pc;;
   aircraft_t_bias) NCO_BASE=abias_air;;
   *) FOUND=NO;;
  esac
  
  if [ $FOUND = YES ]; then
   ln -sf $COMIN/$file $COMOUT/$PRE.t${CYC}z.$NCO_BASE
  else
   echo "NOT FOUND:  $COMIN/$file"
  fi

 done # end for file
fi

exit

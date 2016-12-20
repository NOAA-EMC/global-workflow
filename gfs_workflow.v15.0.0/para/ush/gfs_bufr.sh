#!/bin/ksh
#
#  UTILITY SCRIPT NAME :  gfsbufr.sh
#               AUTHOR :  Hua-Lu Pan
#         DATE WRITTEN :  02/03/97
#
#  Abstract:  This utility script produces BUFR file of
#             station forecasts from the GFS suite.
#
#     Input:  none
#
echo "History: February 2003 - First implementation of this utility script"
#

set -ax

if test "$F00FLAG" = "YES"
then
   f00flag=".true."
else
   f00flag=".false."
fi

export pgm=gfs_flux
#. prep_step

cat << EOF > gfsflxparm
 &NAMKEN
  nout=$FINT,lonf=$LONB,latg=$LATB,nsfc=80,
  nstart=$FSTART,nend=$FEND,nint=$FINT,nzero=$NZERO,f00=$f00flag,
/
EOF
hh=$FSTART
while  test $hh -le $FEND
do  
   ln -sf $COMIN/${RUN}.${cycle}.sfluxgrbf${hh} flxf${hh} 
   hh=` expr $hh + $FINT `
   if test $hh -lt 10
   then
      hh=0$hh
   fi
done

ln -sf $PARMbufr/bufr_stalist.meteo.gfs${FINT} fort.8
#ln -sf $PARMbufr/parm_snd/bufr_stalist.meteo.gfs${FINT}.update fort.8
#ln -sf $STNLIST fort.8
ln -sf gfs12.dat fort.17
ln -sf metflxmrf fort.18

#startmsg
#$EXECbufr/gfs_flux < gfsflxparm >> $pgmout 2>>errfile
#$HOMEbufr/sorc/gfs_flux.fd/gfs_flux < gfsflxparm >> $pgmout 
#/nwprod/exec/gfs_flux < gfsflxparm >> $pgmout 
$GFLUX < gfsflxparm >> $pgmout 
export err=$?

#rm flxf*

export pgm=gfs_bufr
#. prep_step

if test "$MAKEBUFR" = "YES"
then
   bufrflag=".true."
else
   bufrflag=".false."
fi

mkdir -p $COMOUT/bufr.${cycle}
mkdir -p $COMOUT/bufr.${cycle}/bufr$FINT

cat << EOF > gfsparm
 &NAMMET
  iromb=0,maxwv=$JCAP,levs=$LEVS,makebufr=$bufrflag,
  dird="$COMOUT/bufr.${cycle}/bufr$FINT",
  nstart=$FSTART,nend=$FEND,nint=$FINT,nsfc=80,f00=$f00flag,
/
EOF

hh=$FSTART
while  test $hh -le $FEND
do  
   ln -sf $COMIN/${RUN}.${cycle}.sf${hh} sigf${hh} 

   hh=` expr $hh + $FINT `
   if test $hh -lt 10
   then
      hh=0$hh
   fi
done  

#  define input BUFR table file.
ln -sf $PARMbufr/bufr_gfs${FINT}_class1.tbl fort.1
#ln -sf $PARMbufr/parm_snd/bufr_gfs${FINT}_class1.tbl fort.1
ln -sf $PARMbufr/bufr_stalist.meteo.gfs${FINT} fort.8
#ln -sf $PARMbufr/parm_snd/bufr_stalist.meteo.gfs${FINT}.update fort.8
#ln -sf $STNLIST fort.8
ln -sf metflxmrf fort.12
ln -sf $SIGLEVEL fort.13

#startmsg
#mpirun.lsf $EXECbufr/gfs_bufr < gfsparm >> $pgmout 2>>errfile
#mpirun.lsf $HOMEbufr/sorc/gfs_bufr.fd/gfs_bufr < gfsparm >> $pgmout 
#mpirun.lsf $GLOPARABIN/gfs_bufr < gfsparm >> $pgmout 
$launcher $GBUFR < gfsparm >> $pgmout
export err=$?

#rm  metflxmrf gfs12.dat meteogram

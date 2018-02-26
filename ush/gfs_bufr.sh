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
# Script History Log:
# 2016-10-30  H Chuang: Tranistion to read nems output.
#             Change to read flux file fields in gfs_bufr
#             so remove excution of gfs_flux
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
   hh=` expr $hh + $FINT `
   if test $hh -lt 10
   then
      hh=0$hh
   fi
done

export pgm=gfs_bufr
#. prep_step

if test "$MAKEBUFR" = "YES"
then
   bufrflag=".true."
else
   bufrflag=".false."
fi

cat << EOF > gfsparm
 &NAMMET
  iromb=0,maxwv=$JCAP,levs=$LEVS,makebufr=$bufrflag,
  dird="$COMOUT/bufr.${cycle}/bufr",
  nstart=$FSTART,nend=$FEND,nint=$FINT,nsfc=80,f00=$f00flag,
/
EOF

hh=$FSTART
   if test $hh -lt 100
   then
      hh1=`echo "${hh#"${hh%??}"}"`
      hh=$hh1
   fi
while  test $hh -le $FEND
do  
   if test $hh -lt 100
   then
      hh2=0$hh
   else
      hh2=$hh
   fi

   ln -sf $COMIN/${RUN}.${cycle}.atmf${hh2}.nemsio sigf${hh} 
   ln -sf $COMIN/${RUN}.${cycle}.sfcf${hh2}.nemsio flxf${hh}
##   ln -sf $COMIN/${RUN}.${cycle}.flxf${hh2}.nemsio flxf${hh}

   hh=` expr $hh + $FINT `
   if test $hh -lt 10
   then
      hh=0$hh
   fi
done  

#  define input BUFR table file.
ln -sf $PARMbufrsnd/bufr_gfs_class1.tbl fort.1
ln -sf ${STNLIST:-$PARMbufrsnd/bufr_stalist.meteo.gfs} fort.8
#ln -sf metflxmrf fort.12
#ln -sf $SIGLEVEL fort.13

#startmsg
export APRUN=${APRUN:-'aprun -n 12 -N 3 -j 1'}
${APRUN:-mpirun.lsf} ${GBUFR:-$EXECbufrsnd/gfs_bufr} < gfsparm > out_gfs_bufr_$FEND
export err=$?

##rm  metflxmrf gfs12.dat 

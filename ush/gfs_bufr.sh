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
# 2018-03-22 Guang Ping Lou: Making it works for either 1 hourly or 3 hourly output
# 2018-05-22 Guang Ping Lou: Making it work for both GFS and FV3GFS 
# 2018-05-30  Guang Ping Lou: Make sure all files are available.
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
  nstart=$FSTART,nend=$FEND,nint=$FINT,
  nend1=$NEND1,nint1=$NINT1,nint3=$NINT3,
  nzero=$NZERO,f00=$f00flag,
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

if [ -s ${COMIN}/${RUN}.${cycle}.sfcf000.nemsio ]; then
 SFCF="sfc"
 CLASS="class1fv3"
 else
 SFCF="flx"
 CLASS="class1"
fi 
cat << EOF > gfsparm
 &NAMMET
  iromb=0,maxwv=$JCAP,levs=$LEVS,makebufr=$bufrflag,
  dird="$COMOUT/bufr.${cycle}/bufr",
  nstart=$FSTART,nend=$FEND,nint=$FINT,
  nend1=$NEND1,nint1=$NINT1,nint3=$NINT3,
  nsfc=80,f00=$f00flag,
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

#---------------------------------------------------------
# Make sure all files are available:
   ic=0
   while [ $ic -lt 1000 ]
   do
      if [ ! -f $COMIN/${RUN}.${cycle}.logf${hh2}.nemsio ]
      then
          sleep 10
          ic=`expr $ic + 1`
      else
          break
      fi

      if [ $ic -ge 360 ]
      then
         err_exit "COULD NOT LOCATE logf${hh2} file AFTER 1 HOUR"
      fi
   done
#------------------------------------------------------------------
   ln -sf $COMIN/${RUN}.${cycle}.atmf${hh2}.nemsio sigf${hh} 
   ln -sf $COMIN/${RUN}.${cycle}.${SFCF}f${hh2}.nemsio flxf${hh}

   hh=` expr $hh + $FINT `
   if test $hh -lt 10
   then
      hh=0$hh
   fi
done  

#  define input BUFR table file.
ln -sf $PARMbufrsnd/bufr_gfs_${CLASS}.tbl fort.1
ln -sf ${STNLIST:-$PARMbufrsnd/bufr_stalist.meteo.gfs} fort.8

#startmsg
##export APRUN=${APRUN_POSTSND:-'aprun -n 12 -N 3 -j 1'}
##${APRUN:-mpirun.lsf} ${GBUFR:-$EXECbufrsnd/gfs_bufr} < gfsparm > out_gfs_bufr_$FEND
##mpirun $EXECbufrsnd/gfs_bufr < gfsparm > out_gfs_bufr_$FEND
${APRUN_POSTSND} $EXECbufrsnd/gfs_bufr < gfsparm > out_gfs_bufr_$FEND
export err=$?;err_chk

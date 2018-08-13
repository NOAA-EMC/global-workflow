#! /bin/sh
#
# Metafile Script : gfs_meta_mar_ver.sh
#
# Log :
# J. Carr/PMB    12/08/2004    Pushed into production
#
# Set up Local Variables
#
set -x
#
export PS4='MAR_VER:$SECONDS + '
mkdir -p -m 775 $DATA/MAR_VER
cd $DATA/MAR_VER
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
metatype="mar_ver"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`

export pgm=gdplot2_nc;. prep_step; startmsg

$GEMEXE/gdplot2_nc << EOFplt
\$MAPFIL=hipowo.gsf+mefbao.ncp
gdfile	= F-${MDL} | ${PDY2}/${cyc}00
gdattim	= f00-f48-6
GLEVEL  = 9950
GVCORD  = sgma
PANEL   = 0
SKIP    = 0
SCALE   = 999
GDPFUN  = mask(drct,sea)
TYPE    = p
CONTUR  = 0
CINT    = 0
LINE    = 3
FINT    = 0
FLINE   =
HILO    =
HLSYM   =
CLRBAR  =
WIND    = 
REFVEC  =
TITLE   = 31/-2/~ ? ${MDL} Gridded BL Wind Direction (40m AGL)|~ WATL GRIDDED WIND DIR!0
TEXT    = 0.8/21/1/hw
CLEAR   = YES
GAREA   = 27.2;-81.9;46.7;-61.4
PROJ    = STR/90.0;-67.0;1.0
MAP     = 31+6
LATLON  = 18/1/1/1;1/5;5
DEVICE  = $device
STNPLT  = 31/1.3/22/1.6/hw|25/19/1.3/1.6|buoys.tbl
SATFIL  =
RADFIL  =
LUTFIL  =
STREAM  =
POSN    = 0
COLORS  = 5
MARKER  = 0
GRDLBL  = 0
FILTER  = NO
li
run

GDPFUN  = mask(mul(sped,1.94),sea)
TITLE   = 31/-2/~ ? ${MDL} Gridded BL Wind Speed (40m AGL)|~ WATL GRIDDED WIND SPEED!0
li
run

\$MAPFIL=hipowo.gsf+himouo.nws
GAREA   = 30;-133;48;-120
PROJ	= MER
GDPFUN  = mask(drct,sea)
TITLE   = 31/-2/~ ? ${MDL} Gridded BL Wind Direction (40m AGL)|~ EPAC GRIDDED WIND DIR!0
li
run

GDPFUN  = mask(mul(sped,1.94),sea)
TITLE   = 31/-2/~ ? ${MDL} Gridded BL Wind Speed (40m AGL)|~ EPAC GRIDDED WIND SPEED!0
li
run
exit
EOFplt

export err=$?;err_chk
#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l $metaname
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_ver
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_ver
   fi
fi

exit

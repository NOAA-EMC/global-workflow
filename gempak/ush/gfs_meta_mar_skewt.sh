#! /bin/sh
#
# Metafile Script : gfs_meta_mar_skewt.sh
#
# Log :
# J. Carr/PMB     12/08/2004    Pushed into production

# Set up Local Variables
#
set -x
#
export PS4='MAR_SKEWT:$SECONDS + '
mkdir -p -m 775 $DATA/MAR_SKEWT
cd $DATA/MAR_SKEWT
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
metatype="mar_skewt"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`

for fhr in 000 006 012 018 024 030 036 042 048 054 060 066 072
do
    export pgm=gdprof;. prep_step; startmsg

$GEMEXE/gdprof << EOFplt
GDATTIM  = F${fhr}
GVCORD   = PRES
GDFILE   = F-${MDL}
GVECT    = wnd
LINE     = 2/1/2
MARKER   = 5/5/1
BORDER   = 1/1/1
PTYPE    = SKEW
SCALE    = 0
XAXIS    = -40/50/10/1;1;1
YAXIS    = 1050/100//1;1;1
WIND     = bk1
REFVEC   = 
WINPOS   = 1
FILTER   = no
PANEL    = 0
TEXT     = 1.2/22/2/hw
DEVICE   = $device
OUTPUT   = T
THTALN   = 18/1/1
THTELN   = 23/2/1
MIXRLN   = 23/9/1

! Buoy 44004
GFUNC   = tmpc
TITLE   = 5//~ ? ${MDL} @ 44004 (Hotel 38.5N 70.7W) SNDG|^ Buoy 44004 SNDG
GPOINT  = 38.5;-70.7
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 41001
GFUNC   = tmpc
LINE	= 2/1/2
TITLE   = 5//~ ? ${MDL} @ 41001 (E.Hatteras 34.7N 72.6W) SNDG|^ Buoy 41001 SNDG
GPOINT  = 34.7;-72.6
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 41002
GFUNC   = tmpc
LINE	= 2/1/2
TITLE   = 5//~ ? ${MDL} @ 41002 (S.Hatteras 32.3N 75.2W) SNDG|^ Buoy 41002 SNDG
GPOINT  = 32.3;-75.2
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 44005
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ 44005 (Gulf of Maine 42.9N 68.9W) SNDG|^ Buoy 44005 SNDG
GPOINT  = 42.9;-68.9
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 44009
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL} @ 44009 (Delaware Bay 38.5N 74.7W) SNDG|^ Buoy 44009 SNDG
GPOINT  = 38.5;-74.7
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 44011
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ 44011 (George's Bank 41.1N 66.6W) SNDG|^ Buoy 44011 SNDG
GPOINT  = 41.1;-66.6
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 46005
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL} @ 46005 (Washington 46.1N 131W) SNDG|^ Buoy 46005 SNDG
GPOINT  = 46.1;-131
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 46002
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ 46002 (Oregon 42.5N 130.3W) SNDG|^ Buoy 46002 SNDG
GPOINT  = 42.5;-130.3
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 46059
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ 46059 (Pt. Arena 38N 130W) SNDG|^ Buoy 46059 SNDG
GPOINT  = 38.0;-130
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! Buoy 46001
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ 46001 (Gulf of Alaska 56.3N 148.2W) SNDG|^ Buoy 46001 SNDG
GPOINT  = 56.3;-148.2
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru


! Buoy 46023
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ 46023 (Pt Conception 34.3N 120.7W) SNDG|^ Buoy 46023 SNDG
GPOINT  = 34.3;-120.7
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! TXKF - Bermuda
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ Bermuda SNDG|^ Bermuda SNDG
GPOINT  = TXKF
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! HAT - Hatteras, NC
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL} @ Cape Hatteras NC SNDG|^ Cape Hatteras SNDG
GPOINT  = HAT
CLEAR   = yes
ru

GFUNC   = dwpc
LINE	= 4/1/2
CLEAR   = no
ru

! ILM - Wilmington, NC
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ Wilmington NC SNDG|^ Wilmington SNDG
GPOINT  = 34.3;-78
CLEAR   = yes
ru

GFUNC   = dwpc
LINE    = 4/1/2
CLEAR   = no
ru

! SBY - Salisbury, MD
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ Salisbury MD SNDG|^ Salisbury SNDG
GPOINT  = 38.1;-75.5
CLEAR   = yes
ru

GFUNC   = dwpc
LINE    = 4/1/2
CLEAR   = no
ru

! ORF - Norfolk, VA
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL} @ Norfolk VA SNDG|^ Norfolk SNDG
GPOINT  = 37.0;-76.2
CLEAR   = yes
ru

GFUNC   = dwpc
LINE    = 4/1/2
CLEAR   = no
ru

! CHS - Charleston, SC
GFUNC   = tmpc
LINE     = 2/1/2
TITLE   = 5//~ ? ${MDL}  @ Charleston SC SNDG|^ Charleston SC SNDG
GPOINT  = 33.0;-80.0
CLEAR   = yes
ru

GFUNC   = dwpc
LINE    = 4/1/2
CLEAR   = no
ru

exit
EOFplt
export err=$?;err_chk

done

$GEMEXE/gpend

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l $metaname
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_skewt
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_skewt
   fi
fi

exit

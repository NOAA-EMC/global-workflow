#!/bin/sh 

#
# Metafile Script : gdas_meta_na
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# LJ REED 4/10/98 added line to define BIN_DIR
# J. Carr/HPC        2/99   Changed skip to 0
# B. Gordon          4/00   Modified for production on IBM-SP
#                           and changed gdplot_nc -> gdplot2_nc
# D. Michaud         4/01   Added logic to display different title
#                           for parallel runs
# J. Carr           11/04   Added a ? in all title/TITLE lines.
# J. Carr           11/04   Changed GAREA and PROJ to match GFS and NAM.
#

cd $DATA

set -xa

device="nc | gdas.meta"

PDY2=`echo $PDY | cut -c3-`

if [ "$envir" = "para" ] ; then
   export m_title="GDASP"
else
   export m_title="GDAS"
fi

export pgm=gdplot2_nc; prep_step
startmsg

$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-GDAS | ${PDY2}/${cyc}00
GDATTIM	= FALL
DEVICE	= $device
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP     = 1
CLEAR	= yes
CLRBAR  = 1

!PROJ    = str/90;-95;0
!GAREA   = -5;-135;50;-5
!LATLON	= 0
GAREA   = 17.529;-129.296;53.771;-22.374
PROJ    = str/90;-105;0
LATLON  = 1


restore $USHgempak/restore/pmsl_thkn.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title PMSL, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
ru


restore $USHgempak/restore/850mb_hght_tmpc.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
SKIP    = 0         !0         !0         !0         !/3
FILTER  = NO
TITLE	= 5/-2/~ ? $m_title @ HGT, TEMP AND WIND (KTS)|~@ HGT, TMP, WIND!0
l
ru


restore $USHgempak/restore/700mb_hght_relh_omeg.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT, REL HUMIDITY AND OMEGA|~@ HGT, RH AND OMEGA!0
l
ru


restore $USHgempak/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
l
ru


restore $USHgempak/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? $m_title @ HGT, ISOTACHS AND WIND (KTS)|~@ HGT AND WIND!0
l
ru

exit
EOF
export err=$?;err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l gdas.meta
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
  mv gdas.meta ${COMOUT}/gdas_${PDY}_${cyc}_na
  export err=$?
  if [[ $err -ne 0 ]] ; then
    echo " File gdas.meta does not exist."
    exit $err
  fi

  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gdas_${PDY}_${cyc}_na
  fi
fi

#

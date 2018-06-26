#!/bin/sh
#
# Metafile Script : gfs_meta_ak.sh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# D.W.Plummer/NCEP   3/97   Added ecmwf comparison.
# D.W.Plummer/NCEP   3/97   Added $MAPFIL specification for lower resolution
# D.W.Plummer/NCEP   4/97   Removed run from 3-HOURLY PRECIP
# J. Carr/HPC        2/99   Changed skip to 0
# B. Gordon/NCO      5/00   Modified for production on IBM-SP
#                           Changed gdplot_nc -> gdplot2_nc
# D. Michaud/NCO     4/01   Modified to Reflect Different Title for
#                           Parallel runs
# J. Carr/PMB       11/04   Added a ? to all title lines
#                           Changed contur from a 1 to a 2.
# M. Klein/HPC       6/07   Modify for Alaska medium-range desk and rename script.
#

cd $DATA

set -xa

cp $FIXgempak/datatype.tbl datatype.tbl

device="nc | gfs.meta.ak"
PDY2=`echo $PDY | cut -c3-`

fend=F216

if [ "$envir" = "para" ] ; then
   export m_title="GFSP"
else
   export m_title="GFS"
fi

export pgm=gdplot2_nc;. prep_step
startmsg

$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-GFS | ${PDY2}/${cyc}00
GDATTIM	= F00-$fend-6
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP     = 1
CLEAR	= yes
CLRBAR  = 1
SKIP    = 0

GAREA	= 35.0;178.0;78.0;-94.0
PROJ	= NPS
LATLON	= 18/2/1/1;1/10;10

GLEVEL  = 500:1000!500:1000!0
GVCORD  = pres    !pres    !none
SCALE   = -1        !-1             !0
GDPFUN  = ldf(hght) !ldf(hght)      !pmsl
TYPE    = c
CONTUR  = 2         !2              !7
CINT    = 6/0/540   !6/546/999      !4
LINE    = 6/3/2     !2/3/2          !20//3
FINT    =
FLINE   =
HILO    = !! 26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM   = 2;1.5//21//hw
CLRBAR  = 1
WIND    =
REFVEC  =
TITLE	= 5/-2/~ ? $m_title PMSL, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
run

GLEVEL  = 4400:10000    !0
GVCORD  = sgma          !none
SKIP    = 0 
SCALE   = 0
GDPFUN  = sm5s(relh)     !pmsl
TYPE    = c/f            !c
CINT    = 50;70;90;95    !4
LINE    = 32//2/0        !20//3
FINT    = 50;70;90
FLINE   = 0;24;23;22
HILO    = 26;2/H#;L#/1018-1070;900-1012//30;30/y
HLSYM   = 2;1.5//21//hw
CLRBAR  = 1
TITLE   = 5/-2/~ ? $m_title PMSL, 1000-500MB MEAN RH|~MSLP, 1000-500 MEAN RH!0
run

GLEVEL  = 850
GVCORD  = pres 
SKIP    = 0         !0         !0         !0         !0/1;-1
SCALE   = 0         !0         !0         !-1        !0
GDPFUN  = sm9s(tmpc)!sm9s(tmpc)!sm9s(tmpc)!sm5s(hght)!kntv(wnd)
TYPE    = c/f       !c         !c         !c         !b
CONTUR  = 2
CINT    = 3/-99/50  !3/3/18   !3/21/99    !3
LINE    = 27/1/2 !2/1/2    !16/1/2     !20/1/1/1
FINT    = -24;-18;-12;-6;0;18
FLINE   = 24;30;28;29;25;0;17
HILO    =
HLSYM   =
WIND    = 18//1
TITLE	= 5/-2/~ ? $m_title @ HGT, TEMPERATURE AND WIND (KTS)|~@ HGT, TMP, WIND!0
l
run

GLEVEL  = 700
SKIP    = 0              !0           !0         !0           !0
SCALE   = 0              !0           !-1        !3           !3
GDPFUN  = sm5s(relh)     !sm5s(relh)  !sm5s(hght)!sm9s(omeg)  !sm9s(omeg)
TYPE    = c              !c/f         !c
CINT    = 10;30   !50;70;90  !3 !-15;-13;-11;-9;-7;-5;-3;-1 !1;3;5;7;9;11
LINE    = 8//2/0         !23//2/0        !20/1/1/1 !6/1/1/1 ! 24/5/1/1
FINT    = 70;90
FLINE   = 0;23;22
WIND    =
TITLE	= 5/-2/~ ? $m_title @ HGT, REL HUMIDITY AND OMEGA|~@ HGT, RH AND OMEGA!0
l
run

GLEVEL  = 500
SKIP    = 0                  !0             !0
SCALE   = 5                  !5             !-1
GDPFUN  = abs(avor(wnd))//v  !v             !sm5s(hght)
TYPE    = c/f                !c             !c
CINT    = 2/10/20            !2/4/8         !6
LINE    = 7/5/1/2            ! 29/5/1/2     !5/1/2/1
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!
HLSYM   = 
TITLE	= 5/-2/~ ? $m_title @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
l
run

GLEVEL  = 250
SKIP    = 0/1;-1
SCALE   = 0                           !-1        !0
GDPFUN  = knts((mag(wnd)))            !sm9s(hght)!kntv(wnd)
TYPE    = c/f                         !c         !b
CINT    = 30;50;70;90;110;130;150       ! 12
LINE    = 27/5/2/1               ! 20/1/2/1
FINT    = 70;90;110;130;150
FLINE   = 0;25;24;29;7;15
HILO    =
HLSYM   =
WIND    = 18//1
TITLE	= 5/-2/~ ? $m_title @ HGT, ISOTACHS AND WIND (KTS)|~@ HGT AND WIND!0
l
run

GDATTIM = F06-F180-6
GLEVEL  = 0
SKIP    = 0
GVCORD  = none
SCALE   = 0   
GDPFUN  = p06i !pmsl
TYPE    = f    !c
CONTUR  = 2    
CINT    =      !4
LINE    =      !5/1/1/0
FINT    = .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9
FLINE   = 0;21-30;14-20;5
HILO    =      !26;2////30;30/y
HLSYM   =      2;1.5//21//hw
WIND    = 
TITLE	= 5/-2/~ ? $m_title 6-HR TOTAL PCPN, MSLP|~6-HR TOTAL PCPN, MSLP!0
l
run

GDPFUN  = p06i 
TYPE    = f  
HILO    = 31;0/x#2////y
HLSYM   = 1.5
TITLE   = 5/-2/~ ? $m_title 6-HR TOTAL PCPN |~6-HR TOTAL PCPN!0
run

GDATTIM = F12-$fend-06
GDPFUN  = p12i  !pmsl
TYPE    = f     !c
HILO    =       !26;2////30;30/y
HLSYM   =       2;1.5//21//hw
TITLE   = 5/-2/~ ? $m_title 12-HR TOTAL PCPN, MSLP|~12-HR TOTAL PCPN, MSLP!0
run

GDPFUN  = p12i 
TYPE    = f
HILO    = 31;0/x#2////y
HLSYM   = 1.5
TITLE	= 5/-2/~ ? $m_title 12-HR TOTAL PCPN (IN)|~12-HR TOTAL PCPN!0
l
run


GDATTIM	= F24-$fend-06
GDPFUN  = p24i  !pmsl
TYPE    = f     !c
HILO    =       !26;2////30;30/y
HLSYM   =       2;1.5//21//hw
TITLE   = 5/-2/~ ? $m_title 24-HR TOTAL PCPN, MSLP|~24-HR TOTAL PCPN, MSLP!0
run 

GDPFUN  = p24i      
TYPE    = f
HILO    = 31;0/x#2////y
HLSYM   = 1.5
TITLE	= 5/-2/~ ? $m_title 24-HR TOTAL PCPN (IN)|~24-HR TOTAL PCPN
run

exit
EOF
export err=$?;err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l gfs.meta.ak
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk


if [ $SENDCOM = "YES" ] ; then
  mv gfs.meta.ak ${COMOUT}/gfs_${PDY}_${cyc}_ak
  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/gfs_${PDY}_${cyc}_ak
    if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
      DBN_ALERT_TYPE=GFS_METAFILE
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
       ${COMOUT}/gfs_${PDY}_${cyc}_ak
    fi
    if [ $fhr -eq 216 ] ; then
     ${DBNROOT}/bin/dbn_alert MODEL GFS_METAFILE_LAST $job \
       ${COMOUT}/gfs_${PDY}_${cyc}_ak
    fi
  fi
fi


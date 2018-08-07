#! /bin/sh
#
# Metafile Script : gfs_meta_sa.sh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# J.W.Carr/HPC       4/97   Changed Comparison to 1200 UTC UKMET instead of 0000 UTC UKMET
# J.W.Carr/HPC       4/97   Added UKMET2 --- past 72 hours to the comparison
# J.W.Carr/HPC       2/98   changed garea of sfc conv, bl dwpt and wind product
# J.W.Carr/HPC       5/98   converted gdplot to gdplot2
# J.W.Carr/HPC       8/98   Changed map to medium resolution
# J. Carr/HPC        7/99   Put a filter on map.
# J. Carr/HPC       02/2001 Updated to run on IBM and send to ncodas
# J. Carr/HPC       04/2001 Remove old metafiles from metaout before creating new ones.
# J. Carr/HPC        5/2001 Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC        6/2001 Converted to a korn shell prior to delivering script to Production.
# J. Carr/HPC        8/2001 Submitted.
# J. Carr/HPC        3/2002 Tweaked a few products.
#
# Set Up Local Variables
#
set -x
#
export PS4='SA:$SECONDS + '
mkdir -p -m 775 $DATA/SA
cd $DATA/SA
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL=GFS
metatype="sa"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo ${PDY} | cut -c3-`
#
#if [ ${cyc} -eq 00 ] ; then
#    fend=F126
#elif [ ${cyc} -eq 12 ] ; then
#    fend=F126
#else
#    fend=F126
#fi

fend=F126
#
export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-${MDL} | ${PDY2}/${cyc}00
GDATTIM	= F00-${fend}-06 
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 1
MAP	= 1/1/1/yes
CLEAR	= yes
CLRBAR  = 1
PROJ    = mer//3;3;0;1
GAREA   = -66;-127;14.5;-19
LATLON	= 1//1/1/10
FILTER  = yes

GLEVEL  = 500:1000      !500:1000      !0
GVCORD  = pres          !pres          !none
SKIP    = 0
SCALE   = -1            !-1            !0
GDPFUN  = sm5s(ldf(hght)!sm5s(ldf(hght)!sm5s(pmsl)
TYPE    = c
CINT    = 3/400/540     !3/543         !4
LINE    = 6/3/2         !2/3/2/2       !20//3
FINT    =
FLINE   =
HILO    = !!26;2/H#;L#/1020-1070;900-1012//30;30/y
HLSYM   = 1.3;1.3//21//hw
CLRBAR  = 1
WIND    = !                         ! 
REFVEC  = 
TITLE	= 1/-2/~ ${MDL} MSLP, 1000-500mb THICK|~MSLP, 1000-500 THKN!
ru

GLEVEL  = 850:1000                     !0
GVCORD  = pres                         !none
PANEL   = 0
SKIP    = 0
SCALE   = -1                           !0
GDPFUN  = sm5s(ldf(hght)               !sm5s(pmsl)
TYPE    = c                            !c
CONTUR  = 2
CINT    = 1                            !4
LINE    = 22/5/2/1                     !10/1/1
FINT    =
FLINE   =
HILO    =                              !26;2/H#;L#/1020-1070;900-1016/3/30;30/y
HLSYM   =                              !2;1.5//21//hw
WIND    = 0
TITLE   = 1/-1/~ ${MDL} PMSL, 1000-850mb THKN|~PMSL, 1000-850 THKN!
r

glevel  = 0!0!500:1000!500:1000
gvcord  = none!none!PRES!PRES
SKIP    = 0
scale   = 0!0!-1!-1
gdpfun  = sm5s(pwtr)!sm5s(pmsl)!ldf(hght)!ldf(hght)
type    = c/f!c!c!c
cint    = 13;25;38;50;62!4!4/0/540!4/544/600
line    = 32/1/1!6/1/3!5/5/2!17/5/2
fint    = 13;25;38;50
fline   = 0;23;22;21;2      
hilo    = 26;2/H#;L#/1017-1050;930-1004/2//y
HLSYM   = 0!1.5;1.5//22;22/3;3/hw!0
clrbar  = 1/V/LL!0
wind    =  
refvec  =
title   = 1/-2/~ ${MDL} PW, EST MSLP, THICKNESS|~PRECIP WATER, MSLP!0
r

glevel  = 0!0 
gvcord  = none 
skip    = 0 
scale   = 0
gdpfun  = sm5s(lft4)!sm5s(lft4)!sm5s(lft4)!kntv(wnd@9950%sgma)
type    = c/f       !c         !c         !b
cint    = 3/3       !1/-0.5/0.5!3/-15/-3
line    = 25/1/1    !22/1/2    !21/1/1  
fint    = -9;-6;-3;3;6 
fline   = 2;15;22;0;0;24
hilo    = 0!0
hlsym   = 1;1//22;22/2;2/hw
clrbar  = 1/V/LL!0
wind    = bk0!bk0!bk0!bk9/0.9/2/112
refvec  = 
title   = 1/-2/~ ${MDL} LI AND BL WINDS|~LIFTED INDEX!0
r

PROJ    = mer
GAREA   = -60;-114.5;-4.0;-29.9
LATLON	= 1//1/1/10

glevel  = 9950!9950!9950
gvcord  = sgma!sgma!sgma
skip    = 0
scale   = 7   !0   !0
gdpfun  = sm5s(sdiv(mixr(dwpc;pres@0%none);wnd)!sm5s(dwpc)!sm5s(dwpc)!kntv(wnd@9950%sgma)
type    = f                               !c         !c         !b
cint    = 1//-1                           !3/12      !3/21
line    = 32                              !5//2      !6//2                      
clrbar  = 1/V/LL!0
fint    = -8;-6;-4;-2
fline   = 2;23;22;3;0
hilo    = 0
hlsym   = 0
wind    = bk0!bk0!bk0!bk9/0.7/2/112
refvec  =
title   = 1/-2/~ ${MDL} BL MOIST FLUX CONV, DEWPOINT & WIND |~BL MOISTURE CONV!0
run

PROJ    = mer//3;3;0;1
GAREA   = -66;-127;14.5;-19

GLEVEL  = 0            !0      !0         !0
GVCORD  = none         !none   !none      !none
SKIP    = 0
SCALE   = 0               
GDPFUN  = sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE    = c/f          !c      !c         !c         !b
CINT    = 3/-99/0      !3/3/21 !3/24/99   !4
LINE    = 27/1/2       !2/1/2  !16/1/2    !19//3
FINT    = -18;-15;-12;-9;-6;-3;0      
FLINE   = 30;29;7;6;4;25;24;0       
HILO    = 0            !0      !0         !26;2/H#;L#/1016-1050;930-1006/2//y
HLSYM   = 0            !0      !0         !1.5;1.5//22;22/3;3/hw
CLRBAR  = 1/V/LL       !0
WIND    = bk0          !bk0               !bk0       !bk9/0.7/2/112
REFVEC  =
TITLE   = 1/-2/~ ${MDL} MSLP, TEMP & WIND|~MSLP, TMP & WIND!0
CONTUR	= 1
ru

GAREA   = -50.4;-93.9;-10.8;-37.4
PROJ    = MER/0.0;-73.0;0.0

GDPFUN  = sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(pmsl)!kntv(wnd@9950%sgma)
TITLE   = 1/-2/~ ${MDL} MSLP, TEMP & WIND|~BL (SFC) TMP (ZOOM)!0
r

PROJ    = mer//3;3;0;1
GAREA   = -66;-127;14.5;-19

GLEVEL  = 9950         !9950   !9950      !0
GVCORD  = sgma!sgma!sgma!none
SKIP    = 0
SCALE   = 0               
GDPFUN  = sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE    = c/f          !c      !c         !c         !b
CINT    = 3/-99/0      !3/3/21 !3/24/99   !4
LINE    = 27/1/2       !2/1/2  !16/1/2    !19//3
FINT    = -18;-15;-12;-9;-6;-3;0      
FLINE   = 30;29;7;6;4;25;24;0       
HILO    = 0            !0      !0         !26;2/H#;L#/1016-1050;930-1006/2//y
HLSYM   = 0            !0      !0         !1.5;1.5//22;22/3;3/hw
CLRBAR  = 1/V/LL       !0
WIND    = bk0          !bk0    !bk0       !bk0       !bk9/0.7/2/112
REFVEC  =
TITLE   = 1/-2/~ ${MDL} MSLP, B1 TEMP & WIND|~MSLP, B1 TMP, WIND!0
ru

GDPFUN  = sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(pmsl)!kntv(wnd@9950%sgma)
GAREA   = -50.4;-93.9;-10.8;-37.4
PROJ    = MER/0.0;-73.0;0.0
TITLE   = 1/-2/~ ${MDL} MSLP, B1 TEMP & WIND|~BL (995 MB) TMP (ZOOM)!0
r

PROJ    = mer//3;3;0;1
GAREA   = -66;-127;14.5;-19

glevel  = 0
gvcord  = none
scale   = 0
gdpfun  = pwtr          !kntv(wnd@850%PRES)
type    = c/f           !b
cint    = 13;25;38;50;62!
line    = 32/1/2/1
fint    = 13;25;38;50
fline   = 0;23;22;21;2       
hilo    = 0             !0
HLSYM   = 0             !0
clrbar  = 1
wind    = bk0           !bk9/0.7/2/212
refvec  =
title   = 1/-2/~ ${MDL} 850 MB WIND & PW|~850 WND & PW!0
r

GLEVEL  = 4400:10000
GVCORD  = SGMA
SKIP    = 0
SCALE   = 0
GDPFUN  = sm5s(relh)   !sm5s(relh)
TYPE    = c/f          !c
CINT    = 10;20;80;90  !30;40;50;60;70
LINE    = 32//2        !23//2
FINT    = 10;30;70;90
FLINE   = 18;8;0;22;23
HILO    = 
HLSYM   =
CLRBAR  = 1
WIND    = 
REFVEC  = 
TITLE	= 1/-2/~ ${MDL} @ MEAN LAYER RH|~MEAN LAYER RH!0
ru

GLEVEL  = 700
GVCORD  = PRES
SKIP    = 0
SCALE   = 0         !0         !-1          !3
GDPFUN  = sm5s(relh)!sm5s(relh)!sm5s(hght)  !sm5s(omeg)
TYPE    = c/f       !c
CINT    = 90;105    !50;70     !6           !2/-45/-2
LINE    = 32//2/0   !23//1     !20/1/2/1    !6/1/1/1
FINT    = 70;90
FLINE   = 0;23;22
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    = bk0
REFVEC  =
TITLE   = 1/-2/~ @ HGHTS, RH AND OMEGA|~@ HGHT, RH AND OMEGA!0
ru

GLEVEL  = 500
GVCORD  = PRES
SKIP    = 0                  !0       !0                  !0        !0
SCALE   = 5                  !5       !5                  !5        !-1
GDPFUN  = avor(wnd)//v       !v       !mul(v,-1)          !mul(v,-1)!sm5s(hght)
TYPE    = c/f                !c       !c/f                !c        !c
CINT    = 2/10/99            !2/6/8   !2/10/99            !2/4/8    !6
LINE    = 7/5/1/2            !29/5/1/2!7/5/1/2            !29/5/1/2 !20/1/2/1
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!        !2;6/X;N/10-99;10-99!         !
HLSYM   = 
CLRBAR  = 1
WIND    = bk0
REFVEC  =
TITLE   = 1/-2/~ ${MDL} @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
ru

GAREA   = -52.3;-100.0;7.3;-26.2
PROJ    = MER/0.0;-65.0;0.0
GLEVEL	= 9950!400:250!400:250!0:9950!0:9950!0
GVCORD	= sgma!pres!pres!none!none!none
SKIP	= 0
SCALE	= 0!6!6!0!0!0
GDPFUN	= thte(pres@0%none;tmpc;dwpc)//t!sm5s(mul(-1;pvor(thta,wnd)))//p!p!t!t!pmsl!kntv(wnd@300%pres)
TYPE	= c/f                           !c/f                     !c    !c!c!c   !b
CINT	= 4/332/380                     !1/2                     !2/6  !4/332/380!4/308/328!4//1012
LINE	= 21/1/2                        !32/1/2/0                !6/1/2!21/1/2!21/10/3!20//3
FINT	= 332;348;364!3;4;5
FLINE	= 0;23;22;14!0;24;30;13
HILO	= 0!0!0!0!0!26;2/H#;L#/1020-1090;930-1012/2//y
HLSYM	= 0!0!0!0!0!1.3;1.3//22;22/3;3/hw
CLRBAR	= 0!1/V/LL!0!0
WIND	= bk0!bk0!bk0!bk0!bk0!bk0!bk9/0.8/2/112
REFVEC	=
TITLE	= 1/-2/~ ${MDL} 400-250mb PV,300mb WIND,PMSL,BL THTAE|~400-250mb PV!0
r

glevel   = 300
gvcord   = pres
SKIP     = 0/2;2
scale    = 0                       !5/0               !5/0    !-1
gdpfun   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)
type     = c/f                     !c                 !c      !c 
cint     = 70;90;110;130;150;170   !-11;-9;-7;-5;-3;-1!2/2/100!12
line     = 32/1                    !20/-2/2           !3/1/2  !1//2
fint     = 70;90;110;130;150;170;190!
fline    = 0;24;25;30;28;14;2;1    !
hilo     = 0                       !0                 !0      !1/H#;L#/3
hlsym    = 0                       !0                 !0      !1.3//22/2/hw
clrbar   = 1/V/LL                  !0
wind     = bk0                     !bk0               !bk0    !bk0
refvec   = 10
title    = 1/-1/~ ${MDL} @ DIV(GREEN),ISOTACHS & AGEO WND|~@ AGEO & DIVERG!0
filter   = no
r

GLEVEL   = 250
GDPFUN   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)
r

glevel   = 200
gdpfun   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)
r

glevel   = 0!0!0!0!700:500         !4400:10000
gvcord   = none!none!none!none!PRES!sgma
scale    = 2!2!2!2!3!0
gdpfun   = sm5s(WXTr06)!sm5s(WXTs06)!sm5s(WXTp06)!sm5s(WXTz06)!sm5s(lav(omeg))!sm5s(relh)
refvec   =
type     = c/f!c/f!c/f!c/f!c!c
cint     = 50;200!50;200!50;200!50;200!-1;-3;-5;-7;-9;-11;-13;-15;-17;-19;-21!5/70
line     = 22/1/2/0!4/1/2/0!7/1/2/0!2/1/2/0!6/1/3!21/1/3
fint     = 50;200!50;200!50;200!50;200
fline    = 0;23;23!0;25;25!0;30;30!0;15;15
clrbar   =
title    = 1/0/~ ${MDL} PCPN TYPE, 1000-500 RH & 7-500 VV|~PCPN TYPE & VV!0
hilo     =
hlsym    =
r

glevel   = 0           !0           !0           !0
gvcord   = none        !none        !none        !none
scale    = 2           !2           !2           !2
gdpfun   = sm5s(WXTr06)!sm5s(WXTs06)!sm5s(WXTp06)!sm5s(WXTz06)
refvec   =
type     = c/f         !c/f         !c/f         !c/f
cint     = 50;200      !50;200      !50;200      !50;200
line     = 22/1/2/0    !4/1/2/0     !7/1/2/0     !2/1/2/0
fint     = 50;200      !50;200      !50;200      !50;200
fline    = 0;23;23     !0;25;25     !0;30;30     !0;15;15
clrbar   =
title    = 1/0/~ ${MDL} PCPN TYPE|~PCPN TYPE!0
r

GAREA   = -52;-120;20;-10
PROJ    = mer//3;3;0;1
MAP	= 1/1/1/yes
LATLON	= 1//1/1/10

GLEVEL  = 200
GVCORD  = PRES
SKIP    = 0
SCALE   = 0                         !5/0
GDPFUN  = knts(mag(wnd))//wnd       !sm5s(div(wnd)!kntv(wnd)
TYPE    = c/f                       !c            !b
CINT    = 20/70//                   !2/2
LINE    = 32/1/2/1                  !5/2/2/2
FINT    = 80;90;110;130;150;170;190 !1;2;3;4;5;6;7
FLINE   = 0;25;24;29;7;15;20;14     !0;23;22;21;17;16;2;1
HILO    = 
HLSYM   = 
CLRBAR  = 1/v/ll                    !0
WIND    = bk0                       !am16/0.3//211/0.4!Bk9/0.75/2
REFVEC  = 
TITLE   = 1/-2/~ @ ISOTACHS AND WIND (KTS)|~200 MB WIND!0
FILTER  = yes
ru
 
GAREA   = -66;-127;14.5;-19
LATLON	= 1//1/1/10

GDATTIM	= F12-${fend}-06
GLEVEL	= 0
GVCORD	= none
SKIP	= 0
SCALE	= 0
GDPFUN	= p12m
TYPE	= c/f
CINT	= 0
LINE	= 0
FINT	= 1;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80;85
FLINE	= 0;21-30;14-20;5
HILO	= 31;0/x#/10-500///y
HLSYM	= 1.5
CLRBAR	= 1/V/LL
WIND	= 
REFVEC	=
TITLE	= 1/-2/~ ${MDL} 12-HR TOTAL PCPN|~12-HR TOTAL PCPN!0
r

GDATTIM	= F24-${fend}-06
GDPFUN	= p24m
TITLE	= 1/-2/~ ${MDL} 24-HR TOTAL PCPN|~24-HR TOTAL PCPN!0
r

exit
EOF

export err=$?;err_chk
#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l $metaname
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_${metatype}
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
      ${COMOUT}/${mdl}_${PDY}_${cyc}_${metatype}
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        ${COMOUT}/${mdl}_${PDY}_${cyc}_${metatype}
      fi
   fi
fi

exit

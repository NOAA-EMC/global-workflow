#! /bin/sh
#
# Metafile Script : gfs_meta_mar_atl.sh
#
# Log :
# J. Carr/PMB    12/08/2004    Pushed into production.
#
# Set up Local Variables
#
set -x
#
export PS4='MAR_ATL:$SECONDS + '
mkdir -p -m 775 $DATA/MAR_ATL
cd $DATA/MAR_ATL
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
metatype="mar_atl"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`

export pgm=gdplot2_nc;. prep_step; startmsg

$GEMEXE/gdplot2_nc << EOFplt
\$MAPFIL=mepowo.gsf+mehsuo.ncp+mereuo.ncp+mefbao.ncp
gdfile     = F-${MDL} | ${PDY2}/${cyc}00
gdattim    = f00-f180-6
GAREA      = 16;-100;71;5
PROJ       = mer//3;3;0;1
MAP	   = 31//2 + 6 + 3 + 5
LATLON	   = 18/2///10
CONTUR	   = 0
clear      = y

device     = $device 

GLEVEL	= 850:1000                  !0
GVCORD	= pres                      !none
PANEL	= 0
SCALE	= -1                        ! 0
GDPFUN	= (sub(hght@850,hght@1000)) !sm5s(pmsl) ! kntv(wnd@9950%sgma)
TYPE	= c                         !c          ! b
CINT	= 1                         ! 4
LINE	= 3/5/1/2                   ! 20//2
FINT	=
FLINE	=
HILO	= ! 26;2/H#;L#/1020-1070;900-1012//30;30/y
HLSYM	= 2;1.5//21//hw
CLRBAR	= 1
WIND	= bk9/0.7/2/112
TITLE = 5/-2/~ ? ${MDL} MSLP, 1000-850mb THK & BL (~40m) WIND|~ MSLP,1000-850 THK!0
CLEAR	= yes
li
r

glevel     = 9950!9950!9950!0
gvcord     = sgma!sgma!sgma!none
panel      = 0
skip       = 0!0/2/2!0
scale      = 0
GDPFUN     = tmpc!tmpc!tmpc!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE       = c/f !c   !c   !c         !b
cint       = 3/-99/0!3/3/18!3/21/99!4
line       = 27/1/1/1!2/1/1/1!16/1/1/1!19//2
fint       = -24;-12;0 !
fline      = 29;30;24;0 !
hilo       = 0!0!0!20/H#;L#/1020-1070;900-1012
hlsym      = 0!0!0!1;1//22;22/3;3/hw
clrbar     = 1/V/LL!0
wind       = bk9/0.8/1/112! 
refvec     = 
title      = 1/-2/~ ? |~ PMSL, BL TEMP, WIND!1//${MDL} MSL PRES,BL TEMP,WIND (KTS)!0
text       = 1.2/22/2/hw
clear      = y
li
r

GLEVEL	= 9950!9950!0:9950!0:9950!0
GVCORD	= sgma!sgma!none!none!none
PANEL	= 0
SKIP	= 0/2
SCALE	= 7!0
GDPFUN  = sdiv(mixr(dwpc;pres@0%none),obs!thte(pres@0%none,tmpc,dwpc)//e!e!e!pmsl!kntv(wnd%sgma@9950)
TYPE	= c/f                            !c                             !c!c!c   !b
CONTUR	= 0
CINT	= 1//-1!4//296!4/300/320!4/324!1;2
LINE	= 32/1/1/2!30/1/1/0!29/1/1/0!7/1/1/1!1
FINT	= -9;-7;-5;-3;-1
FLINE	= 2;15;21;22;23;0
HILO	= !!!!6/H#;L#/1020-1070;900-1012
HLSYM	= 1;1/2//4;1.5/0
CLRBAR	= 1
WIND	= bk10/0.75/1.5/112
REFVEC	=
title	= 1/-2/~ ?|~ BL MOISTURE CONV!1//${MDL} 40m AGL MOISTURE CONV, WIND, THTAE!0
TEXT	= 1
CLEAR	= y
li
r

GLEVEL  = 500
GVCORD  = PRES
PANEL   = 0
SKIP    = 0                  !1
SCALE   = 5                  !-1
GDPFUN   = (avor(wnd))        !hght
TYPE   = c/f                !c
CONTUR  = 0
CINT    = 3/3/99             !6/444
LINE    = 7/5/1/2            !20/1/2/1
FINT    = 15;21;27;33;39;45;51;57
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!          !
HLSYM   = 
CLRBAR  = 1
WIND    =  
REFVEC  =
TITLE   = 5//~ ? ${MDL} @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!0
TEXT    = 1/21//hw
CLEAR   = yes
li
ru

GLEVEL	= 9950!500:300!500:300!0:9950!0:9950!0:9950!400!0
GVCORD	= sgma!pres!pres!none!none!none!pres!none
PANEL	= 0
SKIP	= 0/2/2
SCALE	= 0!6!6!0!0!0!-1!0
GDPFUN	= thte(pres@0%none;tmpc;dwpc)//e!pvor(thta,wnd)//p!p!e!e!e!hght!pmsl
TYPE	= c/f!c/f!c
CONTUR	= 0
CINT	= 4/332!1.5;2;2.5;3;3.5;4!.5;1!4/332!4/308/328!4/292/304!12!8//1016
LINE	= 21/1/2!32/1/2/0!6/1/2!21/1/2!21/10/2!23/10/2!15/1/2/0!20//3
FINT	= 332;348;364!1;2;3
FLINE	= 0;23;22;21!0;24;30;13
HILO	= 0!0!0!0!0!0!0!20/H#;L#/1020-1070;900-1012/3
HLSYM	= 0!0!0!0!0!0!0!1;1//22;22/3;3/hw
CLRBAR	= 0!1/V/LL!0!0
WIND	= !!!bk16/.4!
REFVEC	=
TITLE	= 1/-2/~ ?|~ 500-300mb PV !1//${MDL} 500-300mb PV,400mb HGHT,PMSL,BL THTAE!0
TEXT	= 1
CLEAR	= yes
li
r

glevel     = 300!300!300 
gvcord     = pres!pres!pres 
panel      = 0
skip       = 1!1!1/3/3!1
scale      = 0!0!5!5!-1
GDPFUN      = mag(kntv(wnd))//jet!jet!div(wnd)//dvg!dvg!sm5s(hght)
TYPE      = c!c/f!c/f!c!c
cint       = 30;50!70;90;110;130;150;170;190!-11;-9;-7;-5;-3!2/3/18!12/720
line       = 26!32//2!19/-2//2!20!1//2 
fint       = !70;90;110;130;150;170;190!3;5;7;9;11;13!  
fline      = !0;24;25;29;7;15;14;2!0;23;22;21;17;16;2!
hilo       = 0!0!0!0!1/H;L/3
hlsym      = 0!0!0!0!1.5;1.5//22;22/2;2/hw
clrbar     = 0!0!1/V/LL!0
wind       = !!am16/0.3//211/0.4! 
refvec     = 10 
title      = 1/-2/~ ?|~ @ SPEED & DIVERG!1//${MDL} @ HGHTS, ISOTACHS, & DIVERGENCE!0
text       = 1.2/22/2/hw
clear      = y
li
r

!
! West Atlantic stuff
! 1000-850 thk & precip fields
!
GAREA   = 13;-84;50;-38
PROJ    = str/90;-67;1
LATLON  = 18/2///5;5
GDATTIM = f06-f180-06
CLEAR   = y
GLEVEL  = 0!0!850:1000!0!0
GVCORD  = none!none!pres!none!none
SCALE   = 0!0 !-1 !0!0
SKIP    = 0/1
GDPFUN  = p06i!p06i!sub(hght@850,hght@1000)!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE    = c/f !c   !c                      !c         !b
CINT    = 0.1;0.25!0.25/0.5!1!4!
LINE    = 32//1/0!32//1/0!16/15/2/1!20//2!
FINT    = .1;.25;.5;.75;1;1.25;1.5;1.75;2;2.25;2.5;2.75;3;3.25;3.5;3.75;4
FLINE   = 0;22-30;14-20;5
HILO    = 31;0/x#2/.01-10///y!31;0/x#2/.01-10///y !0!26;2/H#;L#///30;30/y !0
HLSYM   = 0!0!2;1.5//21//hw!0 !0
WIND    = bk9/0.7/1/112
TITLE   = 5/-2/~ ? ${MDL} MSLP,1000-850 THK,BL WND,6HR PCP|^ WAtl PCP,MSLP,THK!0
TEXT    = 1.2/22/2/hw
CLRBAR	= 1
li
run

GDPFUN   = c06i!c06i!sub(hght@850,hght@1000)!sm5s(pmsl)!kntv(wnd@9950%sgma)
TITLE   = 5/-2/~ ? ${MDL} MSLP,1000-850 THK,BL WND,6HR CONV PCP|^ WAtl CONV PCP,MSLP,THK!0
li
run

! Do 850 & 925 stability progs

GLEVEL  = 850!850!850:0!0
GVCORD  = pres!pres!pres!none
PANEL   = 0
SCALE   = 0
SKIP    = 0/1
GDPFUN  = mag(kntv(wnd))!sm9s(sub(thta,thta@0%none))//stb!stb!pmsl!kntv(wnd@850%pres)
TYPE    = c/f           !c                               !c  !c   !b
CONTUR  = 0
CINT    = 5/20!1//0!1/1!4//1012
LINE    = 32/1/2/2!7/1/2!6/10/3!19//2
FINT    = 20;35;50;65
FLINE   = 0;24;25;30;15
HILO    = 0!7/;N#/10-20;!0!20/H#;L#/1018-1070;900-1014
HLSYM   = 0!1.2;1.2//21;21/2;2/hw!0!1.2;1.2//21;21/2;2/hw
CLRBAR  = 1/V/LL!0
WIND    = bk9/0.8/1.8/112
REFVEC  =
TITLE   = 1/-2/~ ? ${MDL} PMSL, THETA (850-sfc), @ WND (kts)|~ WATL 850 STBLTY!0
TEXT    = 1/22/2/hw
CLEAR   = YES
l
r

GLEVEL  = 925!925!925:0!0
GDPFUN  = mag(kntv(wnd))!sm9s(sub(thta,thta@0%none))//stb!stb!pmsl!kntv(wnd@925%pres)
TITLE   = 1/-2/~ ? ${MDL} PMSL, THETA (925-sfc), @ WND (kts)|~ WATL 925 STBLTY!0
l
r

! RH & Omega (vv) - maybe swap 700mb hght for 9950 TMPF??

GLEVEL  = 700
GVCORD  = PRES
GDATTIM = f00-f180-6
SKIP    = 0! 1! 0! 0
SCALE   = 0 !-1! 3! 3
GDPFUN   = (relh)!(hght)!(omeg) !(omeg)
TYPE   = c/f! c
CONTUR  = 0
CINT    = 50;70;90!3!-9;-7;-5;-3!3;5;7;9
LINE    = 21//2/0!20/1/2/1 !6/1/1/1!16/5/1/1
FINT    = 70;90
FLINE   = 0;22;23
HILO    =
HLSYM   =
WIND    = ! ! Bk18//1
REFVEC  =
TITLE   = 1/-2/~ ? ${MDL} @ HEIGHTS, RH and OMEGA |~ WAtl @ HGHT, RH & OMEGA!0
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
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_atl
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_atl
   fi
fi


exit

#! /bin/sh
#
# Metafile Script : gfs_meta_crb_new
#
# Log :
# J.Carr/HPC         03/13/2001   New script for the Caribbean desk.
# J. Carr/HPC            5/2001   Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC            6/2001   Converted to a korn shell prior to delivering script to Production.
# J. Carr/HPC            7/2001   Submitted.
# J. Carr/PMB        11/15/2004   Added a ? to all title/TITLE lines. Changed contur parameter to 2.
#                                 Changed 12-hr increments to 6-hr with regards to 12-hr and 24-hr pcpn.
#
# Set Up Local Variables
#
set -x
#
export PS4='crb:$SECONDS + '
mkdir -p -m 775 $DATA/crb
cd $DATA/crb
cp $FIXgempak/datatype.tbl datatype.tbl
#
mdl=gfs
MDL=GFS
metatype="crb"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo ${PDY} | cut -c3-`
#
# DEFINE YESTERDAY
PDYm1=`$NDATE -24 ${PDY}${cyc} | cut -c -8`
PDY2m1=`echo ${PDYm1} | cut -c 3-`
#
#if [ ${cyc} -eq 00 ] ; then
#    fend=F126
#elif [ ${cyc} -eq 12 ] ; then
#    fend=F126
#else
#    fend=F126
#    fend=F84
#fi

fend=F126

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
GDFILE	= F-${MDL} | ${PDY2}/${cyc}00
GDATTIM	= F00-${fend}-06 
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
MAP	= 1//2
CLEAR	= yes
CLRBAR  = 1
PROJ    = mer//3;3;0;1
GAREA   = -10;-115;35;-53
LATLON	= 1//1/1/10
filter  = y

GLEVEL  = 500:1000               !500:1000               !0
GVCORD  = pres                   !pres                   !none
SKIP    = 0                      !0                      !0/1;1
SCALE   = -1                     !-1                     !0
GDPFUN  = sm5s(ldf(hght)         !sm5s(ldf(hght)         !sm5s(pmsl)!kntv(wnd@850%pres)
TYPE    = c                      !c                      !c   !b
CINT    = 3/400/540              !3/543/600              !2
LINE    = 6/3/2                  !2/3/2                  !20//3
FINT    =
FLINE   =
HILO    = !!26;2/H#;L#/1020-1070;900-1012//30;30/y
HLSYM   = !!2;1.5//21//hw
CLRBAR  = 1
WIND    =                        !                       !bk9/0.7/2/112
REFVEC  = 
TITLE	= 5/-2/~ ? ${MDL} MSLP, 1000-500mb THICK & 850mb WIND|~MSLP, 1000-500 THKN!
ru

glevel  = 9950
gvcord  = sgma
scale   = 7                         !0 
gdpfun  = sm5s(sdiv(mixr@0%none;wnd)!kntv(wnd)
type    = f                         !b
cint    = 0                         !
line    = 32                        !
clrbar  = 1/V/LL!
fint    = -16;-14;-12;-10;-8;-6;-4;-2
fline   =  16; 17; 18; 19;20;21;22;23;0
hilo    = 0
hlsym   = 0
wind    = am0!bk9/0.8/2/112
refvec  =
title   = 1/-2/~ ? ${MDL} BL MOIST CONV & WIND|~BL MOISTURE CONV!0
r
 
glevel  = 0         !9950
gvcord  = none      !sgma
scale   = 0
skip    = 0/1
gdpfun  = sm5s(thte)!kntv(wnd)
type    = c/f       !b
cint    = 4/200/336 
line 	= 5/1/1 
fint    = 336;340;344;348;352;356;360;364;368;372;376
fline   = 0 ; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 14
hilo    = 
hlsym   = 
clrbar  = 1/V/LL!0
wind    = bk0        !bk9/0.9/2/112
refvec  = 
title   = 1/-2/~ ? ${MDL} BL THTE & WIND (KTS)|~BL THTE & WIND
r

GLEVEL	= 850
GVCORD  = pres
SKIP    = 0/1;2
GDPFUN	= vor(wnd)              !vor(wnd)!kntv(wnd)
CINT	= 2/-99/-2              !2/2/99
LINE	= 29/5/1/2              !7/5/1/2
HILO	= 2;6/X;N/-99--4;4-99   !                   
SCALE	= 5                     !5 
WIND    = !!bk6/.8/2/112!0 
TITLE	= 1//~ ? ${MDL} @ WIND AND REL VORT|~@ WIND AND REL VORT!0
FINT    = 4;6;8;10;12;14;16;18
FLINE	= 0;14-21
TYPE	= c/f!c!b
r

GLEVEL  = 300:850!850!300
GVCORD  = pres
SCALE   = 0
SKIP    = 0/2;2
GDPFUN  = mag(vldf(wnd))!kntv(wnd)!kntv(wnd)
TYPE    = c/f           !a        !a
CINT    = 5/20
LINE    = 26//1
FINT    = 5/20
FLINE   = 0;24;30;29;23;22;14;15;16;17;20;5
HILO    = 
HLSYM   =
CLRBAR  = 1
WIND    = bk0!ak7/.3/1/221/.4!ak6/.3/1/221/.4
REFVEC  = 0
TITLE   = 1/-2/~ ? ${MDL} @ WIND SHEAR (KNTS)|~850MB-300MB WIND SHEAR!0
filter  = no
 

GLEVEL	= 700
GVCORD  = pres
GDPFUN	= vor(wnd)              !vor(wnd)!kntv(wnd)
CINT	= 2/-99/-2              !2/2/99
LINE	= 29/5/1/2              !7/5/1/2
HILO	= 2;6/X;N/-99--4;4-99   !                   
SCALE	= 5                     !5 
WIND    = !!bk6/.8/2/112!0 
TITLE	= 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~@ WIND AND REL VORT!0
FINT    = 6;8;10;12;14;16;18;20
FLINE	= 0;14-21
TYPE	= c/f!c!b
filter  = yes
r

GLEVEL  = 500
SCALE   = 5              !5       !5         !5        !-1
GDPFUN  = avor(wnd)//v   !v       !mul(v,-1) !mul(v,-1)!sm5s(hght)!kntv(wnd)
TYPE    = c/f            !c       !c/f       !c        !c         !b
CINT    = 2/10/99        !2/4/8   !2/10/99   !2/4/8    !2
LINE    = 7/5/1/2        !29/5/1/2!7/5/1/2   !29/5/1/2 !20/1/2/1
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!        !2;6/X;N/10-99;10-99!       !
HLSYM   = 
WIND    = bk0            !bk0     !bk0       !bk0      !bk0       !bk9/0.7/2/112!0 
TITLE   = 1/-2/~ ? ${MDL} @ HEIGHT AND VORTICITY|~@ HGT AND VORTICITY!0
ru

GLEVEL  = 250
GVCORD  = PRES
GDPFUN  = avor(wnd)//v !v       !mul(v,-1) !mul(v,-1)!hght!kntv(wnd)
ru

GLEVEL	= 250
GVCORD	= PRES
SKIP	= 0/2
SCALE	= 0                !0          !-1
GDPFUN	= mag(kntv(wnd))   !kntv(wnd)  !sm5s(hght)
TYPE	= c/f              !b          !c
CINT	= 50;70;90;110;130 !           !2/720
LINE	= 32/1/2/1         !27/5/2/1   !5/1/2/1
FINT	= 50;70;90;110;130;150;170
FLINE	= 0;25;24;29;7;15;20;14
HILO	=
HLSYM	=
CLRBAR	= 1
WIND	= !Bk9//2
REFVEC	=
TITLE	= 5/-2/~ ? ${MDL} @ HGHT, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
ru

GDATTIM	= F12-${fend}-06
GLEVEL	= 0
GVCORD	= none
SKIP	= 0
SCALE	= 0
GDPFUN	= p12m
TYPE	= c/f
CINT	= 1;5;10
LINE	= 32//1/0
FINT	= 1;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80;85
FLINE	= 0;21-30;14-20;5
HILO	= 31;0/x#/10-400///y
HLSYM	= 1.5
CLRBAR	= 1/V/LL
WIND	= 
REFVEC	=
TITLE	= 1/-2/~ ? ${MDL} 12-HR TOTAL PCPN|~12-HR TOTAL PCPN
r

GDATTIM	= F24-${fend}-06
GDPFUN	= p24m
TITLE	= 1/-2/~ ? ${MDL} 24-HR TOTAL PCPN|~24-HR TOTAL PCPN
r

GDATTIM = F00-${fend}-06
glevel  = 0!0
gvcord  = none!none
scale   = 0!0
gdpfun  = sm5s(pwtr)!sm5s(pwtr)!kntv(wnd@850%PRES)
type    = c   !c/f !b
cint    = 6/6/18!6/24
line    = 22///2!32//2/2
fint    = !13;25;38;50
fline   = !0;23;22;21;2       
hilo    = 0!0
HLSYM   = 0!0
clrbar  = 0!1
wind    = bk0!bk0!bk9/0.7/2/212
refvec  =
title   = 1/-2/~ ? ${MDL} 850 MB WIND & PW|~850 WND & PW!0
r

GLEVEL  = 4400:10000
GVCORD  = SGMA
SCALE   = 0
GDPFUN  = sm5s(relh)  !sm5s(relh)
TYPE    = c/f         !c
CINT    = 10;20;80;90 !30;40;50;60;70
LINE    = 32//2       !23//2
FINT    = 10;30;70;90
FLINE   = 18;8;0;22;23
HILO    = 
HLSYM   =
CLRBAR  = 1
WIND    = 
REFVEC  = 
TITLE	= 1/-2/~ ? ${MDL} @ LYR RH|~MEAN RH!0
ru

exit
EOF
export err=$?;err_chk


if [ ${cyc} -eq 00 ] ; then
    # BV export MODEL=/com/nawips/prod
    # JY export HPCECMWF=${MODEL}/ecmwf.${PDY}
    # JY export HPCUKMET=${MODEL}/ukmet.${PDYm1}
    export HPCECMWF=${COMINecmwf}.${PDY}
    export HPCUKMET=${COMINukmet}.${PDYm1}
    grid1="F-${MDL} | ${PDY2}/${cyc}00"
    grid2="${COMINecmwf}.${PDYm1}/ecmwf_glob_${PDYm1}12"
    grid3="F-UKMETHPC | ${PDY2m1}/1200"
    for gfsfhr in 12 36 60 84 108
    do
        ecmwffhr="F`expr ${gfsfhr} + 12`"
        gfsfhr="F${gfsfhr}"

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF10
GDFILE  = ${grid1} !${grid2}
GDATTIM = ${gfsfhr}!${ecmwffhr}
DEVICE  = ${device}
PANEL   = 0
TEXT    = 1/21//hw
MAP     = 6/1/1/yes
CLEAR   = yes
CLRBAR  = 1
PROJ    = mer//3;3;0;1 
GAREA   = -25;-130;40;-15 
LATLON  = 18//1/1/10

GLEVEL  = 500
GVCORD  = PRES
PANEL   = 0
SKIP    = 0
SCALE   = -1
GDPFUN  = sm5s(hght)!sm5s(hght)
TYPE    = c
CINT    = 6
FINT    =
FLINE   =
HLSYM   =
WIND    =
REFVEC  =
LINE    = 31//2!2//2
HILO    = 31/H#;L#//5/5;5/y!2/H#;L#//5/5;5/y
TITLE   = 31/-1/~ ? GFS @ HGHT (WHITE)|~EC VS GFS 500!2/-2/~ ? ECMWF 500 HGHT (RED)
r

GLEVEL  = 0
GVCORD  = none
PANEL   = 0
SKIP    = 0
SCALE   = 0
GDPFUN  = sm5s(pmsl)!sm5s(pmsl)
TYPE    = c
CINT    = 4
FINT    =
FLINE   =
HLSYM   = 1.5;1.5//21//hw
CLRBAR  = 1
WIND    =
REFVEC  =
TEXT    = 1/21//hw
CLEAR   = yes
GDFILE  = ${grid1}!${grid2}
GDATTIM = ${gfsfhr}!${ecmwffhr}
LINE    = 31//2!2//2
HILO    = 31/H#;L#/1020-1060;900-1010/5/10;10!2/H#;L#/1020-1060;900-1010/5/10;10
TITLE   = 31/-1/~ ? GFS PMSL (WHITE)|~EC VS GFS PMSL!2/-2/~ ? ECMWF PMSL (RED)
r

ex
EOF10
export err=$?;err_chk

    done
    for gfsfhr in 00 12 24 36 48 60 84 108 132
    do
        ukmetfhr="F`expr ${gfsfhr} + 12`"
        gfsfhr=F${gfsfhr}

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF25
DEVICE  = ${device}
PANEL   = 0
TEXT    = 1/21//hw
MAP     = 6/1/1/yes
CLEAR   = yes
CLRBAR  =
GLEVEL  = 500
GVCORD  = PRES
PANEL   = 0
SKIP    = 0
SCALE   = -1
GDPFUN  = sm5s(hght)!sm5s(hght)
TYPE    = c
CINT    = 6
FINT    =
FLINE   =
HLSYM   =
GVECT   =
WIND    =
REFVEC  =
clear   = yes
GDFILE  = ${grid1}!${grid3}
GDATTIM = ${gfsfhr}!${ukmetfhr}
LINE    = 31//2!2//2
HILO    = 31/H#;L#//5/7;7/y!2/H#;L#//5/5;5/y
TITLE   = 31/-1/~ ? GFS @ HGHT (WHITE)|~UK VS GFS 500!2/-2/~ ? UKMET 500 HGHT (RED)
r

GLEVEL  = 0
GVCORD  = none
PANEL   = 0
SKIP    = 0
SCALE   = 0
GDPFUN  = sm5s(pmsl)!sm5s(pmsl)
TYPE    = c
CINT    = 4
FINT    =
FLINE   =
HLSYM   = 1.5;1.5//21//hw
CLRBAR  =
WIND    =
REFVEC  =
TEXT    = 1/21//hw
CLEAR   = yes
GDFILE  = ${grid1}!${grid3}
GDATTIM = ${gfsfhr}!${ukmetfhr}
LINE    = 31//2!2//2
HILO    = 31/H#;L#/1020-1060;900-1010/5/10;10!2/H#;L#/1020-1060;900-1010/5/10;10
TITLE   = 31/-1/~ ? GFS PMSL (WHITE)|~UK VS GFS PMSL!2/-2/~ ? UKMET PMSL (RED)
r

ex
EOF25
export err=$?;err_chk

    done
fi

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

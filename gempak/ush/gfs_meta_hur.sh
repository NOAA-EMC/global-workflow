#! /bin/sh
#
# Metafile Script : gfs_meta_hur_new
#
# Log :
# D.W. Plummer/NCEP   2/97   Add log header
# J.W. Carr/HPC    4/15/97   changed the skip parameter
# J.W. Carr/HPC    4/06/98   Converted from gdplot to gdplot2
# J.L. Partain/MPC 5/25/98   Chg VOR to AVOR @ 500mb,chg 200 to 250mb to match ETA,NGM
# J.W. Carr/HPC    8/05/98   Changed map to medium resolution
# J.W. Carr/HPC    2/02/99   Changed skip to 0
# J.W. Carr/HPC    4/12/99   Added 84-hr time step.
# J. Carr/HPC         6/99   Added a filter to map.
# J. Carr/HPC       2/2001   Edited to run on the IBM.
# J. Carr/HPC       5/2001   Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC       6/2001   Incorporated the crb metafile into this one.
# J. Carr/HPC       6/2001   Converted to a korn shell prior to delivering script to Production.
# J. Carr/HPC       7/2001   Submitted.
# J. Carr/PMB      11/2004   Added a ? to all title lines. Changed contur to a 2 from a 1.
#
# Set up Local Variables
#
set -x
#
export PS4='hur:$SECONDS + '
mkdir  -p -m 775 $DATA/hur
cd $DATA/hur
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL=GFS
metatype="hur"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo ${PDY} | cut -c3-`
#
# DEFINE YESTERDAY
PDYm1=`$NDATE -24 ${PDY}${cyc} | cut -c -8`
PDY2m1=`echo ${PDYm1} | cut -c 3-`
#
if [ ${cyc} -eq 00 ] ; then
    gdat="F000-F126-06"
    gdatpcpn06="F006-F126-06"
    gdatpcpn12="F012-F126-06"
    gdatpcpn24="F024-F126-06"
    gdatpcpn48="F048-F126-06"
    gdatpcpn60="F060-F126-06"
    gdatpcpn72="F072-F126-06"
    gdatpcpn84="F084-F126-06"
    gdatpcpn96="F096-F126-06"
    gdatpcpn120="F120-F126-06"
    gdatpcpn126="F126"
    run="r"
elif [ ${cyc} -eq 12 ] ; then
    gdat="F000-F126-06"
    gdatpcpn06="F006-F126-06"
    gdatpcpn12="F012-F126-06"
    gdatpcpn24="F024-F126-06"
    gdatpcpn48="F048-F126-06"
    gdatpcpn60="F060-F126-06"
    gdatpcpn72="F072-F126-06"
    gdatpcpn84="F084-F126-06"
    gdatpcpn96="F096-F126-06"
    gdatpcpn120="F120-F126-06"
    gdatpcpn126="F126"
    run="r"
else
    gdat="F000-F126-06"
    gdatpcpn06="F006-F126-06"
    gdatpcpn12="F012-F126-06"
    gdatpcpn24="F024-F126-06"
    gdatpcpn48="F048-F126-06"
    gdatpcpn60="F060-F126-06"
    gdatpcpn72="F072-F126-06"
    gdatpcpn84="F084-F126-06"
    gdatpcpn96="F096-F126-06"
    gdatpcpn120="F120-F126-06"
    gdatpcpn126="F126"
    run="r"
fi

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
gdfile  = F-${MDL} | ${PDY2}/${cyc}00
gdattim = ${gdat}
GAREA   = -6;-111;52;-14
PROJ    = MER/0.0;-49.5;0.0
MAP     = 1/1/1/yes
LATLON  = 1//1/1/10
CONTUR	= 2
clear   = y
device  = ${device} 
TEXT	= 1/22/2/hw
PANEL	= 0
filter  = yes

GLEVEL	= 9950!0
GVCORD	= sgma!none
SKIP	= 0
SCALE	= 0
GDPFUN	= mag(kntv(wnd))!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE	= c/f           !c         !b
CINT	= 5/20!2
LINE	= 32/1/2/2!19//2
FINT	= 20;35;50;65
FLINE	= 0;24;25;30;15
HILO	= 0!20/H#;L#/1020-1060;880-1012///1
HLSYM	= 0!1;1//22;22/3;3/hw
CLRBAR	= 1/V/LL!0
WIND	= bk0!bk0!bk9/.8/1.4/112
REFVEC	=
TITLE	= 1/-2/~ ? ${MDL} PMSL, BL WIND (40m AGL; KTS)|~PMSL & BL WIND!0
r

GLEVEL  = 500:1000               !500:1000               !0
GVCORD  = pres                   !pres                   !none
SKIP    = 0                      !0                      !0/1;1
SCALE   = -1                     !-1                     !0
GDPFUN  = sub(hght@500,hght@1000)!sub(hght@500,hght@1000)!sm5s(pmsl)!kntv(wnd@850%pres)
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
TITLE   = 5/-2/~ ? ${MDL} MSLP, 1000-500mb THICK & 850mb WIND|~MSLP,1000-500 TH,850 WND!
ru

glevel  = 0         !0
gvcord  = none      !none
scale   = 0
skip    = 0/1
gdpfun  = sm5s(thte)!kntv(wnd@9950%sgma)
type    = c/f       !b
cint    = 4/200/336
line    = 5/1/1
fint    = 336;340;344;348;352;356;360;364;368;372;376
fline   = 0 ; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 14
hilo    =
hlsym   =
clrbar  = 1/V/LL!0
wind    = bk0        !bk9/0.9/2/112
refvec  =
title   = 1/-2/~ ? ${MDL} BL THTE & WIND (KTS)|~BL THTE & WIND
r

glevel  = 0
gvcord  = none
scale   = 7!0
gdpfun  = sdiv(mixr;wnd%SGMA@9950)!kntv(wnd%sgma@9950)
type    = f                       !b
cint    = 0                       !
line    = 32                      !
clrbar  = 1/V/LL!
fint    = -16;-14;-12;-10;-8;-6;-4;-2
fline   =  16; 17; 18; 19;20;21;22;23;0
hilo    = 0
hlsym   = 0
wind    = am0!bk9/0.8/2/112
refvec  =
title   = 1/-2/~ ? ${MDL} 40m AGL MOIST CONV & WIND|~BL MOISTURE CONV!0
r

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
title   = 1/-2/~ ${MDL} 850 MB WIND & PW|~850 WND & PW!0
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
TITLE   = 1/-2/~ ? ${MDL} @ LYR RH|~MEAN RH!0
ru

GLEVEL	= 850                !850      !0         !850
GVCORD  = pres               !pres     !none      !pres
GDPFUN	= vor(wnd)           !vor(wnd) !sm5s(pmsl)!kntv(wnd)
TYPE	= c/f                !c        !c         !b
CINT	= 2/-99/-2           !2/2/99   !2//1008
LINE	= 29/5/1/2           !7/5/1/2  !6/1/1
HILO	= 2;6/X;N/-99--4;4-99!         !6/L#/880-1004///1         
HLSYM	= 1;1//22;22/3;3/hw
SCALE	= 5                  !5        !0
WIND    = bk0                !bk0      !bk0       !bk9/.8/1.4/112 
TITLE	= 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~@ WIND AND REL VORT!0
FINT    = 4;6;8;10;12;14;16;18
FLINE	= 0;14-21
r

GLEVEL	= 700!700!0!700
GVCORD  = pres!pres!none!pres
GDPFUN	= vor(wnd)           !vor(wnd) !sm5s(pmsl)!kntv(wnd)
CINT	= 2/-99/-2           !2/2/99   !2//1008
LINE	= 29/5/1/2           !7/5/1/2  !6//1
HILO	= 2;6/X;N/-99--4;4-99!         !6/L#/880-1004///1
HLSYM	= 1;1//22;22/3;3/hw
SCALE	= 5                  !5        !0
WIND    = bk0                !bk0      !bk0       !bk9/.8/1.4/112 
TITLE	= 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~@ WIND AND REL VORT!0
FINT    = 4;6;8;10;12;14;16;18
FLINE	= 0;14-21
TYPE	= c/f                !c        !c         !b
r

GLEVEL	= 500
GVCORD  = pres
SKIP    = 0                  !0         !0       !/1
GDPFUN	= avor(wnd)          !avor(wnd) !sm5s(hght)!kntv(wnd)
CINT	= 2/-99/-2           !2/2/99    !2
LINE	= 29/5/1/2           !7/5/1/2   !20/1/2/1
HILO	= 2;6/X;N/-99--4;4-99!          !
HLSYM	= 1;1//22;22/3;3/hw  !
SCALE	= 5                  !5         !-1
WIND    = bk0                !bk0       !bk0       !bk9/.8/1.4/112 
TITLE	= 1/-2/~ ? ${MDL} @ WIND AND ABS VORT|~@ WIND AND ABS VORT!0
FINT    = 16;20;24;28;32;36;40;44
FLINE	= 0;23-15
TYPE	= c/f                !c         !c         !b
r 

GLEVEL  = 250
GVCORD  = PRES
GDPFUN  = avor(wnd)          !avor(wnd) !sm5s(hght)!kntv(wnd)
ru

GLEVEL  = 300:850       !850      !300
GVCORD  = pres          !pres     !pres
SKIP    = 0             !0/3;3    !0/3;3
SCALE   = 0
GDPFUN  = mag(vldf(obs))!kntv(wnd)!kntv(wnd)
TYPE    = c/f           !a        !a
CINT    = 5/20
LINE    = 26//1
FINT    = 5/25
FLINE   = 0;24;30;29;23;22;14;15;16;17;20;5
HILO    = 
HLSYM   =
CLRBAR  = 1
WIND    = ak0!ak7/.1/1/221/.2!ak6/.1/1/221/.2
REFVEC  = 
TITLE   = 1/-2/~ ? ${MDL} @  WIND SHEAR (850=Purple, 300=Cyan) |~850-300MB WIND SHEAR!0
filter  = no
r 

glevel   = 250
gvcord   = pres
skip     = 0             !0       !0       !0/2;2
scale    = 0             !5       !5
gdpfun   = mag(kntv(wnd))!div(wnd)!div(wnd)!kntv(wnd)
type     = c/f           !c       !c       !b
cint     = 30;50;70;90;110;130;150;170;190!2/-13/-3!2/3/18
line     = 26/1/2        !19/2/2  !3/1/2
fint     = 50;70;90;110;130;150;170
fline    = 0;24;25;29;7;15;14;2
hilo     = 0
hlsym    = 0
clrbar   = 1/V/LL!0
wind     = bk0           !bk0      !bk0      !bk9/.8/1.3/112
refvec   = 10
title    = 1/-2/~ ? ${MDL} @ HGHTS, ISOTACHS, & DIVERG|~@ SPEED & DIVERG!0
r

glevel  = 400:850!0
gvcord  = pres!none
scale   = 0
gdpfun  = squo(2,vadd(vlav(wnd@850:700%pres,vlav(wnd@500:400%pres)!sm5s(pmsl)
type    = b                                                       !c
cint    = 0!4
line    = 0!20//3
SKIP    = 0/2;2
fint    = 
fline   = 
hilo    = 0!26;2/H#;L#/1020-1070;900-1012//30;30/y
hlsym   = 0!2;1.5//21//hw
clrbar  = 0
wind    = bk10/0.9/1.4/112!bk0
refvec  = 
title   = 1/-2/~ ? ${MDL} 850-400mb MLW and MSLP|~850-400mb MLW & MSLP!0
r

GDATTIM = ${gdatpcpn12}
GLEVEL  = 0
GVCORD  = none
SKIP    = 0
SCALE   = 0
GDPFUN  = p12m
TYPE    = c/f
CINT    = 1;5;10
LINE    = 32//1/0
FINT    = 1;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80;85
FLINE   = 0;21-30;14-20;5
HILO    = 31;0/x#/10-400///y
HLSYM   = 1.5
CLRBAR  = 1/V/LL
WIND    =
REFVEC  =
TITLE   = 1/-2/~ ? ${MDL} 12-HR TOTAL PCPN|~12-HR TOTAL PCPN
r

GDATTIM = ${gdatpcpn24}
GDPFUN  = p24m
TITLE   = 1/-2/~ ? ${MDL} 24-HR TOTAL PCPN|~24-HR TOTAL PCPN
r

exit
EOF
export err=$?;err_chk

if [ ${cyc} -eq 00 ] ; then
    # BV export MODEL=/com/nawips/prod
    # JY export HPCECMWF=${MODEL}/ecmwf.${PDY}
    # JY export HPCUKMET=${MODEL}/ukmet.${PDY}
    export HPCECMWF=${COMINecmwf}.${PDY}
    export HPCUKMET=${COMINukmet}.${PDY}
    grid1="F-${MDL} | ${PDY2}/${cyc}00"
    grid2="${COMINecmwf}.${PDYm1}/ecmwf_glob_${PDYm1}12"
    grid3="F-UKMETHPC | ${PDY2}/${cyc}00"
    for gfsfhr in 12 36 60 84 108
    do
        ecmwffhr="F`expr ${gfsfhr} + 12`"
        gfsfhr="F${gfsfhr}"

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
GDFILE  = ${grid1} !${grid2}
GDATTIM = ${gfsfhr}!${ecmwffhr}
DEVICE  = ${device}
PANEL   = 0
TEXT    = 1/21//hw
CONTUR  = 2
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
EOF
export err=$?;err_chk

    done
    for gfsfhr in 12 24 36 48 60 72 96 120
    do
        ukmetfhr=F${gfsfhr}
        gfsfhr=F${gfsfhr}

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOF
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
EOF
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

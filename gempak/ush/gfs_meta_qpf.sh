#! /bin/sh
#
# Metafile Script : gfs_meta_qpf.sh
#
# Log :
# D.W.Plummer/NCEP   2/97   Add log header
# J. Carr/HPC      7/7/97   Changed script so that it uses gdplot2 instead of gdplot
# J. Carr/HPC      8/5/98   Removed pcpn potential product and changed map to a medium resolution
# J. Carr/HPC      2/2/99   Changed skip to 0
# J. Carr/HPC     2/10/99   Changed type c/f to just f for pcpn
# J. Carr/HPC     4/12/99   Added 84-hr time for the gfs.
# J. Carr/HPC        6/99   Added a filter on map
# J. Carr/HPC      2/2001   Edited to run on IBM.
# J. Carr/HPC      5/2001   Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC      6/2001   Converted to a korn shell prior to delivering script to Production.
# J. Carr/HPC      7/2001   Submitted.
# J. Carr/HPC     11/2004   Changed contur from 1 to a 2.
#                           Inserted a ? in all title lines.
#                           Commented out if statement for cycles since this is old code based on when various runs of GFS ran
#                           out to differing times.
# M. Klein/HPC    02/2010   Run 48-hour QPF out to F216 for medium-range.
#
# Set up Local Variables
#
set -x
export PS4='qpf:$SECONDS + '
mkdir -p -m 775 $DATA/qpf
cd $DATA/qpf
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL=GFS
metatype="qpf"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`
    gdat="F000-F126-06"
    gdatpcpn06="F006-F126-06"
    gdatpcpn12="F012-F126-06"
    gdatpcpn24="F024-F126-06"
    gdatpcpn48="F048-F216-06"
    gdatpcpn60="F060-F126-06"
    gdatpcpn72="F072-F126-06"
    gdatpcpn84="F084-F126-06"
    gdatpcpn96="F096-F126-06"
    gdatpcpn120="F120-F126-06"
    gdatpcpn126="F126"
    run="r"

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc << EOFplt
gdfile   = F-${MDL} | ${PDY2}/${cyc}00
gdattim  = ${gdat}
garea    = us
proj     = 
map      = 1/1/2/yes
device   = ${device}
clear    = yes
text     = m/22/2/hw
panel    = 0
contur   = 2
latlon   = 0
filter   = yes

glevel   = 0
gvcord   = none 
skip     = 0
scale    = 0
gdpfun   = sm5s(lft4)!sm5s(lft4)  !sm5s(lft4)!kntv(wnd@9950%sgma)
type     = c/f       !c           !c         !b
cint     = 2/2       !-10000;0.05 !2/-100/-2
line     = 20/-32/2  !0;5//0;4/0;0!32//2  
fint     = -8;-6;-4;-2;0.05;10 
fline    = 2;15;21;22;23;0;24
hilo     = 0         !0
hlsym    = 1;1//22;22/2;2/hw!0
clrbar   = 1/V/LL    !0
wind     = bk0       !bk0          !bk0       !bk10/0.8/2/112!bk0 
refvec   = 
title    = 1/-2/~ ? ${MDL} Best LI AND BL WINDS|~BEST LI!0
r

glevel   = 0                      !0   !0         !500:1000      !500:1000      !850:700
gvcord   = none                   !none!none      !PRES          !PRES          !PRES
SKIP     = 0                      !0   !0         !0             !0             !0/2;2
scale    = 0                      !0   !0         !-1            !-1            !0
gdpfun   = sm5s(quo(pwtr;25.4)//pw!pw  !sm5s(pmsl)!sm5s(ldf(hght)!sm5s(ldf(hght)!kntv(vsub(squo(2,vadd(vlav(wnd,vlav(wnd@500:300)),wnd@850)
type     = c                      !c/f !c         !c             !c             !b
cint     = 0.25/0.25/0.5          !0.25/0.75/6.0!4!3/0/540!3/543/1000
line     = 22///2                 !32//2/2!6//3!4/5/2!5/5/2
fint     = 0                      !0.5;1.0;1.5;2.0
fline    = 0                      !0;23;22;30;14       
hilo     = 0                      !0!6/H#;L#/1020-1070;900-1012!0
HLSYM    = 0                      !0!1.5;1.5//22;22/3;3/hw!0
clrbar   = 0                      !1/V/LL!0!0
wind     = am0                    !am0!am0!am0!am0!bk9/0.8/2/112
refvec   =
title    = 1/-2/~ ? ${MDL} PW, MSLP, THICKNESS & C-VEC|~PW, PMSL, C-VEC!0
r

GLEVEL   = 9950      !9950      !0
GVCORD   = sgma      !sgma      !none
SKIP     = 0/1
SCALE    = 0
GDPFUN   = sm5s(dwpf)!sm5s(dwpf)!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE     = c         !c/f       !c         !b
CINT     = 4/32/48   !4/52      !4
LINE     = 3/1/2/1   !32/1/2/1  !5//3
FINT     = 50;56;62;68;74
FLINE    = 0;23;22;30;14;2
HILO     = 0         !0         !5/H#;L#/1020-1070;900-1012
HLSYM    =           !          !1.5;1.5//22;22/3;3/hw
CLRBAR   = 0         !1/V/LL    !0
WIND     = 0         !0         !0         !bk9/0.8/2/112
REFVEC   =
TITLE    = 1/-2/~ ? ${MDL} SFC DWPT, WIND AND MSLP|~SFC DEWPOINT!0
r

glevel	 = 9950
gvcord	 = sgma
skip     = 0
scale	 = 7!0
gdpfun	 = sm5s(sdiv(mixr(dwpc;pres@0%none);wnd)!sm5s(dwpc)!sm5s(dwpc)!sm5s(dwpc)!kntv(wnd)
type	 = f                                    !c    !c   !c   !b
cint	 = 0                                    !2    !2/12!2/20
line	 = 0                                    !19//1!5//1!6//2
fint	 = -11;-9;-7;-5;-3;-1
fline	 = 2;25;24;23;22; 3;0
hilo	 = 0
hlsym	 = 0
clrbar	 = 1/V/LL!0
wind	 = bk0                                  !bk0        !bk0      !bk0       !bk9/0.7/2/112
refvec	 =
title	 = 1/-2/~ ? ${MDL} BL MOIST FLUX CONV, WIND, DEW PT|~BL MOIST CONV!0
r

glevel   = 0!0
gvcord   = none!none
SKIP     = 0/1;1
scale    = 0!0
gdpfun   = sm5s(quo(pwtr;25.4)!sm5s(quo(pwtr;25.4)!kntv(wnd@850%PRES)
type     = c!c/f!b
cint     = 0.25/0.25/0.5!0.25/0.75/6.0
line     = 22///2!32//2/2
fint     = !0.5;1.0;1.5;2.0
fline    = !0;23;22;21;2       
hilo     = 0!0
HLSYM    = 0!0
clrbar   = 0!1/V/LL
wind     = bk0!bk0!bk9/0.8/2/212
refvec   =
title    = 1/-2/~ ? ${MDL} 850mb WIND & PRECIP WATER|~850mb WIND & PW!0
r

GLEVEL   = 0
GVCORD   = none
SKIP     = 0
SCALE    = 0
GDPFUN   = sm5s(quo(mul(quo(pwtr;25.4),relh@4400:10000%sgma),100))
TYPE     = c/f
CINT     = .2/.2
LINE     = 23/1/2/1
FINT     = .6;.8;1;1.2;1.4;1.6
FLINE    = 0;23;22;21;2;30;7
HILO     = 0
HLSYM    = 0
CLRBAR   = 1
WIND     = !
REFVEC   =
TITLE    = 1/-2/~ ? ${MDL} PCPN POTENTIAL (PW X (1000-440 MB RH)) INCHES OF PW|~PCPN POT!0
r
 
glevel   = 850!850!850
gvcord   = pres!pres!pres
skip     = 0/1;1
scale    = 0!0!-1
gdpfun   = sm5s(dwpc)!sm5s(dwpc)!sm5s(hght)!kntv(wnd)
type     = c/f !c   !c         !b
cint     = -4;-2;0;2;4!2/6/28!3
line     = 3//1!32//1!6//3
fint     = 4;8;12;16;20
fline    = 0;23;22;30;14;2
hilo     = 0!0!6/H#;L#  
hlsym    = 0!0!1.5;1.5//22;22/2;2/hw
clrbar   = 1/V/LL!0
wind     = bk0!bk0!bk0!bk9/0.8/2/212 
refvec   = 
title    = 1/-2/~ ? ${MDL} @ DEW POINT, WIND, AND HGHT|~@ DEW POINT!0
r

glevel   = 700!700!700
gvcord   = pres!pres!pres
scale    = 0!0!-1
gdpfun   = sm5s(dwpc)!sm5s(dwpc)!sm5s(hght)!kntv(wnd)
type     = c/f!c!c!b
cint     = -8;-6;-4;-2!1/0/28!3
line     = 3//1!32//1!6//3
fint     = 0;4;8;12;16
fline    = 0;23;22;30;14;2
hilo     = 0!0!6/H#;L#
hlsym    = 0!0!1.5;1.5//22;22/2;2/hw
clrbar   = 1/V/LL!0
wind     = bk0!bk0!bk0!bk9/0.8/2/212
refvec   =
title    = 1/-2/~ ? ${MDL} @ DEWPOINT, WIND, AND HGHT|~@ DEWPOINT!0
r

glevel   = 850                    !850       !0         !850   
gvcord   = pres                   !pres      !none      !pres
skip     = 0/1;2
scale    = 2                      !-1/2      !0                                    !2
gdpfun   = sm5s(mag(smul(mixr;wnd)!sm5s(hght)!sm5s(thte)!smul(mixr;wnd)
type     = c/f                    !c         !c         !a
cint     = 3                      !3         !5
line     = 3                      !6//2      !25/10/2   
fint     = 6;12;18;24;30
fline    = 0;23;22;21;14;15;2
hilo     = 0!6/H#;L#!0
hlsym    = 0!1;1//22;22/2;2/hw
clrbar   = 1/V/LL!0
wind     = bk0!bk0!bk0!am16/0.6/2/211/0.3!bk0 
refvec   = 10 
text     = s/22/2/hw
title    = 1/-2/~ ? ${MDL} @ MOIST. TRNSPT, HGHT, BL THTE|~@ H2O TRANSPORT!0
r
 
glevel	 = 850
gvcord	 = pres
skip     = 0/1;1
scale	 = 4                 !0
gdpfun   = sm5s(adv(thte,wnd)!sm5s(thte)!sm5s(thte)!sm5s(thte)!kntv(wnd)
type	 = c/f               !c         !c         !c         !b
cint	 = 2                 !4//304    !4/308/324 !4/328
line	 = 32/1/2            !23/10/3   !22/10/3   !21/1/2
fint	 = -14;-10;-6;-2;2;6;10;14!
fline	 = 7;29;30;24;0;14;15;18;5!
hilo	 = 
hlsym	 = 
clrbar	 = 1/V/LL!0
wind	 = bk0               !bk0       !bk0        !bk0       !bk9/0.8/2/112!bk0
refvec	 = 10
text     = m/22/2/hw
title	 = 1/-2/~ ? ${MDL} @ THTE ADV, THTE & WIND|~@ THTE ADVECTION!0
r

glevel	 = 850               !850       !850       !850       !850      !1000:700
gvcord	 = pres
SKIP     = 0/2;1
scale	 = 4                 !0         !0         !0         !0        !4
gdpfun	 = sm5s(adv(thte,wnd)!sm5s(thte)!sm5s(thte)!sm5s(thte)!kntv(wnd)!sm5s(msdv(thte,wind))
type	 = c/f               !c         !c         !c         !b        !c
cint	 = 2                 !4//304    !4/308/324 !4/328     !         !3//-3!
line	 = 32/1/2            !23/10/3   !22/10/3   !21/1/2    !         !6/1/2
fint	 = -14;-10;-6;-2;2;6;10;14!
fline	 = 7;29;30;24;0;14;15;18;5!
hilo	 = 0!
hlsym	 = 0!
clrbar	 = 1/V/LL!0
wind	 = bk0               !bk0       !bk0       !bk0       !bk9/0.7/2/112!bk0
refvec   = 10
title	 = 1/-2/~ ? ${MDL} @ THTE ADV,WIND,(1000-700 CNVRG-AQUA)|~@ THTE ADV/CNV!0
r

glevel   = 750                !750       !750       !750       !750      !850:700
gvcord   = pres
SKIP     = 0/2;1
scale    = 4                  !0         !0         !0         !0        !4
gdpfun   = sm5s(adv(thte,wind)!sm5s(thte)!sm5s(thte)!sm5s(thte)!kntv(wnd)!sm5s(msdv(thte,wind))
type     = c/f                !c         !c         !c         !b        !c
cint     = 2                  !4//304    !4/308/324 !4/328     !         !3//-3
line     = 32/1/2             !23/10/3   !22/10/3   !21/1/2    !         !6/1/2
fint     = -14;-10;-6;-2;2;6;10;14!
fline    = 7;29;30;24;0;14;15;18;5!
hilo     = 0!
hlsym    = 0!
clrbar   = 1/V/LL!0
wind     = bk0                !bk0       !bk0       !bk0       !bk9/0.7/2/112!bk0
refvec   = 10
title    = 1/-2/~ ? ${MDL} @ THTE ADV,WIND,850-700 MST-FLUX CNVG(AQUA)|~@ THTE ADV/CNV!0
r

glevel	 = 700       !700       !9950      !0
gdpfun   = sm5s(kinx)!sm5s(tmpc)!sm5s(dwpf)!sm5s(pmsl)
gvcord	 = pres      !pres      !sgma      !none
skip     = 0/1;1
scale	 = 0
type	 = c/f       !c
cint	 = 3/15/60   !2/6       !50;55;60;65;70;75;80!4
line	 = 32/1/2/2  !20/1/2    !23/1/3    !6//3
fint	 = 15;24;33;42
fline	 = 0;24;30;14;2
hilo	 = 0         !0         !0         !6/H;L
hlsym	 =           !          !          !1.5;1.5//22;22/3;3/hw
clrbar	 = 1/V/LL!0
wind	 =
refvec	 =
title	 = 1/-2/~ ? ${MDL} K INDEX, 700mb TEMP (>6 C), sfc DWPT & MSLP|~K INDEX!0
r

glevel   = 300
gvcord   = pres
SKIP     = 0/2;2
scale    = 0                       !5/0               !5/0    !-1        !5/0
gdpfun   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)!age(hght)
type     = c/f                     !c                 !c      !c         !a
cint     = 70;90;110;130;150;170   !-11;-9;-7;-5;-3;-1!2/2/100!12
line     = 32/1                    !20/-2/2           !3/1/2  !1//2
fint     = 70;90;110;130;150;170;190!
fline    = 0;24;25;30;28;14;2;1    !
hilo     = 0                       !0                 !0      !1/H#;L#/3
hlsym    = 0                       !0                 !0      !1.3//22/2/hw
clrbar   = 1/V/LL                  !0
wind     = bk0                     !bk0               !bk0    !bk0       !am16/0.4//211/0.4
refvec   = 10
title    = 1/-1/~ ? ${MDL} @ DIV(GREEN),ISOTACHS & AGEO WND|~@ AGEO & DIVERG!0
filter   = no
r

GLEVEL   = 250
GDPFUN   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)!age(hght)
r

glevel   = 200
gdpfun   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)!age(hght)
r

gdattim  = ${gdatpcpn06}
glevel   = 0!0
gvcord   = none!none
skip     = 0   !0
scale    = 0   !0
gdpfun   = p06i!sm5s(pmsl)
type     = f   !c
cint     =     !4
line     =     !17/1/3
fint     = .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9
fline    = 0;21-30;14-20;5
hilo     = 31;0/x#2/.01-20//50;50/y!17/H#;L#/1020-1070;900-1012
hlsym    = 1.5!1;1//22;22/2;2/hw
clrbar   = 1
wind     = bk0
refvec   =
title    = 1/-2/~ ? ${MDL} 6-HOUR TOTAL PCPN, MSLP |~6-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn12} 
gdpfun   = p12i
type     = f
title    = 1/-2/~ ? ${MDL} 12-HOUR TOTAL PCPN|~12-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn24}       
gdpfun   = p24i
title    = 1/-2/~ ? ${MDL} 24-HOUR TOTAL PCPN|~24-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn48}
gdpfun   = p48i
title    = 1/-2/~ ? ${MDL} 48 HOUR TOTAL PCPN|~48-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn60}
gdpfun   = p60i
title    = 1/-2/~ ? ${MDL} 60 HOUR TOTAL PCPN|~60-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn72}       
gdpfun   = p72i
title    = 1/-2/~ ? ${MDL} 72 HOUR TOTAL PCPN|~72-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn84}       
gdpfun   = p84i
title    = 1/-2/~ ? ${MDL} 84 HOUR TOTAL PCPN|~84-HR TOTAL PCPN!0
r

gdattim  = ${gdatpcpn96}
gdpfun   = p96i
title    = 1/-2/~ ? ${MDL} 96 HOUR TOTAL PCPN|~96-HR TOTAL PCPN!0
${run}

gdattim  = ${gdatpcpn120}
gdpfun   = p120i
title    = 1/-2/~ ? ${MDL} 120 HOUR TOTAL PCPN|~120-HR TOTAL PCPN!0
${run}


gdattim  = ${gdatpcpn06}
gvcord   = none!none
gdpfun   = c06i!sm5s(pmsl)
type     = f   !c
cint     =     !4
line     =     !17/1/3
title    = 1/-2/~ ? ${MDL} 6-HOUR CONV PCPN, MSLP |~6-HR CONV PCPN!0
r

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
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
      ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
      if [ $DBN_ALERT_TYPE = "GFS_METAFILE_LAST" ] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        ${COMOUT}/${mdl}_${PDY}_${cyc}_us_${metatype}
      fi
   fi
fi

exit

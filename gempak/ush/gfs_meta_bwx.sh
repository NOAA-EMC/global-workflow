#! /bin/sh
#
# Metafile Script : gfs_meta_bwx_new
#
# Log :
# D.W.Plummer/NCEP     2/97   Add log header
# J. Carr/HPC      12/12/97   Converted from gdplot to gdplot2
# J. Carr/HPC      08/05/98   Changed map to medium resolution
# J. Carr/HPC      02/02/99   Changed skip to 0
# J. Carr/HPC      04/12/99   Added gfs out to 84 hrs.
# J. Carr/HPC          6/99   Added a filter to map
# J. Carr/HPC        1/2000   Eliminated 250 mb vort and pw field. Eliminated pv field.  Added another ptype field.
# J. Carr/HPC        2/2001   Edited to run on the IBM.
# J. Carr/HPC        5/2001   Added a mn variable for a/b side dbnet root variable.
# J. Carr/HPC        6/2001   Converted to a korn shell prior to delivering script to Production.
# J. Carr/HPC        7/2001   Submitted.
# J. Carr/PMB       11/2004   Added a ? to all title/TITLE lines.
# M. Klein/HPC      01/2010   Extend to 180 hours
#
# Set up Local Variables
#
set -x
#
export PS4='BWX:$SECONDS + '
mkdir -p -m 775 $DATA/BWX
cd $DATA/BWX
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
metatype="bwx"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`
#
#if [ ${cyc} -eq 00 ] ; then
#    fend=F126
#elif [ ${cyc} -eq 12 ] ; then
#    fend=F126
#else
#    fend=F126
#fi

fend=F180

export pgm=gdplot2_nc;. prep_step; startmsg
$GEMEXE/gdplot2_nc<< EOFplt
gdfile   = F-${MDL} | ${PDY2}/${cyc}00
gdattim  = F00-${fend}-6
CONTUR	 = 1
garea    = bwus
proj     = 
map      = 1/1/1/yes
latlon   = 0
text     = 1/22/2/hw
device   = ${device}
clear    = y
panel    = 0
filter   = y
skip     = 0

glevel   = 0
gvcord   = none
scale    = 0
gdpfun   = sm5s(thte)!sm5s(thte)!sm5s(thte)!kntv(wnd@9950%sgma)
type     = c/f       !c         !c         !b
cint     = 4/200/308 !4/312/324 !4/328
line     = 16/1/1    !2/1/3     !32/1/2/1
fint     = 328;336;344;352;360;368
fline    = 0;24;30;29;15;18;20
hilo     = 
hlsym    = 
clrbar   = 1/V/LL    !0
wind     = bk0       !bk0       !bk0        !bk9/0.7/2/112 
refvec   = 
title    = 1/0/~ ? ${MDL} BL THTE & WIND (KTS)|~BL THTE & WIND!0
l
r

glevel   = 9950      !9950      !9950      !0
gvcord   = sgma      !sgma      !sgma      !none
scale    = 0
gdpfun   = sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(pmsl)!kntv(wnd@9950%sgma)
type     = c/f       !c         !c         !c         !b
cint     = 3/-99/0   !3/3/18    !3/21/99   !4
line     = 27/1/2    !2/1/2     !16/1/2    !19//3
fint     = -24;-12;0 !
fline    = 29;30;24;0!
hilo     = 0         !0         !0         !20/H#;L#/1020-1070;900-1012
hlsym    = 0         !0         !0         !1.3;1.3//22;22/3;3/hw
clrbar   = 1/V/LL    !0
wind     = bk0       !bk0       !bk0       !bk0       !bk9/0.7/2/112
refvec   = 
title    = 1/0/~ ? ${MDL} PMSL, BL TEMP, WIND (KTS)|~PMSL, BL TEMP, WIND!0
r 

GLEVEL   = 1000                !1000        !0         !1000
GVCORD   = pres                !pres        !none      !pres
SKIP     = 0/1
SCALE    = 1                   !0           !0
GDPFUN   = sm5s(frnt(tmpf,wnd) !sm5s(tmpf)  !sm5s(pmsl)!kntv(wnd)
TYPE     = c/f                 !c           !c         !b
CONTUR   = 1
CINT     = 10                  !4           !4
LINE     = 32/1/2/1            !2/1/2       !6/1/2/1
FINT     = -50;-35;-20;-5;5;20;35;50
FLINE    = 29;30;25;24;0;23;22;14;20
HILO     = !!6/H#;L#/1020-1070;900-1012
HLSYM    = !!1.5;1.5//22;22/3;3/hw
CLRBAR   = 1
WIND     = !!!bk9/0.6/2/121/.6
REFVEC   =
TITLE    = 1/0/~ ? ${MDL} PMSL, 1000 MB TMP (F), FRONTOGENESIS (F)|~@ FRONTOGENESIS!0
r
 
glevel	 = 700       !700       !9950      !0
gdpfun   = sm5s(kinx)!sm5s(tmpc)!sm5s(dwpf)!sm5s(pmsl)
gvcord	 = pres      !pres      !sgma      !none
scale	 = 0
type	 = c/f       !c
cint	 = 3/15/60   !2/6       !50;55;60;65;70;75;80!4
line	 = 32/1/2/2  !20/1/2    !23/1/3!6//3
fint	 = 15;24;33;42
fline	 = 0;24;30;14;2
hilo	 = 0!0!0!6/H#;L#/1020-1070;900-1012
hlsym	 = !!!1.5;1.5//22;22/3;3/hw
clrbar	 = 1/V/LL!0
wind	 =
refvec	 =
title	 = 1/0/~ ? ${MDL} K INDEX, 700mb TEMP (>6 C), sfc DWPT & MSLP|~K INDEX!0
r

gdattim  = F06-${fend}-06  
glevel   = 0!500:1000!500:1000!0
gvcord   = none!pres!pres!none 
skip     = 0 
scale    = 0   !-1                   !-1            !0 
gdpfun   = p06i!sm5s(ldf(hght)       !sm5s(ldf(hght)!sm5s(pmsl)
type     = f   !c                    !c
cint     =     !3/0/540              !3/543/1000    !4
line     =     !4/5/2                !2/5/2         !19//3  
fint     = .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.5;3;4;5;6;7;8;9
fline    = 0;21-30;14-20;5
hilo     =     !0!0!19/H#;L#/1020-1070;900-1010
hlsym    =     !0!0!1.3;1.3//22;22/3;3/hw
clrbar   = 1
wind     = bk0 
CONTUR	 = 2
refvec   = 
title    = 1/0/~ ? ${MDL} 6-HR TOTAL PCPN, 1000-500mb THK |~6-HR PCPN & 1000-500 THK!0
r

gdattim  =  F00-${fend}-6
GLEVEL	 = 700!700!700!850!850!9950!9950
GVCORD	 = PRES!PRES!PRES!PRES!PRES!sgma!sgma
SKIP	 = 0 
SCALE	 = 0 
GDPFUN	 = sm5s(relh)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)
TYPE	 = c/f        ! c
CINT	 = 50;70;90;95!2;-2 !200;0 !2;-2 !200;0 !2;-2 !-100;0;100    
LINE	 = 32//1/0    !6/3/2!6/1/2 !2/3/2!2/1/2 !20/3/2!20/1/2  
FINT	 = 50;70;90
FLINE	 = 0;24;23;22
HILO	 = 
HLSYM	 = 
CLRBAR	 = 1
WIND	 = 
REFVEC	 =
TITLE    = 1/0/~ ? ${MDL} @ RH, T (BL yel,850 red,700 cyan)|~@ RH, R/S TEMP!0
r 

GLEVEL	 = 4400:10000!700:500!700:500!850 !850 !9950!9950
GVCORD	 = SGMA      !PRES   !PRES   !PRES!PRES!SGMA!SGMA
SCALE	 = 0!3!3!0  
GDPFUN	 = sm5s(relh)!sm5s(lav(omeg))!sm5s(lav(omeg))!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)
TYPE	 = c/f        ! c
CINT	 = 50;70;90;95!1/1!-1;-3;-5;-7;-9;-11;-13;-15;-17;-19;-21!2;-2!200;0!2;-2!200;0    
LINE	 = 32//2/0    !30/10/3!6/1/2 !2/3/2!2/1/2 !20/3/2!20/1/2  
FINT	 = 50;70;90
FLINE	 = 0;24;23;22
HILO	 = 
HLSYM	 = 
CLRBAR   = 1
WIND	 = 
REFVEC	 =
TITLE    = 1/0/~ ? ${MDL} @ RH,T (BL yel,850 red),7-500 VV|~@ RH,R/S T,VV!0
r

glevel	 = 0!0!0!0!700:500         !4400:10000
gvcord	 = none!none!none!none!PRES!sgma
scale	 = 2!2!2!2!3!0
gdpfun   = sm5s(WXTr06)!sm5s(WXTs06)!sm5s(WXTp06)!sm5s(WXTz06)!sm5s(lav(omeg))!sm5s(relh)
refvec	 =
type	 = c/f!c/f!c/f!c/f!c!c
cint	 = 50;200!50;200!50;200!50;200!-1;-3;-5;-7;-9;-11;-13;-15;-17;-19;-21!5/70
line	 = 22/1/2/0!4/1/2/0!7/1/2/0!2/1/2/0!6/1/3!21/1/3
fint	 = 50;200!50;200!50;200!50;200
fline	 = 0;23;23!0;25;25!0;30;30!0;15;15
clrbar	 =
title	 = 1/0/~ ? ${MDL} PCPN TYPE, 1000-500 RH & 7-500 VV|~PCPN TYPE & VV!0
r

glevel	 = 0           !0           !0           !0
gvcord	 = none        !none        !none        !none
scale	 = 2           !2           !2           !2
gdpfun   = sm5s(WXTr06)!sm5s(WXTs06)!sm5s(WXTp06)!sm5s(WXTz06)
refvec	 =
type	 = c/f         !c/f         !c/f         !c/f
cint	 = 50;200      !50;200      !50;200      !50;200
line	 = 22/1/2/0    !4/1/2/0     !7/1/2/0     !2/1/2/0
fint	 = 50;200      !50;200      !50;200      !50;200
fline	 = 0;23;23     !0;25;25     !0;30;30     !0;15;15
clrbar	 =
title	 = 1/0/~ ? ${MDL} PCPN TYPE|~PCPN TYPE!0
r

GLEVEL   = 500
GVCORD   = pres
SKIP     = 0!0!0!0/2;1
SCALE    = 0!0!-1!0
GDPFUN   = mag(kntv(wnd)!sm5s(tmpc)!sm5s(hght)!kntv(wnd)
TYPE     = f!c!c!a
CONTUR   = 7/4
CINT     = !2!6!!
LINE     = !2/12/2/2!6/1/2!
FINT     = 60;65;70;75;80;85;90;95;100;105;110;115;120;125;130
FLINE    = 0;23-1
HILO     =
HLSYM    =
CLRBAR   = 1
WIND     = !!!am1/.2/1/121/.4
REFVEC   =
TITLE    = 1/0/~ ? ${MDL} @ HGHT, TEMP & WIND|~500 HGHT,TMP,WIND!0
TEXT     = 1/21//hw
MAP      = 11/1/2/yes
STNPLT   =
SATFIL   =
RADFIL   =
LUTFIL   =
STREAM   =
POSN     = 0
COLORS   = 0
MARKER   = 0
GRDLBL   = 0
FILTER   = n
r

GAREA	 = 105
PROJ	 = str/90;-105;0
LATLON	 = 0
MAP      = 1/1/2/yes

GDATTIM  = f12
GLEVEL   = 500
GVCORD   = pres
PANEL    = 0
SKIP     = 0
SCALE    = -1        !0                       !0                       !-1
GDPFUN   = sm5s(hght)!(sub(hght^f12,hght^f00))!(sub(hght^f12,hght^f00))!sm5s(hght)
TYPE     = c         !f                       !f                       !c
CONTUR   = 1
CINT     = 6         !20/20                   !20/-240/-20             !6
LINE     = 5/1/3     !32/1/1                  !1/10/2/1                !5/1/3
FINT     = 0         !30;60;90;120;150;180    !-180;-150;-120;-90;-60;-30
FLINE    = 0         !0;24;25;30;29;28;27     !11;12;2;10;15;14;0
HILO     = 0         !0                       !0                       !5/H#;L#
HLSYM    = 0         !                        !0                       !1.5//21//hw
CLRBAR   = 0         !0                       !1                       !0
WIND     = 
REFVEC   = 
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
TEXT     = 1/21////hw
CLEAR    = YES
l
run

GDATTIM  = f24
GDPFUN   = sm5s(hght)!(sub(hght^f24,hght^f12))!(sub(hght^f24,hght^f12))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run 

GDATTIM  = f36
GDPFUN   = sm5s(hght)!(sub(hght^f36,hght^f24))!(sub(hght^f36,hght^f24))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f48
GDPFUN   = sm5s(hght)!(sub(hght^f48,hght^f36))!(sub(hght^f48,hght^f36))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f60
GDPFUN   = sm5s(hght)!(sub(hght^f60,hght^f48))!(sub(hght^f60,hght^f48))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f72
GDPFUN   = sm5s(hght)!(sub(hght^f72,hght^f60))!(sub(hght^f72,hght^f60))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f84
GDPFUN   = sm5s(hght)!(sub(hght^f84,hght^f72))!(sub(hght^f84,hght^f72))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f96
GDPFUN   = sm5s(hght)!(sub(hght^f96,hght^f84))!(sub(hght^f96,hght^f84))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f108
GDPFUN   = sm5s(hght)!(sub(hght^f108,hght^f96))!(sub(hght^f108,hght^f96))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

GDATTIM  = f120
GDPFUN   = sm5s(hght)!(sub(hght^f120,hght^f108))!(sub(hght^f120,hght^f108))!sm5s(hght)
TITLE    = 1/-1/~ ? ${MDL} @ MB HGT|~500 HGT CHG!1/-2/~ ? ${MDL} @ MB 12-HR HGT FALLS!0
l
run

MAP      = 4/1/2/yes 
garea    = 38.5;-91.3;51.4;-71.4
proj     = nps//3;3;0;1
GDATTIM  = F00-${fend}-6
glevel	 = 7200:9400     !850                  !850       !850
gvcord	 = sgma          !pres                 !pres      !pres
scale	 = 0             !0                    !0         !3
gdpfun	 = sm5s(relh)    !sub(tmpc@2%hght,tmpc)!sm5s(tmpc)!sm5s(omeg)!kntv(wnd@30:0%pdly)
type	 = f             !c                    !c         !c         !b
cint	 =               !1/10                 !1//0      !1//-1
line	 =               !20/1/1               !2/3/3     !6/1/2
fint     = 70;80;90;95
fline    = 0 ;24;23;22;21
hilo	 = 0
hlsym	 = 0
clrbar	 = 1/V/LL        !0
wind	 = bk0           !bk0                  !bk0       !bk0        !bk9/0.9/2/112
refvec	 =
title	 = 1/0/~ ? ${MDL} 720-940 MB AVG RH,BL1 WND,850 MB OMG,850-2m dT,850 T|~GR LAKE!0
FILTER   = y
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

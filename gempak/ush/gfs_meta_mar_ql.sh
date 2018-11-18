#! /bin/sh
#
# Metafile Script : gfs_meta_mar_ql.sh
#
# Log :
# J. Carr/PMB    12/07/2004   Pushed into production
#
# Set up Local Variables
#
set -x
#
export PS4='MAR_QL_F${fend}:$SECONDS + '
mkdir -p -m 775  $DATA/MAR_QL
cd $DATA/MAR_QL
cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
metatype="mar_ql"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
PDY2=`echo $PDY | cut -c3-`
# fend=180

export pgm=gdplot2_nc;. prep_step; startmsg

$GEMEXE/gdplot2_nc << EOFplt
\$MAPFIL=mepowo.gsf+mehsuo.ncp+mereuo.ncp+mefbao.ncp
gdfile	= F-${MDL} | ${PDY2}/${cyc}00
gdattim	= f00-f${fend}-6
GAREA	= 15;-100;70;5
PROJ	= mer//3;3;0;1
MAP	= 31 + 6 + 3 + 5
LATLON	= 18/2/1/1/10
CONTUR	= 0
device	= $device 
GLEVEL	= 9950!0
GVCORD	= sgma!none
PANEL	= 0
SKIP	= 0/2
SCALE	= 0
GDPFUN	= mag(kntv(wnd))!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE	= c/f           !c         !b
CINT	= 5/20!4
LINE	= 32/1/2/2!19//2
FINT	= 20;35;50;65
FLINE	= 0;24;25;30;15
HILO	= 0!20/H#;L#
HLSYM	= 0!1;1//22;22/3;3/hw
CLRBAR	= 1/V/LL!0
WIND	= bk9/0.6/2/112
REFVEC	=
TITLE	= 1/-2/~ ? |~ATL PMSL & BL WIND!1//GFS PMSL, BL WIND (40m AGL; KTS)
TEXT	= 1.2/22/2/hw
CLEAR	= YES
li
run

GAREA	= 13;-84;50;-38
PROJ	= str/90;-67;1
LATLON	= 18/2/1/1/5;5
SKIP	= 0/1
TITLE	= 1/-2/~ ? |~WATL PMSL & BL WIND!1//GFS MSL PRES, BL WIND (40m AGL; KTS)
li
run

GLEVEL  = 850:1000                  !0
GVCORD  = pres                      !none
SCALE   = -1                        ! 0
GDPFUN  = (sub(hght@850,hght@1000)) !sm5s(pmsl) ! kntv(wnd@9950%sgma)
TYPE    = c                         ! c         ! b
CINT    = 1                         ! 4
LINE    = 3/5/1/2                   ! 20//2
FINT    =
FLINE   =
HILO    = ! 26;2/H#;L#/1020-1070;900-1012//30;30/y
HLSYM   = 2;1.5//21//hw
CLRBAR  = 1
WIND    = bk9/0.7/2/112
TITLE   = 5/-2/~ ? GFS MSLP, 1000-850 THK & BL (~40m) WIND|~WATL MSLP,1000-850 THK!0
li
ru

GLEVEL  = 500
GVCORD  = PRES
SKIP    = 0                  
SCALE   = 5                  !-1
GDPFUN   = (avor(wnd))        !hght
TYPE   = c/f                !c
CINT    = 3/3/99             !6
LINE    = 7/5/1/2            !20/1/2/1
FINT    = 15;21;27;33;39;45;51;57
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!          !
HLSYM   = 
WIND    = 0
TITLE   = 5//~ ? GFS @ HEIGHTS AND VORTICITY|~WATL @ HGHT AND VORT!0
li
ru

\$MAPFIL=mepowo.gsf+mehsuo.ncp+mereuo.ncp+himouo.nws
GAREA      = 4;120;69;-105
PROJ       = mer//3;3;0;1
LATLON     = 18/2/1/1/10

GLEVEL  = 9950!0
GVCORD  = sgma!none
SKIP    = 0/2
SCALE   = 0
GDPFUN  = mag(kntv(wnd))!sm5s(pmsl)!kntv(wnd@9950%sgma)
TYPE    = c/f           !c         !b
CINT    = 5/20!4
LINE    = 32/1/2/2!19//2
FINT    = 20;35;50;65
FLINE   = 0;24;25;30;15
HILO    = 0!20/H#;L#/1020-1070;900-1012
HLSYM   = 0!1;1//22;22/3;3/hw
CLRBAR  = 1/V/LL!0
WIND    = bk9/0.6/2/112
TITLE   = 1/-2/~ ? |~PAC PMSL & BL WIND!1//GFS MSL PRES, BL WIND (40m AGL; KTS)
li
ru

GAREA   = 11;-135;75;-98
PROJ    = str/90;-100;1
LATLON  = 18/2/1/1/5;5
SKIP	= 0/1
TITLE	= 1/-2/~ ? |~EPAC PMSL & BL WIND!1//GFS MSL PRES, BL WIND (40m AGL; KTS)
li
ru

GLEVEL  = 850:1000                  !0
GVCORD  = pres                      !none
SCALE   = -1                        ! 0
GDPFUN  = (sub(hght@850,hght@1000)) !sm5s(pmsl) ! kntv(wnd@9950%sgma)
TYPE    = c                         ! c         ! b
CINT    = 1                         ! 4
LINE    = 3/5/1/2                   ! 20//2
FINT    =
FLINE   =
HILO    = ! 26;2/H#;L#/1020-1070;900-1012//30;30/y
HLSYM   = 2;1.5//21//hw
CLRBAR  = 1
WIND    = bk9/0.7/2/112
TITLE   = 5/-2/~ ? GFS MSLP, 1000-850 THK & BL (~40m) WIND|~EPAC MSLP,1000-850 THK!0
li
ru

GLEVEL  = 500
GVCORD  = PRES
SKIP    = 0                  
SCALE   = 5                  !-1
GDPFUN  = (avor(wnd))        !hght
TYPE    = c/f                !c
CINT    = 3/3/99             !6
LINE    = 7/5/1/2            !20/1/2/1
FINT    = 15;21;27;33;39;45;51;57
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!          !
HLSYM   = 
WIND    = 0
TITLE   = 5//~ ? GFS @ HEIGHTS AND VORTICITY|~EPAC @ HGHT AND VORT!0
li
ru
exit
EOFplt

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l $metaname
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
   mv ${metaname} ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_ql
   if [ $SENDDBN = "YES" ] ; then
      ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job ${COMOUT}/${mdl}_${PDY}_${cyc}_mar_ql
   fi
fi

exit

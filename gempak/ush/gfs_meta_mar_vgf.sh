#!/bin/sh
#
# Metafile Script : gfs_meta_mar_vgf.sh
#
# Log :
# J. Carr/PMB           12/02/04 Pushed into production.
# C.Janota/OPC		02/13/06 added garea to Atl sfc pres
# C.Janota/OPC		02/13/06 changed text size and label freq on sfc isobars
#
# Set up Local Variables
#
set -x
#
export PS4='OPC_MAR_VGF:$SECONDS + '
workdir="${DATA}/OPC_MAR_VGF"
mkdir -p -m 775 ${workdir}
cd ${workdir}

cp $FIXgempak/datatype.tbl datatype.tbl

mdl=gfs
MDL="GFS"
PDY2=`echo $PDY | cut -c3-`

export DBN_ALERT_TYPE=VGF
export DBN_ALERT_SUBTYPE=OPC

# SET UP A FLAG FOR WHAT CYCLES TO RUN VARIOUS PRODUCTS.
runflag="yes"

if [ ${cyc} -eq 12 ] ; then
    runflag="yes"
elif [ ${cyc} -eq 00 ] ; then
    runflag="yes"
else
    runflag="no"
fi

# 500 MB FOR ATL/PAC

if [ ${runflag} == "yes" ] ; then
    for fcsthr in 00 24 36 48 96
    do
        for ocean in ATL PAC
        do
            if [ ${ocean} == "ATL" ] ; then
                garea="17;-98;64;10"
                proj="mer"
            else
                garea="17;136;64;-116"
                proj="mer"
            fi
            vgfile="500_${ocean}_${PDY2}_${cyc}_f${fcsthr}.vgf"

$GEMEXE/gdplot2_vg << EOF
gdfile	= F-${MDL} | ${PDY2}/${cyc}00
gdattim	= f${fcsthr}
GLEVEL	= 500
GVCORD	= PRES
PANEL	= 0
SKIP	= 0/9;4
SCALE	= -1
GDPFUN	= hght!hght
TYPE	= c!c
CONTUR	= 0
CINT	= 6/-99/558!6/570/999
LINE	= 20/1/3/-2/2/.03!20/1/3/-2/2/.03
FINT	=
FLINE	=
HILO	= 6;2/H#;L#///5;5!0
HLSYM	= 3.7;2.5/2/22;31/3;3/hw!
CLRBAR	=
WIND	=
REFVEC	=
TITLE	= 
TEXT	= 1.5/21/2.2/121/hw
CLEAR	= y
STNPLT	=
SATFIL	=
RADFIL	=
STREAM	=
POSN	= 4
COLORS	= 2
MARKER	= 2
GRDLBL	= 5
LUTFIL	= none
FILTER	= no
GAREA	= ${garea}
PROJ	= ${proj}
MAP	= 0
LATLON	= 0
DEVICE	= vg | ${vgfile}
STNPLT	=
list
run

clear	= no
gdpfun	= hght!vge(kntv(wnd),30)
type	= c!b
cint	= 6/564/564
line	= 20/1/6/-2/2/.03
wind	= bk25/1.6/803/114
hilo	=
hlsym	=
list
run

ex
EOF

            if [ $SENDCOM = "YES" ] ; then
                mv *.vgf ${COMOUT}
                if [ $SENDDBN = "YES" ] ; then
                   ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${vgfile}
                fi
            fi
        done
    done 

# CONTINUE LOOP OF 12 OR 00 UTC CYCLE...

$GEMEXE/gdplot2_vg << EOF
GAREA   = 10;-113;45;-38
PROJ    = str/90;-67;1/0;0;0;1
gdattim	= f24
device	= vg | 500_${PDY2}_${cyc}_f24.vgf
GDPFUN  = hght!hght
TYPE    = c!c
CINT    = 6/-99/558!6/570/999
LINE    = 20/1/3/-2/2/.03!20/1/3/-2/2/.03
WIND    =
HILO    = 6;2/H#;L#///5;5!0
HLSYM   = 3.7;2.5/2/22;31/3;3/hw!
list
run

clear   = no
gdpfun  = hght!vge(kntv(wnd),30)
type    = c!b
cint    = 6/564/564
line    = 20/1/6/-2/2/.03
wind    = bk25/1.6/803/114
hilo	=
hlsym	=
list
run

exit
EOF

    if [ $SENDCOM = "YES" ] ; then
        mv *.vgf ${COMOUT}
        if [ $SENDDBN = "YES" ] ; then
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/500_${PDY2}_${cyc}_f24.vgf
        fi
    fi
else

# 06Z AND 18Z CYCLES...

$GEMEXE/gdplot2_vg << EOFplt
gdfile  = F-${MDL} | ${PDY2}/${cyc}00
gdattim = f06
GLEVEL  = 500
GVCORD  = PRES
PANEL   = 0
SKIP    = 0/9;4
SCALE   = -1
GDPFUN  = hght!hght
TYPE    = c!c
CONTUR  = 0
CINT    = 6/-99/558!6/570/999
LINE    = 20/1/3/-2/2/.03!20/1/3/-2/2/.03
FINT    =
FLINE   =
HILO    = 6;2/H#;L#///5;5!0
HLSYM   = 3.7;2.5/2/22;31/3;3/hw!
CLRBAR  =
WIND    = 
REFVEC  =
TITLE   =
TEXT    = 1.5/21/2.2/121/hw
CLEAR   = y
STNPLT  =
SATFIL  =
RADFIL  =
STREAM  =
POSN    = 4
COLORS  = 2
MARKER  = 2
GRDLBL  = 5
LUTFIL  = none
FILTER  = no
GAREA   = 17;-98;64;10
PROJ    = mer
MAP     = 0
LATLON  = 0
DEVICE  = vg | 500_ATL_${PDY2}_${cyc}_f06.vgf
STNPLT  =
list
run

clear   = no
gdpfun  = hght!vge(kntv(wnd),30)
type    = c!b
cint    = 6/564/564
line    = 20/1/6/-2/2/.03
wind    = bk25/1.6/803/114
hilo	=
hlsym	=
list
run

clear   = yes
device  = vg | 500_PAC_${PDY2}_${cyc}_f06.vgf
garea   = 17;136;64;-116
GDPFUN  = hght!hght
TYPE    = c!c
CINT    = 6/-99/558!6/570/999
LINE    = 20/1/3/-2/2/.03!20/1/3/-2/2/.03
WIND    =
HILO    = 6;2/H#;L#///5;5!0
HLSYM   = 3.7;2.5/2/22;31/3;3/hw!
list
run

clear   = no
gdpfun  = hght!vge(kntv(wnd),30)
type    = c!b
cint    = 6/564/564
line    = 20/1/6/-2/2/.03
wind    = bk25/1.6/803/114
hilo	=
hlsym	=
list
run

exit
EOFplt

    if [ $SENDCOM = "YES" ] ; then
        mv *.vgf ${COMOUT}
        if [ $SENDDBN = "YES" ] ; then
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/500_PAC_${PDY2}_${cyc}_f06.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/500_ATL_${PDY2}_${cyc}_f06.vgf
        fi
    fi
fi

# SFC FOR F48 ON ATL/PAC FOR 00z/12z CYCLES

if [ ${runflag} == "yes" ] ; then
    for fcsthr in 48 96
    do
        for ocean in ATL PAC
        do
        if [ ${ocean} == "ATL" ] ; then
            garea="17;-98;64;10"
            proj="mer"
        elif [ ${ocean} == "PAC" ] ; then
            garea="17;136;64;-116"
            proj="mer"
        fi
        vgfile="${mdl}_${PDY2}_${cyc}_${fcsthr}${ocean}sfc.vgf"

$GEMEXE/gdplot2_vg << EOF
gdattim = f${fcsthr}
garea   = ${garea}
proj    = ${proj}
latlon  =
map     = 0
clear   = yes
device  = vg | ${vgfile}
glevel  = 0
gvcord  = none
panel   = 0
skip    = 0
scale   = 0
gdpfun  = sm5s(pmsl)
type    = c
contur  = 0
cint    = 4
line    = 5//3/-1/2/.10
fint    =
fline   =
!hilo    = 7;7/h#;l#
!hlsym   = 1//21/1/hw
hilo    =
hlsym   =
clrbar  = 0
wind    =
refvec  =
title   =
text    = 1/21/2/hw
list
run

ex
EOF

        if [ $SENDCOM = "YES" ] ; then
            mv *.vgf ${COMOUT}
            if [ $SENDDBN = "YES" ] ; then
                ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${vgfile}
            fi
        fi
        done
    done

gdplot2_vg << EOF
device  = vg | ${mdl}_${PDY2}_${cyc}_48ATLwind.vgf
GLEVEL  = 9950
GAREA   = 17;-98;64;10 
GVCORD  = sgma
PANEL   = 0
SKIP    = /3
SCALE   = 0
GDPFUN  = vmsk(vge(kntv(wnd),35),sea)
TYPE    = b
CONTUR  = 0
CINT    =
LINE    =
FINT    =
FLINE   =
HILO    = 0
FILTER  = n
HLSYM   =
CLRBAR  = 0
WIND    = bk25/1.6/803/114
REFVEC  =
TITLE   =
GDATTIM = f48
TEXT    = 1/21/1/121/hw
list
run

gdattim	= f96
device	= vg | ${mdl}_${PDY2}_${cyc}_96ATLwind.vgf
list
run

GDATTIM = f24
GAREA   = 12;-135;80;159
PROJ    = STR/90;-100;1
DEVICE  = vg | ${mdl}_${PDY2}_${cyc}_24PACwwind.vgf
CLEAR   = yes
GAREA   = 38.17;169.78;47.80;-106.15
PROJ    = STR/90;-175;1/0;2;0;0
list
run

GDATTIM	= f48
GAREA   = 17;136;64;-116
PROJ    = mer
CLEAR   = yes
DEVICE  = vg | ${mdl}_${PDY2}_${cyc}_48PACwind.vgf
list
run

gdattim = f96
device  = vg | ${mdl}_${PDY2}_${cyc}_96PACwind.vgf
list
run

gdattim	= f48
garea   = 17;-98;64;10
device  = vg | ${mdl}_${PDY2}_${cyc}_48ATLwwind.vgf
SKIP	= 0/5
GDPFUN  = vmsk(vge(kntv(wnd),10),sea)
list
run

gdattim = f96
device  = vg | ${mdl}_${PDY2}_${cyc}_96ATLwwind.vgf
GDPFUN  = vmsk(vge(kntv(wnd),10),sea)
list
run

gdattim	= f48
GDPFUN  = vmsk(vge(kntv(wnd),10),sea)
GAREA   = 17;136;64;-116
CLEAR   = yes
DEVICE  = vg | ${mdl}_${PDY2}_${cyc}_48PACwwind.vgf
list
run

gdattim = f96
device  = vg | ${mdl}_${PDY2}_${cyc}_96PACwwind.vgf
GDPFUN  = vmsk(vge(kntv(wnd),10),sea)
list
run

exit
EOF

    if [ $SENDCOM = "YES" ] ; then
        mv *.vgf ${COMOUT}
        if [ $SENDDBN = "YES" ] ; then
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_48ATLwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_96ATLwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_24PACwwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_48PACwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_96PACwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_48ATLwwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_96ATLwwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_48PACwwind.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/${mdl}_${PDY2}_${cyc}_96PACwwind.vgf          
        fi
    fi
fi

# PRODUCE GUESS FIELDS FOR SFC ANALYSIS ON ALL CYCLES.

gdplot2_vg << EOF
gdattim = f06
garea   = -8.50;-55.50;52.60;75.00
proj    = STR/90;-15;0/NM
latlon  =
map     = 0
clear   = yes
device  = vg | ATL_SFC_${PDY2}_${cyc}_f06.vgf
glevel  = 0
gvcord  = none
panel   = 0
skip    = 0
scale   = 0
gdpfun  = sm5s(pmsl)
type    = c
contur  = 0
cint    = 4
line    = 5//3/-1/2/.10
fint    =
fline   =
hilo	=
hlsym	=
clrbar  = 0
wind    =
refvec  =
title   =
text    = 1/21/2/121/hw
list
run

garea   = 3.37;128.89;16.77;-105.30
proj    = STR/90;165;0/NM
clear   = yes
device  = vg | PAC_SFC_${PDY2}_${cyc}_f06.vgf
list
run

exit
EOF

    if [ $SENDCOM = "YES" ] ; then
        mv *.vgf ${COMOUT}
        if [ $SENDDBN = "YES" ] ; then
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/ATL_SFC_${PDY2}_${cyc}_f06.vgf
            ${DBNROOT}/bin/dbn_alert ${DBN_ALERT_TYPE} ${DBN_ALERT_SUBTYPE} $job ${COMOUT}/PAC_SFC_${PDY2}_${cyc}_f06.vgf
        fi
    fi
exit

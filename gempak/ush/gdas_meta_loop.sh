#! /bin/sh
#
# Metafile Script : gdas_meta_loop
#
# Log :
# D.W.Plummer/NCEP   2/97      Add log header
# J. Carr/HPC        3/98      Changed to gdplot2
# J. Carr/HPC        8/98      Changed map to medium resolution
# J. Carr/HPC        2/99      Changed skip to 0
# J. Carr/HPC        2/01      Implemented usage on IBM operationally.
# J. Carr/HPC        5/2001    Added a mn variable for a/b side dbnet root variable.
# M. Klein/HPC      11/2004    Change fnl to gdas
# M. Klein/HPC       2/2005    Changed location of working directory to /ptmp 
# M. Klein/HPC      11/2006    Modify for production on CCS

#cd $DATA

set -xa

device="nc | gdasloop.meta"

PDY2=`echo $PDY | cut -c3-`

if [ "$envir" = "para" ] ; then
   export m_title="GDASP"
else
   export m_title="GDAS"
fi

export pgm=gdplot2_nc;. prep_step; startmsg

#
# Copy in datatype table to define gdfile type
#
cp $FIXgempak/datatype.tbl datatype.tbl
export err=$?
if [[ $err -ne 0 ]] ; then
   echo " File datatype.tbl does not exist."
   exit $err
fi

#
# Define previous days
#
PDYm1=`$NDATE -24 ${PDY}${cyc} | cut -c -8`
PDYm2=`$NDATE -48 ${PDY}${cyc} | cut -c -8`
PDYm3=`$NDATE -72 ${PDY}${cyc} | cut -c -8`
PDYm4=`$NDATE -96 ${PDY}${cyc} | cut -c -8`
PDYm5=`$NDATE -120 ${PDY}${cyc} | cut -c -8`
PDYm6=`$NDATE -144 ${PDY}${cyc} | cut -c -8`
#

verdays="$PDYm6 $PDYm5 $PDYm4 $PDYm3 $PDYm2 $PDYm1 $PDY"

for day in $verdays
    do
    PDY2=`echo $day | cut -c 3-`
    if [ $day -eq $PDY ] ; then
        if [ $cyc -eq "00" ] ; then
            cycles="00"   
        elif [ $cyc -eq "06" ] ; then
            cycles="00 06"
        elif [ $cyc -eq "12" ] ; then
            cycles="00 06 12"
        elif [ $cyc -eq "18" ] ; then
            cycles="00 06 12 18"
        fi
    else
        cycles="00 06 12 18"
    fi

    for cycle in $cycles
        do
#  Test with GDAS in PROD
#        grid="${COMROOT}/nawips/${envir}/gdas.${day}/gdas_${day}${cycle}f000"
         export COMIN=${COMINgdas}.${day}/${cycle}/gempak
         grid="${COMINgdas}.${day}/${cycle}/gempak/gdas_${day}${cycle}f000"

$GEMEXE/gdplot2_nc << EOF
\$MAPFIL = mepowo.gsf
GDFILE	= $grid
GDATTIM	= F00
DEVICE	= $device
PANEL	= 0
TEXT	= m/21//hw
CONTUR	= 2
PROJ    =  
GAREA   = nam
LATLON	= 0
CLEAR	= yes

GLEVEL  = 0                   !500:1000       !500:1000       !0
GVCORD  = none                !PRES           !PRES           !none
SKIP    = 0
SCALE   = 0                   !-1             !-1             !0
GDPFUN  = sm5s(quo(pwtr;25.4))!sm5s(ldf(hght))!sm5s(ldf(hght))!sm5s(pmsl)
TYPE    = c/f                 !c
CINT    = 0.25/0.5/1.0        !6/460/540      !6/546          !4
LINE    = 22//1               !4/5/2          !2/5/2          !20/1/3
FINT    = 1.0;1.5;2.0;2.5     !
FLINE   = 0;23;22;14;2        !
HILO    = 0                   !0              !0              !20/H#;L#/1020-1080;900-1012/
HLSYM   = 0                   !0              !0              !1.5;1.5//22;22/3;3/hw
CLRBAR  = 1/V/LL              !0
WIND    = am0
MAP	= 1/1/1
REFVEC  =
TITLE   = 1/0/~ $m_title PW, EST MSLP, THICKNESS|~NAM PRCP WATER!0
r
 
PROJ    = STR/90;-105;0
GAREA   = 2;-139;27;-22
LATLON  = 1/1/1//15;15
GLEVEL  = 500
GVCORD  = PRES
SKIP    = 0                  !0
SCALE   = 5                  !-1
GDPFUN  = avor(wnd)          !sm5s(hght)
TYPE    = c/f                !c
CONTUR  = 1
CINT    = 3/9/99             !6/444
LINE    = 7/5/1/2            !20/1/2/1
FINT    = 15;21;27;33;39;45;51;57
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!
HLSYM   = 
CLRBAR  = 1
WIND    = 0
REFVEC  =
TITLE   = 5/-2/~ $m_title @ HGT AND VORTICITY|~NAM @ HGT AND VORT!0
r

GLEVEL	= 250
GVCORD	= PRES
SKIP	= 0                !0/4;4      !0
SCALE	= 0                !0          !-1
GDPFUN	= mag(kntv(wnd))   !kntv(wnd)  !sm5s(hght)
TYPE	= f                !b          !c
CINT	=                  !           !12/720
LINE	=                  !27/5/2/1   !5/1/2/1
FINT	= 70;90;110;130;150;170;190
FLINE	= 0;25;24;29;7;15;23;14
HILO	=
HLSYM	=
CLRBAR	= 1
WIND	= 0                !Bk9/.7/2/b/!
REFVEC	=
TITLE	= 5/-2/~ $m_title @ HGHT, ISOTACHS AND WIND (KTS)|~NAM @ HGT & WIND!0
FILTER  = n
r

exit
EOF

    done

done

for day in $verdays
    do
    PDY2=`echo $day | cut -c 3-`
    if [ $day -eq $PDY ] ; then
        if [ $cyc -eq "00" ] ; then
            cycles="00"
        elif [ $cyc -eq "06" ] ; then
            cycles="00 06"
        elif [ $cyc -eq "12" ] ; then
            cycles="00 06 12"
        elif [ $cyc -eq "18" ] ; then
            cycles="00 06 12 18"
        fi
    else
        cycles="00 06 12 18"
    fi

    for cycle in $cycles
        do
#  Test with GDAS in PROD
#        grid="${COMROOT}/nawips/${envir}/gdas.${day}/gdas_${day}${cycle}f000"
         export COMIN=${COMINgdas}.${day}/${cycle}/gempak
         grid="${COMINgdas}.${day}/${cycle}/gempak/gdas_${day}${cycle}f000"
   
$GEMEXE/gdplot2_nc << EOF
\$MAPFIL = mepowo.gsf
GDFILE	= $grid
GDATTIM	= F00
DEVICE	= $device
PANEL	= 0
TEXT	= m/21//hw
CONTUR	= 1
PROJ    =  
GAREA   = samps
LATLON	= 1/1/1//15;15
CLEAR	= yes

GLEVEL  = 0                  !500:1000       !500:1000       !0
GVCORD  = none               !PRES           !PRES           !none
SKIP    = 0
SCALE   = 0                  !-1             !-1             !0
GDPFUN  = sm5s(pwtr)         !sm5s(ldf(hght))!sm5s(ldf(hght))!sm5s(pmsl)
TYPE    = f                  !c
CINT    =                    !6/460/540      !6/546          !4
LINE    =                    !4/5/2          !2/5/2          !20/1/3
FINT    = 13;25;38;50        !
FLINE   = 0;23;22;21;14      !
HILO    = 0                  !0              !0              !20/H#;L#/1020-1080;900-1012/
HLSYM   = 0                  !0              !0              !1.5;1.5//22;22/3;3/hw
CLRBAR  = 1/V/LL             !0
WIND    = am0
MAP	= 1/1/1
REFVEC  =
TITLE   = 1/0/~ $m_title PW, MSLP, THICKNESS|~SAM PRCP WATER!0
r
 
GLEVEL  = 500
GVCORD  = PRES
SKIP    = 0                  !0       !0                  !0        !0
SCALE   = 5                  !5       !5                  !5        !-1
GDPFUN  = avor(wnd)//v       !v       !mul(v,-1)          !mul(v,-1)!sm5s(hght)
TYPE    = c/f                !c       !c/f                !c        !c
CONTUR  = 1
CINT    = 2/10/99            !2/6/8   !2/10/99            !2/4/8    !6
LINE    = 7/5/1/2            !29/5/1/2!7/5/1/2            !29/5/1/2 !20/1/2/1
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!        !2;6/X;N/10-99;10-99!         !
HLSYM   = 
CLRBAR  = 1
WIND    = 0
REFVEC  =
TITLE   = 5/-2/~ $m_title @ HGT AND VORTICITY|~SAM @ HGT & VORT!0
r

GLEVEL	= 250
GVCORD	= PRES
SKIP	= 0                !0/4;4      !0
SCALE	= 0                !0          !-1
GDPFUN	= mag(kntv(wnd))   !kntv(wnd)  !sm5s(hght)
TYPE	= f                !b          !c
CINT	=                  !           !12/720
LINE	=                  !27/5/2/1   !5/1/2/1
FINT	= 70;90;110;130;150;170;190
FLINE	= 0;25;24;29;7;15;23;14
HILO	=
HLSYM	=
CLRBAR	= 1
WIND	= 0                !Bk9/.7/2/b/!
REFVEC	=
TITLE	= 5/-2/~ $m_title @ HGHT, ISOTACHS AND WIND (KTS)|~SAM @ HGT & WIND!0
FILTER  = n
r

GLEVEL  = 850:1000                     !0
GVCORD  = pres                         !none
PANEL   = 0
SKIP    = 0
SCALE   = -1                           !0
GDPFUN  = sm5s(sub(hght@850,hght@1000))!sm5s(pmsl)
TYPE    = c                            !c
CINT    = 1                            !4
LINE    = 22/5/2/1                     !10/1/1
FINT    =
FLINE   = 
HILO    =                              !26;2/H#;L#/1020-1070;900-1012/3/30;30/y
HLSYM   =                              !2;1.5//21//hw
WIND    = 0
TITLE   = 1/-1/~ $m_title PMSL, 1000-850mb THKN|~SAM PMSL, 1000-850 TK!0
r

exit
EOF

    done
done

export err=$?;err_chk
#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l gdasloop.meta
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
    mv gdasloop.meta ${COMOUT}/gdas_${PDY}_${cyc}_loop
    export err=$?
    if [[ $err -ne 0 ]] ; then
      echo " File gdasloop.meta does not exist."
      exit $err
    fi

    if [ $SENDDBN = "YES" ] ; then
        ${DBNROOT}/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
        $COMOUT/gdas_${PDY}_${cyc}_loop
    fi
fi

exit

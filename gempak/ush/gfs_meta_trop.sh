#! /usr/bin/env bash
#
# Metafile Script : gfs_meta_trop.sh
#
# Set Up Local Variables
#

source "${HOMEgfs}/ush/preamble.sh"

mkdir -p -m 775 "${DATA}/TROP"
cd "${DATA}/TROP" || exit 2
cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl

#
# Link data into DATA to sidestep gempak path limits
# TODO: Replace this
#
export COMIN="${RUN}.${PDY}${cyc}"
if [[ ! -L ${COMIN} ]]; then
    ln -sf "${COM_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi

mdl=gfs
MDL=GFS
metatype="trop"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"
#
for domain in ATL PAC WPAC; do
    case ${domain} in
    ATL)
        garea="-6;-111;52;-14"
        proj="MER/0.0;-49.5;0.0"
        ;;
    PAC)
        garea="0;-140;45;-75"
        proj="mer//3;3;0;1"
        ;;
    WPAC)
        garea="0;90;45;180"
        proj="mer//3;3;0;1"
        ;;
    *)
        echo "FATAL ERROR: Unknown domain in ${BASH_SOURCE[0]}"
        exit 100
    esac

    export pgm=gdplot2_nc;. prep_step
    "${GEMEXE}/gdplot2_nc" << EOF
GDFILE	= F-${MDL} | ${PDY:2}/${cyc}00
GDATTIM = F00-F180-12
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP	= 1
CLEAR	= yes
CLRBAR  = 1
FILTER  = yes

PROJ	= ${proj}
GAREA   = ${garea}
LATLON	= 1//1/1/10

GLEVEL  = 9950!0
GVCORD  = sgma!none
SKIP    = 0
SCALE   = 0
GDPFUN  = mag(kntv(wnd))!sm9s(pmsl)!kntv(wnd@9950%sgma)
TYPE    = c/f           !c         !b
CINT    = 5/20!2
LINE    = 32/1/2/2!19//2
FINT    = 20;35;50;65
FLINE   = 0;24;25;30;15
HILO    = 0!20/H#;L#/1020-1060;880-1012///1
HLSYM   = 0!1;1//22;22/3;3/hw
CLRBAR  = 1/V/LL!0
WIND    = bk0!bk0!bk9/.8/1.4/112
REFVEC  =
TITLE   = 1/-2/~ ? ${MDL} PMSL, BL WIND (40m AGL; KTS)|~${domain} PMSL & BL WIND!0
r

GLEVEL  = 850
GVCORD  = pres
SKIP    = 0
GDPFUN  = vor(wnd)              !vor(wnd)!kntv(wnd)
CINT    = 2/-99/-2              !2/2/99
LINE    = 29/5/1/2              !7/5/1/2
HILO    = 2;6/X;N/-99--4;4-99   !
SCALE   = 5                     !5
WIND    = bk0                   !bk0     !bk6/.8/2/112!0
TITLE   = 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~${domain} @ WIND AND REL VORT!0
FINT    = 4;6;8;10;12;14;16;18
FLINE   = 0;14-21
TYPE    = c/f!c!b

GLEVEL  = 850                !850      !0         !850
GVCORD  = pres               !pres     !none      !pres
GDPFUN  = vor(wnd)           !vor(wnd) !sm9s(pmsl)!kntv(wnd)
TYPE    = c/f                !c        !c         !b
CINT    = 2/-99/-2           !2/2/99   !2//1008
LINE    = 29/5/1/2           !7/5/1/2  !6/1/1
HILO    = 2;6/X;N/-99--4;4-99!         !6/L#/880-1004///1
HLSYM   = 1;1//22;22/3;3/hw
SCALE   = 5                  !5        !0
WIND    = bk0                !bk0      !bk0       !bk9/.8/1.4/112
TITLE   = 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~${domain} @ WIND AND REL VORT!0
FINT    = 4;6;8;10;12;14;16;18
FLINE   = 0;14-21
r

GLEVEL	= 700
GDPFUN	= vor(wnd)              !vor(wnd)!kntv(wnd)
TITLE	= 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~${domain} @ WIND AND REL VORT!0

GLEVEL  = 700!700!0!700
GVCORD  = pres!pres!none!pres
GDPFUN  = vor(wnd)           !vor(wnd) !sm9s(pmsl)!kntv(wnd)
CINT    = 2/-99/-2           !2/2/99   !2//1008
LINE    = 29/5/1/2           !7/5/1/2  !6//1
HILO    = 2;6/X;N/-99--4;4-99!         !6/L#/880-1004///1
HLSYM   = 1;1//22;22/3;3/hw
SCALE   = 5                  !5        !0
WIND    = bk0                !bk0      !bk0       !bk9/.8/1.4/112
TITLE   = 1/-2/~ ? ${MDL} @ WIND AND REL VORT|~${domain} @ WIND AND REL VORT!0
FINT    = 4;6;8;10;12;14;16;18
FLINE   = 0;14-21
TYPE    = c/f                !c        !c         !b
r

GLEVEL  = 500
GVCORD  = PRES
SKIP    = 0                  !0       !0                  !0        !0
SCALE   = 5                  !5       !5                  !5        !-1
GDPFUN  = (avor(wnd))//v     !v       !mul(v,-1)          !mul(v,-1)!sm5s(hght) !kntv(wnd)
TYPE    = c/f                !c       !c/f                !c        !c     !b
CINT    = 2/10/99            !2/4/8   !2/10/99            !2/4/8    !3
LINE    = 7/5/1/2      ! 29/5/1/2  ! 7/5/1/2   ! 29/5/1/2 ! 20/1/2/1
FINT    = 16;20;24;28;32;36;40;44
FLINE   = 0;23-15
HILO    = 2;6/X;N/10-99;10-99!        !2;6/X;N/10-99;10-99!         !
HLSYM   =
CLRBAR  = 1
WIND    = bk0!bk0!bk0!bk0!bk0!bk9/0.9/1.4/112!0
REFVEC  =
TITLE   = 1/-2/~ ? ${MDL} @ WIND AND ABS VORT|~${domain} @ WIND AND ABS VORT!0
r

GLEVEL  = 300:850       !850      !300
GVCORD  = pres          !pres     !pres
SKIP    = 0             !0/2;2    !0/2;2
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
WIND    = ak0!ak7/.4/1/221/.2!ak6/.4/1/221/.2
REFVEC  =
TITLE   = 1/-2/~ ? ${MDL} @  WIND SHEAR (850=Purple, 300=Cyan) |~${domain} 850-300MB WIND SHEAR!0
filter  = no


glevel  = 250!250
gvcord  = pres!pres
skip    = 0
filter	= yes
scale   = 0!0!5!5
gdpfun  = mag(kntv(wnd))//jet!jet!div(wnd)//dvg!dvg!kntv(wnd)
type    = c                  !c/f!c/f          !c  !b
cint    = 30;50!70;90;110;130;150;170;190!-11;-9;-7;-5;-3!2/3/18
line    = 26!32//2!19/-2//2!20
fint    = !70;90;110;130;150;170;190!3;5;7;9;11;13!
fline   = !0;24;25;29;7;15;14;2!0;23;22;21;17;16;2!
hilo    = 0!0!0!0
hlsym   = 0!0!0!0
clrbar  = 0!0!1/V/LL!0
wind    = bk0!bk0!bk0!bk0!bk9/.9/1.4/112
refvec  = 10
title   = 1/-2/~ ? ${MDL} @ HGHTS, ISOTACHS, & DIVERG|~${domain} @ SPEED & DIVERG!0
r

glevel  = 400:850!0
gvcord  = pres!none
scale   = 0
gdpfun  = squo(2,vadd(vlav(wnd@850:700%pres,vlav(wnd@500:400%pres)!sm9s(pmsl)
type    = b                                                       !c
cint    = 0!4
line    = 0!20//3
SKIP    = 0
fint    =
fline   =
hilo    = 0!26;2/H#;L#/1020-1070;900-1012//30;30/y
hlsym   = 0!2;1.5//21//hw
clrbar  = 0
wind    = bk10/0.9/1.4/112!bk0
refvec  =
title   = 1/-2/~ ? ${MDL} 850-400mb MLW and MSLP|~${domain} 850-400mb MLW & MSLP!0


GDATTIM	= F24-F144-06
GLEVEL	= 0
GVCORD	= none
SKIP	= 0
SCALE	= 0
GDPFUN	= p24i
TYPE	= f
CINT	= 0
LINE	= 0
FINT	= .01;.1;.25;.5;.75;1;1.25;1.5;1.75;2;2.25;2.5;2.75;3;3.25;3.5;3.75;4
FLINE	= 0;21-30;14-20;5
HILO	= 31;0/x#2/.10-8.0///y
HLSYM	= 1.4//22/2/hw
CLRBAR	= 1/V/LL
WIND	=
REFVEC	=
TITLE	= 1/-2/~ ${MDL} 24-HR TOTAL PCPN|~${domain} 24-HR TOTAL PCPN!0
r

exit
EOF
    export err=$?;err_chk

done

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
if (( err != 0 )) || [[ ! -s "${metaname}" ]] &> /dev/null; then
    echo "FATAL ERROR: Failed to create gempak meta file ${metaname}"
    exit $(( err + 100 ))
fi

mv "${metaname}" "${COM_ATMOS_GEMPAK_META}/${mdl}_${PDY}_${cyc}_${metatype}"
if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
        "${COM_ATMOS_GEMPAK_META}/${mdl}_${PDY}_${cyc}_${metatype}"
    if [[ "${DBN_ALERT_TYPE}" == "GFS_METAFILE_LAST" ]] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
            "${COM_ATMOS_GEMPAK_META}/${mdl}_${PDY}_${cyc}_${metatype}"
    fi
fi

exit

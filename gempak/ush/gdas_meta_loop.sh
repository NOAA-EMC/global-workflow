#! /usr/bin/env bash
#
# Metafile Script : gdas_meta_loop
#

source "${HOMEgfs}/ush/preamble.sh"

device="nc | gdasloop.meta"

#
# Link data into DATA to sidestep gempak path limits
# TODO: Replace this
#
export COMIN="${RUN}.${PDY}${cyc}"
if [[ ! -L "${COMIN}" ]]; then
    ${NLN} "${COMIN_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi

if [[ "${envir}" == "para" ]] ; then
   export m_title="GDASP"
else
   export m_title="GDAS"
fi

export pgm=gdplot2_nc;. prep_step

for (( fhr=24; fhr<=144; fhr+=24 )); do
    day=$(date --utc +%Y%m%d -d "${PDY} ${cyc} - ${fhr} hours")
    if (( ${day}${cyc} < SDATE )); then
        # Stop looking because these cycles weren't run
        if (( fhr == 24 )); then
            exit
        else
            break
        fi
    fi

    cycles=$(seq -s ' ' -f "%02g" 0 6 "${cyc}")
    for cycle in ${cycles}; do
        #  Test with GDAS in PROD
        YMD=${day} HH=${cyc} GRID=1p00 declare_from_tmpl "COMIN_ATMOS_GEMPAK_1p00_past:COM_ATMOS_GEMPAK_TMPL"
        export COMIN="${RUN}.${day}${cycle}"
        if [[ ! -L "${COMIN}" ]]; then
            ${NLN} "${COMIN_ATMOS_GEMPAK_1p00_past}" "${COMIN}"
        fi
        gdfile="${COMIN}/gdas_1p00_${day}${cycle}f000"

        "${GEMEXE}/gdplot2_nc" << EOF
\$MAPFIL = mepowo.gsf
GDFILE	= ${gdfile}
GDATTIM	= F000
DEVICE	= ${device}
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
TITLE   = 1/0/~ ${m_title} PW, EST MSLP, THICKNESS|~NAM PRCP WATER!0
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
TITLE   = 5/-2/~ ${m_title} @ HGT AND VORTICITY|~NAM @ HGT AND VORT!0
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
TITLE	= 5/-2/~ ${m_title} @ HGHT, ISOTACHS AND WIND (KTS)|~NAM @ HGT & WIND!0
FILTER  = n
r

exit
EOF

        gdfile="${COMIN}/gdas_1p00_${day}${cycle}f000"

"${GEMEXE}/gdplot2_nc" << EOF
\$MAPFIL = mepowo.gsf
GDFILE	= ${gdfile}
GDATTIM	= F000
DEVICE	= ${device}
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
TITLE   = 1/0/~ ${m_title} PW, MSLP, THICKNESS|~SAM PRCP WATER!0
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
TITLE   = 5/-2/~ ${m_title} @ HGT AND VORTICITY|~SAM @ HGT & VORT!0
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
TITLE	= 5/-2/~ ${m_title} @ HGHT, ISOTACHS AND WIND (KTS)|~SAM @ HGT & WIND!0
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
TITLE   = 1/-1/~ ${m_title} PMSL, 1000-850mb THKN|~SAM PMSL, 1000-850 TK!0
r

exit
EOF

    done
done

export err=$?

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
if (( err != 0 )) || [[ ! -s gdasloop.meta ]]; then
    echo "FATAL ERROR: Failed to create gdasloop meta file"
    exit "${err}"
fi

mv gdasloop.meta "${COMOUT_ATMOS_GEMPAK_META}/gdas_${PDY}_${cyc}_loop"
export err=$?
if (( err != 0 )) ; then
    echo "FATAL ERROR: Failed to move meta file to ${COMOUT_ATMOS_GEMPAK_META}/gdas_${PDY}_${cyc}_loop"
    exit "${err}"
fi

if [[ ${SENDDBN} == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
        "${COMOUT_ATMOS_GEMPAK_META}/gdas_${PDY}_${cyc}_loop"
fi

exit

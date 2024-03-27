#! /usr/bin/env bash
#
# Metafile Script : gfs_meta_usext.sh
#

source "${HOMEgfs}/ush/preamble.sh"

mkdir -p -m 775 "${DATA}/mrfus"
cd "${DATA}/mrfus" || exit 2
cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl
cp "${HOMEgfs}/gempak/fix/ak_sfstns.tbl" alaska.tbl

#
# Link data into DATA to sidestep gempak path limits
# TODO: Replace this
#
export COMIN="${RUN}.${PDY}${cyc}"
if [[ ! -L ${COMIN} ]]; then
    ln -sf "${COM_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi

device="nc | mrf.meta"

# Not being used?
# case $(( 10#${PDY:4:2} )) in
#     [5-9])
#         fint="60;65;70;75;80;85;90;95;100;105;110;115;120"
#         fline="26;25;24;23;22;21;20;19;18;17;16;15;14;31"
#         ;;
#     *)
#         fint="-5;0;5;10;15;20;25;30;35;40;45;50;55;60;65;70;75;80"
#         fline="4;30;29;28;27;26;25;24;23;22;21;20;19;18;17;16;15;14;31"
#         ;;
# esac

if [[ "${envir}" = "para" ]] ; then
   export m_title="GFSP"
else
   export m_title="GFS"
fi

export pgm=gdplot2_nc; prep_step

"${GEMEXE}/gdplot2_nc" << EOF
GDFILE	= F-GFS | ${PDY:2}/${cyc}00
GDATTIM	= F000-F384-06
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP     = 1
CLEAR	= yes
CLRBAR  = 1

GAREA	= 17.529;-129.296;53.771;-22.374
PROJ	= str/90;-105;0
LATLON	= 18/2

restore ${HOMEgfs}/gempak/ush/restore/pmsl_thkn.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? ${m_title} PMSL, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
run

restore ${HOMEgfs}/gempak/ush/restore/850mb_hght_tmpc.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? ${m_title} @ HGT, TEMPERATURE AND WIND (KTS)|~@ HGT, TMP, WIND!0
l
run

restore ${HOMEgfs}/gempak/ush/restore/700mb_hght_relh_omeg.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? ${m_title} @ HGT, REL HUMIDITY AND OMEGA|~@ HGT, RH AND OMEGA!0
l
run

restore ${HOMEgfs}/gempak/ush/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? ${m_title} @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
l
run

restore ${HOMEgfs}/gempak/ush/restore/500mb_hght_gabsv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? ${m_title} @ HGT AND GEO ABS VORT|~@ HGT, GEO ABS VORT!0
l
run

restore ${HOMEgfs}/gempak/ush/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 1/-2/~ ? ${m_title} @ HGT, ISOTACHS AND WIND (KTS)|~@ HGT AND WIND!0
l
run

GLEVEL  = 9950
GVCORD  = SGMA
PANEL   = 0
SKIP    = 0
SCALE   = 0
GDPFUN  = mag(kntv(wnd)) !kntv(wnd)
TYPE    = c/f            !b
CONTUR  = 2
CINT    = 20;30;40
LINE    = 27/4/2/1
FINT    = 20;30;40
FLINE   = 0;25;24;29
HILO    =
HLSYM   =
CLRBAR  = 1/V/LL!0
WIND    = bk18/0.8/1/112
REFVEC  =
TITLE   = 1/-2/~ ? ${m_title} BOUNDARY LAYER WINDS (KTS) AND ISOTACHS|~BL WIND, ISOTACHS !0
TEXT    = 1/21//hw
CLEAR   = YES
l
run

restore ${HOMEgfs}/gempak/ush/restore/precip.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
HILO    = 31;0/x#2/.25-10///y
HLSYM   = 1.5
GDATTIM	= F12-F384-6
GDPFUN  = p12i
TITLE	= 1/-2/~ ? ${m_title} 12-HR TOTAL PCPN (IN)|~12-HR TOTAL PCPN
l
run

GDATTIM	= F24-F384-6
GDPFUN  = p24i
TITLE	= 5/-2/~ ? ${m_title} 24-HR TOTAL PCPN (IN)|~24-HR TOTAL PCPN
l
run

GDATTIM	= F84
wind    = bk0
gvcord  = none
type    = f
cint    =
line    =
clrbar  = 1/V/LL
fint    = .01;.1;.25;.5;.75;1;1.5;2;2.5;3;4;5;6;7;8;9;10
fline   = 0;21-30;14-20;5
glevel  = 0
scale   = 0
gdpfun  = p72i
refvec  =
title   = 1/-2/~ ? ${m_title} 3-day (F12-F84) PCPN|~DAY 1-3 (F12-F84) PCPN
l
run

GDATTIM = F108
gdpfun  = p96i
title   = 1/-2/~ ? ${m_title} 4-day (F12-F120) PCPN|~DAY 1-4 (F12-F108) PCPN
l
run

GDATTIM = F132
gdpfun  = p120i
title   = 1/-2/~ ? ${m_title} 5-day (F12-F132) PCPN|~DAY 1-5 (F12-F132) PCPN
l
run

GDATTIM = F132
gdpfun  = p48i
title   = 1/-2/~ ? ${m_title} 2-day (F84-F132) PCPN|~DAY 4-5 (F84-F132) PCPN
l
run

! NEW

GDATTIM = F126
gdpfun  = p120i
title   = 1/-2/~ ? ${m_title} 5-day (F06-F126) PCPN|~DAY 1-5 (F06-F126) PCPN
l
run

! NEW

GDATTIM = F126
gdpfun  = p48i
title   = 1/-2/~ ? ${m_title} 2-day (F78-F126) PCPN|~DAY 4-5 (F78-F126) PCPN
l
run

! NEW

GDATTIM = F138
gdpfun  = p48i
title   = 1/-2/~ ? ${m_title} 2-day (F90-F138) PCPN|~DAY 4-5 (F90-F138) PCPN
l
run

GDATTIM = F156
gdpfun  = p72i
title   = 1/-2/~ ? ${m_title} 3-day (F84-F156) PCPN|~DAY 4-6 (F84-F156) PCPN
l
run

\$MAPFIL=hipowo.cia
GAREA   = 49;173;72;-122
PROJ    = mer//3;3;0;1
STNPLT  = 5|5/1//2|alaska.tbl
gdattim = f144
gdpfun  = p72i
title   = 1/-2/~ ? ${m_title} 3-day (F72-F144) PCPN|~AK 3-DAY(F72-F144) PCPN
l
run

STNPLT  =

exit
EOF
export err=$?; err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
if (( err != 0 )) || [[ ! -s mrf.meta ]] &> /dev/null; then
    echo "FATAL ERROR: Failed to create gempak meta file mrf.meta"
    exit $(( err + 100 ))
fi

mv mrf.meta "${COM_ATMOS_GEMPAK_META}/gfs_${PDY}_${cyc}_usext"
if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
        "${COM_ATMOS_GEMPAK_META}/gfs_${PDY}_${cyc}_usext"
    if [[ ${DBN_ALERT_TYPE} == "GFS_METAFILE_LAST" ]] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
            "${COM_ATMOS_GEMPAK_META}/gfs_${PDY}_${cyc}_usext"
    fi
fi

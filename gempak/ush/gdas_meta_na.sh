#! /usr/bin/env bash
#
# Metafile Script : gdas_meta_na
#

source "${HOMEgfs}/ush/preamble.sh"

device="nc | gdas.meta"

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

export pgm=gdplot2_nc; prep_step

"${GEMEXE}/gdplot2_nc" << EOF
GDFILE	= F-GDAS | ${PDY:2}/${cyc}00
GDATTIM	= FALL
DEVICE	= ${device}
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP     = 1
CLEAR	= yes
CLRBAR  = 1

!PROJ    = str/90;-95;0
!GAREA   = -5;-135;50;-5
!LATLON	= 0
GAREA   = 17.529;-129.296;53.771;-22.374
PROJ    = str/90;-105;0
LATLON  = 1


restore ${HOMEgfs}/gempak/ush/restore/pmsl_thkn.2.nts
CLRBAR  = 1
HLSYM   = 2;1.5//21//hw
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? ${m_title} PMSL, 1000-500 MB THICKNESS|~MSLP, 1000-500 THKN!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/850mb_hght_tmpc.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
SKIP    = 0         !0         !0         !0         !/3
FILTER  = NO
TITLE	= 5/-2/~ ? ${m_title} @ HGT, TEMP AND WIND (KTS)|~@ HGT, TMP, WIND!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/700mb_hght_relh_omeg.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? ${m_title} @ HGT, REL HUMIDITY AND OMEGA|~@ HGT, RH AND OMEGA!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? ${m_title} @ HGT AND VORTICITY|~@ HGT AND VORTICITY!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE	= 5/-2/~ ? ${m_title} @ HGT, ISOTACHS AND WIND (KTS)|~@ HGT AND WIND!0
l
ru

exit
EOF
export err=$?

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
if (( err != 0 )) || [[ ! -s gdas.meta ]] &> /dev/null; then
    echo "FATAL ERROR: Failed to create gempak meta file for North America"
    exit "${err}"
fi

mv gdas.meta "${COMOUT_ATMOS_GEMPAK_META}/gdas_${PDY}_${cyc}_na"
export err=$?
if (( err != 0 )) ; then
    echo "FATAL ERROR: Failed to move meta file to ${COMOUT_ATMOS_GEMPAK_META}/gdas_${PDY}_${cyc}_na"
    exit "${err}"
fi

if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
        "${COMOUT_ATMOS_GEMPAK_META}/gdas_${PDY}_${cyc}_na"
fi

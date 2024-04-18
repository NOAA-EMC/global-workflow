#! /usr/bin/env bash
#
# Metafile Script : mrf_meta_nhsh
#

source "${HOMEgfs}/ush/preamble.sh"

mkdir -p -m 775 "${DATA}/mrfnhsh"
cd "${DATA}/mrfnhsh" || exit 2
cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl

#
# Link data into DATA to sidestep gempak path limits
# TODO: Replace this
#
export COMIN="${RUN}.${PDY}${cyc}"
if [[ ! -L ${COMIN} ]]; then
    ln -sf "${COM_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi

if [[ "${envir}" == "para" ]] ; then
   export m_title="GFSP"
else
   export m_title="GFS"
fi

export pgm=gdplot2_nc; prep_step

"${GEMEXE}/gdplot2_nc" << EOF
\$MAPFIL=mepowo.gsf
GDFILE	= F-GFS | ${PDY:2}/${cyc}00
GDATTIM	= F000-F384-12
DEVICE	= nc | Nmeta_nh
PANEL	= 0
TEXT	= 1/21//hw
CONTUR	= 2
MAP	= 1
CLEAR	= yes
CLRBAR  = 1

restore ${HOMEgfs}/gempak/ush/restore/garea_nh.nts

restore ${HOMEgfs}/gempak/ush/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
SKIP	= 0                  !0                  !1
SCALE	= 5                  !5                  !-1
GFUNC	= (avor(wnd))//v     !mul(v,-1)          !hght
CTYPE	= c/f                !c/f                !c
CINT	= 2/10/99            !2/10/99            !6
LINE	= 7/5/1/2            ! 7/5/1/2           ! 20/1/2/1
FINT	= 16;20;24;28;32;36;40;44
HILO	= 2;6/X;N/10-99;10-99!2;6/X;N/10-99;10-99!
TITLE	= 5//~ ? @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!
TEXT	= 1/21//hw
CLEAR	= yes

TITLE	= 5//~ ? ${m_title} @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/garea_sh.nts

DEVICE	= nc | Nmeta_sh
TITLE	= 5//~ ? ${m_title} @ HEIGHTS AND VORTICITY|~ @ HGHT AND VORTICITY!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/garea_nh.nts
DEVICE	= nc | Nmeta_nh

restore ${HOMEgfs}/gempak/ush/restore/250mb_hght_wnd.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
GDPFUN  = knts((mag(wnd)))            !sm9s(hght)
TITLE	= 5/-2/~ ? ${m_title} @ HEIGHTS, ISOTACHS AND WIND (KTS)|~ @ HGHT AND WIND!0
l
ru


restore ${HOMEgfs}/gempak/ush/restore/garea_sh.nts
DEVICE	= nc | Nmeta_sh
ru

restore ${HOMEgfs}/gempak/ush/restore/precip.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
GDATTIM = F12-F240-12
GDPFUN   = (quo(mul(pr12,43200),25.4))
GDPFUN   = (quo(p12m,25.4))
TITLE   = 5//~ ? ${m_title} 12-HOUR TOTAL PRECIPITATION (IN)|~ 12-HOURLY TOTAL PCPN
l
r

restore ${HOMEgfs}/gempak/ush/restore/garea_sh.nts
DEVICE	= nc | Nmeta_sh
ru

exit
EOF
export err=$?; err_chk

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
for metaname in Nmeta_nh Nmeta_sh; do
    if (( err != 0 )) || [[ ! -s "${metaname}" ]] &> /dev/null; then
        echo "FATAL ERROR: Failed to create gempak meta file ${metaname}"
        exit $(( err + 100 ))
    fi

    mv "${metaname}" "${COM_ATMOS_GEMPAK_META}/gfs_${PDY}_${cyc}_${metaname/Nmeta_}"
    if [[ "${SENDDBN}" == "YES" ]] ; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
            "${COM_ATMOS_GEMPAK_META}/gfs_${PDY}_${cyc}_${metaname/Nmeta_}"
        if [[ ${DBN_ALERT_TYPE} = "GFS_METAFILE_LAST" ]] ; then
            DBN_ALERT_TYPE=GFS_METAFILE
            "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
                "${COM_ATMOS_GEMPAK_META}/gfs_${PDY}_${cyc}_${metaname/Nmeta_}"
        fi
    fi
done

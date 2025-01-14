#! /usr/bin/env bash

#########################################################################
#
#   This scripts creates GEMPAK .gif images of forecast fields from
#   GFS model output for archiving at NCDC.
#
#########################################################################

source "${HOMEgfs}/ush/preamble.sh"

LATVAL="1/1/1/1/5;5"
pixels="1728;1472"
cp "${HOMEgfs}/gempak/fix/coltbl.spc" coltbl.xwp

##########################################################
#                   FORECAST CHARTS                      #
##########################################################


# Create time stamp (bottom) label

echo "0${fhr3}${PDY}${cyc}" > dates
export FORT55="title.output"
"${HOMEgfs}/exec/webtitle.x" < dates

TITLE="$(cat title.output)"
echo "TITLE = ${TITLE}"

# Define labels and file names for forecast charts
hgtvor500lab="500MB ${fhr3}HR FORECAST  HEIGHTS/VORTICITY"
hgtvor500dev="gfs_500_hgt_vor_nh_f${fhr3}_${cyc}.gif"

hgtvor500usdev="gfs_500_hgt_vor_uscan_f${fhr3}_${cyc}.gif"

mslpthksfclab="${fhr3}HR FORECAST  MEAN SEA LEVEL PRESSURE/1000-500MB THICKNESS"
mslpthksfcdev="gfs_sfc_mslp_thk_nh_f${fhr3}_${cyc}.gif"

rhvvel700lab="700MB ${fhr3}HR FORECAST  RH/VERT VEL"
rhvvel700dev="gfs_700_rh_vvel_nh_f${fhr3}_${cyc}.gif"


# Set grid date and input file name
gdattim="${PDY:2:6}/${cyc}00F${fhr3}"
gdfile=gem_grids${fhr3}.gem

#  Execute the GEMPAK program

"${GEMEXE}/gdplot2_gif" << EOF


! ANALYSIS MSLP/1000-500 THICKNESS

  restore ${NTS}/base_nh.nts
  restore ${NTS}/sfc_mslp_thk.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${mslpthksfcdev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  MAP     = 0
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${mslpthksfclab}
  l
  r


! 500MB ANALYSIS  HEIGHTS/VORTICITY

  restore ${NTS}/base_nh.nts
  restore ${NTS}/500_hgt_vor.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtvor500dev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtvor500lab}
  l
  r


! 500MB ANALYSIS  HEIGHTS/VORTICITY (US/CANADA)

  restore ${NTS}/base_uscan.nts
  restore ${NTS}/500_hgt_vor.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtvor500usdev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtvor500lab}
  l
  r


! ANALYSIS 700MB RELATIVE HUMIDITY AND VERTICAL VELOCITY

  restore ${NTS}/base_nh.nts
  restore ${NTS}/700_rel_vvel.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${rhvvel700dev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${rhvvel700lab}
  l
  r

  exit
EOF

"${GEMEXE}/gpend"

# Copy the GIF images into my area

cp "${mslpthksfcdev}"   "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtvor500dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtvor500usdev}"  "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${rhvvel700dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"

# Copy the GIF images onto the NCDC area on the public ftp server

if [[ "${SENDDBN}" == YES ]]; then
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${mslpthksfcdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtvor500dev}"
  #  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtvor500usdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${rhvvel700dev}"
fi

echo "GEMPAK_GIF ${fhr3} hour completed normally"

exit

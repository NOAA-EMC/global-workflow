#! /usr/bin/env bash

#########################################################################
#
#   Script:  gempak_gfs_f00_gif.sh
#
#   This scripts creates GEMPAK .gif images of 00HR/Analysis fields from
#   GFS model output for archiving at NCDC.
#
#
#   History:   Ralph Jones     02/16/2005   JIF original version.
#   History:   Steve Lilly     04/30/2008   Change font size of the Titles
#                                           from .8 to a larger size (1 or 2)
#
#
#########################################################################

source "${HOMEgfs}/ush/preamble.sh"

LATVAL="1/1/1/1/5;5"
pixels="1728;1472"
cp "${HOMEgfs}/gempak/fix/coltbl.spc" coltbl.xwp

#################################################################
#                       ANALYSIS CHARTS                         #
#################################################################


# Create time stamp (bottom) label

echo "0000${PDY}${cyc}" > dates
export FORT55="title.output"
"${HOMEgfs}/exec/webtitle.x" < dates
TITLE="$(cat title.output)"
echo "TITLE = ${TITLE}"

# Define labels and file names for analysis charts

hgttmp700lab="700MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp700dev="gfs_700_hgt_tmp_nh_anl_${cyc}.gif"

hgttmp500lab="500MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp500dev="gfs_500_hgt_tmp_nh_anl_${cyc}.gif"

hgtiso300lab="300MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso300dev="gfs_300_hgt_iso_nh_anl_${cyc}.gif"

hgtiso250lab="250MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso250dev="gfs_250_hgt_iso_nh_anl_${cyc}.gif"

hgttmp250lab="250MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp250dev="gfs_250_hgt_tmp_nh_anl_${cyc}.gif"

hgtiso200lab="200MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso200dev="gfs_200_hgt_iso_nh_anl_${cyc}.gif"

# Not being used?
# hgttmp200lab="200MB ANALYSIS  HEIGHTS/TEMPERATURE"
# hgttmp200dev="gfs_200_hgt_tmp_nh_anl_${cyc}.gif"

hgtiso100lab="100MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso100dev="gfs_100_hgt_iso_nh_anl_${cyc}.gif"

hgttmp100lab="100MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp100dev="gfs_100_hgt_tmp_nh_anl_${cyc}.gif"

hgtvor500lab="500MB ANALYSIS  HEIGHTS/VORTICITY"
hgtvor500dev="gfs_500_hgt_vor_nh_anl_${cyc}.gif"

hgtvor500usdev="gfs_500_hgt_vor_uscan_anl_${cyc}.gif"

mslpthksfclab="ANALYSIS  MEAN SEA LEVEL PRESSURE/1000-500MB THICKNESS"
mslpthksfcdev="gfs_sfc_mslp_thk_nh_anl_${cyc}.gif"

mslpthksfcusdev="gfs_sfc_mslp_thk_uscan_anl_${cyc}.gif"

rhvvel700lab="700MB ANALYSIS  RH/VERT VEL"
rhvvel700dev="gfs_700_rh_vvel_nh_anl_${cyc}.gif"

liftlab="ANALYSIS  LIFTED INDEX"
liftdev="gfs_lift_nh_anl_${cyc}.gif"

prswshtroplab="TROPOPAUSE PRESSURE/WIND SHEAR"
prswshtropdev="gfs_trop_prs_wsh_nh_anl_${cyc}.gif"

# Set grid date and input file name

gdattim=${PDY:2:6}/${cyc}00F000
gdfile=gem_grids${fhr3}.gem

#  Execute the GEMPAK program

"${GEMEXE}/gdplot2_gif" << EOF

! 700MB HEIGHTS/TEMPERATURES

  restore ${NTS}/base_nh.nts
  restore ${NTS}/700_hgt_tmp.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp700dev} | ${pixels}
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

  TITLE   = 1/3/${hgttmp700lab}
  l
  r


! 500MB HEIGHTS/TEMPERATURES

  restore ${NTS}/base_nh.nts
  restore ${NTS}/500_hgt_tmp.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp500dev} | ${pixels}
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

  TITLE   = 1/3/${hgttmp500lab}
  l
  r


! 300MB HEIGHTS/ISOTACHS

  restore ${NTS}/base_nh.nts
  restore ${NTS}/300_hgt_iso.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtiso300dev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = 1/1/1/1/5;5             !
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso300lab}
  l
  r


! 250MB HEIGHTS/TEMPERATURES

  restore ${NTS}/base_nh.nts
  restore ${NTS}/250_hgt_tmp.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp250dev} | ${pixels}
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

  TITLE   = 1/3/${hgttmp250lab}
  l
  r


! 250MB ANALYSIS HEIGHTS/ISOTACHS

  restore ${NTS}/base_nh.nts
  restore ${NTS}/250_hgt_iso.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtiso250dev} | ${pixels}
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

  TITLE   = 1/3/${hgtiso250lab}
  l
  r


! 200MB HEIGHTS/ISOTACHS

  restore ${NTS}/base_nh.nts
  restore ${NTS}/200_hgt_iso.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtiso200dev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = 1/1/1/1/5;5             !
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso200lab}
  l
  r


! 100MB HEIGHTS/TEMPERATURES

  restore ${NTS}/base_nh.nts
  restore ${NTS}/100_hgt_tmp.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp100dev} | ${pixels}
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

  TITLE   = 1/3/${hgttmp100lab}
  l
  r


! 100MB HEIGHTS/ISOTACHS

  restore ${NTS}/base_nh.nts
  restore ${NTS}/100_hgt_iso.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtiso100dev} | ${pixels}
  TITLE   =
  TEXT    = 1/2/2/c/sw
  LATLON  = 1/1/1/1/5;5             !
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/2/2/c/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso100lab}
  l
  r


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


! ANALYSIS MSLP/1000-500 THICKNESS (US/CANADA)

  restore ${NTS}/base_uscan.nts
  restore ${NTS}/sfc_mslp_thk.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${mslpthksfcusdev} | ${pixels}
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
  MAP     = 0
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

  TITLE   = 1/3/${hgtvor500lab}
  l
  r


! ANALYSIS  LIFTED INDEX

  restore ${NTS}/base_nh.nts
  restore ${NTS}/100_lift.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${liftdev} | ${pixels}
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

  TITLE   = 1/3/${liftlab}
  l
  r


! ANALYSIS  TROPOPAUSE PRESSURE/WIND SHEAR

  restore ${NTS}/base_nh.nts
  restore ${NTS}/trop_pres_wshr.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${prswshtropdev} | ${pixels}
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

  TITLE   = 1/3/${prswshtroplab}
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
cp "${hgttmp700dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgttmp500dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtiso300dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtiso250dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgttmp250dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtiso200dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtiso100dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgttmp100dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${mslpthksfcdev}"   "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${mslpthksfcusdev}" "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtvor500dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${hgtvor500usdev}"  "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${liftdev}"         "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${prswshtropdev}"   "${COMOUT_ATMOS_GEMPAK_GIF}"
cp "${rhvvel700dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}"

# Copy the GIF images onto the NCDC area on the public ftp server

if [[ "${SENDDBN}" == "YES" ]]; then
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp700dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp500dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso300dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso250dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp250dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso200dev}"
# "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp200dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso100dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp100dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${mslpthksfcdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${mslpthksfcusdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtvor500dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtvor500usdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${liftdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${prswshtropdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${rhvvel700dev}"



fi

# Convert the 500mb NH Hgts/Temps chart to tif, attach a heading and
#   send to TOC via the NTC

export input=${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp500dev}
export HEADER=YES
export OUTPATH=${DATA}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif
"${USHgfs}/make_tif.sh"

exit

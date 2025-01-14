#! /usr/bin/env bash

#########################################################################
#
#   This scripts creates GEMPAK .gif images of 00HR/Analysis fields from
#   GDAS model output for archiving at NCDC.
#
#########################################################################

source "${HOMEgfs}/ush/preamble.sh"

LATVAL="1/1/1/1/5;5"
LATSOUTH="1/1/1/1;4/5;5"
pixels="1728;1472"

cp "${HOMEgfs}/gempak/fix/coltbl.spc" coltbl.xwp

#################################################################
#              NORTHERN HEMISPHERE ANALYSIS CHARTS              #
#################################################################

# Create time stamp (bottom) label

echo "0000${PDY}${cyc}" > dates
export FORT55="title.output"
"${HOMEgfs}/exec/webtitle.x" < dates

TITLE="$(cat title.output)"
echo "TITLE = ${TITLE}"

# Define labels and file names for Northern Hemisphere analysis charts

hgttmp850lab="850MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp850dev="gdas_850_hgt_tmp_nh_anl_${cyc}.gif"

hgttmp700lab="700MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp700dev="gdas_700_hgt_tmp_nh_anl_${cyc}.gif"

hgttmp500lab="500MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp500dev="gdas_500_hgt_tmp_nh_anl_${cyc}.gif"

hgtiso300lab="300MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso300dev="gdas_300_hgt_iso_nh_anl_${cyc}.gif"

hgtiso250lab="250MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso250dev="gdas_250_hgt_iso_nh_anl_${cyc}.gif"

hgtiso200lab="200MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso200dev="gdas_200_hgt_iso_nh_anl_${cyc}.gif"

mslpthksfclab="ANALYSIS  MEAN SEA LEVEL PRESSURE/1000-500MB THICKNESS"
mslpthksfcdev="gdas_sfc_mslp_thk_nh_anl_${cyc}.gif"


# Set grid date and input file name

gdattim="${PDY:2:6}/${cyc}00F000"
gdfile=gem_grids${fhr3}.gem


#  Execute the GEMPAK program

"${GEMEXE}/gdplot2_gif" << EOF


! 850MB HEIGHTS/TEMPERATURES

  restore ${NTS}/base_nh.nts
  restore ${NTS}/850_hgt_tmp.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp850dev} | ${pixels}
  TITLE   =
  TEXT    = 1/3/2/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgttmp850lab}
  l
  r


! 700MB HEIGHTS/TEMPERATURES

  restore ${NTS}/base_nh.nts
  restore ${NTS}/700_hgt_tmp.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp700dev} | ${pixels}
  TITLE   =
  TEXT    = 1/3/2/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
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
  TEXT    = 1/3/2/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
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
  TEXT    = 1/3/2/sw
  LATLON  = 1/1/1/1/5;5             !
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso300lab}
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
  TEXT    = 1/3/2/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  MAP     = 0
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
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
  TEXT    = 1/3/2/sw
  LATLON  = 1/1/1/1/5;5             !
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso200lab}
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
  TEXT    = 1/3/2/sw
  LATLON  = ${LATVAL}
  l
  r

  CLEAR   = no
  MAP     = 0
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${mslpthksfclab}
  l
  r

  exit
EOF

"${GEMEXE}/gpend"

# Copy the GIF images into my area

cp "${hgttmp850dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgttmp700dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgttmp500dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgtiso300dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgtiso250dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgtiso200dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${mslpthksfcdev}"   "${COMOUT_ATMOS_GEMPAK_GIF}/."

# Send the GIF images onto the NCDC area on the public ftp server

if [[ ${SENDDBN} == YES ]]; then
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp850dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp700dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp500dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso300dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso250dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso200dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${mslpthksfcdev}"
fi


##########################################################
#         SOUTHERN HEMISPHERE ANALYSIS CHARTS            #
##########################################################

mslpthksfclab="ANALYSIS  MEAN SEA LEVEL PRESSURE/1000-500MB THICKNESS"
mslpthksfcdev="gdas_sfc_mslp_thk_sh_anl_${cyc}.gif"

hgttmp500lab="500MB ANALYSIS  HEIGHTS/TEMPERATURE"
hgttmp500dev="gdas_500_hgt_tmp_sh_anl_${cyc}.gif"

hgtiso300lab="300MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso300dev="gdas_300_hgt_iso_sh_anl_${cyc}.gif"

hgtiso250lab="250MB ANALYSIS  HEIGHTS/ISOTACHS"
hgtiso250dev="gdas_250_hgt_iso_sh_anl_${cyc}.gif"


#  Execute the GEMPAK program

"${GEMEXE}/gdplot2_gif" << EOF


! ANALYSIS MSLP/1000-500 THICKNESS

  restore ${NTS}/base_sh.nts
  restore ${NTS}/sfc_mslp_thk.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${mslpthksfcdev} | ${pixels}
  TITLE   =
  TEXT    = 1/3/2/sw
  LATLON  = ${LATSOUTH}
  l
  r

  CLEAR   = no
  MAP     = 0
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${mslpthksfclab}
  l
  r


! 500MB ANALYSIS  HEIGHTS/TEMPERATURES

  restore ${NTS}/base_sh.nts
  restore ${NTS}/500_hgt_tmp.nts


  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgttmp500dev} | ${pixels}
  TITLE   =
  TEXT    = 1/3/2/sw
  LATLON  = ${LATSOUTH}
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgttmp500lab}
  l
  r


! 300MB HEIGHTS/ISOTACHS

  restore ${NTS}/base_sh.nts
  restore ${NTS}/300_hgt_iso.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtiso300dev} | ${pixels}
  TITLE   =
  TEXT    = 1/3/2/sw
  LATLON  = ${LATSOUTH}               !
  l
  r

  CLEAR   = no
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso300lab}
  l
  r


! 250MB ANALYSIS HEIGHTS/ISOTACHS

  restore ${NTS}/base_sh.nts
  restore ${NTS}/250_hgt_iso.nts

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtiso250dev} | ${pixels}
  TITLE   =
  TEXT    = 1/3/2/sw
  LATLON  = ${LATSOUTH}
  l
  r

  CLEAR   = no
  MAP     = 0
  GDPFUN  =
  TITLE   = 1/-4/${TITLE}
  TEXT    = 2/3/2/sw
  LATLON  = 0
  l
  r

  TITLE   = 1/3/${hgtiso250lab}
  l
  r

  exit
EOF


"${GEMEXE}/gpend"


# Copy the GIF images into my area
cp "${mslpthksfcdev}"   "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgttmp500dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgtiso300dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."
cp "${hgtiso250dev}"    "${COMOUT_ATMOS_GEMPAK_GIF}/."

# Copy the GIF images onto the NCDC area on the public ftp server

if [[ ${SENDDBN} == YES ]]; then
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${mslpthksfcdev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgttmp500dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso300dev}"
  "${DBNROOT}/bin/dbn_alert" MODEL NCDCGIF "${job}" "${COMOUT_ATMOS_GEMPAK_GIF}/${hgtiso250dev}"
fi

exit

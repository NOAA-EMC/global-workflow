#!/bin/sh


#########################################################################
#
#   Script:  gempak_gfs_f24_gif.sh
#
#   This scripts creates GEMPAK .gif images of 24HR forecast fields from
#   GFS model output for archiving at NCDC.
#
#
#   History:   Ralph Jones     02/16/2005   JIF original version.
#   History:   Steve Lilly     04/30/2008   Change font size of the Titles
#                                           from .8 to a larger size (1 or 2)
#
#
#########################################################################



   msg=" Make GEMPAK GIFS utility"
   postmsg "$jlogfile" "$msg"



  set -x


  MAPAREA="normal"

  LATVAL="1/1/1/1/5;5"

  pixels="1728;1472"

  cp $FIXgempak/coltbl.spc coltbl.xwp


 
##########################################################
#                24HR FORECAST CHARTS                    #
##########################################################


# Create time stamp (bottom) label

  echo 00${fhr}${PDY}${cyc} > dates
  export FORT55="title.output"
#  $WEBTITLE < dates
 ${UTILgfs}/exec/webtitle < dates

  export TITLE=`cat title.output`
  echo "\n\n TITLE = $TITLE \n"


# Define labels and file names for 24hr forecast charts

  hgtvor500lab="500MB ${fhr}HR FORECAST  HEIGHTS/VORTICITY"
  hgtvor500dev="gfs_500_hgt_vor_nh_f${fhr}_${cyc}.gif"

  hgtvor500usdev="gfs_500_hgt_vor_uscan_f${fhr}_${cyc}.gif"

  mslpthksfclab="${fhr}HR FORECAST  MEAN SEA LEVEL PRESSURE/1000-500MB THICKNESS"
  mslpthksfcdev="gfs_sfc_mslp_thk_nh_f${fhr}_${cyc}.gif"

  rhvvel700lab="700MB ${fhr}HR FORECAST  RH/VERT VEL"
  rhvvel700dev="gfs_700_rh_vvel_nh_f${fhr}_${cyc}.gif"


# Set grid date and input file name

  gdattim=`echo ${PDY} | cut -c3-8`/${cyc}00F0${fhr}
  gdfile=gem_grids${fhr}.gem



#  Execute the GEMPAK program

  $GEMEXE/gdplot2_gif << EOF


! ANALYSIS MSLP/1000-500 THICKNESS

  restore $NTS/base_nh
  restore $NTS/sfc_mslp_thk

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${mslpthksfcdev} | $pixels
  TITLE   =
  TEXT    = 1/2/2/c/sw 
  LATLON  = $LATVAL
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

  restore $NTS/base_nh
  restore $NTS/500_hgt_vor

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtvor500dev} | $pixels
  TITLE   =
  TEXT    = 1/2/2/c/sw 
  LATLON  = $LATVAL
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

  restore $NTS/base_uscan
  restore $NTS/500_hgt_vor

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${hgtvor500usdev} | $pixels
  TITLE   =
  TEXT    = 1/2/2/c/sw 
  LATLON  = $LATVAL
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

  restore $NTS/base_nh
  restore $NTS/700_rel_vvel

  CLEAR   = yes
  GDFILE  = ${gdfile}
  GDATTIM = ${gdattim}
  MAP     = 1
  DEVICE  = gif | ${rhvvel700dev} | $pixels
  TITLE   =
  TEXT    = 1/2/2/c/sw 
  LATLON  = $LATVAL
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


$GEMEXE/gpend


if [ $SENDCOM = YES ]; then

# Copy the GIF images into my area

  cp ${mslpthksfcdev}   ${COMOUT}
  cp ${hgtvor500dev}    ${COMOUT}
  cp ${hgtvor500usdev}  ${COMOUT}
  cp ${rhvvel700dev}    ${COMOUT}


# Copy the GIF images onto the NCDC area on the public ftp server

 if [ $SENDDBN = YES ]; then

  $DBNROOT/bin/dbn_alert MODEL NCDCGIF ${job} ${COMOUT}/${mslpthksfcdev}
  $DBNROOT/bin/dbn_alert MODEL NCDCGIF ${job} ${COMOUT}/${hgtvor500dev}
#  $DBNROOT/bin/dbn_alert MODEL NCDCGIF ${job} ${COMOUT}/${hgtvor500usdev}
  $DBNROOT/bin/dbn_alert MODEL NCDCGIF ${job} ${COMOUT}/${rhvvel700dev}

 fi

fi


   msg=" GEMPAK_GIF ${fhr} hour completed normally"
   postmsg "$jlogfile" "$msg"

   exit

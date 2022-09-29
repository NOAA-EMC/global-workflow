#!/bin/sh

cd $DATA
#
#     Use Image Magick system module to convert the GIF to TIF
#        format
#

  outname=out.tif                            

  convert gif:$input fax:$outname                            

#
#  Add the ntc heading: 
# 

WMO=QTUA11
ORIG=KWBC
PDYHH=${PDY}${cyc}

if [ $HEADER = "YES" ] 
then
   INPATH=$DATA/$outname
   SUB=DFAX1064       
#   make_NTC_file.pl $WMO $ORIG $PDYHH $SUB $INPATH $OUTPATH   
   $UTILgfs/ush/make_NTC_file.pl $WMO $ORIG $PDYHH $SUB $INPATH $OUTPATH   
#
#  Send the graphic to TOC

   cp $OUTPATH ${COMOUTwmo}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif  
 if [ $SENDDBN = YES ]; then

   $DBNROOT/bin/dbn_alert GRIB_LOW ${NET} ${job} ${COMOUTwmo}/gfs_500_hgt_tmp_nh_anl_${cyc}.tif
 fi
fi


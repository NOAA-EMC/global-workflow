#!/bin/sh

cd $DATA
#
#     Use Image Magick to convert the GIF to TIF    
#        format
#
# module show  imagemagick-intel-sandybridge/6.8.3    on   CRAY
# export PATH=$PATH:/usrx/local/prod/imagemagick/6.8.3/intel/sandybridge/bin:.
# export LIBPATH="$LIBPATH":/usrx/local/prod/imagemagick/6.8.3/intel/sandybridge/lib
# export DELEGATE_PATH=/usrx/local/prod/imagemagick/6.8.3/intel/sandybridge/share/ImageMagick-6

# module show  imagemagick/6.9.9-25      on  DELL
  export PATH=$PATH:/usrx/local/dev/packages/ImageMagick/6.9.9-25/bin:.
  export LIBPATH="$LIBPATH":/usrx/local/dev/packages/ImageMagick/6.9.9-25/lib
  export DELEGATE_PATH=/usrx/local/dev/packages/ImageMagick/6.9.9-25/share/ImageMagick-6

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


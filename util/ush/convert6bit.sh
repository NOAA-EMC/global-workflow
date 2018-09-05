#!/bin/ksh

set -x

  cd $DATA

#    convert6bit.sh
#
#    Convert fax graphics to another format.  Start by 
#      getting the conversion parameters 
#      from the fax.tbl
#  
#    History:
#    Oct 2005   L. Sager    New script
#    Feb 2016 SPA   - convert into util_shared module
#
   if [ $toc != "WAF" ]
   then
#     grep $KEYW $FIXshared/convertfax.tbl  |  read keyword width height FMAT WMO ORIG HEADER ROTATE
     grep $KEYW $UTILgfs/fix/convertfax.tbl  |  read keyword width height FMAT WMO ORIG HEADER ROTATE
   else
     echo "DO not used convertfax.tbl"
   fi

  echo keyword is $keyword 
  echo width is $width
  echo height is $height
  echo FMAT is $FMAT
  echo WMO is $WMO
  echo ORIG is $ORIG
  echo HEADER is $HEADER
  echo ROTATE is $ROTATE


#      extract a pure raster graphic using fxcompoz
#

  ras=ras.out
  INFILEcrd=crd.out
  parmsg="ssno="$subnn

  export FORT66="$ras"
  export FORT67="$INFILEcrd"    # out card

#  $FXCOMPOZ $INFILE parm=$parmsg > /dev/null
  $UTILgfs/exec/fxcompoz $INFILE parm=$parmsg > /dev/null

#
#     Use Image Magick to convert the Raster to another 
#        format
#
# module show  imagemagick-intel-sandybridge/6.8.3  on CRAY
#  export PATH=$PATH:/usrx/local/prod/imagemagick/6.8.3/intel/sandybridge/bin:.
#  export LIBPATH="$LIBPATH":/usrx/local/prod/imagemagick/6.8.3/intel/sandybridge/lib
#  export DELEGATE_PATH=/usrx/local/prod/imagemagick/6.8.3/intel/sandybridge/share/ImageMagick-6

# module show  imagemagick/6.9.9-25      on  DELL
  export PATH=$PATH:/usrx/local/dev/packages/ImageMagick/6.9.9-25/bin:.
  export LIBPATH="$LIBPATH":/usrx/local/dev/packages/ImageMagick/6.9.9-25/lib
  export DELEGATE_PATH=/usrx/local/dev/packages/ImageMagick/6.9.9-25/share/ImageMagick-6

  echo P4 > header
  echo # NCEPtest >> header
  echo $width '   '  $height >> header
  cat header $ras  >  image

  rotat="" 
  if [ $ROTATE != "NO" ] 
  then
     rotat="-rotate $ROTATE"
  fi

  echo rotat is $rotat
  
  convert $rotat PBM:image  $FMAT:$outname

#
#  Add the ntc heading: 
# 

if [ $HEADER = "YES" ] 
then
   INPATH=$DATA/$outname
   SUB=DFAX1064       
#   make_NTC_file.pl $WMO $ORIG $PDYHH $SUB $INPATH $OUTPATH   
   $UTILgfs/ush/make_NTC_file.pl $WMO $ORIG $PDYHH $SUB $INPATH $OUTPATH   
fi

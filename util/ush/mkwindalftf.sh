#!/bin/ksh
# UTILITY SCRIPT NAME : mkwindalftf.sh
#              AUTHOR : Mary Jacobs
#                       
# ABSTRACT : NOW PRODUCE UPPER AIR WIND PLOTS
#
#   SCRIPT TO RUN WNDANFTF HDS REPLACEMENT
#   The script runs a bufr extraction
#   program to extract soundings from
#   an gfs PREPBUFR file and
#   write them as a FORMATTED HUMAN
#   READABLE REPORT.  (rsonde.x).  It
#   then runs an NCAR graphics program to
#   plot wind barbs and temperatures
#   (wndanftf.x).  
#   This generates a cgm metafile which
#   is rasterized (raster2bit.x),
#   prefixed with a CNTR standard header
#   (glue.head.x), and packed to
#    the NOAA bedient sixbit format
#   demanded by OSO.  (sixbitb.x)
#   Author:    George VandenBerghe Cray Research.
#   HISTORY:   1/23/97 first written.
#   HISTORY:   9/12/05 Remove snd2forgn
#   HISTORY:  10/05/05 Converted fax graphics to T4
#                      format and stopped writing to
#                      the status file.
#
set -x

###########################################################
#  Extract significant Height observations
###########################################################
cp $COMIN/gfs.${cycle}.prepbufr gfs.${cycle}.prepbufr
  export err=$?
  if [[ $err -ne 0 ]] ; then
    echo " File .${cycle}.prepbufr gfs.${cycle}.prepbufr does not exist."
    exit $err
  fi

export pgm=rsonde
. prep_step

export FORT11=gfs.${cycle}.prepbufr 
export FORT51=f51Z                    

startmsg
# $RSONDE < $PARMshared/graph_rsonde.ft05.Z >> $pgmout 2> errfile
${UTILgfs}/exec/rsonde < ${UTILgfs}/parm/graph_rsonde.ft05.Z >> $pgmout 2> errfile
export err=$?;err_chk

###########################################################
#  Extract significant pressure observations
###########################################################

export pgm=rsonde
. prep_step

export FORT11=gfs.${cycle}.prepbufr    
export FORT51=f51P                       

startmsg
# $RSONDE <$PARMshared/graph_rsonde.ft05.P >> $pgmout 2>errfile 
${UTILgfs}/exec/rsonde < ${UTILgfs}/parm/graph_rsonde.ft05.P >> $pgmout 2> errfile
export err=$?;err_chk

###########################################################
# cat the oservation files together
###########################################################

cat f51Z f51P >formatted.soundings

###########################################################
# draw ncar graphics map
###########################################################

export pgm=wndanftf
. prep_step

export FORT20=formatted.soundings 

startmsg
# $WNDANFTF >> $pgmout  2> errfile
${UTILgfs}/exec/wndanftf >> $pgmout  2> errfile
export err=$?;err_chk

###########################################################
# rasterize the map
###########################################################

ictrans -d xwd  -fdn 2 -resolution 1728x2440  -e 'zoom 0.08 0.0 0.79  1.0' -e 'plot 1' gmeta >f8
###########################################################
# pack 8 bit pixels into one bit bits 
###########################################################

export pgm=ras2bit
. prep_step

export FORT11=f8     
export FORT59=f59  

fssize=`cat f8 | wc -c `
echo $fssize  >fin
echo 1728 >>fin
echo 2440 >>fin

startmsg

# $RAS2BIT <fin  >> $pgmout 2> errfile
${UTILgfs}/exec/ras2bit <fin  >> $pgmout 2> errfile
export err=$?;err_chk
###########################################################
# Glue the header onto the image file by getting rid of HGLUE
###########################################################

# cat $FIXshared/graph_wndanftf.header f59 > mapback.pur
cat ${UTILgfs}/fix/graph_wndanftf.header f59 > mapback.pur
cp mapback.pur image002.pur
cp mapback.pur image001.pur

###########################################################
# now we have bit image background field (in mapback.pur)  which
# actually contains the ENTIRE MAP and we
# merely need "cntr" to do the six bit bedient
# packing.
# A second image, (duplicate but for alternative
# disposition with a different subset number )
# is in image002.pur. 
###########################################################

export pgm=sixbitb
. prep_step
 
jobn=${jobn:-$job}
FAXOUT=bkwndalf.${cyc}

export FORT11="mapback.pur"               
export FORT12="mapback.pur"               
# export FORT15=$FIXshared/graph_sixbitb.generic.f15      
export FORT15=${UTILgfs}/fix/graph_sixbitb.generic.f15      
# export FORT18=$FIXshared/graph_sixbitb.wndanftf.$cycle 
export FORT18=${UTILgfs}/fix/graph_sixbitb.wndanftf.$cycle 
export FORT52=x6b             
export FORT55=putlab.55      
export FORT60=f60                   
export FORT61=f61                  
export FORT62=f62                 
export FORT63=f63                
export FORT71=ras               
export FORT72=rs2              
# the 6 bit output is assigned here
export FORT81="bkwndalf.${cyc}"             
rm fort.80

startmsg
# $SIXBITB >> $pgmout 2> errfile
${UTILgfs}/exec/sixbitb >> $pgmout 2> errfile

export err=$?;err_chk

 for KEYW in WNDAFT WNDAFT_g
 do

#  grep $KEYW $FIXshared/identifyfax.tbl | read Keyword sub00 sub06 sub12 sub18 gif toc prt lprt name
 grep $KEYW ${UTILgfs}/fix/identifyfax.tbl | read Keyword sub00 sub06 sub12 sub18 gif toc prt lprt name

 if [ ${cyc} = '00' ]; then submn=$sub00; fi
 if [ ${cyc} = '12' ]; then submn=$sub12; fi

 echo $FAXOUT $submn $name $Keyword $gif $toc $prt $jobn $lprt
 export FAXOUT submn name Keyword gif toc prt jobn lprt
# mk_graphics.sh
 ${UTILgfs}/ush/mk_graphics.sh

 done

exit

#!/bin/sh
set -x
CDATE=2008030100
CDUMP=gdas
DMPDIR=/global/shared
COMDMP='$DMPDIR/$CDATE/${CDUMP}c,$DMPDIR/$CDATE/${CDUMP},$DMPDIR/$CDATE/${CDUMP}x'
#for cdm in $(eval echo $COMDMP|tr , ' ') ; do if [ -s $cdm/tmp*godas* ] ; then echo 'file exists' ; fi done
for cdm in $(eval echo $COMDMP|tr , ' ') ; do
 echo $cdm
 if [ -s $cdm/tmp*godas* ] ; then
   nn=$(echo $cdm | tr / ' ' | wc -w)
   xx=$(echo $cdm | tr / ' ' | eval awk \'{print \$$nn}\')
   if [ $xx != $CDUMP ] ; then
    nc=$(echo $xx | wc -c)
    cc=$(echo $xx | cut -c$((nc-1)))
   fi
   break
 fi
done

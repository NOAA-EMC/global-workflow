#!/bin/sh
#########################################################################
#									#
# Script:  gfs_bfr2gpk							#
#									#
#  This script reads GFS BUFR output and transfers it into GEMPAK	#
#  surface and sounding data files.					#
#									#
# Log:									#
# K. Brill/HPC		04/12/05					#
#########################################################################  
set -x

# Set GEMPAK paths.

#. /nwprod/gempak/.gempak

#  Go to a working directory.

cd $DATA

#  Set input directory name.

#BPATH=$COMIN/bufr.t${cyc}z
BPATH=$COMOUT/bufr.t${cyc}z
export BPATH

#  Set output directory:

COMAWP=${COMAWP:-$COMROOT/nawips/${envir}/${RUN}.${PDY}}
OUTDIR=$COMAWP
mkdir -p $OUTDIR

outfilbase=gfs_${PDY}${cyc}

#  Get the list of individual station files.

date
##filelist=`/bin/ls -1 $BPATH | grep bufr`
##rm -f bufr.combined
##for file in $filelist; do
##  cat $BPATH/$file >> bufr.combined
##done
  cat $BPATH/bufr.*.${PDY}${cyc} > bufr.combined
date
namsnd << EOF > /dev/null
SNBUFR   = bufr.combined
SNOUTF   = ${outfilbase}.snd
SFOUTF   = ${outfilbase}.sfc
SNPRMF   = sngfs.prm
SFPRMF   = sfgfs.prm
TIMSTN   = 170/2100
r

ex
EOF
date

/bin/rm *.nts

snd=${outfilbase}.snd
sfc=${outfilbase}.sfc
cp $snd $OUTDIR/.$snd
cp $sfc $OUTDIR/.$sfc
mv $OUTDIR/.$snd $OUTDIR/$snd
mv $OUTDIR/.$sfc $OUTDIR/$sfc

if [ $SENDDBN = "YES" ]
then
   $DBNROOT/bin/dbn_alert MODEL GFS_PTYP_SFC $job $OUTDIR/$sfc
   $DBNROOT/bin/dbn_alert MODEL GFS_PTYP_SND $job $OUTDIR/$snd
fi
echo done > $DATA/gembufr.done

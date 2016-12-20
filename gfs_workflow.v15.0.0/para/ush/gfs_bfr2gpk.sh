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

. /nwprod/gempak/.gempak

#  Go to a working directory.

cd $DATA

#  Set input directory name.

#BPATH=$COMIN/bufr.t${cyc}z
BPATH=$COMOUT/bufr.t${cyc}z
export BPATH

#  Set output directory:

#COMAWP=/com/nawips/${envir}/${RUN}.${PDY}
COMAWP=$COMOUT/nawips
OUTDIR=$COMAWP
mkdir -p $OUTDIR

outfilbase=gfs_${PDY}${cyc}

#  Get the list of individual station files.

filelist=`/bin/ls -1 $BPATH | grep bufr`
for file in $filelist; do
/nwprod/ush/cwordsh block $BPATH/$file $BPATH/$file.block
mv $BPATH/$file.block $BPATH/$file
    namsnd << EOF > /dev/null
SNBUFR   = \$BPATH/$file
SNOUTF   = ${outfilbase}.snd
SFOUTF   = ${outfilbase}.sfc
SNPRMF   = sngfs.prm
SFPRMF   = sfgfs.prm
TIMSTN   = 128/1800
r

ex
EOF
done

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

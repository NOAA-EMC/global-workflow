#!/bin/ksh
################################################################################
# This script runs the data preprocessing in the GDAS cycle.
# Usage: prep.sh
# Imported variables:
#   CONFIG
#   CDATE
#   CDUMP
#   CSTEP
# Configuration variables:
#   DATATMP
#   COMDMP
#   COMROT
#   NCP
#   NDATE
#   MAKEPREPBUFRSH
#   PREPROCESSSH
#   PBEG
#   PERR
#   PEND
################################################################################
set -ux

################################################################################
# Go configure

set -a;. $CONFIG;set +a
export CKSH=$(echo $CSTEP|cut -c-4)
export CKND=$(echo $CSTEP|cut -c5-)
export machine=${machine:-WCOSS}
machine=$(echo $machine|tr '[a-z]' '[A-Z]')
eval export DATA=$DATATMP
cd;rm -rf $DATA||exit 1;mkdir -p $DATA||exit 1;cd $DATA||exit 1
#chgrp ${group_name:-rstprod} $DATA
chmod ${permission:-755} $DATA
export CHGRP_CMD=${CHGRP_CMD:-"chgrp ${group_name:-rstprod}"}
export APRNPREP=${APRNPREP:-""}
export APRNRELOC=${APRNRELOC:-""}
export APRNGETTX=${APRNGETTX:-""}
#
export BASEDIR=${BASEDIR:-/nwprod}
export SHDIR=${SHDIR:-$BASEDIR/bin}
export NWPROD=${NWPROD:-$BASEDIR}
export PARMSUBDP=${PARMSUBDP:-parm/parm_prep}
export FIXSUBDP=${FIXSUBDP:-fix/fix_prep}
export GRIBVERSION=${GRIBVERSION:-'grib2'}
#
export PBEG=${PBEG:-$SHDIR/pbeg}
export PEND=${PEND:-$SHDIR/pend}
export PERR=${PERR:-$SHDIR/perr}
$PBEG

################################################################################
# Set other variables

export PCOP=${PCOP:-$SHDIR/pcop}
export NDATE=${NDATE:-$NWPROD/util/exec/ndate}
export MAKEPREPBUFRSH=${MAKEPREPBUFRSH:-$NWPROD/ush/prepobs_makeprepbufr.sh}
export RELOCATESH=${RELOCATESH:-$NWPROD/ush/tropcy_relocate.sh}
export DO_RELOCATE=${DO_RELOCATE:-YES}
export RESTORE_GES=${RESTORE_GES:-NO}
export PROCESS_TROPCY=${PROCESS_TROPCY:-YES}
export QCTROPCYSH=${QCTROPCYSH:-$NWPROD/ush/syndat_qctropcy.sh}
export XLF_LINKSSH=${XLF_LINKSSH:-$NWPROD/util/ush/xlf_links.sh}
export CYINC=${CYINC:-06}
export GDATE=$($NDATE -$CYINC $CDATE)
export HDATE=$($NDATE -$CYINC $GDATE)
export CDFNL=${CDFNL:-fnl}
export GDUMP=${GDUMP:-$CDFNL}
export NET=gdas
export RUN=gdas1
export envir=prod
export COMSP=${DATA}/
export SENDCOM=${SENDCOM:-YES}
export SENDDBN=${SENDDBN:-NO}

if [ $machine = IBMP6 ] ; then
 export NPROCS=$(($(echo $LOADL_PROCESSOR_LIST|wc -w)+0))
elif [ $machine = WCOSS ] ; then
 export NPROCS=${NPROCS_PREP:-8}
 export BACK=YES
elif [ $machine = WCOSS_C ]; then
 export NPROCS=${NPROCS_PREP:-8}
 export BACK=YES
 . $MODULESHOME/init/sh
 module load cfp-intel-sandybridge
else
 export NPROCS=${NPROCS_PREP:-8}
fi
export NSPLIT=${NSPLIT:-8}
export l4densvar=${l4densvar:-".false."}

#############################
# Set up the UTILITIES
##############################
export ushscript=${USHGLOBAL:-$NWPROD/ush}
export utilscript=${USHUTIL:-$NWPROD/util/ush}
export utilities=${USHUTIL:-$NWPROD/util/ush}
export jlogfile=${jlogfile:-""}

export BUFRLIST=${BUFRLIST:-"adpupa proflr aircar aircft satwnd adpsfc sfcshp\
                  vadwnd wdsatr ascatw rassda gspipw sfcbog goesnd spssmi erscat qkswnd"}
export pgmout=stdout
export tmmark=tm00
export cyc=$(echo $CDATE|cut -c9-10)
export DUMPROCESSSH=${DUMPROCESSSH:-"echo no special preproccessing"}
export PREPROCESSSH=${PREPROCESSSH:-"echo no special preproccessing"}
gcycl=$(echo $GDATE|cut -c9-10)
gdump=$(echo $GDUMP|tr '[a-z]' '[A-Z]')
export FHOUT_GES=$(eval echo \${FHOUTFCST$gcycl$gdump:-03}|cut -f1 -d,)
export LONA_P=${LONA_P:-$LONB}
export LATA_P=${LATA_P:-$LATB}
export LATA=${LATA_P:-$LATB}
export LONA=${LONA_P:-$LONB}
export COMDMPTMP=${COMDMPTMP:-$COMDMP}
eval export COMDMP=$COMDMPTMP
eval export COMDMPG=$(CDATE=$GDATE CDUMP=$GDUMP eval echo $COMDMPTMP)
eval export COMDMPH=$(CDATE=$HDATE CDUMP=$GDUMP eval echo $COMDMPTMP)

export PRPC=${PRPC:-'$NWPROD/parm/prepobs_prepdata.$CDUMP.parm'}
export PRPCTMP=${PRPCTMP:-$PRPC}
eval export PRPC=$PRPCTMP

################################################################################
# Copy in restart and input files

$PCOP $CDATE/$CDUMP/$CSTEP/ROTI $COMROT $DATA <$RLIST
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

$PCOP $CDATE/$CDUMP/$CSTEP/OPTI $COMROT $DATA <$RLIST

#if [ $l4densvar = .true. ]; then
  ln ${SIGOSUF}f03.$GDUMP.$GDATE sgm3prep
  ln ${SIGOSUF}f04.$GDUMP.$GDATE sgm2prep
  ln ${SIGOSUF}f05.$GDUMP.$GDATE sgm1prep
  ln ${SIGOSUF}f06.$GDUMP.$GDATE sgesprep
  ln ${SIGOSUF}f07.$GDUMP.$GDATE sgp1prep
  ln ${SIGOSUF}f08.$GDUMP.$GDATE sgp2prep
  ln ${SIGOSUF}f09.$GDUMP.$GDATE sgp3prep
#else
# ln ${SIGOSUF}f03.$GDUMP.$GDATE sgm3prep
# ln ${SIGOSUF}f06.$GDUMP.$GDATE sgesprep
# ln ${SIGOSUF}f09.$GDUMP.$GDATE sgp3prep
#fi
export pgb_typ4prep=${pgb_typ4prep:-$flag_pgb}

if [ $GRIBVERSION = grib2 ]; then
   export pgb_prefix="pgrb"
   export pgb_suffix=".grib2"
   export pgb_zero="0"
   if [ $pgb_typ4prep = m ]; then
     export pgb_zero=""
   fi
else
   export pgb_prefix="pgb"
   export pgb_suffix=""
   export pgb_zero=""
fi

#if [ $l4densvar = .true. ]; then
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}00.$GDUMP.$GDATE${pgb_suffix} pgm6prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}01.$GDUMP.$GDATE${pgb_suffix} pgm5prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}02.$GDUMP.$GDATE${pgb_suffix} pgm4prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}03.$GDUMP.$GDATE${pgb_suffix} pgm3prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}04.$GDUMP.$GDATE${pgb_suffix} pgm2prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}05.$GDUMP.$GDATE${pgb_suffix} pgm1prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}06.$GDUMP.$GDATE${pgb_suffix} pgesprep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}07.$GDUMP.$GDATE${pgb_suffix} pgp1prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}08.$GDUMP.$GDATE${pgb_suffix} pgp2prep
  ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}09.$GDUMP.$GDATE${pgb_suffix} pgp3prep
#else
# ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}00.$GDUMP.$GDATE${pgb_suffix} pgm6prep
# ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}03.$GDUMP.$GDATE${pgb_suffix} pgm3prep
# ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}06.$GDUMP.$GDATE${pgb_suffix} pgesprep
# ln ${pgb_prefix}${pgb_typ4prep}${pgb_zero}09.$GDUMP.$GDATE${pgb_suffix} pgp3prep
#fi

$PCOP $CDATE/$CDUMP/$CSTEP/DMPI $COMDMP $DATA <$RLIST
$PCOP $CDATE/$CDUMP/$CSTEP/DMPG $COMDMPG $DATA <$RLIST
$PCOP $CDATE/$CDUMP/$CSTEP/DMPH $COMDMPH $DATA <$RLIST
$DUMPROCESSSH
ln tcvitl.$GDUMP.$GDATE tcvitals.m6
ln tcvitl.$GDUMP.$HDATE tcvitals.m12
ln tcvitl.$CDUMP.$CDATE ${COMSP}syndata.tcvitals.${tmmark}
for name in $BUFRLIST;do
  eval eval fin=\${$(echo $name|tr '[a-z]' '[A-Z]')TMP:-$name.$CDUMP.$CDATE}
  ln -fs $fin ${COMSP}${name}.${tmmark}.bufr_d
done
>tcvitals
>rel_inform
export PREPDATA=${PREPDATA:-YES}  # Execute programs PREPOBS_MPCOPYBUFR, PREPOBS_PREPDATA,
[ $PREPDATA = YES -a ! -s ${COMSP}aircft.${tmmark}.bufr_d ]  && export PREPACQC=${PREPACQC:-NO}
#[[ -s ${COMSP}aircft.${tmmark}.bufr_d ]]||export PREPACQC=${PREPACQC:-NO}

################################################################################
# Run Tropical storm relocation

if [[ "$PROCESS_TROPCY" = 'YES' ]]; then
  $QCTROPCYSH $CDATE
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
fi

if [[ "$DO_RELOCATE" = 'YES' ]]; then
  export XLSMPOPTS='parthds=1:stack=256000000'
  $RELOCATESH $CDATE
rc=$?
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi

  [ ! -s ${COMSP}tropcy_relocation_status.$tmmark ]  &&  \
    echo "RECORDS PROCESSED" > ${COMSP}tropcy_relocation_status.$tmmark

fi

# If requested, restore unrelocated guess files. 
if [[ "$RESTORE_GES" = 'YES' ]]; then
  mv sgesprep ${SIGOSUF}ges.$CDUMP.$CDATE.relocate
  mv sgm3prep ${SIGOSUF}gm3.$CDUMP.$CDATE.relocate
  mv sgm2prep ${SIGOSUF}gm2.$CDUMP.$CDATE.relocate
  mv sgm1prep ${SIGOSUF}gm1.$CDUMP.$CDATE.relocate
  mv sgp1prep ${SIGOSUF}gp1.$CDUMP.$CDATE.relocate
  mv sgp2prep ${SIGOSUF}gp2.$CDUMP.$CDATE.relocate
  mv sgp3prep ${SIGOSUF}gp3.$CDUMP.$CDATE.relocate

  cp ${SIGOSUF}f03.$GDUMP.$GDATE sgm3prep
  cp ${SIGOSUF}f04.$GDUMP.$GDATE sgm2prep
  cp ${SIGOSUF}f05.$GDUMP.$GDATE sgm1prep
  cp ${SIGOSUF}f06.$GDUMP.$GDATE sgesprep
  cp ${SIGOSUF}f07.$GDUMP.$GDATE sgp1prep
  cp ${SIGOSUF}f08.$GDUMP.$GDATE sgp2prep
  cp ${SIGOSUF}f09.$GDUMP.$GDATE sgp3prep
fi

################################################################################
# Run MAKEPREPBUFR

export SYNDATA=${SYNDATA:-YES}    # Execute program SYNDAT_SYNDATA   if YES
export PREPACQC=${PREPACQC:-YES}  # Execute program PREPOBS_PREPACQC if YES
export PROCESS_ACQC=${PROCESS_ACQC:-YES} # Execute program PREPOBS_PREPACQC if YES
export PROCESS_ACPF=${PROCESS_ACPF:-NO}  # Execute program PREPOBS_PREPACPF if YES
export PROFCQC=${PROFCQC:-YES}    # Execute program PREPOBS_PROFCQC  if YES
export CQCVAD=${CQCVAD:-YES}      # Execute program PREPOBS_CQCVAD   if YES
export CQCBUFR=${CQCBUFR:-YES}    # Execute program PREPOBS_CQCBUFR  if YES
export OIQCBUFR=${OIQCBUFR:-YES}  # Execute program PREPOBS_OIQCBUFR if YES
export PREPDATA=${PREPDATA:-YES}  # Execute programs PREPOBS_MPCOPYBUFR, PREPOBS_PREPDATA,
                                  # PREPOBS_LISTHEADERS and PREPOBS_MONOPREPBUFR if YES
export GETGUESS=${GETGUESS:-YES}  # Encode first guess (background) values interpolated to
                                  # observation locations in the PREPBUFR file for use by
                                  # the q.c. programs.  This guess is always from a global
                                  # sigma guess file valid at the center PREPBUFR
                                  # processing date/time.
export DO_QC=${DO_QC:-YES}        # IF NO, programs PREPOBS_PREPACQC, PREPOBS_ACARSQC,
                                  # PREPOBS_PROFCQC, PREPOBS_CQCVAD, PREPOBS_CQCBUFR and
                                  # PREPOBS_OIQCBUFR will NEVER execute regardless of
                                  # switches above -
                                  # should be set to NO only as a last resort!!!

if [[ "$DO_MAKEPREPBUFR" = 'YES' ]]; then
  echo $(date) EXECUTING $MAKEPREPBUFRSH $CDATE >&2
  $MAKEPREPBUFRSH $CDATE
rc=$?
  echo $(date) EXITING $MAKEPREPBUFRSH with return code $rc >&2
  cat relocate.stdout*
  cat $pgmout
if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
fi

$PREPROCESSSH
if [ ! -s $BLENDED_ICE_FILE ] ; then
   eval BLENDED_ICE_FILEG='$COMROT/seaice.5min.blend.grb.$CDUMP.$GDATE'
   if [ -s $BLENDED_ICE_FILEG ] ; then
      echo "***WARNING***  copy $BLENDED_ICE_FILEG to $BLENDED_ICE_FILE"
      cp $BLENDED_ICE_FILEG $BLENDED_ICE_FILE
   fi
fi

################################################################################
# Combine sea temperature in situ observation data files to be a single one if nst_gsi > 0
if [[ $NST_GSI -gt 0 ]]; then
   export COMIN_DUMP=${COMIN_DUMP:-$COMDMP}

   export SFCSHPBF=${SFCSHPBF:-$COMIN_DUMP/sfcshp.$CDUMP.$CDATE}
   export TESACBF=${TESACBF:-$COMIN_DUMP/tesac.$CDUMP.$CDATE}
   export BATHYBF=${BATHYBF:-$COMIN_DUMP/bathy.$CDUMP.$CDATE}
   export TRKOBBF=${TRKOBBF:-$COMIN_DUMP/trkob.$CDUMP.$CDATE}
   export NSSTBF=${NSSTBF:-nsstbufr.$CDUMP.$CDATE}

   cat $SFCSHPBF $TESACBF $BATHYBF $TRKOBBF > $NSSTBF
fi
################################################################################
# Copy out restart and output files
  
cycle=t$(echo $CDATE|cut -c9-10)z

if [[ "$DO_MAKEPREPBUFR" = 'YES' ]]; then
  mv prepda.$cycle prepqc.$CDUMP.$CDATE
else
  mv $PREPBUFRFILE prepqc.$CDUMP.$CDATE
fi

chmod 640  prepqc.$CDUMP.$CDATE
$CHGRP_CMD prepqc.$CDUMP.$CDATE

mv prepbufr.acft_profiles prepbufr.acft_profiles.$CDUMP.$CDATE
chmod 640  prepbufr.acft_profiles.$CDUMP.$CDATE
$CHGRP_CMD prepbufr.acft_profiles.$CDUMP.$CDATE

mv sgesprep ${SIGOSUF}ges.$CDUMP.$CDATE
mv sgm3prep ${SIGOSUF}gm3.$CDUMP.$CDATE
mv sgm2prep ${SIGOSUF}gm2.$CDUMP.$CDATE
mv sgm1prep ${SIGOSUF}gm1.$CDUMP.$CDATE
mv sgp1prep ${SIGOSUF}gp1.$CDUMP.$CDATE
mv sgp2prep ${SIGOSUF}gp2.$CDUMP.$CDATE
mv sgp3prep ${SIGOSUF}gp3.$CDUMP.$CDATE
mv tcvitals tcvitals_relocate.$CDUMP.$CDATE
mv rel_inform1 tcinform_relocate.$CDUMP.$CDATE
mv cqc_events cqc_events.$CDUMP.$CDATE
mv cqc_stncnt cqc_stncnt.$CDUMP.$CDATE
mv cqc_stnlst cqc_stnlst.$CDUMP.$CDATE

$PCOP $CDATE/$CDUMP/$CSTEP/ROTO $DATA $COMROT <$RLIST
rc=$?

$PCOP $CDATE/$CDUMP/$CSTEP/OPTO $DATA $COMROT <$RLIST

################################################################################
# Exit gracefully

if [[ $rc -ne 0 ]];then $PERR;exit 1;fi
if [ ${KEEPDATA:-NO} != YES ] ; then rm -rf $DATA ; fi
$PEND

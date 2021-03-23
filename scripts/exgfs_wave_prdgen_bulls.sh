#!/bin/bash
###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the              #
#  WW3 wave model.                                                            #
#                                                                             #
# Remarks :                                                                   #
# - Supplemental error output is witten to the gfswave_prdgbulls.log file.    #
#                                                                             #
#                                                                             #
# Origination  : 05/02/2007                                                   #
# Last update  : 08/20/2020                                                   # 
#                                                                             #
# Aug/2020 RPadilla & JHAlves - Merging wave scripts to GFSv16 global workflow#
#                                                                             #
###############################################################################
# --------------------------------------------------------------------------- #
# 0.  Preparations
# 0.a Basic modes of operation
 set -xa
 # Use LOUD variable to turn on/off trace.  Defaults to YES (on).
 export LOUD=${LOUD:-YES}; [[ $LOUD = yes ]] && export LOUD=YES
 [[ "$LOUD" != YES ]] && set +x

# PATH for working and home directories
 export RUNwave=${RUNwave:-${RUN}${COMPONENT}}
 export envir=${envir:-ops}
 export cyc=${cyc:-00}
 export cycle=${cycle:-t${cyc}z}
 export pgmout=OUTPUT.$$
 export DATA=${DATA:-${DATAROOT:?}/${job}.$$}
 #export CODEwave=${CODEwave:-${NWROOT}/${NET}_code.${wave_code_ver}/${code_pkg}}
 export EXECwave=${EXECwave:-$HOMEgfs/exec}
 export FIXwave=${FIXwave:-$HOMEgfs/fix}
 export PARMwave=${PARMwave:-$HOMEgfs/parm/parm_wave}
 export USHwave=${USHwave:-$HOMEgfs/ush}
 #export EXECcode=${EXECcode:-CODEwave/exec}

 mkdir -p $DATA
 cd $DATA
 export wavelog=${DATA}/${RUNwave}_prdgbulls.log
 
 postmsg "$jlogfile" "HAS BEGUN on `hostname`"

 msg="Starting MWW3 BULLETINS PRODUCTS SCRIPT"
 postmsg "$jlogfile" "$msg"
 touch $wavelog
# 0.b Date and time stuff
 export date=$PDY
 export YMDH=${PDY}${cyc}
 set +x
 echo ' '
 echo '                  **************************************'
 echo '                  *** MWW3 BULLETINS PRODUCTS SCRIPT ***'
 echo '                  **************************************'
 echo "                                         $date $cycle"
 echo ' '
 echo "Starting at : `date`"
 echo ' '
 echo ' '
 [[ "$LOUD" = YES ]] && set -x

# 1.  Get necessary files
 set +x
 echo "   Copying bulletins from $COMIN"
 [[ "$LOUD" = YES ]] && set -x

# 1.a Link the input file and untar it
 BullIn=$COMIN/station/${RUNwave}.$cycle.cbull_tar
 if [ -f $BullIn ]; then
   cp $BullIn cbull.tar
 else
   msg="ABNORMAL EXIT: NO BULLETIN TAR FILE"
   postmsg "$jlogfile" "$msg"
   set +x
   echo ' '
   echo '************************************ '
   echo '*** ERROR : NO BULLETIN TAR FILE *** '
   echo '************************************ '
   echo ' '
   echo $msg
   [[ "$LOUD" = YES ]] && set -x
   msg="FATAL ERROR ${RUNwave} prdgen $date $cycle : bulletin tar missing."
   echo $msg >> $wavelog
   export err=1; ${errchk}
   exit $err
 fi

 set +x
 echo "   Untarring bulletins ..."
 [[ "$LOUD" = YES ]] && set -x
 tar -xf cbull.tar
 OK=$?

 if [ "$OK" = '0' ]; then
   set +x
   echo "      Unpacking successfull ..."
   [[ "$LOUD" = YES ]] && set -x
   rm -f cbull.tar
 else
   msg="ABNORMAL EXIT: ERROR IN BULLETIN UNTAR"
   postmsg "$jlogfile" "$msg"
   set +x
   echo ' '
   echo '****************************************** '
   echo '*** ERROR : ERROR IN BULLETIN TAR FILE *** '
   echo '****************************************** '
   echo ' '
   echo $msg
   [[ "$LOUD" = YES ]] && set -x
   echo "${RUNwave} prdgen $date $cycle : bulletin untar error." >> $wavelog
   err=2;export err;err_chk
   exit $err
 fi

# 1.b Output locations from bulletin files
 set +x
 echo ' Nb=`ls -1 *.cbull | wc -l`'
 Nb=`ls -1 *.cbull | wc -l`
 [[ "$LOUD" = YES ]] && set -x
  echo ' '
  echo "   Number of bulletin files :   $Nb"
  echo '   --------------------------'
  echo ' '
# 1.c Get the datat cards
 if [ -f $PARMwave/bull_awips_gfswave ]; then
   cp $PARMwave/bull_awips_gfswave awipsbull.data
 else
   msg="ABNORMAL EXIT: NO AWIPS BULLETIN HEADER DATA FILE"
   postmsg "$jlogfile" "$msg"
   set +x
   echo ' '
   echo '******************************************* '
   echo '*** ERROR : NO AWIPS BULLETIN DATA FILE *** '
   echo '******************************************* '
   echo ' '
   echo $msg
   [[ "$LOUD" = YES ]] && set -x
   echo "${RUNwave} prdgen $date $cycle : Bulletin header data file  missing." >> $wavelog
   err=3;export err;err_chk
   exit $err
 fi

# 2. AWIPS bulletins for output points
 echo ' '
 echo 'AWIPS bulletins ...'
 echo '-------------------'
 echo '   Sourcing data file with header info ...'

# 2.b Set up environment variables
 [[ "$LOUD" = YES ]] && set -x
 . awipsbull.data

# 2.c Generate list of bulletins to process
 echo '   Generating buoy list ...'
 echo 'bulls=`sed -e 's/export b//g' -e 's/=/ /' awipsbull.data | grep -v "#" |awk '{ print $1}'`'
 bulls=`sed -e 's/export b//g' -e 's/=/ /' awipsbull.data | grep -v "#" |awk '{ print $1}'`
  
# 2.d Looping over buoys running formbul
 echo '   Looping over buoys ... \n'

 for bull in $bulls; do
   fname="${RUNwave}.$bull.cbull"
   oname="awipsbull.$bull.$cycle.${RUNwave}"
   headr=`grep "b${bull}=" awipsbull.data | sed 's/=/ /g' |  awk '{ print $3}'`  
   echo "      Processing $bull ($headr $oname) ..." 
 
   if [ -z "$headr" ] || [ ! -s $fname ]; then
     [[ "$LOUD" = YES ]] && set -x
     msg="ABNORMAL EXIT: MISSING BULLETING INFO"
     postmsg "$jlogfile" "$msg"
     set +x
     echo ' '
     echo '******************************************** '
     echo '*** FATAL ERROR : MISSING BULLETING INFO *** '
     echo '******************************************** '
     echo ' '
     echo $msg
     [[ "$LOUD" = YES ]] && set -x
     echo "${RUNwave} prdgen $date $cycle : Missing bulletin data." >> $wavelog
     err=4;export err;err_chk
     exit $err
   fi
  
   [[ "$LOUD" = YES ]] && set -x
   
   formbul.pl -d $headr -f $fname -j $job -m ${RUNwave} \
              -p $PCOM -s NO -o $oname > formbul.out 2>&1
   OK=$?

   if [ "$OK" != '0' ] || [ ! -f $oname ]; then
     [[ "$LOUD" = YES ]] && set -x
     cat formbul.out
     msg="ABNORMAL EXIT: ERROR IN formbul"
     postmsg "$jlogfile" "$msg"
     set +x
     echo ' '
     echo '************************************** '
     echo '*** FATAL ERROR : ERROR IN formbul *** '
     echo '************************************** '
     echo ' '
     echo $msg
     [[ "$LOUD" = YES ]] && set -x
     echo "${RUNwave} prdgen $date $cycle : error in formbul." >> $wavelog
     err=5;export err;err_chk
     exit $err
   fi
   
   cat $oname >> awipsbull.$cycle.${RUNwave}

 done

# 3. Send output files to the proper destination
 [[ "$LOUD" = YES ]] && set -x
 if [ "$SENDCOM" = YES ]; then
   cp  awipsbull.$cycle.${RUNwave} $PCOM/awipsbull.$cycle.${RUNwave}
   if [ "$SENDDBN_NTC" = YES ]; then
     make_ntc_bull.pl  WMOBH NONE KWBC NONE $DATA/awipsbull.$cycle.${RUNwave} $PCOM/awipsbull.$cycle.${RUNwave}
   else
     if [ "${envir}" = "para" ] || [ "${envir}" = "test" ] || [ "${envir}" = "dev" ]; then
       echo "Making NTC bulletin for parallel environment, but do not alert."
       [[ "$LOUD" = YES ]] && set -x
       (export SENDDBN=NO; make_ntc_bull.pl  WMOBH NONE KWBC NONE \
               $DATA/awipsbull.$cycle.${RUNwave} $PCOM/awipsbull.$cycle.${RUNwave})
     fi
   fi
 fi

# --------------------------------------------------------------------------- #
# 4.  Clean up

  set +x; [[ "$LOUD" = YES ]] && set -v
  rm -f ${RUNwave}.*.cbull  awipsbull.data
  set +v

# --------------------------------------------------------------------------- #
# 5.  Ending output

  set +x
  echo ' '
  echo ' '
  echo "Ending at : `date`"
  echo ' '
  echo '                *** End of MWW3 BULLETINS product generation ***'
  echo ' '
  [[ "$LOUD" = YES ]] && set -x

  msg="$job completed normally"
  postmsg "$jlogfile" "$msg"

# End of MWW3 product generation script -------------------------------------- #

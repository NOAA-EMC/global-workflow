#! /usr/bin/env bash

###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the              #
#  WW3 wave model.                                                            #
#                                                                             #
# COM inputs:                                                                 #
#  - ${COMIN_WAVE_STATION}/${RUN}.wave.${cycle}.cbull_tar                     #
# COM outputs:                                                                #
#  - ${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave                        #
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

# PATH for working and home directories
 export envir=${envir:-ops}
 export cyc=${cyc:-00}
 export cycle=${cycle:-t${cyc}z}
 export pgmout=OUTPUT.$$
 export DATA=${DATA:-${DATAROOT:?}/${job}.$$}

# 0.b Date and time stuff
 export date=${PDY}
 export YMDH=${PDY}${cyc}
 set +x
 echo ' '
 echo '                  **************************************'
 echo '                  *** MWW3 BULLETINS PRODUCTS SCRIPT ***'
 echo '                  **************************************'
 echo "                                         ${date} ${cycle}"
 echo ' '
 echo "Starting at : $(date)"
 echo ' '
 echo ' '
 set_trace

# 1.  Get necessary files
 set +x
 echo "   Copying bulletins from ${COMIN_WAVE_STATION}"
 set_trace

# 1.a Link the input file and untar it
 BullIn="${COMIN_WAVE_STATION}/${RUN}.wave.${cycle}.cbull.tar"
 if [ -f "${BullIn}" ]; then
   cp "${BullIn}" "cbull.tar"
 else
   echo "ABNORMAL EXIT: NO BULLETIN TAR FILE"
   set +x
   echo ' '
   echo '************************************ '
   echo '*** ERROR : NO BULLETIN TAR FILE *** '
   echo '************************************ '
   echo ' '
   set_trace
   msg="FATAL ERROR ${RUN} wave prdgen ${date} ${cycle} : bulletin tar missing."
   echo "${msg}"
   export err=1; ${errchk}
   exit ${err}
 fi

 set +x
 echo "   Untarring bulletins ..."
 set_trace
 tar -xf cbull.tar
 OK=$?

 if [ "${OK}" = '0' ]; then
   set +x
   echo "      Unpacking successfull ..."
   set_trace
   rm -f cbull.tar
 else
   echo "ABNORMAL EXIT: ERROR IN BULLETIN UNTAR"
   set +x
   echo ' '
   echo '****************************************** '
   echo '*** ERROR : ERROR IN BULLETIN TAR FILE *** '
   echo '****************************************** '
   echo ' '
   set_trace
   echo "${RUN} wave prdgen ${date} ${cycle} : bulletin untar error."
   err=2;export err;err_chk
   exit ${err}
 fi

# 1.b Output locations from bulletin files
 set +x
 echo ' Nb=$(ls -1 *.cbull | wc -l)'
 Nb=$(ls -1 *.cbull | wc -l)
 set_trace
  echo ' '
  echo "   Number of bulletin files :   ${Nb}"
  echo '   --------------------------'
  echo ' '
# 1.c Get the datat cards
 if [ -f "${PARMgfs}/wave/bull_awips_gfswave" ]; then
   cp "${PARMgfs}/wave/bull_awips_gfswave" "awipsbull.data"
 else
   msg="ABNORMAL EXIT: NO AWIPS BULLETIN HEADER DATA FILE"
   set +x
   echo ' '
   echo '******************************************* '
   echo '*** ERROR : NO AWIPS BULLETIN DATA FILE *** '
   echo '******************************************* '
   echo ' '
   echo "${msg}"
   set_trace
   echo "${RUN} wave prdgen ${date} ${cycle} : Bulletin header data file missing."
   err=3;export err;err_chk
   exit ${err}
 fi

# 2. AWIPS bulletins for output points
 echo ' '
 echo 'AWIPS bulletins ...'
 echo '-------------------'
 echo '   Sourcing data file with header info ...'

# 2.b Set up environment variables
 set_trace
 . awipsbull.data

# 2.c Generate list of bulletins to process
 echo '   Generating buoy list ...'
 bulls=$(sed -e 's/export b//g' -e 's/=/ /' awipsbull.data | grep -v "#" |awk '{print $1}')
  
# 2.d Looping over buoys running formbul
 echo '   Looping over buoys ... \n'

 for bull in ${bulls}; do
   fname="${RUN}.wave.${bull}.cbull"
   oname="awipsbull.${bull}.${cycle}.${RUN}.wave"
   headr=$(grep "b${bull}=" awipsbull.data | sed 's/=/ /g' |  awk '{ print $3}')  
   echo "Processing ${bull} (${headr} ${oname}) ..."
 
   if [[ -z "${headr}" ]] || [[ ! -s "${fname}" ]]; then
     set_trace
     msg="ABNORMAL EXIT: MISSING BULLETIN INFO"
     set +x
     echo ' '
     echo '******************************************** '
     echo '*** FATAL ERROR : MISSING BULLETIN INFO *** '
     echo '******************************************** '
     echo ' '
     echo "${msg}"
     set_trace
     echo "${RUN} wave prdgen ${date} ${cycle} : Missing bulletin data."
     err=4;export err;err_chk
     exit ${err}
   fi
  
   set_trace
   
   formbul.pl -d "${headr}" -f "${fname}" -j "${job}" -m "${RUN}.wave" \
              -p "${COMOUT_WAVE_WMO}" -s "NO" -o "${oname}" > formbul.out 2>&1
   OK=$?

   if [[ ${OK} -ne 0 ]] || [[ ! -f "${oname}" ]]; then
     set_trace
     cat formbul.out
     msg="ABNORMAL EXIT: ERROR IN formbul"
     set +x
     echo ' '
     echo '************************************** '
     echo '*** FATAL ERROR : ERROR IN formbul *** '
     echo '************************************** '
     echo ' '
     echo "${msg}"
     set_trace
     echo "${RUN} wave prdgen ${date} ${cycle} : error in formbul."
     err=5;export err;err_chk
     exit ${err}
   fi
   
   cat "${oname}" >> "awipsbull.${cycle}.${RUN}.wave"

 done

# 3. Send output files to the proper destination
set_trace
cp "awipsbull.${cycle}.${RUN}.wave" "${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave"
if [ "${SENDDBN_NTC}" = YES ]; then
    make_ntc_bull.pl "WMOBH" "NONE" "KWBC" "NONE" "${DATA}/awipsbull.${cycle}.${RUN}.wave" \
		     "${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave"
else
    if [ "${envir}" = "para" ] || [ "${envir}" = "test" ] || [ "${envir}" = "dev" ]; then
	echo "Making NTC bulletin for parallel environment, but do not alert."
	(export SENDDBN=NO; make_ntc_bull.pl "WMOBH" "NONE" "KWBC" "NONE" \
					     "${DATA}/awipsbull.${cycle}.${RUN}.wave" "${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave")
    fi
fi

# --------------------------------------------------------------------------- #
# 4.  Clean up

  set -v
  rm -f "${RUN}".wave.*.cbull  awipsbull.data
  set +v

# --------------------------------------------------------------------------- #
# 5.  Ending output


# End of MWW3 product generation script -------------------------------------- #

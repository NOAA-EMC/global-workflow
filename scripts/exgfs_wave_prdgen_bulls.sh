#! /usr/bin/env bash

###############################################################################
#                                                                             #
# This script is the product generator ("graphics job")  for the              #
#  WW3 wave model.                                                            #
#                                                                             #
# COM inputs:                                                                 #
#  - ${COMIN_WAVE_STATION}/${RUN}.${cycle}.cbull_tar                          #
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
 export pgm="wave prdgen"

# 0.b Date and time stuff
 export date=${PDY}
 export YMDH=${PDY}${cyc}
 cat << EOF

                   **************************************
                   *** MWW3 BULLETINS PRODUCTS SCRIPT ***
                   **************************************
                                         ${date} ${cycle}

 Starting at : $(date)


EOF

# 1.  Get necessary files
 echo "   Copying bulletins from ${COMIN_WAVE_STATION}"

# 1.a Link the input file and untar it
 BullIn="${COMIN_WAVE_STATION}/${RUN}.${cycle}.cbull.tar"
 if [[ -f "${BullIn}" ]]; then
   cpreq "${BullIn}" "cbull.tar"
 else
   export err=1
   err_exit "${RUN} wave prdgen ${date} ${cycle} : bulletin tar file missing."
 fi

 echo "   Untarring bulletins ..."
 tar -xf cbull.tar
 OK=$?

 if [[ ${OK} -eq 0 ]]; then
   echo "      Unpacking successfull ..."
   rm -f cbull.tar
 else
   export err=2
   err_exit "ERROR IN BULLETIN TAR FILE"
 fi

# 1.b Output locations from bulletin files
 Nb=$(ls -1 *.cbull | wc -l)
 echo ' '
 echo "   Number of bulletin files :   ${Nb}"
 echo '   --------------------------'
 echo ' '
# 1.c Get the datat cards
 if [ -f "${PARMgfs}/wave/bull_awips_gfswave.${waveGRD}" ]; then
   cpreq "${PARMgfs}/wave/bull_awips_gfswave.${waveGRD}" "awipsbull.data"
 else
   export err=3
   err_exit "Bulletin header data file missing."
 fi

# 2. AWIPS bulletins for output points
 printf "\nAWIPS bulletins ...\n------------------\n   Sourcing data file with header info ..."

# 2.b Set up environment variables
 source awipsbull.data

# 2.c Generate list of bulletins to process
 echo '   Generating buoy list ...'
 bulls=$(sed -e 's/export b//g' -e 's/=/ /' awipsbull.data | grep -v "#" |awk '{print $1}')

# 2.d Looping over buoys running formbul
 echo '   Looping over buoys ... \n'

 for bull in ${bulls}; do
   fname="${RUN}.${bull}.cbull"
   oname="awipsbull.${bull}.${cycle}.${RUN}.wave"
   headr=$(grep "b${bull}=" awipsbull.data | sed 's/=/ /g' |  awk '{ print $3}')
   echo "Processing ${bull} (${headr} ${oname}) ..."

   if [[ -z "${headr}" ]] || [[ ! -s "${fname}" ]]; then
     export err=4
     err_exit "MISSING BULLETIN INFO"
   fi

   formbul.pl -d "${headr}" -f "${fname}" -j "${job}" -m "${RUN}.wave" \
              -p "${COMOUT_WAVE_WMO}" -s "NO" -o "${oname}" > formbul.out 2>&1
   OK=$?

   if [[ ${OK} -ne 0 || ! -f "${oname}" ]]; then
     cat formbul.out
     export err=5
     export pgm="formbul"
     err_exit "error in formbul.pl failed for bulletin ${bull}"
   fi

   cat "${oname}" >> "awipsbull.${cycle}.${RUN}.wave"

 done

# 3. Send output files to the proper destination
cpfs "awipsbull.${cycle}.${RUN}.wave" "${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave"
if [[ "${SENDDBN_NTC}" == YES ]]; then
    make_ntc_bull.pl "WMOBH" "NONE" "KWBC" "NONE" "${DATA}/awipsbull.${cycle}.${RUN}.wave" \
                     "${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave"
else
    if [[ "${envir}" == "para" || "${envir}" == "test" || "${envir}" == "dev" ]]; then
        echo "Making NTC bulletin for parallel environment, but do not alert."
        SENDDBN=NO make_ntc_bull.pl "WMOBH" "NONE" "KWBC" "NONE" \
          "${DATA}/awipsbull.${cycle}.${RUN}.wave" \
          "${COMOUT_WAVE_WMO}/awipsbull.${cycle}.${RUN}.wave"
    fi
fi

# --------------------------------------------------------------------------- #
# 4.  Clean up

  rm -f "${RUN}".*.cbull awipsbull.data

# --------------------------------------------------------------------------- #
# 5.  Ending output


# End of MWW3 product generation script -------------------------------------- #

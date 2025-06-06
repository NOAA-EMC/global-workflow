#! /usr/bin/env bash

################################################################
# Script Name:          gfs_sndp.sh
# Script Description:   Format GFS BUFR sounding files for AWIPS
# Script History Log:
#   1) 2004-09-10       Steve Gilbert       First Implementation
################################################################

  #  Create "collectives" consisting of groupings of the soundings
  #  into files designated by geographical region.   Each input
  #  file gfs_collective*.list (1-9) contains the list of stations to
  #  put in a particular collective output file. 
export m=$1
mkdir $DATA/$m
cd $DATA/$m
  cpreq ${FIXgfs}/product/gfs_collective${m}.list $DATA/$m/.
  CCCC=KWBC
    file_list=gfs_collective${m}.list

    if [ $m -le 2 ]
    then 
      WMOHEAD=JUSA4$m
    elif [ $m -le 6 ]
    then 
      WMOHEAD=JUSB4$m
    else 
      WMOHEAD=JUSX4$m
    fi

    for stn in $(cat $file_list)
    do
       cpreq "${COMOUT_ATMOS_BUFR}/bufr.${stn}.${PDY}${cyc}" "${DATA}/${m}/bufrin"
       export pgm=tocsbufr.x
       #. prep_step
       export FORT11=$DATA/${m}/bufrin
       export FORT51=./bufrout
       ${EXECgfs}/${pgm} << EOF
 &INPUT
  BULHED="$WMOHEAD",KWBX="$CCCC",
  NCEP2STD=.TRUE.,
  SEPARATE=.TRUE.,
  MAXFILESIZE=600000
 /
EOF
       export err=$?;
       if [[ ${err} -ne 0 ]]; then
          echo "FATAL ERROR Failed during execution of ${pgm}"
          exit "${err}"
       fi

       cat "${DATA}/${m}/bufrout" >> "${DATA}/${m}/gfs_collective${m}.fil"
       rm -f "${DATA}/${m}/bufrin" "${DATA}/${m}/bufrout"
    done

    if [[ ${SENDDBN} == 'YES' ]] ; then
        cpfs "${DATA}/${m}/gfs_collective${m}.fil" "${COMOUT_ATMOS_WMO}/gfs_collective${m}.postsnd_${cyc}"
        "${DBNROOT}/bin/dbn_alert" NTC_LOW BUFR "${job}" \
				   "${COMOUT_ATMOS_WMO}/gfs_collective${m}.postsnd_${cyc}"
    fi
    cpfs "${DATA}/${m}/gfs_collective${m}.fil" "${COMOUT_ATMOS_BUFR}/."


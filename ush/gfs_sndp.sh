#! /usr/bin/env bash

################################################################
# Script Name:          gfs_sndp.sh
# Script Description:   Format GFS BUFR sounding files for AWIPS
# Script History Log:
#   1) 2004-09-10       Steve Gilbert       First Implementation
################################################################

source "${USHgfs}/preamble.sh"

  #  Create "collectives" consisting of groupings of the soundings
  #  into files designated by geographical region.   Each input
  #  file gfs_collective*.list (1-9) contains the list of stations to
  #  put in a particular collective output file. 
export m=$1
mkdir $DATA/$m
cd $DATA/$m
  cp ${FIXgfs}/product/gfs_collective${m}.list $DATA/$m/.
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
       cp "${COM_ATMOS_BUFR}/bufr.${stn}.${PDY}${cyc}" "${DATA}/${m}/bufrin"
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
       if (( err != 0 )); then
          echo "FATAL ERROR in ${pgm}"
          err_chk
          exit 3
       fi

       cat $DATA/${m}/bufrout >> $DATA/${m}/gfs_collective$m.fil
       rm $DATA/${m}/bufrin
       rm $DATA/${m}/bufrout
    done

    if [[ ${SENDDBN} == 'YES' ]] ; then
        cp "${DATA}/${m}/gfs_collective${m}.fil" "${COM_ATMOS_WMO}/gfs_collective${m}.postsnd_${cyc}"
        "${DBNROOT}/bin/dbn_alert" NTC_LOW BUFR "${job}" \
				   "${COM_ATMOS_WMO}/gfs_collective${m}.postsnd_${cyc}"
    fi
    cp "${DATA}/${m}/gfs_collective${m}.fil" "${COM_ATMOS_BUFR}/."


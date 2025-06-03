#! /usr/bin/env bash

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

# Set GEMPAK paths.

#. /nwprod/gempak/.gempak

#  Go to a working directory.

cd "${DATA}" || exit 2

#  Set output directory:
if [[ ! -d "${COMOUT_ATMOS_GEMPAK}" ]]; then mkdir -p "${COMOUT_ATMOS_GEMPAK}"; fi

outfilbase=gfs_${PDY}${cyc}

#  Get the list of individual station files.

date
cat "${COMOUT_ATMOS_BUFR}/bufr."*".${PDY}${cyc}" > bufr.combined
date

namsnd << EOF > /dev/null
SNBUFR   = bufr.combined
SNOUTF   = ${outfilbase}.snd
SFOUTF   = ${outfilbase}.sfc
SNPRMF   = sngfs.prm
SFPRMF   = sfgfs.prm
TIMSTN   = 170/2150
r

ex
EOF

date

/bin/rm ./*.nts

snd=${outfilbase}.snd
sfc=${outfilbase}.sfc
cpfs "${snd}" "${COMOUT_ATMOS_GEMPAK}/${snd}"
cpfs "${sfc}" "${COMOUT_ATMOS_GEMPAK}/${sfc}"

if [[ ${SENDDBN} == "YES" ]]; then
   "${DBNROOT}/bin/dbn_alert" MODEL GFS_PTYP_SFC "${job}" "${COMOUT_ATMOS_GEMPAK}/${sfc}"
   "${DBNROOT}/bin/dbn_alert" MODEL GFS_PTYP_SND "${job}" "${COMOUT_ATMOS_GEMPAK}/${snd}"
fi
echo "done" > "${DATA}/gembufr.done"

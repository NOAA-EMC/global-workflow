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

snd=${outfilbase}.snd
sfc=${outfilbase}.sfc

namsnd << EOF > /dev/null
SNBUFR   = bufr.combined
SNOUTF   = ${snd}
SFOUTF   = ${sfc}
SNPRMF   = sngfs.prm
SFPRMF   = sfgfs.prm
TIMSTN   = 170/2150
r

ex
EOF

date

/bin/rm ./*.nts

snd_out=${outfilbase}.soundings.bufr
sfc_out=${outfilbase}.sfc.bufr
cpfs "${snd}" "${COMOUT_ATMOS_GEMPAK}/${snd_out}"
cpfs "${sfc}" "${COMOUT_ATMOS_GEMPAK}/${sfc_out}"

if [[ ${SENDDBN} == "YES" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL GFS_PTYP_SFC "${job}" "${COMOUT_ATMOS_GEMPAK}/${sfc_out}"
    "${DBNROOT}/bin/dbn_alert" MODEL GFS_PTYP_SND "${job}" "${COMOUT_ATMOS_GEMPAK}/${snd_out}"
fi
echo "done" > "${DATA}/gembufr.done"

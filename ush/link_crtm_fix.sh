#! /usr/bin/env bash

source "${USHgfs}/preamble.sh"

# Get CRTM fix directory from (in this order):
# 1. First argument to script, or
# 2. $CRTM_FIX environment variable, or
# 3. Nowhere.  Give up.  Whine.
CRTM_FIX="${1:-${CRTM_FIX:-MISSING}}"

if [[ "${CRTM_FIX}" == "MISSING" ]] ; then
	echo "Please specify CRTM fix location.  Giving up." 1>&2
	exit 19
fi
if [[ ! -d "${CRTM_FIX}" ]] ; then
	echo "${CRTM_FIX}: \${CRTM_FIX} is not a directory.  Giving up." 1>&2
	exit 38
fi

for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
	"imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
	"ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
	"ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
	"tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" "ahi_himawari8" ; do
	ln -s "${CRTM_FIX}/${what}.TauCoeff.bin" .
	ln -s "${CRTM_FIX}/${what}.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
	ln -s "${CRTM_FIX}/${what}Coeff.bin" .
done

for what in  ${CRTM_FIX}/*Emis* ; do
	ln -s ${what} .
done

exit 0

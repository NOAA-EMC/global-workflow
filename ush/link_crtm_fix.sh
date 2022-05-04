#! /bin/sh

# Get CRTM fix directory from (in this order):
# 1. First argument to script, or
# 2. $FIXCRTM environment variable, or
# 3. Nowhere.  Give up.  Whine.
FIXCRTM="${1:-${FIXCRTM:-MISSING}}"

if [[ "$FIXCRTM" == "MISSING" ]] ; then
    echo "Please specify CRTM fix location.  Giving up." 1>&2
    exit 19
fi
if [[ ! -d "$FIXCRTM" ]] ; then
    echo "$FIXCRTM: \$FIXCRTM is not a directory.  Giving up." 1>&2
    exit 38
fi

for what in "amsre_aqua" "imgr_g11" "imgr_g12" "imgr_g13" \
    "imgr_g15" "imgr_mt1r" "imgr_mt2" "seviri_m10" \
    "ssmi_f13" "ssmi_f14" "ssmi_f15" "ssmis_f16" \
    "ssmis_f17" "ssmis_f18" "ssmis_f19" "ssmis_f20" \
    "tmi_trmm" "v.seviri_m10" "imgr_insat3d" "abi_gr" "ahi_himawari8" ; do
    ln -s "$FIXCRTM/$what.TauCoeff.bin" .
    ln -s "$FIXCRTM/$what.SpcCoeff.bin" .
done

for what in 'Aerosol' 'Cloud' ; do
    ln -s "$FIXCRTM/${what}Coeff.bin" .
done

for what in  $FIXCRTM/*Emis* ; do
   ln -s $what .
done

exit 0

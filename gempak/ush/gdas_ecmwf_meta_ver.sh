#! /usr/bin/env bash
#
# Creates a loop comparing the 6 hr gdas fcst to the pervious 7 days
# of ecmwf fcsts
#

source "${HOMEgfs}/ush/preamble.sh"

export pgm=gdplot2_nc;. prep_step

cyc2=12
device="nc | ecmwfver.meta"

#
# Copy in datatype table to define gdfile type
#

cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl
export err=$?
if (( err != 0 )) ; then
   echo "FATAL ERROR: File datatype.tbl does not exist."
   exit "${err}"
fi

export COMIN="gdas.${PDY}${cyc}"
if [[ ! -L ${COMIN} ]]; then
    ${NLN} "${COMIN_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi
vergrid="F-GDAS | ${PDY:2}/0600"
fcsthr="0600f006"

# GENERATING THE METAFILES.
areas="SAM NAM"

for area in ${areas}; do
    if [[ "${area}" == "NAM" ]] ; then
        garea="5.1;-124.6;49.6;-11.9"
        proj="STR/90.0;-95.0;0.0"
        latlon="0"
        run="run"
    else
        garea="-33.7;-150.5;8.0;-35.0"
        proj="str/-85;-70;0"
        latlon="1/10/1/2/10;10"
        run=" "
    fi
    for (( fhr=24; fhr<=168; fhr+=24 )); do
        dgdattim=$(printf "f%03d" "${fhr}")
        sdatenum=$(date --utc +%y%m%d -d "${PDY} ${cyc2} - ${fhr} hours")

        if [[ ! -L "ecmwf.20${sdatenum}" ]]; then
            ${NLN} "${COMINecmwf}/ecmwf.20${sdatenum}/gempak" "ecmwf.20${sdatenum}"
        fi
        gdfile="ecmwf.20${sdatenum}/ecmwf_glob_20${sdatenum}12"

        # 500 MB HEIGHT METAFILE

        "${GEMEXE}/gdplot2_nc" << EOFplt
\$MAPFIL = mepowo.gsf
PROJ     = ${proj}
GAREA    = ${garea}
map      = 1//2
clear    = yes
text     = 1/22/////hw
contur   = 2
skip     = 0
type     = c
latlon   = ${latlon}

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
device   = ${device}
gdpfun   = sm5s(hght)
glevel   = 500
gvcord   = pres
scale    = -1
cint     = 6
line     = 6/1/3
title    = 6/-2/~ GDAS 500 MB HGT (6-HR FCST)|~${area} 500 HGT DF
r

gdfile   = ${gdfile}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ECMWF 500 MB HGT
clear    = no
latlon   = 0
r

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
gdpfun   = sm5s(pmsl)
glevel   = 0
gvcord   = none
scale    = 0
cint     = 4
line     = 6/1/3
contur   = 2
title    = 6/-2/~ GDAS PMSL (6-HR FCST)|~${area} PMSL DF
clear    = yes
latlon   = ${latlon}
r

gdfile   = ${gdfile}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ECMWF PMSL
clear    = no
r

PROJ     =
GAREA    = bwus
gdfile   = ${vergrid}
gdattim  = ${fcsthr}
gdpfun   = sm5s(pmsl)
glevel   = 0
gvcord   = none
scale    = 0
cint     = 4
line     = 6/1/3
contur   = 2
title    = 6/-2/~ GDAS PMSL (6-HR FCST)|~US PMSL DIFF
clear    = yes
latlon   = ${latlon}
${run}

gdfile   = ${gdfile}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ECMWF PMSL
clear    = no
${run}

ex
EOFplt

    done
done

export err=$?

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
if (( err != 0 )) || [[ ! -s ecmwfver.meta ]]; then
    echo "FATAL ERROR: Failed to create ecmwf meta file"
    exit "${err}"
fi

mv ecmwfver.meta "${COMOUT_ATMOS_GEMPAK_META}/ecmwfver_${PDY}_${cyc2}"
export err=$?
if (( err != 0 )) ; then
    echo "FATAL ERROR: Failed to move meta file to ${COMOUT_ATMOS_GEMPAK_META}/ecmwfver_${PDY}_${cyc2}"
    exit "${err}"
fi

if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL ECMWFVER_HPCMETAFILE "${job}" \
        "${COMOUT_ATMOS_GEMPAK_META}/ecmwfver_${PDY}_${cyc2}"
fi

exit

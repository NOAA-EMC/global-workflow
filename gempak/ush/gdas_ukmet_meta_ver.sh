#! /usr/bin/env bash
#
# Metafile Script : gdas_ukmet_meta_ver
#
# Creates a comparison loop between the 6 hr gdas fcst and the prvs
# 9 cycles of the ukmet
#
# Log :
# J. Carr/HPC     3/2001   Added new metafile
# J. Carr/HPC     5/2001   Added a mn variable for a/b side dbnet root variable.
# M. Klein/HPC   11/2004   Change fnl/FNL to gdas/GDAS (name change only)
# M. Klein/HPC    2/2005   Changed location of working directory to /ptmp
# M. Klein/HPC   11/2006   Modify to run in production.
#

source "${HOMEgfs}/ush/preamble.sh"

export pgm=gdplot2_nc;. prep_step

device="nc | ukmetver_12.meta"
cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl

# SET CURRENT CYCLE AS THE VERIFICATION GRIDDED FILE.
export COMIN="gdas.${PDY}${cyc}"
if [[ ! -L ${COMIN} ]]; then
    ${NLN} "${COMINT_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi
vergrid="F-GDAS | ${PDY:2}/0600"
fcsthr="0600f006"

# SET WHAT RUNS TO COMPARE AGAINST BASED ON MODEL CYCLE TIME.
areas="SAM NAM"

# GENERATING THE METAFILES.
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

    fhrs=$(seq -s ' ' 12 12 72)
    fhrs="${fhrs} $(seq -s ' ' 96 24 144)"
    for fhr in ${fhrs}; do
        stime=$(date --utc +%y%m%d -d "${PDY} ${cyc} - ${fhr} hours")
        dgdattim=$(printf "f%03d" "${fhr}")
        sdatenum=${stime:0:6}
        cyclenum=${stime:6}

        if [[ ! -L "ukmet.20${sdatenum}" ]]; then
            ${NLN} "${COMINukmet}/ukmet.20${sdatenum}/gempak" "ukmet.20${sdatenum}"
        fi
        gdfile="ukmet.20${sdatenum}/ukmet_20${sdatenum}${cyclenum}${dgdattim}"

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
title    = 6/-2/~ GDAS 500 MB HGT (6-HR FCST)|~${area} 500 HGT DIFF
r

gdfile   = ${gdfile}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ UKMET 500 MB HGT
clear    = no
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
title    = 6/-2/~ GDAS PMSL (6-HR FCST)|~${area} PMSL DIFF
clear    = yes
latlon   = ${latlon}
r

gdfile   = ${gdfile}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ UKMET PMSL
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
title    = 5/-1/~ UKMET PMSL
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
if (( err != 0 )) || [[ ! -s ukmetver_12.meta ]]; then
    echo "FATAL ERROR: Failed to create ukmet meta file"
    exit "${err}"
fi

mv ukmetver_12.meta "${COMOUT_ATMOS_GEMPAK_META}/ukmetver_${PDY}_12"
export err=$?
if (( err != 0 )) ; then
    echo "FATAL ERROR: Failed to move meta file to ${COMOUT_ATMOS_GEMPAK_META}/ukmetver_${PDY}_12"
    exit "${err}"
fi

if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL UKMETVER_HPCMETAFILE "${job}" \
        "${COMOUT_ATMOS_GEMPAK_META}/ukmetver_${PDY}_12"
fi

exit

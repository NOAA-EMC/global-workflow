#! /usr/bin/env bash
#
# Metafile Script : gfs_meta_ver_new
#
# Set up Local Variables
#

source "${HOMEgfs}/ush/preamble.sh"

mkdir -p -m 775 "${DATA}/VER"
cd "${DATA}/VER" || exit 2
cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl

MDL=GFS
metaname="gfsver_${cyc}.meta"
device="nc | ${metaname}"

#
# Link data into DATA to sidestep gempak path limits
# TODO: Replace this
#
export COMIN="${RUN}.${PDY}${cyc}"
if [[ ! -L ${COMIN} ]]; then
    ln -sf "${COM_ATMOS_GEMPAK_1p00}" "${COMIN}"
fi

# SET CURRENT CYCLE AS THE VERIFICATION GRIDDED FILE.
vergrid="F-${MDL} | ${PDY:2}/${cyc}00"
fcsthr="f00"

MDL2="GFSHPC"
#GENERATING THE METAFILES.
# seq won't give us any splitting problems, ignore warnings
# shellcheck disable=SC2207,SC2312
IFS=$'\n' lookbacks=($(seq 6 6 180) $(seq 192 12 216))
for lookback in "${lookbacks[@]}"; do
    init_time="$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${lookback} hours")"
    init_PDY=${init_time:0:8}
    init_cyc=${init_time:8:2}

    if (( init_time <= ${SDATE:-0} )); then
        echo "Skipping ver for ${init_time} because it is before the experiment began"
        if (( lookback == "${lookbacks[0]}" )); then
            echo "First forecast time, no metafile produced"
            exit 0
        else
            break
        fi
    fi

    dgdattim="f$(printf "%03g" "${lookback}")"

    # Create symlink in DATA to sidestep gempak path limits
    HPCGFS="${RUN}.${init_time}"
    if [[ ! -L "${HPCGFS}" ]]; then
        YMD=${init_PDY} HH=${init_cyc} GRID="1p00" generate_com source_dir:COM_ATMOS_GEMPAK_TMPL
        ln -sf "${source_dir}" "${HPCGFS}"
    fi

    grid="F-${MDL2} | ${init_PDY}/${init_cyc}00"

    # 500 MB HEIGHT METAFILE

    export pgm=gdplot2_nc;. prep_step
    "${GEMEXE}/gdplot2_nc" << EOFplt
PROJ     = STR/90.0;-95.0;0.0
GAREA    = 5.1;-124.6;49.6;-11.9
map      = 1//2
clear    = yes
text     = 1/22/////hw
contur   = 2
skip     = 0
type     = c

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
device   = ${device}
gdpfun   = sm5s(hght)
glevel   = 500
gvcord   = pres
scale    = -1
cint     = 6
line     = 6/1/3
title    = 6/-2/~ ? GFS 500 MB HGT|~500 HGT DIFF
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS 500 MB HGT
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
title    = 6/-2/~ ? GFS PMSL|~PMSL DIFF
clear    = yes
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS PMSL
clear    = no
r

!PROJ     =
!GAREA    = bwus
!gdfile   = ${vergrid}
!gdattim  = ${fcsthr}
!gdpfun   = sm5s(pmsl)
!glevel   = 0
!gvcord   = none
!scale    = 0
!cint     = 4
!line     = 6/1/3
!contur   = 2
!title    = 6/-2/~ ? GFS PMSL |~US PMSL DIFF
!clear    = yes
!r
!
!gdfile   = ${grid}
!gdattim  = ${dgdattim}
!line     = 5/1/3
!contur   = 4
!title    = 5/-1/~ ? GFS PMSL
!clear    = no
!r

! SOUTH AMERICAN AREA.
! 500 MB

PROJ     =
GAREA    = samps
map      = 1//2
clear    = yes
text     = 1/22/////hw
contur   = 2
skip     = 0
type     = c

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
device   = ${device}
gdpfun   = sm5s(hght)
glevel   = 500
gvcord   = pres
scale    = -1
cint     = 6
line     = 6/1/3
title    = 6/-2/~ ? GFS 500 MB HGT|~SAM 500 HGT DIFF
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS 500 MB HGT
clear    = no
r

! SOUTH AMERICAN AREA.
! PMSL

gdfile   = ${vergrid}
gdattim  = ${fcsthr}
gdpfun   = sm5s(pmsl)
glevel   = 0
gvcord   = none
scale    = 0
cint     = 4
line     = 6/1/3
contur   = 2
title    = 6/-2/~ ? GFS PMSL|~SAM PMSL DIFF
clear    = yes
r

gdfile   = ${grid}
gdattim  = ${dgdattim}
line     = 5/1/3
contur   = 4
title    = 5/-1/~ ? GFS PMSL
clear    = no
r

ex
EOFplt
    export err=$?;err_chk

    #####################################################
    # GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
    # WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
    # FOR THIS CASE HERE.
    #####################################################
    if (( err != 0 )) || [[ ! -s "${metaname}" ]] &> /dev/null; then
        echo "FATAL ERROR: Failed to create gempak meta file ${metaname}"
        exit $(( err + 100 ))
    fi

done

mv "${metaname}" "${COM_ATMOS_GEMPAK_META}/gfsver_${PDY}_${cyc}"
if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
        "${COM_ATMOS_GEMPAK_META}/gfsver_${PDY}_${cyc}"
    if [[ "${DBN_ALERT_TYPE}" = "GFS_METAFILE_LAST" ]] ; then
        DBN_ALERT_TYPE=GFS_METAFILE
        "${DBNROOT}/bin/dbn_alert" MODEL "${DBN_ALERT_TYPE}" "${job}" \
            "${COM_ATMOS_GEMPAK_META}/gfsver_${PDY}_${cyc}"
    fi
fi

exit

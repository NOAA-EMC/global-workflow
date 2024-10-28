#! /usr/bin/env bash
#
# Metafile Script : gfs_meta_mar_comp.sh
#
# Set up Local Variables
#

source "${HOMEgfs}/ush/preamble.sh"

rm -Rf "${DATA}/GEMPAK_META_MAR"
mkdir -p -m 775 "${DATA}/GEMPAK_META_MAR" "${DATA}/MAR_COMP"

cd "${DATA}/MAR_COMP" || exit 2
cp "${HOMEgfs}/gempak/fix/datatype.tbl" datatype.tbl

export COMIN="gfs.multi"
mkdir -p "${COMIN}"
for cycle in $(seq -f "%02g" -s ' ' 0 "${INTERVAL_GFS}" "${cyc}"); do
    YMD=${PDY} HH=${cycle} GRID="1p00" declare_from_tmpl gempak_dir:COM_ATMOS_GEMPAK_TMPL
    for file_in in "${gempak_dir}/gfs_1p00_${PDY}${cycle}f"*; do
        file_out="${COMIN}/$(basename "${file_in}")"
        if [[ ! -L "${file_out}" ]]; then
            ${NLN} "${file_in}" "${file_out}"
        fi
    done
done

#
# Link data into DATA to sidestep gempak path limits
# TODO: Replace this
#
export HPCNAM="nam.${PDY}"
if [[ ! -L ${HPCNAM} ]]; then
    ${NLN} "${COMINnam}/nam.${PDY}/gempak" "${HPCNAM}"
fi

mdl=gfs
MDL="GFS"
metatype="mar_comp"
metaname="${mdl}_${metatype}_${cyc}.meta"
device="nc | ${metaname}"

grid="F-${MDL} | ${PDY:2}/${cyc}00"
for garea in NAtl NPac; do
    case ${garea} in
        NAtl)
            garea="natl"
            proj=" "
            latlon="18/2/1/1/10"
            ;;
        NPac)
            garea="mpac"
            proj=" "
            latlon="18/2/1/1/10"
            ;;
        *)
            echo "FATAL ERROR: Unknown domain"
            exit 100
    esac

    offsets=(6 12)
    for offset in "${offsets[@]}"; do
        init_time=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${offset} hours")
        init_PDY=${init_time:0:8}
        init_cyc=${init_time:8:2}

        if (( init_time <= SDATE )); then
            echo "Skipping generation for ${init_time} because it is before the experiment began"
            if (( offset == "${offsets[0]}" )); then
                echo "First forecast time, no metafile produced"
                exit 0
            fi
            continue
        fi

        # Create symlink in DATA to sidestep gempak path limits
        HPCGFS="${RUN}.${init_time}"
        if [[ ! -L ${HPCGFS} ]]; then
            YMD="${init_PDY}" HH="${init_cyc}" GRID="1p00" declare_from_tmpl source_dir:COM_ATMOS_GEMPAK_TMPL
            ${NLN} "${source_dir}" "${HPCGFS}"
        fi

        case ${cyc} in
            00 | 12)
                contours=1
                type_param="CTYPE"
                ex=""
                ;;
            06 | 18)
                contours=2
                type_param="TYPE"
                ex="ex"
                ;;
            *)
                echo "FATAL ERROR: Invalid cycle ${cyc} passed to ${BASH_SOURCE[0]}"
        esac

        case ${cyc}_${init_cyc} in
            00_*)   testgfsfhr=114;;
            06_00)  testgfsfhr=84;;
            06_18)  testgfsfhr=72;;
            12_00)  testgfsfhr=114;;
            12_06)  testgfsfhr=78;;
            18_06)  testgfsfhr=72;;
            18_12)  testgfsfhr=84;;
            *)
                echo "FATAL ERROR: Undefined pairing of cycles"
                exit 200
                ;;
        esac

        for fhr in $(seq -s ' ' 0 6 126); do
            gfsfhr=F$(printf "%02g" "${fhr}")
            gfsoldfhr=F$(printf "%02g" $((fhr + offset)))
            grid2="F-GFSHPC | ${init_time:2}/${init_cyc}00"
            gdpfun1="sm5s(hght)!sm5s(hght)"
            gdpfun2="sm5s(pmsl)!sm5s(pmsl)"
            line="5/1/3/2/2!6/1/3/2/2"
            hilo1="5/H#;L#//5/5;5/y!6/H#;L#//5/5;5/y"
            hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y!6/H#;L#/1018-1060;900-1012/5/10;10/y"
            title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${garea} ${cyc}Z vs ${init_cyc}Z 500 HGT!6/-3/~ ? ${MDL} @ HGT (${init_cyc}Z CYAN)"
            title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${garea} ${cyc}Z vs ${init_cyc}Z PMSL!6/-3/~ ? ${MDL} PMSL (${init_cyc}Z CYAN)"
            if (( fhr > testgfsfhr )); then
                grid2=" "
                gfsoldfhr=" "
                gdpfun1="sm5s(hght)"
                gdpfun2="sm5s(pmsl)"
                line="5/1/3/2/2"
                hilo1="5/H#;L#//5/5;5/y"
                hilo2="5/H#;L#/1018-1060;900-1012/5/10;10/y"
                title1="5/-2/~ ? ^ ${MDL} @ HGT (${cyc}Z YELLOW)|^${garea} ${cyc}Z vs ${init_cyc}Z 500 HGT"
                title2="5/-2/~ ? ^ ${MDL} PMSL (${cyc}Z YELLOW)|^${garea} ${cyc}Z vs ${init_cyc}Z PMSL"
            fi

            export pgm=gdplot2_nc;. prep_step
            "${GEMEXE}/gdplot2_nc" << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
SKIP    = 0
PANEL   = 0
CONTUR  = ${contours}
CLRBAR  =
FINT    =
FLINE   =
REFVEC  =
WIND    = 0

GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
GLEVEL  = 500
GVCORD  = PRES
GDPFUN  = ${gdpfun1}
LINE    = ${line}
SCALE   = -1
${type_param}   = c
CINT    = 6
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
HILO    = ${hilo1}
TITLE   = ${title1}
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = ${gdpfun2}
CINT    = 4
HLSYM   = 1.2;1.2//21//hw
TEXT    = 1/21//hw
GDFILE  = ${grid}  !${grid2}
GDATTIM = ${gfsfhr}!${gfsoldfhr}
LINE    = ${line}
HILO    = ${hilo2}
TITLE   = ${title2}
run

${ex}
EOF
            export err=$?;err_chk
        done
    done

    if (( 10#${cyc} % 12 ==0 )); then

        #
        # There are some differences between 00z and 12z
        # The YEST string makes sense (but is inconsistently used)
        # The others I'm not sure why they differ. - WCK
        #
        case ${cyc} in
            00)
                type_param="TYPE"
                hlsym="1.2;1.2//21//hw"
                wind=""
                yest=" YEST"
                run_cmd="run"
                extra_cmd="\nHLSYM   = 1.2;1.2//21//hw\nTEXT    = s/21//hw"
                ;;
            12)
                type_param="CTYPE"
                hlsym="1;1//21//hw"
                wind="0"
                yest=""
                run_cmd="ru"
                extra_cmd=""
                ;;
            *)
                echo "FATAL ERROR: Invalid cycle {cyc} in ${BASH_SOURCE[0]}"
                exit 100
                ;;
        esac

        # COMPARE THE GFS MODEL TO THE UKMET MODEL 12-HOURS PRIOR
        ukmet_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - 12 hours")
        ukmet_PDY=${ukmet_date:0:8}
        ukmet_cyc=${ukmet_date:8:2}

        export HPCUKMET="ukmet.${ukmet_PDY}"
        if [[ ! -L "${HPCUKMET}" ]]; then
            ${NLN} "${COMINukmet}/ukmet.${ukmet_PDY}/gempak" "${HPCUKMET}"
        fi
        grid2="F-UKMETHPC | ${ukmet_PDY:2}/${ukmet_date}"

        for fhr in 00 12 24 84 108; do
            gfsfhr=F$(printf "%02g" "${fhr}")
            ukmetfhr=F$(printf "%02g" $((fhr + 12)))

            export pgm=gdplot2_nc;. prep_step
            "${GEMEXE}/gdplot2_nc" << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0
PANEL   = 0
CONTUR  = 2
CLRBAR  =
GLEVEL  = 500
GVCORD  = PRES
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2
SCALE   = -1
${type_param}   = c
CINT    = 6
FINT    =
FLINE   =
HLSYM   = ${hlsym}
TEXT    = s/21//hw
WIND    = 0
REFVEC  =
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (${cyc}Z YELLOW)|~${garea} ${cyc}Z vs UKM ${ukmet_cyc}Z 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${ukmetfhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 6/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? UKMET @ HGT (${ukmet_cyc}Z${yest} CYAN)!0
l
${run_cmd}

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4${extra_cmd}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (${cyc}Z YELLOW)|~${garea} ${cyc}Z vs UKM ${ukmet_cyc}Z PMSL!0
l
${run_cmd}

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${ukmetfhr}
LINE    = 6/1/3/2
HILO    = 6/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? UKMET PMSL (${ukmet_cyc}Z CYAN)!0
l
${run_cmd}

EOF
            export err=$?;err_chk
        done

        # COMPARE THE GFS MODEL TO THE 12 UTC ECMWF FROM YESTERDAY
        offset=$(( (10#${cyc}+12)%24 + 12 ))
        ecmwf_date=$(date --utc +%Y%m%d%H -d "${PDY} ${cyc} - ${offset} hours")
        ecmwf_PDY=${ecmwf_date:0:8}
        # ecmwf_cyc=${ecmwf_date:8:2}


        HPCECMWF=ecmwf.${PDY}
        if [[ ! -L "${HPCECMWF}" ]]; then
            ${NLN} "${COMINecmwf}/ecmwf.${ecmwf_PDY}/gempak" "${HPCECMWF}"
        fi
        grid2="${HPCECMWF}/ecmwf_glob_${ecmwf_date}"

        for fhr in $(seq -s ' ' $(( offset%24 )) 24 120 ); do
            gfsfhr=F$(printf "%02g" "${fhr}")
            ecmwffhr=F$(printf "%02g" $((fhr + 24)))

            export pgm=gdplot2_nc;. prep_step
            "${GEMEXE}/gdplot2_nc" << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0
PANEL   = 0
CONTUR  = 2
CLRBAR  =
GLEVEL  = 500
GVCORD  = PRES
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2
SCALE   = -1
${type_param}   = c
CINT    = 6
FINT    =
FLINE   =
HLSYM   = ${hlsym}
TEXT    = s/21//hw
WIND    = ${wind}
REFVEC  =
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (12Z YELLOW)|~${garea} 12Z vs ECM yest 12Z 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${ecmwffhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 6/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? ECMWF @ HGT (12Z YEST CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4${extra_cmd}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (12Z YELLOW)|~${garea} 12Z vs ECM yest 12Z PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${ecmwffhr}
LINE    = 6/1/3/2
HILO    = 6/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? ECMWF PMSL (12Z YEST CYAN)!0
l
run

EOF
            export err=$?;err_chk
        done

        # COMPARE THE GFS MODEL TO THE NAM and NGM
        grid2="F-NAMHPC | ${PDY:2}/${cyc}00"
        for fhr in $(seq -s ' ' 0 6 84); do
            gfsfhr=F$(printf "%02g" "${fhr}")
            namfhr=F$(printf "%02g" "${fhr}")

            "${GEMEXE}/gdplot2_nc" << EOF
DEVICE  = ${device}
MAP     = 1/1/1/yes
CLEAR   = yes
GAREA   = ${garea}
PROJ    = ${proj}
LATLON  = ${latlon}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}

SKIP    = 0
PANEL   = 0
CONTUR  = 2
CLRBAR  =
GLEVEL  = 500
GVCORD  = PRES
GDPFUN  = sm5s(hght)
LINE    = 5/1/3/2
SCALE   = -1
TYPE    = c
CINT    = 6
FINT    =
FLINE   =
HLSYM   = ${hlsym}
TEXT    = s/21//hw
WIND    =
REFVEC  =
HILO    = 3/H#;L#//5/5;5/y
TITLE   = 5/-1/~ ? ${MDL} @ HGT (${cyc}Z YELLOW)|~${garea} ${MDL}/NAM/NGM 500 HGT!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDATTIM = ${namfhr}
GDPFUN  = sm5s(hght)
LINE    = 6/1/3/2
HILO    = 5/H#;L#//5/5;5/y
TITLE   = 6/-2/~ ? NAM @ HGT (${cyc}Z CYAN)!0
l
run

CLEAR   = yes
GLEVEL  = 0
GVCORD  = none
SCALE   = 0
GDPFUN  = sm5s(pmsl)
CINT    = 4${extra_cmd}
GDFILE  = ${grid}
GDATTIM = ${gfsfhr}
LINE    = 5/1/3/2
HILO    = 3/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 5/-1/~ ? ${MDL} PMSL (${cyc}Z YELLOW)|~${garea} ${MDL}/NAM/NGM PMSL!0
l
run

CLEAR   = no
GDFILE  = ${grid2}
GDPFUN  = sm5s(pmsl)
GDATTIM = ${namfhr}
LINE    = 6/1/3/2
HILO    = 5/H#;L#/1018-1060;900-1012/5/10;10/y
TITLE   = 6/-2/~ ? NAM PMSL (${cyc}Z CYAN)!0
l
run

EOF
            export err=$?;err_chk
        done
    fi
done

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
if (( err != 0 )) || [[ ! -s "${metaname}" ]] &> /dev/null; then
    echo "FATAL ERROR: Failed to create gempak meta file ${metaname}"
    exit $(( err + 100 ))
fi

mv "${metaname}" "${COM_ATMOS_GEMPAK_META}/${mdl}_${PDY}_${cyc}_mar_comp"
if [[ "${SENDDBN}" == "YES" ]] ; then
    "${DBNROOT}/bin/dbn_alert MODEL" "${DBN_ALERT_TYPE}" "${job}" \
        "${COM_ATMOS_GEMPAK_META}/${mdl}_${PDY}_${cyc}_mar_comp"
fi

exit

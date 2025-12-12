#! /usr/bin/env bash

# Script to recover JTWC Bulletins from Tank
#  $TANK_TROPCY/$ymddir/wtxtbul/tropcyc

#  Y2K VERSION --  This script can process JTWC bulletins with EITHER a
#                    2-digit year starting in column 20 or a 4-digit year
#                    starting in column 20.
# Mar 2013, DStokes - modified for WCOSS.  Added option to email developer.
# Oct 2013, DStokes - Add check of stormname length and truncate if needed
#                     in response to recent problems with JTWC reports.
#                     Remove option to email developer.
#
#
# Positional parameters passed in:
#   1 - Run date (YYYYMMDDHH)

# Imported variables that must be passed in:
#   DATA         - path to working directory
#   TANK_TROPCY  - path to home directory containing tropical cyclone record
#                  data base

cd "${DATA}" || exit 1

if [[ "$#" -ne '1' ]]; then
    echo "**NON-FATAL ERROR PROGRAM  SYNDAT_GETJTBUL  run date not in \
positional parameter 1"
    exit
fi

run_date=$1

ymd=${run_date:2:6}

echo "${PDYm1}"
pdym1=${PDYm1}

echo
echo "Entering sub-shell syndat_getjtbul.sh to recover JTWC Bulletins"
echo

if [[ "${cyc}" -eq "00" ]]; then

    # For 00Z cycle, need to go to prior day's tank
    # ---------------------------------------------

    jtwcdir="${TANK_TROPCY}/${PDY}/wtxtbul"
    jtwcdirm1="${TANK_TROPCY}/${pdym1}/wtxtbul"
else
    jtwcdir="${TANK_TROPCY}/${PDY}/wtxtbul"
fi

set +x
echo
echo "  Run date is ${run_date}"
echo
echo "  PDY is      ${PDY}"
echo
echo "  pdym1 is    ${pdym1}"
echo
echo "  ymddir is   ${PDY}"
echo
set_trace

find="${ymd} ${cyc}"
echo "looking for string ${find} in ${jtwcdir}/tropcyc"

rm -f jtwcbul
# shellcheck disable=SC2312
grep "${ymd} ${cyc}" "${jtwcdir}/tropcyc" | grep JTWC > jtwcbul
if [[ -s jtwcbul ]]; then
    echo "String found: contents of JTWC bulletin are:"
    cat jtwcbul
else
    echo "String not found: no JTWC bulletins available for this run"
fi

if [[ "${cyc}" == "00" ]]; then
    # shellcheck disable=SC2312
    grep "${ymd} ${cyc}" "${jtwcdirm1}/tropcyc" | grep JTWC >> jtwcbul
    if [[ -s jtwcbul ]]; then
        echo "String found: contents of JTWC bulletin are:"
        cat jtwcbul
    else
        echo "String not found: no JTWC bulletins available for this run"
    fi
fi

# Check for and truncate stormnames with length greater than nine characters and leave rest of record intact.
#  This spell makes no attempt to correct any other potential errors in the record format.
perl -wpi.ORIG -e 's/(^.... ... )(\S{9,9})(\S{1,})/$1$2/' jtwcbul
diff jtwcbul.ORIG jtwcbul > jtwcbul_changes.txt
if [[ -s jtwcbul_changes.txt ]]; then
    echo "***WARNING:  SOME JTWC VITALS SEGMENTS REQUIRED PRELIMINARY MODIFICATION!"
    cat jtwcbul_changes.txt
fi

# Execute bulletin processing

if [[ -s jtwcbul ]]; then
    echo "Processing JTWC bulletin halfs into tcvitals records"
fi

pgm=$(basename "${EXECgfs}/syndat_getjtbul.x")
export pgm
if [[ -s prep_step ]]; then
    set +u
    source prep_step
    set -u
else
    [[ -f errfile ]] && rm errfile
    # shellcheck disable=SC2046,SC2312
    unset FORT00 $(env | grep "^FORT[0-9]\{1,\}=" | awk -F= '{print $1}')
fi

rm -f fnoc

export FORT11=jtwcbul
export FORT51=fnoc
# shellcheck disable=SC2312
time -p "${EXECgfs}/${pgm}" 2> errfile
errget=$?
cat errfile
rm errfile
set +x
echo
echo "The foreground exit status for SYNDAT_GETJTBUL is ${errget}"
echo
set_trace
if [[ "${errget}" -gt '0' ]]; then
    if [[ "${errget}" -eq '1' ]]; then
        msg="No JTWC bulletins in ${jtwcdir}/tropcyc, no JTWC tcvitals available for qctropcy for ${run_date}"
        if [[ "${RUN}" == "gfs" ]]; then
            if [[ "${SENDSDM}" == "YES" ]]; then
                ecf_family=$(echo "${ECF_NAME}" | awk 'BEGIN {FS="/j"} {print $1}')
                export ecf_family
                echo "${msg}" > "${COMOUT}/${NET}_${RUN}.t${cyc}z.emailbody"
                echo "export subject='No JTWC bulletins available for ${run_date} ${RUN} run'" > "${COMOUT}/${NET}_${RUN}.t${cyc}z.emailvar"
                # JY echo "export maillist='sdm@noaa.gov'" >> $COMOUT/${NET}_${RUN}.t${cyc}z.emailvar
                echo "export maillist=${maillist}" >> "${COMOUT}/${NET}_${RUN}.t${cyc}z.emailvar"
                ecflow_client --run "${ecf_family}/j${RUN}_jtwc_bull_email"
            fi
        fi
    else
        echo "**NON-FATAL ERROR PROGRAM  SYNDAT_GETJTBUL  FOR ${run_date} \
RETURN CODE ${errget}"
    fi
else
    echo "program  SYNDAT_GETJTBUL  completed normally for ${run_date}, JTWC \
rec. passed to qctropcy"
fi
set +x
echo
echo "----------------------------------------------------------"
echo "***********  COMPLETED PROGRAM syndat_getjtbul  **********"
echo "----------------------------------------------------------"
echo
set_trace

if [[ "${errget}" -eq '0' ]]; then
    echo "Completed JTWC tcvitals records are:"
    cat fnoc
fi

echo "Leaving sub-shell syndat_getjtbul.sh to recover JTWC Bulletins"

echo " "

exit

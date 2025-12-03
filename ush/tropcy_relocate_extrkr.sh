#! /usr/bin/env bash

# This script is executed by the script tropcy_relocate.sh
# --------------------------------------------------------

export machine=${machine^^}
export err pgm

# Variable "gribver" defines if input GRIB data is in
# GRIB1 (gribver=1) or GRIB2 (gribver=2) format.
export gribver=${gribver:-1}

#	PDYcyc - 10 digit date
#	RUN    - network run (gfs, gdas, etc)
#	DATA   - working directory
#	SUPVX  - tcvitals update program
#	GETTX  - Tim's get track program
##XLF_LINKSSH=${XLF_LINKSSH:-""}
export APRNGETTX=${APRNGETTX:-""}

TIMEIT=""
if [[ -s "${DATA}/timex" ]]; then
    TIMEIT="${DATA}/timex"
fi

vdir=${vdir:-${DATA}/trakout}
mkdir -p "${vdir}"

cd "${vdir}" || exit 1

# This script kicks off the hurricane tracker system.  You have the option of
# running the tracker on several operational models (the system will
# automatically search in the appropriate operational directories for the
# data), or on your own model data.  In either case, the current system
# will ONLY be able to read GRIB data.  To run the tracker, fill in the
# required fields below on the "export" lines, then llsubmit this script.

#     -------------------
#     1. ENTER MODEL TYPE
#     -------------------
#
#     Enter the name of the model that you're running the tracker on.  The
#     tracker is already hard-wired to read the operational GFS, MRF, UKMET,
#     ECMWF, NGM, Early NAM and NAVGEM files in their operational directories
#     on /com.  If your model is one of these, *AND* the data files are still
#     on /com (data on the SP sits on /com for ~10 days), enter one of the
#     following below:  GFS, MRF, UKMET, ECMWF, NGM, NAM, NGPS.  Otherwise,
#     enter OTHER below:
#
#     Example:  export CMODEL=gfs

export CMODEL=${CMODEL:-${RUN}}

#     -------------------
#     2. ENTER FILE NAME
#     -------------------
#
#     If you entered an operational model above, then skip this step, as the
#     script already knows the filename and directory location .  Otherwise,
#     you must enter the full name (include directory) of one of your GRIB
#     forecast files.  Also, your 00h forecast file must have the characters
#     "00" in the name, not just "anl".
#
#     Finally, and this is important, in the export statement below, in the
#     character positions where the forecast hour characters "00" would appear
#     replace those characters with the characters "XX".  This allows the
#     tracker script to manipulate the file names, and to avoid confusion of
#     the forecast hour "00" with the initial hour "00" that may be in a name.
#
#     Example: If the actual name of your datafile is
#              /ptmp/wx20tm/trakdat/ngm.1997110700.pgrb.f00, then enter below:
#
#              export INPFILE=/ptmp/wx20tm/trakdat/ngm.1997110700.pgrb.fXX

export INPFILE=${INPFILE:-}

#     -------------------------------
#     3. ENTER FORECAST HOUR INTERVAL
#     -------------------------------
#
#     If above you entered an operational model from an operational directory,
#     then skip this step, as the script already knows the forecast hour
#     interval.  Otherwise, enter the integer number of hours between
#     successive forecasts.  By the way, there are a couple of caveats:
#      a) Your forecast hours must be evenly spaced (i.e., if you start out
#         with 00, 06, 12,... etc, and then after 48 you skip to 60, the
#         program will stop after 48 hours).
#      b) Currently, a maximum of 14 forecast times can be handled (this is
#         enough to get you 6h forecast intervals from 00 to 78h, or 12h
#         forecast intervals from 00 to 156h).

export FHINT=${FHINT:-03}

#     ------------------------------
#     4. ENTER FORECAST INITIAL TIME
#     ------------------------------
#
#     Enter the starting date, in yyyymmddhh format, of the forecast:

export YYYYMMDDHH=${GDATE10}

#     -------------------------
#     5. ENTER STORM IDENTIFIER
#     -------------------------
#
#     Enter the 3-character identifier for the storm that you want to track
#     (e.g., 06L, 04E, etc...).  If you want all the storms from the input
#     forecast time, then just leave it blank, as "export INPSTORM=".

export INPSTORM=

#     -----------------------------------------
#     6. ENTER NAME OF AUXILIARY TC VITALS FILE
#     -----------------------------------------
#
#     If you are going to use this script to track a storm that has been
#     tracked by an operational hurricane center (NHC, JTWC, etc.), then
#     skip this step, as the TC Vitals files for 1991-1999 are online
#     (except we're currently missing 1992) and will be used.
#     However, if you're trying to track, for example, a midlatitude
#     synoptic storm or a subtropical low, then you need to create a TC
#     Vitals record for that storm that includes the following parameters
#     in the appropriate character positions: 3-character storm ID, Storm
#     name, starting YYMMDD, starting HHMM, initial Lat, initial Lon,
#     direction of storm movement, and speed of storm movement.  (See
#     Tim Marchok or Steve Lord to get the exact format).  NOTE: the
#     online TC Vitals files are the operational files, NOT Best Track.
#     If you want to use Best Track data, include that file name here and
#     the tracker will use it instead of the operational file.  Enter the
#     name of your auxiliary TC Vitals file:
#
#     Example: export AUXTCVIT=/ptmp/wx20tm/ecoast.wint97

export AUXTCVIT=

#     -----------------------------------------------------
#     ENTER THRESHOLDS FOR USE IN VERIFYING STORM STRUCTURE
#     -----------------------------------------------------
#
#     This allows you to enter your own thresholds for mslp gradient and
#     850 mb tangential winds.  These are used in subroutine is_it_a_storm
#     at each forecast hour to verify that the center that you've found
#     at least resembles a storm.  It helps to have this check so that,
#     in the case of weak storms that have dissipated, you don't end up
#     latching onto some weak passing trough.  When this check was not in
#     there in the original version of the tracker, we wound up with some
#     bizarre, jumpy tracks for weak and/or dissipating systems.  In the
#     operational version, the values are hard-wired in as requiring a
#     mslp gradient of at least 1 mb / 200 km (0.005 mb/km), and requiring
#     the average cyclonic tangential winds at 850 mb within a specified
#     radius (the radius depends on each model's grid resolution) to be
#     at least 5 m/s.  If you want different thresholds, then change the
#     default values below....
#
#     Example:
#     export MSLPTHRESH=0.0025
#     export V850THRESH=2.0000

export MSLPTHRESH=0.003
export V850THRESH=3.000

#${scrdir}/gentrak_step2.sh

# Set the default for background files to be 3 hourly
export BKGFREQ=${BKGFREQ:-1}

# This script is the second in a set of 2 that starts the tracker for either
# operational or user-defined model output.  This script makes sure that the
# data files exist, it also pulls all of the needed data records out of the
# various GRIB forecast files and puts them into one, consolidated GRIB file,
# it also runs scripts that read the TC Vitals records for the input day and
# updates the TC Vitals (if necessary).  It then executes the gettrk
# executable, which actually does the tracking.

inpfile=${INPFILE}
fhint=${FHINT}
cmodel=${CMODEL}
symdh=${YYYYMMDDHH}

export TMPDIR=${DATA}/trak/${RANDOM}
mkdir -p "${TMPDIR}"
rm -f "${TMPDIR}/tmpsynvit"
rm -f "${TMPDIR}/tmprawvit"

export flag_pgb=${flag_pgb:-q}

wgrib_parmlist="HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 PRMSL:MSL MSLET"

export maxtime=22 # Max number of forecast time levels

mkdir -p "${vdir}"
mkdir -p "${TMPDIR}"

scc=${symdh:0:2}
syy=${symdh:2:2}
smm=${symdh:4:2}
sdd=${symdh:6:2}
shh=${symdh:8:2}
dishh=${shh}
symd=${symdh:0:8}

#---------------------------------------------------#
# Convert the input model to lowercase letters and
# check to see if it's a valid model, and assign a
# model ID number to it.
#---------------------------------------------------#

cmodel=${cmodel,,}

case ${cmodel} in

    gdas)
        echo " "
        echo " ++ operational GDAS chosen"
        fcsthrs=""
        for fhr in $(seq 0 "${BKGFREQ}" 9); do
            fhrchar=$(printf %02d "${fhr}")
            fcsthrs="${fcsthrs} ${fhrchar}"
        done
        atcfnum=72
        atcfname="gdas"
        if [[ "${BKGFREQ}" -eq 1 ]]; then
            atcffreq=100
        elif [[ "${BKGFREQ}" -eq 3 ]]; then
            atcffreq=300
        fi
        modtyp='global'
        file_sequence="onebig"
        lead_time_units='hours'
        export PHASEFLAG=n
        export WCORE_DEPTH=1.0
        export PHASE_SCHEME=both
        export STRUCTFLAG=n
        export IKEFLAG=n
        export trkrtype='tracker'
        # g2_jpdtn sets the variable that will be used as "JPDTN" for
        # the call to getgb2, if gribver=2.  jpdtn=1 for ens data,
        # jpdtn=0 for deterministic data.
        g2_jpdtn=0
        model=8
        ;;
    gfs)
        echo " "
        echo " ++ operational GFS chosen"
        fcsthrsgfs='00 06 12 18 24 30 36 42 48 54 60 66 72 78'
        gfsdir="${COMIN}"
        gfsgfile=gfs.t${dishh}z.pgrbf
        model=1
        ;;
    mrf)
        echo " "
        echo " ++ operational MRF chosen"
        fcsthrsmrf='00 12 24 36 48 60 72'
        mrfdir="${COMIN}"
        mrfgfile=drfmr.t${dishh}z.pgrbf
        model=2
        ;;
    ukmet)
        echo " "
        echo " ++ operational UKMET chosen"
        fcsthrsukmet='00 12 24 36 48 60 72'
        ukmetdir="${COMIN}"
        ukmetgfile=ukmet.t${dishh}z.ukmet
        model=3
        ;;
    ecmwf)
        echo " "
        echo " ++ operational ECMWF chosen"
        ecmwfdir="${COMIN}"
        ecmwfgfile=ecmgrb25.t12z
        model=4
        ;;
    ngm)
        set +x
        echo " "
        echo " ++ operational NGM chosen"
        set_trace
        fcsthrsngm='00 06 12 18 24 30 36 42 48'
        ngmdir="${COMIN}"
        ngmgfile=ngm.t${dishh}z.pgrb.f
        model=5
        ;;
    nam)
        echo " "
        echo " ++ operational Early NAM chosen"
        fcsthrsnam='00 06 12 18 24 30 36 42 48'
        namdir="${COMIN}"
        namgfile=nam.t${dishh}z.awip32
        model=6
        ;;
    ngps)
        echo " "
        echo " ++ operational NAVGEM chosen"
        fcsthrsngps='00 12 24 36 48 60 72'
        #ngpsdir=/com/hourly/prod/hourly.${symd};
        ngpsdir="${COMIN}"
        ngpsgfile=fnoc.t${dishh}z
        model=7
        ;;
    other)
        cat << EOF
Model selected by user is ${cmodel}, which is a 
user-defined model, NOT operational....
 
EOF
        model=9
        ;;
    *)
        msg=$(
            cat << EOF

FATAL ERROR: Model selected is not recognized.
Model= ---> ${cmodel} <--- ..... Please submit the script again....

EOF
        )
        err=8
        err_exit "${msg}"
        ;;

esac

#-------------------------------------------------
# Initialize the fh array that's used in
# telling the fortran program the number of
# forecast hours and what those forecast
# hours are.  If the model selected is an
# operational model, the tracker already knows
# what those hours are; it only needs this info
# if the model is user-defined.
#-------------------------------------------------

export fh
for ((fct = 1; fct <= 14; fct++)); do
    fh[fct]=99
done

#------------------------------------------------------#
# Set the directories for the operational files.  For
# a user-specified model, we need to process the
# input file to get the necessary info about the
# data directory, etc.....
#------------------------------------------------------#

if [[ "${cmodel}" == 'other' ]]; then

    # This next bit of code tears apart the input file name to get the
    # data directory and data file names.  pos1, pos2, pos3 and pos4
    # refer to character string positions in the filename string.  The
    # idea of this next bit of code is to pull the file name apart to
    # get a shell for the file name, so that if a user has a file
    # name such as pgbf00.97090100, the script knows where in the
    # string to substitute the forecast hours.  IMPORTANT NOTE: The
    # file name that the user enters must have the characters "XX" in
    # in place of the forecast hour characters "00" in order for this
    # to work.
    #
    # pos1= character position immediately before "00" starts
    # pos2= character position at which "00" starts
    # pos3= character position immediately after "00"
    # pos4= character position of last character in name

    otherdir=$(dirname "${inpfile}")
    fname=$(basename "${inpfile}")

    fnamebeg=$(grep -Po '.*(?=XX)' <<< "${fname}")
    fnameend=$(grep -Po '(?<=XX).*($)' <<< "${fname}")

    if [[ -z "${fnamebeg}" ]]; then
        read -r -d '' msg << EOF
FATAL ERROR: Something wrong with name of input file name for the
analysis file.  Either the input file is missing, or you did not
replace the forecast hour characters 00 with XX.  Please check the
name in the kickoff script and qsub it again.
EOF
        err=8
        err_exit "${msg}"
    fi

    fflag='y'
    fhour=0
    fcsthrsother=''
    fhrct=0
    while [[ ${fflag} == 'y' ]]; do

        if [[ "${fhrct}" -eq 14 ]]; then
            cat << EOF

!!! Exiting loop, only processing 14 forecast files ....

EOF
            break
        fi

        fhour=$(printf '%02d' "${fhour}")

        if [[ -s "${otherdir}/${fnamebeg}${fhour}${fnameend}" ]]; then
            maxhour=${fhour}
            fcsthrsother="${fcsthrsother} ${fhour}"
            cat << EOF

+++ Found file ${fnamebeg}${fhour}${fnameend}

EOF
            fhrct=$((fhrct + 1))
        else
            fflag='n'
        fi

        fhour=$((fhour + fhint))

    done

    if [[ ! -s "${otherdir}/${fnamebeg}00${fnameend}" ]]; then
        msg=$(
            cat << EOF

!!! FATAL ERROR in $(basename "$0"):
!!! Input analysis file cannot be found.
!!! The tracker is looking for this file in:  
!!! ---->  ${otherdir}/${fnamebeg}00${fnameend}
!!! Please check the directory to make sure the file
!!! is there and then submit this job again.

EOF
        )
        err=8
        err_exit "${msg}"
    fi

    cat << EOF

Max forecast hour is ${maxhour}
List of forecast hours: ${fcsthrsother}

EOF

    # --------------------------------------------------
    # In order for the fortran program to know how many
    # forecast times there are and what those forecast
    # hours are, we need to include this information in
    # the namelist file.  So first, put this information
    # into an array, then at the end of this script,
    # we'll put it into the namelist file.

    fhour=0
    for ((fcnt = 1; fcnt <= 14; fcnt++)); do
        if [[ "${fhour}" -le "${maxhour}" ]]; then
            fh[fct]=${fhour}
        else
            fh[fct]=99
        fi
        fhour=$((fhour + fhint))
    done
fi

cpfs "${DATA}/tcvitals" "${vdir}/vitals.${symd}${dishh}"

# shellcheck disable=SC2312
grep -v TEST "${vdir}/vitals.${symd}${dishh}" |
    awk 'substr($0,6,1) !~ /[8-9]/ {print $0}' > "${vdir}/tempvit.nonameless"

mv "${vdir}/tempvit.nonameless" "${vdir}/vitals.${symd}${dishh}"

#--------------------------------------------------------------#
# Now run a fortran program that will read all the TC vitals
# records for the current dtg and the dtg from 6h ago, and
# sort out any duplicates.  If the program finds a storm that
# was included in the vitals file 6h ago but not for the current
# dtg, this program updates the 6h-old first guess position
# and puts these updated records as well as the records from
# the current dtg into a temporary vitals file.  It is this
# temporary vitals file that is then used as the input for the
# tracking program.
#--------------------------------------------------------------#

ymdh6ago=$(date --utc +%Y%m%d%H -d "${symd} ${dishh} - 6 hours")
syy6=${ymdh6ago:2:2}
smm6=${ymdh6ago:4:2}
sdd6=${ymdh6ago:6:2}
shh6=${ymdh6ago:8:2}

ymdh6ahead=$(date --utc +%Y%m%d%H -d "${symd} ${dishh} + 6 hours")
syyp6=${ymdh6ahead:2:2}
smmp6=${ymdh6ahead:4:2}
sddp6=${ymdh6ahead:6:2}
shhp6=${ymdh6ahead:8:2}

vit_incr=6

cat << EOF > "${TMPDIR}/suv_input"
&datenowin   dnow%yy=${syy}, dnow%mm=${smm},
             dnow%dd=${sdd}, dnow%hh=${dishh}/
&dateoldin  dold%yy=${syy6}, dold%mm=${smm6},
             dold%dd=${sdd6}, dold%hh=${shh6}/
&datefuturein  dfuture%yy=${syyp6}, dfuture%mm=${smmp6},
               dfuture%dd=${sddp6}, dfuture%hh=${shhp6}/
&hourinfo  vit_hr_incr=${vit_incr}/
EOF

numvitrecs=$(wc -l "${vdir}/vitals.${symd}${dishh}")
if [[ ${numvitrecs} -eq 0 ]]; then
    msg=$(
        cat << EOF

!!! FATAL ERROR: There are no vitals records for this time period.
!!! File ${vdir}/vitals.${symd}${dishh} is empty.
!!! It could just be that there are no storms for the current
!!! time.  Please check the dates and submit this job again....

EOF
    )
    err=8
    err_exit "${msg}"
fi

# - - - - - - - - - - - - -
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of
# which is to convert all the 2-digit years into 4-digit years.
# Beginning 4/21/99, NHC and JTWC will begin sending the vitals
# with 4-digit years, however it is unknown when other global
# forecasting centers will begin using 4-digit years, thus we
# need the following logic to ensure that all the vitals going
# into supvitql.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - -

sed -e "s/\:/ /g" "${vdir}/vitals.${symd}${dishh}" > "${TMPDIR}/tempvit"
mv "${TMPDIR}/tempvit" "${vdir}/vitals.${symd}${dishh}"

awk '
{
  yycheck = substr($0,20,2)
  if ((yycheck == 20 || yycheck == 19) && (length($4) == 8)) {
    printf ("%s\n",$0)
  }
  else {
    if (yycheck >= 0 && yycheck <= 50) {
      printf ("%s20%s\n",substr($0,1,19),substr($0,20))
    }
    else {
      printf ("%s19%s\n",substr($0,1,19),substr($0,20))
    }
  }
} ' "${vdir}/vitals.${symd}${dishh}" > "${TMPDIR}/vitals.${symd}${dishh}.y4"

mv "${TMPDIR}/vitals.${symd}${dishh}.y4" "${vdir}/vitals.${symd}${dishh}"

pgm=$(basename "${SUPVX}")
if [[ -s "${DATA}/prep_step" ]]; then
    source "${DATA}/prep_step"
else
    [[ -f errfile ]] && rm errfile
    export XLFUNITS=0
    # shellcheck disable=SC2046,SC2312
    unset $(env | grep XLFUNIT | awk -F= '{print $1}')
    export XLFRTEOPTS="${XLFRTEOPTS:+${XLFRTEOPTS}:}unit_vars=yes"
fi

rm -f fort.*

${NLN} "${vdir}/vitals.${symd}${dishh}" fort.31
${NLN} "${vdir}/vitals.upd.${cmodel}.${symd}${dishh}" fort.51

${TIMEIT} "${SUPVX}" < "${TMPDIR}/suv_input" > outout 2> errfile
err=$?
cat errfile >> outout
cat outout >> supvit.out
if [[ -n "${pgmout}" ]]; then
    cat outout >> "../${pgmout}"
fi
rm outout
cat << EOF

The foreground exit status for SUPVIT is ${err}

EOF

if [[ "${err}" -eq 0 ]]; then
    cat << EOF

Normal end for program supvitql (which updates TC vitals file).

EOF
else
    msg=$(
        cat << EOF

!!! FATAL ERROR: An error occurred while running supvitql, 
!!! which is the program that updates the TC Vitals file.
!!! Return code from supvitql = ${err}
!!! model= ${cmodel}, forecast initial time = ${symd}${dishh}
!!! Exiting....

EOF
    )
    err=9
    err_exit "${msg}"
fi

#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

numvitrecs=$(wc -l "${vdir}/vitals.upd.${cmodel}.${symd}${dishh}")
if [[ "${numvitrecs}" -eq 0 ]]; then
    cat << EOF
 
!!! ERROR -- There are no vitals records for this time period 
!!! in the UPDATED vitals file.
!!! File ${vdir}/vitals.upd.${cmodel}.${symd}${dishh} is empty.
!!! Please check the dates and submit this job again....
 
EOF
    err=8
    err_exit
fi

# shellcheck disable=SC2312
cat << EOF | tee stormlist



*--------------------------------*
|        STORM SELECTION         |
*--------------------------------*

Below is a list of the storms to be processed: 

$(cat "${vdir}/vitals.upd.${cmodel}.${symd}${dishh}")

EOF

if [[ -n "${pgmout}" ]]; then
    cat storm_list >> "../${pgmout}"
fi

rm storm_list

for ((ict = 1; ict <= 15; ict++)); do
    stormflag[ict]=3
done

dtg_current="${symd} ${dishh}00"
smax=$(grep -c "${dtg_current}" "${vdir}/vitals.upd.${cmodel}.${symd}${dishh}")

for ((ict = 1; ict <= smax; ict++)); do
    stormflag[sct]=1
done

#-----------------------------------------------------------------#
#
#         ------  CUT APART INPUT GRIB FILES  -------
#
# For the selected model, cut apart the GRIB input files in order
# to pull out only the variables that we need for the tracker.
# Put these selected variables from all forecast hours into 1 big
# GRIB file that we'll use as input for the tracker.
#
# The utility /nwprod/util/exec/wgrib is used to cut out the
# needed parms for the GFS, MRF, UKMET and NAVGEM files.
# The utility /nwprod/util/exec/copygb is used to interpolate the
# NGM (polar stereographic) and NAM (Lambert Conformal) data from
# their grids onto lat/lon grids.  Note that while the lat/lon
# grid that I specify overlaps into areas that don't have any data
# on the original grid, Mark Iredell wrote the copygb software so
# that it will mask such "no-data" points with a bitmap (just be
# sure to check the lbms in your fortran program after getgb).
#-----------------------------------------------------------------#

cat << EOF

-----------------------------------------
  NOW CUTTING APART INPUT GRIB FILES TO 
  CREATE 1 BIG GRIB INPUT FILE 
-----------------------------------------

EOF

#grid='255 0 151 71 70000 190000 128 0000 340000 1000 1000 64'
#grid='255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 64'
#grid='255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 0'

regflag=$(grep -c NHC "${vdir}/vitals.upd.${cmodel}.${symd}${dishh}")

# ----------------------------
#   Process NGM, if selected
# ----------------------------

if [[ "${model}" -eq 5 ]]; then

    grid='255 0 151 71 70000 190000 128 0000 340000 1000 1000 64'

    if [[ "${regflag}" == 'n' ]]; then
        cat << EOF

*******************************************************************
!!! NGM model has been selected, but there are no storms in the
!!! TC Vitals file that are from NHC.  Therefore, unless you have
!!! entered your own auxiliary TC vitals file that has a storm 
!!! within the NGM domain, the tracker will exit after reading 
!!! in the analysis data.
*******************************************************************

EOF
    fi

    rm -f "${vdir}/ngmlatlon.pgrb.${symd}${dishh}"

    for fhour in ${fcsthrsngm}; do

        if [[ ! -s "${ngmdir}/${ngmgfile}${fhour}" ]]; then
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! NGM File missing: ${ngmdir}/${ngmgfile}${fhour}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EOF
            continue
        fi
        rm -f "${TMPDIR}/tmpixfile"
        ${GRBINDEX:?} "${ngmdir}/${ngmgfile}${fhour}" "${TMPDIR}/tmpixfile"
        x1="${TMPDIR}/tmpixfile"

        cat << EOF

Extracting NGM GRIB data for forecast hour = ${fhour}

EOF

        g1="${ngmdir}/${ngmgfile}${fhour}"

        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1 33 100 850' "${g1}" "${x1}" "${TMPDIR}/ngmllu850.grb.f${fhour}"
        rcc1=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1 33 100 700' "${g1}" "${x1}" "${TMPDIR}/ngmllu700.grb.f${fhour}"
        rcc2=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1 33 100 500' "${g1}" "${x1}" "${TMPDIR}/ngmllu500.grb.f${fhour}"
        rcc3=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1 33 105 10' "${g1}" "${x1}" "${TMPDIR}/ngmllu10m.grb.f${fhour}"
        rcc4=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1 41 100 850' "${g1}" "${x1}" "${TMPDIR}/ngmllav850.grb.f${fhour}"
        rcc5=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1 41 100 700' "${g1}" "${x1}" "${TMPDIR}/ngmllav700.grb.f${fhour}"
        rcc6=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1  7 100 850' "${g1}" "${x1}" "${TMPDIR}/ngmllz850.grb.f${fhour}"
        rcc7=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1  7 100 700' "${g1}" "${x1}" "${TMPDIR}/ngmllz700.grb.f${fhour}"
        rcc8=$?
        ${COPYGB:?} -g"${grid}" -k'2*-1 104 -1  2 102 0' "${g1}" "${x1}" "${TMPDIR}/ngmllmslp.grb.f${fhour}"
        rcc9=$?

        if [[ ${rcc1} -eq 134 || ${rcc2} -eq 134 || ${rcc3} -eq 134 || ${rcc4} -eq 134 || ${rcc5} -eq 134 ||
            ${rcc6} -eq 134 || ${rcc7} -eq 134 || ${rcc8} -eq 134 || ${rcc9} -eq 134 ]]; then
            msg=$(
                cat << EOF

!!! FATAL ERROR using ${COPYGB} to interpolate ngm data.  We will stop execution because
!!! some variables may have been copied okay, while some obviously have not, 
!!! and that could lead to unreliable results from the tracker.  Check to make
!!! sure you've allocated enough memory for this job (error 134 using ${COPYGB} is 
!!! typically due to using more memory than you've allocated).  Exiting.....

EOF
            )
            err=8
            err_exit "${msg}"
        fi

        cat "${TMPDIR}/ngmllu850.grb.f${fhour}" "${TMPDIR}/ngmllu700.grb.f${fhour}" \
            "${TMPDIR}/ngmllu500.grb.f${fhour}" "${TMPDIR}/ngmllz850.grb.f${fhour}" \
            "${TMPDIR}/ngmllz700.grb.f${fhour}" "${TMPDIR}/ngmllmslp.grb.f${fhour}" \
            "${TMPDIR}/ngmllav850.grb.f${fhour}" "${TMPDIR}/ngmllav700.grb.f${fhour}" \
            "${TMPDIR}/ngmllu10m.grb.f${fhour}" \
            >> "${vdir}/ngmlatlon.pgrb.${symd}${dishh}"

    done

    ${GRBINDEX:?} "${vdir}/ngmlatlon.pgrb.${symd}${dishh}" "${vdir}/ngmlatlon.pgrb.ix.${symd}${dishh}"
    gribfile="${vdir}/ngmlatlon.pgrb.${symd}${dishh}"
    ixfile="${vdir}/ngmlatlon.pgrb.ix.${symd}${dishh}"

fi

# ----------------------------------
#   Process Early NAM, if selected
# ----------------------------------

if [[ "${model}" -eq 6 ]]; then

    grid='255 0 301 141 70000 190000 128 0000 340000  500  500 64'

    if [[ "${regflag}" == 'n' ]]; then
        cat << EOF

*******************************************************************
!!! NAM model has been selected, but there are no storms in the
!!! TC Vitals file that are from NHC.  Therefore, unless you have
!!! entered your own auxiliary TC vitals file that has a storm 
!!! within the NAM domain, the tracker will exit after reading 
!!! in the analysis data.
*******************************************************************

EOF
    fi

    rm -f "${vdir}/namlatlon.pgrb.${symd}${dishh}"

    for fhour in ${fcsthrsnam}; do
        if [[ ! -s "${namdir}/${namgfile}${fhour}.tm00" ]]; then
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Early NAM File missing: ${namdir}/${namgfile}${fhour}.tm00
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EOF
            continue
        fi
        rm -f "${TMPDIR}/tmpixfile"
        ${GRBINDEX:?} "${namdir}/${namgfile}${fhour}.tm00" "${TMPDIR}/tmpixfile"
        x1="${TMPDIR}/tmpixfile"

        cat << EOF

Extracting Early NAM GRIB data for forecast hour = ${fhour}

EOF

        g1="${namdir}/${namgfile}${fhour}.tm00"

        rm -f "${TMPDIR}/namlatlon.pgrb"
        ${COPYGB:?} -g"${grid}" -k'4*-1 33 100 850' "${g1}" "${x1}" "${TMPDIR}/namllu850.grb.f${fhour}"
        rcc1=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1 33 100 700' "${g1}" "${x1}" "${TMPDIR}/namllu700.grb.f${fhour}"
        rcc2=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1 33 100 500' "${g1}" "${x1}" "${TMPDIR}/namllu500.grb.f${fhour}"
        rcc3=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1 33 105 10' "${g1}" "${x1}" "${TMPDIR}/namllu10m.grb.f${fhour}"
        rcc4=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1 41 100 850' "${g1}" "${x1}" "${TMPDIR}/namllav850.grb.f${fhour}"
        rcc5=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1 41 100 700' "${g1}" "${x1}" "${TMPDIR}/namllav700.grb.f${fhour}"
        rcc6=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1  7 100 850' "${g1}" "${x1}" "${TMPDIR}/namllz850.grb.f${fhour}"
        rcc7=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1  7 100 700' "${g1}" "${x1}" "${TMPDIR}/namllz700.grb.f${fhour}"
        rcc8=$?
        ${COPYGB:?} -g"${grid}" -k'4*-1  2 102   0' "${g1}" "${x1}" "${TMPDIR}/namllmslp.grb.f${fhour}"
        rcc9=$?

        if [[ "${rcc1}" -eq 134 || "${rcc2}" -eq 134 || "${rcc3}" -eq 134 || "${rcc4}" -eq 134 || "${rcc5}" -eq 134 ||
            "${rcc6}" -eq 134 || "${rcc7}" -eq 134 || "${rcc8}" -eq 134 || "${rcc9}" -eq 134 ]]; then
            cat << EOF

!!! FATAL ERROR using ${COPYGB} to interpolate nam data.  We will stop execution because
!!! some variables may have been copied okay, while some obviously have not, 
!!! and that could lead to unreliable results from the tracker.  Check to make
!!! sure you've allocated enough memory for this job (error 134 using ${COPYGB} is 
!!! typically due to using more memory than you've allocated).  Exiting.....

EOF
            err=8
            err_exit
        fi

        cat "${TMPDIR}/namllu850.grb.f${fhour}" "${TMPDIR}/namllu700.grb.f${fhour}" \
            "${TMPDIR}/namllu500.grb.f${fhour}" "${TMPDIR}/namllz850.grb.f${fhour}" \
            "${TMPDIR}/namllz700.grb.f${fhour}" "${TMPDIR}/namllmslp.grb.f${fhour}" \
            "${TMPDIR}/namllav850.grb.f${fhour}" "${TMPDIR}/namllav700.grb.f${fhour}" \
            "${TMPDIR}/namllu10m.grb.f${fhour}" \
            >> "${vdir}/namlatlon.pgrb.${symd}${dishh}"

    done

    ${GRBINDEX:?} "${vdir}/namlatlon.pgrb.${symd}${dishh}" "${vdir}/namlatlon.pgrb.ix.${symd}${dishh}"
    gribfile="${vdir}/namlatlon.pgrb.${symd}${dishh}"
    ixfile="${vdir}/namlatlon.pgrb.ix.${symd}${dishh}"

fi

# ------------------------------
#   Process ECMWF, if selected
# ------------------------------

# ECMWF is not a regional grid, however they currently (6/98) only send us the
# global belt from 35S to 35N.  Thus, it will have grid boundaries that may
# interfere with the tracking algorithm.  It is crucial to the proper
# functioning of the tracking program to give any regional grid dataset a
# buffer zone around the grid boundaries, with null values in that buffer
# zone that are bitmapped out.  That's why we use Mark Iredell's grib
# interpolater here, to add a 5 degree buffer zone to the north and south of
# the ECMWF grid boundary; his interpolater adds the null values in the
# bitmap surrounding the area with valid data.  If ECMWF begins sending us
# the entire global data set, then this bit of code should
# be taken out, and the data should then be processed as the other normal
# full-coverage global models (ukmet, mrf, gfs, NAVGEM) currently are.

if [[ "${model}" -eq 4 ]]; then
    if [[ ! -s "${ecmwfdir}/${ecmwfgfile}" ]]; then
        msg=$(
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! ECMWF GRIB or Index File missing from directory: ${ecmwfdir}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! Due to missing ECMWF file, execution is ending....

EOF
        )
        err=8
        err_exit "${msg}"
    fi

    ${GRBINDEX:?} "${ecmwfdir}/${ecmwfgfile}" "${TMPDIR}/${ecmwfgfile}.ix"
    x1="${TMPDIR}/${ecmwfgfile}.ix"

    rm -f "${vdir}/ecmwf.bufzone.grb.${symd}${dishh}"
    rm -f "${vdir}/ecmwf.bufzone.ix.${symd}${dishh}"

    g1="${ecmwfdir}/${ecmwfgfile}"
    ecgrid='255 0 144 33 40000 0000 128 -40000 357500 2500 2500 64'
    ${COPYGB:?} -g"${ecgrid}" "${g1}" "${x1}" "${vdir}/ecmwf.bufzone.grb.${symd}${dishh}"
    ${GRBINDEX:?} "${vdir}/ecmwf.bufzone.grb.${symd}${dishh}" "${vdir}/ecmwf.bufzone.ix.${symd}${dishh}"
    gribfile="${vdir}/ecmwf.bufzone.grb.${symd}${dishh}"
    ixfile="${vdir}/ecmwf.bufzone.ix.${symd}${dishh}"

fi

# ------------------------------
#   Process GFS, if selected
# ------------------------------

if [[ "${model}" -eq 1 ]]; then

    rm -f "${vdir}/gfsgribfile.${symd}${dishh}"

    for fhour in ${fcsthrsgfs}; do
        if [[ ! -s "${gfsdir}/${gfsgfile}${fhour}" ]]; then
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! GFS File missing: ${gfsdir}/${gfsgfile}${fhour}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EOF
            continue
        fi

        gfile="${gfsdir}/${gfsgfile}${fhour}"
        ${WGRIB:?} -s "${gfile}" > "${TMPDIR}/gfs.ix"

        for parm in ${wgrib_parmlist}; do
            case "${parm}" in
                "SurfaceU") parm="UGRD:10 m" ;;
                "SurfaceV") parm="VGRD:10 m" ;;
                *) ;;
            esac
            # shellcheck disable=SC2312
            grep "${parm}" "${TMPDIR}/gfs.ix" | ${WGRIB:?} -s "${gfile}" -i -grib -append \
                -o "${vdir}/gfsgribfile.${symd}${dishh}"
        done

    done

    ${GRBINDEX:?} "${vdir}/gfsgribfile.${symd}${dishh}" "${vdir}/gfsixfile.${symd}${dishh}"
    gribfile="${vdir}/gfsgribfile.${symd}${dishh}"
    ixfile="${vdir}/gfsixfile.${symd}${dishh}"

fi

# ------------------------------
#   Process GDAS, if selected
# ------------------------------

if [[ "${model}" -eq 8 ]]; then

    export nest_type="fixed"
    export trkrebd=360.0
    export trkrwbd=0.0
    export trkrnbd=85.0
    export trkrsbd=-85.0
    rundescr="xxxx"
    atcfdescr="xxxx"

    rm -f "${vdir}/gdasgribfile.${symd}${dishh}"

    if [[ "${gribver}" -eq 1 ]]; then

        # Use GRIB1 input data

        for ((fhr = -6; fhr <= 3; fhr += BKGFREQ)); do
            if [[ "${fhr}" -lt 0 ]]; then
                fpref="pgm$((-fhr))"
            elif [[ "${fhr}" -eq 0 ]]; then
                fpref=pges
            elif [[ "${fhr}" -gt 0 ]]; then
                fpref="pgp${fhr}"
            fi
            gfile="${DATA}/${fpref}prep"

            if [[ ! -s "${gfile}" ]]; then
                cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! gdas File missing: ${gfile}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
                continue
            fi

            ${WGRIB:?} -s "${gfile}" > "${TMPDIR}/gdas.ix"

            for parm in ${wgrib_parmlist}; do
                case "${parm}" in
                    "SurfaceU") parm="UGRD:10 m" ;;
                    "SurfaceV") parm="VGRD:10 m" ;;
                    *) ;;
                esac
                # shellcheck disable=SC2312
                grep "${parm}" "${TMPDIR}/gdas.ix" | ${WGRIB:?} -s "${gfile}" -i -grib -append \
                    -o "${vdir}/gdasgribfile.${symd}${dishh}"
            done

        done

    else

        # Use GRIB2 input data....
        for ((fhr = -6; fhr <= 3; fhr += BKGFREQ)); do
            if [[ "${fhr}" -lt 0 ]]; then
                fhour="0$((-fhr))"
                fpref=pgm$((-fhr))
            elif [[ "${fhr}" -eq 0 ]]; then
                fhour=00
                fpref=pges
            elif [[ "${fhr}" -gt 0 ]]; then
                fhour="0${fhr}"
                fpref="pgp${fhr}"
            fi
            gfile="${DATA}/${fpref}prep"

            if [[ ! -s "${gfile}" ]]; then
                cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! gdas File missing: ${gfile}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
                continue
            fi

            ${WGRIB2:?} -s "${gfile}" > "${TMPDIR}/gdas.ix"

            for parm in ${wgrib_parmlist}; do
                case "${parm}" in
                    "SurfaceU") parm="UGRD:10 m" ;;
                    "SurfaceV") parm="VGRD:10 m" ;;
                    *) ;;
                esac
                # shellcheck disable=SC2312
                grep "${parm}" "${TMPDIR}/gdas.ix" | ${WGRIB2:?} -i "${gfile}" -append -grib \
                    "${vdir}/gdasgribfile.${symd}${dishh}"
            done

        done

    fi

    if [[ "${gribver}" -eq 1 ]]; then
        ${GRBINDEX:?} "${vdir}/gdasgribfile.${symd}${dishh}" "${vdir}/gdasixfile.${symd}${dishh}"
    else
        ${GRB2INDEX:?} "${vdir}/gdasgribfile.${symd}${dishh}" "${vdir}/gdasixfile.${symd}${dishh}"
    fi

    gribfile="${vdir}/gdasgribfile.${symd}${dishh}"
    ixfile="${vdir}/gdasixfile.${symd}${dishh}"

fi

# ------------------------------
#   Process MRF, if selected
# ------------------------------

if [[ "${model}" -eq 2 ]]; then

    rm -f "${vdir}/mrfgribfile.${symd}${dishh}"

    for fhour in ${fcsthrsmrf}; do
        if [[ ! -s "${mrfdir}/${mrfgfile}${fhour}" ]]; then
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! MRF File missing: ${mrfdir}/${mrfgfile}${fhour}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
            continue
        fi

        gfile="${mrfdir}/${mrfgfile}${fhour}"
        ${WGRIB:?} -s "${gfile}" > "${TMPDIR}/mrf.ix"

        for parm in ${wgrib_parmlist}; do
            case "${parm}" in
                "SurfaceU") parm="UGRD:10 m" ;;
                "SurfaceV") parm="VGRD:10 m" ;;
                *) ;;
            esac
            # shellcheck disable=SC2312
            grep "${parm}" "${TMPDIR}/mrf.ix" | ${WGRIB:?} -s "${gfile}" -i -grib -append \
                -o "${vdir}/mrfgribfile.${symd}${dishh}"
        done

    done

    ${GRBINDEX:?} "${vdir}/mrfgribfile.${symd}${dishh}" "${vdir}/mrfixfile.${symd}${dishh}"
    gribfile="${vdir}/mrfgribfile.${symd}${dishh}"
    ixfile="${vdir}/mrfixfile.${symd}${dishh}"

fi

# ------------------------------
#   Process UKMET, if selected
# ------------------------------

if [[ "${model}" -eq 3 ]]; then

    rm -f "${vdir}/ukmetgribfile.${symd}${dishh}"

    wgrib_parmlist='HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 UGRD:sfc VGRD:sfc ABSV:850 ABSV:700 PRMSL:MSL '

    for fhour in ${fcsthrsukmet}; do
        if [[ ! -s "${ukmetdir}/${ukmetgfile}${fhour}" ]]; then
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! UKMET File missing: ${ukmetdir}/${ukmetgfile}${fhour}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
            continue
        fi

        gfile="${ukmetdir}/${ukmetgfile}${fhour}"
        ${WGRIB:?} -s "${gfile}" > "${TMPDIR}/ukmet.ix"

        for parm in ${wgrib_parmlist}; do
            # shellcheck disable=SC2312
            grep "${parm}" "${TMPDIR}/ukmet.ix" | ${WGRIB:?} -s "${gfile}" -i -grib -append \
                -o "${vdir}/ukmetgribfile.${symd}${dishh}"
        done

    done

    ${GRBINDEX:?} "${vdir}/ukmetgribfile.${symd}${dishh}" "${vdir}/ukmetixfile.${symd}${dishh}"
    gribfile="${vdir}/ukmetgribfile.${symd}${dishh}"
    ixfile="${vdir}/ukmetixfile.${symd}${dishh}"

fi

# ------------------------------
#   Process NAVGEM, if selected
# ------------------------------

if [[ "${model}" -eq 7 ]]; then

    rm -f "${vdir}/ngpsgribfile.${symd}${dishh}"

    if [[ ! -s "${ngpsdir}/${ngpsgfile}" ]]; then
        msg=$(
            cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! NAVGEM File missing: ${ngpsdir}/${ngpsgfile}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! Due to missing NAVGEM file, execution is ending....

EOF
        )
        err=8
        err_exit "${msg}"
    fi

    gfile="${ngpsdir}/${ngpsgfile}"
    ${WGRIB:?} -s "${gfile}" > "${TMPDIR}/ngps.ix"

    for fhour in ${fcsthrsngps}; do
        if [[ "${fhour}" == '00' ]]; then
            vtstring=":anl:"
        else
            vtstring="${fhour}hr"
        fi

        for parm in ${wgrib_parmlist}; do
            case "${parm}" in
                "SurfaceU") parm="UGRD:19 m" ;;
                "SurfaceV") parm="VGRD:19 m" ;;
                *) ;;
            esac
            # shellcheck disable=SC2312
            grep "${parm}" "${TMPDIR}/ngps.ix" | grep "${vtstring}" |
                ${WGRIB:?} -s "${gfile}" -i -grib -append -o "${vdir}/ngpsgribfile.${symd}${dishh}"
        done

    done

    ${GRBINDEX:?} "${vdir}/ngpsgribfile.${symd}${dishh}" "${vdir}/ngpsixfile.${symd}${dishh}"
    gribfile="${vdir}/ngpsgribfile.${symd}${dishh}"
    ixfile="${vdir}/ngpsixfile.${symd}${dishh}"

fi

# ---------------------------------------------
#   Process User-specified Model, if selected
# ---------------------------------------------

if [[ "${model}" -eq 9 ]]; then

    # We need to first check whether or not the data in the file are stored
    # on a lat/lon grid or not.  We do this by scanning the analysis file
    # with Wesley's grib utility, and checking the value of the "Data
    # Representation Type", which is stored in byte #6 in the GDS of each
    # grib file.  A value of 0 indicates an equidistant lat/lon grid.

    rm -f "${vdir}/otherlatlon.pgrb.${symdh}"

    # shellcheck disable=SC2312
    gridtyp=$(${WGRIB:?} -GDS10 "${otherdir}/${fnamebeg}00${fnameend}" |
        awk -FGDS10= '{print $2}' | awk '{print $6}' | sed -n 1p)

    if [[ "${gridtyp}" -eq 0 ]]; then

        #   The data are already on a lat/lon grid, we do not need to
        #   interpolate the data, just pull out the records that we need
        #   using wgrib.

        for fhour in ${fcsthrsother}; do
            if [[ ! -s "${otherdir}/${fnamebeg}${fhour}${fnameend}" ]]; then
                cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Forecast File missing: ${otherdir}/${fnamebeg}00${fnameend}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
                continue
            fi

            gfile="${otherdir}/${fnamebeg}${fhour}${fnameend}"
            ${WGRIB:?} -s "${gfile}" > "${TMPDIR}/other.ix"

            for parm in ${wgrib_parmlist}; do
                case "${parm}" in
                    "SurfaceU") parm="UGRD:10 m" ;;
                    "SurfaceV") parm="VGRD:10 m" ;;
                    *) ;;
                esac
                # shellcheck disable=SC2312
                grep "${parm}" "${TMPDIR}/other.ix" | ${WGRIB:?} -s "${gfile}" -i -grib -append \
                    -o "${vdir}/otherlatlon.pgrb.${symdh}"
            done

        done

    else

        #   The data are on a grid that is something other than a lat/lon grid.
        #   Use Mark Iredell's interpolator to interpolate the data to a lat/lon
        #   grid and pull out the records that we need.

        othergrid='255 0 360 181 90000 0000 128 -90000 -1000 1000 1000 64'

        for fhour in ${fcsthrsother}; do

            if [[ ! -s "${otherdir}/${fnamebeg}${fhour}${fnameend}" ]]; then
                cat << EOF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Forecast File missing: ${otherdir}/${fnamebeg}00${fnameend}
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

EOF
                continue
            fi

            rm -f "${TMPDIR}/tmpixfile"
            ${GRBINDEX:?} "${otherdir}/${fnamebeg}${fhour}${fnameend}" "${TMPDIR}/tmpixfile"
            x1="${TMPDIR}/tmpixfile"

            g1="${otherdir}/${fnamebeg}${fhour}${fnameend}"

            ${COPYGB:?} -g"${othergrid}" -k'4*-1 33 100 850' "${g1}" "${x1}" "${TMPDIR}/otherllu850.grb.f${fhour}"
            rcc1=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1 33 100 700' "${g1}" "${x1}" "${TMPDIR}/otherllu700.grb.f${fhour}"
            rcc2=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1 33 100 500' "${g1}" "${x1}" "${TMPDIR}/otherllu500.grb.f${fhour}"
            rcc3=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1 33 105 10' "${g1}" "${x1}" "${TMPDIR}/otherllu10m.grb.f${fhour}"
            rcc4=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1 41 100 850' "${g1}" "${x1}" "${TMPDIR}/otherllav850.grb.f${fhour}"
            rcc5=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1 41 100 700' "${g1}" "${x1}" "${TMPDIR}/otherllav700.grb.f${fhour}"
            rcc6=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1  7 100 850' "${g1}" "${x1}" "${TMPDIR}/otherllz850.grb.f${fhour}"
            rcc7=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1  7 100 700' "${g1}" "${x1}" "${TMPDIR}/otherllz700.grb.f${fhour}"
            rcc8=$?
            ${COPYGB:?} -g"${othergrid}" -k'4*-1  2 102   0' "${g1}" "${x1}" "${TMPDIR}/otherllmslp.grb.f${fhour}"
            rcc9=$?

            if [[ "${rcc1}" -eq 134 || "${rcc2}" -eq 134 || "${rcc3}" -eq 134 || "${rcc4}" -eq 134 || "${rcc5}" -eq 134 ||
                "${rcc6}" -eq 134 || "${rcc7}" -eq 134 || "${rcc8}" -eq 134 || "${rcc9}" -eq 134 ]]; then
                msg=$(
                    cat << EOF

!!! FATAL ERROR using ${COPYGB} to interpolate data.  We will stop execution because
!!! some variables may have been copied okay, while some obviously have not, 
!!! and that could lead to unreliable results from the tracker.  Check to make
!!! sure you've allocated enough memory for this job (error 134 using ${COPYGB} is 
!!! typically due to using more memory than you've allocated).  Exiting.....

EOF
                )
                err=8
                err_exit "${msg}"
            fi

            cat "${TMPDIR}/otherllu850.grb.f${fhour}" "${TMPDIR}/otherllu700.grb.f${fhour}" \
                "${TMPDIR}/otherllu500.grb.f${fhour}" "${TMPDIR}/otherllz850.grb.f${fhour}" \
                "${TMPDIR}/otherllz700.grb.f${fhour}" "${TMPDIR}/otherllmslp.grb.f${fhour}" \
                "${TMPDIR}/otherllav850.grb.f${fhour}" "${TMPDIR}/otherllav700.grb.f${fhour}" \
                "${TMPDIR}/otherllu10m.grb.f${fhour}" \
                >> "${vdir}/otherlatlon.pgrb.${symdh}"

        done

    fi

    ${GRBINDEX:?} "${vdir}/otherlatlon.pgrb.${symdh}" "${vdir}/otherlatlon.pgrb.ix.${symdh}"
    gribfile="${vdir}/otherlatlon.pgrb.${symdh}"
    ixfile="${vdir}/otherlatlon.pgrb.ix.${symdh}"

fi

#-------------------------------------------#
#   Now qsub the jobs to run the tracker    #
#-------------------------------------------#

for ((ist = 1; ist <= 15; ist++)); do
    if [[ "${stormflag[ist]}" -ne 1 ]]; then
        echo "Storm number ${ist} NOT selected for processing"
    else
        echo "Storm number ${ist} IS selected for processing...."
    fi
done

# Load the forecast hours for this particular model into an array
# that will be passed into the executable via a namelist....

for ((ifh = 1; ifh <= mattime; ifh++)); do
    fh[ifh]=$(echo "${fcsthrs}" | awk '{print $n}' n="${ifh}")
done

namelist="${vdir}/gettrk.input.${cmodel}.${symdh}"

export atcfymdh="${scc}${syy}${smm}${sdd}${shh}"

contour_interval=100.0
write_vit=n
want_oci=.TRUE.

cat << EOF > "${namelist}"
&datein inp%bcc=${scc},inp%byy=${syy},inp%bmm=${smm},
        inp%bdd=${sdd},inp%bhh=${shh},inp%model=${model},
        inp%modtyp='${modtyp}',
        inp%lt_units='${lead_time_units}',
        inp%file_seq='${file_sequence}',
        inp%nesttyp='${nest_type}'/
&atcfinfo atcfnum=${atcfnum},atcfname='${atcfname^^}',
          atcfymdh=${atcfymdh},atcffreq=${atcffreq}/
&trackerinfo trkrinfo%westbd=${trkrwbd},
      trkrinfo%eastbd=${trkrebd},
      trkrinfo%northbd=${trkrnbd},
      trkrinfo%southbd=${trkrsbd},
      trkrinfo%type='${trkrtype}',
      trkrinfo%mslpthresh=${MSLPTHRESH},
      trkrinfo%v850thresh=${V850THRESH},
      trkrinfo%gridtype='${modtyp}',
      trkrinfo%contint=${contour_interval},
      trkrinfo%want_oci=${want_oci},
      trkrinfo%out_vit='${write_vit}',
      trkrinfo%gribver=${gribver},
      trkrinfo%g2_jpdtn=${g2_jpdtn}/
&phaseinfo phaseflag='${PHASEFLAG}',
           phasescheme='${PHASE_SCHEME}',
           wcore_depth=${WCORE_DEPTH}/
&structinfo structflag='${STRUCTFLAG}',
            ikeflag='${IKEFLAG}'/
&fnameinfo  gmodname='${atcfname}',
            rundescr='${rundescr}',
            atcfdescr='${atcfdescr}'/
&verbose verb=3/
&waitinfo use_waitfor='n',
          wait_min_age=10,
          wait_min_size=100,
          wait_max_wait=1800,
          wait_sleeptime=5,
          per_fcst_command=''/
EOF

pgm=$(basename "${GETTX}")
if [[ -s "${DATA}/prep_step" ]]; then
    source "${DATA}/prep_step"
else
    rm -f errfile
    export XLFUNITS=0
    # shellcheck disable=SC2046,SC2312
    unset $(env | grep XLFUNIT | awk -F= '{print $1}')
    export XLFRTEOPTS="${XLFRTEOPTS:+${XLFRTEOPTS}:}unit_vars=yes"
fi

touch "${vdir}/tmp.gfs.atcfunix.${symdh}"

#----------------------------------

rm -f fort.*

${NLN} "${gribfile}" fort.11
${NLN} "${vdir}/tmp.gfs.atcfunix.${symdh}" fort.14
${NLN} "${vdir}/vitals.upd.${cmodel}.${symd}${dishh}" fort.12
${NLN} "${ixfile}" fort.31
${NLN} "${vdir}/trak.${cmodel}.all.${symdh}" fort.61
${NLN} "${vdir}/trak.${cmodel}.atcf.${symdh}" fort.62
${NLN} "${vdir}/trak.${cmodel}.radii.${symdh}" fort.63
${NLN} "${vdir}/trak.${cmodel}.atcfunix.${symdh}" fort.64

if [[ "${BKGFREQ}" -eq 1 ]]; then
    ${NLN} "${FIXgfs}/am/${cmodel}.tracker_leadtimes_hrly" "fort.15"
elif [[ "${BKGFREQ}" -eq 3 ]]; then
    ${NLN} "${FIXgfs}/am/${cmodel}.tracker_leadtimes" "fort.15"
fi

${TIMEIT} "${APRNGETTX}" "${GETTX}" < "${namelist}" > outout 2> errfile
err=$?
cat errfile >> outout
cat outout > "${DATA}/model_track.out"
if [[ -n "${pgmout}" ]]; then
    cat outout >> "../${pgmout}"
fi
rm outout
cat << EOF

"The foreground exit status for GETTRK is ${err}"

EOF

if [[ "${err}" -gt 0 ]]; then
    err=9
    err_exit
fi

rm -f fort.*

cpreq "${vdir}/trak.${cmodel}.all.${symdh}" "${DATA}/model_track.all"

exit 0

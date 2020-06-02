#!/bin/ksh

#module load ics
export PS4='+t+$SECONDS extrkr.sh:$LINENO -- '

userid=$LOGNAME

set +x
##############################################################################
cat<<EOF
 
------------------------------------------------
xxxx - Track vortices in model GRIB output
------------------------------------------------
History: Mar 1998 - Marchok - First implementation of this new script.
         Apr 1999 - Marchok - Modified to allow radii output file and
                              to allow reading of 4-digit years from
                              TC vitals file.
         Oct 2000 - Marchok - Fixed bugs: (1) copygb target grid scanning mode
                              flag had an incorrect value of 64 (this prevented
                              NAM, NGM and ECMWF from being processed correctly); 
                              Set it to 0.  (2) ECMWF option was using the 
                              incorrect input date (today's date instead of 
                              yesterday's).
         Jan 2001 - Marchok - Hours now listed in script for each model and 
                              passed into program.  Script included to process
                              GFDL & Ensemble data.  Call to DBN included to 
                              pass data to OSO and the Navy.  Forecast length
                              extended to 5 days for GFS & MRF.
         Aug 2005 - Marchok - Added ability to process ECMWF global, ECMWF
                              hires out to 240 (every 12h), CMC hires, CMC
                              global, GFS extended from 126h to 180h.
         May 2006 - Wobus -   For 2006 NCEP global implementation, changed
                              directory names.
         Jun 2006 - Marchok - Changed handling of NCEP global files beyond
                              180h.  These are now 1-deg instead of 2.5-deg,
                              so there is no longer a need to interpolate 
                              down to 1-deg for these files.  Also, changed
                              the COM directory for CMC.
         Jun 2013 - Magee   - Replaced NOGAPS with NAVGEM. 
         May 2014 - Marchok/QFLiu - Added GRIB2 support.  Added \$gribver
                              variable to switch between GRIB versions.
         May 2014 - Trahan  - Sped up script, added 252 hour forecast GFS

                    In the event of a crash, you can contact Tim 
                    Marchok at GFDL at (609) 452-6534 or tpm@gfdl.gov

Current time is: $( date )
EOF
##############################################################################
set -x

##############################################################################
#
#    FLOW OF CONTROL
#
# 1. Define data directories and file names for the input model 
# 2. Process input starting date/cycle information
# 3. Update TC Vitals file and select storms to be processed
# 4. Cut apart input GRIB files to select only the needed parms and hours
# 5. Execute the tracker
# 6. Copy the output track files to various locations
#
##############################################################################

prep_step=${prep_step:-prep_step}
postmsg=${postmsg:-postmsg}

########################################
msg="has begun for ${cmodel} at ${CYL}z"
$postmsg "$jlogfile" "$msg"
########################################

# This script runs the hurricane tracker using operational GRIB model output.  
# This script makes sure that the data files exist, it then pulls all of the 
# needed data records out of the various GRIB forecast files and puts them 
# into one, consolidated GRIB file, and then runs a program that reads the TC 
# Vitals records for the input day and updates the TC Vitals (if necessary).
# It then runs gettrk, which actually does the tracking.
# 
# Environmental variable inputs needed for this scripts:
#  gribver -- GRIB version: 1=GRIB1, 2=GRIB2.  New in 2014!!
#  PDY   -- The date for data being processed, in YYYYMMDD format
#  CYL   -- The numbers for the cycle for data being processed (00, 06, 12, 18)
#  cmodel -- Model being processed (gfs, mrf, ukmet, ecmwf, nam, ngm, ngps,
#                                   gdas, gfdl, ens (ncep global))
#  envir -- 'prod' or 'test'
#  SENDCOM -- 'YES' or 'NO'
#  SENDNHC -- 'YES' or 'NO' -- send tracks to NHC deck areas.
#             If unspecified, will default to $SENDCOM
#  stormenv -- This is only needed by the tracker run for the GFDL model.
#              'stormenv' contains the name/id that is used in the input
#              grib file names.
# For testing script interactively in non-production set following vars:
#     gfsvitdir  - Directory for GFS Error Checked Vitals
#     namvitdir  - Directory for NAM Error Checked Vitals
#     gltrkdir   - Directory for output tracks
#     homesyndir - Directory with syndir scripts/exec/fix 
#     archsyndir - Directory with syndir scripts/exec/fix 
#

bad_hour=-99  # special forecast hour for "stop processing"

qid=$$
#----------------------------------------------#
#   Get input date information                 #
#----------------------------------------------#

#export PDY=` echo $ymdh | cut -c1-8`
#export CYL=` echo $ymdh | cut -c9-10`

# ----------------------------------------------------------------------
# Allow overriding of later decisions on track input times based on
# optional --gfs-last-hour argument to this script.  Also allow
# waiting for data via --wait-for-data NN (where NN is seconds to wait
# for each file).

override_fcsthrs=""
override_fcstlen=""
override_atcffreq=1200
wait_max_time=0
sleep_time=20 # sleep time between retries
gfsfhout=${FHOUT:-3}
gfs_times='0 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99 102 105 108 111 114 117 120 123 126 129 132 135 138 141 144 147 150 153 156 159 162 165 168 171 174 177 180 183 186 189 192 198 204 210 216 222 228 234 240 252 264 276 288 300 312 324 336 348 360 372 384'
if [ $gfsfhout -eq 6 ]; then
 gfs_times='0 6 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126 132 138 144 150 156 162 168 174 180 186 192 198 204 210 216 222 228 234 240 252 264 276 288 300 312 324 336 348 360 372 384'
fi

gdas_times='0 3 6 9'
gfs_last_three_hourly=192
gfs_last_six_hourly=240
gfs_last_twelve_hourly=384
while [[ "$#" -gt 0 ]] ; do
    case "$1" in
        --) shift 1 ; break ;;
        --gfs-last-hour|--gdas-last-hour)
            # Override default forecast hour, regenerate from $2
            whichm="$1"  # which model
            nicename=GFS
            allhours="$gfs_times"
            if [[ "$whichm" == "--gdas-last-hour" ]] ; then
                nicename=GDAS
                allhours="$gdas_times"
            fi
            gfharg="$2"  # argument to --gfs-last-hour
            shift 2
            lasthour=$( echo "$gfharg" | bc )
            if [[ ! ( "${lasthour:-}" -ge 0 ) ]] ; then
                err_exit "ERROR: $whichm specified, but next argument (\"$gfharg\" = \"$lasthour\" according to bc) is not an integer greater than or equal to 0."
            fi
            set +x
            echo "Option $whichm $gfharg specified.  Will override"
            echo "later script decisions about forecast length and forecast"
            echo "hours.  I will assume these are the possible forecast hours:"
            echo $allhours | fold -s -w 72
            echo "and I will keep all forecast hours up to $lasthour."
            echo "Calculating forecast hours now."
            set -x
            prev=0
            for now in $allhours ; do
                if [[ ! ( "$now" -le "$lasthour" ) ]] ; then
                    break
                else
                    override_fcsthrs="$override_fcsthrs $now"
                    override_fcstlen=$now
                    step=$(( now - prev ))
                    step00="${step}00"
                    if [[ "$step" -gt 0 && "$step00" -lt "$override_atcffreq" ]] ; then
                        override_atcffreq="$step00"
                    fi
                fi
            done
            if [[ -z "$override_fcsthrs" || -z "$override_fcstlen" ]] ; then
                $postmsg "$jlogfile" "ERROR: requested forecast hour from $whichm is $lasthour (from parsing \"$gfharg\") but could not find any valid $nicename forecast hours at or before that time.  This is probably an error in the tracker script."
            fi
            ;;
        --wait-for-data)
            # Wait up to X seconds for input data
            wait_max_time=$( echo $2 | bc )
            shift 2
            ;;
        --*)
            err_exit "ERROR: Unrecognized argument $1 to extrkr.sh"
            exit 2
            ;;
        *) break
    esac
done
# End of forecast hour override code
# ----------------------------------------------------------------------

export PDY=${PDY:-$1}
export CYL=${CYL:-${cyc:-$2}}
export CYCLE=t${CYL}z
export cmodel=${cmodel:-$3}
export jobid=${jobid:-testjob}
export envir=${envir:-prod}
export SENDCOM=${SENDCOM:-NO}
export SENDNHC=${SENDNHC:-${SENDCOM:-NO}}
export PARAFLAG=${PARAFLAG:-NO}
export DISK_TRAK=${DISK_TRAK:-/global/save}
export DATA=${DATA:-$DATAROOT/${userid}/trakout}
export TRKDATA=${TRKDATA:-$DATA}
export COMDIR=${COMDIR:-""}
export ATCFdir=${ATCFdir:-$COMROOTp1/nhc/${envir}/atcf}
#export PHASEFLAG=y
export PHASEFLAG=n
export WCORE_DEPTH=1.0
#export PHASE_SCHEME=vtt
#export PHASE_SCHEME=cps
export PHASE_SCHEME=both
export STRUCTFLAG=n
export IKEFLAG=n

export trkrtype=tracker
export loopnum=1
# Define tracker working directory for this global member
##export TRKDATA=/ptmpp1/${userid}/trakout2/${PDY}${cyc}/${cmodel}

##export TMPDIR=$DATA/trak/${RANDOM}

export flag_pgb=${flag_pgb:-q}

export NWPROD=${NWPROD:-${NWROOT}}
export NWPROD=${NWPROD:-/nwprod}
export rundir=${rundir:-$COMOUT}

if [ ! -d $DATA ]
then
   mkdir -p $DATA
fi
cd $DATA

#$NWPROD/util/ush/setup.sh

if [ ${PARAFLAG} = 'YES' ]
then 
  $NWPROD/util/ush/setup.sh
#else
#TM take out this else part for operations.....
#  $NWPROD/util/ush/setup.sh
fi

if [ ${#PDY} -eq 0 -o ${#CYL} -eq 0 -o ${#cmodel} -eq 0 ]
then
  set +x
  echo
  echo "Something wrong with input data.  One or more input variables has length 0"
  echo "PDY= ${PDY}, CYL= ${CYL}, cmodel= ${cmodel}"
  echo "EXITING...."
  set -x
  err_exit " FAILED ${jobid} -- BAD INPUTS AT LINE $LINENO IN TRACKER SCRIPT - ABNORMAL EX
IT"
else
  set +x
  echo " "
  echo " #-----------------------------------------------------------------#"
  echo " At beginning of tracker script, the following imported variables "
  echo " are defined: "
  echo "   PDY ................................... $PDY"
  echo "   CYL ................................... $CYL"
  echo "   CYCLE ................................. $CYCLE"
  echo "   cmodel ................................ $cmodel"
  echo "   jobid ................................. $jobid"
  echo "   envir ................................. $envir"
  echo "   SENDCOM ............................... $SENDCOM"
  echo "   SENDNHC ............................... $SENDNHC"
  echo " "
  set -x
fi

scc=`echo ${PDY} | cut -c1-2`
syy=`echo ${PDY} | cut -c3-4`
smm=`echo ${PDY} | cut -c5-6`
sdd=`echo ${PDY} | cut -c7-8`
shh=${CYL}
symd=`echo ${PDY} | cut -c3-8`
syyyy=`echo ${PDY} | cut -c1-4`
symdh=${PDY}${CYL}

export gfsvitdir=${gfsvitdir:-${COMDIR}${COMROOThps}/gfs/prod/gfs.$PDY}
export namvitdir=${namvitdir:-${COMDIR}${COMROOTp1}/nam/prod/nam.$PDY}
export gltrkdir=${gltrkdir:-${COMDIR}${COMROOTp1}/hur/${envir}/global}
export TPCATdir=/tpcprd/atcf

export homesyndir=${homesyndir:-${HOMERELO}}
export homesyndir=${homesyndir:-$NWPROD/util}
export exectrkdir=${exectrkdir:-${homesyndir}/exec}
export ushtrkdir=${ushtrkdir:-${homesyndir}/ush}
export archsyndir=${archsyndir:-${COMDIR}${COMROOTp1}/arch/prod/syndat}

##cp /com/date/t${CYL}z ncepdate
##export CENT=` cut -c7-8 ncepdate `

export CENT=`echo ${PDY} | cut -c1-2`

export maxtime=65    # Max number of forecast time levels

#----------------------------------------------------------------#
#
#    --- Define data directories and data file names ---
#               
# Convert the input model to lowercase letters and check to see 
# if it's a valid model, and assign a model ID number to it.  
# This model ID number is passed into the Fortran program to 
# let the program know what set of forecast hours to use in the 
# ifhours array.  Also, set the directories for the operational 
# input GRIB data files and create templates for the file names.
# While only 1 of these sets of directories and file name 
# templates is used during a particular run of this script, 
# "gfsvitdir" is used every time, because that is the directory 
# that contains the error-checked TC vitals file that Steve Lord 
# produces, and so it is included after the case statement.
#
#----------------------------------------------------------------#

cmodel=`echo ${cmodel} | tr "[A-Z]" "[a-z]"`

set -x                                           
# "gribver" is an environmental variable that should be defined
# and exported in the parent script that calls this script.
export gribver=${gribver:-2}
#if [ ${gribver} -eq 1 ]; then
#  gfsdir=/com/gfs/prod/gfs.${PDY}
#  gfsgfile=gfs.t${CYL}z.master.grbf
#  gfsifile=gfs.t${CYL}z.master.grbif
#else
#  gfsdir=/com/gfs/prod/gfs.${PDY}
#  gfsgfile=gfs.t${CYL}z.master.grb2f
#  gfsifile=gfs.t${CYL}z.master.grb2if

#  gfsdir=/ptmpp1/Qingfu.Liu/gfs025
#  gfsdir=/global/noscrub/Russ.Treadon/com/gfs/para/gfs.${PDY}
#  gfsdir=/global/noscrub/Qingfu.Liu/tracker_data_2014042912
#  gfsgfile=gfs.t${CYL}z.pgrb2.0p25.f
#  gfsifile=gfs.t00z.pgrb2.0p25.f015.idx
#fi
#COM=${COM:-/com/gfs/${envir}/gfs.${PDY}} 
#fcstlen=384                                      
#fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
#          84 90 96 102 108 114 120 126 132 138 144
#          150 156 162 168 174 180 186 192 198 204 210
#          216 222 228 234 240  -99  -99  -99  -99  -99  -99
#           -99  -99  -99  -99  -99  -99  -99  -99  -99  -99  -99
#           -99  -99  -99  -99  -99  -99  -99'
#fcsthrs=' 000 006 012 018 024 030 036 042 048 054 060 066 072 078
#          084 090 096 102 108 114 120 126 132 138 144 150 156 162
#          168 174 180 186 192 198 204 210 216 222 228 234 240'

if [ ${cmodel} = 'gfs' ]; then
  if [ ${gribver} -eq 1 ]; then
#     gfsdir=/com/gfs/prod/gfs.${PDY}
      gfsdir=$COMINgfs
#     gfsgfile=gfs.t${CYL}z.master.grbf
     gfsgfile=gfs.t${CYL}z.pgrbq
  else
#     gfsdir=/com/gfs/prod/gfs.${PDY}
     gfsdir=$COMINgfs
     gfsgfile=gfs.t${CYL}z.pgrb2.0p25.f
  fi
  COM=${COM:-$COMINgfs} 
  fcstlen=180
  fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
          84 90 96 102 108 114 120 126 132 138 144 150 156 162
          168 174 180'
  cdump=${CDUMP:-gfs}
  atcfnum=91                                       
  atcfname="gfso"            
  atcfout="gfso"             
  atcffreq=600                                     
  mslpthresh=0.0015                                
  v850thresh=1.5000                                
  modtyp='global'                                  
  file_sequence="onebig"                           
  lead_time_units='hours'                          
# g2_jpdtn sets the variable that will be used as "JPDTN" for 
# the call to getgb2, if gribver=2.  jpdtn=1 for ens data, 
# jpdtn=0 for deterministic data.
  g2_jpdtn=0
  model=1
fi

if [ ${cmodel} = 'para' ]; then
  if [ ${gribver} -eq 1 ]; then
#     gfsdir=/global/noscrub/Russ.Treadon/com/gfs/para/gfs.${PDY}
     gfsdir=${paradir}
#     gfsgfile=gfs.t${CYL}z.master.grbf
  else
#     gfsdir=/global/noscrub/Russ.Treadon/com/gfs/para/gfs.${PDY}
     gfsdir=${paradir}
#     gfsgfile=gfs.t${CYL}z.pgrb2.0p25.f
  fi
  COM=${COMOUT} 
  fcstlen=180
  para_def_fcsthrs=' 00 06 12 18 24 30 36 42 48 54 60 66 72 78
          84 90 96 102 108 114 120 126 132 138 144 150 156 162
          168 174 180'
  fcsthrs=${FCSTHRS:-$para_def_fcsthrs}
  atcfnum=71                                       
  if [[ -z "$TRACKID" ]] ; then
      err_exit 'ERROR: $TRACKID was not set when running cmodel=para'
      exit 3
  fi
  atcfname="${TRACKID}"
  atcfout="${TRACKID}" 
  atcffreq=600                                     
  mslpthresh=0.0015                                
  v850thresh=1.5000                                
  modtyp='global'                                  
  file_sequence="onebig"                           
  lead_time_units='hours'                          
# g2_jpdtn sets the variable that will be used as "JPDTN" for 
# the call to getgb2, if gribver=2.  jpdtn=1 for ens data, 
# jpdtn=0 for deterministic data.
  g2_jpdtn=0
  model=1
  cdump=${CDUMP:-gfs}
  if [ ${cdump} = 'gdas' ]; then
    fcstlen=6
    atcfnum=72
    model=8
  fi
fi

if [ ${cmodel} = 'gdas' ]; then
  if [ ${gribver} -eq 1 ]; then
     gfsdir=${paradir:-$COMINgdas}
#     gfsgfile=gdas.t${CYL}z.master.grbf
  else
     gfsdir=${paradir:-$COMINgdas}
#     gfsgfile=gfs.t${CYL}z.pgrb2.0p25.f
  fi
  COM=${COM:-$COMINgdas} 
  fcstlen=6
  fcsthrs=' 00 06'
  atcfnum=72                                       
  atcfname="gdas"
  atcfout="gdas" 
  atcffreq=300                                     
  mslpthresh=0.0015                                
  v850thresh=1.5000                                
  modtyp='global'                                  
  file_sequence="onebig"                           
  lead_time_units='hours'                          
# g2_jpdtn sets the variable that will be used as "JPDTN" for 
# the call to getgb2, if gribver=2.  jpdtn=1 for ens data, 
# jpdtn=0 for deterministic data.
  g2_jpdtn=0
  model=8
fi

# ----------------------------------------------------------------------
# Override forecast hours based on earlier calculations from
# --gfs-last-hour option.
if [[ ! -z "$override_fcsthrs" && ! -z "$override_fcstlen" ]] ; then
    set +x
    echo "        ----------------------------------------        "
    echo "Overriding forecast hours due to $whichm $override_fcstlen option."
    echo "Last forecast hour: $override_fcstlen"
    echo "Forecast hours to process: $override_fcsthrs"
    echo "ATCF frequency: $override_atcffreq (in centihours)"
    echo "        ----------------------------------------        "
    set -x
    fcsthrs="$override_fcsthrs"
    fcstlen="$override_fcstlen"
    atcffreq="$override_atcffreq"
fi
# ----------------------------------------------------------------------

# NOTE: MSLET instead of PRMSL for GFS and GDAS.  This selects the
# Membrane Mean Sea Level Pressure instead of Scheul.
if [ ${PHASEFLAG} = 'y' ]; then
  wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET HGT:925 HGT:900 HGT:800 HGT:750 HGT:650 HGT:600 HGT:550 HGT:500 HGT:450 HGT:400 HGT:350 HGT:300 HGT:250 TMP:500 TMP:450 TMP:400 TMP:350 TMP:300 TMP:250"
else
  wgrib_parmlist=" HGT:850 HGT:700 UGRD:850 UGRD:700 UGRD:500 VGRD:850 VGRD:700 VGRD:500 SurfaceU SurfaceV ABSV:850 ABSV:700 MSLET "
fi

#---------------------------------------------------------------#
#
#      --------  TC Vitals processing   --------
#
# Check Steve Lord's operational tcvitals file to see if any 
# vitals records were processed for this time by his system.  
# If there were, then you'll find a file in /com/gfs/prod/gfs.yymmdd 
# with the vitals in it.  Also check the raw TC Vitals file in
# /com/arch/prod/syndat , since this may contain storms that Steve's 
# system ignored (Steve's system will ignore all storms that are 
# either over land or very close to land);  We still want to track 
# these inland storms, AS LONG AS THEY ARE NHC STORMS (don't 
# bother trying to track inland storms that are outside of NHC's 
# domain of responsibility -- we don't need that info).
# UPDATE 3/27/09 MARCHOK: The SREF is run at off-synoptic times
#   (03,09,15,21Z).  There are no tcvitals issued at these offtimes,
#   so the updating of the "old" tcvitals is critical for running
#   the tracker on SREF.  For updating the old tcvitals for SREF,
#   we need to look 3h back, not 6h as for the other models that
#   run at synoptic times.  Therefore, we've introduced a
#   variable called "vit_incr" here.
#--------------------------------------------------------------#

if [ ${cmodel} = 'sref' ]; then
  vit_incr=3
else
  vit_incr=6
fi

# First check to see if the vitals file is in gfsvitdir or not.  If 
# it's not, then run Hua-Lu's ftp script to get the file from one
# of the other machines.  If it's still not there, then no big 
# deal; this script will exit just a little further down once it
# realizes there are not any storms to process.

old_ymdh=` ${NDATE:?} -${vit_incr} ${PDY}${CYL}`
old_4ymd=${old_ymdh:0:8}
old_ymd=${old_ymdh:2:6}
old_hh=${old_ymdh:8:2}
old_str="${old_ymd} ${old_hh}00"

future_ymdh=` ${NDATE:?} ${vit_incr} ${PDY}${CYL}`
future_4ymd=${future_ymdh:0:8}
future_ymd=${future_ymdh:2:6}
future_hh=${future_ymdh:8:2}
future_str="${future_ymd} ${future_hh}00"

# NOTE: Change synvitdir to point to /com/nam for regional models.

synvitdir=${synvitdir:-${COMDIR}${COMROOThps}/gfs/prod/gfs.${PDY}}
synvitfile=${synvitfile:-gfs.t${CYL}z.syndata.tcvitals.tm00}
synvitold_dir=${synvitold_dir:-${COMDIR}${COMROOThps}/gfs/prod/gfs.${old_4ymd}}
synvitold_file=${synvitold_file:-gfs.t${old_hh}z.syndata.tcvitals.tm00}
synvitfuture_dir=${synvitfuture_dir:-${COMDIR}${COMROOThps}/gfs/prod/gfs.${future_4ymd}}
synvitfuture_file=${synvitfuture_file:-gfs.t${future_hh}z.syndata.tcvitals.tm00}

set +x
echo " "
echo "              -----------------------------"
echo " "
echo " Now sorting and updating the TC Vitals file.  Please wait...."
echo " "
set -x

current_str="${symd} ${CYL}00"

if [ -s ${synvitdir}/${synvitfile} -o\
     -s ${synvitold_dir}/${synvitold_file} -o\
     -s ${synvitfuture_dir}/${synvitfuture_file} ]
then
  grep "${old_str}" ${synvitold_dir}/${synvitold_file}        \
                  >${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${current_str}"  ${synvitdir}/${synvitfile}                  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
  grep "${future_str}" ${synvitfuture_dir}/${synvitfuture_file}  \
                 >>${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL}
else
  set +x
  echo " "
  echo " There is no (synthetic) TC vitals file for ${CYL}z in ${synvitdir},"
  echo " nor is there a TC vitals file for ${old_hh}z in ${synvitold_dir}."
  echo " nor is there a TC vitals file for ${future_hh}z in ${synvitfuture_dir},"
  echo " Checking the raw TC Vitals file ....."
  echo " "
  set -x
fi

# Take the vitals from Steve Lord's /com/gfs/prod tcvitals file,
# and cat them with the NHC-only vitals from the raw, original
# /com/arch/prod/synda_tcvitals file.  Do this because the nwprod
# tcvitals file is the original tcvitals file, and Steve runs a
# program that ignores the vitals for a storm that's over land or
# even just too close to land, and for tracking purposes for the
# US regional models, we need these locations.  Only include these
# "inland" storm vitals for NHC (we're not going to track inland 
# storms that are outside of NHC's domain of responsibility -- we 
# don't need that info).  
# UPDATE 5/12/98 MARCHOK: awk logic is added to screen NHC 
#   vitals such as "89E TEST", since TPC 
#   does not want tracks for such storms.

grep "${old_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | awk 'substr($0,6,1) !~ /8/ {print $0}' \
      >${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${current_str}"  ${archsyndir}/syndat_tcvitals.${CENT}${syy}   | \
      grep -v TEST | awk 'substr($0,6,1) !~ /8/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}
grep "${future_str}" ${archsyndir}/syndat_tcvitals.${CENT}${syy} | \
      grep -v TEST | awk 'substr($0,6,1) !~ /8/ {print $0}' \
      >>${DATA}/tmprawvit.${atcfout}.${PDY}${CYL}


# IMPORTANT:  When "cat-ing" these files, make sure that the vitals
# files from the "raw" TC vitals files are first in order and Steve's
# TC vitals files second.  This is because Steve's vitals file has
# been error-checked, so if we have a duplicate tc vitals record in
# these 2 files (very likely), program supvit.x below will
# only take the last vitals record listed for a particular storm in
# the vitals file (all previous duplicates are ignored, and Steve's
# error-checked vitals records are kept).

cat ${DATA}/tmprawvit.${atcfout}.${PDY}${CYL} ${DATA}/tmpsynvit.${atcfout}.${PDY}${CYL} \
        >${DATA}/vitals.${atcfout}.${PDY}${CYL}

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

oldymdh=` ${NDATE:?} -${vit_incr} ${PDY}${CYL}`
oldyy=${oldymdh:2:2}
oldmm=${oldymdh:4:2}
olddd=${oldymdh:6:2}
oldhh=${oldymdh:8:2}
oldymd=${oldyy}${oldmm}${olddd}

futureymdh=` ${NDATE:?} 6 ${PDY}${CYL}`
futureyy=${futureymdh:2:2}
futuremm=${futureymdh:4:2}
futuredd=${futureymdh:6:2}
futurehh=${futureymdh:8:2}
futureymd=${futureyy}${futuremm}${futuredd}

cat<<EOF >${DATA}/suv_input.${atcfout}.${PDY}${CYL}
&datenowin   dnow%yy=${syy}, dnow%mm=${smm},
             dnow%dd=${sdd}, dnow%hh=${CYL}/
&dateoldin   dold%yy=${oldyy}, dold%mm=${oldmm},
             dold%dd=${olddd}, dold%hh=${oldhh}/
&datefuturein  dfuture%yy=${futureyy}, dfuture%mm=${futuremm},
               dfuture%dd=${futuredd}, dfuture%hh=${futurehh}/
&hourinfo  vit_hr_incr=${vit_incr}/
EOF


numvitrecs=`cat ${DATA}/vitals.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
then

  if [ ${trkrtype} = 'tracker' ]
  then
    set +x
    echo " "
    echo "!!! NOTE -- There are no vitals records for this time period."
    echo "!!! File ${DATA}/vitals.${atcfout}.${PDY}${CYL} is empty."
    echo "!!! It could just be that there are no storms for the current"
    echo "!!! time.  Please check the dates and submit this job again...."
    echo " "
    set -x
    exit 1
  fi

fi

# For tcgen cases, filter to use only vitals from the ocean 
# basin of interest....

if [ ${trkrtype} = 'tcgen' ]
  then

  if [ ${numvitrecs} -gt 0 ]
  then
    
    fullvitfile=${DATA}/vitals.${atcfout}.${PDY}${CYL}
    cp $fullvitfile ${DATA}/vitals.all_basins.${atcfout}.${PDY}${CYL}
    basin=` echo $regtype | cut -c1-2`

    if [ ${basin} = 'al' ]; then
      cat $fullvitfile | awk '{if (substr($0,8,1) == "L") print $0}' \
               >${DATA}/vitals.tcgen_al_only.${atcfout}.${PDY}${CYL}
      cp ${DATA}/vitals.tcgen_al_only.${atcfout}.${PDY}${CYL} \
         ${DATA}/vitals.${atcfout}.${PDY}${CYL}
    fi
    if [ ${basin} = 'ep' ]; then
      cat $fullvitfile | awk '{if (substr($0,8,1) == "E") print $0}' \
               >${DATA}/vitals.tcgen_ep_only.${atcfout}.${PDY}${CYL}
      cp ${DATA}/vitals.tcgen_ep_only.${atcfout}.${PDY}${CYL} \
         ${DATA}/vitals.${atcfout}.${PDY}${CYL}
    fi
    if [ ${basin} = 'wp' ]; then
      cat $fullvitfile | awk '{if (substr($0,8,1) == "W") print $0}' \
               >${DATA}/vitals.tcgen_wp_only.${atcfout}.${PDY}${CYL}
      cp ${DATA}/vitals.tcgen_wp_only.${atcfout}.${PDY}${CYL} \
         ${DATA}/vitals.${atcfout}.${PDY}${CYL}
    fi

    cat ${DATA}/vitals.${atcfout}.${PDY}${CYL}

  fi
    
fi

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Before running the program to read, sort and update the vitals,
# first run the vitals through some awk logic, the purpose of 
# which is to convert all the 2-digit years into 4-digit years.
# We need this logic to ensure that all the vitals going
# into supvit.f have uniform, 4-digit years in their records.
#
# 1/8/2000: sed code added by Tim Marchok due to the fact that 
#       some of the vitals were getting past the syndata/qctropcy
#       error-checking with a colon in them; the colon appeared
#       in the character immediately to the left of the date, which
#       was messing up the "(length($4) == 8)" statement logic.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

sed -e "s/\:/ /g"  ${DATA}/vitals.${atcfout}.${PDY}${CYL} > ${DATA}/tempvit
mv ${DATA}/tempvit ${DATA}/vitals.${atcfout}.${PDY}${CYL}

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
} ' ${DATA}/vitals.${atcfout}.${PDY}${CYL} >${DATA}/vitals.${atcfout}.${PDY}${CYL}.y4

mv ${DATA}/vitals.${atcfout}.${PDY}${CYL}.y4 ${DATA}/vitals.${atcfout}.${PDY}${CYL}

if [ ${numvitrecs} -gt 0 ]
then

  export pgm=supvit
  . $prep_step

  ln -s -f ${DATA}/vitals.${atcfout}.${PDY}${CYL}         fort.31
  ln -s -f ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}     fort.51

  msg="$pgm start for $atcfout at ${CYL}z"
  $postmsg "$jlogfile" "$msg"

  ${exectrkdir}/supvit <${DATA}/suv_input.${atcfout}.${PDY}${CYL}
  suvrcc=$?

  if [ ${suvrcc} -eq 0 ]
  then
    msg="$pgm end for $atcfout at ${CYL}z completed normally"
    $postmsg "$jlogfile" "$msg"
  else
    set +x
    echo " "
    echo "!!! ERROR -- An error occurred while running supvit.x, "
    echo "!!! which is the program that updates the TC Vitals file."
    echo "!!! Return code from supvit.x = ${suvrcc}"
    echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
    echo "!!! Exiting...."
    echo " "
    set -x
    err_exit " FAILED ${jobid} - ERROR RUNNING SUPVIT IN TRACKER SCRIPT- ABNORMAL EXIT"
  fi

else

  touch ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL}

fi

#-----------------------------------------------------------------
# In this section, check to see if the user requested the use of 
# operational TC vitals records for the initial time only.  This 
# option might be used for a retrospective medium range forecast
# in which the user wants to initialize with the storms that are
# currently there, but then let the model do its own thing for 
# the next 10 or 14 days....

#------------------------------------------------------------------#
# Now select all storms to be processed, that is, process every
# storm that's listed in the updated vitals file for the current
# forecast hour.  If there are no storms for the current time,
# then exit.
#------------------------------------------------------------------#

numvitrecs=`cat ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`
if [ ${numvitrecs} -eq 0 ]
then
  if [ ${trkrtype} = 'tracker' ]
  then
    set +x
    echo " "
    echo "!!! NOTE -- There are no vitals records for this time period "
    echo "!!! in the UPDATED vitals file."
    echo "!!! It could just be that there are no storms for the current"
    echo "!!! time.  Please check the dates and submit this job again...."
    echo " "
    set -x
    exit 1
  fi
fi

set +x
echo " "
echo " *--------------------------------*"
echo " |        STORM SELECTION         |"
echo " *--------------------------------*"
echo " "
set -x

ict=1
while [ $ict -le 15 ]
do
  stormflag[${ict}]=3
  let ict=ict+1
done

dtg_current="${symd} ${CYL}00"
stormmax=` grep "${dtg_current}" ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`

if [ ${stormmax} -gt 15 ]
then
  stormmax=15
fi

sct=1
while [ ${sct} -le ${stormmax} ]
do
  stormflag[${sct}]=1
  let sct=sct+1
done


#---------------------------------------------------------------#
#
#    --------  "Genesis" Vitals processing   --------
#
# May 2006:  This entire genesis tracking system is being
# upgraded to more comprehensively track and categorize storms.
# One thing that has been missing from the tracking system is
# the ability to keep track of storms from one analysis cycle
# to the next.  That is, the current system has been very
# effective at tracking systems within a forecast, but we have
# no methods in place for keeping track of storms across
# difference initial times.  For example, if we are running
# the tracker on today's 00z GFS analysis, we will get a
# position for various storms at the analysis time.  But then
# if we go ahead and run again at 06z, we have no way of
# telling the tracker that we know about the 00z position of
# this storm.  We now address that problem by creating
# "genesis" vitals, that is, when a storm is found at an
# analysis time, we not only produce "atcfunix" output to
# detail the track & intensity of a found storm, but we also
# produce a vitals record that will be used for the next
# run of the tracker script.  These "genesis vitals" records
# will be of the format:
#
#  YYYYMMDDHH_AAAH_LLLLX_TYP
#
#    Where:
#
#      YYYYMMDDHH = Date the storm was FIRST identified
#                   by the tracker.
#             AAA = Abs(Latitude) * 10; integer value
#               H = 'N' for norther hem, 'S' for southern hem
#            LLLL = Abs(Longitude) * 10; integer value
#               X = 'E' for eastern hem, 'W' for western hem
#             TYP = Tropical cyclone storm id if this is a
#                   tropical cyclone (e.g., "12L", or "09W", etc).
#                   If this is one that the tracker instead "Found
#                   On the Fly (FOF)", we simply put those three
#                   "FOF" characters in there.

d6ago_ymdh=` ${NDATE:?} -6 ${PDY}${CYL}`
d6ago_4ymd=` echo ${d6ago_ymdh} | cut -c1-8`
d6ago_ymd=` echo ${d6ago_ymdh} | cut -c3-8`
d6ago_hh=`  echo ${d6ago_ymdh} | cut -c9-10`
d6ago_str="${d6ago_ymd} ${d6ago_hh}00"

d6ahead_ymdh=` ${NDATE:?} 6 ${PDY}${CYL}`
d6ahead_4ymd=` echo ${d6ahead_ymdh} | cut -c1-8`
d6ahead_ymd=` echo ${d6ahead_ymdh} | cut -c3-8`
d6ahead_hh=`  echo ${d6ahead_ymdh} | cut -c9-10`
d6ahead_str="${d6ahead_ymd} ${d6ahead_hh}00"

syyyym6=` echo ${d6ago_ymdh} | cut -c1-4`
smmm6=`   echo ${d6ago_ymdh} | cut -c5-6`
sddm6=`   echo ${d6ago_ymdh} | cut -c7-8`
shhm6=`   echo ${d6ago_ymdh} | cut -c9-10`

syyyyp6=` echo ${d6ahead_ymdh} | cut -c1-4`
smmp6=`   echo ${d6ahead_ymdh} | cut -c5-6`
sddp6=`   echo ${d6ahead_ymdh} | cut -c7-8`
shhp6=`   echo ${d6ahead_ymdh} | cut -c9-10`

set +x
echo " "
echo " d6ago_str=    --->${d6ago_str}<---"
echo " current_str=  --->${current_str}<---"
echo " d6ahead_str=  --->${d6ahead_str}<---"
echo " "
echo " for the times 6h ago, current and 6h ahead:"
echo " "
echo " "
set -x

  touch ${DATA}/genvitals.upd.${cmodel}.${atcfout}.${PDY}${CYL}


#-----------------------------------------------------------------#
#
#         ------  CUT APART INPUT GRIB FILES  -------
#
# For the selected model, cut apart the GRIB input files in order
# to pull out only the variables that we need for the tracker.  
# Put these selected variables from all forecast hours into 1 big 
# GRIB file that we'll use as input for the tracker.
# 
#-----------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------"
echo "   NOW CUTTING APART INPUT GRIB FILES TO "
echo "   CREATE 1 BIG GRIB INPUT FILE "
echo " -----------------------------------------"
echo " "
set -x

#gix=$NWPROD/util/exec/grbindex
#g2ix=$NWPROD/util/exec/grb2index
#cgb=$NWPROD/util/exec/copygb
#cgb2=$NWPROD/util/exec/copygb2

regflag=`grep NHC ${DATA}/vitals.upd.${atcfout}.${PDY}${CYL} | wc -l`

# ----------------------------------------------------------------------
find_gfile() {
    # This subroutine finds an input file from a list of possible
    # input filenames, and calls err_exit if no file is found.  The
    # first file found is returned.

    # Calling conventions:
    #   find_gfile GFS 30 /path/to/file1.master.pgrbq30.grib2 /path/to/file2.master.pgrbq030.grib2 ...
    nicename="$1"
    nicehour="$2"
    shift 2
    gfile=none
    echo "Searching for input $nicename data for forecast hour $nicehour"
    set -x
    now=$( date +%s )
    later=$(( now + wait_max_time ))
    # Note: the loop has only one iteration if --wait-max-time is
    # unspecified.  That is because later=now
    while [[ ! ( "$now" -gt "$later" ) ]] ; do
        for gfile in "$@" ; do
            if [[ ! -e "$gfile" ]] ; then
                set +x
                echo "$gfile: does not exist"
                set -x
                gfile=none
            elif [[ ! -s "$gfile" ]] ; then
                set +x
                echo "$gfile: exists, but is empty"
                set -x
                gfile=none
            else
                set +x
                echo "$gfile: exists, is non-empty, so I will use this file"
                set -x
                return 0
            fi
        done
        now=$( date +%s )
        if [[ "$gfile" == none ]] ; then
            if [[ ! ( "$now" -lt "$later" ) ]] ; then
                set +x
                echo " "
                echo " "
                echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                echo " !!! $nicename missing for hour $nicehour"
                echo " !!! Check for the existence of these file:"
                for gfile in "$@" ; do
                    echo " !!!    $nicename File: $gfile"
                done
                echo " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                echo " "
                set -x
                err_exit "ERROR: mandatory input GFS file for hour $nicehour is missing or empty.  Aborting.  Checked for these files: $*"
                continue
            else
                set +x
                echo " "
                echo " !!! Mandatory input $nicename missing for hour $nicehour"
                echo " !!! Will retry after $sleep_time second sleep."
                echo " !!! Checked these files:"
                for gfile in "$@" ; do
                    echo " !!!    $nicename File: $gfile"
                done
                echo " "
                set -x
                sleep $sleep_time
            fi
        fi
    done
}

# --------------------------------------------------
#   Process GFS or GDAS data
# --------------------------------------------------
if [[ ${model} -eq 1 || $model == 8 ]] ; then

  export nest_type="fixed"
  export trkrebd=360.0
  export trkrwbd=0.0
  export trkrnbd=85.0
  export trkrsbd=-85.0
  rundescr="xxxx"
  atcfdescr="xxxx"


  if [ $loopnum -eq 1 ]
  then

  if [ -s ${DATA}/gfsgribfile.${PDY}${CYL} ]
  then
    rm ${DATA}/gfsgribfile.${PDY}${CYL}
  fi

  rm ${DATA}/master.gfsgribfile.${PDY}${CYL}.f*
  rm ${DATA}/gfsgribfile.${PDY}${CYL}.f*
  >${DATA}/gfsgribfile.${PDY}${CYL}

  set +x
  echo " "
  echo "Time before gfs wgrib loop is `date`"
  echo " "
  set -x

  if [[ "$model" -eq 8 ]] ; then
      name=gdas
      name1=gdas
      nicename=GDAS
  else  # not model 8, so assume GFS
      name=gfs
      name1=gfs
      nicename=GFS
  fi

  for fhour in ${fcsthrs} ; do
      fhour=$( echo "$fhour" | bc )

      if [ ${fhour} -eq $bad_hour ]
      then
        continue
      fi

      fhour00=$( printf %02d "$fhour" )
      fhour000=$( printf %03d "$fhour" )
      fhour0000=$( printf %03d "$fhour" )

    if [[ "$gribver" == 1 ]] ; then

          find_gfile "$nicename" "$fhour" \
              ${gfsdir}/$name1.t${CYL}z.${flag_pgb}$fhour00 \
              ${gfsdir}/$name1.t${CYL}z.${flag_pgb}$fhour000 \
              ${gfsdir}/pgb${flag_pgb}$fhour00.$name.${symdh} \
              ${gfsdir}/pgrb${flag_pgb}$fhour00.$name.${symdh}
           ${WGRIB:?} -s $gfile >gfs.ix

      for parm in ${wgrib_parmlist}
      do
        case ${parm} in
          "SurfaceU") grep "UGRD:10 m " gfs.ix  ;;
          "SurfaceV") grep "VGRD:10 m " gfs.ix  ;;
                   *) grep "${parm}"    gfs.ix  ;;
        esac
      done | ${WGRIB:?} -s $gfile -i -grib -append \
                        -o ${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour000}

      gfs_master_file=${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour000}
      gfs_converted_file=${DATA}/gfsgribfile.${PDY}${CYL}.f${fhour000}
      gfs_cat_file=${DATA}/gfsgribfile.${PDY}${CYL}
#      $cgb -g4 -i2 -x ${gfs_master_file} ${gfs_converted_file}
#      cat ${gfs_converted_file} >>${gfs_cat_file}
      cat ${gfs_master_file} >>${gfs_cat_file}

    else # gribver is not 1, so assume GRIB2

        find_gfile "$nicename" "$fhour" \
          ${gfsdir}/$name1.t${CYL}z.pgrb2.0p25.f${fhour000} \
          ${gfsdir}/$name1.t${CYL}z.pgrb2.0p25.f${fhour00} \
          ${gfsdir}/pgb${flag_pgb}$fhour00.$name.${symdh}.grib2 \
          ${gfsdir}/pgrb${flag_pgb}${fhour000}.$name.${symdh}.grib2
      ${WGRIB2:?} -s $gfile >gfs.ix

      for parm in ${wgrib_parmlist}
      do
        case ${parm} in
          "SurfaceU") grep "UGRD:10 m " gfs.ix ;;
          "SurfaceV") grep "VGRD:10 m " gfs.ix ;;
                   *) grep "${parm}"    gfs.ix ;;
        esac
      done | ${WGRIB2:?} -i $gfile -append -grib \
                            ${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour000}

      gfs_master_file=${DATA}/master.gfsgribfile.${PDY}${CYL}.f${fhour000}
      gfs_converted_file=${DATA}/gfsgribfile.${PDY}${CYL}.f${fhour000}
      gfs_cat_file=${DATA}/gfsgribfile.${PDY}${CYL}

      ${GRB2INDEX:?} ${gfs_master_file} ${gfs_master_file}.ix

      g1=${gfs_master_file}
      x1=${gfs_master_file}.ix

#        grid4="0 6 0 0 0 0 0 0 720 361 0 0 90000000 0 48 -90000000 359500000 500000 500000 0"
#        $cgb2 -g "${grid4}" ${g1} ${x1} ${gfs_converted_file}
#        cat ${gfs_converted_file} >>${gfs_cat_file}

      cat ${gfs_master_file} >>${gfs_cat_file}

    fi
  
  done
  
  if [ ${gribver} -eq 1 ]; then
    ${GRBINDEX:?} ${DATA}/gfsgribfile.${PDY}${CYL} ${DATA}/gfsixfile.${PDY}${CYL}
  else
    ${GRB2INDEX:?} ${DATA}/gfsgribfile.${PDY}${CYL} ${DATA}/gfsixfile.${PDY}${CYL}
  fi

#   --------------------------------------------

  if [[ "$PhaseFlag" == y ]] ; then

  catfile=${DATA}/gfs.${PDY}${CYL}.catfile
  >${catfile}

    for fhour in ${fcsthrs}
    do


      fhour=$( echo "$fhour" | bc )

      if [ ${fhour} -eq $bad_hour ]
      then
        continue
      fi

      fhour00=$( printf %02d "$fhour" )
      fhour000=$( printf %03d "$fhour" )
      fhour0000=$( printf %03d "$fhour" )

      set +x
      echo " "
      echo "Date in interpolation for model= $cmodel and fhour= $fhour000 before = `date`"
      echo " "
      set -x

      gfile=${DATA}/gfsgribfile.${PDY}${CYL}
      ifile=${DATA}/gfsixfile.${PDY}${CYL}

      if [ ${gribver} -eq 1 ]; then
        ${GRBINDEX:?} $gfile $ifile
      else
        ${GRB2INDEX:?} $gfile $ifile
      fi

      gparm=7
      namelist=${DATA}/vint_input.${PDY}${CYL}.z
      echo "&timein ifcsthour=${fhour000},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}
  
      ln -s -f ${gfile}                                       fort.11
      ln -s -f ${FIXRELO}/gfs_hgt_levs.txt                     fort.16
      ln -s -f ${ifile}                                       fort.31
      ln -s -f ${DATA}/${cmodel}.${PDY}${CYL}.z.f${fhour000}     fort.51
  
      ${exectrkdir}/vint.x <${namelist}
      rcc1=$?
  
  
      gparm=11
      namelist=${DATA}/vint_input.${PDY}${CYL}.t
      echo "&timein ifcsthour=${fhour000},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}
  
      ln -s -f ${gfile}                                       fort.11
      ln -s -f ${FIXRELO}/gfs_tmp_levs.txt                     fort.16
      ln -s -f ${ifile}                                       fort.31 
      ln -s -f ${DATA}/${cmodel}.${PDY}${CYL}.t.f${fhour000}     fort.51
  
      ${exectrkdir}/vint.x <${namelist}
      rcc2=$?
  
      namelist=${DATA}/tave_input.${PDY}${CYL}
      echo "&timein ifcsthour=${fhour000},"       >${namelist}
      echo "        iparm=${gparm},"          >>${namelist}
      echo "        gribver=${gribver},"      >>${namelist}
      echo "        g2_jpdtn=${g2_jpdtn}/"    >>${namelist}
  
      ffile=${DATA}/${cmodel}.${PDY}${CYL}.t.f${fhour000}
      ifile=${DATA}/${cmodel}.${PDY}${CYL}.t.f${fhour000}.i

      if [ ${gribver} -eq 1 ]; then
        ${GRBINDEX:?} ${ffile} ${ifile}
      else
        ${GRB2INDEX:?} ${ffile} ${ifile}
      fi
  
      ln -s -f ${ffile}                                          fort.11
      ln -s -f ${ifile}                                          fort.31
      ln -s -f ${DATA}/${cmodel}.tave.${PDY}${CYL}.f${fhour000}     fort.51
      ln -s -f ${DATA}/${cmodel}.tave92.${PDY}${CYL}.f${fhour000}     fort.92
    
      ${exectrkdir}/tave.x <${namelist}
      rcc3=$?

      if [ $rcc1 -eq 0 -a $rcc2 -eq 0 -a $rcc3 -eq 0 ]; then
        echo " "
      else
        mailfile=${rundir}/errmail.${cmodel}.${PDY}${CYL}
        echo "CPS/WC interp failure for $cmodel ${PDY}${CYL}" >${mailfile}
        mail -s "GFS Failure (CPS/WC int) $cmodel ${PDY}${CYL}" ${userid} <${mailfile}
        exit 8
      fi
    
      tavefile=${DATA}/${cmodel}.tave.${PDY}${CYL}.f${fhour000}
      zfile=${DATA}/${cmodel}.${PDY}${CYL}.z.f${fhour000}
      cat ${zfile} ${tavefile} >>${catfile}
##      rm $tavefile $zfile
    
      set +x
      echo " "
      echo "Date in interpolation for cmodel= $cmodel and fhour= $fhour000 after = `date`"
      echo " "
      set -x
    
    done
    fi # end of "If PhaseFlag is on"
  fi # end of "If loopnum is 1"

  gfile=${DATA}/gfsgribfile.${PDY}${CYL}
  ifile=${DATA}/gfsixfile.${PDY}${CYL}

  if [[ "$PhaseFlag" == y ]] ; then
      cat ${catfile} >>${gfile}
      if [ ${gribver} -eq 1 ]; then
          ${GRBINDEX:?} ${gfile} ${ifile}
      else
          ${GRB2INDEX:?} ${gfile} ${ifile}
      fi
  fi

  # File names for input to tracker:
  gribfile=${DATA}/gfsgribfile.${PDY}${CYL}
  ixfile=${DATA}/gfsixfile.${PDY}${CYL}
fi

$postmsg "$jlogfile" "SUCCESS: have all inputs needed to run tracker.  Will now run the tracker."

#------------------------------------------------------------------------#
#                         Now run the tracker                            #
#------------------------------------------------------------------------#

ist=1
while [ $ist -le 15 ]
do
  if [ ${stormflag[${ist}]} -ne 1 ]
  then
    set +x; echo "Storm number $ist NOT selected for processing"; set -x
  else
    set +x; echo "Storm number $ist IS selected for processing...."; set -x
  fi
  let ist=ist+1
done

namelist=${DATA}/input.${atcfout}.${PDY}${CYL}
ATCFNAME=` echo "${atcfname}" | tr '[a-z]' '[A-Z]'`

if [ ${cmodel} = 'sref' ]; then
  export atcfymdh=` ${NDATE:?} -3 ${scc}${syy}${smm}${sdd}${shh}`
else
  export atcfymdh=${scc}${syy}${smm}${sdd}${shh}
fi

contour_interval=100.0
write_vit=n
want_oci=.TRUE.

cat <<EOF > ${namelist}
&datein inp%bcc=${scc},inp%byy=${syy},inp%bmm=${smm},
        inp%bdd=${sdd},inp%bhh=${shh},inp%model=${model},
        inp%modtyp='${modtyp}',
        inp%lt_units='${lead_time_units}',
        inp%file_seq='${file_sequence}',
        inp%nesttyp='${nest_type}'/
&atcfinfo atcfnum=${atcfnum},atcfname='${ATCFNAME}',
          atcfymdh=${atcfymdh},atcffreq=${atcffreq}/
&trackerinfo trkrinfo%westbd=${trkrwbd},
      trkrinfo%eastbd=${trkrebd},
      trkrinfo%northbd=${trkrnbd},
      trkrinfo%southbd=${trkrsbd},
      trkrinfo%type='${trkrtype}',
      trkrinfo%mslpthresh=${mslpthresh},
      trkrinfo%v850thresh=${v850thresh},
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

export pgm=gettrk
. $prep_step

ln -s -f ${gribfile}                                               fort.11
ln -s -f ${DATA}/vitals.upd.${atcfout}.${PDY}${shh}                fort.12
ln -s -f ${DATA}/genvitals.upd.${cmodel}.${atcfout}.${PDY}${CYL}   fort.14
ihour=1
for fhour in ${fcsthrs} ; do
    fhour=$( echo "$fhour" | bc )   # strip leading zeros
    printf "%4d %5d\n" $ihour $(( fhour * 60 ))
    let ihour=ihour+1
done > leadtimes.txt
ln -s -f leadtimes.txt                                             fort.15
#ln -s -f ${FIXRELO}/${cmodel}.tracker_leadtimes                     fort.15
ln -s -f ${ixfile}                                                 fort.31

if [[ -z "$atcfout" ]] ; then
    err_exit 'ERROR: exgfs_trkr script forgot to set $atcfout variable'
fi

track_file_path=nowhere

if [ ${trkrtype} = 'tracker' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.all.${stormenv}.${PDY}${CYL}       fort.61
    ln -s -f ${DATA}/trak.${atcfout}.atcf.${stormenv}.${PDY}${CYL}      fort.62
    ln -s -f ${DATA}/trak.${atcfout}.radii.${stormenv}.${PDY}${CYL}     fort.63
    ln -s -f ${DATA}/trak.${atcfout}.atcf_gen.${stormenv}.${PDY}${CYL}  fort.66
    ln -s -f ${DATA}/trak.${atcfout}.atcf_sink.${stormenv}.${PDY}${CYL} fort.68
    ln -s -f ${DATA}/trak.${atcfout}.atcf_hfip.${stormenv}.${PDY}${CYL} fort.69
    track_file_path=${DATA}/trak.${atcfout}.atcfunix.${stormenv}.${PDY}${CYL}
  else
    ln -s -f ${DATA}/trak.${atcfout}.all.${PDY}${CYL}       fort.61
    ln -s -f ${DATA}/trak.${atcfout}.atcf.${PDY}${CYL}      fort.62
    ln -s -f ${DATA}/trak.${atcfout}.radii.${PDY}${CYL}     fort.63
    ln -s -f ${DATA}/trak.${atcfout}.atcf_gen.${PDY}${CYL}  fort.66
    ln -s -f ${DATA}/trak.${atcfout}.atcf_sink.${PDY}${CYL} fort.68
    ln -s -f ${DATA}/trak.${atcfout}.atcf_hfip.${PDY}${CYL} fort.69
    track_file_path=${DATA}/trak.${atcfout}.atcfunix.${PDY}${CYL}
  fi
else
  ln -s -f ${DATA}/trak.${atcfout}.all.${regtype}.${PDY}${CYL}       fort.61
  ln -s -f ${DATA}/trak.${atcfout}.atcf.${regtype}.${PDY}${CYL}      fort.62
  ln -s -f ${DATA}/trak.${atcfout}.radii.${regtype}.${PDY}${CYL}     fort.63
  ln -s -f ${DATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL}  fort.66
  ln -s -f ${DATA}/trak.${atcfout}.atcf_sink.${regtype}.${PDY}${CYL} fort.68
  ln -s -f ${DATA}/trak.${atcfout}.atcf_hfip.${regtype}.${PDY}${CYL} fort.69
  track_file_path=${DATA}/trak.${atcfout}.atcfunix.${regtype}.${PDY}${CYL}
fi

if [[ "$track_file_path" == nowhere ]] ; then
    err_exit 'ERROR: exgfs_trkr script forgot to set $track_file_path variable'
fi

ln -s -f $track_file_path  fort.64

if [ ${atcfname} = 'aear' ]
then
  ln -s -f ${DATA}/trak.${atcfout}.initvitl.${PDY}${CYL}           fort.65
fi

if [ ${write_vit} = 'y' ]
then
  ln -s -f ${DATA}/output_genvitals.${atcfout}.${PDY}${shh}        fort.67
fi

if [ ${PHASEFLAG} = 'y' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.cps_parms.${stormenv}.${PDY}${CYL}          fort.71
  else
    ln -s -f ${DATA}/trak.${atcfout}.cps_parms.${PDY}${CYL}          fort.71
  fi
fi

if [ ${STRUCTFLAG} = 'y' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.structure.${stormenv}.${PDY}${CYL}          fort.72
    ln -s -f ${DATA}/trak.${atcfout}.fractwind.${stormenv}.${PDY}${CYL}          fort.73
    ln -s -f ${DATA}/trak.${atcfout}.pdfwind.${stormenv}.${PDY}${CYL}            fort.76
  else
    ln -s -f ${DATA}/trak.${atcfout}.structure.${PDY}${CYL}          fort.72
    ln -s -f ${DATA}/trak.${atcfout}.fractwind.${PDY}${CYL}          fort.73
    ln -s -f ${DATA}/trak.${atcfout}.pdfwind.${PDY}${CYL}            fort.76
  fi
fi

if [ ${IKEFLAG} = 'y' ]; then
  if [ ${atcfout} = 'gfdt' -o ${atcfout} = 'gfdl' -o \
       ${atcfout} = 'hwrf' -o ${atcfout} = 'hwft' ]; then
    ln -s -f ${DATA}/trak.${atcfout}.ike.${stormenv}.${PDY}${CYL}                fort.74
  else
    ln -s -f ${DATA}/trak.${atcfout}.ike.${PDY}${CYL}                fort.74
  fi
fi

if [ ${trkrtype} = 'midlat' -o ${trkrtype} = 'tcgen' ]; then
  ln -s -f ${DATA}/trkrmask.${atcfout}.${regtype}.${PDY}${CYL}     fort.77
fi


set +x
echo " "
echo " -----------------------------------------------"
echo "           NOW EXECUTING TRACKER......"
echo " -----------------------------------------------"
echo " "
set -x

msg="$pgm start for $atcfout at ${CYL}z"
$postmsg "$jlogfile" "$msg"

set +x
echo "+++ TIMING: BEFORE gettrk  ---> `date`"
set -x

set +x
echo " "
echo "TIMING: Before call to gettrk at `date`"
echo " "
set -x

##/usrx/local/bin/getrusage -a /hwrf/save/Qingfu.Liu/trak/para/exec/gettrk <${namelist}

${exectrkdir}/gettrk <${namelist} | tee gettrk.log
gettrk_rcc=$?

set +x
echo " "
echo "TIMING: After call to gettrk at `date`"
echo " "
set -x

set +x
echo "+++ TIMING: AFTER  gettrk  ---> `date`"
set -x

#--------------------------------------------------------------#
# Send a message to the jlogfile for each storm that used 
# tcvitals for hour 0 track/intensity info.
#--------------------------------------------------------------#

pcount=0
cat gettrk.log | grep -a 'NOTE: TCVITALS_USED_FOR_ATCF_F00' | \
while read line
do
    echo "line is [$line]"
    if [[ ! ( "$pcount" -lt 30 ) ]] ; then
      $postmsg "$jlogfile" "Hit maximum number of postmsg commands for tcvitals usage at hour 0.  Will stop warning about that, to avoid spamming jlogfile."
      break
    fi
    $postmsg "$jlogfile" "$line"
    pcount=$(( pcount + 1 ))
done

#--------------------------------------------------------------#
# Now copy the output track files to different directories
#--------------------------------------------------------------#

set +x
echo " "
echo " -----------------------------------------------"
echo "    NOW COPYING OUTPUT TRACK FILES TO COM  "
echo " -----------------------------------------------"
echo " "
set -x

if [[ ! -e "$track_file_path" ]] ; then
    $postmsg "$jlogfile" "WARNING: tracker output file does not exist.  This is probably an error.  File: $track_file_path"
    $postmsg "$jlogfile" "WARNING: exgfs_trkr will create an empty track file and deliver that."
    cat /dev/null > $track_file_path
elif [[ ! -s "$track_file_path" ]] ; then
    $postmsg "$jlogfile" "WARNING: tracker output file is empty.  That is only an error if there are storms or genesis cases somewhere in the world.  File: $track_file_path"
else
    $postmsg "$jlogfile" "SUCCESS: Track file exists and is non-empty: $track_file"
    if [[ "$PHASEFLAG" == n ]] ; then
        echo "Phase information was disabled.  I will remove the empty phase information from the track file before delivery."
        cp -p $track_file_path $track_file_path.orig
        cut -c1-112 < $track_file_path.orig > $track_file_path
        if [[ ! -s "$track_file_path" ]] ; then
            $postmsg "$jlogfile" "WARNING: Something went wrong with \"cut\" command to remove phase information.  Will deliver original file."
            /bin/mv -f $track_file_path.orig $track_file_path
        else
            $postmsg "$jlogfile" "SUCCESS: Removed empty phase information because phase information is disabled."
        fi
    fi
fi

#mkdir /global/save/Qingfu.Liu/gfspara_track/gfs.${PDY}${CYL}
#cp /ptmpp1/Qingfu.Liu/trakout2/${PDY}${CYL}/gfs/trak.gfso.atcf* /global/save/Qingfu.Liu/gfspara_track/gfs.${PDY}${CYL}/.
#rm -rf /ptmpp1/Qingfu.Liu/trakout2/${PDY}${CYL}/gfs/*

if [ ${gettrk_rcc} -eq 0 ]; then

  if [ -s ${DATA}/output_genvitals.${atcfout}.${PDY}${shh} ]; then
    cat ${DATA}/output_genvitals.${atcfout}.${PDY}${shh} >>${genvitfile}
  fi
 
  if [ ${PARAFLAG} = 'YES' ]
  then

    if [[ ! -s "$track_file_path" ]] ; then
        $postmsg "$jlogfile" "WARNING: delivering empty track file to rundir."
    fi

    cp $track_file_path ../.
    cat $track_file_path >> \
           ${rundir}/${cmodel}.atcfunix.${syyyy}
    if [ ${cmodel} = 'gfs' ]; then
      cat ${rundir}/${cmodel}.atcfunix.${syyyy} | sed -e "s/ GFSO/ AVNO/g" >>${rundir}/avn.atcfunix.${syyyy}
    fi
#    cp ${DATA}/trak.${atcfout}.atcf_sink.${regtype}.${PDY}${CYL} ../.
#    cp ${DATA}/trak.${atcfout}.atcf_gen.${regtype}.${PDY}${CYL} ../.
  fi

  msg="$pgm end for $atcfout at ${CYL}z completed normally"
  $postmsg "$jlogfile" "$msg"

# Now copy track files into various archives....

  if [ ${SENDCOM} = 'YES' ]
  then

    if [[ ! -s "$track_file_path" ]] ; then
        $postmsg "$jlogfile" "WARNING: delivering an empty track file to COM."
        return
    fi

    glatuxarch=${glatuxarch:-${gltrkdir}/tracks.atcfunix.${syy}}

    cat $track_file_path  >>${glatuxarch}
    if [ ${cmodel} = 'gfs' ]; then
      cat $track_file_path | sed -e "s/ GFSO/ AVNO/g" >>${glatuxarch}
    fi

    if [ ${PARAFLAG} = 'YES' ]
    then
      echo " "
      tmatuxarch=${tmatuxarch:-/gpfs/gd2/emc/hwrf/save/${userid}/trak/prod/tracks.atcfunix.${syy}}
      cat $track_file_path  >>${tmatuxarch}
      if [ ${cmodel} = 'gfs' ]; then
        cat $track_file_path | sed -e "s/ GFSO/ AVNO/g" >>${tmatuxarch}
      fi
    else

      if [ ${cmodel} = 'gfdl' ]
      then
        cp $track_file_path ${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
      else
        cp $track_file_path ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
        if [ ${cmodel} = 'gfs' ]; then
          cat $track_file_path | sed -e "s/ GFSO/ AVNO/g" >${COM}/avn.t${CYL}z.cyclone.trackatcfunix
        fi
      fi

      tmscrdir=/gpfs/gd2/emc/hwrf/save/${userid}/trak/prod

      tmtrakstat=${tmscrdir}/tracker.prod.status
      echo "${atcfout} tracker completed okay for ${PDY}${CYL}" >>${tmtrakstat}

      export SENDDBN=${SENDDBN:-YES}
      if [ ${SENDDBN} = 'YES' ]
      then
        if [ ${cmodel} = 'gfdl' ]
        then
          $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
        else
          $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/${atcfout}.t${CYL}z.cyclone.trackatcfunix
          if [ ${cmodel} = 'gfs' ]; then
            $DBNROOT/bin/dbn_alert ATCFUNIX GFS_NAVY $job ${COM}/avn.t${CYL}z.cyclone.trackatcfunix
          fi
        fi
      fi

      if [[ "$SENDNHC" == YES ]] ; then
      # We need to parse apart the atcfunix file and distribute the forecasts to 
      # the necessary directories.  To do this, first sort the atcfunix records 
      # by forecast hour (k6), then sort again by ocean basin (k1), storm number (k2)
      # and then quadrant radii wind threshold (k12).  Once you've got that organized 
      # file, break the file up by putting all the forecast records for each storm 
      # into a separate file.  Then, for each file, find the corresponding atcfunix 
      # file in the /nhc/com/prod/atcf directory and dump the atcfunix records for that
      # storm in there. 

      if [ ${cmodel} = 'gfdl' ]
      then
        auxfile=${COM}/${stormenv}.${PDY}${CYL}.trackeratcfunix
      else
        auxfile=$track_file_path
      fi

      sort -k6 ${auxfile} | sort -k1 -k2 -k12  >atcfunix.sorted

      old_string="XX, XX"

      ict=0
      while read unixrec
      do
        storm_string=` echo "${unixrec}" | cut -c1-6`
        if [ "${storm_string}" = "${old_string}" ]
        then
          echo "${unixrec}" >>atcfunix_file.${ict}
        else
          let ict=ict+1
          echo "${unixrec}"  >atcfunix_file.${ict}
          old_string="${storm_string}"
        fi
      done <atcfunix.sorted

      if [ $ict -gt 0 ]
      then
        mct=0
        while [ $mct -lt $ict ]
        do
          let mct=mct+1
          at=` head -1 atcfunix_file.$mct | cut -c1-2 | tr '[A-Z]' '[a-z]'`
          NO=` head -1 atcfunix_file.$mct | cut -c5-6`
          if [ ! -d ${ATCFdir}/${at}${NO}${syyyy} ]
          then
              mkdir -p ${ATCFdir}/${at}${NO}${syyyy}
          fi
          if [ -d ${ATCFdir}/${at}${NO}${syyyy} ]
          then
            #XXW cat atcfunix_file.$mct >>${ATCFdir}/${at}${NO}${syyyy}/a${at}${NO}${syyyy}.dat
            cat atcfunix_file.$mct >>${ATCFdir}/${at}${NO}${syyyy}/a${at}${NO}${syyyy}.dat
            cat atcfunix_file.$mct >>${ATCFdir}/${at}${NO}${syyyy}/ncep_a${at}${NO}${syyyy}.dat
            if [ ${cmodel} = 'gfs' ]; then
              cat atcfunix_file.$mct | sed -e "s/ GFSO/ AVNO/g" >>${ATCFdir}/${at}${NO}${syyyy}/a${at}${NO}${syyyy}.dat
              cat atcfunix_file.$mct | sed -e "s/ GFSO/ AVNO/g" >>${ATCFdir}/${at}${NO}${syyyy}/ncep_a${at}${NO}${syyyy}.dat
            fi
            set +x
            echo " "
            echo "+++ Adding records to  TPC ATCFUNIX directory: /tpcprd/atcf_unix/${at}${NO}${syyyy}"
            echo " "
            set -x
          else
            set +x
            echo " "
            echo "There is no TPC ATCFUNIX directory for: /tpcprd/atcf_unix/${at}${NO}${syyyy}"
            set -x
          fi
        done
      fi
     fi
    fi

  fi

else

  if [ ${PARAFLAG} = 'YES' ]
  then
    echo " "
  else
    tmtrakstat=/gpfs/gd2/emc/hwrf/save/${userid}/trak/prod/tracker.prod.status
    echo "ERROR: ${atcfout} tracker FAILED for ${PDY}${CYL}" >>${tmtrakstat}
  fi

  set +x
  echo " "
  echo "!!! ERROR -- An error occurred while running gettrk.x, "
  echo "!!! which is the program that actually gets the track."
  echo "!!! Return code from gettrk.x = ${gettrk_rcc}"
  echo "!!! model= ${atcfout}, forecast initial time = ${PDY}${CYL}"
  echo "!!! Exiting...."
  echo " "
  set -x
  err_exit " FAILED ${jobid} - ERROR RUNNING GETTRK IN TRACKER SCRIPT- ABNORMAL EXIT"

fi

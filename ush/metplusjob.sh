#!/bin/ksh
set -x
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------
## NCEP EMC GLOBAL MODEL VERIFICATION
##
## SCRIPT: metplusjob.sh
## CONTRIBUTORS: Mallory Row, mallory.row@noaa.gov, NOAA/NWS/NCEP/EMC-VPPGB
## PURPOSE: metplusjob.sh is called by vrfy.sh and is used as a driver script
##          in the NCEP/EMC FV3GFS workflow to do various types verification 
##          using NCAR/DTC's METplus while forecast is running. It manages step 
##          1 and step 2 for grid-to-grid, grid-to-obs, and precipitation verification.
##
## HISTORY:
## Mallory Row, May 1, 2018: This script was created to connect METplus into the
##              FV3GFS workflow. It was modifed from the previously used vsdbjob.sh
##              from the VSDB global verification system.              
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Below are all the variables that must be set
## Variables passed in from call in vrfy.sh
export VSTART_DATE=${1:-$(echo $($NDATE -${VRFYBACKDATE} $CDATE) | cut -c1-8)}   # Step 1 forecast starting date
export VEND_DATE=${2:-$(echo $($NDATE -${VRFYBACKDATE} $CDATE) | cut -c1-8)}     # Step 1 forecast ending date
export CHECK_DATE=${3:-$CDATE}                                                   # Check with $STEP2_END_DATE to check to see if time to start making plots 
export cycle=${4:-$cyc}                                                          # Forecast cycle
export expname=${5:-$PSLOT}                                                      # Experiment name
export expdump=${6:-$CDUMP}                                                      # Experiment dump
export rundir_base=${7:-${RUNDIR}/${CDUMP}/${CDATE}/vrfy/metplus_exp}            # Run directory for METplus verification
export expdir=${8:-${NOSCRUB}/archive}                                           # Experiment online archive
export iauf00=${9:-"NO"}                                                         # Set pgbf00=pgbanl for forecasts with IAU
## Environment variables inherited from that are used: config.base, config.vrfy
## config.base
##   gfs_cyc, METver, METPLUSver
## config.vrfy
##   VRFY_STEP(1)(2), VRFY_GRID2GRID, VRFY_GRID2OBS, VRFY_PRECIP, VRFYBACKDATE,
##   VRFYBACKDATE_PRECIP, metplussave, metplushome, metplusconfig, metplusfix, vfhmin, vfmax,
##   ftypelist, ptyplist, anltype, rain_bucket, g2g_sfc, STEP2_START_DATE, STEP2_END_DATE
##   webhost, webhostid, SEND2WEB, WEB_DIR, mdlist
## Make sure variables are set and set to names used in this script
gfs_cyc=${gfs_cyc:-1}                                                       # number of GFS cycles, 1-->00Z, 2-->00Z 12Z, 4-->00Z 06Z 12Z and 18Z
METver=${METver:-6.1}                                                       # MET version to use
METPLUSver=${METPLUSver:-1.0}                                               # METplus version to use
VRFY_STEP1=${VRFY_STEP1:-NO}                                                # run METplus verification step 1: partial sum and/or contingency table counts
VRFY_STEP2=${VRFY_STEP2:-NO}                                                # run METplus verification step 2: make plots
VRFY_GRID2GRID=${VRFY_GRID2GRID:-NO}                                        # run METplus desired steps for grid-to-grid verification
VRFY_GRID2OBS=${VRFY_GRID2OBS:-NO}                                          # run METplus desired steps for grid-to-obs verification
VRFY_PRECIP=${VRFY_PRECIP:-NO}                                              # run METplus desired steps for precipitation verification
VRFYBACKDATE=${VRFYBACKDATE:-24}                                            # execute step 1 metplusjob for the x hours
VRFYBACKDATE_PRECIP=${VRFYBACKDATE_PRECIP:-24}                              # additonal back up time for QPF verification data to allow observation data to come in 
metplussave=${metplussave:-"$NOSCRUB/archive/metplus_data"}                 # place to save METplus database
metplushome=${metplushome:-$BASE_VERIF_METPLUS}                             # location of global verification script
metplusconfig=${metplusconfig:-"$PARMgfs/verif"}                            # locaton of configuration files to run METplus
metplusfix=${metplusfix:-"$FIXgfs/fix_verif"}                               # location of fix files to run METplus
vfhmin=${vfmin:-$FHMIN_GFS}                                                 # start forecast hour
vfhmax=${vfhmax:-$FHMAX_GFS}                                                # end forecast hour
anltype=${anltype:-"gfs"}                                                   # analysis type for verification: gfs or gdas
ftyplist=${ftyplist:-"pgbq"}                                                # verif. files used for computing precip. verif.
ptyplist=${ptyplst:-"PRATE"}                                                # precip types in GRIB: PRATE or APCP
rain_bucket=${rain_bucket:-6}                                               # prate/apcp in pgb files in 6 hour buckets for GSM, continuous for FV3
g2g_sfc=${g2g_sfc:-"NO"}                                                    # include the group of surface variables for grid-to-grid verification
STEP2_START_DATE=${STEP2_START_DATE:-"$SDATE"}                              # starting date for METplus plots
STEP2_END_DATE=${STEP2_END_DATE:-"$EDATE"}                                  # ending date for METplus plots
webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}                                 # host for web display
webhostid=${webhostid:-$LOGNAME}                                            # id of webhost
ftpdir=${WEBDIR:-/home/people/emc/www/htdocs/gmb/$webhostid/METplus/$PSLOT} # web save directory
doftp=${SEND2WEB:-"NO"}                                                     # whether or not to sent maps to ftpdir
mdlist=${mdlist:-"gfs $PSLOT "}                                             # exps (up to 10) to compare in plots
##-------------------------------------------------------------------


##---------------------------------------------------------------------------
## Set up some general and machine related paths, load modules
export machine=${machine}                                        # machine name              
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')               # machine name in CAPS
export ACCOUNT=${ACCOUNT}                                        # computer ACCOUNT task
export CUE2RUN=${CUE2RUN:-$QUEUE}                                # batch queue
export CUE2FTP=${CUE2FTP:-$QUEUE_ARCH}                           # queue for data transfer
export GROUP=${GROUP:-g01}                                       # account group, not sure what this is       
chost=`echo $(hostname) |cut -c 1-1 `
chost2=`echo $(hostname) |cut -c 1-2 `

if [ $machine = THEIA ]; then
    export STMP=${STMP:-/scratch4/NCEPDEV/stmp3/${USER}}                                 # temporary directory
    export PTMP=${PTMP:-/scratch4/NCEPDEV/stmp4/${USER}}                                 # temporary directory
    export gstat=/scratch4/NCEPDEV/global/noscrub/stat                                   # global stats directory
    export ndate=${NDATE:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate}  # date executable
elif [ $machine = WCOSS_C ]; then
    export STMP=${STMP:-/gpfs/hps3/stmp/${USER}}                                  # temporary directory
    export PTMP=${PTMP:-/gpfs/hps3/ptmp/${USER}}                                  # temporary directory
    export gstat=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat                  # global stats directory
    export ndate=${NDATE:-/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.24/exec/ndate}  # date executable
else
    echo "EXIT ERROR: ${machine} IS NOT CURRENTLY SUPPORTED IN metplusjob.sh AT THIS TIME. EXITING metplusjob.sh!"
    exit
fi
export PATH="${metplushome}/ush:${PATH}"
export PYTHONPATH="${metplushome}/ush:${PYTHONPATH}"
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Set switches to run various verification steps based variables set in config.vrfy, current cycle,
## number of cycles running per day, and date (for step 2)
## VRFY_GRID2GRID_STEP1, VRFY_GRID2GRID_STEP2,
## VRFY_GRID2OBS_STEP1, VRFY_GRID2OBS_STEP2
## VRFY_PRECIP_STEP1, VRFY_PRECIP_STEP2
if [ $gfs_cyc = 1 ]; then
    export vhrlist=${vhrlist:-"$cycle"}            #verification hours
    export fcyclist="$cycle"                       #forecast cycles to be included in step 1
    export cyc2runmetplus="$cycle"                    #cycle to run step 1 which will generate METplus data for all cycles of the day
elif [ $gfs_cyc = 2 ]; then
    export vhrlist=${vhrlist:-"00 12 "}            #verification hours
    export fcyclist="00 12"                        #forecast cycles to be included in step 1 computation
    export cyc2runmetplus=12                          #cycle to run step 1 which will generate METplus data for all cycles of the day
elif [ $gfs_cyc = 4 ]; then
    export vhrlist=${vhrlist:-"00 06 12 18"}       #verification hours
    export fcyclist="00 06 12 18"                  #forecast cycles to be included in step 1 computation
    export cyc2runmetplus=18                          #cycle to run p which will generate vsdb data for all cycles of the day
else
    echo "EXIT ERROR: gfs_cyc must be 1, 2 or 4. EXITING metplusjob.sh!"                                          
    exit
fi
# Checks for step 1
if [ $cycle != $cyc2runmetplus ]; then 
    VRFY_GRID2GRID_STEP1=NO 
    VRFY_GRID2OBS_STEP1=NO 
    VRFY_PRECIP_STEP1=NO
else
    if [ $VRFY_STEP1 = YES -a $VRFY_GRID2GRID = YES ]; then
       VRFY_GRID2GRID_STEP1=YES
    else
       VRFY_GRID2GRID_STEP1=NO
    fi
    if [ $VRFY_STEP1 = YES -a $VRFY_GRID2OBS = YES ]; then
       VRFY_GRID2OBS_STEP1=YES
    else 
       VRFY_GRID2OBS_STEP1=NO
    fi
    if [ $VRFY_STEP1 = YES -a $VRFY_PRECIP = YES ]; then
       VRFY_PRECIP_STEP1=YES
    else
       VRFY_PRECIP_STEP1=NO 
    fi
fi
# Checks for step 2
if [ $CHECK_DATE != $STEP2_END_DATE ] ; then
    VRFY_GRID2GRID_STEP2=NO
    VRFY_GRID2OBS_STEP2=NO
    VRFY_PRECIP_STEP2=NO
else
    if [ $VRFY_STEP2 = YES -a $VRFY_GRID2GRID = YES ]; then
       VRFY_GRID2GRID_STEP2=YES
    else
       VRFY_GRID2GRID_STEP2=NO
    fi
    if [ $VRFY_STEP2 = YES -a $VRFY_GRID2OBS = YES ]; then
       VRFY_GRID2OBS_STEP2=YES
    else
       VRFY_GRID2OBS_STEP2=NO
    fi
    if [ $VRFY_STEP2 = YES -a $VRFY_PRECIP = YES ]; then
       VRFY_PRECIP_STEP2=YES
    else
       VRFY_PRECIP_STEP2=NO
    fi
fi
# Special checks for precip
if [ $cycle != 00 -a $cycle != 12 ]; then 
    VRFY_PRECIP_STEP1=NO
    VRFY_PRECIP_STEP2=NO
fi
##--------------------------------------------------------------------------- 

##---------------------------------------------------------------------------
## Determine MPMD jobs setup
if [ $machine = WCOSS -o $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
    if [ $CUE2RUN = dev_shared ]; then
        export MPMD="NO"
    else
        export MPMD="YES"
        if [ $machine = WCOSS -a $CUE2RUN = dev ] ; then
            export nproc=16
        else
            export nproc=24
        fi
    fi
else
    export MPMD="NO"
fi
##--------------------------------------------------------------------------

##---------------------------------------------------------------------------
## Grid-to-grid verification step 1: compute partial sums
if [ $VRFY_GRID2GRID_STEP1 = YES ] ; then
    echo "===== RUNNING VRFY_GRID2GRID_STEP1 ====="
    echo "===== creating partial sum data for grid-to-grid verifcation using METplus ====="
    #set some environment variables
    export fcyclist="$fcyclist"                              #all fcst cycles to be included in verification 
    export vhrlist="$vhrlist"                                #valid hours to verify
    export expnlist=$expname                                 #experiment names 
    export expdlist=$expdir                                  #exp online archive directories
    export complist=$(hostname)                              #computers where experiments are run
    export dumplist=".gfs."                                  #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_g2g1_base=$rundir_base/grid2grid_step1     #run directory, where to save METplus output
    export anltype=$anltype                                  #analysis type for verification: gfs or gdas
    export VDATEST=$VSTART_DATE                              #verification starting date
    export VDATEND=$VEND_DATE                                #verification ending date
    export vfhmin=$vfhmin                                    #start forecast hour  
    export vfhmax=$vfhmax                                    #end forecast hour
    #export asub=${asub:-a}                                   #string in pgb anl file after pgb, say, pgbanl, pgbhnl 
    #export fsub=${fsub:-f}                                   #string in pgb fcst file after pgb, say, pgbf06, pgbh06
    #do some checks 
    if [ ! -d $metplushome ]; then
        echo "EXIT ERROR: $metplushome does not exist "
        exit
    fi
    if [ ! -d $expdlist ]; then
        echo "EXIT ERROR: $expdlist does not exist "
        exit
    fi
    #create directories for output
    mkdir -p $rundir_g2g1_base $metplussave
    #determine requested forecast hours for verification
    export nvhr=`echo $vhrlist |wc -w`           #number of verification hours
    export nfcyc=`echo $fcyclist |wc -w`         #number of forecast cycles per day
           nfout=$nvhr ; if [ $nfcyc -gt $nvhr ]; then nfout=$nfcyc ; fi
    export fhout=`expr 24 \/ $nfout `            #forecast output frequency
    export nfcst=`expr $vlength \/ $fhout ` 
    export vlength=`expr $nfcst \* $fhout  `
    ffcst=${vfhmin}
    ffcst_count=0
    while [ $ffcst -le $vfhmax ] ; do
        if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
        if [ $ffcst_count -eq 0 ] ; then
            vfh_list=${ffcst}
        else
            vfh_list[$ffcst_count]=${ffcst}
        fi
        ffcst_count=` expr $ffcst_count + 1 `
        ffcst=$((ffcst+fhout))
    done
    export vfh_list_config=$( printf "%s," "${vfh_list[@]}" | cut -d "," -f 1-${#vfh_list[@]} )
    echo $vfh_list_config
    #determine which grid-to-grid verification types to run
    if [ $g2g_sfc = "YES" ]; then      
        typelist="anom pres sfc"
    else
        typelist="anom pres"
    fi
    #run METplus
    nexp=`echo $expnlist |wc -w`
    n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
    n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
    n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
    #
    nn=1
    while [ $nn -le $nexp ] ; do
        export exp=${expname[nn]}           #exp name
        export exp_dir=${expdir[nn]}        #exp directory
        export cdump=${dumpname[nn]:-".gfs."} #file dump format
        export obtype=$exp                  #obs/analysis data type for verification
        for vhr in $vhrlist; do
            VDATE=${VDATEST}${vhr}
            while [ $VDATE -le ${VDATEND}${vhr} ] ; do
                export VDATE=$VDATE
                export rundir_g2g1=${rundir_g2g1_base}/${VDATE}
                if [ -d ${rundir_g2g1} ] ; then
                    echo "REMOVING ${rundir_g2g1}"
                    rm -r $rundir_g2g1
                fi
                mkdir -p ${rundir_g2g1}/make_met_data
                mkdir -p ${rundir_g2g1}/VSDB_format
                mkdir -p ${rundir_g2g1}/logs/${exp}
                mkdir -p ${rundir_g2g1}/confs/${exp}
                mkdir -p ${rundir_g2g1}/jobs/${exp}
                #create poejob scripts, if MPMD=YES, else run METplus
                if [ $MPMD = YES ] ; then
                    #do some pre-checks for files and creating directories
                    anlfile=$exp_dir/$exp/pgbanl${cdump}${VDATE}
                    if [ -s $anlfile ]; then
                        mpmd_test_pass="YES"
                        if [ $g2g_sfc = "YES" ]; then
                           f00file=$exp_dir/$exp/pgbf00${cdump}${VDATE}
                           if [ -s $f00file ]; then
                               export typelist4mpmd="anom pres sfc"
                           else
                               export typelist4mpmd="anom pres"
                               work=${rundir_g2g1}/make_met_data/sfc/${exp}
                               echo "ERROR: ${f00file} doesn't exist or zero-sized. SKIPPING grid-to-grid sfc verification for this date." 
                               mkdir -p ${work}/${VDATE}00
                               echo "${f00file} doesn't exist or zero-sized. No grid-to-grid sfc verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt      
                           fi
                        else
                           export typelist4mpmd="anom pres"
                        fi
                    else
                        mpmd_test_pass="NO"
                        work1=${rundir_g2g1}/make_met_data/pres/${exp}
                        work2=${rundir_g2g1}/make_met_data/anom/${exp}
                        echo "ERROR: ${anlfile} doesn't exist or zero-sized. SKIPPING grid-to-grid pres and anom verification for this date." 
                        mkdir -p ${work1}/${VDATE}00
                        echo "${anlfile} doesn't exist or zero-sized. No grid-to-grid pres verification for this date." >> ${work1}/${VDATE}00/error_${VDATE}.txt
                        mkdir -p ${work2}/${VDATE}00
                        echo "${anlfile} doesn't exist or zero-sized. No grid-to-grid anom verification for this date." >> ${work2}/${VDATE}00/error_${VDATE}.txt
                    fi
                    #
                    if [ $mpmd_test_pass = "YES" ] ; then
                        export poedir=${rundir_g2g1}/jobs/${exp}/poe_scripts
                        mkdir -p $poedir
                        #output from METplus poejobs need to be written to individual directories
                        #otherwise sometimes errors get thrown by METplus when trying to make the
                        #directory; need to create directory to move all output to when jobs are done
                        for type in $typelist4mpmd ; do
                            mkdir -p ${rundir_g2g1}/make_met_data/${type}/${exp}/${VDATE}00/grid_stat
                        done
                        python ${USHgfs}/metplusjob_create_g2g_poejobscripts.py
                        cd $poedir ||exit 8
                        chmod u+x poejob*.sh
                        ncount=$(ls -l poejob*.sh |wc -l)
                        nc=0; iproc=0; node=1
                        while [ $nc -lt $ncount ]; do
                            if [ $iproc -ge $nproc ]; then iproc=0; node=$((node+1)); fi
                                poescript=$poedir/poe_node${node}
                                if [ $iproc -eq 0 ]; then
                                    rm -f $poescript; touch $poescript
                                fi
                                nc=$((nc+1))
                                iproc=$((iproc+1))
                                echo "$poedir/poejob${nc}.sh" >>$poescript
                                if [ $iproc -eq $nproc -o $nc -eq $ncount ]; then
                                    export MP_PGMMODEL=mpmd
                                    export MP_CMDFILE=${poescript}
                                    launcher="aprun -j 1 -n ${iproc} -N ${iproc} -d 1 cfp"
                                    if [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ] ; then
                                        $launcher $MP_CMDFILE
                                    else
                                        $launcher
                                    fi
                                    export err=$?
                                    echo "EXIT ${MP_CMDFILE} WITH ${err}"
                                    if [ $err -ne 0 ]; then sh +x ${poescript} ; fi
                               fi
                        done
                        for type in $typelist4mpmd ; do
                            export type=$type
                            export savedir=${metplussave}/${type}/${vhr}Z/${exp}
                            mkdir -p ${savedir}
                            if [ ${type} = anom ] ; then
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2grid_${type}_statanalysis_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            else
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2grid_${type}_statanalysis_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            fi
                            cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                        done
                    fi
                else
                    for type in $typelist ; do
                        export type=${type}
                        export savedir=${metplussave}/${type}/${vhr}Z/${exp}
                        mkdir -p ${savedir}
                        export work=${rundir_g2g1}/make_met_data/${type}/${exp}
                        mkdir -p $work
                        if [ ${type} = pres ] ; then
                            anlfile=$exp_dir/$exp/pgbanl${cdump}${VDATE}
                            if [ -s $anlfile ]; then
                                echo "==== running METplus grid-to-grid for ${type} for ${VDATE} ${exp} ===="
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            else
                                echo "ERROR: ${anlfile} doesn't exist or zero-sized. SKIPPING grid-to-grid ${type} verification for this date." 
                                mkdir -p ${work}/${VDATE}00
                                echo "${anlfile} doesn't exist or zero-sized. No grid-to-grid ${type} verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt
                            fi
                        elif [ ${type} = anom ] ; then
                            anlfile=$exp_dir/$exp/pgbanl${cdump}${VDATE}
                            if [ -s $anlfile ]; then
                                echo "==== running METplus grid-to-grid for ${type} for ${VDATE} ${exp} ===="
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1a.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            else
                                echo "ERROR: ${anlfile} doesn't exist or zero-sized. SKIPPING grid-to-grid ${type} verification for this date." 
                                mkdir -p ${work}/${VDATE}00
                                echo "${anlfile} doesn't exist or zero-sized. No grid-to-grid ${type} verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt
                            fi
                        elif [ ${type} = sfc ] ; then
                            f00file=$exp_dir/$exp/pgbf00${cdump}${VDATE}
                            if [ -s $f00file ]; then
                                echo "==== running METplus grid-to-grid for ${type} for ${VDATE} ${exp} ===="
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            else
                                echo "ERROR: ${f00file} doesn't exist or zero-sized. SKIPPING grid-to-grid ${type} verification for this date." 
                                mkdir -p ${work}/${VDATE}00
                                echo "${f00file} doesn't exist or zero-sized. No grid-to-grid ${type} verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt
                            fi
                        else
                            echo "ERROR: grid-to-grid ${type} is not supported."
                            mkdir -p ${work}/${VDATE}00
                            echo "grid-to-grid ${type} is not supported."  >> ${work}/${VDATE}00/error_${VDATE}.txt   
                        fi
                    done 
                fi
                VDATE=$($ndate +24 $VDATE)
            done
        done
        nn=`expr $nn + 1 `
    done
fi
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Grid-to-grid verification step 2: create plots
if [ $VRFY_GRID2GRID_STEP2 = YES ] ; then
    echo "ERROR: VRFY_GRID2GRID_STEP2 IS NOT SUPPORTED AT THIS TIME"
fi
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Grid-to-observations verification step 1: compute regular partial sums
if [ $VRFY_GRID2OBS_STEP1 = YES ] ; then
    echo "ERROR: VRFY_GRID2OBS_STEP1 IS NOT SUPPORTED AT THIS TIME"
fi
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Grid-to-observations verification step 2: create plots
if [ $VRFY_GRID2OBS_STEP2 = YES ] ; then
    echo "ERROR: VRFY_GRID2OBS_STEP2 IS NOT SUPPORTED AT THIS TIME"
fi
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Precipitation verification step 1: compute contingency table counts
if [ $VRFY_PRECIP_STEP1 = YES ] ; then
    echo "ERROR: VRFY_PRECIP_STEP1 IS NOT SUPPORTED AT THIS TIME"
fi
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Precipitation verification step 2: create plots
if [ $VRFY_PRECIP_STEP2 = YES ] ; then
    echo "ERROR: VRFY_PRECIP_STEP2 IS NOT SUPPORTED AT THIS TIME"
fi
## --------------------------------------------------------------
exit
########################################################################
########################################################################
      if [ $MAKEVSDBDATA = YES ] ; then
###   make vsdb database

export fcyclist="$fcyclist"                         ;#all fcst cycles to be included in verification
export expnlist=$exp1name                           ;#experiment names 
export expdlist=$exp1dir                            ;#exp online archive directories
export complist=$(hostname)                         ;#computers where experiments are run
export dumplist=".gfs."                             ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}

export anl_type=$anl_type                           ;#analysis type for verification: gfs, gdas or canl
export DATEST=$DATEST                               ;#verification starting date
export DATEND=$DATEND                               ;#verification ending date
export vlength=$vlength                             ;#forecast length in hour
export asub=${asub:-a}                              ;#string in pgb anal file after pgb, say, pgbanl, pgbhnl 
export fsub=${fsub:-f}                              ;#string in pgb fcsy file after pgb, say, pgbf06, pgbh06

if [ ! -d $vsdbhome ]; then
 echo "$vsdbhome does not exist "
 exit
fi
if [ ! -d $expdlist ]; then
 echo "$expdlist does not exist "
 exit
fi

export rundir=$rundir0/acrmse_stat
#export listvar1=fcyclist,vhrlist,expnlist,expdlist,complist,dumplist,DATEST,DATEND,vlength,rundir
#export listvar2=machine,anl_type,scppgb,sfcvsdb,canldir,ecmanldir,vsdbsave,vsdbhome,gd,NWPROD
#export listvar="$listvar1,$listvar2"

${vsdbhome}/verify_exp_step1.sh

### --------------------------------------------------------------
      fi                                       
### --------------------------------------------------------------


 
### --------------------------------------------------------------
###   make AC and RMSE maps            
      if [ $MAKEMAPS = YES ] ; then
### --------------------------------------------------------------
#
export mdlist=${mdlist:-"gfs $exp1name"}        ;#experiment names, up to 10                                     
export fcyclist="$fcyclist"                     ;#forecast cycles to show on map 
export DATEST=${VSDB_START_DATE:-$DATEST}       ;#map starting date  starting date to show on map
export DATEND=$DATEND                           ;#verification ending date to show on map
export vlength=$vlength                         ;#forecast length in hour to show on map
export maptop=${maptop:-10}                     ;#can be set to 10, 50 or 100 hPa for cross-section maps
export maskmiss=${maskmiss:-1}                  ;#remove missing data from all models to unify sample size, 0-->NO, 1-->Yes

set -A namelist $mdlist
export rundir=$rundir0/acrmse_map  

${vsdbhome}/verify_exp_step2.sh
### --------------------------------------------------------------
    fi
### --------------------------------------------------------------


### --------------------------------------------------------------
###   make CONUS precip plots
      if [ $CONUSPLOTS = YES ] ; then
### --------------------------------------------------------------
export expnlist=$mdlist                                             ;#experiment names, up to 6 
export expdlist=${expd_list:-"$exp1dir $exp1dir $exp1dir $exp1dir $exp1dir $exp1dir"}    ;#precip stats online archive dirs
export complist=${comp_list:-"$(hostname) $(hostname) $(hostname) $(hostname) $(hostname) $(hostname) "}  ;#computers where experiments are run

export cycle=$cycle                                       ;#cycle to make QPF plots 
export DATEST=$DATEST                                     ;#forecast starting date to show on map
export DATEND=$(echo $($NWPROD/util/exec/ndate -${VBACKUP_PRCP:-00} ${DATEND}00 ) |cut -c1-8 )
export rundir=$rundir0/rain_map  
export scrdir=${vsdbhome}/precip                  
export vhour=${vhr_rain:-${vhour:-180}}                                 ;#verification length in hour
                                                                                                                           
${scrdir}/plot_pcp.sh
### --------------------------------------------------------------
      fi
### --------------------------------------------------------------
                                                                                                                           

### --------------------------------------------------------------
###   compute precip threat score stats over CONUS
      if [ $CONUSDATA = YES ] ; then
### --------------------------------------------------------------
export cycle=$cycle                                 ;#cycle to generate QPF stats data
export expnlist=$exp1name                           ;#experiment names 
export expdlist=`dirname $COMROT`                   ;#exp online archive directories
export complist=$(hostname)                         ;#computers where experiments are run
export dumplist=".gfs."                             ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
export DATEST=`$NWPROD/util/exec/ndate -${VBACKUP_PRCP:-00} ${DATEST}00 |cut -c 1-8 ` ;#verification starting date
export DATEND=`$NWPROD/util/exec/ndate -${VBACKUP_PRCP:-00} ${DATEND}00 |cut -c 1-8 ` ;#verification starting date

export ftyplist=${ftyplist:-"flxf"}                 ;#file types: pgbq or flxf
export dumplist=${dumplist:-".gfs."}                ;#file format ${ftyp}f${fhr}${dump}${yyyymmdd}${cyc}
export ptyplist=${ptyplist:-"PRATE"}                ;#precip types in GRIB: PRATE or APCP
export fhout=6                                      ;#forecast output frequency in hours
export vhour=${vhr_rain:-${vhour:-180}}             ;#verification length in hour
export ARCDIR=${ARCDIR1:-$GNOSCRUB/$LOGNAME/archive} ;#directory to save stats data
export rundir=$rundir0/rain_stat  
export scrdir=${vsdbhome}/precip

#export listvar1=expnlist,expdlist,complist,ftyplist,dumplist,ptyplist,bucket,fhout,cyclist,vhour
#export listvar2=machine,DATEST,DATEND,ARCDIR,rundir,scrdir,OBSPCP,mapdir,scppgb,NWPROD
#export listvar="$listvar1,$listvar2"

${scrdir}/mkup_rain_stat.sh  
### --------------------------------------------------------------
      fi
### --------------------------------------------------------------


### --------------------------------------------------------------
###   make grid2obs vsdb database
      if [ $VRFYG2OBS = YES ] ; then
### --------------------------------------------------------------
export cyclist="$fcyclist"                  ;#all fcst cycles to be included in verification
export expnlist="$exp1name"                  ;#experiment names 
export expdlist="$exp1dir"                   ;#exp online archive directories
export complist="$(hostname)"               ;#computers where experiments are run
export dumplist=".gfs."                     ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
export fhoutair="6"                         ;#forecast output frequency in hours for raobs vrfy
export fhoutsfc="3"                         ;#forecast output frequency in hours for sfc vrfy
export gdtype="3"                           ;#pgb file resolution, 2 for 2.5-deg and 3 for 1-deg
export vsdbsfc="YES"                        ;#run sfc verification
export vsdbair="YES"                        ;#run upper-air verification
if [ $vlength -ge 168 ]; then
 export vlength=168                          ;#forecast length in hour
else
 export vlength=$vlength                     ;#forecast length in hour
fi
export DATEST=`$NWPROD/util/exec/ndate -${VBACKUP_G2OBS:-00} ${DATEST}00 |cut -c 1-8 ` ;#verification starting date
export DATEND=`$NWPROD/util/exec/ndate -${VBACKUP_G2OBS:-00} ${DATEND}00 |cut -c 1-8 ` ;#verification ending date
export batch=YES
export rundir=$rundir0/grid2obs_stat  
export HPSSTAR=${HPSSTAR:-/u/Fanglin.Yang/bin/hpsstar}
export hpssdirlist=${hpsslist:-"/5year/NCEPDEV/emc-global/$LOGNAME/$machine"}
export runhpss=${runhpss:-NO}               ;#run hpsstar in batch mode if data are missing

if [ ! -d $vsdbhome ]; then
 echo "$vsdbhome does not exist "
 exit
fi
if [ ! -d $expdlist ]; then
 echo "$expdlist does not exist "
 exit
fi


#listvar1=vsdbhome,vsdbsave,cyclist,expnlist,expdlist,dumplist,complist,fhoutair,fhoutsfc,vsdbsfc,vsdbair,gdtype,vlength
#listvar2=NWPROD,SUBJOB,ACCOUNT,CUE2RUN,CUE2FTP,GROUP,DATEST,DATEND,rundir,HPSSTAR,gdas_prepbufr_arch,batch,runhpss,APRUN,COMROTNCO
#export listvar=$listvar1,$listvar2
${vsdbhome}/grid2obs/grid2obs.sh


### --------------------------------------------------------------
      fi                                       
### --------------------------------------------------------------

exit


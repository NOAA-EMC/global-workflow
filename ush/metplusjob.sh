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
##   VRFYBACKDATE_PRECIP, metplussave, metplushome, metplusconfig, metplusfix, fhrmin, fhrmax,
##   vhr_rain, ftypelist, ptyplist, anltype, rain_bucket, g2g_sfc, STEP2_START_DATE, STEP2_END_DATE
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
fhrmin=${fhrmin:-$FHMIN_GFS}                                                # start forecast hour
fhrmax=${fhrmax:-$FHMAX_GFS}                                                # end forecast hour
vhr_rain=${vhr_rain:-$FHMAX_GFS}                                            # end verification forecast hour for precip, needed to create 0.25 deg grib1 files
anltype=${anltype:-"gfs"}                                                   # analysis type for verification: gfs or gdas
ftyplist=${ftyplist:-"pgbf"}                                                # file types: pgbf, pgbq, or flxf
ptyplist=${ptyplist:-"APCP"}                                                # precip types in GRIB: PRATE or APCP
rain_bucket=${rain_bucket:-"6"}                                             # accumulation bucket in hours. bucket=0 -- continuous accumulation
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


export prepbufr_prod_upper_air_dir=/gpfs/hps/nco/ops/com/gfs/prod                    # directory leading to ops. prepbufr files 
export prepbufr_prod_conus_sfc_dir=/com2/nam/prod                                    # directory leading to ops. prepbufr files
export ccpa_prod_dir=/com/verf/prod                                                  # directory leading to ops. 24 accum. CCPA files

if [ $machine = THEIA ]; then
    export STMP=${STMP:-/scratch4/NCEPDEV/stmp3/${USER}}                                 # temporary directory
    export PTMP=${PTMP:-/scratch4/NCEPDEV/stmp4/${USER}}                                 # temporary directory
    export gstat=/scratch4/NCEPDEV/global/noscrub/stat                                   # global stats directory
    export prepbufr_arch_dir=/scratch4/NCEPDEV/global/noscrub/stat/prepbufr              # prepbufr archive directory
    export ndate=${NDATE:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate}  # date executable
elif [ $machine = WCOSS_C ]; then
    export STMP=${STMP:-/gpfs/hps3/stmp/${USER}}                                         # temporary directory
    export PTMP=${PTMP:-/gpfs/hps3/ptmp/${USER}}                                         # temporary directory
    export gstat=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat                         # global stats directory
    export prepbufr_arch_dir=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/prepbufr         # prepbufr archive directory
    export ndate=${NDATE:-/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.24/exec/ndate}         # date executable
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
    export cyc2runmetplus="$cycle"                 #cycle to run step 1 which will generate METplus data for all cycles of the day
elif [ $gfs_cyc = 2 ]; then
    export vhrlist=${vhrlist:-"00 12 "}            #verification hours
    export fcyclist="00 12"                        #forecast cycles to be included in step 1 computation
    export cyc2runmetplus=12                       #cycle to run step 1 which will generate METplus data for all cycles of the day
elif [ $gfs_cyc = 4 ]; then
    export vhrlist=${vhrlist:-"00 06 12 18"}       #verification hours
    export fcyclist="00 06 12 18"                  #forecast cycles to be included in step 1 computation
    export cyc2runmetplus=18                       #cycle to run p which will generate vsdb data for all cycles of the day
else
    echo "EXIT ERROR: gfs_cyc must be 1, 2 or 4. EXITING metplusjob.sh!"                                          
    exit
fi
## Checks for step 1
if [ $cycle != $cyc2runmetplus ]; then 
    VRFY_GRID2GRID_STEP1=NO 
    VRFY_GRID2OBS_STEP1=NO 
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
fi
## Checks for step 2
if [ $CHECK_DATE != $STEP2_END_DATE ] ; then
    VRFY_GRID2GRID_STEP2=NO
    VRFY_GRID2OBS_STEP2=NO
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
fi
## Special checks for precip
if [ $cycle != 00 -a $cycle != 12 ]; then 
    VRFY_PRECIP_STEP1=NO
    VRFY_PRECIP_STEP2=NO
else 
    #check for step 1
    if [ $VRFY_STEP1 = YES -a $VRFY_PRECIP = YES ]; then
       VRFY_PRECIP_STEP1=YES
    else
       VRFY_PRECIP_STEP1=NO
    fi
    #check for step 2
    if [ $CHECK_DATE != $STEP2_END_DATE ] ; then
        VRFY_PRECIP_STEP2=NO
    else
        if [ $VRFY_STEP2 = YES -a $VRFY_PRECIP = YES ]; then
            VRFY_PRECIP_STEP2=YES
        else
            VRFY_PRECIP_STEP2=NO 
        fi
    fi   
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
    export dumplist=".gfs."                                  #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_g2g1_base=$rundir_base/grid2grid_step1     #run directory, where to save METplus output
    export anltype=$anltype                                  #analysis type for verification: gfs or gdas
    export VDATEST=$VSTART_DATE                              #verification starting date
    export VDATEND=$VEND_DATE                                #verification ending date
    export fhrmin=$fhrmin                                    #start forecast hour  
    export fhrmax=$fhrmax                                    #end forecast hour
    #do some checks 
    if [ ! -d $metplushome ]; then
        echo "EXIT ERROR: $metplushome does not exist "
        exit
    fi
    #create directories for output
    mkdir -p $rundir_g2g1_base $metplussave
    #determine requested forecast hours for verification
    export nvhr=`echo $vhrlist |wc -w`                       #number of verification hours
    export nfcyc=`echo $fcyclist |wc -w`                     #number of forecast cycles per day
    nfout=$nvhr ; if [ $nfcyc -gt $nvhr ]; then nfout=$nfcyc ; fi
    export fhrout=`expr 24 \/ $nfout `                        #forecast output frequency
    ffcst=${fhrmin}
    ffcst_count=0
    while [ $ffcst -le $fhrmax ] ; do
        if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
        if [ $ffcst_count -eq 0 ] ; then
            fhr_list=${ffcst}
        else
            fhr_list[$ffcst_count]=${ffcst}
        fi
        ffcst_count=` expr $ffcst_count + 1 `
        ffcst=$((ffcst+fhrout))
    done
    export fhr_list_config=$( printf "%s," "${fhr_list[@]}" | cut -d "," -f 1-${#fhr_list[@]} )
    #determine which grid-to-grid verification types to run
    if [ $g2g_sfc = "YES" ]; then      
        typelist="anom pres sfc"
    else
        typelist="anom pres"
    fi
    #run METplus
    n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
    n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
    n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
    nexp=`echo $expnlist |wc -w`
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
            mkdir -p ${rundir_g2g1}/logs
            mkdir -p ${rundir_g2g1}/confs
            mkdir -p ${rundir_g2g1}/jobs
            mkdir -p ${rundir_g2g1}/data
            #
            nn=1
            while [ $nn -le $nexp ] ; do
                export exp=${expname[nn]}           #exp name
                export exp_dir=${expdir[nn]}        #exp directory
                export cdump=${dumpname[nn]}        #file dump format
                export obtype=$exp                  #obs/analysis data type for verification
                mkdir -p ${rundir_g2g1}/logs/${exp}
                mkdir -p ${rundir_g2g1}/confs/${exp}
                mkdir -p ${rundir_g2g1}/jobs/${exp}
                mkdir -p ${rundir_g2g1}/data/${exp}
                #get model files
                if [ -s $exp_dir/$exp/pgbanl${cdump}${VDATE} ] ; then
                    ln -sf $exp_dir/$exp/pgbanl${cdump}${VDATE} ${rundir_g2g1}/data/${exp}/.
                fi
                fhr=${fhrmin}
                while [ $fhr -le $fhrmax ] ; do
                    if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
                    IDATE=$($ndate -$fhr $VDATE)
                    if [ -s $exp_dir/$exp/pgbf${fhr}.gfs.${IDATE} ] ; then
                        ln -sf $exp_dir/$exp/pgbf${fhr}.gfs.${IDATE} ${rundir_g2g1}/data/${exp}/.
                    fi
                    fhr=$((fhr+fhrout))
                done 
                #do checks for needed "truth" files, run verfication types for files that exist
                anlfile=${rundir_g2g1}/data/${exp}/pgbanl${cdump}${VDATE}
                if [ -s $anlfile ]; then 
                    if [ $g2g_sfc = "YES" ]; then
                        f00file=${rundir_g2g1}/data/$exp/pgbf00${cdump}${VDATE}
                        if [ -s $f00file ]; then
                            export typelist="anom pres sfc"
                        else
                            export typelist="anom pres"
                            echo "ERROR: ${f00file} doesn't exist or zero-sized. SKIPPING grid-to-grid sfc verification for this date."
                            work=${rundir_g2g1}/make_met_data/sfc/${exp}
                            mkdir -p ${work}/${VDATE}00
                           echo "${f00file} doesn't exist or zero-sized. No grid-to-grid sfc verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt
                        fi
                    else
                        export typelist="anom pres"
                    fi
                else
                    echo "ERROR: ${anlfile} doesn't exist or zero-sized. SKIPPING grid-to-grid pres and anom verification for this date." 
                    work=${rundir_g2g1}/make_met_data/pres/${exp}
                    mkdir -p ${work}/${VDATE}00
                    echo "${anlfile} doesn't exist or zero-sized. No grid-to-grid pres verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt
                    work=${rundir_g2g1}/make_met_data/anom/${exp}
                    mkdir -p ${work}/${VDATE}00
                    echo "${anlfile} doesn't exist or zero-sized. No grid-to-grid anom verification for this date." >> ${work}/${VDATE}00/error_${VDATE}.txt
                    typelist=""                    
                fi
                #run grid-to-grid verification for requested and avaiable types
                ntype=`echo $typelist |wc -w`
                if [ $ntype -ge 2 ] ; then
                    #run in MPMD style if MPMD=YES
                    if [ $MPMD = YES ] ; then
                        export poedir=${rundir_g2g1}/jobs/${exp}/poe_scripts
                        mkdir -p $poedir
                        #output from METplus poejobs need to be written to individual directories
                        #otherwise sometimes errors get thrown by METplus when trying to make the
                        #directory; need to create directory to move all output to when jobs are done
                        for type in $typelist ; do
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
                        for type in $typelist ; do
                            export type=$type
                            export savedir=${metplussave}/${type}/${vhr}Z/${exp}
                            mkdir -p ${savedir}
                            if [ ${type} = anom ] ; then
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2grid_${type}_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            else
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2grid_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                             fi
                             cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                        done
                    #run in serially if MPMD=NO
                    else
                        for type in $typelist ; do
                            export type=${type}
                            export savedir=${metplussave}/${type}/${vhr}Z/${exp}
                            mkdir -p ${savedir}
                            export work=${rundir_g2g1}/make_met_data/${type}/${exp}
                            mkdir -p $work
                            echo "==== running METplus grid-to-grid for ${type} for ${VDATE} ${exp} ===="
                            if [ ${type} = anom ] ; then
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1a.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            elif [ $type = pres -o $type = sfc ] ; then
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1.conf -c ${metplusconfig}/machine_config/machine.${machine}
                                cp ${rundir_g2g1}/VSDB_format/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            else
                                echo "ERROR: grid-to-grid ${type} is not supported."
                                mkdir -p ${work}/${VDATE}00
                                echo "grid-to-grid ${type} is not supported."  >> ${work}/${VDATE}00/error_${VDATE}.txt   
                            fi
                        done 
                    fi
                fi
                nn=`expr $nn + 1 `
            done
            VDATE=$($ndate +24 $VDATE)
        done
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
    echo "===== RUNNING VRFY_GRID2OBS_STEP1 ====="
    echo "===== creating partial sum data for grid-to-obs verifcation using METplus ====="
    #set some environment variables
    export fcyclist="$fcyclist"                              #all fcst cycles to be included in verification 
    export expnlist=$expname                                 #experiment names 
    export expdlist=$expdir                                  #exp online archive directories
    export dumplist=".gfs."                                  #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_g2o1_base=$rundir_base/grid2obs_step1      #run directory, where to save METplus output
    export VDATEST=$VSTART_DATE                              #verification starting date
    export VDATEND=$VEND_DATE                                #verification ending date
    export fhrmin=$fhrmin                                    #start forecast hour 
    if [ $fhrmax -ge 168 ]; then                             #end forecast hour
        export fhrmax=168
    else
        export fhrmax=$fhrmax
    fi
    export run_upper_air="YES"                               #run grid-to-obs upper_air use case
    export fhrout_upper_air="6"                              #grid-to-obs upper_air use case forecast output frequency in hours
    export grid_upper_air="G003"                             #grid for grid-to-obs upper_air use case, format GXXX
    export run_conus_sfc="YES"                               #run grid-to-obs conus_sfc use case
    export fhrout_conus_sfc="3"                              #grid-to-obs conus_sfc use case forecast output frequency in hours
    export grid_conus_sfc="G104"                             #grid for grid-to-obs conus_sfc use case, format GXXX
    #do some checks 
    if [ ! -d $metplushome ]; then
        echo "EXIT ERROR: $metplushome does not exist "
        exit
    fi
    #create directories for output
    mkdir -p $rundir_g2o1_base $metplussave
    #specify verification hours in a day 
    if [ $fhrout_upper_air -eq 12 ]; then
        export vhrlist_upper_air="00 12"
        export vhr_upper_air_start="00"
        export vhr_upper_air_end="12"
        export vhr_upper_air_inc="12"
    elif [ $fhrout_upper_air -eq 6 ]; then
        export vhrlist_upper_air="00 06 12 18"
        export vhr_upper_air_start="00"
        export vhr_upper_air_end="18"
        export vhr_upper_air_inc="6"
    elif [ $fhrout_upper_air -eq 3 ]; then
        export vhrlist_upper_air="00 03 06 09 12 15 18 21"
        export vhr_upper_air_start="00"
        export vhr_upper_air_end="21"
        export vhr_upper_air_inc="3"
    else
        echo "ERROR: fhrout_upper_air=$fhrout_upper_air hours is not supported"
        export run_upper_air="NO"
    fi
    if [ $fhrout_conus_sfc -eq 12 ]; then
        export vhrlist_conus_sfc="00 12"
        export vhr_conus_sfc_start="00"
        export vhr_conus_sfc_end="12"
        export vhr_conus_sfc_inc="12"
    elif [ $fhrout_conus_sfc -eq 6 ]; then
        export vhrlist_conus_sfc="00 06 12 18"
        export vhr_conus_sfc_start="00"
        export vhr_conus_sfc_end="18"
        export vhr_conus_sfc_inc="6"
    elif [ $fhrout_conus_sfc -eq 3 ]; then
        export vhrlist_conus_sfc="00 03 06 09 12 15 18 21"
        export vhr_conus_sfc_start="00"
        export vhr_conus_sfc_end="21"
        export vhr_conus_sfc_inc="3"
    else
        echo "ERROR: fhrout_conus_sfc=$fhrout_conus_sfc hours is not supported"
        export run_conus_sfc="NO"
    fi
    if [ $run_upper_air = "NO" -a $run_conus_sfc = "NO" ]; then 
        echo "EXIT ERROR: run_upper_air and run_conus_sfc are both NO"
        exit
    fi
    export vhr_upper_air_end=$((vhr_upper_air_end+1))
    export vhr_conus_sfc_end=$((vhr_conus_sfc_end+1))
    #run METplus
    n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
    n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
    n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
    nexp=`echo $expnlist |wc -w`
    VDATE=${VDATEST}
    while [ $VDATE -le ${VDATEND} ] ; do
        export VDATE=$VDATE
        export rundir_g2o1=${rundir_g2o1_base}/${VDATE}
        if [ -d ${rundir_g2o1} ] ; then
            echo "REMOVING ${rundir_g2o1}"
            rm -r $rundir_g2o1
        fi
        mkdir -p ${rundir_g2o1}/make_met_data
        mkdir -p ${rundir_g2o1}/VSDB_format
        mkdir -p ${rundir_g2o1}/logs
        mkdir -p ${rundir_g2o1}/confs
        mkdir -p ${rundir_g2o1}/jobs
        mkdir -p ${rundir_g2o1}/data
        #get prepbufr data
        if [ $run_upper_air = "YES" ]; then
            export prepbufr_upper_air="gdas"
            mkdir -p ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}
            for vhr in $vhrlist_upper_air ; do
                #first check online directories
                if [ -s ${prepbufr_prod_upper_air_dir}/${prepbufr_upper_air}.${VDATE}/${prepbufr_upper_air}.t${vhr}.prepbufr  ] ; then
                    ln -sf ${prepbufr_prod_upper_air_dir}/${prepbufr_upper_air}.${VDATE}/${prepbufr_upper_air}.t${vhr}.prepbufr ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}
                elif [ -s ${prepbufr_arch_dir}/${prepbufr_upper_air}/prepbufr.${prepbufr_upper_air}.${VDATE}${vhr} ] ; then
                    ln -sf ${prepbufr_arch_dir}/${prepbufr_upper_air}/prepbufr.${prepbufr_upper_air}.${VDATE}${vhr} ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}
                else
                    #get file from HPSS if not in online directories
                    HPSSGFS="/NCEPPROD/hpssprod/runhistory"
                    yyyy=`echo $VDATE |cut -c 1-4 `
                    mm=`echo $VDATE |cut -c 5-6 `
                    dd=`echo $VDATE |cut -c 7-8 `
                    if [ ${yyyy}${mm}${dd} -ge 20170720 ] ; then
                        gdas_tar=${HPSSGFS}/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}/gpfs_hps_nco_ops_com_gfs_prod_gdas.${VDATE}${vhr}.tar
                        gdas_prepbufr_file=gdas.t${vhr}z.prepbufr
                    elif [ ${yyyy}${mm}${dd} -ge 20160510 ] && [ ${yyyy}${mm}${dd} -lt 20170720 ] ; then
                        gdas_tar=${HPSSGFS}/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}/com2_gfs_prod_gdas.${VDATE}${vhr}.tar
                        gdas_prepbufr_file=gdas1.t${vhr}z.prepbufr
                    else
                        gdas_tar=${HPSSGFS}/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}/com_gfs_prod_gdas.${VDATE}${vhr}.tar
                        gdas_prepbufr_file=gdas1.t${vhr}z.prepbufr
                    fi
                    htar -xf ${gdas_tar} ./${gdas_prepbufr_file}
                    if [ $? -eq 0 ]; then
                        mv ${gdas_prepbufr_file} ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}
                    else
                        echo "NO GDAS PREPBUFR FILE FOR ${VDATE}${vhr}"
                    fi
                fi
            done
        fi
        if [ $run_conus_sfc = "YES" ]; then
            if [ $VDATE -le 20170319 ]; then
                export prepbufr_conus_sfc="ndas"
                mkdir -p ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}
                for vhr in $vhrlist_conus_sfc; do
                    DATE=${VDATE}${vhr}
                    for xh in 00 03 06 09 12 15 18 21; do
                        xdate=$($ndate +$xh $DATE )
                        eval YYYY${xh}=`echo $xdate |cut -c 1-4 `
                        eval YYYYMM${xh}=`echo $xdate |cut -c 1-6 `
                        eval PDY${xh}=`echo $xdate |cut -c 1-8 `
                        eval HH${xh}=`echo $xdate |cut -c 9-10 `
                    done
                    HPSSNAM="/NCEPPROD/hpssprod/runhistory"
                    
                    case $HH00 in
                     00) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                         hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
                     03) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
                     06) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                         hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
                     09) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
                     12) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                         hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
                     15) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
                     18) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                         hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
                     21) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                         hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
                    esac
                    case $HH00 in
                     00) date1=${PDY12}
                         date2=${PDY06}
                         date3=${PDY00};;
                     03) date1=${PDY09}
                         date2=${PDY03};;
                     06) date1=${PDY12}
                         date2=${PDY06}
                         date3=${PDY00};;
                     09) date1=${PDY09}
                         date2=${PDY03};;
                     12) date1=${PDY12}
                         date2=${PDY06}
                         date3=${PDY00};;
                     15) date1=${PDY09}
                         date2=${PDY03};;
                     18) date1=${PDY12}
                         date2=${PDY06}
                         date3=${PDY00};;
                     21) date1=${PDY09}
                         date2=${PDY03};;
                    esac
                    case $HH00 in
                     00) ndas1=ndas.t${HH12}z.prepbufr.tm12
                         ndas2=ndas.t${HH06}z.prepbufr.tm06
                         ndas3=nam.t${HH00}z.prepbufr.tm00;;
                     03) ndas1=ndas.t${HH09}z.prepbufr.tm09
                         ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                     06) ndas1=ndas.t${HH12}z.prepbufr.tm12
                         ndas2=ndas.t${HH06}z.prepbufr.tm06
                         ndas3=nam.t${HH00}z.prepbufr.tm00;;
                     09) ndas1=ndas.t${HH09}z.prepbufr.tm09
                         ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                     12) ndas1=ndas.t${HH12}z.prepbufr.tm12
                         ndas2=ndas.t${HH06}z.prepbufr.tm06
                         ndas3=nam.t${HH00}z.prepbufr.tm00;;
                     15) ndas1=ndas.t${HH09}z.prepbufr.tm09
                         ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                     18) ndas1=ndas.t${HH12}z.prepbufr.tm12
                         ndas2=ndas.t${HH06}z.prepbufr.tm06
                         ndas3=nam.t${HH00}z.prepbufr.tm00;;
                     21) ndas1=ndas.t${HH09}z.prepbufr.tm09
                         ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                    esac
                    if [ -s ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date1}/$ndas1 ] ; then
                        ln -sf ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date1}/$ndas1 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                    else
                        htar -xf ${hpss1} ./$ndas1
                        if [ $? -eq 0 ]; then
                            mv ${ndas1} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                        else
                            if [ -s ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date2}/$ndas2 ] ; then
                                ln -sf ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date2}/$ndas2 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                            else
                                htar -xf ${hpss2} ./$ndas2
                                if [ $? -eq 0 ]; then
                                    mv ${ndas2} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                                else
                                    if [ -s ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date3}/$ndas3 ] ; then
                                        ln -sf ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date3}/$ndas3 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                                    else
                                        htar -xf ${hpss3} ./$ndas3
                                        if [ $? -eq 0 ]; then
                                            mv ${ndas3} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                                        else
                                            echo "NO NDAS PREPBUFR FILE FOR ${VDATE}${vhr}"
                                        fi
                                    fi
                                fi
                            fi
                        fi
                    fi
                done
            else
                export prepbufr_conus_sfc="nam"
                mkdir -p ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}
                for vhr in $vhrlist_conus_sfc; do
                    DATE=${VDATE}${vhr}
                    HH=`echo $DATE |cut -c 9-10 `
                    if [ $(((HH/6)*6)) -eq $HH ]; then
                        xdate=$DATE
                        suffix=tm00
                    else
                        xdate=$($ndate +3 $DATE )
                        suffix=tm03
                    fi
                    eval YYYY=`echo $xdate |cut -c 1-4 `
                    eval YYYYMM=`echo $xdate |cut -c 1-6 `
                    eval PDY=`echo $xdate |cut -c 1-8 `
                    eval CYC=`echo $xdate |cut -c 9-10 `
                    namcomdir=${prepbufr_prod_conus_sfc_dir}/nam.$PDY
                    namarcdir=${prepbufr_arch_dir}/${prepbufr_conus_sfc}/nam.$PDY
                    bufrfile=nam.t${CYC}z.prepbufr.$suffix
                    if [ -s $namcomdir/$bufrfile ]; then
                        ln -sf $namcomdir/$bufrfile ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                    elif [ -s $namarcdir/$bufrfile ]; then
                        ln -sf $namarcdir/$bufrfile ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                    else
                        HPSSNAM="/NCEPPROD/hpssprod/runhistory"
                        if [ $PDY -eq 20170320 ] ; then
                            nam_tar=$HPSSNAM/rh${YYYY}/${YYYYMM}/${PDY}/com_nam_prod_nam.${PDY}${CYC}.bufr.tar
                        else
                            nam_tar=$HPSSNAM/rh${YYYY}/${YYYYMM}/${PDY}/com2_nam_prod_nam.${PDY}${CYC}.bufr.tar
                        fi
                        htar -xf ${nam_tar} ./$bufrfile
                        if [ $? -eq 0 ]; then
                            mv ${bufrfile} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                        else
                            echo "NO NAM PREPBUFR FILE FOR ${VDATE}${vhr}"
                        fi
                    fi
                done
            fi
        fi
        #
        nn=1
        while [ $nn -le $nexp ] ; do
            export exp=${expname[nn]}             #exp name
            export exp_dir=${expdir[nn]}          #exp directory
            export cdump=${dumpname[nn]}          #file dump format
            mkdir -p ${rundir_g2o1}/logs/${exp}
            mkdir -p ${rundir_g2o1}/confs/${exp}
            mkdir -p ${rundir_g2o1}/jobs/${exp}
            mkdir -p ${rundir_g2o1}/data/${exp}
            #get model files
            if [ $run_upper_air = "YES" ]; then
                for vhr in $vhrlist_upper_air ; do
                    DATE=${VDATE}${vhr}
                    fhr=${fhrmin}
                    while [ $fhr -le $fhrmax ] ; do
                        if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
                        IDATE=$($ndate -$fhr $DATE)
                        if [ -s $exp_dir/$exp/pgbf${fhr}.gfs.${IDATE} ] ; then
                            ln -sf $exp_dir/$exp/pgbf${fhr}.gfs.${IDATE} ${rundir_g2o1}/data/${exp}/.
                        fi
                        fhr=$((fhr+fhrout_upper_air))
                    done
                done
            fi
            if [ $run_conus_sfc = "YES" ]; then
                for vhr in $vhrlist_conus_sfc ; do
                    DATE=${VDATE}${vhr}
                    fhr=${fhrmin}
                    while [ $fhr -le $fhrmax ] ; do
                        if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
                        IDATE=$($ndate -$fhr $DATE)
                        if [ -s $exp_dir/$exp/pgbf${fhr}.gfs.${IDATE} ] ; then
                            ln -sf $exp_dir/$exp/pgbf${fhr}.gfs.${IDATE} ${rundir_g2o1}/data/${exp}/.
                        fi
                        fhr=$((fhr+fhrout_conus_sfc))
                    done
                done
            fi
            #run in MPMD style if MPMD=YES, else run serially METplus
            if [ $MPMD = YES ] ; then
                if [ $run_upper_air = "YES" ]; then
                    type="upper_air"
                    echo "==== running METplus grid-to-obs for ${type} for ${VDATE} ${exp} ===="
                    export pb2nc_upper_air_dir=${rundir_g2o1}/make_met_data/upper_air/${exp}/pb2nc
                    export point_stat_upper_air_dir=${rundir_g2o1}/make_met_data/upper_air/${exp}/point_stat
                    mkdir -p ${pb2nc_upper_air_dir} ${point_stat_upper_air_dir}
                fi
                if [ $run_conus_sfc = "YES" ]; then
                    type="conus_sfc"
                    echo "==== running METplus grid-to-obs for ${type} for ${VDATE} ${exp} ===="
                    export pb2nc_conus_sfc_dir=${rundir_g2o1}/make_met_data/conus_sfc/${exp}/pb2nc
                    export point_stat_conus_sfc_dir=${rundir_g2o1}/make_met_data/conus_sfc/${exp}/point_stat
                    mkdir -p ${pb2nc_conus_sfc_dir} ${point_stat_conus_sfc_dir}
                fi
                #first run pb2nc to keep from writing over or having numerous copies of same file
                export g2o_process="pb2nc"
                export poedir=${rundir_g2o1}/jobs/${exp}/poe_scripts
                mkdir -p $poedir/${g2o_process}
                python ${USHgfs}/metplusjob_create_g2o_poejobscripts.py
                cd $poedir/${g2o_process} ||exit 8
                chmod u+x poejob*.sh
                ncount=$(ls -l poejob*.sh |wc -l)
                nc=0; iproc=0; node=1
                while [ $nc -lt $ncount ]; do
                    if [ $iproc -ge $nproc ]; then iproc=0; node=$((node+1)); fi
                        poescript=$poedir/${g2o_process}/poe_node${node}
                        if [ $iproc -eq 0 ]; then
                            rm -f $poescript; touch $poescript
                        fi
                        nc=$((nc+1))
                        iproc=$((iproc+1))
                        echo "$poedir/${g2o_process}/poejob${nc}.sh" >>$poescript
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
                            #if [ $err -ne 0 ]; then sh +x ${poescript} ; fi
                        fi
                done
                #second run point_stat
                export g2o_process="point_stat"
                export poedir=${rundir_g2o1}/jobs/${exp}/poe_scripts
                mkdir -p $poedir/${g2o_process}
                python ${USHgfs}/metplusjob_create_g2o_poejobscripts.py
                cd $poedir/${g2o_process} ||exit 8
                chmod u+x poejob*.sh
                ncount=$(ls -l poejob*.sh |wc -l)
                nc=0; iproc=0; node=1
                while [ $nc -lt $ncount ]; do
                    if [ $iproc -ge $nproc ]; then iproc=0; node=$((node+1)); fi
                        poescript=$poedir/${g2o_process}/poe_node${node}
                        if [ $iproc -eq 0 ]; then
                            rm -f $poescript; touch $poescript
                        fi
                        nc=$((nc+1))
                        iproc=$((iproc+1))
                        echo "$poedir/${g2o_process}/poejob${nc}.sh" >>$poescript
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
                            #if [ $err -ne 0 ]; then sh +x ${poescript} ; fi
                        fi
                done
                #third run stat_analysis
                n=0 ; for fcyc in $fcyclist ; do n=$((n+1)) ; fcycname[n]=$fcyc  ; done
                export fcyc_start=${fcycname[1]}
                export fcyc_end=${fcycname[$gfs_cyc]}
                export fcyc_int=`expr 24 \/ $gfs_cyc  \* 3600 `
                if [ $run_upper_air = "YES" ]; then
                    type="upper_air"
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2obs_upper_air_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                    for fcycl in $fcyclist ; do
                        export savedir=${metplussave}/grid2obs/${fcycl}Z/${exp}
                        mkdir -p ${savedir}
                        cp ${rundir_g2o1}/VSDB_format/upper_air/${fcycl}Z/${exp}/*.stat ${savedir}/${exp}_air_${VDATE}.stat
                    done
                fi
                if [ $run_conus_sfc = "YES" ]; then
                    type="conus_sfc"
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2obs_conus_sfc_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                    for fcycl in $fcyclist ; do
                        export savedir=${metplussave}/grid2obs/${fcycl}Z/${exp}
                        mkdir -p ${savedir}
                        cp ${rundir_g2o1}/VSDB_format/conus_sfc/${fcycl}Z/${exp}/*.stat ${savedir}/${exp}_sfc_${VDATE}.stat
                    done
                fi
            else
                if [ $run_upper_air = "YES" ]; then
                    type="upper_air"
                    echo "==== running METplus grid-to-obs for ${type} for ${VDATE} ${exp} ===="   
                    #export prepbufr_upper_air="gdas"
                    export pb2nc_upper_air_dir=${rundir_g2o1}/make_met_data/upper_air/${exp}/pb2nc
                    export point_stat_upper_air_dir=${rundir_g2o1}/make_met_data/upper_air/${exp}/point_stat
                    mkdir -p ${pb2nc_upper_air_dir} ${point_stat_upper_air_dir}
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_upper_air_step1a.conf -c ${metplusconfig}/machine_config/machine.${machine}
                    #now run stat_analysis
                    n=0 ; for fcyc in $fcyclist ; do n=$((n+1)) ; fcycname[n]=$fcyc  ; done
                    export fcyc_start=${fcycname[1]}
                    export fcyc_end=${fcycname[$gfs_cyc]}
                    export fcyc_int=`expr 24 \/ $gfs_cyc  \* 3600 `
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_upper_air_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                    for fcycl in $fcyclist ; do
                        export savedir=${metplussave}/grid2obs/${fcycl}Z/${exp}
                        mkdir -p ${savedir}
                        cp ${rundir_g2o1}/VSDB_format/upper_air/${fcycl}Z/${exp}/*.stat ${savedir}/${exp}_air_${VDATE}.stat
                    done
                fi
                if [ $run_conus_sfc = "YES" ]; then
                    type="conus_sfc"
                    echo "==== running METplus grid-to-obs for ${type} for ${VDATE} ${exp} ===="
                    export pb2nc_conus_sfc_dir=${rundir_g2o1}/make_met_data/conus_sfc/${exp}/pb2nc
                    export point_stat_conus_sfc_dir=${rundir_g2o1}/make_met_data/conus_sfc/${exp}/point_stat
                    mkdir -p ${pb2nc_conus_sfc_dir} ${point_stat_conus_sfc_dir} 
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_conus_sfc_step1a.conf -c ${metplusconfig}/machine_config/machine.${machine} 
                    #now run stat_analysis
                    n=0 ; for fcyc in $fcyclist ; do n=$((n+1)) ; fcycname[n]=$fcyc  ; done
                    export fcyc_start=${fcycname[1]}
                    export fcyc_end=${fcycname[$gfs_cyc]}
                    export fcyc_int=`expr 24 \/ $gfs_cyc  \* 3600 `
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_conus_sfc_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                    for fcycl in $fcyclist ; do
                        export savedir=${metplussave}/grid2obs/${fcycl}Z/${exp}
                        mkdir -p ${savedir}
                        cp ${rundir_g2o1}/VSDB_format/conus_sfc/${fcycl}Z/${exp}/*.stat ${savedir}/${exp}_sfc_${VDATE}.stat
                    done
                fi
            fi
            nn=`expr $nn + 1 `
        done
        VDATE=$(echo $($ndate +24 ${VDATE}00 ) |cut -c 1-8 )
    done
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
    export cycle=$cycle                                                                #cycle to generate QPF stats data
    export expnlist=$expname                                                           #experiment names 
    export expdlist=$expdir                                                            #exp online archive directories
    export dumplist=".gfs."                                                            #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_precip1_base=$rundir_base/precip_step1                               #run directory, where to save METplus output
    export VDATEST_precip=`$ndate -${VRFYBACKDATE_PRCP} ${VSTART_DATE}00 |cut -c 1-8 ` #verification starting date
    export VDATEND_precip=`$ndate -${VRFYBACKDATE_PRCP} ${VEND_DATE}00 |cut -c 1-8 `   #verification ending date
    export ftyplist=${ftyplist}                                                        #file types: pgbf, pgbq, or flxf
    export ptyplist=${ptyplist}                                                        #precip types in GRIB: PRATE or APCP
    export fhrmin=$fhrmin                                                              #start forecast hour
    if [ $vhr_rain -ge 168 ]; then                                                     #end forecast hour
        export fhrmax=168
    else
        export fhrmax=$vhr_rain
    fi
    export rain_bucket=${rain_bucket}                                                  #accumulation buckets in hours. bucket=0 -- continuous accumulation
    #do some checks 
    if [ ! -d $metplushome ]; then
        echo "EXIT ERROR: $metplushome does not exist "
        exit
    fi
    #create directories for output
    mkdir -p $rundir_precip1_base $metplussave
    #determine requested forecast hours for verification that match 12Z - 12Z valid frame
    if [ $cycle -eq 00 ]; then
        export fhrmin=$(($fhrmin+12))
        export fhrmax=$(($fhrmax-12))
    elif [ $cycle -eq 12 ]; then
        export fhrmin=${fhrmin}
        export fhrmax=${fhrmax}
    else
        echo "cycle=${cycle}Z not supported. Exiting."
        exit
    fi
    #run METplus
    n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
    n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
    n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
    n=0 ; for ftyp in $ftyplist ; do n=$((n+1)) ; ftypname[n]=$ftyp  ; done
    n=0 ; for ptyp in $ptyplist ; do n=$((n+1)) ; ptypname[n]=$ptyp  ; done
    n=0 ; for rb in $rain_bucket ; do n=$((n+1)) ; accum_bucket[n]=$rb  ; done
    nexp=`echo $expnlist |wc -w`
    VDATE=${VDATEST_precip}12
    while [ $VDATE -le ${VDATEND_precip}12 ] ; do
        export VDATE=$VDATE
        export YYYY=$(echo ${VDATE}  |cut -c 1-4 )
        export YYYYMM=$(echo ${VDATE}  |cut -c 1-6 )
        export PDY=$(echo ${VDATE}  |cut -c 1-8 )
        export rundir_precip1=${rundir_precip1_base}/${PDY}12/init_${cycle}Z
        if [ -d ${rundir_precip1} ] ; then
            echo "REMOVING ${rundir_precip1}"
            rm -r $rundir_precip1
        fi
        mkdir -p ${rundir_precip1}/make_met_data
        mkdir -p ${rundir_precip1}/VSDB_format
        mkdir -p ${rundir_precip1}/logs
        mkdir -p ${rundir_precip1}/confs
        mkdir -p ${rundir_precip1}/jobs
        mkdir -p ${rundir_precip1}/data
        #get CCPA data, valid VDATE-24hr 12Z - VDATE 12Z
        mkdir -p ${rundir_precip1}/data/CCPA
        if [ -s ${ccpa_prod_dir}/precip.${PDY}/ccpa.${PDY}12.24h ]; then
            ln -sf ${ccpa_prod_dir}/precip.${PDY}/ccpa.${PDY}12.24h ${rundir_precip1}/data/CCPA/.
        else
            ccpa_tar=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${PDY}/com_verf_prod_precip.${PDY}.precip.tar
            if [ $machine = THEIA ]; then
                echo "#!/bin/bash" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -l nodes=1:ppn=1" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -l walltime=0:05:00" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -A fv3-cpu" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -q service" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -o ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.out" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -j oe" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -N get_ccpa" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "#PBS -W umask=022" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "module load hpss" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh  
                echo "htar -xf ${ccpa_tar} ./ccpa.${PDY}12.24h" >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                echo "mv ccpa.${PDY}12.24h ${rundir_precip1}/data/CCPA/." >> ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
                qsub ${rundir_precip1}/data/CCPA/get_ccpa_data_${PDY}12.sh
            else
                htar -xf ${ccpa_tar} ./ccpa.${PDY}12.24h
            fi
            sleep 300
        fi
        #
        nn=1
        while [ $nn -le $nexp ] ; do
            export exp=${expname[nn]}           #exp name
            export exp_dir=${expdir[nn]}        #exp directory
            export cdump=${dumpname[nn]}        #file dump format
            export file_type=${ftypname[nn]}    #file type
            export precip_type=${ptypname[nn]}  #PRATE -> precip rate; APCP -> accumulated precip
            export bucket=${accum_bucket[nn]}   #bucket accumulation
            export file_template="${file_type}{lead?fmt=%HH}.gfs.{init?fmt=%Y%m%d%H}"
            mkdir -p ${rundir_precip1}/logs/${exp}
            mkdir -p ${rundir_precip1}/confs/${exp}
            mkdir -p ${rundir_precip1}/jobs/${exp}
            mkdir -p ${rundir_precip1}/data/${exp}
            #get model files, valid 12Z - 12Z for cycle
            if [ $bucket -eq 0 ]; then
                export fhrout=24
            else
                export fhrout=$bucket
            fi
            export nfhr_day=`expr 24 \/ $fhrout ` #number of model files to make 24 hour accumulation
            fhr_start=$((fhrmin+24))
            while [ $fhr_start -le $fhrmax ] ; do
                 if [ $fhr_start -lt 10 ]; then fhr_start=0$fhr_start ; fi
                 if [ $fhr_start -eq $((fhrmin+24)) ]; then
                     fhr_list=${fhr_start}
                     fhr_count=1
                 else
                     fhr_list[$fhr_count]=${fhr_start}
                     fhr_count=` expr $fhr_count + 1 `    
                 fi
                 IDATE=$($ndate -$fhr_start ${VDATE})
                 nfhr=1
                 while [ $nfhr -le $nfhr_day ]; do 
                     fhr=$(($fhr_start-$(($((nfhr-1))*$bucket))))
                     if [ $fhr -lt 10 ]; then fhr_start=0$fhr ; fi
                     if [ -s $exp_dir/$exp/${file_type}${fhr}.gfs.${IDATE} ] ; then
                         ln -sf $exp_dir/$exp/${file_type}${fhr}.gfs.${IDATE} ${rundir_precip1}/data/${exp}/.
                     fi
                     nfhr=$((nfhr+1))
                 done
                fhr_start=$((fhr_start+24))
            done
            export fhr_list_config=$( printf "%s," "${fhr_list[@]}" | cut -d "," -f 1-${#fhr_list[@]} )
            #run in MPMD style if MPMD=YES, else run serially METplus
            if [ $MPMD = YES ] ; then
                echo "MPMD HOLDER"
            else
                echo "==== running METplus precip for ${VDATE} ${exp} ===="
                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/precip_step1.conf -c ${metplusconfig}/machine_config/machine.${machine}
                export savedir=${metplussave}/precip/${cycle}/${exp}
                mkdir -p ${savedir}
                cp ${rundir_precip1}/VSDB_format/accum24/12Z/${exp}/*.stat ${savedir}/${exp}_precip_${PDY}.stat
            fi
            nn=`expr $nn + 1 `
        done
        VDATE=$(echo $($ndate +24 ${VDATE}))
    done

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

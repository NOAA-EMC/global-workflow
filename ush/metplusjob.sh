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
## Mallory Row, Oct 5, 2018: All step 1 verification connected.             
## Mallory Row, Dec 18, 2018: Added to collect partial sums by valid time
##                           Data can be collected in this new way or how
##                           VSDB gather data by variable gather_by 
##---------------------------------------------------------------------------
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Below are all the variables that must be set
## Variables passed in from call in vrfy.sh
export VSTART_DATE=${1:-$(echo $($NDATE -${VRFYBACK_HRS} $CDATE) | cut -c1-8)}   # Step 1 forecast starting date
export VEND_DATE=${2:-$(echo $($NDATE -${VRFYBACK_HRS} $CDATE) | cut -c1-8)}     # Step 1 forecast ending date
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
##   VRFY_STEP(1)(2), VRFY_GRID2GRID, VRFY_GRID2OBS, VRFY_PRECIP, VRFYBACK_HRS,
##   VRFYBACK_HRS_PRECIP, metplussave, metplushome, metplusconfig, metplusfix, fhrmin, fhrmax,
##   grid2grid_typelist, grid2obs_typelist, precip_accumlist, vhr_rain, ftypelist, ptyplist, anltype, fbucketlist,
##   STEP2_START_DATE, STEP2_END_DATE, webhost, webhostid, SEND2WEB, WEB_DIR, mdlist
## Make sure variables are set and set to names used in this script
gfs_cyc=${gfs_cyc:-1}                                                       # number of GFS cycles, 1-->00Z, 2-->00Z 12Z, 4-->00Z 06Z 12Z and 18Z
METver=${METver:-6.1}                                                       # MET version to use
METPLUSver=${METPLUSver:-1.0}                                               # METplus version to use
VRFY_STEP1=${VRFY_STEP1:-NO}                                                # run METplus verification step 1: partial sum and/or contingency table counts
VRFY_STEP2=${VRFY_STEP2:-NO}                                                # run METplus verification step 2: make plots
VRFY_GRID2GRID=${VRFY_GRID2GRID:-NO}                                        # run METplus desired steps for grid-to-grid verification
VRFY_GRID2OBS=${VRFY_GRID2OBS:-NO}                                          # run METplus desired steps for grid-to-obs verification
VRFY_PRECIP=${VRFY_PRECIP:-NO}                                              # run METplus desired steps for precipitation verification
VRFYBACK_HRS=${VRFYBACK_HRS:-24}                                            # execute step 1 metplusjob for the x hours
VRFYBACK_HRS_PRECIP=${VRFYBACK_HRS_PRECIP:-24}                              # additonal back up time for QPF verification data to allow observation data to come in 
metplussave=${metplussave:-"$NOSCRUB/archive/metplus_data"}                 # place to save METplus database
metplushome=${metplushome:-$BASE_VERIF_METPLUS}                             # location of global verification script
metplusconfig=${metplusconfig:-"$PARMgfs/verif"}                            # locaton of configuration files to run METplus
metplusfix=${metplusfix:-"$FIXgfs/fix_verif"}                               # location of fix files to run METplus
fhrmin=${fhrmin:-$FHMIN_GFS}                                                # start forecast hour
fhrmax=${fhrmax:-$FHMAX_GFS}                                                # end forecast hour
anltype=${anltype:-"gfs"}                                                   # default=gfs, analysis type for verification (future support for other analysis files)
grid2grid_typelist=${grid2grid_typelist:-"anom pres sfc"}                   # type of verifications to run for grid-to-grid
grid2obs_typelist=${grid2obs_typelist:-"conus_sfc upper_air"}               # type of verifications to run for grid-to-obs
precip_accumlist=${precip_accumlist:-"24"}                                  # accumulation lengths (in hrs) to verify
gather_by=${gather_by:-"VSDB_format"}                                       # how to gather METplus output: VSDB_format (future support: valid_format)
vhr_rain=${vhr_rain:-$FHMAX_GFS}                                            # needed to create 0.25 deg grib1 files
ftyplist=${ftyplist:-"pgbf"}                                                # file types: pgbf (future support: pgbq and flxf)
ptyplist=${ptyplist:-"APCP"}                                                # precip types in GRIB: APCP (future support: PRATE)
fbucketlist=${fbucketlist:-"6"}                                             # accumulation bucket in hrs: 6 (future support: other accums, bucket=0 -- continuous accumulation)
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
#machine info
export machine=${machine}                                        # machine name              
export machine=$(echo $machine|tr '[a-z]' '[A-Z]')               # machine name in CAPS
export ACCOUNT=${ACCOUNT}                                        # computer ACCOUNT task
export CUE2RUN=${CUE2RUN:-$QUEUE}                                # batch queue
export CUE2FTP=${CUE2FTP:-$QUEUE_ARCH}                           # queue for data transfer
export GROUP=${GROUP:-g01}                                       # account group, not sure what this is       

#operational data directories
export prepbufr_prod_upper_air_dir=/gpfs/hps/nco/ops/com/gfs/prod                    # directory leading to ops. prepbufr files 
export prepbufr_prod_conus_sfc_dir=/com2/nam/prod                                    # directory leading to ops. prepbufr files
export ccpa_prod_dir=/com/verf/prod                                                  # directory leading to ops. 24 accum. CCPA files

#set up machine paths and load modules
if [ $machine = THEIA ]; then
    source /apps/lmod/lmod/init/ksh
    module load hpss/hpss
    module use /contrib/modulefiles
    module load anaconda/anaconda2-4.4.0
    if [ $METver -eq 6.1 ]; then
        module load met/6.1
        export met_install_dir="/contrib/met/${METver}"
    elif [ $METver -eq 8.0]; then
        module load met/8.0
        export met_install_dir="/contrib/met/${METver}"
    else
        echo "EXIT ERROR: METV${METver} IS NOT CURRENTLY SUPPORTED IN metplusjob.sh AT THIS TIME ON ${machine}. EXITING metplusjob.sh!"
        exit
    fi
    export STMP=${STMP:-/scratch4/NCEPDEV/stmp3/${USER}}                                 # temporary directory
    export PTMP=${PTMP:-/scratch4/NCEPDEV/stmp4/${USER}}                                 # temporary directory
    export gstat=/scratch4/NCEPDEV/global/noscrub/stat                                   # global stats directory
    export prepbufr_arch_dir=/scratch4/NCEPDEV/global/noscrub/stat/prepbufr              # prepbufr archive directory
    export ndate=${NDATE:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate}  # date executable
    export nhour=${NHOUR:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/nhour}  # hour executable
elif [ $machine = WCOSS_C ]; then
    source /opt/modules/default/init/ksh
    module use /usrx/local/prod/modulefiles 
    module load hpss
    module use /usrx/local/dev/modulefiles
    module load python/2.7.14 
    if [ $METver -eq 6.1 ]; then
        module load met/6.1
        export met_install_dir="/usrx/local/dev/met/${METver}"
    elif [ $METver -eq 8.0]; then
        module use /gpfs/hps3/emc/global/noscrub/Julie.Prestopnik/modulefiles
        module load met/8.0
        export met_install_dir="/gpfs/hps3/emc/global/noscrub/Julie.Prestopnik/met/${METver}"
    else
        echo "EXIT ERROR: METV${METver} IS NOT CURRENTLY SUPPORTED IN metplusjob.sh AT THIS TIME ON ${machine}. EXITING metplusjob.sh!"
        exit
    fi
    export STMP=${STMP:-/gpfs/hps3/stmp/${USER}}                                         # temporary directory
    export PTMP=${PTMP:-/gpfs/hps3/ptmp/${USER}}                                         # temporary directory
    export gstat=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/stat                         # global stats directory
    export prepbufr_arch_dir=/gpfs/hps3/emc/global/noscrub/Fanglin.Yang/prepbufr         # prepbufr archive directory
    export ndate=${NDATE:-/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.24/exec/ndate}         # date executable
    export nhour=${NHOUR:-/gpfs/hps/nco/ops/nwprod/prod_util.v1.0.24/exec/nhour}         # hour executable
elif [ $machine = WCOSS_DELL_P3 ] ; then
    . /usrx/local/prod/lmot/lmod/init/profile
    module load ips/18.0.1.163
    module load impi/18.0.1    
    module load lsf/10.1       
    module load EnvVars/1.0.2
    module load HPSS/5.0.2.5
    module use -a /usrx/local/dev/modulefiles
    module load python/2.7.14
    if [ $METver -eq 8.0 ]; then
        module use /gpfs/dell2/emc/verification/noscrub/Julie.Prestopnik/modulefiles
        module load met/8.0
        export met_install_dir="/gpfs/dell2/emc/verification/noscrub/Julie.Prestopnik/met/${METver}"
    else
        echo "EXIT ERROR: METV${METver} IS NOT CURRENTLY SUPPORTED IN metplusjob.sh AT THIS TIME ON ${machine}. EXITING metplusjob.sh!"
        exit
    fi
    export STMP=${STMP:-/gpfs/dell3/stmp/$USER}                                               # temporary directory
    export PTMP=${PTMP:-/gpfs/dell3/ptmp/$USER}                                               # temporary directory
    export gstat=/gpfs/dell2/emc/modeling/noscrub/Fanglin.Yang/stat                           # global stats directory
    export prepbufr_arch_dir=/gpfs/dell2/emc/modeling/noscrub/Fanglin.Yang/prepbufr           # prepbufr archive directory
    export ndate=${NDATE:-/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/ndate}         # date executable
    export nhour=${NHOUR:-/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.0/exec/ips/nhour}         # hour executable
else
    echo "EXIT ERROR: ${machine} IS NOT CURRENTLY SUPPORTED IN metplusjob.sh AT THIS TIME. EXITING metplusjob.sh!"
    exit
fi

#check METplus exists
if [ -s ${metplushome} ]; then
    export PATH="${metplushome}/ush:${PATH}"
    export PYTHONPATH="${metplushome}/ush:${PYTHONPATH}"
else
    echo "EXIT ERROR: ${METPLUSver} DOES NOT EXIST. EXITING metplusjob.sh!"
    exit
fi
##---------------------------------------------------------------------------


##---------------------------------------------------------------------------
## Set switches to run various verification steps based variables set in config.vrfy, current cycle,
## number of cycles running per day, and date (for step 2)
## VRFY_GRID2GRID_STEP1, VRFY_GRID2GRID_STEP2,
## VRFY_GRID2OBS_STEP1, VRFY_GRID2OBS_STEP2
## VRFY_PRECIP_STEP1, VRFY_PRECIP_STEP2
if [ $gfs_cyc = 1 ]; then
    export fcyclist="$cycle"                       #forecast cycles to be included in step 1
    export cyc2runmetplus="$cycle"                 #cycle to run step 1 which will generate METplus data for all cycles of the day
elif [ $gfs_cyc = 2 ]; then
    export fcyclist="00 12"                        #forecast cycles to be included in step 1 computation
    export cyc2runmetplus=12                       #cycle to run step 1 which will generate METplus data for all cycles of the day
elif [ $gfs_cyc = 4 ]; then
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
if [ ${VEND_DATE}${cyc2runmetplus} -le $SDATE ]; then
    VRFY_GRID2GRID_STEP1=NO
    VRFY_GRID2OBS_STEP1=NO
fi
VDATEEND_precip=`$ndate -${VRFYBACK_HRS_PRECIP} ${VEND_DATE}00 |cut -c 1-8 `
if [ ${VDATEEND_precip}${cyc2runmetplus} -le $SDATE ]; then
    VRFY_PRECIP_STEP1=NO
fi
## Checks for step 2
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
    export expnlist=$expname                                 #experiment names 
    export expdlist=$expdir                                  #exp online archive directories
    export dumplist=".gfs."                                  #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_g2g1_base=$rundir_base/grid2grid_step1     #run directory, where to save METplus output
    export anltype=$anltype                                  #analysis type for verification: gfs or gdas
    export VDATESTART=$VSTART_DATE                           #verification starting date
    export VDATEEND=$VEND_DATE                               #verification ending date
    export fcyclist=$fcyclist                                #all fcst cycles to be included in verification
    export fhrmin=$fhrmin                                    #start forecast hour  
    export fhrmax=$fhrmax                                    #end forecast hour
    export gather_by=${gather_by}                            #how to gather METplus output: for g2g VSDB_format == valid_format
    #create directories for output
    mkdir -p $rundir_g2g1_base $metplussave
    #determine valid hours and requested forecast hours for verification
    if [ $gfs_cyc = 1 ]; then
        export vhrlist="$cycle"                              #verification hours
        export vhr_start="$cycle"                            #verification start hour
        export vhr_end="$cycle"                              #verification end hour
        export vhr_inc=86400                                 #verification hour increment
    elif [ $gfs_cyc = 2 ]; then
        export vhrlist="00 12"                               #verification hours
        export vhr_start=00                                  #verification start hour
        export vhr_end=12                                    #verification end hour
        export vhr_inc=43200                                 #verification hour increment
    elif [ $gfs_cyc = 4 ]; then
        export vhrlist="00 06 12 18"                         #verification hours
        export vhr_start=00                                  #verification start hour
        export vhr_end=18                                    #verification end hour
        export vhr_inc=21600                                 #verification hour increment
    fi
    export nvhr=`echo $vhrlist |wc -w`                       #number of verification hours
    export nfcyc=`echo $fcyclist |wc -w`                     #number of forecast cycles per day
    nfout=$nvhr ; if [ $nfcyc -gt $nvhr ]; then nfout=$nfcyc ; fi
    export fhrout=`expr 24 \/ $nfout `                       #forecast output frequency
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
    #run METplus
    n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
    n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
    n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
    nexp=`echo $expnlist |wc -w`
    VDATE=${VDATESTART}
    while [ $VDATE -le ${VDATEEND} ] ; do
        export VDATE=$VDATE
        export rundir_g2g1=${rundir_g2g1_base}/valid_${VDATE}
        if [ -d ${rundir_g2g1} ] ; then
            echo "REMOVING ${rundir_g2g1}"
            rm -r $rundir_g2g1
        fi
        mkdir -p  ${rundir_g2g1}/output_metplus_data ${rundir_g2g1}/output_${gather_by} ${rundir_g2g1}/logs ${rundir_g2g1}/confs ${rundir_g2g1}/jobs ${rundir_g2g1}/data
        nn=1
        while [ $nn -le $nexp ] ; do
            export exp=${expname[nn]}           #exp name
            export exp_dir=${expdir[nn]}        #exp directory
            export cdump=${dumpname[nn]}        #file dump format
            mkdir -p ${rundir_g2g1}/logs/${exp} ${rundir_g2g1}/confs/${exp} ${rundir_g2g1}/jobs/${exp} ${rundir_g2g1}/data/${exp}
            #get model data
            for vhr in $vhrlist; do
                #truth files
                if [ -s $exp_dir/$exp/pgbanl${cdump}${VDATE}${vhr} ] ; then
                    ln -sf $exp_dir/$exp/pgbanl${cdump}${VDATE}${vhr} ${rundir_g2g1}/data/${exp}/.
                fi
                if [ -s $exp_dir/$exp/pgbf00${cdump}${VDATE}${vhr} ] ; then
                    ln -sf $exp_dir/$exp/pgbf00${cdump}${VDATE}${vhr} ${rundir_g2g1}/data/${exp}/.
                fi
                #forecast files
                fhr=${fhrmin}
                while [ $fhr -le $fhrmax ] ; do
                    if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
                    IDATE=$($ndate -$fhr ${VDATE}${vhr})
                    if [ -s $exp_dir/$exp/pgbf${fhr}${cdump}${IDATE} ] ; then
                        ln -sf $exp_dir/$exp/pgbf${fhr}${cdump}${IDATE} ${rundir_g2g1}/data/${exp}/.
                    fi
                    fhr=$((fhr+fhrout))
                done
                #do checks for needed "truth" files, so verfication for types with existing files are only run
                for type in $grid2grid_typelist ; do
                    if [[ $type = pres || $type = anom ]]; then
                        truth_file=${rundir_g2g1}/data/${exp}/pgbanl${cdump}${VDATE}${vhr}
                    elif [ $type = sfc ]; then
                        truth_file=${rundir_g2g1}/data/$exp/pgbf00${cdump}${VDATE}${vhr}
                    fi
                    if [ -s $truth_file ]; then
                        grid2grid_run_typelist+=(${type})
                    else
                        echo "ERROR: ${truth_file} doesn't exist or zero-sized. SKIPPING grid-to-grid ${type} verification for this date (${VDATE}${vhr})." 
                        mkdir -p ${rundir_g2g1}/output_metplus_data/${type}/${exp}/${VDATE}${vhr}00
                        echo "${truth_file} doesn't exist or zero-sized. No grid-to-grid ${type} verification for this date (${VDATE}${vhr})." >> ${rundir_g2g1}/output_metplus_data/${type}/${exp}/${VDATE}${vhr}00/error_${VDATE}${vhr}.txt
                    fi
                done
            done
            #run grid-to-grid verification for requested and avaiable types
            if [ ${#grid2grid_run_typelist[@]} -ge 1 ] ; then
                #run in MPMD style if MPMD=YES
                if [ $MPMD = YES ] ; then
                    export poedir=${rundir_g2g1}/jobs/${exp}/poe_scripts
                    mkdir -p $poedir
                    #run MPMD for GridStat
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
                        fi
                    done
                    for type in "${grid2grid_run_typelist[@]}" ; do
                        for vhr in $vhrlist; do
                            export type=$type
                            #gather files
                            mkdir -p ${rundir_g2g1}/output_metplus_data/${type}/${exp}/${VDATE}${vhr}00/grid_stat
                            mv ${rundir_g2g1}/output_metplus_data/${type}/${exp}/poejob*/${VDATE}${vhr}00/grid_stat/* ${rundir_g2g1}/output_metplus_data/${type}/${exp}/${VDATE}${vhr}00/grid_stat/.
                            rm -r ${rundir_g2g1}/output_metplus_data/${type}/${exp}/poejob* 
                            export savedir=${metplussave}/${gather_by}/grid2grid/${type}/${vhr}Z/${exp}
                            mkdir -p ${savedir}
                            if [ ${type} = anom ] ; then
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2grid_${type}_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            else
                                ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/mpmd_confs/grid2grid_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                             fi
                             cp ${rundir_g2g1}/output_${gather_by}/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                        done
                    done
                else
                    for type in "${grid2grid_run_typelist[@]}" ; do
                        export type=${type}
                        mkdir -p ${rundir_g2g1}/output_metplus_data/${type}/${exp}
                        echo "==== running METplus grid-to-grid:${type} for ${exp} on ${VDATE}, ${vhrlist} ===="
                        #run grid_stat and stat_analysis
                        if [ ${type} = anom ] ; then
                            ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1a.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1c.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            #copy files to archive
                            for vhr in $vhrlist; do
                                export savedir=${metplussave}/${gather_by}/grid2grid/${type}/${vhr}Z/${exp}
                                mkdir -p ${savedir}
                                cp ${rundir_g2g1}/output_${gather_by}/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            done
                        elif [ $type = pres -o $type = sfc ] ; then
                            ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2grid_${type}_step1.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            #copy files to archive
                            for vhr in $vhrlist; do
                                export savedir=${metplussave}/${gather_by}/grid2grid/${type}/${vhr}Z/${exp}
                                mkdir -p ${savedir}
                                cp ${rundir_g2g1}/output_${gather_by}/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                            done
                        else
                            echo "ERROR: grid-to-grid ${type} is not supported."
                            for vhr in $vhrlist; do
                                mkdir -p ${rundir_g2g1}/output_metplus_data/${type}/${exp}/${VDATE}${vhr}00
                                echo "grid-to-grid ${type} is not supported."  >> ${rundir_g2g1}/output_metplus_data/${type}/${exp}/${VDATE}${vhr}00/error_${VDATE}.txt
                            done
                        fi
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
    export expnlist=$expname                                 #experiment names 
    export expdlist=$expdir                                  #exp online archive directories
    export dumplist=".gfs."                                  #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_g2o1_base=$rundir_base/grid2obs_step1      #run directory, where to save METplus output
    export VDATESTART=$VSTART_DATE                           #verification starting date
    export VDATEEND=$VEND_DATE                               #verification ending date
    export fcyclist=$fcyclist                                #all fcst cycles to be included in verification
    export fhrmin=$fhrmin                                    #start forecast hour 
    if [ $fhrmax -ge 168 ]; then                             #end forecast hour
        export fhrmax=168
    else
        export fhrmax=$fhrmax
    fi
    export fhrout_upper_air="6"                              #grid-to-obs upper_air use case forecast output frequency in hours
    export grid_upper_air="G003"                             #grid for grid-to-obs upper_air use case, format GXXX
    export fhrout_conus_sfc="3"                              #grid-to-obs conus_sfc use case forecast output frequency in hours
    export grid_conus_sfc="G104"                             #grid for grid-to-obs conus_sfc use case, format GXXX
    export gather_by=${gather_by}                            #how to gather METplus output: for g2o VSDB_format != valid_format
    export runhpss="NO"                                      #get missing prepbufr data from HPSS
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
    VDATE=${VDATESTART}
    while [ $VDATE -le ${VDATEEND} ] ; do
        export VDATE=$VDATE
        export rundir_g2o1=${rundir_g2o1_base}/valid_${VDATE}
        if [ -d ${rundir_g2o1} ] ; then
            echo "REMOVING ${rundir_g2o1}"
            rm -r $rundir_g2o1
        fi
        mkdir -p ${rundir_g2o1}/output_metplus_data ${rundir_g2o1}/output_${gather_by} ${rundir_g2o1}/logs ${rundir_g2o1}/confs ${rundir_g2o1}/jobs ${rundir_g2o1}/data
        #get prepbufr data
        for type in $grid2obs_typelist ; do
            if [ $type = "upper_air" ]; then
                export prepbufr_upper_air="gdas"
                mkdir -p ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}
                for vhr in $vhrlist_upper_air ; do
                    #first check online directories
                    if [ -s ${prepbufr_prod_upper_air_dir}/${prepbufr_upper_air}.${VDATE}/${prepbufr_upper_air}.t${vhr}.prepbufr  ] ; then
                        ln -sf ${prepbufr_prod_upper_air_dir}/${prepbufr_upper_air}.${VDATE}/${prepbufr_upper_air}.t${vhr}.prepbufr ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}
                    elif [ -s ${prepbufr_arch_dir}/${prepbufr_upper_air}/prepbufr.${prepbufr_upper_air}.${VDATE}${vhr} ] ; then
                        ln -sf ${prepbufr_arch_dir}/${prepbufr_upper_air}/prepbufr.${prepbufr_upper_air}.${VDATE}${vhr} ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}
                    elif [ $runhpss = YES ]; then
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
                        echo "htar -xf ${gdas_tar} ./${gdas_prepbufr_file}" > ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.sh
                        echo "mv ${gdas_prepbufr_file} ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}" >> ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.sh
                        if [ $machine = THEIA ]; then
                            qsub -l procs=1,walltime=0:05:00 -q service -A fv3-cpu -o ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.out -N get_gdas_prepbufr.${VDATE}${vhr} -W umask=022 ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.sh
                        elif [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
                            bsub -W 0:10 -q dev_transfer -P GFS_T2O -o ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.out -e ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.out -N get_gdas_prepbufr.${VDATE}${vhr} -J get_gdas_prepbufr.${VDATE}${vhr} -M 100 -R affinity[mem=100] ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr}.sh
                        fi
                    fi
                    if [ ! -s ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/prepbufr.${VDATE}${vhr} ]; then
                        echo "prepbufr.${VDATE}${vhr} doesn't exist or zero-sized." >> ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}/error_${prepbufr_upper_air}_${VDATE}${vhr}.txt
                    fi
                done
            elif [ $type = "conus_sfc" ]; then
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
                             hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar
                             date1=${PDY12}
                             date2=${PDY06}
                             date3=${PDY00}
                             ndas1=ndas.t${HH12}z.prepbufr.tm12
                             ndas2=ndas.t${HH06}z.prepbufr.tm06
                             ndas3=nam.t${HH00}z.prepbufr.tm00;;
                         03) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar
                             date1=${PDY09}
                             date2=${PDY03}
                             ndas1=ndas.t${HH09}z.prepbufr.tm09
                             ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                         06) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                             hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar
                             date1=${PDY12}
                             date2=${PDY06}
                             date3=${PDY00}
                             ndas1=ndas.t${HH12}z.prepbufr.tm12
                             ndas2=ndas.t${HH06}z.prepbufr.tm06
                             ndas3=nam.t${HH00}z.prepbufr.tm00;;
                         09) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar
                             date1=${PDY09}
                             date2=${PDY03}
                             ndas1=ndas.t${HH09}z.prepbufr.tm09
                             ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                         12) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                             hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar
                             date1=${PDY12}
                             date2=${PDY06}
                             date3=${PDY00}
                             ndas1=ndas.t${HH12}z.prepbufr.tm12
                             ndas2=ndas.t${HH06}z.prepbufr.tm06
                             ndas3=nam.t${HH00}z.prepbufr.tm00;;
                         15) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar
                             date1=${PDY09}
                             date2=${PDY03}
                             ndas1=ndas.t${HH09}z.prepbufr.tm09
                             ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                         18) hpss1=$HPSSNAM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
                             hpss3=$HPSSNAM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar
                             date1=${PDY12}
                             date2=${PDY06}
                             date3=${PDY00}
                             ndas1=ndas.t${HH12}z.prepbufr.tm12
                             ndas2=ndas.t${HH06}z.prepbufr.tm06
                             ndas3=nam.t${HH00}z.prepbufr.tm00;;
                         21) hpss1=$HPSSNAM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
                             hpss2=$HPSSNAM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar
                             date1=${PDY09}
                             date2=${PDY03}
                             ndas1=ndas.t${HH09}z.prepbufr.tm09
                             ndas2=ndas.t${HH03}z.prepbufr.tm03;;
                        esac
                        #check first file
                        if [ -s ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date1}/$ndas1 ] ; then
                            ln -sf ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date1}/$ndas1 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                        elif [ $runhpss = YES ];  then
                            echo "htar -xf ${hpss1} ./$ndas1" > ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.sh
                            echo "mv ${ndas1} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}" >> ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.sh
                            if [ $machine = THEIA ]; then
                                qsub -l procs=1,walltime=0:05:00 -q service -A fv3-cpu -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.out -N get_ndas1_prepbufr.${DATE} -W umask=022 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.sh
                            elif [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
                                bsub -W 0:10 -q dev_transfer -P GFS_T2O -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.out -e ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.out -N get_ndas1_prepbufr.${DATE} -J get_ndas1_prepbufr.${DATE} -M 100 -R affinity[mem=100] ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.1.sh
                            fi
                        fi
                        #check second file if first not found
                        if [ ! -s ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE} ]; then
                            if [ -s ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date2}/$ndas2 ] ; then
                                ln -sf ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date2}/$ndas2 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                            elif [ $runhpss = YES ];  then
                                echo "htar -xf ${hpss2} ./$ndas2" > ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.sh
                                echo "mv ${ndas2} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}" >> ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.sh
                                if [ $machine = THEIA ]; then
                                    qsub -l procs=1,walltime=0:05:00 -q service -A fv3-cpu -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.out -N get_ndas2_prepbufr.${DATE} -W umask=022 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.sh
                                elif [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
                                    bsub -W 0:10 -q dev_transfer -P GFS_T2O -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.out -e ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.out -N get_ndas2_prepbufr.${DATE} -J get_ndas2_prepbufr.${DATE} -M 100 -R affinity[mem=100] ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.2.sh
                                fi
                            fi
                        fi
                        #check third file if first and second not found
                        if [ ! -s ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE} ]; then
                            if [ -s ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date3}/$ndas3 ] ; then
                                ln -sf ${prepbufr_arch_dir}/${prepbufr_conus_sfc}/${prepbufr_conus_sfc}.${date3}/$ndas3 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}
                            elif [ $runhpss = YES ];  then
                                echo "htar -xf ${hpss3} ./$ndas3" > ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.sh
                                echo "mv ${ndas3} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}" >> ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.sh
                                if [ $machine = THEIA ]; then
                                    qsub -l procs=1,walltime=0:05:00 -q service -A fv3-cpu -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.out -N get_ndas3_prepbufr.${DATE} -W umask=022 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.sh
                                elif [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
                                    bsub -W 0:10 -q dev_transfer -P GFS_T2O -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.out -e ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.out -N get_ndas3_prepbufr.${DATE} -J get_ndas3_prepbufr.${DATE} -M 100 -R affinity[mem=100] ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.3.sh
                                fi
                            fi
                        fi
                        #make sure we have file
                        if [ ! -s ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE} ]; then
                            echo "prepbufr.${DATE} doesn't exist or zero-sized." >> ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/error_${prepbufr_conus_sfc}_${DATE}.txt
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
                        elif [ $runhpss = YES ]; then
                            HPSSNAM="/NCEPPROD/hpssprod/runhistory"
                            if [ $PDY -eq 20170320 ] ; then
                                nam_tar=$HPSSNAM/rh${YYYY}/${YYYYMM}/${PDY}/com_nam_prod_nam.${PDY}${CYC}.bufr.tar
                            else
                                nam_tar=$HPSSNAM/rh${YYYY}/${YYYYMM}/${PDY}/com2_nam_prod_nam.${PDY}${CYC}.bufr.tar
                            fi
                            echo "htar -xf ${nam_tar} ./$bufrfile" > ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.sh
                            echo "mv ${bufrfile} ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}" >> ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.sh
                            if [ $machine = THEIA ]; then
                                qsub -l procs=1,walltime=0:05:00 -q service -A fv3-cpu -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.out -N get_nam_prepbufr.${DATE} -W umask=022 ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.sh
                            elif [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
                                bsub -W 0:10 -q dev_transfer -P GFS_T2O -o ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.out -e ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.out -N get_nam_prepbufr.${DATE} -J get_nam_prepbufr.${DATE} -M 100 -R affinity[mem=100] ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE}.sh
                            fi
                        fi
                        if [ ! -s ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/prepbufr.${DATE} ]; then
                            echo "prepbufr.${DATE} doesn't exist or zero-sized." >> ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}/error_${prepbufr_conus_sfc}_${DATE}.txt
                        fi
                    done
                fi
            else
                echo "ERROR: grid-to-obs ${type} is not supported."
            fi
        done
        nn=1
        while [ $nn -le $nexp ] ; do
            export exp=${expname[nn]}             #exp name
            export exp_dir=${expdir[nn]}          #exp directory
            export cdump=${dumpname[nn]}          #file dump format
            mkdir -p ${rundir_g2o1}/logs/${exp} ${rundir_g2o1}/confs/${exp} ${rundir_g2o1}/jobs/${exp} ${rundir_g2o1}/data/${exp}
            #get model files
            for type in $grid2obs_typelist ; do
                if [ $type = "upper_air" ]; then
                    for vhr in $vhrlist_upper_air ; do
                        DATE=${VDATE}${vhr}
                        fhr=${fhrmin}
                        while [ $fhr -le $fhrmax ] ; do
                            if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
                            IDATE=$($ndate -$fhr $DATE)
                            if [ -s $exp_dir/$exp/pgbf${fhr}${cdump}${IDATE} ] ; then
                                ln -sf $exp_dir/$exp/pgbf${fhr}${cdump}${IDATE} ${rundir_g2o1}/data/${exp}/.
                            fi
                            fhr=$((fhr+fhrout_upper_air))
                        done
                    done
                elif [ $type = "conus_sfc" ]; then
                    for vhr in $vhrlist_conus_sfc ; do
                        DATE=${VDATE}${vhr}
                        fhr=${fhrmin}
                        while [ $fhr -le $fhrmax ] ; do
                            if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
                            IDATE=$($ndate -$fhr $DATE)
                            if [ -s $exp_dir/$exp/pgbf${fhr}${cdump}${IDATE} ] ; then
                                ln -sf $exp_dir/$exp/pgbf${fhr}${cdump}${IDATE} ${rundir_g2o1}/data/${exp}/.
                            fi
                             fhr=$((fhr+fhrout_conus_sfc))
                        done
                    done
                else
                    echo "ERROR: grid-to-obs ${type} is not supported."
                fi
            done
            #do checks for needed "truth" files, run verfication types for files that exist
            for type in $grid2obs_typelist ; do
                if [ $type = upper_air ]; then
                    num_prepbufr_upper_air=`ls ${rundir_g2o1}/data/prepbufr/${prepbufr_upper_air}* | wc -l`
                    if [ $num_prepbufr_upper_air -ge 1 ]; then
                        grid2obs_run_typelist+=(upper_air)
                        export pb2nc_upper_air_dir=${rundir_g2o1}/output_metplus_data/upper_air/${exp}/${VDATE}${vhr_upper_air_start}00_${VDATE}$((vhr_upper_air_end-1))00/pb2nc
                        export point_stat_upper_air_dir=${rundir_g2o1}/output_metplus_data/upper_air/${exp}/${VDATE}${vhr_upper_air_start}00_${VDATE}$((vhr_upper_air_end-1))00/point_stat
                        mkdir -p ${pb2nc_upper_air_dir} ${point_stat_upper_air_dir}
                    fi
                elif [ $type = conus_sfc ]; then
                    num_prepbufr_conus_sfc=`ls ${rundir_g2o1}/data/prepbufr/${prepbufr_conus_sfc}* | wc -l`
                    if [ $num_prepbufr_conus_sfc -ge 1 ]; then
                        grid2obs_run_typelist+=(conus_sfc)
                        export pb2nc_conus_sfc_dir=${rundir_g2o1}/output_metplus_data/conus_sfc/${exp}/${VDATE}${vhr_conus_sfc_start}00_${VDATE}$((vhr_conus_sfc_end-1))00/pb2nc
                        export point_stat_conus_sfc_dir=${rundir_g2o1}/output_metplus_data/conus_sfc/${exp}/${VDATE}${vhr_conus_sfc_start}00_${VDATE}$((vhr_conus_sfc_end-1))00/point_stat
                        mkdir -p ${pb2nc_conus_sfc_dir} ${point_stat_conus_sfc_dir}
                    fi
                else
                    echo "ERROR: grid-to-obs ${type} is not supported."
                    echo "grid-to-obs ${type} is not supported."  >> ${rundir_g2o1}/output_metplus_data/${type}/${exp}/error_${VDATE}.txt
                fi
            done
            export typelist="${grid2obs_run_typelist[@]}"
            #run grid-to-obs verification for requested and avaiable types
            if [ ${#grid2obs_run_typelist[@]} -ge 1 ] ; then
                #run in MPMD style if MPMD=YES, else run serially METplus
                if [ $MPMD = YES ] ; then
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
                            fi
                    done
                    #third run stat_analysis
                    for type in "${grid2obs_run_typelist[@]}" ; do
                        export type=${type}
                        if [ $type = upper_air ]; then
                            vhrlist=$vhrlist_upper_air
                            export lookin_dir=$point_stat_upper_air_dir
                            save_name="air"
                        elif [ $type = conus_sfc ]; then
                            vhrlist=$vhrlist_conus_sfc
                            export lookin_dir=$point_stat_lookin_sfc_dir
                            save_name="sfc"
                        fi
                        if [ $gather_by = VSDB_format ]; then
                            n=0 ; for fcyc in $fcyclist ; do n=$((n+1)) ; fcycname[n]=$fcyc  ; done
                            export fcyc_start=${fcycname[1]}
                            export fcyc_end=${fcycname[$gfs_cyc]}
                            export fcyc_int=`expr 24 \/ $gfs_cyc  \* 3600 `
                            ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            for fcycl in $fcyclist ; do
                                export savedir=${metplussave}/${gather_by}/grid2obs/${fcycl}Z/${exp}
                                mkdir -p ${savedir}
                                cp ${rundir_g2o1}/output_${gather_by}/${type}/${fcycl}Z/${exp}/${exp}_${VDATE}.stat ${savedir}/${exp}_${save_name}_${VDATE}.stat
                            done
                        else
                            #this is a temporary workaround until METplus can handle this format type
                            nfiles_point_stat=$(ls $lookin_dir | wc -l)
                            for vhr in $vhrlist ; do
                                mkdir -p ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}
                                if [ $nfiles_point_stat -ge 1 ] ; then
                                    export savedir=${metplussave}/${gather_by}/grid2obs/${type}/${vhr}Z/${exp}
                                    mkdir -p ${savedir}
                                    rand_point_stat_file=$(ls ${lookin_dir} | sort -R | head -n 1)
                                    head -1 ${lookin_dir}/$rand_point_stat_file > ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/${exp}_${VDATE}.stat
                                    grep --no-filename -R "${VDATE}_${vhr}0000" ${lookin_dir} >> ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/${exp}_${VDATE}.stat
                                    cp ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                                else
                                    echo "ERROR: NO point_stat ${type} files in ${lookin_dir}."
                                    echo "NO point_stat ${type} files in ${lookin_dir}"  >  ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/error_${VDATE}.stat
                                fi
                            done
                        fi
                    done
                #run in serially if MPMD=NO
                else
                    for type in "${grid2obs_run_typelist[@]}" ; do
                        export type=${type}
                        if [ $type = upper_air ]; then
                            vhrlist=$vhrlist_upper_air
                            export lookin_dir=$point_stat_upper_air_dir
                            save_name="air"
                        elif [ $type = conus_sfc ]; then
                            vhrlist=$vhrlist_conus_sfc
                            export lookin_dir=$point_stat_conus_sfc_dir
                            save_name="sfc"
                        fi
                        echo "==== running METplus grid-to-obs:${type} for ${exp} on ${VDATE}, ${vhrlist} ====" 
                        #run pb2nc and point_stat
                        ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_${type}_step1a.conf -c ${metplusconfig}/machine_config/machine.${machine}
                        #now run stat_analysis
                        if [ $gather_by = VSDB_format ]; then
                            n=0 ; for fcyc in $fcyclist ; do n=$((n+1)) ; fcycname[n]=$fcyc  ; done
                            export fcyc_start=${fcycname[1]}
                            export fcyc_end=${fcycname[$gfs_cyc]}
                            export fcyc_int=`expr 24 \/ $gfs_cyc  \* 3600 `
                            ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/grid2obs_${type}_step1b.conf -c ${metplusconfig}/machine_config/machine.${machine}
                            for fcycl in $fcyclist ; do
                                export savedir=${metplussave}/${gather_by}/grid2obs/${fcycl}Z/${exp}
                                mkdir -p ${savedir}
                                cp ${rundir_g2o1}/output_${gather_by}/${type}/${fcycl}Z/${exp}/${exp}_${VDATE}.stat ${savedir}/${exp}_${save_name}_${VDATE}.stat
                            done
                        else
                            #this is a temporary workaround until METplus can handle this format type
                            nfiles_point_stat=$(ls $lookin_dir | wc -l)
                            for vhr in $vhrlist ; do
                                mkdir -p ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}
                                if [ $nfiles_point_stat -ge 1 ] ; then
                                    export savedir=${metplussave}/${gather_by}/grid2obs/${type}/${vhr}Z/${exp}
                                    mkdir -p ${savedir}
                                    rand_point_stat_file=$(ls ${lookin_dir} | sort -R | head -n 1)
                                    head -1 ${lookin_dir}/$rand_point_stat_file > ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/${exp}_${VDATE}.stat
                                    grep --no-filename -R "${VDATE}_${vhr}0000" ${lookin_dir} >> ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/${exp}_${VDATE}.stat 
                                    cp ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/*.stat ${savedir}/.
                                else
                                    echo "ERROR: NO point_stat ${type} files in ${lookin_dir}."
                                    echo "NO point_stat ${type} files in ${lookin_dir}"  >  ${rundir_g2o1}/output_${gather_by}/${type}/${vhr}Z/${exp}/error_${VDATE}.stat
                                fi
                            done
                        fi
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
    echo "===== RUNNING VRFY_PRECIP_STEP1 ====="
    echo "===== creating contingency table data for precip verifcation using METplus ====="
    #set some environment variables
    export expnlist=$expname                                                                #experiment names 
    export expdlist=$expdir                                                                 #exp online archive directories
    export dumplist=".gfs."                                                                 #file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
    export rundir_precip1_base=$rundir_base/precip_step1                                    #run directory, where to save METplus output
    export VDATESTART_precip=`$ndate -${VRFYBACK_HRS_PRECIP} ${VSTART_DATE}00 |cut -c 1-8 ` #verification starting date
    export VDATEEND_precip=`$ndate -${VRFYBACK_HRS_PRECIP} ${VEND_DATE}00 |cut -c 1-8 `     #verification ending date
    export precip_accumlist=${precip_accumlist}                                             #accumulation lengths (in hrs) to verify
    export ftyplist=${ftyplist}                                                             #file type
    export ptyplist=${ptyplist}                                                             #precip variable
    export fbucketlist=${fbucketlist}                                                       #file accumulations
    export fcyclist=$fcyclist                                                               #all fcst cycles to be included in verification
    export fhrmin=$fhrmin                                                                   #start forecast hour
    if [ $vhr_rain -ge 180 ]; then                                                          #end forecast hour
        export fhrmax=180
    else
        export fhrmax=$vhr_rain
    fi
    export gather_by=${gather_by}                                                           #how to gather METplus output: VSDB_format (future support: valid_format)
    #create directories for output
    mkdir -p $rundir_precip1_base $metplussave
    #run METplus
    n=0 ; for runn in $expnlist ; do n=$((n+1)) ; expname[n]=$runn ; done
    n=0 ; for rund in $expdlist ; do n=$((n+1)) ; expdir[n]=$rund  ; done
    n=0 ; for dump in $dumplist ; do n=$((n+1)) ; dumpname[n]=$dump  ; done
    n=0 ; for ftyp in $ftyplist ; do n=$((n+1)) ; ftypname[n]=$ftyp  ; done
    n=0 ; for ptyp in $ptyplist ; do n=$((n+1)) ; ptypname[n]=$ptyp  ; done
    n=0 ; for facc in $fbucketlist ; do n=$((n+1)) ; faccum_bucket[n]=$facc  ; done
    nexp=`echo $expnlist |wc -w`
    VDATE=${VDATESTART_precip}
    while [ $VDATE -le ${VDATEEND_precip} ] ; do
        export VDATE=$VDATE
        export YYYY=$(echo ${VDATE}  |cut -c 1-4 )
        export YYYYMM=$(echo ${VDATE}  |cut -c 1-6 )
        export rundir_precip1=${rundir_precip1_base}/valid_${VDATE}
        if [ -d ${rundir_precip1} ] ; then
            echo "REMOVING ${rundir_precip1}"
            rm -r $rundir_precip1
        fi
        mkdir -p ${rundir_precip1}/output_metplus_data ${rundir_precip1}/output_${gather_by} ${rundir_precip1}/logs ${rundir_precip1}/confs ${rundir_precip1}/jobs ${rundir_precip1}/data
        #get obs data
        #NOTE, ONLY OBS WITH VALID TIMES AT 12Z ARE CURRENTLY SUPPORTED
        for accum in ${precip_accumlist} ; do
            export type="accum_${accum}hr"
            if [ $type = "accum_24hr" ] ; then 
                #accum_24hr observation file: CCPA 24 hour accumulations, valid: VDATE-24hr 12Z - VDATE 12Z
                mkdir -p ${rundir_precip1}/data/CCPA/${type}
                export obs_name="CCPA"
                export obs_data_type="GRIB"
                export obs_daily_file="True"
                export obs_file_now=ccpa.${VDATE}12.24h
                if [ -s ${ccpa_prod_dir}/precip.${VDATE}/ccpa.${VDATE}12.24h ]; then
                    ln -sf ${ccpa_prod_dir}/precip.${VDATE}/ccpa.${VDATE}12.24h ${rundir_precip1}/data/CCPA/${type}/.
                else
                    ccpa_tar=/NCEPPROD/hpssprod/runhistory/rh${YYYY}/${YYYYMM}/${VDATE}/com_verf_prod_precip.${VDATE}.precip.tar
                    echo "htar -xf ${ccpa_tar} ./ccpa.${VDATE}12.24h" >> ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.sh
                    echo "mv ccpa.${VDATE}12.24h ${rundir_precip1}/data/CCPA/${type}/." >> ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.sh
                    if [ $machine = THEIA ]; then
                        qsub -l procs=1,walltime=0:10:00 -q service -A fv3-cpu -o ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.out -e ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.out -N get_ccpa.${VDATE}12.24h -W umask=022 ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.sh
                    elif [ $machine = WCOSS_C -o $machine = WCOSS_DELL_P3 ]; then
                        bsub -W 0:10 -q dev_transfer -P GFS_T2O -o ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.out -e ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.out -N get_ccpa.${VDATE}12.24h -J get_ccpa.${VDATE}12.24h -M 100 -R affinity[mem=100] ${rundir_precip1}/data/CCPA/${type}/get_ccpa.${VDATE}12.24h.sh
                    fi
                    #wait for transfer
                    nsleeps=1
                    while  [ ! -f  "${rundir_precip1}/data/CCPA/${type}/ccpa.${VDATE}12.24h" ] ; do
                       echo "Waiting for HPSS transfer for ccpa.${VDATE}12.24h...time elapsed: $((15*$nsleeps)) seconds"
                       sleep 15
                       nsleeps=$((nsleeps+1))
                    done
                fi
            else
                echo "ERROR: precip ${type} is not supported."
            fi
        done
        nn=1
        while [ $nn -le $nexp ] ; do
            export exp=${expname[nn]}                #exp name
            export exp_dir=${expdir[nn]}             #exp directory
            export cdump=${dumpname[nn]}             #file dump format
            export file_type=${ftypname[nn]}         #file type
            export precip_type=${ptypname[nn]}       #precip var
            export file_accum=${faccum_bucket[nn]}   #bucket accumulation
            export fcst_file_template="${file_type}{lead?fmt=%HH}${cdump}{init?fmt=%Y%m%d%H}"
            mkdir -p ${rundir_precip1}/logs/${exp} ${rundir_precip1}/confs/${exp} ${rundir_precip1}/jobs/${exp} ${rundir_precip1}/data/${exp}
            #get model files
            export fcst_init_intv=`expr 24 \/ $gfs_cyc `
            for accum in ${precip_accumlist} ; do
                export accum=$accum
                export type="accum_${accum}hr"
                if [ $file_accum -eq 0 ] ; then
                    export nfiles_accum=2
                    export file_accum_int=$accum
                    export pcp_combine_method="SUBTRACT"
                else
                    export nfiles_accum=`expr $accum \/ $file_accum `
                    export file_accum_int=$file_accum
                    export pcp_combine_method="SUM"
                fi
                #get model files
                fhr_count=0
                end_init=$(echo $($ndate -${fhrmax} ${VDATE}12 ))
                for fcyc in $fcyclist ; do
                    init_now=${VDATE}${fcyc}
                    while [ $init_now -ge $end_init ] ; do
                        accum_fhr_end=$(echo $($nhour ${VDATE}12 $init_now))
                        accum_fhr_start=$(($accum_fhr_end - $accum))
                        if [[ $accum_fhr_start -ge 0 && $accum_fhr_end -ge 0 ]]; then
                            nf=1
                            while [ $nf -le $nfiles_accum ] ; do
                                nf_fhr=$(($accum_fhr_end - $(($((nf-1))*$file_accum_int))))
                                if [ $nf_fhr -lt 10 ]; then nf_fhr=0$nf_fhr ; fi
                                if [ -s $exp_dir/$exp/${file_type}${nf_fhr}${cdump}${init_now} ]; then
                                    ln -sf $exp_dir/$exp/${file_type}${nf_fhr}${cdump}${init_now} ${rundir_precip1}/data/${exp}/.
                                    if [ $nf -eq 1 ]; then
                                        fhr_list[$fhr_count]=$nf_fhr
                                        fhr_count=` expr $fhr_count + 1 `
                                    fi
                                fi
                                nf=$((nf+1))
                            done
                        fi
                        init_now=$(echo $($ndate -24 ${init_now}))
                    done
                done
                export fhr_list_config=$( printf "%s," "${fhr_list[@]}" | cut -d "," -f 1-${#fhr_list[@]} )
                #run METplus serially, this runs pretty fast
                if [ -s ${rundir_precip1}/data/${obs_name}/${type}/${obs_file_now} ]; then
                    echo "==== running METplus precip:${type} for ${exp} on ${VDATE}12 ===="
                    #run pcp_combine, grid_stat
                    ${metplushome}/ush/master_metplus.py -c ${metplusconfig}/metplus_config/METplus-${METPLUSver}/precip_step1_${type}.conf -c ${metplusconfig}/machine_config/machine.${machine}
                    #now run stat_analysis
                    #this is a temporary workaround until METplus can handle this format type
                    lookin_dir=${rundir_precip1}/output_metplus_data/${type}/${exp}/${VDATE}1200/grid_stat
                    nfiles_point_stat=$(ls $lookin_dir | wc -l)
                    if [ $nfiles_point_stat -ge 1 ] ; then
                        rand_point_stat_file=$(ls ${lookin_dir} | sort -R | head -n 1)
                        if [ $gather_by = VSDB_format ]; then
                            for fcyc in $fcyclist ; do
                                mkdir -p ${rundir_precip1}/output_${gather_by}/${type}/${fcyc}Z/${exp}
                                head -1 ${lookin_dir}/$rand_point_stat_file > ${rundir_precip1}/output_${gather_by}/${type}/${fcyc}Z/${exp}/${exp}_${VDATE}.stat
                            done
                            for fhr_search in $fhr_list_config ; do
                                fhr_from_fcyc=$(echo $($ndate -$fhr_search ${VDATE}12) |cut -c 9-10)
                                 awk '{ if ($4 == '$fhr_search'0000) { print } }' ${lookin_dir}/*  >> ${rundir_precip1}/output_${gather_by}/${type}/${fhr_from_fcyc}Z/${exp}/${exp}_${VDATE}.stat
                            done
                            for fcyc in $fcyclist ; do
                                export savedir=${metplussave}/${gather_by}/precip/${type}/${fcyc}Z/${exp}
                                mkdir -p ${savedir}
                                cp ${rundir_precip1}/output_${gather_by}/${type}/${fcyc}Z/${exp}/${exp}_${VDATE}.stat ${savedir}/.
                            done
                        else
                            export savedir=${metplussave}/${gather_by}/precip/${type}/12Z/${exp}
                            mkdir -p ${savedir}
                            mkdir -p ${rundir_precip1}/output_${gather_by}/${type}/12Z/${exp}
                            head -1 ${lookin_dir}/$rand_point_stat_file > ${rundir_precip1}/output_${gather_by}/${type}/12Z/${exp}/${exp}_${VDATE}.stat
                            grep --no-filename -R "${VDATE}_120000" ${lookin_dir} >> ${rundir_precip1}/output_${gather_by}/${type}/12Z/${exp}/${exp}_${VDATE}.stat
                            cp ${rundir_precip1}/output_${gather_by}/${type}/12Z/${exp}/*.stat ${savedir}/.
                        fi
                    else
                        echo "ERROR: NO point_stat ${type} files in ${lookin_dir}."
                        echo "NO point_stat ${type} files in ${lookin_dir}"  >  ${rundir_precip1}/output_${gather_by}/${type}/12Z/${exp}/error_${VDATE}.stat
                    fi
                else
                    echo "ERROR: ${rundir_precip1}/data/${obs_name}/${type}/${obs_file_now} doesn't exist or zero-sized. SKIPPING precip. verification for this date."
                    mkdir -p ${rundir_precip1}/output_metplus_data/precip/${type}/${exp}/${VDATE}1200
                    echo "${rundir_precip1}/data/${obs_name}/${type}/${obs_file_now} doesn't exist or zero-sized. No precip. verification for this date." >> ${rundir_precip1}/output_metplus_data/precip/${type}/${exp}/${VDATE}1200/error_${VDATE}.txt  
                fi
            done 
            nn=`expr $nn + 1 `
        done
        VDATE=$(echo $($ndate +24 ${VDATE}00 ) |cut -c 1-8 )
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

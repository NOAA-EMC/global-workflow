Step 5. Build, Run and Document  {#HowToOldToNewBuildRun}
==========================

 
Step 5.1:
---------

Try to build your component. To do this, make sure to run the proper
command for your shell.  To find out which shell you are using:

    echo $0

In tcsh:

    ./NEMS/NEMSAppBuilder app=coupledGSM_WW3 >& coupledbuild.log &

In bash or ksh:

    ./NEMS/NEMSAppBuilder app=coupledGSM_WW3 > coupledbuild.log 2>&1 &

Alternatively, you could use the GUI mode:

    ./NEMS/NEMSAppBuilder

Make sure you select your new app (coupledGSM_WW3 in our case) You can
find more detailed information on the AppBuilder here
 
You will be able to tell your build was successful by looking to see
if the NEMS.x executable exists in the NEMS/exe folder.  The
coupledbuild.log will give you information about the build to look for
errors if the executable is not built successfully.

\todo link to the help for build not running 
 
Step 5.2:
---------

Now try to run your compset.  From your top level directory: 

    ./NEMS/NEMSCompsetRun -f
 
For more information on the NEMSCompsetRun see \ref building

Step 5.3: 
-------- 

Hopefully, the system is now running.  For this example you can see where
the app is at the following tag: 
* https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/tags/NEMSTutorial/StepLast

If you would like you can try this app: 

    $svn co https://svnemc.ncep.noaa.gov/projects/nems/apps/UGCS-Weather/tags/NEMSTutorial/StepLast UpAppEx
    $cd UpAppEx 
    $./NEMS/NEMSCompsetRun -f 


All options for the NEMSCompsetRun can be found at \ref building

The above command should then gives the following output: 
    

    $ ./NEMS/NEMSCompsetRun -f
    06/19 12:33:17Z NEMSCompsetRun-INFO:  Test suite subset = *
    06/19 12:33:17Z NEMSCompsetRun-INFO:  Starting: exe('account_params')
    06/19 12:33:17Z NEMSCompsetRun-INFO:    - and will capture output.
    Processing Unix group file /etc/group
    Processing Allocation file /home/admin/userdb/theia_allocations.csv
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Auto-chosen project for job submission is 'fv3-cpu'
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Auto-chosen ptmp is '/scratch4/NCEPDEV/stmp4/Jessica.Meixner'
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Parsing compset descriptions.
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Verifying repo fingerprint against data fingerprint.
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Baseline fingerprint matches repo fingerprint. Rejoice.
    06/19 12:33:25Z NEMSCompsetRun-INFO:    Baseline fingerprint file: /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/REGTEST-FINGERPRINT.md
    06/19 12:33:25Z NEMSCompsetRun-INFO:    Repository fingerprint file: /scratch4/NCEPDEV/nems/noscrub/Jessica.Meixner/StepLast/parm/REGTEST-FINGERPRINT.md
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Generating workflow with id 60503.
    06/19 12:33:25Z NEMSCompsetRun-INFO:  Requested test has been generated.
    06/19 12:33:25Z rtrun INFO: check dependencies and submit jobs...
    06/19 12:33:26Z rtrun INFO: check status...
    06/19 12:33:26Z rtrun INFO: workflow is still running and no jobs have failed.
    06/19 12:33:26Z rtrun INFO: sleep 2
    06/19 12:33:28Z rtrun INFO: get queue information
     Job ID   Reserv.     Queue    Procs ST Queue Time  Stdout Location
    -------- -------- ------------ ----- -- ----------- ------------------------------------
    25340171          batch            1 Q  06/19 12:33 /scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/log/build_gsm_ww3.x.log
    From qstat -x  job list (age 0 sec.)
    06/19 12:33:28Z rtrun INFO: sleep 100
    From qstat -x  job list (age 0 sec.)
    06/19 12:33:28Z rtrun INFO: sleep 100
    06/19 12:35:08Z rtrun INFO: check dependencies and submit jobs...
    06/19 12:35:09Z rtrun INFO: check status...
    06/19 12:35:09Z rtrun INFO: workflow is still running and no jobs have failed.
    06/19 12:35:09Z rtrun INFO: sleep 2
    06/19 12:35:11Z rtrun INFO: get queue information
     Job ID   Reserv.     Queue    Procs ST Queue Time  Stdout Location
    -------- -------- ------------ ----- -- ----------- ------------------------------------
    25340171          batch            1 R  06/19 12:33 /scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/log/build_gsm_ww3.x.log
    From qstat -x  job list (age 0 sec.)
    06/19 12:35:11Z rtrun INFO: sleep 100
        ...
    06/19 12:59:10Z rtrun INFO: check dependencies and submit jobs...
    06/19 12:59:10Z rtrun INFO: check status...
    06/19 12:59:10Z rtrun INFO: workflow is still running and no jobs have failed.
    06/19 12:59:11Z rtrun INFO: sleep 2
    06/19 12:59:13Z rtrun INFO: get queue information
     Job ID   Reserv.     Queue    Procs ST Queue Time  Stdout Location
    -------- -------- ------------ ----- -- ----------- ------------------------------------
    25340210          debug           60 R  06/19 12:48 /scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/log/test_cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188.log
    From qstat -x  job list (age 0 sec.)
    06/19 12:59:13Z rtrun INFO: sleep 100
    06/19 13:00:53Z rtrun INFO: check dependencies and submit jobs...
    06/19 13:00:53Z rtrun INFO: check status...
    06/19 13:00:53Z rtrun INFO: workflow is still running and no jobs have failed.
    06/19 13:00:54Z rtrun INFO: sleep 2
    06/19 13:00:56Z rtrun INFO: get queue information
     Job ID   Reserv.     Queue    Procs ST Queue Time  Stdout Location
    -------- -------- ------------ ----- -- ----------- ------------------------------------
    25340210          debug           60 R  06/19 12:48 /scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/log/test_cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188.log
    From qstat -x  job list (age 0 sec.)
    06/19 13:00:56Z rtrun INFO: sleep 100
    06/19 13:02:36Z rtrun INFO: check dependencies and submit jobs...
    06/19 13:02:36Z rtrun INFO: check status...
    06/19 13:02:36Z rtrun INFO: workflow is complete and all jobs succeeded.
    06/19 13:02:37Z NEMSCompsetRun-INFO:  generate report
    06/19 13:02:37Z NEMSCompsetRun-INFO:  copy build logs to /scratch4/NCEPDEV/nems/noscrub/Jessica.Meixner/StepLast/NEMS/tests/../../log/report-theia-log
    Report says test succeeded.
    TEST RESULT: PASS


Note from this we can see which queue (project), where the output directory will be: 
the ptmp directory, the workflow id and more. This also gives you links to the log files of the 
different parts of the execution.  

In this case, the log file for the build is
`/scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/log/build_gsm_ww3.x.log`
and the logfile for the compset is
`/scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/log/test_cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188.log`.
The output of each compset is located in the `tmp/<compsetname>`
directory, so for this case it would be:
`/scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/tmp/cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188`.

For general information about the generated work directories and workflow see: 
\todo link to description of regtest output folder/workflow directores. 

If your compset fails, you can rerun the compset by going to the workflow directory, in this case: 
`/scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/` and rewinding the job, with the following command: 

    ./rtrewind <compset name> 

For more information on rtrewind, see: 
\todo link do doc on rtrewind 

Then, go back to your source code directory, and resume the job.  This
requires knowing the path to your workflow directory. Again in this
case the workflow directory is
`/scratch4/NCEPDEV/stmp4/Jessica.Meixner/rtgen.60503/`. The command is:

    ./NEMS/NEMSCompsetRun --resume <path to workflow directory> 

If your compset fails, some things to look for:

- Is it just because your baseline data does not exist or is not in the right location?
- Are all variables set in the compset and in the run_exglobal_fcst section?
- Are you using the new versions of your branches?
- Double check that all old changes were migrated to new branches.

\todo other items to add here? 

\todo how to run a subset of the all.input based on the keywords 

\todo can you run the workflow without a terminal open?  

\todo is there an option to only run one compset? 
 
Step 5.4:
-------

Add documentation for your application; this should include a basic 
description of your application and any other application specific
infromation you think should be included. Note documentation of 
individual compsets occurs within the compsets themselves.  Put this
documentation in the `app/doc/README.md`. Additional files can be 
added, but the `README.md` is the main page.  For this application the 
documentation is simply the following: 


    UGCS-Weather Application
    ========================
    This site documents the UGCS Weather application, which
    is for coupling ATM<->WAV models.
    The available components for ATM is GSM and for WAV
    is WAVEWATCH III.
    The goal is to determine the impact of including
    sea-state dependent feedback from the WAV model on
    both the ATM and WAV models.




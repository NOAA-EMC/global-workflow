Step 4. Transition to new Compsets  {#HowToOldToNewCompsets}
=============================
 
 
Step 4.1:  
---------

Now it is time to start transitioning to the new
compsets. Additional documentation on compsets can be found here.  To
create a new compset, start by copying the compset/*.input that is the
most similar to what you will be running, which is compset/gsm.input
in this example:
 
    cd compset
    svn cp gsm.input gsm_ww3.input

\todo link to compsets above step 4.1
 
Step 4.2:
---------

In your new compset file (compset/gsm_ww3.input), you will want to
delete all unneeded tests/compsets keeping only one that you will update for your
case.  If using gsm.input, delete everything except the gfs_slg or
gfs_eulerian test and change "`test gfs_<test>`" to "`compset
your_compset_name`".  Note, any `%` signs in the compset name should be
replaced with `@` signs.
 
In this example, we are converting the old compset
[cfsr%20150401_1day_leapfrog_gsm%slg%T126_ww3%t188](https://svnemc.ncep.noaa.gov/projects/nems/branches/UGCS-Seasonal/twoWayWW3_from_r80562/compsets/cfsr%2520150401_1day_leapfrog_gsm%25slg%25T126_ww3%25t188)
 
So we will change `test gfs_slg`, to:

    compset cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188 
 
Step 4.3: 
---------

The next step is to delete any builds you do not need and create any
builds that you do need. In this example, we do the following:
 
* We are starting with the gsm.x build, as we do not need gocart.

* Next we rename gsm.x to gsm_ww3.x This has to be changed in the
  `build gsm.x` command as well as in the variables NEMS.x and
  modules.nems

* Lastly, we need to update the name of the app= in the build
  variable.  This is the app name that appears in the appBuilder file
  (`<appname>.appBuilder`) that you created in the top level of your
  directory.  For this case, this is coupledGSM_WW3.

* Delete any other builds you do not need, which in this case is  build gsm_gocart.x. 
 
Step 4.4: 
---------

Connect the new build to the compset.  This should be in the top line
where we replaced gsm.x with gsm_ww3.x and in the build=gsm.x with
build=gsm_ww3.x line. This is in two places.  The first is the
dependency list:
 
    compset cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188: gsm_ww3.x {
 
The second is the build variable:
 
    build=gsm_ww3.x
 
Step 4.5: 
---------

Update the compset description in the variable 

    TEST_DESCR=

which can simply be copying the definition from the  old compset.
 
Step 4.6: 
---------

Update the CNTL_NAME variable, which should be the name of the
subdirectory that contains the baseline data. 
CNTL_DIR directory name).i

Update the CNTL_NAME variable, which should be the name of the 
subdirectory that contains the baseline data.  (Potentially 
the CNTL_DIR directory name in the old compset). 
 
    CNTL_NAME='cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188'
 
Step 4.7: 
---------

Set up the initial condition and baseline directory path variables.
To do this, we first look at the old compset where we have

    export IC_DIR=$DATADIR/GSM/T126_CFSR_mask%0.5
 
In the new compset, we now set: 

    GSM_IC_NAME="GSM/T126_CFSR_mask%0.5"
    GSM_IC_DIR="@[plat%BASELINE]/@[GSM_IC_NAME]"
    CNTL="@[plat%BASELINE]/RT-Baselines/@[CNTL_NAME]"
 
Note that the "`@[plat%BASELINE]`" replaces the `$DATADIR` in the old
compset and gives you the platform specific folders. Note `BASELINE` for
each platform is defined in the params.input file in the compsets directory. 
 
Step 4.8: 
---------

Now we need to update all.input replacing `gsm.input` to the name of
the file you created (in this case gsm_ww3.input), ie:

    load 'gsm.input'

becomes

    load 'gsm_ww3.input'
 
Now we update the following line: 

    run gfs_slg                @ gfs, standard, baseline, slg

to correspond to the compset we just created: 

    run cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188  @ gfs, standard, baseline, slg
 
After the @ sign in the above line are keywords that allow you to
define sets of compsets.  For any given set of compsets that have
bitwise identical output (and hence the same CNTL variable), one and
only one of the compsets should have the keyword "baseline".  In
general, this means that all compsets should have "baseline".  The
only exceptions are when several compsets intentionally have identical
output, such as for testing bitwise identicality of thread or
decomposition changes.
 
For this case, we will use "gfs, ww3, standard, baseline, slg" for keywords: 

    run cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188  @ gfs, ww3, standard, baseline, slg
 
All other run lines that correspond to deleted compsets in gsm_ww3.input are
also removed here.
 
Step 4.9: 
---------

Next, make the input directory.  You will need to start with the
NEMSGSM data directory for your platform because it contains the
latest FIXGLOBAL and other files.  If your platform doesn't have that
data, you need to copy it from a platform that does.  You can find the
directories in platforms.input, jet.input, and wcoss.input in the
compsets/ directory.  For example, for Theia:

    BASELINE="/scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/NEMSGSM/trunk-20170213/data"

\cond 
and for wcoss: 

    BASELINE="/nems/noscrub/emc.nemspara/RT/NEMSGSM/trunk-20170213/data"
\endcond 
 
Make a copy of that directory, from the trunk-(date) level, in your application-specific 
area of the RT directory.  The directory structure looks like this:
 
    RT/(AppName)/(Branch-or-trunk)-(revision-or-date)
 
The RT directory exists at these locations:
 
    Theia: /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT
    WCOSS 1 & 2: /nems/noscrub/emc.nemspara/RT
    WCOSS Cray: /gpfs/hps/emc/nems/noscrub/emc.nemspara/RT
    Jet: /lfs3/projects/hfv3gfs/emc.nemspara/RT/

\todo confirm that Jet's RT is /lfs3/projects/hfv3gfs/emc.nemspara/RT/ 

In our case, we place our data here:

    /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531

\cond
    wcoss: /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531
\endcond 

If you do not have emc.nemspara access, then place the data in your
personal area and have an application code manager copy it for you.

Now, copy the data: 

\cond
Theia:
\endcond 

    mkdir -p /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531
    cd /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531
    rsync -arv /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/NEMSGSM/trunk-20170213/data/. .
    mkdir RT-Baselines

\cond 
Wcoss: 

    mkdir -p /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531
    cd /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531
    rsync -arv /nems/noscrub/emc.nemspara/RT/NEMSGSM/trunk-20170213/. .
    mkdir RT-Baselines
\endcond 

Next, update the BASELINE and BASELINE_TEMPLATE in compsets/platforms.input: 

    BASELINE="/scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531"
    BASELINE_TEMPLATE="@[BASELINE]"

Note, compsets/platforms.input gets information for wcoss from compsets/wcoss.input. 
 
Step 4.10:
---------

Within the baseline directory that you just copied, there is a file
called REGTEST-FINGERPRINT.md which must be updated.  This file
documents the compset input and baseline data stored outside the
repository.  It also serves as a verification method to ensure the
data directory in use matches the one desired by this version of the
application. Therefore, anytime there is an update to the compset
inputs or baseline data, this file needs to be updated in the data
storage area and in the repository.
 
So the two files that much match are: 

    /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/REGTEST-FINGERPRINT.md
    /<path to svn app directory>/parm/REGTEST-FINGERPRINT.md
 
We updated the file in

    /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/REGTEST-FINGERPRINT.md

and copied those changes to our local copy of /parm/REGTEST-FINGERPRINT.md.
 
Step 4.11:  
---------

Now we need to copy ALL of the needed initial conditions and other
needed inputs from GSM for the compset in the input directory, that we
specified in the gsm_ww3.input.  In this case, we have:

    GSM_IC_NAME="GSM/T126_CFSR_mask%0.5"
    GSM_IC_DIR="@[plat%BASELINE]/@[GSM_IC_NAME]"

 
So for theia, this would correspond to: 

    /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/GSM/T126_CFSR_mask%0.5
\cond
Or wcoss

    /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/GSM/T126_CFSR_mask%0.5
\endcond
 
So first, we make this directory: 

\cond
theia:
\endcond

    mkdir -p /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/GSM/T126_CFSR_mask%0.5
    cd /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/GSM/T126_CFSR_mask%0.5
 
\cond
wcoss:

    mkdir -p /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/GSM/T126_CFSR_mask%0.5
    cd /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/GSM/T126_CFSR_mask%0.5
\endcond 

To find where your initial condition data was originally stored, look in your original compset 

    export IC_DIR=$DATADIR/GSM/T126_CFSR_mask%0.5

We find the value of DATADIR in NEMS/NEMSCompsetRun by looking in the correct platforms definition so for theia: 

    export DATADIR=/scratch4/NCEPDEV/nems/noscrub/NEMS-Data

\cond
and for wcoss: 

    export DATADIR=/climate/noscrub/emc.climpara/NEMS-Data
\endcond 
 
Then copy the data: 

\cond
Theia:
\endcond

    rsync -arv /scratch4/NCEPDEV/nems/noscrub/NEMS-Data/GSM/T126_CFSR_mask%0.5/. .
 
\cond
wcoss:

    rsync -arv /climate/noscrub/emc.climpara/NEMS-Data/GSM/T126_CFSR_mask%0.5/. .
\endcond  

Step 4.12: 
---------

The next step is to get initial conditions and inputs for each of your
other components.  You can find where these files are using your old
app, compset and NEMSCompsetRun.  First, look in your old compset for
component specific set up calls, ie:
 
    # - component specific setup calls ---
    setup_ww3Case2
 
And then find the corresponding code in the old NEMS/NEMSCompsetRun: 

    setup_ww3Case2(){
      if [ $MACHINE_ID = theia ] ; then
        cp -r /scratch4/NCEPDEV/nems/noscrub/Jessica.Meixner/esmf_files/Case2_20160831/* ${RUNDIR}/.
      elif [ $MACHINE_ID = wcoss ] ; then
        cp -r /marine/noscrub/Jessica.Meixner/esmf_files/Case2_20170323/* ${RUNDIR}/.
      fi
    }
 
Then, copy this information to a folder in your RT directory
`RT/<AppName>/<branch-or-trunk>-<date>/<component>/<description>` ie:

    cp -r /scratch4/NCEPDEV/nems/noscrub/Jessica.Meixner/esmf_files/Case2_20160831/* /scratch4/NCEPDEV/nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/WW3/Case2 
 
\cond
wcoss:

    cp -r  /marine/noscrub/Jessica.Meixner/esmf_files/grid/Case2_20170531/* /nems/noscrub/emc.nemspara/RT/UGCS-Weather/UpdateStructure-20170531/WW3/Case2 
\endcond  
 
Step 4.13: 
---------

Now within your new compset, add lines to copy the newly created directory of component specific 
setup files into the filters input function. This is the line below "# WW3 Specific":

    filters input {
      # Parse any files that need variables from other scopes, and do
      # not need fancy scripting.  Presently, this is just the NEMS
      # and mediator init files.
      #           WORK FILE <=method==  SOURCE
      'atmos.configure'     <=atparse=  "@[CONF]/atmos.configure_gfs"
      'nems.configure'      <=atparse=  "@[CONF]/nems.configure.@[nems_configure].IN"
      # WW3 Specific
                        '*' <=copydir= "@[plat%BASELINE]/WW3/Case2"
    }
 
Step 4.14:
---------

Now it is time to go through your original compset and update/add appropriate values to
variables in your new compset. Start with the following variables: 

    NTHREADS=1
    TASKS=60
    wallclock=1800 #in seconds 
    walltime=1800 #wall clock limit in seconds

NOTE: wallclock time is now in seconds and NOT minutes. 
 

Step 4.15: 
---------

Update variables in the prerun=run_exglobal_fcst section.  Variable 
names in the new scripts match the ones in the
exglobal_fcst.sh.  In the old system, there were one or more aliases
for the same variable and some variables set in the compset did not
actually do anything.  That has been corrected now, but you do need to
find the correct variable names.  The list of supported variables is
in exglobal_fcst.input. If a variable is not in exglobal_fcst.input,
check the master list of GFS variables, maintained by Kate Howard, to 
see the meaning of your variable and any possible synonyms. The variables
in your old compset to look at are likely in the gsm configure section. 
Note that you should put quotes around most variable values. 

In this section you should also define NHRS, the number of hours of 
simulation, instead of NDAYS as before. This is because NDAYS is
subsequently internally defined as NDAYS=NHRS/24. 

To add a new variable needs to be added, that is not already supported in 
exglobal_fcst.input, include it in the prerun section as well as defining
it within exglobal_fcst.input. 

 
Step 4.16:
---------

Next, we need to make the `atmos.configure` and `nems.configure` files and
set the appropriate coupling variables.  In this case, the
atmos.configure file that is used (`@[CONF]/atmos.configure_gfs`) is
`parm/atmos.configure_gfs`:

    core: gfs

    atm_model:                      @[atm_model]
    atm_coupling_interval_sec:      @[coupling_interval_fast_sec]

The `@[variable_names]` get substituted with values defined in your
`*.input` file compset block.  Some of these variables might have
default values such as atm_model.


For the nems.configure file, make sure to add all the needed 
`@[variable_names] into your new compset as well.  Most likely, this 
will mean copying the definition of the variables under ‘nems.configure’ 
section of your old compset, such as: 

    # - nems.configure ---
    export_nems
    export nems_configure=blocked_atm_wav
    export atm_model=gsm
    export atm_petlist_bounds="0 47"    
    export wav_model=ww3
    export wav_petlist_bounds="47 59"  
    export coupling_interval_sec=1800.0  # 30min coupling
     
First note, the nems_configure variable, determines which template file to use, 
ie. `@[CONF]/nems.configure.@[nems_configure].IN`, which  corresponds to 
parm/nems.configure.blocked_atm_wav.IN in this case.
 
The template file in the old structure was located in `NEMS/test/`
directory. If there were any updates to these files, be sure to
update them, these are now located at the app level in the `parm/`
directory.
 
The `@[variable_names]` get substituted with values defined in your
`*.input` file compset block. Some of these variables might have
default values such as atm_model. Make sure to define any of the
non-default `@[variable_names]` for the template file are defined
in the new compset.  For example the above `nems.configure` block 
in the new compset becomes:


Now add the needed variables in the new compset making sure to put quotes 
around variable names. For this example, we add: 
 
    # - nems.configure ---
    nems_configure='blocked_atm_wav'
    atm_model='gsm'
    atm_petlist_bounds="0 47"
    wav_model='ww3'
    wav_petlist_bounds="47 59"
    coupling_interval_sec=1800.0  # 30min coupling

Step 4.17
---------

Add a comment block at the top of the compset section to describe your compset.
This will be automatically parsed and put with application level documentation. 
For this compset we added the following: 

    compset cfsr@20150401_1day_blocked_gsm@slg@T126_ww3@t188: gsm_ww3.x {
        ##! GSM-WW3 coupling with 30 minute intervals
        #
        # This compset couples:
        #
        # * Semilagrangian GSM using the T126 grid with
        # * Wavewatch3 using the t188 grid
        #
        # using a blocked coupling scheme with a  
        # 30 minute timestep.

\todo   are there any other specifics for compset documentation? 

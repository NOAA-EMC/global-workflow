Unsorted Content {#nemsguide}
================

\warning This page will soon be removed.  It is slowly being corrected
and dismantled.  Its contents will reside in other subpages of the
"Documentation" family of pages.

6.3    NEMS Compsets and CompsetRun
--------------------------------------

NEMS uses `"component sets"`, or `"compsets,"` to systematically label run configurations. The labels are associated with scripts that pull together all the files and inputs needed to run the specified configurations. Compset labels include which components and mediators are part of the configuration, whether each component is running in a prognostic or test mode, resolutions, and initial conditions. This approach offers a number of benefits:
* standard runs can be set up easily and consistently
* it is a very effective way to implement regression testing across a coupled system with many possible combinations of components
* easy access to configurations with non-prognostic versions of components facilitates controlled experimentation

Compsets were originated by the [Community Earth System Model (CESM)](http://www.cesm.ucar.edu/models/cesm1.2/cesm/doc/modelnl/compsets.html).

[Supported NEMS compsets are listed here](https://docs.google.com/spreadsheets/d/1v9tJb03YuCbwDsXff4M5i6jz4lvBxUrhdImqaGnK_IE/edit#gid=0).

### 6.3.1 Running NEMS with Compsets

NEMS compsets are available for revision 52376 and later versions of NEMS. The front-end to using the compset feature is provided by the NEMSCompsetRun script located under the NEMS root directory.

From within a modeling application, e.g., UGCS-Seasonal, the `NEMSCompsetRun` is typically invoked from the root directory:

    ./NEMS/NEMSCompsetRun

The script will look for file(s) `*.compsetRun` in the root directory and execute the compsets listed therein.

The script can also be invoked from within the NEMS directory itself:

    ./NEMSCompsetRun [COMPSET_LIST_FILE]

or

    ./NEMSCompsetRun -compset COMPSET_FILE

Running this script without the optional `COMPSET_LIST_FILE` argument will execute the compsets that are listed in file `./compsets/default.compsetRun` under the NEMS directory. An alternative list of compsets can be specified by using the `COMPSET_LIST_FILE` argument. The format of this file is very simple:

-# A line starting with # is a comment.
-# A line starting with DIR= specifies an alternative search directory for individual compset configuration files (explained in more detail below).
-# Blank lines are ignored.
-# All other lines are assumed to list a single compset configuration filename at the beginning of the line. The remainder of the line is ignored and can be used for comments.

For example, here is the content of a sample COMPSET_LIST_FILE:

    ### List of compsets ###
    #####################

    #DIR=  # optionally set this if not using default ./compsets directory

    2013_sbys_nmmb%glob   	 ! stand-alone global NMM-B
    AMIP_sbys_gsm         	 ! stand-alone GSM - fake example
    2009_nems_gsm_cice_mom5   ! NEMS mediator coupled GSM-CICE-MOM5
    2011_sbys_gsm%wam     	 ! stand-alone GSM run as WAM
    2011_sbys_gsm%wam%ndsl	 ! stand-alone GSM run as WAM with NDSL

For each compset listed, there must be an associated compset configuration file of the same name. By default the `./compsets` subdirectory under the NEMS root directory is searched for compset configuration files. The search directory can be changed by using the DIR=  directive in the `COMPSET_LIST_FILE`.The format of the compset configuration files is described below.

Calling `NEMSCompsetRun` with the -compset `COMPSET_FILE` option allows to run a single compset without the need to first create a `COMPSET_LIST_FILE`. The `COMPSET_FILE` file must be specified with path (absolute or relative) so the compset file can be found by `NEMSCompsetRun`.

**Starting in April, 2016, a more detailed specification for NEMS compsets, below, was introduced**. It adds the run duration after an initial condition label, and includes grid resolution as part of the specifier for each component.

    cfsr%20150401_1hr_nems%cold_gsm%eul%T126_cice%0.5_mom5%0.5
    cfsr%20150401_1hr_nems%cold_gsm%slg%T126_cice%0.5_mom5%0.5
    cfsr%20150401_1day_nems_gsm%eul%T126_cice%0.5_mom5%0.5
    cfsr%20150401_1day_nems_gsm%slg%T126_cice%0.5_mom5%0.5
    cfsr%20150401_30day_nems_gsm%eul%T126_cice%0.5_mom5%0.5
    cfsr%20150401_30day_nems_gsm%slg%T126_cice%0.5_mom5%0.5

### 6.3.2   How to Add a New Compset

A new compset is added to the system by adding a new compset configuration file, either in the default location under `./compsets`, or in a different location referenced by the `DIR=` setting as explained above. The name of the new configuration file is that of the new compset. The existing default compset configuration files under the `./compsets` subdirectory can be used as templates for new configurations. The compset configuration files represent the connection between the compset based approach to label different configurations and the existing NEMS run scripts. The files are very simple short pieces of shell scripts, exporting a number of variables that are being used by the existing NEMS run scripts. Writing a new compset thus requires some knowledge about the standard NEMS run scripts.

#### 6.3.2.1	Compset Nomenclature

On the machine level the name of a compset is not dissected or interpreted. Instead, the compset names are simply matched as literal strings against the name of the available compset configuration files. However, there is a nomenclature to be followed when naming compsets. The purpose of this  nomenclature is to help a human reader to quickly grasp what a specific compset is testing, and to bring some level of order into the many possible component configurations. The general naming format is:

`caselabel[%optDescriptor][_optRunDuration]_architecture_model1[%opt1[%opt2[...[%optN]]]]_model2[...]_..._modelN[...]`

The `"caselabel"` may be as simple as a calendar year, like "2009", indicating a specific set of initial conditions that are being used. This label can be used very freely to indicate specifics about the case being run. For the sake of uniformity the convention is to keep the case label below 16 characters, not use any special characters like underscores, and instead compound by camel case. It can take an option, for example the start data and/or time of an initial condition.

The `"optRunDuration"` specifies the duration of the run specified in the compset.

The `"architecture"` specifies the fundamental coupling mode. Details of the run sequence are set inside the compset file through the nems_configure variable (see below "Compset Configuration"). The currently available options are:



|Architecture options | Description|
|---------------------|------------|
|blocked | Model components are coupled directly through connectors. All connectors are executed in a single block at the beginning of the coupling cycle, followed by executing all of the model components.|
|leapfrog| Model components are coupled directly through connectors. Connectors and model components are interleaved. As a result, some model components receive forcing data that is at the start time of the next coupling step.|
|nems    | Model components are coupled through a central mediator component. Most or all interactions are through the mediator. There may be some direct connections between model components. The default NEMS mediator is used.|
|sbys    | Model components are running side-by-side for the same simulation conditions (start/end times), but without any interaction between the individual models. This architecture option also covers the standalone mode for the case where only a single model component is present.|
|spaceweather | Model components are coupled through a central mediator component. Most or all interactions are through the mediator. There may be some direct connections between model components. The NEMS Spaceweather mediator is used.|

The "model" labels indicate which specific model instances are being run. In order to reduce redundancies introduced by identical permutations, a specific order of model types is used when constructing compset names:
-# `ATM` - Atmosphere
-# `LND` - Land
-# `ICE` - Sea Ice
-# `OCN` - Ocean
-# `WAV` - Wave
-# `IPM` - Ionosphere-Plasmasphere
-# `HYD` - Hydraulic

Several model instances are available for each model type (instances in ** are not yet accessible):


| Model type          | Instance options           | 
| ------------------- | ----------- | 
| `ATM`               | `satm, xatm, gsm, nmmb`    | 
| `LND`               | `slnd, xlnd, lis**`          | 
| `ICE`               | `sice, xice, cice`    | 
| `OCN`               | `socn, xocn, hycom, mom5` | 
| `WAV`               | `swav, xwav, ww3**`    | 
| `IPM`               | `sipm, xipm, ipe` | 
| `HYD`               | `shyd, xhyd, wrfhydro`    | 

For each model type the current non-active instance options are listed below. The definition of these two options is similar to their use in CESM:
- **s<model type>:** ***Stub components*** conform to the NUOPC rules for model components. They do not advertise any fields in their importState or exportState. Their primary use is to test control flow between components in a driver.
- **x<model type>:** ***Dead components*** conform to the NUOPC rules for model components. They advertise fields in the importState and exportState that are appropriate for the specific model type. Import fields may be ignored internally. Export fields are filled with data that changes during time stepping, but has no scientific relevance. Their primary use is in coupled systems with other dead components to test the data transfers between components.

Only model components that are present in specific compset configurations are listed in the compset name.

The compset nomenclature supports appending option strings after each model component. Each option is introduced by a "%" character. Multiple options are simply concatenated. Each option string should be less than 16 characters long, use no special characters like underscores, and compounded by camel case.

#### 6.3.2.2	Compset Configuration Parameters

Each compset configuration file is a bash script that is sourced within the NEMSCompsetRun script as it iterates through the compset file list.  The variables defined in a compset configuration file are referenced by individual components to determine the configuration for that run.  The set of runtime configuration variables varies by component.  Please refer to the component for more information.  A list of variables specific to NEMS is listed below.  The compset configuration script is also responsible for setting up the run directory with component specific runtime configuration and data files.  Setup functions have been defined in NEMSCompsetRun for some components.  Once the runtime configuration is complete please call the appropriate regression test script.

#### 6.3.2.2.1     Compset Variables

|                      |                         |
|----------------------|-------------------------|
| `TEST_DESCR`         | : Compset description   |


#### 6.3.2.2.2     NEMS Variables
These variables are passed to the NEMS configuration file located in tests/nems.configure.[NEMS configuration filename].IN and read at runtime.


|                               |                         |
|-------------------------------|-------------------------|
|`nems_configure`               |: Name of the NEMS configuration file template - includes the run sequence pattern|
|`atm_model`                    |: Atmosphere component ("satm", "xatm", "gsm", "nmmb", "none")|
|`atm_petlist_bounds`           |: Atmosphere component petlist lower and upper bounds|
|`lnd_model`                    |: Land component ("slnd", "xlnd", "lis", "none")|
|`lnd_petlist_bounds`           |: Land component petlist lower and upper bounds|
|`ice_model`                    |: Sea ice component ("sice", "xice", "cice", "none")|
|`ice_petlist_bounds`           |: Sea ice component petlist lower and upper bounds|
|`ocn_model`                    |: Ocean component ("socn", "xocn", "mom5", "hycom", "none")|
|`ocn_petlist_bounds`           |: Ocean component petlist lower and upper bounds|
|`wav_model`                    |: Wave component ("swav", "xwav", "ww3", "none"|
|`wav_petlist_bounds`           |: Wave component petlist lower and upper bounds|
|`ipm_model`                    |: Space weather component ("sipm", "xipm", "ipe", "none")|
|`ipm_petlist_bounds`           |: Space weather component petlist lower and upper bounds|
|`hyd_model`                    |: Hydrology component ("shyd", "xhyd", "wrfhydro", "none")|
|`hyd_petlist_bounds`           |: Hydrology component petlist lower and upper bounds|
|`med_model`                    |: Mediator ("nems", "none")|
|`med_petlist_bounds`           |: Mediator petlist lower and upper bounds|
|`atm_coupling_interval_sec`    |: Atmosphere run sequence coupling interval|
|`ocn_coupling_interval_sec`    |: Ocean run sequence coupling interval|
|`coupling_interval_sec`        |: Run sequence coupling interval|
|`coupling_interval_slow_sec`   |: Run sequence slow coupling interval|
|`coupling_interval_fast_sec`   |: Run sequence fast coupling interval|
:

#### 6.3.2.2.3     Component Default Variable Functions
|                               |                         |
|-------------------------------|-------------------------|
|`export_nmm`             |: Set NMM configuration variables to default values |
|`export_gsm`             |: Set GSM configuration variables to default values |
|`export_fim `            |: Set FIM configuration variables to default values |
|`export_nems`            |: Set NEMS configuration variables to default values|


#### 6.3.2.2.4     Component Setup Functions
|                                |                         |
|--------------------------------|-------------------------|
|`setup_hycom`                   |: Link HYCOM test case runtime configuration and data files to regression test directory|
|`setup_mom5cice`                |: Copy MOM5 test case runtime configuration and data files to regression test directory.|
|`setup_ipe`                     |: Link IPE test case runtime configuration and data files to regression test directory.|
|`setup_spaceweather`            |: Link SpaceWeather test case runtime configuration and data files to regression test directory.|
|`setup_wrfhydro`                |: \<hydro_namelist_file\> \<parameter_data_folder\> \<namelist_hrldas_file\>: Link WRFHydro test case|
|`setup_lis \<lis_config_file>`  |: Link LIS/Noah test case config and data|
|`setup_ww3`                     |: Link WaveWatch3 test case config and data|



#### 6.3.2.2.5     GFS Regression Test Variables
|                               |                         |
|-------------------------------|-------------------------|
|`CNTL_DIR`                     |: Control run directory.  This is the source directory for control run files.|
|`LIST_FILES `                    |: Control run file list.  Files are compared using the unix cmp utility.|

##### 6.3.2.2.6     Run Script
|                               |                         |
|-------------------------------|-------------------------|
|`RUN_SCRIPT`|: Script to be executed after component variables have been set. E.g. rt_gfs.sh, rt_nmm.sh, ...



7.  How to Configure a NEMS Application
==========================================

7.1    Changing Run Time, Queue, Project Number
-----------------------------------------------

The wall times are set in the compset, and those files are in NEMS/compsets and the variable name is WLCL, for example:

    export WLCLK=30

You can set more than 60 minutes. Change the queue and project number in `NEMS/NEMSCompsetRun`, in the section like this:

    elif [ $MACHINE_ID = yellowstone ]; then
    export DATADIR=/glade/p/work/theurich/NEMS-Data
    export ACCNR=P35071400
    # export ACCNR=UCUB0024                                                                             
    export QUEUE=small

7.2    Changing Run Directory
---------------------------------

All outputs go into the run directory, which goes to a unique directory (e.g. under /glade/scratch/${user}). The directory will have a name like rt_*.  When you run the `./NEMS/NEMSCompsetRun`, you will see output like this,

    RUNDIR:
    '/scratch3/NCEPDEV/stmp1/Anthony.Craig/rt_128577/20150401short_nem_gsm_cice_mom5'.



7.3  Model Output Control
---------------------------

This is the directory where things are run and where the output data will be. The default is hourly output from all parts of the model and is a lot of data.  You will want to reduce that if you are running 30 or more days. To reduce the output, in your nems.configure settings, edit this file, NEMS/tests/nems.configure.med_atm_ocn_ice.IN

and set

`DumpFields = false`

everywhere DumpFields is set.  That will turn off output from the mediator and all the caps.  Or you can selectively turn of some of the output in the different caps and mediator.

To change the **GSM output frequency**, edit your compset, for example NEMS/compsets/20150401long_nems_gsm_cice_mom5.

Change to:

    export FHZER=6
    export FHOUT=6

`FHZER` is the GSM accumulated fields zeroing frequency.

`FHOUT` is the GSM output frequency in hours. 

To change the **MOM output frequency**, edit

    NEMS/NEMSCompsetRun

and under setup_mom5, change from 1 hourly to 6 hourly output by changing

    cp ${OCN_INPUT}/diag_table.1hr ${dst_dir}/diag_table

to
    
    cp ${OCN_INPUT}/diag_table.6hr ${dst_dir}/diag_table

To change the **CICE output history frequency**, the ice_in namelist needs to be modified.

Edit ice_in_mine in $SOMEDIR.

    histfreq = ‘m’,’d’,’h’,’x’,’x’
    histfreq_n = 1,1,6,1,1,1

To add a second daily stream and a third 6-hourly stream. Change the ‘d’ or ‘h’ to ‘x’ if you do not want the extra streams. Also, you need to tell the ice what variables go on what stream.

For example:

    f_hi = ‘mdhxx’
    f_aice = ‘mdhxx’
    f_Tsfc = ‘mdxxx’

Where ice thickness (hi) and concentration (aice) will go on all three streams, while Tsfc will only go on the first two (monthly and daily) streams. This will create history files under the ‘history’ subdirectory of the form:

iceh.YYYY-MM.nc or iceh.YYYY-MM-DD.nc, etc

You can edit the NEMS/NEMSCompsetRun file to make sure it is copying your namelist.

    cp $SOMEDIR/ice_in_mine ${dst_dir}/ice_in


7.4  Changing Restart Frequency
----------------------------------

The restart frequency is controlled in each model separately. 

To modify the **MOM restart frequency**, edit restart_interval in the NEMS configure file, NEMS/tests/nems.configure.med_atm_ocn_ice.IN.  restart_interval is set in seconds.

To modify the **GSM restart frequency**:

The GSM does not currently support true restart from restart files. Instead, it is able to restart from the forecast output history files. This is controlled by the variable “NDAYS” specified in the compset. For instance, if a restart after 30 days is required, set NDAYS to 30 and run the forecast. Two files from this run are needed to restart the GSM, sigf720 and sfcf720. These will be read by the GSM when the run is setup for a restart. When true restart from restart file capability works in GSM, restart file frequency will be controlled by the variable “FHRES”, specified in the compset. (For now, FHRES must be set to a number greater than “NDAYS * 24”, otherwise the model will not finish successfully.)

The **CICE restart output frequency** is also controlled by the namelist (ice_in). Edit your copy of the namelist:
Edit `$SOMEDIR/ice_in_mine`

Change the variables:

    dumpfreq = ‘s’
    dumpfreq_n = 21600

To have 4 x daily restarts written. Acceptable units for dumpfreq are ‘s’, ‘d’, ‘m’, and ‘y’.
You can edit the NEMS/NEMSCompsetRun file to make sure it is copying your namelist.
cp $SOMEDIR/ice_in_mine ${dst_dir}/ice_in

**The mediator is currently setup to write restarts at the end of the run** and to overwrite the restarts in the run directory.  To write out restarts during the run, edit the mediator restart_interval in the NEMS configure file, NEMS/tests/nems.configure.med_atm_ocn_ice.IN.  restart_interval is set in seconds.  Restarts written during the run with have a timestamp prepended to the filename.



7.5  Changing PET Counts (PE layout)
--------------------------------------

To change pe counts, do the following.  First, edit the compset (ie. NEMS/compsets/cfsr%20150401_1day_nems_gsm%slg%T126_cice%0.5_mom5%0.5) and change these lines:

    export TASKS=136
    export atm_petlist_bounds="0 23"     
    export ocn_petlist_bounds="24 55"  
    export ice_petlist_bounds="56 75" 	
    export med_petlist_bounds="76 135"

Those specify the start and end task ID for each component as well as the total number of TASKS required. 

If you have changed the CICE pe count, then you will need to modify the file CICE/comp_ice.backend and change the following lines and rebuild the model from scratch.

    setenv NTASK 	20   	# total number of processors
    setenv BLCKX 	72   	# x-dimension of blocks ( not including )
    setenv BLCKY	205	# y-dimension of blocks (  ghost cells  )

If you have changed the MOM pe count, then you will need to modify the mom namelist input file, input.nml and set

    layout = 8,4
    io_layout = 1,4

If you have changes the GSM pe count, then **\<information needed>**
If you have changed the MEDIATOR pe count, the mediator will adapt automatically.



8.  How to Modify a NEMS Application
==========================================



9.  How to Create a NEMS Application
==========================================

In order to use the AppBuilder, a “project” directory and repository at EMC needs to be set up with an AppBuilder configuration file. This can be done either with existing EMC projects or new projects. The approach is outlined below.


9.1  How to create a *new* NEMS application that uses AppBuilder
-------------------------------------------------------------------

We want to create a project ABC that does not exist yet.  An EMC
contact first needs to set up a new repository/directory on the EMC
server: /projects/ABC

* Check out your new ABC repo: 

       svn co svnemc.ncep.noaa.gov:/projects/ABC
       cd ABC

* Make the standard subdirectories: 

       svn mkdir trunk branches tags
       cd trunk

* Edit the externals property of your project root directory: 

       svn propedit svn:externals .

* In the editor, define external links, for example:

       # comments are okay
       NEMS       -r 123456 https://svnemc.ncep.noaa.gov/projects/nems/trunk
       WW3        -r 234567 https://svnemc.ncep.noaa.gov/projects/ww3/branches/esmf2/model
       MOM5       -r 456    https://github.com/feiliuesmf/mom/branches/mom5_5_0_2_nuopc
       MOM5_CAP   -r 12     https://github.com/feiliuesmf/nems_mom_cap/trunk
       CICE       -r 65     https://github.com/feiliuesmf/lanl_cice/trunk
       CICE_CAP   -r 32     https://github.com/feiliuesmf/lanl_cice_cap/trunk

* Commit the svn:externals property:

       svn commit

* After exiting the editor from the previous step 

       svn update

 This will create subdirectories for each of the externals (for
 example NEMS) and will checkout the specified version of each of the
 external codes.

* Create an AppBuilder configuration file in your the current directory (`/projects/ABC/trunk`). The file must have the following format and must have the extension `\<project name>.appbuilder`  Templates are located in `./NEMS/AppBuilder`. [An example configuration for the Climate Forecast System (now UGCS-Seasonal) is included on this page](http://esgf.esrl.noaa.gov/projects/couplednems/appbuilder).

* Build in this directory by using the AppBuilder script:
  `./NEMS/NEMSAppBuilder` If the component is already built on the
  platform: if the component executable exists in the expected
  directory, it will not build it again. Currently there is not a
  mechanism to point to an executable in a different location, but
  that can be added.

* Component configurations (compsets) can now be run that are
  comprised of the components that were identified in the AppBuilder
  file. Compsets that include components not identified in the project
  AppBuilder will abort. There are standard NEMS compsets in the
  directory:

       ./NEMS/compsets/*
       ./NEMS/NEMSCompsetRun -compset 
       ./NEMS/compsets/20150401short_nems_gsm_cice_mom5

People can add to this global collection of compsets, or can create
compsets locally. For local compsets, we recommend adding a compset
directory to the current directory (`projects/ABC/trunk/`). [This page
discusses compset syntax]().

* For regression testing or other cases where there is a desire to
   perform multiple runs in sequence, you can create a compset run
   file. It needs to have a `*.compsetRun` extension. This basically
   just a list of compsets, one per line. The whole list (say
   abc.compsetRun) can be run with the command

       ./NEMS/NEMSCompsetRun abc.compsetRun


9.2  How to create a NEMS application that uses AppBuilder for an existing EMC project
--------------------------------------------------------------------------------------

We recommend creating a new directory that sits at the same level as
the current trunk, e.g. transition_trunk, and treating that
transition_trunk as the current directory. Associated directories
might be called transition_branches, transition_tags, etc. Eventually
these should be renamed to trunk, branches, tags once the transition
is complete and the previous trunk can be removed.





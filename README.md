# How to use the unified workflow for the ufs-s2s-model application (work in progress)

Note, currently the wave gridded post does not work properly. There is work actively ongoing to fix it. 
This message will be deleted when the fix for wave post is committed. 

## Checkout the source code and scripts
```
git clone https://github.com/NOAA-EMC/global-workflow coupled-workflow
cd coupled-workflow
git checkout feature/coupled-crow
git submodule update --init --recursive     #Update submodules 
cd sorc
sh checkout.sh -c                    # Check out forecast model with CCPP=YES/COUPLED=YES
```
## Compile code used in ufs-s2s-model and EMC_post and link fixed files and executable programs:
```
sh build_all.sh -c           #This command will build only execs for coupled

To link fixed files and executable programs for the coupled application:
On Hera: 
sh link_fv3gfs.sh emc hera coupled
On Orion: 
sh link_fv3gfs.sh emc orion coupled
```

## Create CROW user file
CROW is a python-based configuration toolbox, residing under /workflow folder as a git submodule. The /workflow directory also contains
a series of YAML-formatted text files, describing the workflow line-up, default settings and platform descriptions. They collectively
serve as the interface between the coupled-workflow and the CROW toolbox. For general users, only the user file and case file need to 
be worked with.

The user file is a short text file in YAML format, telling CROW the information of the user. Such as scratch space for EXPDIR, 
account information and email for cron job notification purpose. A template named user.yaml.default is included in the repository.

```
cd ../workflow
cp user.yaml.default user.yaml
```

Then, open and edit user.yaml:

- EXPROOT: Place for experiment directory, make sure you have write access.
- FIX_SCRUB: True if you would like to fix the path to ROTDIR(under COMROOT) and RUNDIR(under DATAROOT)
             False if you would like CROW to detect available disk space automatically.
             *** Please use FIX_SCRUB: True on Hera/Orion until further notice (2020/03)
- COMROOT: Place to generate ROTDIR for this experiment.
- DATAROOT: Place for temporary storage for each job of this experiment.
- cpu_project: cpu project that you are working with.
- hpss_project: hpss project that you are working with.

## Create experiment directory using CROW
CROW gets information of the targeted experiment from case files. A case file is a text file in YAML format, describing the information
of the experiment to be configured. A series of pre-generated case files are given under /workflow/cases. You could generate your
own case from scratch as well. For this project, we start with "coupled_free_forecast.yaml". The "coupled_free_forecast.yaml" will
generate a 3-day run case (test_3d) starting from 2016040100. From the /workflow/CROW directory:
```
mkdir -p $EXPROOT
(Note that $EXPROOT is the experiment directory that has been set in the user.yaml file.)
./setup_case.sh -p HERA $CASE test2d

or

./setup_case.sh -p HERA ../cases/$CASE.yaml test2d
```
where $CASE is one of the following:
- prototype_5: Used for UFS S2S Prototype 5 (Runs 35 day cases with atm-ocn-ice-wav coupling, use with p5 workflow only)
- coupled_free_forecast: 2 day tests for atm-ocn-ice coupling 
- coupled_free_forecast_wave: 2 day test for atm-ocn-ice-wav coupling (frac grid)
- coupled_free_forecast_nofrac_wave: 2 day test for atm-ocn-ice-wav coupling (non frac grid)
- atm_free_forecast:  Run the atm only case with same ICs as coupled tests 
Please see the bottom of the README for information about particular versions and ICs

This will create a experiment directory ($EXPERIMENT_DIRECTORY). In the current example, $EXPERIMENT_DIRECTORY=$EXPROOT/test_3d.

For Orion: 
First make sure you have python loaded: 
```
module load contrib
module load rocoto #Make sure to use 1.3.2 
module load intelpython3
```
and then replace ORION with HERA in the commands above. 

## Create Rocoto XML using CROW
The final process of workflow configuration is to generate a XML file for Rocoto. After the previous step, CROW will pop-up the
command for this step:
```
./make_rocoto_xml_for.sh $EXPERIMENT_DIRECTORY
```

## Run the model using the workflow
```
cd $EXPERIMENT_DIRECTORY
module load rocoto
rocotorun -w workflow.xml -d workflow.db
```
The first jobs will be submitted and should now be queued or running. Note, you will need to continue 
to run rocotorun until the workflow is complete. This can be done via cron jobs or manually submitting 
rocotorun until the workflow is complete.

Rocotodocumentation can be found here: https://github.com/christopherwharrop/rocoto/wiki/documentation

# Monitor your rocoto-based run

Click here to view full rocoto documentation on GitHub:

https://github.com/christopherwharrop/rocoto/wiki/documentation

## Use rocoto commands on the command line

Start or continue a run:

```
rocotorun -d /path/to/workflow/database/file -w /path/to/workflow/xml/file
```

Check the status of the workflow:

```
rocotostat -d /path/to/workflow/database/file -w /path/to/workflow/xml/file [-c YYYYMMDDCCmm,[YYYYMMDDCCmm,...]] [-t taskname,[taskname,...]] [-s] [-T]
```

Note: YYYYMMDDCCmm = YearMonthDayCycleMinute ...where mm/Minute is ’00’ for all cycles currently.

Check the status of a job:

```
rocotocheck -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname
```

Force a task to run (ignores dependencies - USE CAREFULLY!):

```
rocotoboot -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname
```

Rerun task(s):

```
rocotorewind -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname
```

Several dates and task names may be specified in the same command by adding more -c and -t options. However, lists are not allowed.



## Set up your experiment cron

### HPCs with access to directly edit your crontab files (WCOSS-Cray, Hera, Jet)

`
crontab -e
`

or

`
crontab workflow.crontab
`

_(WARNING: "crontab workflow.crontab" command will overwrite existing crontab file on your login node. If running multiple crons recommend editing crontab file with "crontab -e" command._

Check your crontab setting:

`
crontab -l
`

## Using crons on Orion 

On orion, you will need to update the path to rocotorun in the autogenerated workflow.crontab  to be
/apps/contrib/rocoto/1.3.1/bin/rocotorun

Also note, on orion you need to be on login node 1 for cronjobs. 

## Resource handling
There are two ways of changing resource settings (cpu count, time limits, threads) for a job that has already been defined in the workflow.

If you would like to only change the resource settings for an experiment that already set up. Then all you need to do is editing a file named "resources_sum.yaml" under $EXPDIR, and run CROW again with -f option activated. More specifically:
```
cd $EXPDIR
(edit resources_sum.yaml)
cd ~/workflow/CROW
./setup_case.sh -p HERA -f ../cases/coupled_free_forecast.yaml test_3d
./make_rocoto_xml_for.sh $EXPERIMENT_DIRECTORY
```
In this way, both your config files and rocoto xml will be updated with the new resource settings.

If you would like to change the resource settings on a permanent base, and push into the repository. Then you would need to edit this file: ~/workflow/defaults/default_resources.yaml. Changes in this file is trivial in some cases while complicated in others. For now, instructions are only made for changing the following parameters: ranks(number of cpu), ppn(number of cpu per node), clock time limit, for prep and atmospheric post; and changing ONLY clock time limit for medcold forecast and primary forecast.

resources for all forecast jobs are listed in gfs_resource_table in this file. Resources for most jobs are resolution-dependent. If you would like to change settings for prep and atmospheric post job. Locate "prep" or "gfspost" entry in corresponding resolution within the table (C192, C384, C768), and upate the entry with the desired value.

If you would like to update clock time limit for primary or medcold forecast job: update this entry: "coupfcst_medcold_wall/coupfcst_wall".

If you want more advanced changes in the resource settings, contact a EIB person.

After changing this file, you NEED to start a new experiment instead of overwriting the existing one. The reason is that, the "resources_sum.yaml" within $EXPDIR has the highest priority so that any changes in default_resources.yaml will be overwritten.


## Initial Conditions 

Currently this is set up for benchmark runs which have ICs which are already generated.  Available dates are the 1st and the 15th of the month starting April 1, 2011 thourgh March 15, 2018. 

## Particular versions 

Prototype 5 was run with global-workflow hash 005468b9299ea6fc9afdbeace33c336c6797833a

## Managing Default / Adding new configuration variables

workflow/default is the central place of managing default values of configuration variables. In principle, every single variable that could be configured by CROW need to have a default value included in workflow/default. A series of YAML files are used to storage these values. During the configuration step, CROW will load values from workflow/default first, and then customized by the overriding values in the YAML file selected by the user within workflow/cases/.

defaults/case.yaml is the top-level structure of the configuration system, consisted of a series of XX_settings blocks. Each of the blocks describes the source of information that it got from, and form a "!MergeMapping" object in YAML. Typically, these information started from "defaults" and then overriden by the case file.

In each of the XX_settings file, the first line is the title and label of this piece of file. For example: "ice_defaults: &ice_defaults". By doing this, CROW will be able to locate "ice_defaults" to here when reads "doc.ice_defaults" in the defaults/case.yaml. The following lines are standard key-value pairs.

In order to add new variable and set a default, you could either add "Varname : Value" pair into an existing YAML files in workflow/default, or create a new YAML file under workflow/default. The later solution should only be chosen when the earlier one doesn't work out. 

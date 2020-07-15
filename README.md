# How to use the unified workflow for the ufs-s2s-model application (work in progress)

## Checkout the source code and scripts
```
git clone https://github.com/NOAA-EMC/global-workflow coupled-workflow
cd coupled-workflow
git checkout feature/coupled-crow
git submodule update --init --recursive                   #Update submodules if necessary
cd sorc
sh checkout.sh coupled                                    # Check out the coupled code, EMC_post, gsi, ...
```
## Compile code used in ufs-s2s-model and EMC_post and link fixed files and executable programs:
```
sh build_ncep_post.sh        #This command will build ncep_post.
sh build_fv3_coupled.sh      #This command will build ufs-s2s-model

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

2020/03: Please use FIX_SCRUB: True option on Hera until further notice. 

```
cd ../workflow
cp user.yaml.default user.yaml
Then, open and edit user.yaml:

- EXPROOT: Place for experiment directory, make sure you have write access.
- FIX_SCRUB: True if you would like to fix the path to ROTDIR(under COMROOT) and RUNDIR(under DATAROOT)
             False if you would like CROW to detect available disk space automatically.
- COMROOT: Place to generate ROTDIR for this experiment.
- DATAROOT: Place for temporary storage for each job of this experiment.
- cpu_project: cpu project that you are working with.
- hpss_project: hpss project that you are working with.
```

## Create experiment directory using CROW
CROW gets information of the targeted experiment from case files. A case file is a text file in YAML format, describing the information
of the experiment to be configured. A series of pre-generated case files are given under /workflow/cases. You could generate your
own case from scratch as well. For this project, we start with "coupled_free_forecast.yaml". The "coupled_free_forecast.yaml" will
generate a 3-day run case (test_3d) starting from 2016040100. From the /workflow/CROW directory:
```
mkdir -p $EXPROOT
(Note that $EXPROOT is the experiment directory that has been set in the user.yaml file.)
./setup_case.sh -p HERA coupled_free_forecast test_3d

or

./setup_case.sh -p HERA ../cases/coupled_free_forecast.yaml test_3d


For Orion: 
First make sure you have python loaded: 
module load contrib
module load rocoto
module load intelpython3
and then replace ORION with HERA in the commands above. 

```
This will create a experiment directory ($EXPERIMENT_DIRECTORY). In the current example, $EXPERIMENT_DIRECTORY=$EXPROOT/test_3d.

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

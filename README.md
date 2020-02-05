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
sh link_fv3gfs.sh emc hera coupled
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
Then, open and edit user.yaml:

- PROJECT_DIR: Place for experiment directory, make sure you have write access.
- cpu_project: cpu project that you are working with.
- hpss_project: hpss project that you are working with.
```

## Create experiment directory using CROW
CROW gets information of the targeted experiment from case files. A case file is a text file in YAML format, describing the information
of the experiment to be configured. A series of pre-generated case files are given under /workflow/cases. You could generate your
own case from scratch as well. For this project, we start with "coupled_free_forecast.yaml". The "coupled_free_forecast.yaml" will
generate a 3-day run case starting from 2016040100. From the /workflow/CROW directory:
```
mkdir -p $PROJECT_DIR
(Note that $PROJECT_DIR is the experiment directory that has been set in the user.yaml file.)
./setup_case.sh -p HERA coupled_free_forecast test_3d

or

./setup_case.sh -p HERA ../cases/coupled_free_forecast.yaml test_3d
```
This will create a experiment directory ($EXPERIMENT_DIRECTORY), containing all config files under the $PROJECT_DIR defined in user file.

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

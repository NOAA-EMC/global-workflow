# Workflow Setup Utility:

## Introduction
This utility is designed to be an automated ecFlow and Rocoto generation application,
used to create the folder structures and scripts needed to execute the workflows
for either application. As of April 2022, this application only works for ecFlow.

### How ecFlow Setup Works
For ecFlow creation, the application takes a YAML file as input, pulls in any
environment variables that are specified in the YAML, then using the ecFlow
API, a definition file is created. Additionally, since ecFlow definition files
are dependent on folder structures, the application also identifies the scripts
associated with tasks and creates the folders for them, checks the script repository
folder and puts the scripts in their appropriate location.

Please refer to the [setup the YAML](#configuring-the-yaml-file) section for instructions
on how to setup the YAML file for what you want.

## Setup for using the utility with ecFlow
This utility uses Python3.6 and later. It will not work with Python anything before
Python3.6.

### Pre-Requisites
In order to run the application the following Python3 modules need to be available:
* ecflow
* numpy
* PyYAML
These modules should be available on Hera and Orion.

### Experiment Setup
This application requires the use of a config.base file. The location of the file
can be specified with the `--expdir` parameter. The file will be read in and
the ush/rocoto/workflow_utils.py script will be used to populate any environment
variables that are needed.

### Required Environment Variables
If not setup within the script, the following environmnt variables are required:
* Account
* Queue
* machine
* RUN_ENVIR
These parameters are populated as 'edits' within the ecFlow definition file for
any of the suites that are created.

An additional environment variable that is needed is:
* ECFgfs
This will be used as the base location for storing the suite scripts and also
used as the base location to look for the script repository. The application
assumes the default that the script repo is ECFgfs/scripts. Suggested edit is to
add the following to the config.base file:
* export ECFgfs=$HOMEgfs/ecf

## Configuring the YAML file
The utility works primarily off of the yaml file used to define the suites,
families, and tasks.

### Setting up externs
To add externs, add the `externs:` label to the base level of the yaml file,
then add each extern as a yaml list object
* Example:

```
externs:
- "/prod18/enkfgdas/post"
- "/prod18/enkfgfs/post"
```

### Setting up a suite
To add a suite add the `suites:` label to the base level and then add the suite
names you want as dictionary objects:
* Example:

```
suites:
  fcstonly:
  fcstplus:
```

### Setting up families
Once a suite has been setup, add families as dictionary objects under the families.
Families can be dictionary objects under other families. In the example below the
fcstonly suite has the family gfs and the post family under that. The fcstplus
suite has the family gdas and the family atmos under that.
* Example

```
suites:
  fcstonly:
    gfs:
      post:
  fcstplus:
    gdas:
      atmos:
```

### Adding edits
Edits can be added to either families, tasks or suites by putting an `edit:`
dictionary tag and then listing the edits below. In the example below, the `RUN`
edit will be assigned the parameter `00` for the `gfs` family.
* Example

```
suites:
  fcstonly:
    gfs:
      edits:
        RUN: '00'
```

### Setting up the tasks
After the families are defined, tasks are defined by placing a `tasks:` dictionary
object under a family. Tasks are then put into dictionary objects. In the example below, the `gfs` family has an `atmos` family underneath it
with the `jgfs_forecast` task  
* Example

```
gfs:
  atmos:
    tasks:
      jgfs_forecast:
```

#### Task Options
Additional options are `triggers`, `events`, `edits`.

## Run the utility
1. Change into the workflow directory:
` cd global-workflow/workflow_generator`
1. Run the utility
```
python3 setup_workflow.py --expdir ../parm/config
```

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

### Script Repository
The scriptrepo can be specified either by an environment variable or by setting
a `scriptrepo: /path/to/scripts` variable in the YAML file at the top level.

The script repository is the location the application will search for the ecf scripts to 
deploy the to the correct location. 

### Setting up externs
To add externs, add the `externs:` label to the base level of the yaml file,
then add each extern as a yaml list object
* Example:

```YAML
externs:
- "/prod18/enkfgdas/post"
- "/prod18/enkfgfs/post"
```

### Setting up a suite
To add a suite add the `suites:` label to the base level and then add the suite
names you want as dictionary objects:
* Example:

```YAML
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

```YAML
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

```YAML
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

```YAML
gfs:
  atmos:
    tasks:
      jgfs_forecast:
```

#### Task Options
Additional options are `triggers`, `events`, `edits`, and `defstatus`.

##### Triggers
To add a trigger to a task, add a `triggers:` heading underneath the task or family. The triggers need to be a list
item with the identifier for what you want the trigger to look for. So for a task, it would be `- task: task_name` or
for a family it would be `- family: family_name`

Trigger list items can also have events, states, or suites as part of their configuration. Valid events must be listed
in the definition file previously. Valid states are complete, active, or queued. The suite must also be a defined suite
somewhere in the YAML file. 

* Example
```YAML
gfs:
  edits:
    RUN: 'gfs'
    NET: 'gfs'
  tasks:
    jgfs_forecast:
      triggers:
      - task: jgfs_atmos_analysis
        event: release_fcst
      - task: jgfs_wave_prep
```

##### Events
To add events to a task, add an `events:` heading underneath the task. The items in the events is a list of strings.
* Example: This has two triggers and one event associated with it. 
```YAML
analysis:
  tasks:
    jgfs_atmos_analysis:
      triggers:
      - task: jobsproc_gfs_atmos_prep
      - task: jgfs_atmos_emcsfc_sfc_prep
      events:
      - "release_fcst"
```

##### Edits
To add edits to a task or family or suite, add an `edits:` heading underneath the node. The items go in dictionary
format such that `edit: edit_value`
* Example
```YAML
gfs:
  edits:
    RUN: 'gfs'
    NET: 'gfs'
```

##### Defstatus
To add defstatus to a task or family, add a `defstatus:` parameter underneath the node. It will have a value associated
with it such that `defstatus: value`
* Example
```YAML
obsproc:
  defstatus: complete
```
### Ranges and Lists
Families or tasks, or even triggers and events can have ranges or lists associated with them to shorten the creation
of the definition YAML. The range notation uses the `( )` bracket syntax to specify and follows the same pattern as 
Python ranges. If one value is specified, it assumes it is the max value, starts at 1, increments by 1 up to the value
specified. If two values are presented, then it uses the first as the initial starting value, increments by 1 up to the
max value specified. If all three, it uses them with the initial, increment, and max range values. If no value or 
no max value is specified it uses what ever values are used in the parent counter. 
* Example
```YAML
jgfs_atmos_post_f( 384 ):
    template: jgfs_atmos_post_master
    triggers:
    - task: jgfs_atmos_post_manager
      event: release_post( )
    edits:
      FHRGRP: '( 1, )'
      FHRLST: 'f( )'
      FHR: 'f( )'
      HR: '( )'
```

Lists are similar to the ranges but use the `[ ]` bracket syntax. Items in the list can be of any type and will run 
the same way as ranges do
* Example
```YAML
post:
  tasks:
    jgfs_atmos_post_manager[ 1,2 ]:
```
## Run the utility
1. Change into the workflow directory:
` cd global-workflow/workflow_generator`
1. Run the utility
```
python3 setup_workflow.py --expdir ../parm/config
```

### Command Line Options
* --ecflow-config
  * Path to the YAML configuration file to use to generate the definition and folder/scripts. 
* --expdir
  * Path to the experiment directory. Must contain config.base.
* --savedir
  * Path to where the definition file will be saved. Defaults to current directory.
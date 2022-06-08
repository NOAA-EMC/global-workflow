# Workflow Setup Utility:

## Introduction
This utility is designed to be an automated ecFlow and Rocoto generation application,
used to create the folder structures and scripts needed to execute the workflows
for either application. As of June 2022, this application only works for ecFlow.

### How ecFlow Setup Works
For ecFlow creation, the application takes a YAML file as input, pulls in any
environment variables that are specified in the YAML, pulls in any environment
variables that are set in the shell, then using the ecFlow
API, a definition file is created. While the application creates the definition file
it also uses the path defined as `ECFgfs`, which will be elaborated on later in this
guide, and creates the folders and scripts that match the definition file, setting the
`ECFHome` variable in the definition file to match the `ECFgfs` parameter.  

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
This application requires the use of a config.base file. With Rocoto and other applications
the config.base file was setup with the experiment scripts so this application does
presume that the `config.base` file is setup and working. It does NOT import any other 
configuration files from that same directory, though it would not be hard to modify the code
to pull in other parameters as well. The command line argument `--expdir` can be used
to identify the folder that contains the `config.base` file. The file will be read in and
the ush/rocoto/workflow_utils.py script will be used to populate any environment
variables that are needed. 

### Required Environment Variables
If not setup within the script, the following environment variables are required:
* Account
* Queue
* machine
* RUN_ENVIR

These parameters are populated as 'edits' within the ecFlow definition file for
any of the suites that are created.

An additional environment variable that is needed is:
* ECFgfs
This parameter is used in a number of different locations within the application. 
First, it will set the 
used as the base location to look for the script repository. The application
assumes the default that the script repo is ECFgfs/scripts. Suggested edit is to
add the following to the config.base file:
* `export ECFgfs=$HOMEgfs/ecf`
* **NOTE**: Older versions of the `config.base` may not contain this export so it
will be important to add as the application does rely on some pieces of information
from that folder.
* **NOTE**: In the examples provided below and their output, the `HOMEgfs` parameter
is set to `/usr1/knevins/global-workflow` so you can make the associated reference in
yours to match the output that you are looking to accomplish.

## Configuring the YAML file
The utility works primarily off of the yaml file used to define the suites,
families, and tasks.

### Script Repository
The workflow generator will create the folders and scripts necessary to run the suite 
that is defined within the YAML file. It will create the items at the ECFgfs folder location.
However, the generator needs to know where to look for the scripts to copy them to the 
final destination spot. This is done using the scriptrepo variable. The scriptrepo 
can be specified either by an environment variable or by setting
a `scriptrepo: /path/to/scripts` variable in the YAML file at the top level. By default the
generator will look in `$ECFgfs/scripts` for the scripts. 

The scripts in the script repo do not need to be in any particular folders, they just need to
be unique names. The generator will recursively look in all folders for a script that matches
the task that was set up in the YAML file. For tasks that are loops or lists, a template 
parameter can be used. Please refer to the [Setting up the tasks](#setting-up-the-tasks) 
section of this guide. 

Running with just a suite declared in the YAML file such as:
```YAML
---
suites:
  prod00:
```
Will return a definition file that has the following content with the default suite edits as 
well as the `ECF_HOME` and `ECF_INCLUDE` parameters being set to the `ECFgfs` parameter set
in the config.base file. 
```commandline
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
endsuite
# enddef
```

### Setting up externs
To add externs, add the `externs:` label to the base level of the yaml file,
then add each extern as a yaml list object. Please note that the configuration of externs 
as options for tasks has not yet been enabled. Those would have to be manually configured
after creating the definition file with the generator. 
* Example:

```YAML
---
externs:
  - "/prod18/enkfgdas/post"
  - "/prod18/enkfgfs/post"
suites:
  prod00:
```
Will produce
```commandline
#5.8.1
extern /prod18/enkfgdas/post
extern /prod18/enkfgfs/post
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
endsuite
# enddef
```
### Setting up a suite
To add items to a suite, first add a `suites:` line to the YAML file at the top level. Once 
the `suites:` line has been added, as a sub object underneath, add whatever suites you would 
like to have as dictionary objects.

Suites can be either strings that define the single suite or it can be a list object defined
by using the `[ ]` syntax, such as `prod[00,06]`, which would create two suites, one called 
`prod00` and one called `prod06`. 

A suite can be defined more than once. For example, if you want two suites, `prod00` and `prod06`
which contain almost completely exact entities but the `prod06` has one extra task, you
can define a suite for `prod[00,06]` and put all of the tasks for both in that section and then
below that define a `prod06` suite and add in the specific task you want.

The generator goes through the suites serially, taking whatever is defined first in the
file and then go through the rest. This means that overrides are possible, so the order
in which the file is set up does matter.

* Example:
This will produce a definition file with two suites, prod00 and prod06. 
```YAML
suites:
  prod[00,06]:
  prod00:
```
Will produce:
```commandline
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
endsuite
suite prod06
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
endsuite
# enddef
```

### Setting up families and tasks
Once a suite line has been added, families need to be added under a `nodes:` header.
First add the `nodes:` dictionary line under the suite name, then create the hierarchical 
structure for the families. 
Families can be dictionary objects under other families. In the example below, the suites
`prod00` and `prod06` will have the family `gfs`.  
Then only the `prod00` suite will have the family `gdas` added to it. 
* Example
```YAML
suites:
  prod[00,06]:
    nodes:
      gfs:
        tasks:
          jgfs_forecast:
  prod00:
    nodes:
      gdas:
        tasks:
          jgdas_forecast:
```


### Adding edits
Edits can be added to either families, tasks or suites by putting an `edit:`
dictionary tag and then listing the edits below. In the example below, the `RUN`
edit will be assigned the parameter `00` for the `gfs` family.
* Example

```YAML
suites:
  prod[00,06]:
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
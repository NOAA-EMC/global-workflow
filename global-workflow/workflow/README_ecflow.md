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

Additionally, this utility makes use of modules within the ecflow_setup folder so you
need to ensure that your `PYTHONPATH` or whatever module pathing you are using
does allow you to import modules from other parts of the application folders. By
default this should generally be done without setting additional parameters but it
is possible that a custom parameter may be needed.

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

If not setup within the script, the following environment variables are required
either in the shell or within the config.base file:
* Account
* Queue
* machine
* RUN_ENVIR

These parameters are populated as 'edits' within the ecFlow definition file for
any of the suites that are created.

An additional environment variable that is needed is:
* ECFgfs
This parameter is used in a number of different locations within the application.
First, it will set the destination for populating the scripts and directories
needed to execute the definition plan within that folder. Second it will set the
`ECF_HOME` and `ECF_INCLUDE` parameters to that value. Third, if no scriptrepo
parameter is set, it will look in the `$ECFgfs/scripts` folder for the scripts
associated with the tasks or templates defined within the YAML file.

If not already in the environment, it is suggested to add the following to the config.base file:
`export ECFgfs=$HOMEgfs/ecf`

* **NOTE**: Older versions of the `config.base` may not contain this export so it
will be important to add as the application does rely on some pieces of information
from that folder.
* **NOTE**: In the examples provided below and their output, the `HOMEgfs` parameter
is set to `/usr1/knevins/global-workflow` so you can make the associated reference in
yours to match the output that you are looking to accomplish.

## Configuring the YAML file

The utility works primarily off of the yaml file used to define the suites,
families, and tasks. You will need to define the pieces within the file using a
YAML syntax and then the reserved words identified below.

### Using Environment Variables in the YAML

This application was built to use environment variables provided either through an export in the
shell environment or by specifying a value in the config.base file. To use an environment value,
the YAML file has a reserved word prefix `env.`. The code functions by parsing the YAML file into
a dictionary then doing a recursive search over that dictionary to determine if the `env.` prefix
is used anywhere, either a value or key. When a node uses that syntax, the application will search first
the current shell environment variables for a match, if none exists, then it will search
the `config.base` file for any configurations that may have been exported from there. Finally, it will
then replace the string `env.PARAMETER` with the value from the shell or `config.base` file in the
dictionary that was imported. The original YAML file will remain unchanged.

**NOTE:** The environment variable cannot be used in conjunction with a string so trying to use
`env.ECFgfs/include` will return only the value for `ECFgfs`, it will not append any strings or
values to the beginning or end of the value.

Example:
Entering `env.FHMAX_GFS` as a value for a node will use the value that was
specified in the `config.base` file for the `FHMAX_GFS` export. This will be reflected in the final
definition file. It will not be updated in the original YAML file, that will remain as
`env.FHMAX_GFS`.


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

### A Basic YAML File

The YAML file follows the standard YAML syntax structure. It is suggested to use the `---`
triple hyphen line to start the file, followed by the start of the syntax. Some YAML editors
will allow you to specify the indentation between the lines but the common practice
is to set the following when creating your YAML file:
* Use `---` to start the file
* Use two spaces for indentation on sub-items
* A colon followed by a space, `: `, is an indicator of a mapping.
* A space followed by a pound sign ` #` is an indicator of a comment.

Running with just a suite declared in the YAML file such as:
```YAML
---
suites:
  prod00:
```
Will return a definition file that has the following content with the default suite edits as
well as the `ECF_HOME` and `ECF_INCLUDE` parameters being set to the `ECFgfs` parameter set
in the config.base file.
```bash
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
* Example

```YAML
---
externs:
  - "/prod18/enkfgdas/post"
  - "/prod18/enkfgfs/post"
suites:
  prod00:
```
* Result:
```bash
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
* Result:
```bash
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

### Setting Up Families and Tasks

Once a suite line has been added, families need to be added under a `nodes:` header.
First add the `nodes:` dictionary line under the suite name, then create the hierarchical
structure for the families.
Families can be dictionary objects under other families. In the example below, the suites
`prod00` and `prod06` will have the family `gfs`.
Then only the `prod00` suite will have the family `gdas` added to it.

Once the family structure has been setup, add in a `tasks` dictionary under the
family to which you want to add that task. In the example below, both the `prod00`
and `prod06` suites will have the `gfs` family with the `gfs_forecast` task but the
`prod00` suite will also have the `gdas` family with the `jgdas_forecast` task.

**Note**: The families can have a hierarchy but if there is nothing in the family
then the definition file considers it an empty one and does not add it to the overall
definition file. As long as there is a node definition within the family, such as a
task, repeat, other family,  or trigger, it will show up in the definition file.

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
* Result:
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    task jgfs_forecast
  endfamily
  family gdas
    task jgdas_forecast
  endfamily
endsuite
suite prod06
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    task jgfs_forecast
  endfamily
endsuite
# enddef
```

### Adding edits

Edits can be added to either families, tasks or suites by putting an `edits:`
dictionary tag and then listing the edits below. The format for edits will be
the edit variable on the left and then the value on the right.

So in this example below, lets consider that we want the `RUN` value to be
`GFS` for both the `prod00` and `prod06` suite but we wnat the `CYC` value
to be `00` for the `prod00` suite and `06` for the `prod06` suite. So in
that case we would use the individual declaration for the suites for the
`CYC` value only and then the listed suites declaration for the rest.
* Example

```YAML
suites:
  prod00:
    edits:
      CYC: '00'
  prod06:
    edits:
      CYC: '06'
  prod[00,06]:
    nodes:
      gfs:
        edits:
          RUN: 'GFS'
        tasks:
          jgfs_forecast:
```
* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  edit CYC '00'
  family gfs
    edit RUN 'GFS'
    task jgfs_forecast
  endfamily
endsuite
suite prod06
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  edit CYC '06'
  family gfs
    edit RUN 'GFS'
    task jgfs_forecast
  endfamily
endsuite
# enddef
```

#### Task Setup Extras

Tasks are added in as a dictionary under the `tasks:` header. So if you want to add
multiple tasks to a family, do not add them in list syntax, add them as hashes to the dictionary.

* Example
```YAML
suites:
  prod[00,06]:
    nodes:
      gfs:
        atmos:
          analysis:
            tasks:
              jgfs_atmos_analysis:
              jgfs_atmos_analysis_calc:
```
* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family analysis
        task jgfs_atmos_analysis
        task jgfs_atmos_analysis_calc
      endfamily
    endfamily
  endfamily
endsuite
suite prod06
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family analysis
        task jgfs_atmos_analysis
        task jgfs_atmos_analysis_calc
      endfamily
    endfamily
  endfamily
endsuite
# enddef
```

#### Task Script Repository and Templates

When adding tasks, it is possible that you may want to run a task for every forecast hour in a large range
but not want to copy and paste the same script for every forecast hour. With the generator application, you
can specify a `template:` parameter. After defining the [script repo](#script-repository) parameter, the
application will search the defined directory for the template script. It will then copy the template script to
the destination folder for the suite with an adjusted name.

In the example below, you can see the range used for the `jgfs_atmos_post_f` forecast hour task with a template.
Please refer to the [ranges and lists](#ranges-and-lists) section of this document for information on how to set
up a range but for the purposes of the example below, we are focusing on the template. What is relevant here is
that we want 4 instances of the `jgfs_atmos_post_f` forecast hour script to be in place and use the same
`jgfs_atmos_post_master` script for the template.

In addition to the resultant defintion file, noted below is the folder that was created for the `prod00` suite. The
`prod00` folder is located at the `$HOMEecf`, in the case below you cans see it is defined as
`/usr1/knevins/global-workflow/ecf` location and contains four instances of the `jgfs_atmos_post_master`
script, each renamed to match the `task` name in the definition file.

**NOTE:** A special template value is `skip`. If you use `template: skip` in a task, the generator will know that
the script is in fact not in the script repository and it will not attempt to copy or create it but it will
add it to the definition file. This is useful in conjunction with the [defstatus](#defstatus) parameter so the
suite will skip already done tasks and there won't be a representation of it in the final directory.

* Example
```YAML
suites:
  prod00:
    nodes:
      gfs:
        atmos:
          post:
            tasks:
              jgfs_atmos_post_f( 4 ):
                template: jgfs_atmos_post_master
```

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family post
        task jgfs_atmos_post_f000
        task jgfs_atmos_post_f001
        task jgfs_atmos_post_f002
        task jgfs_atmos_post_f003
      endfamily
    endfamily
  endfamily
endsuite
# enddef
```

* Resulting Folder Setup
```bash
$ tree prod00
prod00
└── gfs
    └── atmos
        └── post
            ├── jgfs_atmos_post_f000.ecf
            ├── jgfs_atmos_post_f001.ecf
            ├── jgfs_atmos_post_f002.ecf
            └── jgfs_atmos_post_f003.ecf

3 directories, 4 files
```

### Setting Up Triggers, Events, and Defstatus

#### Events
To add an event, you first need to add the `events:` dictionary heading underneath the node to which it needs to be
added. Then underneath that `events:` heading, as a list object, add the list of events that you want have attached.

**NOTE:** Events can be ranges or list objects, please see the section below on creating lists or ranges.

**NOTE:** Events must be added in a list. This is not the same structure as adding tasks, which are dictionary objects,
the events list is an actual list so please make sure to add a hyphen, `-`, in front of every event that you wish to
add.

* Example
```YAML
suites:
  prod00:
    nodes:
      gfs:
        atmos:
          analysis:
            tasks:
              jgfs_atmos_analysis:
                events:
                  - release_fcst
```

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family analysis
        task jgfs_atmos_analysis
          event release_fcst
      endfamily
    endfamily
  endfamily
endsuite
# enddef
```

#### Triggers

To add a trigger, add a `triggers:` dictionary heading underneath the task or family. The triggers need to be a list
item with the identifier for what you want the trigger to look for. So for a task, it would be `- task: task_name` or
for a family it would be `- family: family_name`

**NOTE:** It was mentioned above but an important distinction from tasks is that triggers need to be in list format.
The reason for this is due to triggers being either families or tasks, and that is determined by the dictionary
label for the list item.

**NOTE:** By default, multiple triggers are added to a node with __AND__

Triggers can also have the following items associated with it:
* `event:`
  * This is listed as part of the list item but in it's own `event:` header. The `event:` must exist within the suite
  or it will be rejected.
  * Events can be lists or ranges.
* `state:`
  * This will identify the state of the task or family in the trigger. States are generally `active`, `complete`, or
  `queued`.
* `suite:`
  * If the trigger is for a task within another suite, add the suite identifier to the trigger and it will look up
  the full path to the trigger and add it. **NOTE:** The suite reference must exist within the definition file, this
  will not work for `externs`.
* `operand:`
  * By default multiple triggers are added to the node with __AND__ as their connection. However, in the event that
  the triggers need to be added with an __OR__ statement, add the `operand: OR` kesuites:
  prod00:
    nodes:
      gfs:
        atmos:
          post:
            tasks:
              jgfs_atmos_post_f( 4 ):
                template: jgfs_atmos_post_master
```YAML
suites:
  prod00:
    nodes:
      gfs:
        tasks:
          jgfs_forecast:
            triggers:
            - task: jgfs_atmos_analysis
              event: release_fcst
            - task: jgfs_wave_prep
              state: complete
        atmos:
          analysis:
            tasks:
              jgfs_atmos_analysis:
                events:
                  - release_fcst
        wave:
          prep:
            tasks:
              jgfs_wave_prep:
```

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family analysis
        task jgfs_atmos_analysis
          event release_fcst
      endfamily
    endfamily
    family wave
      family prep
        task jgfs_wave_prep
      endfamily
    endfamily
    task jgfs_forecast
      trigger /prod00/gfs/atmos/analysis/jgfs_atmos_analysis:release_fcst
      trigger -a /prod00/gfs/wave/prep/jgfs_wave_prep == complete
  endfamily
endsuite
# enddef
```

#### Defstatus

At the time of this README, the use case for the def status was to be able to add nodes to a definition file, and have
them marked as complete so that the ecflow run knows that the script is there but acknowleges as done without having to
do anything. This is useful when running development tasks, that rely on an operational task, but the operational task
is already done and nothing else needs to be executed.

To add defstatus to a task or family, add a `defstatus:` parameter underneath the node, not a dictionary, this will be
a key/value pair. It will have a value associated with it so the item will look like `defstatus: value`

**NOTE:** A defstatus can be added to a family or a task object. Both are acceptable formats.

**NOTE:** When a defstatus is defined for a parent object, all child nodes under the object inherit that so in the
example below, all families and tasks are considered complete and since the `template: skip` value is there for the
task, the script generator will not attempt to look for it in the script repo.

* Example
```YAML
suites:
  prod00:
    nodes:
      obsproc:
        defstatus: complete
        v1.0:
          gfs:
            atmos:
              dump:
                tasks:
                  jobsproc_gfs_atmos_dump:
                    template: skip
                    events:
                    - "release_sfcprep"
```

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family obsproc
    defstatus complete
    family v1.0
      family gfs
        family atmos
          family dump
            task jobsproc_gfs_atmos_dump
              event release_sfcprep
          endfamily
        endfamily
      endfamily
    endfamily
  endfamily
endsuite
# enddef
```

### Repeats

Repeats are in a standalone section because of the nature of how ecflow handles repeating tasks. Ecflow has multiple
methods for handling repeating tasks but they lack a lot of the specificity that one would hope. Trying to identify
something as simple as run every 6 hours for the next three days is a rather complex setup. With that, after adding
a repat, please double check the setup to make sure that the code has done the repeat type that you are looking to
accomplish.

Repeats are declared with the `repeat:` key value and the value has a specific syntax as follows:
  `YYYYMMDD(HH)? to YYYYMMDD(HH)? (by DD:HH:MM)?`
where the items in the `()?` are optional.

The first value is the start time specified in year, month, day with a hour value as optional. The second value
is the end date in year, month, day format with an hour as an optional value. The third is the increment time in
day, hour and minute format. The day is optional as well in third value. It can be read as starting at the first
value, repeat until the second value is reached and increment by the third value. If no third value is specified
increment by 1 hour.

The value `2022032400 to 2022042400` is valid as is the value `2022032400 to 2022042400 by 18:00`.

* If the repeat string has the start and end dates on the same day, just a `time` string with a `date` option will
be used.
* If the repeat string has the start and end on different days but within a 24 hour window, there will be a start
date with a repeats and a time string added to the definition file.
* If the repeat spans multiple days, it requires a combination of time, date and crons in the definition file.

To elaborate on the example below of `2022032400 to 2022042400 by 18:00`. That will be read as starting at 00Z on
March 24th 2022, run every 18 hours until April 24th 2022. This will be reflected in the definition file with a
`date` value of March 24th, `24.3.2022` to start, a `time` value of `00:00` indicating start, a relative `time`
value of `+18:00` to indicate that after running and waiting 18 hours, run again, and a `repeat` value
to indicate that this needs to happen 42 times to get to April 24th.

* Example
```YAML
suites:
  prod00:
    nodes:
      gfs:
        repeat: "2022032400 to 2022042400 by 18:00"
```

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    repeat integer RUN 1 42
    time 00:00
    time +18:00
    date 24.3.2022
  endfamily
endsuite
# enddef
```

## Ranges and Lists

If you need to have multiple forecast hours or have a similar node object with just a few characters difference, the
concept of ranges and lists will be very useful in this situation. Families, tasks, or even triggers and events can
have ranges or lists associated with them to shorten the creation of the definition YAML. The goal is to have one
line that can create multiple suites, familes, or tasks or even events.

A range is a basic counting structure that follows the [Python range](https://docs.python.org/3.3/library/stdtypes.html?highlight=range#range) class object format. It is specified in one of the following three formats:
* `( $MAX_VALUE )`
* `( $START_VALUE, $MAX_VALUE )`
* `( $START_VALUE, $MAX_VALUE, $STEP )`

As you can see from the examples, if only one value is specified then it uses that as the max value, if two, then a
start and end, and three includes an increment. It uses default values of 0 for the start value and 1 for the increment
if nothing else is specified.

### Range Hierarchy

The code also uses a heirarchy structure so that range values can be passed down to child node objects but also allowed
to modify them slightly. To use a parent counter, use the same notation as the list or range but do not put any values in
the notation. So if there is a range of `(4)` for a parent node and the child node has the notation `( )` in it then when
the parent node uses the value `1`, so will the child node. An example of this would be that if a parent node has a
string value like `jgfs_atmos_post_f( 4 )` there will be 4 objects  created in the definition file,
`jgfs_atmos_post_f000`, `jgfs_atmos_post_f001`, `jgfs_atmos_post_f002`, `jgfs_atmos_post_f003`.
Then if that task has an edit that reads `FHR: 'f( )'` then the node `jgfs_atmos_post_f001` will have an edit that
reads `FHR: f001` and so on.

If there is no maximum value as well, you can also modify the increment or start values. In the same example from
above if `jgfs_atmos_post_f( 4 )` is the node definition but you wanted the edit value to start at 1 instead of
0, then using `FHRGRP: '( 1, )'` which uses 1 as the start value but as you can see has no max value, will set the
value of the edit in node `jgfs_atmos_post_f001` to `FHRGRP: 002`. Similar can also be done for something like
the incremenet value so if the edit was specified as `FHRGRP: '( ,,6 )'` the value for the edit in node
`jgfs_atmos_post_f001` would be set to `FHRGRP: 006` because it would incrememnt by 6 but still use the same
parent counter for the base since no start or max value was specified.

* Example
```YAML
suites:
  prod00:
    nodes:
      gfs:
        atmos:
          post:
            tasks:
              jgfs_atmos_post_manager:
                events:
                  - "release_postanl"
                  - "release_post( 4 )"
              jgfs_atmos_post_f( 4 ):
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

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family post
        task jgfs_atmos_post_manager
          event release_postanl
          event release_post000
          event release_post001
          event release_post002
          event release_post003
        task jgfs_atmos_post_f000
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post000
          edit FHRGRP '001'
          edit FHRLST 'f000'
          edit FHR 'f000'
          edit HR '000'
        task jgfs_atmos_post_f001
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post001
          edit FHRGRP '002'
          edit FHRLST 'f001'
          edit FHR 'f001'
          edit HR '001'
        task jgfs_atmos_post_f002
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post002
          edit FHRGRP '003'
          edit FHRLST 'f002'
          edit FHR 'f002'
          edit HR '002'
        task jgfs_atmos_post_f003
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post003
          edit FHRGRP '004'
          edit FHRLST 'f003'
          edit FHR 'f003'
          edit HR '003'
      endfamily
    endfamily
  endfamily
endsuite
# enddef
```

Lists are similar to the ranges but use the `[ ]` bracket syntax. Items in the list can be of any type and will run
the same way as ranges. The list cane be either within a string or just a list format for YAML and both should be
interpreted by the generator properly.

**NOTE:** Lists will also match ranges if they are equal in length. So if you have a range of four and a list of
four, when the first element of the range is used, the first element of the list is also used.

**NOTE:** Lists do not inheret the parent values directly. They read the position but given the flexibility needed
it does not pass down the actual values. The code could be updated to do that easily if it turns out to be a
future need but due to potential conflicts, it was not set that way in this version.

* Example
```YAML
suites:
  prod00:
    nodes:
      gfs:
        atmos:
          post:
            tasks:
              jgfs_atmos_post_manager:
                events:
                  - "release_postanl"
                  - "release_post( 4 )"
              jgfs_atmos_post_f[000,001,002,003]:
                template: jgfs_atmos_post_master
                triggers:
                - task: jgfs_atmos_post_manager
                  event: release_post[000,001,002,003]
                edits:
                  FHRGRP: [ 'a', 'b', 'c', 'd' ]
                  HR: [1,2,3,4]
```

* Result
```bash
#5.8.1
suite prod00
  edit ECF_HOME '/usr1/knevins/global-workflow/ecf'
  edit ECF_INCLUDE '/usr1/knevins/global-workflow/ecf'
  edit ACCOUNT 'fv3-cpu'
  edit QUEUE 'PARTITION_BATCH'
  edit MACHINE 'HERA'
  edit RUN_ENVIR 'emc'
  family gfs
    family atmos
      family post
        task jgfs_atmos_post_manager
          event release_postanl
          event release_post000
          event release_post001
          event release_post002
          event release_post003
        task jgfs_atmos_post_f000
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post000
          edit FHRGRP 'a'
          edit HR '001'
        task jgfs_atmos_post_f001
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post001
          edit FHRGRP 'b'
          edit HR '002'
        task jgfs_atmos_post_f002
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post002
          edit FHRGRP 'c'
          edit HR '003'
        task jgfs_atmos_post_f003
          trigger /prod00/gfs/atmos/post/jgfs_atmos_post_manager:release_post003
          edit FHRGRP 'd'
          edit HR '004'
      endfamily
    endfamily
  endfamily
endsuite
# enddef

```

# Running the Utility

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

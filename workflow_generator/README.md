# Workflow Setup Utility:

## Setup for using the utility
1. Follow the guidance to have the experiment directory setup with configuration directories.
As 3/30/2022 the only config file that is imported is the config.base.
1. Ensure the following item is configured in the config.base file:
`export ECFgfs=$HOMEgfs/ecf`
1. If running locally, activate your conda ecflow environment
`conda activate ecflow`
1. If this is the first time running the following Python modules need to be
```
pip3 install numpy
pip3 install PyYAML
```

## Setup the yaml
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

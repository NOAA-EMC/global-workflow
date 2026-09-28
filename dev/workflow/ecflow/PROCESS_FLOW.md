# ecFlow C48_ATM Test — Complete Process Flow

## Step 0: Start the ecFlow Server (one-time)

```
User on Ursa login node (e.g. ufe01)
│
├── ssh uecflow01                    # server runs on the dedicated ecFlow node
│
│   On uecflow01:
│   ├── module load ecflow
│   ├── export ECF_PORT=$(( $(id -u) + 1500 ))
│   ├── export ECF_HOME=/scratch3/NCEPDEV/global/$USER/ecflow
│   ├── mkdir -p ${ECF_HOME}
│   ├── ecflow_start.sh -p ${ECF_PORT} -d ${ECF_HOME}
│   │     └── starts ecflow_server daemon on uecflow01:${ECF_PORT}
│   └── exit                         # back to login node
│
│   Back on login node (ufe01):
├── module load ecflow
├── unset ECF_HOSTFILE
├── export ECF_HOST=uecflow01        # point client to the ecFlow node
├── export ECF_PORT=$(( $(id -u) + 1500 ))
├── export ECF_HOME=/scratch3/NCEPDEV/global/$USER/ecflow
├── export HOMEglobal=/scratch3/NCEPDEV/global/$USER/global-workflow
├── export MACHINE_ID=URSA           # workaround for ufe node detection
├── export HPC_ACCOUNT=fv3-cpu       # Slurm account for job submission
│
└── ecflow_client --ping
      └── "ping server(uecflow01:24385) succeeded"
```

If the server has stopped (node reboot, etc.):

```
├── ssh uecflow01
│   ├── module load ecflow
│   ├── export ECF_PORT=$(( $(id -u) + 1500 ))
│   ├── export ECF_HOME=/scratch3/NCEPDEV/global/$USER/ecflow
│   ├── ps -u $USER -f | grep ecflow_server   # check if running
│   ├── ecflow_start.sh -p ${ECF_PORT} -d ${ECF_HOME}   # restart
│   │     └── restores state from checkpoint — suites reappear
│   └── exit
│
└── ecflow_client --ping             # verify from login node
```

## Step 1: Launch the Test Case

```
$ python3 dev/workflow/ecflow/c48_atm_ecflow.py

c48_atm_ecflow.py                              ← entry point
│   (dev/workflow/ecflow/c48_atm_ecflow.py)
│
├── sets default YAML = dev/ci/cases/pr/C48_ATM.yaml
│
└── calls load_ecflow_case.run(default_yaml)
      │   (dev/workflow/ecflow/load_ecflow_case.py)
      │
      ├── parse_args()
      │     reads --yaml, --pslot, --expdir, --comroot, --stmp, --overwrite
      │
      ├── validate_environment()
      │     checks ECF_HOST, ECF_PORT, ECF_HOME, HOMEglobal are set
      │
      ├── load_case_yaml(yaml_path)
      │     │  parses C48_ATM.yaml with Jinja2
      │     │  resolves: net=gfs, mode=forecast-only, app=ATM,
      │     │            resdetatmos=48, idate=2021032312
      │     └── returns testconf AttrDict
      │
      ├── [Step 0/4] cleanup_stale_files()
      │     removes old EXPDIR, COMROOT, RUNDIRS for this pslot
      │
      ├── [Step 1/4] create_experiment(testconf)
      │     │
      │     └── calls setup_expt.main(["gfs", "forecast-only", ...])
      │           │   (dev/workflow/setup_expt.py)
      │           │
      │           ├── detects machine via hosts.py → Host()
      │           │     reads dev/workflow/hosts/ursa.yaml
      │           │     → PARTITION_BATCH=u1-compute, STMP, COMROOT, etc.
      │           │
      │           ├── renders Jinja2 config templates into EXPDIR/<pslot>/
      │           │     config.base.j2  → config.base
      │           │     config.fcst.j2  → config.fcst
      │           │     config.ufs.j2   → config.ufs
      │           │     ... (~30 config files)
      │           │
      │           └── copies config.resources + config.resources.URSA
      │
      │     output: RUNTESTS/EXPDIR/my_C48_test/
      │
      ├── [Step 2/4] generate_ecflow_def(expdir)
      │     │
      │     └── calls setup_workflow.main([expdir, "ecflow"])
      │           │   (dev/workflow/setup_workflow.py)
      │           │
      │           ├── creates AppConfig("gfs", "forecast-only")
      │           │     │   (dev/workflow/applications/applications.py)
      │           │     ├── sources all config files via Configuration.parse_config()
      │           │     │     config.base → config.fcst → config.resources → ...
      │           │     │     each config sourced in bash subshell, vars captured as Python dict
      │           │     └── resolves task_names list:
      │           │           [stage_ic, fcst, atmos_prod, tracker, genesis, arch_vrfy, cleanup]
      │           │
      │           ├── ecflow_suite_factory.create("gfs_forecast-only", app_config, ecflow_config)
      │           │     │   (dev/workflow/ecflow/ecflow_suite_factory.py)
      │           │     └── returns GFSForecastOnlyEcFlowSuite instance
      │           │
      │           └── suite.write()
      │                 │   (dev/workflow/ecflow/gfs_forecast_only_ecflow.py)
      │                 │
      │                 ├── __init__():
      │                 │     ecflow_tasks_factory.create("gfs", app_config, "gfs")
      │                 │       (dev/workflow/ecflow/ecflow_tasks_factory.py)
      │                 │       → GFSEcFlowTasks instance
      │                 │           (dev/workflow/ecflow/gfs_ecflow_tasks.py)
      │                 │
      │                 ├── builds .def lines:
      │                 │     suite my_C48_test
      │                 │       edit ECF_HOME '...'
      │                 │       edit ECF_JOB_CMD 'sbatch %ECF_JOB%'
      │                 │       edit EXPDIR '...'
      │                 │       ...
      │                 │       family 2021032312
      │                 │         edit PDY '20210323'
      │                 │         edit CYC '12'
      │                 │         family gfs
      │                 │           edit RUN 'gfs'
      │                 │
      │                 ├── for each task in [stage_ic, fcst, atmos_prod, ...]:
      │                 │     task_dict = GFSEcFlowTasks.get_ecflow_task(task_name)
      │                 │       │   dispatches to e.g. self.fcst()
      │                 │       │   calls self._simple_task() or self._product_task()
      │                 │       │     (dev/workflow/ecflow/ecflow_tasks.py)
      │                 │       │   calls Tasks.get_resource() from parent class
      │                 │       │     (dev/workflow/rocoto/tasks.py — inherited)
      │                 │       └── returns dict:
      │                 │             { task_name, jjob, step, trigger, resources, ... }
      │                 │
      │                 │     _emit_simple_task(task_dict) or _emit_product_family(task_dict)
      │                 │       → .def lines with edit STEP, trigger expressions
      │                 │       → NO resource edits (#SBATCH baked into .ecf at copy time)
      │                 │
      │                 ├── writes EXPDIR/my_C48_test.def
      │                 │
      │                 ├── creates ECF_HOME/my_C48_test/2021032312/gfs/ directories
      │                 │     (so ecflow_server can write .job files)
      │                 │
      │                 └── _create_ecf_scripts()
      │                       creates {EXPDIR}/ecf_scripts/include/ + scripts/{category}/
      │                       copies dev/ecflow/include/ → include/
      │                       copies dev/ecflow/scripts/{category}/*.ecf → scripts/{category}/
      │                       injects resolved #SBATCH directives after #!/bin/bash
      │                       product family children get copies of parent .ecf
      │                       writes ecf_scripts.manifest
      │
      ├── [Step 3/4] load_suite(suite_name, def_file)
      │     │
      │     ├── ecflow_client --ping         (verify server alive on uecflow01)
      │     ├── ecflow_client --delete=force  (remove old suite if exists)
      │     └── ecflow_client --load=<def>   (load .def into server)
      │
      └── prints:
            "Suite loaded but NOT started."
            "ecflow_client --begin=my_C48_test"
```

## Step 2: Begin the Suite

```
$ ecflow_client --begin=my_C48_test

ecflow_server (on uecflow01)
│
└── walks the suite tree, finds tasks with satisfied triggers
```

## Step 3: Task Submission (per task, repeated for each)

```
ecflow_server (on uecflow01) finds:
  /my_C48_test/2021032312/gfs/stage_ic (triggers satisfied)
│
├── reads EXPDIR/ecf_scripts/stage_ic.ecf
│     %include <head.h>      → ecf_scripts/include/head.h
│     %include <envir.h>     → ecf_scripts/include/envir.h
│     export HOMEglobal=%HOMEglobal%
│     export PDY=%PDY%
│     ...
│     %include <tail.h>      → ecf_scripts/include/tail.h
│
├── preprocesses: replaces all %VAR% with .def values
│     %HOMEglobal% → /scratch3/.../global-workflow
│     %PDY%        → 20210323
│     %CYC%        → 12
│     %STEP%       → stage_ic
│     %ECF_NAME%   → /my_C48_test/2021032312/gfs/stage_ic
│     ...
│
├── writes: ECF_HOME/my_C48_test/2021032312/gfs/stage_ic.job1
│     (pure bash — no macros left)
│
└── runs ECF_JOB_CMD (from uecflow01):
      sbatch stage_ic.job1
      │
      │   stage_ic.job1 contains #SBATCH directives (baked at config time)
      │   with resources resolved from config.resources:
      │     --job-name=gfs_stage_ic_12
      │     --account=fv3-cpu
      │     --partition=u1-compute
      │     --time=00:15:00
      │     --nodes=1
      │     --ntasks-per-node=1
      │     --output=<jobout_path>
      │     --export=NONE
      │
      └── Slurm responds: "Submitted batch job 1618091"
          ecFlow captures 1618091 as ECF_RID
```

## Step 4: Job Execution (on Slurm compute node)

```
Slurm allocates compute node, runs stage_ic.job1:
│
├── #!/bin/bash                              (from .ecf shebang)
├── #SBATCH directives                       (baked into .ecf at config time)
├── date; hostname
├── module load ecflow
├── export ECF_NAME, ECF_HOST, ECF_PORT, ECF_PASS, ECF_TRYNO
├── ecflow_client --init=$ECF_RID            ← tells server: "I'm running"
├── trap ERROR on ERR/EXIT
│
├── export HOMEglobal=...                    (from stage_ic.ecf body)
├── export EXPDIR=..., COMROOT=..., PDY=..., cyc=...
├── source load_modules.sh
│
├── dev/jobs/JGLOBAL_STAGE_IC                ← the J-Job
│     │
│     ├── sources jjob_header.sh
│     ├── sources config.base, config.stage_ic
│     ├── sets up ROTDIR, DATA directories
│     │
│     └── calls dev/scripts/exglobal_stage_ic.sh
│           │   the actual work:
│           └── copies/links initial condition files
│                 from icsdir → ROTDIR/gfs.20210323/12/
│
├── (on success)
│     module load ecflow
│     ecflow_client --complete               ← tells server on uecflow01: "I'm done"
│
└── (on failure)
      ecflow_client --abort="error message"  ← tells server on uecflow01: "I failed"
```

## Step 5: Trigger Chain Continues

```
ecflow_server (on uecflow01) receives --complete for stage_ic
│
├── evaluates triggers for all tasks:
│     fcst: "stage_ic == complete" → YES → submit fcst
│     atmos_prod: "fcst == complete" → NO → wait
│     ...
│
├── submits fcst via same ECF_JOB_CMD → sbatch
│     (fcst .def edits carry the correct node count from config.resources)
│
│   ... fcst completes ...
│
├── atmos_prod family: "fcst == complete" → YES
│     submits all fhr children in parallel:
│       f000_f003, f004_f007, ..., f117_f120
│     each inherits STEP=atmos_products from family
│     each gets its own FHR_LIST edit variable
│
│   ... all atmos_prod children complete ...
│
├── tracker: "atmos_prod == complete" → YES → submit
├── genesis: "atmos_prod == complete" → YES → submit (parallel with tracker)
│
│   ... tracker + genesis complete ...
│
├── arch_vrfy: "atmos_prod == complete and tracker == complete
│               and genesis == complete" → YES → submit
│
│   ... arch_vrfy completes ...
│
└── cleanup: "arch_vrfy == complete" → YES → submit
      └── removes RUNDIRS temporary data
          suite complete
```

## Step 6: Monitor and Troubleshoot

```
# All client commands run from any login node (ufe01, etc.)
# with ECF_HOST=uecflow01

$ ecflow_client --get_state /my_C48_test
  /my_C48_test {state:active}
    /2021032312 {state:active}
      /gfs {state:active}
        /stage_ic {state:complete}
        /fcst {state:complete}
        /atmos_prod {state:active}
          /f000_f003 {state:complete}
          /f004_f007 {state:active}     ← currently running
          /f008_f011 {state:queued}     ← waiting for resources
          ...

$ ecflow_client --get_state /my_C48_test/2021032312/gfs/fcst
  /fcst {state:complete}

# View job output
$ cat $ECF_HOME/my_C48_test/2021032312/gfs/fcst.1

# Kill a running task
$ ecflow_client --kill /my_C48_test/2021032312/gfs/fcst
  → runs ECF_KILL_CMD = scancel <ECF_RID>

# Requeue a failed task
$ ecflow_client --force=set /my_C48_test/2021032312/gfs/fcst queued

# GUI (needs X11 forwarding: ssh -X)
$ ecflow_ui &

# Delete and start over
$ ecflow_client --delete=force yes /my_C48_test
$ python3 dev/workflow/ecflow/c48_atm_ecflow.py
```

# Running Global Workflow with ecFlow

This guide walks through running the C48_ATM forecast-only test case
using ecFlow on Ursa. The same steps apply to other cases and platforms
with minor path adjustments.

> **Note:** This guide is Ursa-specific. Paths (e.g. `/scratch3/NCEPDEV/global/${USER}/`),
> hostnames (e.g. `uecflow01`), and Slurm settings (e.g. `fv3-cpu`, `u1-compute`) below
> are examples for Ursa; portability to other platforms is not yet supported.

## Prerequisites

- NOAA RDHPCS account with Ursa access
- PuTTY (Windows) or SSH client (Mac/Linux)
- Global-workflow repo checked out and built on Ursa
- Slurm scheduler access for job submission

## 0. Connecting to Ursa

### From Windows (PuTTY)

1. Open PuTTY
2. Enter the hostname: `ursa.rdhpcs.noaa.gov`
3. Port: `22`
4. Connection type: SSH
5. Click **Open**
6. Login with your RDHPCS username and RSA token + PIN

To save this for future use:
- In the **Session** panel, type a name (e.g. `Ursa`) in
  "Saved Sessions" and click **Save**
- Next time, double-click the saved session to connect

### From Mac/Linux (terminal)

```bash
ssh <username>@ursa.rdhpcs.noaa.gov
```

### Forwarding X11 for ecflow_ui (GUI)

ecflow_ui requires X11 forwarding to display the GUI on your
local machine.

**PuTTY:** Go to Connection → SSH → X11 → check "Enable X11
forwarding". You also need an X server running locally
(install [VcXsrv](https://sourceforge.net/projects/vcxsrv/)
or [Xming](https://sourceforge.net/projects/xming/) on Windows).

**Mac/Linux:**
```bash
ssh -X <username>@ursa.rdhpcs.noaa.gov
```

Verify X11 works after logging in:
```bash
xterm &    # a small terminal window should appear on your screen
```

### After logging in

```bash
# Check you are on Ursa
hostname
# Expected: ufe01 or similar

# Navigate to your workspace
cd /scratch3/NCEPDEV/global/${USER}
```

## 1. Environment Setup

Set these variables before running any ecFlow scripts. Add them to
your `~/.bashrc` or source them in each session.

```bash
# ── Step 1a: Load the ecFlow module ──────────────────────────────
module load ecflow

# Remove any stale host file that might redirect ecflow_client
unset ECF_HOSTFILE

# ── Step 1b: Set the global-workflow repo path ───────────────────
# Point this to wherever you cloned the repo.  The loader script
# auto-detects HOMEglobal from its own location, but ecFlow tasks
# read it from the environment during validation.
export HOMEglobal=/scratch3/NCEPDEV/global/${USER}/global-workflow

# ── Step 1b-workaround: Machine detection on ufe nodes ───────────
# The mount-based auto-detection in hosts.py may misidentify some
# Ursa front-end nodes (e.g. ufe12) as Hera.  If you hit unexpected
# platform errors, force the machine identity:
export MACHINE_ID=URSA

# ── Step 1c: Choose an ecFlow server ─────────────────────────────
# Each user runs their own ecFlow server on a unique port.
# Use your UID offset by 1500 to avoid collisions with other users:
export ECF_PORT=$(( $(id -u) + 1500 ))
export ECF_HOST=uecflow01
echo "Your ecFlow server: ${ECF_HOST}:${ECF_PORT}"

# ── Step 1d: Set the ecFlow job directory ────────────────────────
# This is where ecFlow writes .job files and captures .jobout output.
# Create it if it doesn't exist.
export ECF_HOME=/scratch3/NCEPDEV/global/${USER}/ecflow
mkdir -p "${ECF_HOME}"
```

Verify everything is set:
```bash
echo "ECF_HOST   = ${ECF_HOST}"
echo "ECF_PORT   = ${ECF_PORT}"
echo "ECF_HOME   = ${ECF_HOME}"
echo "HOMEglobal = ${HOMEglobal}"
ecflow_client --ping   # should say "ping ... succeeded"
```

If `ecflow_client --ping` fails, go to [section 2](#2-ecflow-server)
to start the server first, then come back here.

## 2. ecFlow Server

### Check if a server is already running

From any login node, check if your server on `uecflow01` is alive:

```bash
export ECF_HOST=uecflow01
export ECF_PORT=$(( $(id -u) + 1500 ))
ecflow_client --ping
```

If it responds with `ping server(...) succeeded`, skip to step 3.
If it fails, start or restart the server (see below).

### Start the server (first time)

The ecFlow server must run on the dedicated node `uecflow01`.

```bash
# 1. SSH into the ecFlow node
ssh uecflow01

# 2. Load ecflow and set your port
module load ecflow
export ECF_PORT=$(( $(id -u) + 1500 ))

# 3. Create the job directory
export ECF_HOME=/scratch3/NCEPDEV/global/${USER}/ecflow
mkdir -p "${ECF_HOME}"

# 4. Start the server
ecflow_start.sh -p ${ECF_PORT} -d ${ECF_HOME}

# 5. Verify
export ECF_HOST=uecflow01
ecflow_client --ping

# 6. Exit back to your login node — the server keeps running
exit
```

Your port is always `$(id -u) + 1500` — deterministic for your user,
no need to remember it.

### Restart a stopped server

If the server was killed (node reboot, timeout, etc.), SSH back
to `uecflow01` and restart it. Your suites and checkpoints are
preserved in `ECF_HOME`.

```bash
ssh uecflow01
module load ecflow
export ECF_PORT=$(( $(id -u) + 1500 ))
export ECF_HOME=/scratch3/NCEPDEV/global/${USER}/ecflow

# Check if it's still running
ps -u ${USER} -f | grep ecflow_server

# If not running, restart
ecflow_start.sh -p ${ECF_PORT} -d ${ECF_HOME}

# The server restores state from its checkpoint file.
# Previously loaded suites reappear with their last known state.
ecflow_client --ping
exit
```

If a suite was mid-run when the server died, tasks that were
`active` will show as `aborted` after restart. Requeue them:

```bash
ecflow_client --force=set /<suite>/<path_to_task> queued
```

### Stop the server (when completely done)

```bash
ecflow_client --halt=yes       # stop scheduling
ecflow_client --check_pt       # save server state
ecflow_client --terminate=yes  # shut down the server process
```

## Day-to-Day Operations (Quick Reference)

Once steps 0–2 are done, this is all you need each day.

### Starting a new session

```bash
# 1. SSH into Ursa
ssh -X <username>@ursa.rdhpcs.noaa.gov

# 2. Source your environment (or add to ~/.bashrc once)
module load ecflow
unset ECF_HOSTFILE
export ECF_PORT=$(( $(id -u) + 1500 ))
export ECF_HOST=uecflow01
export ECF_HOME=/scratch3/NCEPDEV/global/${USER}/ecflow
export HOMEglobal=/scratch3/NCEPDEV/global/${USER}/global-workflow  # adjust to your clone path
export MACHINE_ID=URSA  # prevent misdetection on ufe nodes

# 3. Verify the server is alive (must have been started on uecflow01)
ecflow_client --ping
```

If the ping fails, start the server per
[section 2](#2-ecflow-server) before continuing.

### Run a case

```bash
cd ${HOMEglobal}

# Quick start (all defaults)
python3 dev/workflow/ecflow/c48_atm_ecflow.py

# Or with custom paths
python3 dev/workflow/ecflow/c48_atm_ecflow.py \
    --pslot my_C48_test \
    --comroot /scratch4/NCEPDEV/stmp/${USER}/COMROOT \
    --expdir /scratch3/NCEPDEV/global/${USER}/EXPDIR \
    --stmp /scratch4/NCEPDEV/stmp/${USER}
```

Answer `y` to the cleanup and delete prompts. The suite starts
automatically. Monitor with:

```bash
ecflow_ui &                    # GUI (needs X11 forwarding)
# or
ecflow_client --get_state /C48_ATM_ecflow   # CLI
```

### Check task progress

```bash
# See all tasks and their states
ecflow_client --get_state /C48_ATM_ecflow

# Check if a specific task is done
ecflow_client --get_state /C48_ATM_ecflow/gfs/2021032312/fcst
```

### If a task fails (red/aborted)

```bash
# 1. Check the job output for the error
cat ${ECF_HOME}/<task_name>.1

# 2. Fix the issue (edit .ecf, fix paths, etc.)

# 3. Rerun the task
ecflow_client --force=queued /C48_ATM_ecflow/gfs/2021032312/<task_name>
```

### Rerun the whole suite from scratch

```bash
python3 dev/workflow/ecflow/c48_atm_ecflow.py --overwrite
```

### Clean up after a run

```bash
# Delete the suite from the server
ecflow_client --suspend /C48_ATM_ecflow
ecflow_client --kill /C48_ATM_ecflow
sleep 5
ecflow_client --delete=force yes /C48_ATM_ecflow

# Optionally remove the experiment directories
rm -rf ${RUNTESTS}/EXPDIR/C48_ATM_ecflow
rm -rf ${RUNTESTS}/COMROOT/C48_ATM_ecflow
rm -rf ${STMP}/RUNDIRS/C48_ATM_ecflow
```

### Update .ecf scripts after editing (without regenerating)

```bash
bash dev/workflow/ecflow/sync_ecf_scripts.sh \
    ${RUNTESTS}/EXPDIR/C48_ATM_ecflow/ecf_scripts
```

### End of day

The ecFlow server persists across sessions. You can log out and
come back tomorrow — the suite continues running (or waiting) on
the server. Just re-source your environment variables when you
reconnect.

## 3. Run the C48_ATM Case

### Quick start (all defaults)

```bash
cd ${HOMEglobal}
python3 dev/workflow/ecflow/c48_atm_ecflow.py
```

This will:
1. Clean up any previous run directories (with prompt)
2. Create the experiment via `setup_expt`
3. Generate the `.def` file and copy `.ecf` scripts
4. Load the suite into the ecFlow server (with prompt if it already exists)

The suite is loaded but **not started**. The script prints the
`ecflow_client --begin` command to run when you are ready.

### With custom paths

```bash
python3 dev/workflow/ecflow/c48_atm_ecflow.py \
    --pslot my_C48_test \
    --comroot /scratch4/NCEPDEV/stmp/${USER}/COMROOT \
    --expdir /scratch3/NCEPDEV/global/${USER}/EXPDIR \
    --stmp /scratch4/NCEPDEV/stmp/${USER}
```

### CLI options

| Option | Description |
|--------|-------------|
| `--yaml PATH` | Override the case YAML file |
| `--pslot NAME` | Override experiment name (default: `C48_ATM_ecflow`) |
| `--comroot PATH` | Override output data directory |
| `--expdir PATH` | Override experiment config directory |
| `--stmp PATH` | Override runtime scratch directory |
| `--suite-name NAME` | Override ecFlow suite name |
| `--overwrite` | Overwrite a previously created experiment |

## 4. Monitoring

### ecflow_ui (GUI)

```bash
ecflow_ui &
```

Connect to `${ECF_HOST}:${ECF_PORT}`. The suite tree shows task
states: queued (blue), submitted (cyan), active (green),
complete (yellow), aborted (red).

### Command line

```bash
# Suite status overview
ecflow_client --get_state /C48_ATM_ecflow

# Watch a specific task
ecflow_client --get_state /C48_ATM_ecflow/gfs/2021032312/fcst

# View job output for a task
cat ${ECF_HOME}/fcst.1    # .1 = first try number
```

### Log files

Task logs are written to `{ROTDIR}/logs/`:
```bash
ls ${COMROOT}/C48_ATM_ecflow/logs/
```

## 5. Common Operations

### Rerun a failed task

```bash
# In ecflow_ui: right-click task → Rerun
# Or from CLI:
ecflow_client --force=queued /C48_ATM_ecflow/gfs/2021032312/fcst
```

### Resume a partial run (keep previous output)

If a run failed partway through and you want to reuse the existing
output data (COMROOT, RUNDIRS) rather than starting from scratch:

**CLI approach:**

```bash
# 1. Reload the .def — say "no" to cleaning EXPDIR/COMROOT
#    so previous output is preserved, then "yes" to replace the suite
python3 dev/workflow/ecflow/c48_atm_ecflow.py

# 2. Suspend the suite before beginning so nothing auto-runs
ecflow_client --suspend /C48_ATM_ecflow

# 3. Begin the suite (all tasks go to "queued" but stay held)
ecflow_client --begin=C48_ATM_ecflow

# 4. Mark tasks that already completed successfully
ecflow_client --force=complete /C48_ATM_ecflow/gfs/2021032312/init/stage_ic
ecflow_client --force=complete /C48_ATM_ecflow/gfs/2021032312/forecast/fcst
# ... repeat for each task that finished in the previous run

# 5. Resume — remaining tasks will run based on their triggers
ecflow_client --resume /C48_ATM_ecflow
```

**ecflow_ui approach:**

1. Load the `.def` as above (say "no" to cleanup)
2. In ecflow_ui, right-click the suite → **Suspend**
3. Click **Begin** on the suite
4. For each task that already completed: right-click →
   **Force** → **Complete**
5. Right-click the suite → **Resume**

The CLI and ecflow_ui approaches can be combined — for example,
load and begin from the command line, then mark completed tasks
in the GUI where the suite tree makes it easier to see what ran.
The trigger logic will pick up from where the previous run left
off, running only the tasks whose dependencies are now satisfied.

### Suspend / resume the suite

```bash
ecflow_client --suspend /C48_ATM_ecflow
ecflow_client --resume /C48_ATM_ecflow
```

### Delete the suite

```bash
ecflow_client --suspend /C48_ATM_ecflow
ecflow_client --kill /C48_ATM_ecflow
sleep 5
ecflow_client --delete=force yes /C48_ATM_ecflow
```

### Cancel Slurm jobs

```bash
# Cancel a specific job by ID (get the ID from squeue)
squeue -u ${USER}
scancel <job_id>

# Cancel all your running jobs at once
scancel -u ${USER}
```

### Update .ecf scripts without regenerating the .def

```bash
bash dev/workflow/ecflow/sync_ecf_scripts.sh \
    ${EXPDIR}/C48_ATM_ecflow/ecf_scripts
```

### Regenerate the .def from scratch

```bash
python3 dev/workflow/ecflow/c48_atm_ecflow.py --overwrite
```

## 6. Troubleshooting

### "Missing environment variables"

```
[ERROR] Missing environment variables: ECF_HOST, ECF_PORT, ECF_HOME
```

**Fix:** Source the environment variables from step 1. Make sure
`module load ecflow` has been run and `unset ECF_HOSTFILE` is set.

### "Cannot reach ecFlow server"

```
[ERROR] Cannot reach ecFlow server at uecflow01:23385
```

**Fix:** Check that the server is running (`ecflow_client --ping`).
If using a shared server, verify the hostname and port. If running
your own, start it with `ecflow_start.sh`.

### Suite delete times out

```
[WARN] Delete failed: timeout
```

**Fix:** The suite has active jobs that are blocking the delete.
Kill them manually first:
```bash
ecflow_client --suspend /C48_ATM_ecflow
ecflow_client --kill /C48_ATM_ecflow
sleep 20
ecflow_client --delete=force yes /C48_ATM_ecflow
```

### "ecflow_client --load" fails

```
[ERROR] ecflow_client failed: ecflow_client --load=...
  stderr: <error message>
```

**Fix:** The `.def` file has a syntax error. Check the generated file:
```bash
cat ${EXPDIR}/C48_ATM_ecflow/C48_ATM_ecflow.def
```

Common causes:
- Task names with special characters
- Missing `endfamily` or `endsuite` closing tags
- Invalid trigger expressions

You can validate the `.def` before loading:
```bash
ecflow_client --check ${EXPDIR}/C48_ATM_ecflow/C48_ATM_ecflow.def
```

### Tasks stay in "queued" state

**Check triggers:** The task might be waiting for an upstream task.
```bash
ecflow_client --get_state /C48_ATM_ecflow/gfs/2021032312/<task_name>
```

**Check Slurm:** The job might be pending in the Slurm queue.
```bash
squeue -u ${USER}
```

### Tasks abort immediately

**Check the .ecf script exists:**
```bash
ls ${EXPDIR}/C48_ATM_ecflow/ecf_scripts/<task_name>.ecf
```

**Check the job output:**
```bash
cat ${ECF_HOME}/<task_name>.1
```

Common causes:
- `load_modules.sh` failure (missing modules)
- J-Job script not found (HOMEglobal path wrong)
- File permissions

### Jinja2 or other Python imports not found

```
ModuleNotFoundError: No module named 'jinja2'
```

**Fix:** The workflow's Python dependencies (Jinja2, PyYAML, etc.)
are provided by the build modules. Load them before running any
ecFlow or setup script:
```bash
module use ${HOMEglobal}/modulefiles
module load module_gwsetup.ursa
```

If `module_gwsetup.ursa` is not available, load the stack that was
used to build the workflow (e.g. `module load intel`, `module load
spack-stack`) — the exact modules depend on your build.

### METplus or archive tasks appear when they shouldn't

If `metp` or `arch_tars` show up in the suite despite being
disabled, check the rendered `config.base`:
```bash
grep DO_METP ${EXPDIR}/C48_ATM_ecflow/config.base
grep DO_ARCHCOM ${EXPDIR}/C48_ATM_ecflow/config.base
```

On Ursa, these should be set to `"NO"` by the platform guards
in `config.base.j2`. If they show `"YES"`, the experiment was
generated before the guards were added — regenerate with
`--overwrite`.

## 7. Directory Layout

After a successful run, the experiment produces:

```
${RUNTESTS}/
  EXPDIR/C48_ATM_ecflow/           ← experiment config
    config.base                     config files
    config.fcst
    config.atmos_products
    ...
    C48_ATM_ecflow.def              generated ecFlow definition
    ecf_scripts/                    copied .ecf files + manifest
  COMROOT/C48_ATM_ecflow/           ← output data
    gfs.20210323/12/                 forecast output
      model_data/atmos/history/      atmospheric history files
    logs/                            task log files
  RUNDIRS/C48_ATM_ecflow/           ← runtime scratch (cleaned up)
```

## 8. Architecture Overview

The ecFlow engine mirrors the Rocoto architecture:

```
Entry point:      c48_atm_ecflow.py
                       │
Orchestrator:     load_ecflow_case.run()
                       │
                  ┌────┴────┐
                  │         │
Experiment:  setup_expt  setup_workflow ──► ecflow_suite_factory
                              │
Task defs:   ecflow_tasks_factory ──► GFSEcFlowTasks
                              │          (one method per task)
Suite gen:   GFSForecastOnlyEcFlowSuite.write()
                              │
Output:      {pslot}.def + ecf_scripts/
                              │
Server:      ecflow_client --load / --begin
                              │
Execution:   .ecf scripts ──► #SBATCH + head.h + envir.h + J-Job + tail.h
                              │
Submission:  sbatch runs the preprocessed .job (#SBATCH directives baked into .ecf)
```

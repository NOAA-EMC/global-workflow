# Requirements Document

## Introduction

This feature modernizes the NOAA EMC global-workflow into an immutable, DAG-based orchestration system that produces a self-contained, versioned deployment artifact suitable for delivery to NCEP Central Operations (NCO).

The system is built around six guiding architectural principles:

1. **Directed Acyclic Graph (DAG) Orchestration** — every cycle resolves to a finite, acyclic graph of tasks where edges encode true data and event dependencies. The DAG is expressed as an ecFlow suite/family/task definition.
2. **Immutable Deployment (Snapshot Paradigm)** — each experiment or production tag produces a versioned EXPDIR that contains every script, parameter file, fix-file pointer, environment file, ecFlow definition, and rendered configuration the workflow needs to run; once published, the snapshot is read-only.
3. **Declarative Configuration & Deployment-Time Templating with Nested Includes** — the source of truth is YAML with Jinja2 templating (including nested includes and inheritance); all templating is resolved at deployment time, not at run time, so production jobs read pure, fully-rendered files.
4. **Ephemeral Execution & Idempotency** — each task runs in a clean, disposable working directory under `${DATAROOT}`, derives its outputs purely from declared inputs, and can be re-run safely without producing duplicate or corrupted products.
5. **Universal Wrappers (DRY Principle)** — a single set of wrapper utilities (`Universal_Wrapper`) provides scheduler abstraction, environment setup, error checking, and logging for every job, eliminating per-component boilerplate.
6. **Atomic Delivery** — products are published to `${COMOUT}` only after all artifacts of a deliverable set are written and verified, using stage-and-rename or equivalent file-system atomicity guarantees.

In addition to these principles, the user has set an explicit, non-negotiable orchestration constraint:

> **ecFlow is the sole orchestration engine. Rocoto MUST NOT be used.**
> The DAG must be expressed as ecFlow suite/family/task definitions. Existing Rocoto-based workflow generation (`dev/workflow/rocoto/`, `dev/job_cards/rocoto/`, and the `rocoto` subparser in `dev/workflow/setup_workflow.py`) is decommissioned by this feature.

Authoring lives entirely under `dev/` (templates, J-Jobs, ex-scripts, ush utilities, parm files, env files, ecFlow suite/family/task templates, and workflow configuration YAML). Deployment renders these into the canonical NCO production layout (`jobs/`, `scripts/`, `ush/`, `parm/`, `sorc/`, `fix/`, `ecf/`, `versions/`, `modulefiles/`) inside an EXPDIR. The system uses **wxflow** (NOAA-EMC Python utilities, including `parse_j2yaml`) for YAML/Jinja2 resolution and **uwtools** (UFS Unified Workflow Tools) for the driver framework, scheduler abstractions, and file-staging APIs.

Supported HPC platforms at delivery: WCOSS2, Hera, Hercules, Orion, Gaea (C6), Derecho, Ursa, and Parallel Works clouds (AWS PW, AzurePW, GooglePW), plus the generic CONTAINER target. The list mirrors the env files currently present in `env/`.

## Glossary

- **Global_Workflow**: The top-level system being modernized.
- **EcFlow_Server**: The ecFlow daemon that owns the live suite state at runtime (`ecflow_server`). Used in production at NCO.
- **EcFlow_Suite**: The top-level ecFlow node that contains one cycle's families and tasks (for example `gfs_v17`).
- **EcFlow_Family**: An ecFlow group node that contains tasks or further families (for example `gfs/atmos/post`).
- **EcFlow_Task**: A leaf ecFlow node corresponding to a single submitted job; the scheduler entry point is the rendered `*.ecf` script.
- **Suite_Definition**: The text artifact (`*.def`) that ecFlow loads to instantiate the suite. It must be deterministically generated from the Workflow_Configuration.
- **Workflow_Configuration**: A declarative YAML document describing all tasks, their dependencies (triggers, events, completes), scheduling attributes, and cycle definitions for one application (for example `gfs_cycled`, `gefs`, `sfs`, `gcafs`).
- **DAG_Generator**: The Python component that consumes Workflow_Configuration and emits the Suite_Definition plus the per-task ecf scripts.
- **Deployment_Tool**: The component that consumes `dev/` sources plus a configuration YAML and produces a rendered, self-contained EXPDIR. Invokes the Template_Renderer and the DAG_Generator.
- **EXPDIR**: An immutable, versioned deployment directory containing all rendered artifacts required to run the workflow. Layout follows the NCO production convention (`jobs/`, `scripts/`, `ush/`, `parm/`, `sorc/`, `fix/`, `ecf/`, `versions/`, `modulefiles/`).
- **Template_Renderer**: The wxflow-backed component responsible for resolving Jinja2 templates and nested YAML includes into concrete files.
- **Universal_Wrapper**: A single shell wrapper used as the scheduler entry point for every task. Sources environment, sets EE2 variables (`DATA`, `COMOUT`, `pgmout`, `jobid`), runs the JJob, performs error handling, and emits structured logs.
- **JJob**: A J-Job script under `jobs/` that follows the EE2 naming convention `JAAAAA`, sets up location and temporal variables, and invokes an Ex_Script.
- **Ex_Script**: A driver script under `scripts/` named `exaaaaa.sh` (or `.py`/`.pl`) that performs the bulk of a job's work.
- **NCO**: NCEP Central Operations.
- **EE2**: NCO Environmental Equivalence v2 implementation standards (NCEP WCOSS Implementation Standards v11.0.0).
- **Cycle**: A single forecast cycle identified by `PDY` (YYYYMMDD) and `cyc` (HH).
- **Task_Node**: One ecFlow leaf task instance, identified by `(suite, cycle, family-path, task_name)`.
- **Snapshot_ID**: A monotonically increasing identifier (semantic version plus content hash) assigned to a published EXPDIR.
- **Manifest**: A machine-readable file at the EXPDIR root that lists every file in the snapshot with its SHA-256 hash, the Snapshot_ID, and the source git commit.
- **Atomic_Publish**: The operation that moves a fully-staged product set into `${COMOUT}` such that downstream readers never observe a partial set.
- **wxflow**: NOAA-EMC Python utility library providing `parse_j2yaml`, `YAMLFile`, `Jinja`, and related helpers.
- **uwtools**: UFS Unified Workflow Tools providing driver framework, file-staging APIs (`uw fs copy`), scheduler abstractions for slurm/pbs/lsf, and YAML rendering.
- **Platform**: A supported HPC target. Initial set: WCOSS2, Hera, Hercules, Orion, Gaea (C6), Derecho, Ursa, AWS PW, AzurePW, GooglePW, CONTAINER.

## Requirements

### Requirement 1: ecFlow-Only Orchestration

**User Story:** As an NCO implementer, I want the workflow to be orchestrated exclusively by ecFlow, so that the operational suite uses one well-supported engine and we eliminate the cost of maintaining a parallel Rocoto path.

#### Acceptance Criteria

1. THE Global_Workflow SHALL express every Workflow_Configuration as an ecFlow Suite_Definition consumed by an EcFlow_Server.
2. THE DAG_Generator SHALL emit one `*.def` Suite_Definition file plus the corresponding `*.ecf` Task scripts under `<EXPDIR>/ecf/` for each rendered application.
3. THE Deployment_Tool SHALL NOT produce Rocoto XML, Rocoto database files, or any other Rocoto runtime artifacts.
4. THE Global_Workflow SHALL remove the Rocoto subparser from `dev/workflow/setup_workflow.py` and SHALL delete the `dev/workflow/rocoto/` and `dev/job_cards/rocoto/` source trees.
5. IF a developer attempts to invoke a Rocoto code path that has been decommissioned, THEN THE Deployment_Tool SHALL emit a `FATAL ERROR` whose message references this requirement and the ecFlow-only policy.
6. WHERE a third-party tool requests a non-ecFlow representation of the workflow, THE Global_Workflow SHALL provide that representation only as an exported, non-runtime view (for example a GraphViz `.dot` or a JSON dump) and SHALL NOT use it for scheduling.

### Requirement 2: Directed Acyclic Graph (DAG) Orchestration

**User Story:** As a workflow developer, I want every workflow configuration to be a strict DAG with explicit dependencies, so that scheduling order is unambiguous and the workflow can be analyzed, visualized, and validated.

#### Acceptance Criteria

1. THE DAG_Generator SHALL represent each Workflow_Configuration as a directed graph whose vertices are Task_Nodes and whose edges denote ecFlow `trigger`, `complete`, or event dependencies that the dependent Task_Node requires before submission.
2. WHEN a Workflow_Configuration is loaded, THE DAG_Generator SHALL validate that the resulting graph contains no cycles and SHALL emit a `FATAL ERROR` identifying the offending cycle if one is detected.
3. THE DAG_Generator SHALL support the ecFlow dependency primitives `trigger`, `complete`, `event`, `meter`, `time`, `date`, `cron`, and boolean compositions (`and`, `or`, `not`) of those primitives.
4. THE DAG_Generator SHALL support inter-cycle dependencies via ecFlow `repeat` constructs, allowing a Task_Node in cycle `N` to depend on a Task_Node in cycle `N-1`.
5. WHEN all declared dependencies of a Task_Node are satisfied, THE EcFlow_Server SHALL submit that Task_Node to the configured scheduler within 60 seconds of dependency satisfaction during normal operation.
6. WHEN a Task_Node is queried, THE EcFlow_Server SHALL return one of the ecFlow node states `unknown`, `queued`, `submitted`, `active`, `complete`, `aborted`, or `suspended`.
7. IF a Task_Node has been retried up to its configured `ECF_TRIES` limit and still aborts, THEN THE EcFlow_Server SHALL leave that Task_Node in state `aborted` and SHALL NOT release its downstream Task_Nodes.
8. THE DAG_Generator SHALL expose a query function `downstream(task)` and `upstream(task)` that returns the set of Task_Nodes reachable through dependency edges from `task` in the corresponding direction.

### Requirement 3: Immutable EXPDIR Deployment (Snapshot Paradigm)

**User Story:** As an NCO Senior Production Analyst, I want each workflow deployment to produce a fully self-contained, read-only EXPDIR, so that production runs are exactly reproducible and code drift between development and operations is eliminated.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL produce an EXPDIR that contains every artifact required to execute the workflow without reading from `dev/` at run time.
2. THE EXPDIR SHALL include subdirectories named `jobs/`, `scripts/`, `ush/`, `parm/`, `sorc/`, `fix/`, `ecf/`, `versions/`, and `modulefiles/` matching the NCO production layout described in EE2 Table 3.
3. THE Deployment_Tool SHALL write a Manifest at `<EXPDIR>/manifest.yaml` listing the Snapshot_ID, source git commit hash, deployment timestamp, deployment user, deployment host, and the SHA-256 hash of every file under the EXPDIR.
4. WHEN a Deployment_Tool run completes successfully, THE Deployment_Tool SHALL set every regular file under the EXPDIR to mode `0444` and every directory to mode `0555` for non-owner users.
5. IF the Deployment_Tool detects a write attempt to a previously published EXPDIR, THEN THE Deployment_Tool SHALL refuse the write and SHALL emit a `FATAL ERROR` referencing the existing Snapshot_ID.
6. THE Deployment_Tool SHALL assign each EXPDIR a Snapshot_ID composed of a semantic version string and a 12-character prefix of the SHA-256 of the Manifest content.
7. FOR ALL files listed in the Manifest, the on-disk SHA-256 hash SHALL equal the Manifest-recorded hash (manifest integrity property).
8. WHEN two Deployment_Tool runs execute against the same source git commit and the same input configuration YAML on the same Platform, THE Deployment_Tool SHALL produce EXPDIRs whose Manifests list identical file hashes for every rendered file (deployment determinism property).

### Requirement 4: Declarative Configuration & Deployment-Time Templating with Nested Includes

**User Story:** As a workflow developer, I want all configuration to be declarative YAML with nested Jinja2 templating resolved at deployment time, so that runtime jobs read concrete files and templating bugs surface during deployment rather than mid-run.

#### Acceptance Criteria

1. THE Template_Renderer SHALL accept YAML files that use Jinja2 syntax (`{{ }}`, `{% %}`, `{# #}`) and SHALL resolve all templates against a deployment-time context that includes at least `PDY`, `cyc`, `NET`, `RUN`, `MODE`, `MACHINE`, `model_ver`, `EXPDIR`, `COMROOT`, and the loaded configuration tree.
2. THE Template_Renderer SHALL support nested includes via the wxflow `parse_j2yaml` `searchpath` mechanism so that a parent YAML may `{% include %}` or `!INC` a child YAML located in any directory listed in the search path.
3. THE Template_Renderer SHALL support nested template inheritance such that a base template may declare `{% block %}` regions and a deriving template may override those blocks via `{% extends %}`.
4. WHEN the Template_Renderer encounters an undefined Jinja2 variable in `strict` mode, THE Template_Renderer SHALL emit a `FATAL ERROR` identifying the file, line number, and variable name.
5. THE Deployment_Tool SHALL invoke the Template_Renderer on every file under `dev/parm/`, `dev/workflow/`, and `dev/ecf/` whose name ends with `.j2` (or that is otherwise marked templated) before writing the corresponding non-`.j2` file into the EXPDIR.
6. THE EXPDIR SHALL NOT contain any unresolved Jinja2 tokens (`{{`, `{%`, `{#`) in files that the Deployment_Tool has marked as rendered.
7. WHERE a configuration value is declared in multiple YAML files in the include chain, THE Template_Renderer SHALL resolve the value using a documented precedence order with the most specific (deepest) include taking precedence over more general parents.
8. THE Template_Renderer SHALL provide a Pretty_Printer that serializes a rendered configuration tree back to canonical YAML.
9. FOR ALL valid rendered configuration trees `cfg`, `parse_yaml(pretty_print(cfg))` SHALL return a tree equal to `cfg` (round-trip property for the YAML serializer).
10. WHERE the Template_Renderer encounters `${VAR}` shell-style references inside a Jinja2 string value, THE Template_Renderer SHALL preserve them verbatim and SHALL leave their expansion to the runtime shell (because EE2 J-Jobs and ex-scripts depend on bash variable expansion).

### Requirement 5: Ephemeral Execution and Idempotency

**User Story:** As an operator, I want every task to run in a fresh, disposable working directory and produce the same products when re-run on the same inputs, so that retries and reruns are safe.

#### Acceptance Criteria

1. WHEN a Task_Node is dispatched, THE Universal_Wrapper SHALL create a fresh working directory at `${DATAROOT}/${jobid}`, change into it, and export `DATA` to that path.
2. WHEN a Task_Node completes with an EE2 success exit code (`0`) and `KEEPDATA` is not set to `YES`, THE Universal_Wrapper SHALL remove the working directory at `${DATAROOT}/${jobid}` before exiting.
3. THE Universal_Wrapper SHALL NOT depend on any state left behind by a previous invocation of the same Task_Node in `${DATAROOT}` (statelessness property).
4. WHEN a Task_Node is re-run with identical inputs, an identical EXPDIR, and identical cycle parameters, THE JJob SHALL produce a set of `${COMOUT}` output files whose SHA-256 hashes equal those of the prior successful run (idempotence property), excluding files explicitly listed as `nondeterministic` in the Workflow_Configuration.
5. IF a previous output file already exists at the staged path before a successful run completes, THEN THE JJob SHALL overwrite or remove that file rather than append to it.
6. WHEN a Task_Node is re-run after a partial failure, THE JJob SHALL not require any manual cleanup of `${DATAROOT}` or `${COMOUT}` to succeed.
7. THE Universal_Wrapper SHALL set `pgmout=OUTPUT.$$` and SHALL redirect verbose program output to `${DATA}/${pgmout}` per EE2 conventions.

### Requirement 6: Universal Wrappers (DRY Principle)

**User Story:** As a workflow developer, I want a single wrapper to handle scheduler entry, environment setup, error checking, and logging for every task, so that I do not repeat boilerplate in each J-Job and behavior stays consistent across components.

#### Acceptance Criteria

1. THE Universal_Wrapper SHALL be the entry point of every rendered `*.ecf` Task script in every Workflow_Configuration.
2. THE Universal_Wrapper SHALL set `set -x`, `export PS4='+ $SECONDS + '`, `umask 022`, and trap `ERR` and `EXIT` signals before invoking any JJob.
3. THE Universal_Wrapper SHALL load the platform-appropriate environment by sourcing the rendered env file at `<EXPDIR>/env/${MACHINE}.env` (passing the task name as its first argument) and SHALL emit a `FATAL ERROR` if that file does not exist.
4. WHEN the JJob exits with a non-zero status, THE Universal_Wrapper SHALL invoke `err_exit` with a descriptive message that includes the JJob name, `jobid`, and the exit status.
5. WHEN any executable invoked by the JJob sets `${err}` to a non-zero value, the JJob SHALL invoke `err_chk` immediately after the executable returns (per EE2 standards).
6. THE Universal_Wrapper SHALL emit a structured log record for every Task_Node lifecycle event (`init`, `start`, `succeeded`, `failed`, `aborted`, `complete`) containing at minimum the fields `task`, `cycle`, `jobid`, `attempt`, `state`, `timestamp`, and `duration_seconds`.
7. WHERE the target scheduler is one of `slurm`, `pbs`, or `lsf`, THE Universal_Wrapper SHALL submit jobs using the corresponding uwtools driver abstraction without changes to the underlying JJob.
8. THE Universal_Wrapper SHALL exist as a single rendered file at `<EXPDIR>/ush/universal_wrapper.sh` and SHALL NOT be duplicated per task.
9. THE Universal_Wrapper SHALL preserve and consolidate the existing helper utilities `jjob_header.sh`, `jjob_standard_vars.sh`, and `jjob_shell_setup.sh` so that current J-Jobs continue to work without per-job edits.

### Requirement 7: Atomic Delivery

**User Story:** As a downstream consumer of forecast products, I want a deliverable set to appear in `${COMOUT}` only after all of its files are complete, so that I never read partial or corrupted products.

#### Acceptance Criteria

1. WHEN a JJob writes products to `${COMOUT}`, THE JJob SHALL first stage all files under a hidden staging path `${COMOUT}/.staging/${jobid}/` and SHALL move them into their final names only after every file in the deliverable set is fully written and verified.
2. WHILE a deliverable set is staging, THE JJob SHALL use `cpfs` (per EE2) for any inter-filesystem copies so that partial files are not visible to readers.
3. WHEN every file of a deliverable set has been verified non-empty and (where applicable) hash-checked, THE JJob SHALL atomically move each file from the staging path to its final `${COMOUT}` location using `mv` within the same filesystem or an equivalent atomic operation.
4. IF any file in a deliverable set fails verification, THEN THE JJob SHALL leave `${COMOUT}` unchanged for that set and SHALL invoke `err_exit` with a message identifying the failed file.
5. WHEN a `dbn_alert` is to be sent for a product, THE JJob SHALL send the alert only after the corresponding file is in its final `${COMOUT}` location and only when `${SENDDBN^^}` equals `YES`.
6. FOR ALL deliverable sets `S` declared in the Workflow_Configuration, after a successful run either every file in `S` is present at its final `${COMOUT}` location with the expected name, or no file in `S` is present at its final location (atomicity property).

### Requirement 8: dev/ as Authoring Source and Deployment to NCO Layout

**User Story:** As a developer, I want all editable sources to live under `dev/` and be deployed into the canonical NCO production layout, so that operations receives a structure they recognize without polluting development with rendered artifacts.

#### Acceptance Criteria

1. THE Global_Workflow SHALL place all human-edited templates, J-Jobs, ex-scripts, ush utilities, parm files, env files, ecFlow templates, and workflow configuration YAML under the `dev/` directory tree.
2. THE Deployment_Tool SHALL map `dev/jobs/` to `<EXPDIR>/jobs/`, `dev/scripts/` to `<EXPDIR>/scripts/`, `dev/ush/` to `<EXPDIR>/ush/`, `dev/parm/` to `<EXPDIR>/parm/`, `dev/sorc/` (executables only) to `<EXPDIR>/sorc/`, and `dev/workflow/` artifacts that produce ecFlow definitions to `<EXPDIR>/ecf/`.
3. THE Deployment_Tool SHALL preserve the EE2 J-Job naming convention `JAAAAA` (uppercase, no extension) when copying J-Jobs into `<EXPDIR>/jobs/`.
4. THE Deployment_Tool SHALL preserve the EE2 ex-script naming convention `exaaaaa.sh` (lowercase, with extension) when copying ex-scripts into `<EXPDIR>/scripts/`.
5. THE Deployment_Tool SHALL place rendered ecFlow definition files under `<EXPDIR>/ecf/defs/`, rendered ecf task scripts under `<EXPDIR>/ecf/scripts/`, and ecFlow includes under `<EXPDIR>/ecf/include/` (matching the existing repository convention).
6. IF a file under `dev/` violates the EE2 naming convention for its target subdirectory, THEN THE Deployment_Tool SHALL emit a `FATAL ERROR` identifying the file and the violated convention.
7. THE Deployment_Tool SHALL NOT include `dev/ci/`, `dev/ctests/`, or any test-only assets in the EXPDIR by default.
8. THE Deployment_Tool SHALL accept an explicit allowlist parameter that specifies which optional development assets (for example `dev/ctests/`) are included in a non-production EXPDIR.
9. THE Deployment_Tool SHALL render `dev/parm/config/<app>/config.base.j2` (and all sibling `.j2` configs) into concrete shell files at `<EXPDIR>/parm/config/<app>/config.base` (and siblings) using wxflow `parse_j2yaml` with the deployment-time context.

### Requirement 9: wxflow and uwtools Integration

**User Story:** As a workflow developer, I want the system to use wxflow for YAML/Jinja2 templating and uwtools for driver, file-staging, and scheduler abstractions, so that we share infrastructure with the broader UFS community and avoid reinventing tooling.

#### Acceptance Criteria

1. THE Template_Renderer SHALL use `wxflow.parse_j2yaml` (or its API-equivalent successor) to resolve compound Jinja2-templated YAML files.
2. THE Deployment_Tool SHALL use the uwtools `uw fs copy` API or its Python equivalent to stage files into the EXPDIR, including HTTP and HPSS sources where declared.
3. THE Universal_Wrapper SHALL use the uwtools driver framework to abstract scheduler submission for `slurm`, `pbs`, and `lsf` targets.
4. THE Global_Workflow SHALL pin both `wxflow` and `uwtools` to specific versions in `dev/workflow/requirements.txt` (or its packaging equivalent) and SHALL record those versions in the Manifest.
5. IF the installed `wxflow` or `uwtools` version does not match the pinned version recorded in the Manifest, THEN THE Deployment_Tool SHALL emit a `FATAL ERROR` before producing any EXPDIR file.
6. THE Global_Workflow SHALL NOT depend on `uw rocoto realize`, `uw rocoto validate`, or any other Rocoto-related uwtools subcommand at deployment or run time.

### Requirement 10: Workflow Configuration Parser, Pretty-Printer, and Round-Trip

**User Story:** As a tooling developer, I want the Workflow_Configuration format and the generated Suite_Definition to have a parser, a pretty-printer, and round-trip guarantees, so that downstream tools (visualizers, validators, CI) can consume and re-emit workflows safely.

#### Acceptance Criteria

1. THE DAG_Generator SHALL provide a Parser that reads a YAML Workflow_Configuration file and returns an in-memory DAG object.
2. WHEN a malformed Workflow_Configuration is provided to the Parser, THE Parser SHALL return a descriptive error that identifies the file, line number, and reason.
3. THE DAG_Generator SHALL provide a Pretty_Printer that serializes an in-memory DAG object back into a canonical YAML Workflow_Configuration.
4. FOR ALL valid Workflow_Configuration files `f`, `pretty_print(parse(f))` SHALL parse to a DAG object that is structurally equal to `parse(f)` (parser round-trip property).
5. FOR ALL valid in-memory DAG objects `d`, `parse(pretty_print(d))` SHALL be structurally equal to `d` (printer round-trip property).
6. THE Pretty_Printer SHALL produce output that is deterministic for a given input DAG (byte-for-byte identical across invocations on the same machine and version).
7. THE DAG_Generator SHALL emit an ecFlow Suite_Definition string that, when consumed by the ecFlow Python API (`ecflow.Defs().load_from_string`), produces a `Defs` object whose set of `(family-path, task-name)` pairs is equal to the set of Task_Nodes in the source DAG (definition fidelity property).
8. THE DAG_Generator SHALL emit a Suite_Definition that, when round-tripped through `Defs.save_as_defs(path)` followed by `Defs.read_from_path(path)`, is structurally equal to the original `Defs` (ecFlow round-trip property).

### Requirement 11: EE2 Compliance Preservation

**User Story:** As an NCO implementer, I want the modernized workflow to remain EE2-compliant, so that it can be accepted into the production suite without exception requests.

#### Acceptance Criteria

1. THE JJob SHALL set the EE2 standard environment variables `DATA`, `cycle`, `PDY`, `NET`, `RUN`, `COMIN`, `COMOUT`, `pgmout`, and `jobid` before invoking the Ex_Script.
2. THE JJob SHALL call `setpdy.sh` after `cd ${DATA}` and before deriving date-shifted variables `PDYm#` and `PDYp#`.
3. THE Ex_Script SHALL wrap every C or Fortran executable invocation with `prep_step` and SHALL check the return code with `err_chk` immediately after the executable returns.
4. WHERE a JJob writes WMO-headed products, THE JJob SHALL place them under `${COMOUT}/wmo` per EE2 directory standards.
5. WHERE a JJob writes GEMPAK products, THE JJob SHALL place them under `${COMOUT}/gempak` per EE2 directory standards.
6. THE Deployment_Tool SHALL run an EE2 compliance scan over every rendered J-Job, ex-script, and ush script in the EXPDIR and SHALL emit a `FATAL ERROR` if any of the categories `error_handling`, `environment_variables`, `file_naming`, or `shebang_compliance` reports a violation.
7. THE JJob SHALL load required production utility modules (`prod_envir`, `prod_util`, and any GRIB or `wgrib2` modules used by the job) before referencing utilities they provide.
8. THE Universal_Wrapper SHALL refuse to run on WCOSS2 if `${envir}` is unset or is not one of `prod`, `para`, or `test`, and SHALL emit a `FATAL ERROR` in that case.

### Requirement 12: Multi-Platform Support

**User Story:** As a developer who runs experiments on different HPC systems, I want the same EXPDIR to be deployable to every supported platform, so that I can reproduce a configuration without rewriting platform-specific glue.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL accept a `--platform` argument selected from `{WCOSS2, HERA, HERCULES, ORION, GAEAC6, DERECHO, URSA, AWSPW, AZUREPW, GOOGLEPW, CONTAINER}` (matching the env files in `env/`).
2. THE Deployment_Tool SHALL render and copy `dev/env/${PLATFORM}.env` to `<EXPDIR>/env/${PLATFORM}.env` and SHALL render the platform-specific resource file `dev/parm/config/<app>/config.resources.${PLATFORM}` to `<EXPDIR>/parm/config/<app>/config.resources.${PLATFORM}`.
3. WHEN the same Workflow_Configuration is deployed against two different supported Platforms, THE Deployment_Tool SHALL produce EXPDIRs that differ only in the platform-conditioned files (env, resources, modulefiles) and SHALL NOT differ in J-Jobs, ex-scripts, or ush utilities (platform isolation property).
4. THE Universal_Wrapper SHALL detect the running Platform via `${MACHINE}` (or `ush/detect_machine.sh` for unset cases) and SHALL source the matching env file under `<EXPDIR>/env/`.
5. WHERE a Platform's scheduler differs (PBS on WCOSS2, Slurm on Hera/Hercules/Orion, etc.), THE DAG_Generator SHALL emit `*.ecf` Task scripts that use the correct scheduler directives for that Platform without changes to the underlying JJob.

### Requirement 13: Observability and Provenance

**User Story:** As an operator, I want to know exactly which Snapshot_ID, git commit, and inputs produced a given product, so that I can diagnose issues and reproduce runs.

#### Acceptance Criteria

1. THE EcFlow_Server SHALL record, for every Task_Node lifecycle event, the Snapshot_ID, git commit hash, cycle, attempt number, scheduler job id, and exit status to a workflow database file under `<EXPDIR>/workflow/state.db`.
2. THE Universal_Wrapper SHALL embed the Snapshot_ID and git commit hash as a comment header in every standard-output log it emits.
3. WHEN a `dbn_alert` is sent, THE JJob SHALL include the Snapshot_ID in the alert event metadata where the alert schema permits.
4. THE Deployment_Tool SHALL write a `<EXPDIR>/workflow/provenance.yaml` file capturing the source git remote URL, commit hash, branch, deployment user, deployment host, deployment timestamp, and rendered configuration values.
5. WHEN the workflow database file is read, THE DAG_Generator SHALL return query results that round-trip to the same on-disk representation when re-written (database round-trip property).

### Requirement 14: Decommissioning of Existing Rocoto Code Paths

**User Story:** As a maintainer, I want the existing Rocoto generation code to be removed cleanly, so that no developer is misled into using a deprecated path and CI runs only the supported workflow.

#### Acceptance Criteria

1. THE Global_Workflow SHALL delete the `dev/workflow/rocoto/` directory tree.
2. THE Global_Workflow SHALL delete the `dev/job_cards/rocoto/` directory tree.
3. THE Global_Workflow SHALL remove the `rocoto` subparser, `rocoto_xml_factory`, and all Rocoto-conditioned branches from `dev/workflow/setup_workflow.py`.
4. THE Global_Workflow SHALL remove `rocoto_viewer.py` and any other Rocoto-only tooling under `dev/workflow/`.
5. THE Global_Workflow SHALL remove or replace the existing CI cases (under `dev/ci/cases/`) that exercise Rocoto so that no CI job depends on Rocoto.
6. THE Global_Workflow SHALL update `dev/workflow/README_ecflow.md` (or replace it with a new `dev/workflow/README.md`) to describe the ecFlow-only deployment model and SHALL remove statements that reference Rocoto generation as supported.
7. WHERE existing documentation, comments, or commit messages reference Rocoto as the primary engine, THE Global_Workflow SHALL update those references to state that ecFlow is the sole orchestrator.

# Requirements Document

## Introduction

The current deployment pipeline copies ALL J-Jobs, ex-scripts, ush scripts, and config files from `dev/` into the EXPDIR regardless of whether the workflow YAML actually references them. Config files retain runtime conditionals on values known at deploy time, and model inputs are generated at runtime rather than pre-rendered and sealed. This results in bloated EXPDIRs, a gap between development and NCO production, and unnecessary runtime complexity.

This feature transforms the deployment pipeline to produce a **minimal, sealed, production-ready EXPDIR** containing only what the specific workflow needs, with all deploy-time-known values resolved and model inputs pre-rendered. The goal is to make development and NCO deployment nearly identical — minimizing the T2O (transition to operations) gap.

The feature comprises three core capabilities:
1. DAG-filtered staging — only deploy artifacts reachable from the workflow YAML task DAG
2. Deploy-time config resolution — evaluate and eliminate conditionals on deploy-time-known variables
3. Pre-rendered model inputs — seal model namelists/configs at deploy time using uwtools/wxflow

This spec is a child of the `immutable-dag-workflow-modernization` parent spec and operates within the 8-stage deployment pipeline defined there.

## Glossary

- **Deployment_Pipeline**: The 8-stage pipeline (`validate → build context → render templates → stage files → generate DAG → EE2 scan → manifest → seal`) implemented in `dev/workflow/deployment/pipeline.py`
- **EXPDIR**: The sealed, immutable experiment directory produced by the Deployment_Pipeline; matches the NCO production package layout (jobs/, scripts/, ush/, parm/, ecf/, etc.)
- **Workflow_YAML**: A YAML configuration file under `dev/parm/workflow/` (e.g., `gfs_forecast_only.yaml`) that defines the task DAG including suite structure, families, tasks, triggers, and jjob references
- **Task_DAG**: The directed acyclic graph of tasks defined in the Workflow_YAML; each task node references a `jjob` field mapping to a J-Job file in `dev/jobs/`
- **J-Job**: A shell script in `dev/jobs/` following JAAAAA naming (all caps, starts with J, no extension) whose purpose is to set up location/temporal variables and call the ex-script
- **Ex-Script**: A shell script in `dev/scripts/` following exaaaaa.sh naming (all lowercase, starts with ex) that drives the bulk of the application processing
- **Ush_Script**: A utility script in `dev/ush/` called by ex-scripts; named in all lowercase, not beginning with "ex"
- **Config_File**: A configuration file under `dev/parm/config/<app>/` (e.g., `config.fcst.j2`, `config.base.j2`) that parameterizes job behavior
- **Deploy_Time_Variable**: A variable whose value is fully determined at deployment time from the Workflow_YAML and platform selection (e.g., RUN, NET, CASE, MACHINE, CDUMP, NMEM_ENS)
- **Runtime_Variable**: A variable whose value is only known at job execution time (e.g., PDY, cyc, DATA, COMOUT)
- **DAG_Reachability_Set**: The complete set of J-Jobs, ex-scripts, ush scripts, and config files transitively referenced by the Task_DAG
- **Model_Input**: A namelist or configuration file consumed by a UFS model component at forecast runtime (e.g., input.nml, model_configure, diag_table, ufs.configure, ice_in, MOM_input, ww3_shel.nml)
- **Pre-Rendering**: The process of evaluating a Jinja2 template at deploy time using uwtools/wxflow to produce a fully resolved, static output file
- **Config_Conditioner**: A component that evaluates deploy-time-known conditionals in config files and eliminates dead branches, producing a config specific to the target workflow
- **uwtools**: The Unified Workflow Tools library (pinned at v2.16.0) providing template rendering and file staging utilities
- **wxflow**: The Workflow Execution Framework library (pinned at v0.3.0) providing `parse_j2yaml` and YAML manipulation utilities
- **EE2**: NCEP Environmental Equivalence version 2 standards (v11) governing the structure and behavior of operational packages on WCOSS2
- **cpreq**: An EE2 production utility that copies essential files and aborts with FATAL ERROR on failure
- **T2O_Gap**: The set of differences between a development deployment and an NCO production deployment; this feature aims to minimize it

## Requirements

### Requirement 1: DAG-Filtered J-Job Staging

**User Story:** As a workflow developer, I want the EXPDIR to contain only the J-Jobs referenced by the workflow YAML task DAG, so that the deployed package is minimal and specific to the configured workflow.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline stages files, THE DAG_Filter SHALL extract the set of `jjob` values from all task definitions in the Workflow_YAML
2. WHEN a J-Job file in `dev/jobs/` is not referenced by any task in the Task_DAG, THE Deployment_Pipeline SHALL exclude that J-Job from the EXPDIR `jobs/` directory
3. WHEN a J-Job file in `dev/jobs/` is referenced by at least one task in the Task_DAG, THE Deployment_Pipeline SHALL copy that J-Job into the EXPDIR `jobs/` directory preserving EE2 JAAAAA naming
4. FOR ALL deployed J-Jobs in the EXPDIR, THE DAG_Filter SHALL verify that each file follows the JAAAAA naming convention (all caps, starts with J, no extension)
5. WHEN a `jjob` value in the Workflow_YAML references a J-Job that does not exist in `dev/jobs/`, THE Deployment_Pipeline SHALL emit a FATAL ERROR naming the missing J-Job and the referencing task

### Requirement 2: DAG-Filtered Ex-Script Staging

**User Story:** As a workflow developer, I want the EXPDIR to contain only the ex-scripts called by the DAG-reachable J-Jobs, so that no unreachable processing scripts are deployed.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline stages files, THE DAG_Filter SHALL parse each DAG-reachable J-Job to identify the ex-script it invokes
2. WHEN an ex-script in `dev/scripts/` is not invoked by any DAG-reachable J-Job, THE Deployment_Pipeline SHALL exclude that ex-script from the EXPDIR `scripts/` directory
3. WHEN an ex-script in `dev/scripts/` is invoked by at least one DAG-reachable J-Job, THE Deployment_Pipeline SHALL copy that ex-script into the EXPDIR `scripts/` directory preserving exaaaaa.sh naming
4. IF a DAG-reachable J-Job references an ex-script that does not exist in `dev/scripts/`, THEN THE Deployment_Pipeline SHALL emit a FATAL ERROR naming the missing ex-script and the invoking J-Job

### Requirement 3: DAG-Filtered Ush Script Staging

**User Story:** As a workflow developer, I want the EXPDIR to contain only the ush scripts transitively sourced by DAG-reachable ex-scripts, so that utility scripts unrelated to this workflow are excluded.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline stages files, THE DAG_Filter SHALL perform transitive dependency analysis on DAG-reachable ex-scripts to identify all sourced or called Ush_Scripts
2. WHEN a Ush_Script in `dev/ush/` is not transitively referenced by any DAG-reachable ex-script, THE Deployment_Pipeline SHALL exclude that Ush_Script from the EXPDIR `ush/` directory
3. WHEN a Ush_Script in `dev/ush/` is transitively referenced by at least one DAG-reachable ex-script, THE Deployment_Pipeline SHALL copy that Ush_Script into the EXPDIR `ush/` directory
4. THE DAG_Filter SHALL detect circular source dependencies among Ush_Scripts and emit a WARNING without entering an infinite traversal loop
5. IF a DAG-reachable script references a Ush_Script that does not exist in `dev/ush/`, THEN THE Deployment_Pipeline SHALL emit a WARNING naming the missing Ush_Script (non-fatal, as the script may source conditionally)

### Requirement 4: DAG-Filtered Config File Staging

**User Story:** As a workflow developer, I want the EXPDIR to contain only the config files needed by DAG-reachable tasks, so that configuration for unrelated subsystems is excluded.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline stages config files, THE DAG_Filter SHALL determine the set of Config_Files required by the DAG-reachable J-Jobs and ex-scripts
2. WHEN a Config_File under `dev/parm/config/<app>/` is not required by any DAG-reachable task, THE Deployment_Pipeline SHALL exclude that Config_File from the EXPDIR `parm/config/` directory
3. WHEN a Config_File is required by at least one DAG-reachable task, THE Deployment_Pipeline SHALL stage that Config_File into the EXPDIR after applying deploy-time config resolution (Requirement 5)
4. THE DAG_Filter SHALL include `config.base.j2` and `config.com` unconditionally, as these provide foundational variables consumed by all tasks
5. THE DAG_Filter SHALL include platform-specific resource files (`config.resources.<PLATFORM>`) only for the target platform specified at deploy time

### Requirement 5: Deploy-Time Config Conditional Resolution

**User Story:** As a workflow developer, I want config files to have their deploy-time-known conditionals resolved at deployment, so that the deployed config is specific to this workflow with no dead code branches.

#### Acceptance Criteria

1. WHEN the Config_Conditioner processes a Config_File, THE Config_Conditioner SHALL identify all conditional blocks (e.g., `if [[ "${RUN}" == "gfs" ]]`) that test Deploy_Time_Variables
2. WHEN a conditional block tests only Deploy_Time_Variables whose values are known from the Workflow_YAML and platform selection, THE Config_Conditioner SHALL evaluate the condition and retain only the matching branch
3. WHEN a conditional block tests any Runtime_Variable (e.g., PDY, cyc, FHOUR), THE Config_Conditioner SHALL preserve that conditional block unchanged in the deployed config
4. THE Config_Conditioner SHALL recognize the following as Deploy_Time_Variables resolvable from the Workflow_YAML: RUN, NET, CASE, CASE_ENS, MACHINE, CDUMP, NMEM_ENS, APP, CCPP_SUITE, DO_COUPLED, DO_WAVE, DO_OCN, DO_ICE, DO_AERO, REPLAY_ICS
5. WHEN the Config_Conditioner eliminates a dead branch, THE Config_Conditioner SHALL insert a comment indicating the original conditional and the resolved value (e.g., `# Resolved: RUN=gfs at deploy time`)
6. THE Config_Conditioner SHALL preserve all shell variable references (`${VAR}`) for Runtime_Variables without modification
7. IF a conditional block mixes Deploy_Time_Variables and Runtime_Variables in the same test expression, THEN THE Config_Conditioner SHALL preserve that conditional block unchanged (conservative approach)
8. WHEN the Config_Conditioner completes processing of a Config_File, THE Config_Conditioner SHALL validate that the resulting file is syntactically valid shell (parseable by `bash -n`)

### Requirement 6: Pre-Rendered Model Input Staging

**User Story:** As a workflow developer, I want model inputs (namelists and configuration files) to be pre-rendered at deploy time and sealed in the EXPDIR, so that the forecast runtime path performs only file copies with no runtime template generation.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline renders templates, THE Model_Input_Renderer SHALL render all Jinja2 templates under `dev/parm/ufs/` using uwtools and wxflow with the deploy-time context
2. THE Model_Input_Renderer SHALL produce pre-rendered outputs for the following Model_Inputs: input.nml, model_configure, diag_table, ufs.configure, ice_in, MOM_input, ww3_shel.nml
3. WHEN a Model_Input template is rendered, THE Model_Input_Renderer SHALL write the output to `<EXPDIR>/parm/ufs/<component>/` following the directory structure: fv3/, ocean/, ice/, wave/, gocart/
4. THE Model_Input_Renderer SHALL resolve all deploy-time Jinja2 variables and produce output files containing zero unresolved Jinja2 tokens (`{{`, `{%`, `{#`)
5. THE Model_Input_Renderer SHALL preserve shell variable references (`${VAR}`) for variables that are only known at runtime (e.g., `${DATA}`, `${ROTDIR}`)
6. IF a required Jinja2 variable is undefined in the deploy-time context, THEN THE Model_Input_Renderer SHALL emit a FATAL ERROR naming the undefined variable, the template file, and the line number
7. WHEN the Deployment_Pipeline completes, THE EXPDIR SHALL contain all pre-rendered Model_Inputs required by the DAG-reachable forecast task under `parm/ufs/<component>/`

### Requirement 7: Forecast Runtime Sealed-Copy Path

**User Story:** As a forecast runtime operator, I want the forecast job to copy pre-rendered model inputs from the sealed EXPDIR into the working directory using `cpreq`, so that no runtime namelist generation occurs.

#### Acceptance Criteria

1. WHEN the forecast ex-script stages model inputs, THE Forecast_Runtime SHALL use `cpreq` to copy pre-rendered files from `${EXPDIR}/parm/ufs/<component>/` to `${DATA}/`
2. THE Forecast_Runtime SHALL NOT invoke any `parsing_namelists_*.sh` scripts or runtime template rendering for model inputs that have been pre-rendered and sealed in the EXPDIR
3. WHEN a pre-rendered Model_Input file is missing from the EXPDIR at runtime, THE Forecast_Runtime SHALL emit a FATAL ERROR with a descriptive message naming the missing file path before attempting the copy
4. THE Forecast_Runtime SHALL use `cpreq` (not `cp` or `cpfs`) for essential model input files, per EE2 standards requiring abort-on-failure for essential inputs
5. THE Forecast_Runtime SHALL stage model inputs from the EXPDIR using variables established in the J-Job (`${EXPDIR}`, `${DATA}`) and SHALL NOT alter those variables

### Requirement 8: DAG Reachability Completeness Verification

**User Story:** As a deployment operator, I want the pipeline to verify that the DAG-filtered EXPDIR is complete (all transitive dependencies satisfied), so that no runtime failures occur due to missing files.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline completes DAG-filtered staging, THE Completeness_Verifier SHALL confirm that every J-Job in the EXPDIR `jobs/` directory references an ex-script present in the EXPDIR `scripts/` directory
2. WHEN the Deployment_Pipeline completes DAG-filtered staging, THE Completeness_Verifier SHALL confirm that every Ush_Script sourced by a staged ex-script is present in the EXPDIR `ush/` directory
3. IF the Completeness_Verifier detects a missing transitive dependency, THEN THE Deployment_Pipeline SHALL emit a FATAL ERROR naming the missing file and the referencing script
4. THE Completeness_Verifier SHALL run after all staging is complete but before the manifest generation stage

### Requirement 9: EXPDIR Size Reduction Reporting

**User Story:** As a workflow developer, I want the deployment pipeline to report the reduction in staged files compared to a full (unfiltered) deployment, so that I can verify the DAG filter is effective.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline completes DAG-filtered staging, THE Deployment_Pipeline SHALL log the count of staged J-Jobs versus total available J-Jobs in `dev/jobs/`
2. WHEN the Deployment_Pipeline completes DAG-filtered staging, THE Deployment_Pipeline SHALL log the count of staged ex-scripts versus total available ex-scripts in `dev/scripts/`
3. WHEN the Deployment_Pipeline completes DAG-filtered staging, THE Deployment_Pipeline SHALL log the count of staged Ush_Scripts versus total available Ush_Scripts in `dev/ush/`
4. WHEN the Deployment_Pipeline completes DAG-filtered staging, THE Deployment_Pipeline SHALL log the count of staged Config_Files versus total available Config_Files in `dev/parm/config/`

### Requirement 10: EE2 Compliance of Sealed EXPDIR

**User Story:** As an NCO deployment engineer, I want the sealed EXPDIR to comply with EE2 v11 standards for package structure, so that the transition to operations requires only job card variable changes.

#### Acceptance Criteria

1. THE sealed EXPDIR SHALL organize files into EE2-mandated subdirectories: jobs/, scripts/, ush/, parm/, ecf/, versions/, modulefiles/
2. THE sealed EXPDIR SHALL contain only J-Jobs following JAAAAA naming in the jobs/ directory
3. THE sealed EXPDIR SHALL contain only ex-scripts following exaaaaa.sh naming in the scripts/ directory
4. WHEN the EE2 compliance scan runs over the sealed EXPDIR, THE Deployment_Pipeline SHALL verify that all J-Jobs use Bash and set `PS4='+ $SECONDS + '` for timing
5. WHEN the EE2 compliance scan runs over the sealed EXPDIR, THE Deployment_Pipeline SHALL verify that all scripts use `set -x` for debug logging
6. THE sealed EXPDIR SHALL satisfy the EE2 principle: "To move a model from development to production, it must only be necessary to change the variables exported in the job cards"

### Requirement 11: Deploy-Time Variable Source of Truth

**User Story:** As a workflow developer, I want a single, documented source of truth for which variables are considered deploy-time-known, so that the Config_Conditioner and Model_Input_Renderer resolve the correct set.

#### Acceptance Criteria

1. THE Deployment_Pipeline SHALL maintain a documented registry of Deploy_Time_Variables with their source (Workflow_YAML field or platform selection)
2. WHEN a variable is added to or removed from the Deploy_Time_Variable registry, THE change SHALL require explicit review (the registry is version-controlled)
3. THE Deploy_Time_Variable registry SHALL include at minimum: RUN, NET, CASE, CASE_ENS, MACHINE, CDUMP, NMEM_ENS, APP, CCPP_SUITE, DO_COUPLED, DO_WAVE, DO_OCN, DO_ICE, DO_AERO, REPLAY_ICS
4. THE Config_Conditioner and Model_Input_Renderer SHALL both consume the same Deploy_Time_Variable registry as their resolution context

### Requirement 12: Idempotent Deployment

**User Story:** As a workflow developer, I want repeated deployments of the same Workflow_YAML at the same commit to produce byte-identical EXPDIRs, so that the deployment is deterministic and auditable.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline deploys the same Workflow_YAML, platform, and git commit twice, THE resulting EXPDIRs SHALL have identical file manifests (same SHA-256 hashes for all files)
2. THE DAG_Filter SHALL produce a deterministic ordering of files regardless of filesystem enumeration order
3. THE Config_Conditioner SHALL produce deterministic output (same input always yields same output, no timestamps or random values injected)
4. THE Model_Input_Renderer SHALL produce deterministic output for the same deploy-time context

### Requirement 13: Backward Compatibility with Full Deployment

**User Story:** As a workflow developer, I want the option to deploy without DAG filtering (full deployment mode), so that existing workflows and CI pipelines continue to function during the transition period.

#### Acceptance Criteria

1. WHERE the `--dag-filter` flag is enabled, THE Deployment_Pipeline SHALL apply DAG-filtered staging as specified in Requirements 1-4
2. WHERE the `--dag-filter` flag is disabled (default during transition), THE Deployment_Pipeline SHALL stage all files from `dev/` using the existing full-copy behavior
3. WHEN the `--dag-filter` flag is enabled, THE Deployment_Pipeline SHALL still apply deploy-time config resolution (Requirement 5) and model input pre-rendering (Requirement 6) to the filtered set
4. THE Deployment_Pipeline SHALL log whether DAG filtering is active or inactive at the start of the staging phase

### Requirement 14: Pre-Rendered Model Input Round-Trip Fidelity

**User Story:** As a model developer, I want to verify that pre-rendered model inputs are semantically equivalent to what the legacy runtime rendering would produce, so that forecast results are unchanged.

#### Acceptance Criteria

1. FOR ALL Model_Input templates, rendering the template with the deploy-time context and then parsing the output SHALL produce a data structure equivalent to what the legacy `parsing_namelists_*.sh` scripts would generate for the same inputs (round-trip property)
2. THE Model_Input_Renderer SHALL preserve Fortran namelist formatting conventions (e.g., proper quoting of string values, correct boolean representation as `.true.`/`.false.`)
3. THE Model_Input_Renderer SHALL preserve MOM6 parameter file formatting (key = value pairs with proper comment handling)
4. WHEN a pre-rendered input.nml is parsed by a Fortran namelist parser, THE parser SHALL accept the file without errors

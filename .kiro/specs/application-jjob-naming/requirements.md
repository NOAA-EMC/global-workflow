# Requirements Document

## Introduction

The NCO production environment requires that deployed J-Job files use application-specific naming (e.g., `JGCAFS_FORECAST`, `JGCDAS_FORECAST`, `JGFS_FORECAST`) rather than the shared development names (e.g., `JGLOBAL_FORECAST`). Currently, source files in `dev/jobs/` use shared prefixes (`JGLOBAL_*`, `JGDAS_*`, `JGFS_*`) that serve multiple applications. The Workflow_YAML `jjob:` fields reference these shared names directly, and the deployment pipeline copies them without renaming.

This feature introduces an **application-specific naming layer** into the deployment pipeline. The Workflow_YAML will reference application-named J-Jobs (e.g., `JGCAFS_FORECAST`), and the pipeline's file stager will resolve these names back to the shared source files in `dev/jobs/` via a deterministic naming convention. The deployed EXPDIR will contain only application-named J-Jobs, satisfying NCO packaging requirements while preserving the single-source-of-truth principle in development.

This feature builds upon the `minimal-sealed-expdir` spec's DAG_Filter, File_Stager, and EE2 Scanner components.

## Glossary

- **Application_Prefix**: The application-specific segment of a J-Job name that identifies which application the job belongs to in the deployed EXPDIR (e.g., `GCAFS`, `GCDAS`, `GFS`, `GDAS`, `GEFS`, `SFS`)
- **Shared_Source_Name**: The name of a J-Job file as it exists in `dev/jobs/` using a shared prefix (e.g., `JGLOBAL_FORECAST`, `JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX`)
- **Application_Name**: The name of a J-Job as it appears in the deployed EXPDIR and in Workflow_YAML `jjob:` fields, using an Application_Prefix (e.g., `JGCAFS_FORECAST`, `JGCDAS_AERO_ANALYSIS_INITIALIZE`)
- **Name_Resolver**: The component within the deployment pipeline that maps an Application_Name to its corresponding Shared_Source_Name in `dev/jobs/`
- **Shared_Prefix**: A prefix used by source files in `dev/jobs/` that indicates the file is shared across applications (currently: `JGLOBAL_`)
- **Direct_Match_Source**: A source file in `dev/jobs/` that already carries an application-specific prefix and does not require Shared_Prefix lookup (e.g., `JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX`)
- **Deployment_Pipeline**: The 8-stage pipeline implemented in `dev/workflow/deployment/pipeline.py` that produces the sealed EXPDIR
- **File_Stager**: The component within the Deployment_Pipeline that copies files from `dev/` to the EXPDIR, applying name transformations
- **DAG_Filter**: The component that extracts the reachability set of artifacts from the Workflow_YAML task DAG
- **EXPDIR**: The sealed, immutable experiment directory produced by the Deployment_Pipeline; matches the NCO production package layout
- **Workflow_YAML**: A YAML configuration file under `dev/parm/workflow/` that defines the task DAG including `jjob:` fields referencing Application_Names
- **EE2**: NCEP Environmental Equivalence version 2 standards governing structure and naming of operational packages
- **JAAAAA_Convention**: The EE2 naming convention for J-Jobs: all uppercase, starts with `J`, no file extension
- **Prefix_Registry**: A configuration structure that maps each known Application_Prefix to the ordered list of Shared_Prefixes to search when resolving names

## Requirements

### Requirement 1: Workflow YAML References Application-Specific Names

**User Story:** As a workflow developer, I want the Workflow_YAML `jjob:` fields to reference application-specific J-Job names, so that the workflow definition matches what NCO deploys in production.

#### Acceptance Criteria

1. THE Workflow_YAML SHALL use Application_Names in all `jjob:` field values (e.g., `JGCAFS_FORECAST` instead of `JGLOBAL_FORECAST`)
2. WHEN the Workflow_YAML defines a task with a `jjob:` field, THE `jjob:` value SHALL conform to the JAAAAA_Convention (all uppercase, starts with `J`, no file extension)
3. WHEN the Workflow_YAML defines tasks under a cycle named `gcdas`, THE `jjob:` fields for those tasks SHALL use the `JGCDAS_` prefix
4. WHEN the Workflow_YAML defines tasks under a cycle named `gcafs`, THE `jjob:` fields for those tasks SHALL use the `JGCAFS_` prefix
5. WHEN the Workflow_YAML defines tasks under a cycle named `gdas`, THE `jjob:` fields for those tasks SHALL use the `JGDAS_` prefix
6. WHEN the Workflow_YAML defines tasks under a cycle named `gfs`, THE `jjob:` fields for those tasks SHALL use the `JGFS_` prefix

### Requirement 2: Name Resolution from Application Name to Shared Source

**User Story:** As a deployment pipeline developer, I want the pipeline to resolve application-specific J-Job names back to shared source files in `dev/jobs/`, so that a single implementation serves all applications without code duplication.

#### Acceptance Criteria

1. WHEN the Name_Resolver receives an Application_Name, THE Name_Resolver SHALL strip the Application_Prefix to produce a suffix (e.g., `JGCAFS_FORECAST` → suffix `FORECAST`)
2. WHEN the Name_Resolver has a suffix, THE Name_Resolver SHALL search for a source file named `JGLOBAL_<suffix>` in `dev/jobs/`
3. WHEN no `JGLOBAL_<suffix>` source file exists, THE Name_Resolver SHALL search for a Direct_Match_Source with the original Application_Name in `dev/jobs/`
4. WHEN both `JGLOBAL_<suffix>` and the Direct_Match_Source exist, THE Name_Resolver SHALL prefer the `JGLOBAL_<suffix>` source
5. IF neither a `JGLOBAL_<suffix>` nor a Direct_Match_Source exists in `dev/jobs/`, THEN THE Name_Resolver SHALL emit a FATAL ERROR identifying the unresolvable Application_Name and the paths searched
6. THE Name_Resolver SHALL support the following Application_Prefixes: `JGCAFS_`, `JGCDAS_`, `JGFS_`, `JGDAS_`, `JGEFS_`, `JSFS_`
7. THE Name_Resolver SHALL be configurable via a Prefix_Registry that maps Application_Prefixes to an ordered list of Shared_Prefixes to search

### Requirement 3: File Stager Copies with Application-Specific Naming

**User Story:** As an NCO deployment engineer, I want the deployed EXPDIR `jobs/` directory to contain only application-named J-Jobs, so that the package satisfies NCO naming requirements without manual post-processing.

#### Acceptance Criteria

1. WHEN the File_Stager stages a J-Job, THE File_Stager SHALL copy the resolved Shared_Source_Name file from `dev/jobs/` into the EXPDIR `jobs/` directory using the Application_Name as the destination filename
2. WHEN the File_Stager completes staging, THE EXPDIR `jobs/` directory SHALL contain zero files with the `JGLOBAL_` prefix
3. FOR ALL files in the EXPDIR `jobs/` directory, THE File_Stager SHALL verify that each filename conforms to the JAAAAA_Convention
4. WHEN two tasks in the Workflow_YAML reference the same Application_Name, THE File_Stager SHALL stage that J-Job exactly once (deduplication)
5. WHEN two different Application_Names resolve to the same Shared_Source_Name (e.g., `JGCAFS_FORECAST` and `JGCDAS_FORECAST` both resolve to `JGLOBAL_FORECAST`), THE File_Stager SHALL produce two distinct files in the EXPDIR `jobs/` directory, one for each Application_Name

### Requirement 4: DAG Filter Uses Application Names

**User Story:** As a workflow developer, I want the DAG_Filter to correctly extract application-named J-Jobs from the Workflow_YAML and resolve them to source files for reachability analysis, so that DAG-filtered deployments work with application naming.

#### Acceptance Criteria

1. WHEN the DAG_Filter extracts `jjob:` values from the Workflow_YAML, THE DAG_Filter SHALL collect Application_Names (not Shared_Source_Names)
2. WHEN the DAG_Filter computes reachability for ex-scripts, ush scripts, and config files, THE DAG_Filter SHALL use the resolved Shared_Source_Name to parse the source file for downstream dependencies
3. WHEN the DAG_Filter reports its reachability set, THE DAG_Filter SHALL report both the Application_Name (for EXPDIR staging) and the resolved Shared_Source_Name (for source parsing)
4. IF a `jjob:` value in the Workflow_YAML cannot be resolved by the Name_Resolver, THEN THE DAG_Filter SHALL emit a FATAL ERROR naming the unresolvable J-Job and the referencing task

### Requirement 5: Prefix Registry Configuration

**User Story:** As a workflow developer, I want the mapping between Application_Prefixes and Shared_Prefixes to be explicitly configured, so that new applications can be added without modifying pipeline code.

#### Acceptance Criteria

1. THE Prefix_Registry SHALL define an ordered search list of Shared_Prefixes for each Application_Prefix
2. THE Prefix_Registry SHALL include the following default mappings: `JGCAFS_` → [`JGLOBAL_`], `JGCDAS_` → [`JGLOBAL_`, `JGDAS_`], `JGFS_` → [`JGLOBAL_`, `JGFS_`], `JGDAS_` → [`JGLOBAL_`, `JGDAS_`], `JGEFS_` → [`JGLOBAL_`, `JGEFS_`], `JSFS_` → [`JGLOBAL_`, `JSFS_`]
3. WHEN resolving an Application_Name, THE Name_Resolver SHALL search source prefixes in the order defined by the Prefix_Registry, returning the first match
4. WHEN a new Application_Prefix is added to the Prefix_Registry, THE Name_Resolver SHALL support the new prefix without code changes to the pipeline
5. THE Prefix_Registry SHALL be stored in a version-controlled configuration file within the `dev/workflow/deployment/` directory

### Requirement 6: EE2 Compliance of Application-Named J-Jobs

**User Story:** As an NCO deployment engineer, I want all deployed J-Jobs to satisfy EE2 naming and structural conventions regardless of whether they were renamed during staging, so that the package passes NCO compliance scanning.

#### Acceptance Criteria

1. FOR ALL deployed J-Jobs in the EXPDIR `jobs/` directory, THE EE2_Scanner SHALL validate that each filename conforms to the JAAAAA_Convention (all uppercase, starts with `J`, no file extension)
2. WHEN a J-Job is staged with an Application_Name, THE staged file content SHALL remain identical to the Shared_Source_Name file content (the rename is filename-only, not content modification)
3. WHEN the EE2_Scanner encounters an Application_Named J-Job, THE EE2_Scanner SHALL validate the file content using the same structural rules applied to shared-named J-Jobs (shebang, jjob_header sourcing, ex-script invocation)

### Requirement 7: Dry-Run Name Resolution Reporting

**User Story:** As a workflow developer, I want to preview how application names will resolve to source files before running a full deployment, so that I can catch naming errors early.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline runs with the `--dry-run` flag, THE pipeline SHALL output a table mapping each Application_Name to its resolved Shared_Source_Name
2. WHEN the dry-run encounters an unresolvable Application_Name, THE pipeline SHALL report the error in the dry-run output without halting on the first error (report all errors)
3. WHEN the dry-run completes, THE pipeline SHALL report the total count of resolvable and unresolvable Application_Names

### Requirement 8: Backward Compatibility with Existing Shared-Named Workflows

**User Story:** As a workflow developer maintaining legacy workflow YAMLs, I want the pipeline to still accept shared names (e.g., `JGLOBAL_FORECAST`) in `jjob:` fields when no application renaming is configured, so that existing workflows continue to deploy without modification.

#### Acceptance Criteria

1. WHEN a `jjob:` value in the Workflow_YAML uses a Shared_Prefix (e.g., `JGLOBAL_FORECAST`), THE File_Stager SHALL copy the file from `dev/jobs/` without renaming
2. WHEN a `jjob:` value matches a file directly in `dev/jobs/`, THE Name_Resolver SHALL treat the name as already resolved and skip prefix-based resolution
3. WHEN a Workflow_YAML contains a mix of Application_Names and Shared_Source_Names, THE pipeline SHALL handle both correctly in the same deployment run

### Requirement 9: Fix Directory and Executable Linking Scripts Staged in EXPDIR

**User Story:** As an NCO deployment engineer, I want the deployment pipeline to stage the `link_workflow.sh` and `link_fixdirs.sh` scripts into the sealed EXPDIR, so that the operator can run them on the target machine to link fix directories, copy executables, and wire up submodule artifacts without needing access to the source tree.

#### Acceptance Criteria

1. WHEN the Deployment_Pipeline stages files, THE File_Stager SHALL copy `sorc/link_workflow.sh` into the EXPDIR `sorc/` directory
2. WHEN the Deployment_Pipeline stages files, THE File_Stager SHALL copy `sorc/ufs_utils.fd/fix/link_fixdirs.sh` into the EXPDIR `sorc/ufs_utils.fd/fix/` directory (preserving the relative path structure)
3. WHEN the EXPDIR is deployed on the target HPC platform, THE operator SHALL be able to execute `sorc/link_workflow.sh` from within the EXPDIR to link fix directories, executables, and submodule artifacts for that platform
4. THE staged `link_workflow.sh` SHALL use the EXPDIR root as `HOMEglobal` when executed, requiring no modification to path references
5. WHEN the `--dag-filter` flag is enabled, THE Deployment_Pipeline SHALL still stage both linking scripts (they are unconditional deployment artifacts, not DAG-dependent)
6. THE staged linking scripts SHALL retain their executable permission bits (mode 0755) in the sealed EXPDIR

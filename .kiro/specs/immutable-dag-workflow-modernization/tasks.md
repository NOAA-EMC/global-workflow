# Implementation Plan: Immutable DAG Workflow Modernization

## Overview

This plan converts the global-workflow into an immutable, ecFlow-only DAG orchestration system. Implementation proceeds bottom-up: core libraries first (parser, renderer, DAG generator), then the deployment pipeline, then the universal wrapper and runtime components, then Rocoto decommission, and finally integration wiring and CI.

Python is used for all deployment tooling (`dev/workflow/deployment/`). Bash is used for the Universal_Wrapper, Atomic_Publish, and shell utilities. Property-based tests use `hypothesis`. 

## Tasks

- [x] 1. Set up project structure and core interfaces
  - [x] 1.1 Create the deployment package directory and module stubs
    - Create `dev/workflow/deployment/__init__.py`, `pipeline.py`, `template_renderer.py`, `dag_generator.py`, `workflow_config.py`, `file_stager.py`, `manifest.py`, `ee2_scanner.py`, `seal.py`
    - Define the `TaskNode` and `DAG` dataclasses in `workflow_config.py` with type annotations
    - Define the `Edge` dataclass and `MeterDef` named tuple
    - Add `dev/workflow/requirements.txt` entries pinning `wxflow` and `uwtools` versions
    - _Requirements: 1.1, 2.1, 9.4_

  - [x] 1.2 Create ecFlow template directory and include stubs
    - Create `dev/workflow/ecflow/templates/head.h.j2`, `tail.h.j2`, `envsetup.h.j2`, `task.ecf.j2`
    - Populate `head.h.j2` with ecFlow preamble (`%include <head.h>` pattern, `set -x`, `PS4`, `umask 022`, trap)
    - Populate `tail.h.j2` with cleanup and lifecycle logging calls
    - Populate `envsetup.h.j2` with platform env sourcing logic
    - Populate `task.ecf.j2` with the per-task Jinja2 template that invokes `universal_wrapper.sh`
    - _Requirements: 1.2, 6.1, 6.2_

  - [x] 1.3 Create Workflow_Configuration YAML schema and sample configs
    - Create `dev/parm/workflow/gfs_cycled.yaml` with the schema defined in the design (suite, defaults, cycles, families, tasks, inter_cycle_dependencies)
    - Create minimal `dev/parm/workflow/gfs_forecast_only.yaml` for testing
    - _Requirements: 2.1, 10.1_

- [x] 2. Implement Workflow_Configuration Parser and Pretty-Printer
  - [x] 2.1 Implement the Parser (`workflow_config.py`)
    - Implement `parse(path: str) -> DAG` that reads YAML and constructs the in-memory DAG object
    - Validate required keys (`suite`, `families`, `tasks`)
    - Return descriptive errors with file, line number, and reason on malformed input
    - Build `nodes` dict and `edges` list from `trigger`/`complete`/`event` declarations
    - Support `for_each` expansion of parameterized tasks
    - _Requirements: 10.1, 10.2_

  - [x] 2.2 Implement the Pretty-Printer (`workflow_config.py`)
    - Implement `pretty_print(dag: DAG) -> str` that serializes DAG to canonical YAML
    - Use `sort_keys=False`, deterministic output (byte-for-byte identical across invocations)
    - Preserve comments where possible via `ruamel.yaml` or equivalent
    - _Requirements: 10.3, 10.6_

  - [x] 2.3 Write property test: Parser Round-Trip (Property 9)
    - **Property 9: Parser Round-Trip**
    - Use hypothesis to generate valid Workflow_Configuration YAML structures
    - Assert `pretty_print(parse(f))` parses to a DAG structurally equal to `parse(f)`
    - **Validates: Requirements 10.4**

  - [x] 2.4 Write property test: Printer Round-Trip (Property 10)
    - **Property 10: Printer Round-Trip**
    - Use hypothesis to generate valid DAG objects
    - Assert `parse(pretty_print(d))` is structurally equal to `d`
    - **Validates: Requirements 10.5**

- [x] 3. Implement DAG validation and query functions
  - [x] 3.1 Implement DAG acyclicity validation
    - Implement `DAG.validate_acyclic()` using `networkx.is_directed_acyclic_graph` or topological sort
    - Raise `CycleDetectedError` with the cycle path on failure
    - _Requirements: 2.2_

  - [x] 3.2 Implement `downstream(task)` and `upstream(task)` query functions
    - Use networkx `descendants` and `ancestors` on the DiGraph
    - Return `set[str]` of reachable TaskNode names
    - _Requirements: 2.8_

  - [x] 3.3 Write property test: DAG Acyclicity (Property 12)
    - **Property 12: DAG Acyclicity**
    - Use hypothesis to generate random directed graphs; assert `validate_acyclic()` raises iff networkx detects a cycle
    - **Validates: Requirements 2.2**

  - [x] 3.4 Write unit tests for downstream/upstream queries
    - Test with known DAGs: linear chain, diamond, disconnected components
    - _Requirements: 2.8_

- [x] 4. Implement DAG_Generator ecFlow emission
  - [x] 4.1 Implement ecFlow Suite_Definition emission (`dag_generator.py`)
    - Use `ecflow.Defs`, `Suite`, `Family`, `Task`, `Trigger`, `Event`, `Meter`, `RepeatDate` Python API
    - Emit `.def` file to `<EXPDIR>/ecf/defs/<suite_name>.def`
    - Support all dependency primitives: trigger, complete, event, meter, time, date, cron, boolean compositions
    - Support inter-cycle dependencies via `RepeatDate` constructs
    - _Requirements: 1.2, 2.1, 2.3, 2.4_

  - [x] 4.2 Implement per-task `.ecf` script generation
    - Render `task.ecf.j2` for each TaskNode using the Template_Renderer
    - Include platform-specific scheduler directives (PBS for WCOSS2, Slurm for others)
    - Write to `<EXPDIR>/ecf/scripts/<family_path>/<task_name>.ecf`
    - _Requirements: 1.2, 12.5_

  - [x] 4.3 Write property test: Definition Fidelity (Property 13)
    - **Property 13: Definition Fidelity**
    - Assert set of `(family-path, task-name)` in emitted `Defs` equals set of TaskNodes in source DAG
    - **Validates: Requirements 10.7**

  - [x] 4.4 Write property test: ecFlow Round-Trip (Property 11)
    - **Property 11: ecFlow Round-Trip**
    - Assert `Defs.save_as_defs(path)` → `Defs(path)` produces structurally equal `Defs`
    - **Validates: Requirements 10.8**

- [x] 5. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 6. Implement Template_Renderer
  - [x] 6.1 Implement `TemplateRenderer` class (`template_renderer.py`)
    - Wrap `wxflow.parse_j2yaml` with `searchpath`, `strict` mode, and `allow_missing=False`
    - Implement `render_file(src, dst)` and `render_tree(src_dir, dst_dir)`
    - Configure searchpath: `[dev/parm/config/<app>/, dev/parm/config/, dev/parm/, dev/workflow/]`
    - Support template inheritance (`{% extends %}`, `{% block %}`)
    - _Requirements: 4.1, 4.2, 4.3, 9.1_

  - [x] 6.2 Implement strict undefined variable detection
    - On undefined variable, emit FATAL ERROR with file path, line number, and variable name
    - _Requirements: 4.4_

  - [x] 6.3 Implement shell variable preservation
    - Detect `${VAR}` patterns matching `\$\{[A-Z_][A-Z0-9_]*\}` and exclude from Jinja2 resolution
    - Preserve them verbatim for runtime shell expansion
    - _Requirements: 4.10_

  - [x] 6.4 Implement YAML Pretty_Printer for rendered configs
    - Implement `save_as_yaml(cfg, path)` with `sort_keys=False` for canonical serialization
    - _Requirements: 4.8_

  - [x] 6.5 Write property test: YAML Round-Trip (Property 9 variant for Template_Renderer)
    - Assert `parse_yaml(pretty_print(cfg))` returns a tree equal to `cfg`
    - **Validates: Requirements 4.9**

  - [x] 6.6 Write unit tests for Template_Renderer
    - Test strict mode undefined detection, shell variable preservation, nested includes, template inheritance
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.10_

- [x] 7. Implement EE2_Compliance_Scanner
  - [x] 7.1 Implement scanner module (`ee2_scanner.py`)
    - Implement checks for categories: `error_handling`, `environment_variables`, `file_naming`, `shebang_compliance`
    - `error_handling`: verify `err_chk` after executables, `err_exit` on failure
    - `environment_variables`: verify `DATA`, `cycle`, `PDY`, `NET`, `RUN`, `COMIN`, `COMOUT`, `pgmout`, `jobid` are set
    - `file_naming`: verify J-Jobs match `JAAAAA`, ex-scripts match `exaaaaa.sh`
    - `shebang_compliance`: verify `#!/bin/bash` or `#!/usr/bin/env python3`
    - Emit FATAL ERROR with category, file, and description on violation
    - _Requirements: 11.6, 8.6_

  - [x] 7.2 Write unit tests for EE2_Compliance_Scanner
    - Test with known-good and known-bad scripts for each category
    - _Requirements: 11.6_

- [x] 8. Implement Deployment_Tool pipeline
  - [x] 8.1 Implement pipeline orchestration (`pipeline.py`)
    - Implement the 8-stage pipeline: validate → build context → render templates → stage files → generate DAG → EE2 scan → manifest → seal
    - Wire together Template_Renderer, DAG_Generator, file_stager, ee2_scanner, manifest, seal modules
    - _Requirements: 3.1, 8.1, 8.2_

  - [x] 8.2 Implement input validation stage
    - Check git state, verify wxflow/uwtools versions match pinned versions
    - Refuse if EXPDIR already exists with a manifest (immutability guard)
    - Emit FATAL ERROR referencing existing Snapshot_ID if write attempted to sealed EXPDIR
    - _Requirements: 3.5, 9.5_

  - [x] 8.3 Implement deployment context builder
    - Assemble Jinja2 context dict from Workflow_Configuration YAML, platform, version, git metadata
    - Include `PDY`, `cyc`, `NET`, `RUN`, `MODE`, `MACHINE`, `model_ver`, `EXPDIR`, `COMROOT`
    - _Requirements: 4.1_

  - [x] 8.4 Implement file stager (`file_stager.py`)
    - Use uwtools `uw fs copy` API to stage non-template files from `dev/` to EXPDIR
    - Implement source-to-target mapping per design table
    - Exclude `dev/ci/`, `dev/ctests/` by default; include only if in `--allowlist`
    - _Requirements: 8.2, 8.7, 8.8, 9.2_

  - [x] 8.5 Implement manifest generator (`manifest.py`)
    - Compute SHA-256 of every file under EXPDIR
    - Write `manifest.yaml` with Snapshot_ID, git commit, deployment metadata, and per-file hashes
    - Snapshot_ID format: `<semver>+<sha256_prefix_12>` of manifest content
    - _Requirements: 3.3, 3.6_

  - [x] 8.6 Implement EXPDIR sealing (`seal.py`)
    - Set all regular files to mode `0444`, all directories to mode `0555`
    - Write `workflow/provenance.yaml` with git remote, commit, branch, user, host, timestamp, config
    - _Requirements: 3.4, 13.4_

  - [x] 8.7 Write property test: Deployment Determinism (Property 1)
    - **Property 1: Deployment Determinism**
    - Deploy twice from same git state + config + platform, assert manifests are byte-identical
    - **Validates: Requirements 3.8**

  - [x] 8.8 Write property test: Manifest Integrity (Property 2)
    - **Property 2: Manifest Integrity**
    - After deployment, recompute SHA-256 of every file, assert matches manifest
    - **Validates: Requirements 3.7**

  - [x] 8.9 Write property test: No Unresolved Tokens (Property 14)
    - **Property 14: No Unresolved Tokens**
    - Grep all rendered files for `{{`, `{%`, `{#`; assert none found
    - **Validates: Requirements 4.6**

- [x] 9. Implement CLI entry point
  - [x] 9.1 Implement `deploy_workflow` CLI (`deploy.py`)
    - Parse arguments: `--config`, `--platform`, `--expdir`, `--version`, `--allowlist`, `--dry-run`
    - Validate `--platform` against allowed set: WCOSS2, HERA, HERCULES, ORION, GAEAC6, DERECHO, URSA, AWSPW, AZUREPW, GOOGLEPW, CONTAINER
    - Invoke `pipeline.run()` with parsed arguments
    - On `--dry-run`, validate without writing
    - Emit Rocoto deprecation FATAL ERROR if Rocoto code path invoked
    - _Requirements: 1.5, 3.1, 12.1_

- [x] 10. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 11. Implement Universal_Wrapper
  - [x] 11.1 Create `dev/ush/universal_wrapper.sh.j2` template
    - Implement `set -x`, `PS4='+ $SECONDS + '`, `umask 022`, trap ERR/EXIT
    - Platform detection via `${MACHINE}` or `detect_machine.sh`
    - Source `<EXPDIR>/env/${MACHINE}.env` with task name argument; FATAL ERROR if missing
    - WCOSS2 `envir` guard: refuse if `${envir}` not in `prod`, `para`, `test`
    - Create ephemeral `${DATAROOT}/${jobid}`, export `DATA`, set `pgmout=OUTPUT.$$`
    - Execute JJob passed as `$1`; on non-zero exit call `err_exit` with JJob name, jobid, exit status
    - Cleanup `${DATA}` unless `KEEPDATA=YES`
    - Emit structured JSON lifecycle log records (init, start, succeeded, failed, aborted, complete)
    - _Requirements: 5.1, 5.2, 5.3, 5.7, 6.1, 6.2, 6.3, 6.4, 6.6, 6.8, 11.8, 12.4_

  - [x] 11.2 Create backward-compatibility shims
    - Retain `jjob_header.sh`, `jjob_standard_vars.sh`, `jjob_shell_setup.sh` as thin shims that source `universal_wrapper.sh` internals
    - Ensure existing J-Jobs continue to work without per-job edits
    - _Requirements: 6.9_

  - [x] 11.3 Implement `log_task_event.py` utility
    - Write structured lifecycle events to `<EXPDIR>/workflow/state.db` (SQLite)
    - Schema: `task_events` table with snapshot_id, git_commit, cycle, family_path, task_name, attempt, scheduler_job_id, state, exit_status, timestamp, duration_seconds
    - Create indexes on `(cycle, task_name)` and `(state)`
    - _Requirements: 13.1, 13.2_

  - [x] 11.4 Write unit tests for Universal_Wrapper
    - Test ephemeral directory creation/cleanup, env sourcing, error handling, lifecycle logging
    - _Requirements: 5.1, 5.2, 6.2, 6.4, 6.6_

- [x] 12. Implement Atomic_Publish
  - [x] 12.1 Create `dev/ush/atomic_publish.sh`
    - Stage files to `${COMOUT}/.staging/${jobid}/`
    - Verify all files non-empty (and hash-check where applicable)
    - Atomic `mv` to final `${COMOUT}` location
    - On verification failure, `err_exit` and leave COMOUT unchanged
    - `dbn_alert` only after file is at final location and `${SENDDBN^^}` equals `YES`
    - Use `cpfs` for inter-filesystem copies per EE2
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

  - [x] 12.2 Write property test: Atomicity (Property 5)
    - **Property 5: Atomicity**
    - Simulate partial failure during staging, verify no partial files in COMOUT
    - **Validates: Requirements 7.6**

- [x] 13. Implement Rocoto decommission
  - [x] 13.1 Remove Rocoto source trees and tooling
    - Delete `dev/workflow/rocoto/` directory tree
    - Delete `dev/job_cards/rocoto/` directory tree
    - Delete `dev/workflow/rocoto_viewer.py`
    - Delete `dev/workflow/setup_buildxml.py`
    - _Requirements: 14.1, 14.2, 14.4_

  - [x] 13.2 Modify `setup_workflow.py` to remove Rocoto
    - Remove `rocoto` subparser, `rocoto_xml_factory`, and all Rocoto-conditioned branches
    - Add deprecation guard that emits FATAL ERROR referencing Requirement 1 if Rocoto path invoked
    - _Requirements: 14.3, 1.5_

  - [x] 13.3 Update CI cases and documentation
    - Replace Rocoto CI cases under `dev/ci/cases/` with ecFlow equivalents
    - Remove `-c` crontab option from `generate_workflows.sh`
    - Rename `dev/workflow/README_ecflow.md` to `dev/workflow/README.md`
    - Update documentation references from Rocoto to ecFlow-only
    - _Requirements: 14.5, 14.6, 14.7_

- [x] 14. Implement multi-platform support
  - [x] 14.1 Implement platform-conditioned rendering in Deployment_Tool
    - Render `dev/env/${PLATFORM}.env` to `<EXPDIR>/env/${PLATFORM}.env`
    - Render `dev/parm/config/<app>/config.resources.${PLATFORM}` to EXPDIR
    - Copy platform modulefiles to `<EXPDIR>/modulefiles/${PLATFORM}/`
    - Ensure non-platform files (J-Jobs, ex-scripts, ush) are identical across platforms
    - _Requirements: 12.2, 12.3_

  - [x] 14.2 Write property test: Platform Isolation (Property 8)
    - **Property 8: Platform Isolation**
    - Deploy for HERA and WCOSS2, diff file trees; assert differences only in platform-conditioned paths
    - **Validates: Requirements 12.3**

- [x] 15. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 16. Integration wiring and end-to-end validation
  - [x] 16.1 Wire full deployment pipeline end-to-end
    - Integrate all components: CLI → pipeline → renderer → stager → DAG generator → scanner → manifest → seal
    - Test with `gfs_forecast_only.yaml` minimal config
    - Verify EXPDIR structure matches NCO layout (`jobs/`, `scripts/`, `ush/`, `parm/`, `sorc/`, `fix/`, `ecf/`, `versions/`, `modulefiles/`)
    - _Requirements: 3.1, 3.2, 8.1, 8.2_

  - [x] 16.2 Create ecFlow CI smoke test cases
    - Create `dev/ci/cases/C48_ATM_gfs_fcst_only.yaml` → ecFlow deployment + `ecflow_client` smoke
    - Create `dev/ci/cases/C48_S2SW_gfs_cycled.yaml` → full cycled DAG validation
    - Each case: deploy → load def → verify task count matches config
    - _Requirements: 14.5, 1.1_

  - [x] 16.3 Write integration test: Self-Containment (Property 4)
    - **Property 4: Self-Containment**
    - Deploy, then `chmod 000 dev/`, run ecFlow smoke test from EXPDIR alone
    - **Validates: Requirements 3.1**

  - [x] 16.4 Write integration test: Immutability (Property 3)
    - **Property 3: Immutability**
    - After sealing, attempt write to EXPDIR file, expect EPERM
    - **Validates: Requirements 3.4**

- [x] 17. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Remember to use the agentcore-mcp-rag when dealing with questions about global-workflow or EE2 compliance
- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate universal correctness properties from the design document
- Unit tests validate specific examples and edge cases
- Python is used for all deployment tooling (`dev/workflow/deployment/`)
- Bash is used for Universal_Wrapper, Atomic_Publish, and shell utilities
- `hypothesis` is the property-based testing framework for Python components
- `pytest` is the test runner
- `networkx` is used for DAG graph operations (cycle detection, reachability)
- The ecFlow Python API (`ecflow` package) is used for Suite_Definition emission

## Task Dependency Graph

```json
{
  "waves": [
    { "id": 0, "tasks": ["1.1", "1.2", "1.3"] },
    { "id": 1, "tasks": ["2.1", "7.1"] },
    { "id": 2, "tasks": ["2.2", "2.3", "3.1", "3.2", "6.1", "7.2"] },
    { "id": 3, "tasks": ["2.4", "3.3", "3.4", "6.2", "6.3", "6.4"] },
    { "id": 4, "tasks": ["4.1", "6.5", "6.6"] },
    { "id": 5, "tasks": ["4.2", "4.3", "4.4"] },
    { "id": 6, "tasks": ["8.1", "8.2", "8.3", "8.4", "8.5", "8.6"] },
    { "id": 7, "tasks": ["8.7", "8.8", "8.9", "9.1"] },
    { "id": 8, "tasks": ["11.1", "11.2", "11.3", "12.1", "13.1"] },
    { "id": 9, "tasks": ["11.4", "12.2", "13.2", "13.3", "14.1"] },
    { "id": 10, "tasks": ["14.2", "16.1", "16.2"] },
    { "id": 11, "tasks": ["16.3", "16.4"] }
  ]
}
```

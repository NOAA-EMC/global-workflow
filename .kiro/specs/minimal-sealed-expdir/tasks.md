# Implementation Plan: Minimal Sealed EXPDIR

## Overview

This plan implements DAG-filtered staging, deploy-time config conditioning, model input pre-rendering, and completeness verification for the deployment pipeline. The implementation follows a dependency-driven order: foundation registry first, then core algorithm, then consumers of the registry and algorithm, then integration wiring, and finally property-based tests proving correctness.

All new modules live under `dev/workflow/deployment/` and tests under `dev/workflow/tests/`. The test runner is `.venv/bin/python -m pytest tests/ -q` from `dev/workflow`.

## Tasks

- [x] 1. Deploy_Time_Variable registry (foundation)
  - [x] 1.1 Create `dev/workflow/deployment/deploy_time_vars.py`
    - Define `DeployTimeVariable` dataclass with `name`, `source`, `description` fields
    - Define `DEPLOY_TIME_REGISTRY` list containing all 15 deploy-time variables (RUN, NET, CASE, CASE_ENS, MACHINE, CDUMP, NMEM_ENS, APP, CCPP_SUITE, DO_COUPLED, DO_WAVE, DO_OCN, DO_ICE, DO_AERO, REPLAY_ICS)
    - Implement `get_deploy_time_values(context: dict) -> dict[str, str]` that extracts values from the pipeline context
    - Export the registry and accessor from the module
    - _Requirements: 11.1, 11.2, 11.3, 11.4_

  - [x] 1.2 Write unit tests for deploy_time_vars registry
    - Create `dev/workflow/tests/test_deploy_time_vars.py`
    - Test that `get_deploy_time_values` extracts only registered variables from a context dict
    - Test that missing context keys are silently skipped (no KeyError)
    - Test that all 15 required variables are present in the registry
    - Test that the registry is deterministic (same order on repeated access)
    - _Requirements: 11.1, 11.3, 11.4_

- [x] 2. DAG_Filter (core reachability algorithm)
  - [x] 2.1 Create `dev/workflow/deployment/dag_filter.py` with `DAGReachabilitySet` dataclass
    - Define `DAGReachabilitySet` with `jjobs`, `ex_scripts`, `ush_scripts`, `config_files`, `warnings` fields and `is_valid` property
    - Use `frozenset` for immutable sets and `tuple` for warnings
    - Include statistics fields (`total_available_*`) for size reduction reporting
    - Include `contains_*` helper methods
    - _Requirements: 1.1, 2.1, 3.1, 4.1, 9.1_

  - [x] 2.2 Implement `DAGFilter` class — Layer 1 (J-Job extraction from Workflow_YAML)
    - Implement `__init__(self, dev_root, workflow_yaml, platform)` constructor
    - Implement `extract_jjobs_from_yaml()` walking `families[].tasks[].jjob`
    - Validate each extracted J-Job exists in `dev/jobs/`; raise `PipelineError` for missing ones
    - _Requirements: 1.1, 1.3, 1.4, 1.5_

  - [x] 2.3 Implement `DAGFilter` — Layer 2 (ex-script extraction from J-Jobs)
    - Define `_EX_SCRIPT_PATTERNS` regex list for `${SCRglobal}/ex*.sh`, assignment patterns, and export patterns
    - Implement `extract_ex_scripts(jjobs)` parsing each J-Job file for ex-script invocations
    - Raise `PipelineError` for referenced ex-scripts not found in `dev/scripts/`
    - _Requirements: 2.1, 2.2, 2.3, 2.4_

  - [x] 2.4 Implement `DAGFilter` — Layer 3 (transitive ush script resolution)
    - Define `_USH_SOURCE_PATTERNS` regex list for `source "${USH*}/..."` and dot-source patterns
    - Implement `extract_ush_scripts(ex_scripts)` with BFS transitive closure using a visited set
    - Handle circular dependencies gracefully (emit WARNING, do not loop)
    - Emit WARNING for missing ush scripts (non-fatal, may be conditionally sourced)
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5_

  - [x] 2.5 Implement `DAGFilter` — Layer 4 (config file extraction)
    - Define `_JJOB_HEADER_PATTERN` regex for `jjob_header.sh -c "base fcst"` parsing
    - Define `_UNCONDITIONAL_CONFIGS` set (`config.base.j2`, `config.base`, `config.com`)
    - Implement `extract_config_files(jjobs)` mapping basenames to actual config files
    - Include platform-specific resource file (`config.resources.<PLATFORM>`)
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5_

  - [x] 2.6 Implement `DAGFilter.compute_reachability()` orchestrator
    - Wire all four layers together into the public `compute_reachability()` method
    - Populate statistics fields (total_available counts from `dev/` directories)
    - Return a complete `DAGReachabilitySet` with frozen collections
    - _Requirements: 1.1, 2.1, 3.1, 4.1, 9.1, 9.2, 9.3, 9.4_

  - [x] 2.7 Write unit tests for DAG_Filter
    - Create `dev/workflow/tests/test_dag_filter.py`
    - Test J-Job extraction from a minimal workflow YAML fixture
    - Test ex-script extraction with known J-Job content patterns
    - Test ush script transitive resolution including cycles
    - Test config extraction from jjob_header -c patterns
    - Test `config.base` always included regardless of tasks
    - Test FATAL ERROR on missing J-Job and missing ex-script
    - Test WARNING on missing ush script
    - _Requirements: 1.1–1.5, 2.1–2.4, 3.1–3.5, 4.1–4.5_

- [x] 3. Checkpoint — DAG_Filter foundation
  - Ensure all tests pass, ask the user if questions arise.

- [x] 4. Config_Conditioner (depends on registry)
  - [x] 4.1 Create `dev/workflow/deployment/config_conditioner.py`
    - Define `ConditionerResult` dataclass with `output`, `eliminated_branches`, `preserved_conditionals`, `is_valid_shell` fields
    - Implement `ConfigConditioner.__init__(deploy_time_vars: dict[str, str])` consuming the registry values
    - Implement `_is_deploy_time_expression(expr)` checking if all variables in an expression are deploy-time
    - _Requirements: 5.1, 5.4, 5.7_

  - [x] 4.2 Implement `ConfigConditioner.condition_file()` — if-block handling
    - Define `_IF_BLOCK_PATTERN` and `_CONDITIONAL_VAR_PATTERN` regexes
    - Implement if/elif/else/fi block parsing and evaluation for deploy-time-only conditionals
    - Preserve unchanged any conditional testing runtime variables or mixed variables
    - Insert `# Resolved: VAR=value at deploy time` comment for eliminated branches
    - _Requirements: 5.1, 5.2, 5.3, 5.5, 5.6_

  - [x] 4.3 Implement `ConfigConditioner.condition_file()` — case-block handling
    - Define `_CASE_BLOCK_PATTERN` regex for `case ${VAR} in ... esac` blocks
    - Evaluate case patterns against known deploy-time values
    - Preserve case blocks testing runtime variables unchanged
    - _Requirements: 5.1, 5.2, 5.3_

  - [x] 4.4 Implement `ConfigConditioner.validate_shell_syntax()`
    - Run `bash -n` on conditioned output via subprocess
    - Return boolean validity; capture stderr for error reporting
    - Integrate validation into `condition_file()` flow — raise `PipelineError` on invalid output
    - _Requirements: 5.8_

  - [x] 4.5 Write unit tests for Config_Conditioner
    - Create `dev/workflow/tests/test_config_conditioner.py`
    - Test `case ${RUN}` pattern with known RUN value resolves to correct branch
    - Test `if [[ "${PDY}" ]]` runtime conditional preserved unchanged
    - Test mixed deploy-time + runtime conditional preserved unchanged
    - Test eliminated branch gets resolution comment
    - Test output passes `bash -n` validation
    - Test nested conditionals handled correctly
    - _Requirements: 5.1–5.8_

- [x] 5. Model_Input_Renderer enhancement (depends on registry + DAG filter)
  - [x] 5.1 Enhance `dev/workflow/deployment/model_config_renderer.py` with DAG-aware rendering
    - Add `render_for_dag(model_context, expdir, reachability_set)` method to `ModelConfigRenderer`
    - Determine active UFS components from reachability set (skip wave/ if no wave tasks reachable)
    - Render only templates for active components under `dev/parm/ufs/{fv3,ocean,ice,wave,gocart}/`
    - _Requirements: 6.1, 6.3, 6.7_

  - [x] 5.2 Implement zero-token verification and shell-var preservation checks
    - Add `verify_no_unresolved_tokens(rendered_files)` scanning for `{{`, `{%`, `{#` patterns
    - Add `verify_shell_vars_preserved(rendered_files, runtime_vars)` checking `${DATA}`, `${ROTDIR}` etc. survive
    - Raise `PipelineError` with file, line number, and token on any unresolved Jinja2 token
    - _Requirements: 6.4, 6.5, 6.6_

  - [x] 5.3 Integrate uwtools for Fortran namelist rendering
    - Add `_render_fortran_namelist(template_path, context, output_path)` delegating to `uwtools.api.template.render`
    - Add post-render validation with format-specific validators (namelist, MOM6 parameter, ESMF config)
    - Ensure Fortran namelist formatting conventions preserved (`.true.`/`.false.`, proper quoting)
    - _Requirements: 6.1, 6.2, 14.1, 14.2, 14.3, 14.4_

  - [x] 5.4 Write unit tests for enhanced Model_Input_Renderer
    - Create `dev/workflow/tests/test_model_input_renderer.py`
    - Test DAG-aware rendering skips inactive components
    - Test zero-token verification catches unresolved `{{` tokens
    - Test shell variable `${DATA}` preserved in rendered output
    - Test Fortran namelist output is parseable
    - Test FATAL ERROR on undefined Jinja2 variable
    - _Requirements: 6.1–6.7, 14.1–14.4_

- [x] 6. Checkpoint — Core components complete
  - Ensure all tests pass, ask the user if questions arise.

- [x] 7. Completeness_Verifier (depends on DAG filter output)
  - [x] 7.1 Create `dev/workflow/deployment/completeness_verifier.py`
    - Define `CompletenessResult` dataclass with `passed`, `missing_ex_scripts`, `missing_ush_scripts`, `missing_configs` fields
    - Implement `CompletenessVerifier.__init__(expdir: Path)` constructor
    - Implement `verify()` orchestrating all three cross-reference checks
    - Raise `PipelineError` on any missing dependency (FATAL)
    - _Requirements: 8.1, 8.2, 8.3, 8.4_

  - [x] 7.2 Implement cross-reference checks
    - Implement `_check_jjob_ex_script_refs()` — parse staged J-Jobs for ex-script references, verify in `scripts/`
    - Implement `_check_ex_script_ush_refs()` — parse staged ex-scripts for ush source refs, verify in `ush/`
    - Implement `_check_config_refs()` — parse staged J-Jobs for config requirements, verify in `parm/config/`
    - Reuse regex patterns from DAG_Filter for consistency
    - _Requirements: 8.1, 8.2, 8.3_

  - [x] 7.3 Write unit tests for Completeness_Verifier
    - Create `dev/workflow/tests/test_completeness_verifier.py`
    - Test passing verification with complete EXPDIR fixture
    - Test detection of missing ex-script referenced by a J-Job
    - Test detection of missing ush script sourced by an ex-script
    - Test FATAL ERROR message format includes missing file and referencing script
    - _Requirements: 8.1, 8.2, 8.3, 8.4_

- [x] 8. Pipeline integration (wires everything together with --dag-filter flag)
  - [x] 8.1 Add `--dag-filter` CLI flag to `dev/workflow/deploy.py`
    - Add `--dag-filter` argument (store_true, default False) to the argument parser
    - Pass `dag_filter=` parameter through to `pipeline.run()`
    - _Requirements: 13.1, 13.2, 13.4_

  - [x] 8.2 Integrate DAG_Filter into `pipeline.py` Stage 4a
    - Import `DAGFilter`, `DAGReachabilitySet` from `dag_filter` module
    - Add `dag_filter: bool = False` parameter to `run()`
    - When enabled: instantiate `DAGFilter`, call `compute_reachability()`, pass result to file stager
    - When disabled: pass `reachability=None` to file stager (full-copy behavior)
    - Log DAG filter status at start of staging phase
    - _Requirements: 13.1, 13.2, 13.3, 13.4_

  - [x] 8.3 Integrate Config_Conditioner into pipeline Stage 4c
    - Import `ConfigConditioner` from `config_conditioner` module
    - Add `_stage_condition_configs(expdir, context)` helper function
    - Instantiate conditioner with `get_deploy_time_values(context)`
    - Process all staged config files (runs regardless of `--dag-filter` flag)
    - _Requirements: 5.1, 13.3_

  - [x] 8.4 Integrate Completeness_Verifier into pipeline Stage 4d
    - Import `CompletenessVerifier` from `completeness_verifier` module
    - Run verification after staging when `--dag-filter` is enabled
    - Raise `PipelineError` on verification failure
    - _Requirements: 8.3, 8.4_

  - [x] 8.5 Integrate size reduction reporting
    - Implement `_log_size_reduction(dev_root, reachability)` helper
    - Define `SizeReductionReport` dataclass with staged/total counts
    - Log reduction statistics after successful DAG-filtered staging
    - _Requirements: 9.1, 9.2, 9.3, 9.4_

  - [x] 8.6 Integrate enhanced Model_Input_Renderer into pipeline Stage 3
    - Call `render_for_dag()` when reachability set is available
    - Call `verify_no_unresolved_tokens()` on all rendered model inputs
    - Model input pre-rendering applies regardless of `--dag-filter` flag
    - _Requirements: 6.1, 6.7, 13.3_

  - [x] 8.7 Write integration tests for pipeline with --dag-filter
    - Create `dev/workflow/tests/test_pipeline_dag_filter.py`
    - Test full pipeline with `--dag-filter` enabled produces minimal EXPDIR
    - Test full pipeline with `--dag-filter` disabled produces full EXPDIR (backward compat)
    - Test config conditioning runs in both modes
    - Test size reduction report logged when filtering enabled
    - Test FATAL ERROR propagation from DAG_Filter and Completeness_Verifier
    - _Requirements: 13.1, 13.2, 13.3, 13.4, 9.1–9.4_

- [x] 9. Checkpoint — Pipeline integration complete
  - Ensure all tests pass, ask the user if questions arise.

- [x] 10. Forecast runtime sealed-copy path update
  - [x] 10.1 Update forecast ex-script model input staging
    - Modify `dev/ush/forecast_postdet.sh` (or relevant forecast ush script) to use `cpreq` from sealed EXPDIR
    - Add pre-flight existence check with descriptive FATAL ERROR before each `cpreq`
    - Replace runtime `parsing_namelists_*.sh` invocations with `cpreq` from `${EXPDIR}/parm/ufs/<component>/`
    - Follow EE2 pattern: existence check → FATAL ERROR message → `cpreq`
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

  - [x] 10.2 Write unit tests for forecast sealed-copy path
    - Create or extend `dev/workflow/tests/test_forecast_postdet_cpreq.py`
    - Test that the script uses `cpreq` (not `cp` or `cpfs`) for model inputs
    - Test that pre-flight existence checks are present for each pre-rendered file
    - Test that FATAL ERROR messages name the missing file path
    - Test that no `parsing_namelists_*.sh` invocations remain for pre-rendered inputs
    - _Requirements: 7.1–7.5_

- [x] 11. Property-based tests proving correctness properties
  - [x] 11.1 Write property test for DAG Filter Soundness (Property 1)
    - Create `dev/workflow/tests/test_dag_filter_property.py`
    - **Property 1: DAG Filter Soundness (no false exclusions)**
    - Generate random workflow YAMLs with known jjob sets; verify all referenced jjobs appear in output
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 1.1, 1.3, 2.1, 2.3**

  - [x] 11.2 Write property test for DAG Filter Completeness (Property 2)
    - **Property 2: DAG Filter Completeness (no false inclusions)**
    - Generate random available J-Job sets larger than referenced; verify unreferenced are excluded
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 1.2, 2.2**

  - [x] 11.3 Write property test for Transitive Ush Reachability (Property 3)
    - **Property 3: Transitive Ush Reachability**
    - Generate random dependency graphs (including cycles); verify transitive closure correctness
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 3.1, 3.2, 3.3, 3.4**

  - [x] 11.4 Write property test for Config Conditioner Preserves Runtime (Property 4)
    - Create `dev/workflow/tests/test_config_conditioner_property.py`
    - **Property 4: Config Conditioner Preserves Runtime Conditionals**
    - Generate config content with runtime-variable conditionals; verify byte-identical passthrough
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 5.3, 5.6, 5.7**

  - [x] 11.5 Write property test for Config Conditioner Evaluates Deploy-Time (Property 5)
    - **Property 5: Config Conditioner Evaluates Deploy-Time Conditionals**
    - Generate conditionals on deploy-time vars with known values; verify correct branch selection
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 5.1, 5.2, 5.5**

  - [x] 11.6 Write property test for Config Conditioner Output Validity (Property 6)
    - **Property 6: Config Conditioner Output Validity**
    - Generate random config inputs; condition; verify `bash -n` passes
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 5.8**

  - [x] 11.7 Write property test for Model Input Zero-Token Guarantee (Property 7)
    - Create `dev/workflow/tests/test_model_input_property.py`
    - **Property 7: Model Input Zero-Token Guarantee**
    - Generate random complete contexts; render templates; scan for unresolved tokens
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 6.4, 14.1**

  - [x] 11.8 Write property test for Model Input Round-Trip Fidelity (Property 8)
    - **Property 8: Model Input Round-Trip Fidelity**
    - Generate random model contexts; render; parse with format validator; verify no errors
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 14.1, 14.2, 14.3, 14.4**

  - [x] 11.9 Write property test for Completeness Verifier (Property 9)
    - Create `dev/workflow/tests/test_completeness_property.py`
    - **Property 9: Completeness Verifier Detects All Missing Dependencies**
    - Generate random EXPDIRs with intentional gaps; verify detection
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 8.1, 8.2, 8.3**

  - [x] 11.10 Write property test for Deployment Idempotence (Property 10)
    - Create `dev/workflow/tests/test_idempotence_property.py`
    - **Property 10: Deployment Idempotence**
    - Deploy same config twice; compare manifests for byte-identical output
    - Use `@settings(max_examples=50)` (heavier test)
    - **Validates: Requirements 12.1, 12.2, 12.3, 12.4**

  - [x] 11.11 Write property test for Unconditional Config Inclusion (Property 11)
    - Add to `dev/workflow/tests/test_dag_filter_property.py`
    - **Property 11: Unconditional Config Inclusion**
    - Generate random workflow YAMLs; verify config.base always present in output
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 4.4**

  - [x] 11.12 Write property test for JAAAAA Naming Enforcement (Property 12)
    - Add to `dev/workflow/tests/test_dag_filter_property.py`
    - **Property 12: JAAAAA Naming Enforcement**
    - Generate random filenames; verify naming validator accepts/rejects correctly
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 1.4, 10.2**

  - [x] 11.13 Write property test for Size Reduction Accuracy (Property 13)
    - Create `dev/workflow/tests/test_size_reduction_property.py`
    - **Property 13: Size Reduction Accuracy**
    - Generate random available/staged sets; verify reported counts match actual file counts
    - Use `@settings(max_examples=100)`
    - **Validates: Requirements 9.1, 9.2, 9.3, 9.4**

- [x] 12. Final checkpoint — Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate universal correctness properties from the design document
- Unit tests validate specific examples and edge cases
- The test runner command is `.venv/bin/python -m pytest tests/ -q` from `dev/workflow`
- Do NOT call the live RAG server in any committed/gate/CI code
- Follow EE2 SME-corrected patterns (cpreq/err_chk/err_exit, no set -e) in shell scripts
- Config conditioning and model input pre-rendering apply regardless of `--dag-filter` flag
- The `--dag-filter` flag defaults to off for backward compatibility during transition

## Task Dependency Graph

```json
{
  "waves": [
    { "id": 0, "tasks": ["1.1"] },
    { "id": 1, "tasks": ["1.2", "2.1"] },
    { "id": 2, "tasks": ["2.2", "2.3", "2.4", "2.5"] },
    { "id": 3, "tasks": ["2.6"] },
    { "id": 4, "tasks": ["2.7", "4.1"] },
    { "id": 5, "tasks": ["4.2", "4.3", "5.1"] },
    { "id": 6, "tasks": ["4.4", "4.5", "5.2", "5.3"] },
    { "id": 7, "tasks": ["5.4", "7.1"] },
    { "id": 8, "tasks": ["7.2"] },
    { "id": 9, "tasks": ["7.3", "8.1"] },
    { "id": 10, "tasks": ["8.2", "8.3", "8.4", "8.5", "8.6"] },
    { "id": 11, "tasks": ["8.7", "10.1"] },
    { "id": 12, "tasks": ["10.2"] },
    { "id": 13, "tasks": ["11.1", "11.2", "11.3", "11.4", "11.5", "11.6", "11.7", "11.8", "11.9", "11.10", "11.11", "11.12", "11.13"] }
  ]
}
```

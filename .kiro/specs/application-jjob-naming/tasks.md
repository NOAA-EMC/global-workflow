# Implementation Plan: Application J-Job Naming

## Overview

This plan implements application-specific J-Job naming in the deployment pipeline. The work introduces a `PrefixRegistry` configuration and `NameResolver` module, modifies the `DAGFilter` and `FileStager` to use resolution-based renaming, updates the `gcafs.yaml` Workflow_YAML to reference application-named J-Jobs, and adds unconditional artifact staging for linking scripts. All code is Python, tested with pytest and Hypothesis.

## Tasks

- [x] 1. Create Prefix Registry configuration and dataclass
  - [x] 1.1 Create `dev/workflow/deployment/prefix_registry.yaml` with the default registry mapping
    - Define the YAML structure with `registry:` top-level key
    - Include all 6 Application_Prefix → Shared_Prefix search lists per Requirement 5.2: `JGCAFS_` → [`JGLOBAL_`], `JGCDAS_` → [`JGLOBAL_`, `JGDAS_`], `JGFS_` → [`JGLOBAL_`, `JGFS_`], `JGDAS_` → [`JGLOBAL_`, `JGDAS_`], `JGEFS_` → [`JGLOBAL_`, `JGEFS_`], `JSFS_` → [`JGLOBAL_`, `JSFS_`]
    - _Requirements: 5.1, 5.2, 5.5_

  - [x] 1.2 Implement `PrefixRegistry` dataclass in `dev/workflow/deployment/name_resolver.py`
    - Create a frozen dataclass with `registry: dict[str, list[str]]`
    - Implement `load(cls, path: Path)` classmethod to parse YAML
    - Implement `default(cls)` classmethod returning built-in defaults
    - Implement `get_search_prefixes(app_prefix: str) -> list[str] | None`
    - Implement `known_prefixes() -> frozenset[str]`
    - Validate YAML structure on load: raise `PipelineError` if file missing or malformed
    - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5_

- [x] 2. Implement Name Resolver module
  - [x] 2.1 Implement `ResolvedName` dataclass and `NameResolver` class in `dev/workflow/deployment/name_resolver.py`
    - Define `ResolvedName(application_name, source_name, is_passthrough)` frozen dataclass
    - Implement `NameResolver.__init__(dev_root: Path, registry: PrefixRegistry)`
    - Implement `resolve(application_name: str) -> ResolvedName` following the 5-step algorithm:
      1. Direct check: if `dev/jobs/{application_name}` exists → pass-through
      2. Prefix identification from registry
      3. Ordered search through shared prefixes
      4. Direct fallback
      5. FATAL error with searched paths
    - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 8.1, 8.2_

  - [x] 2.2 Implement `resolve_all()` and `resolve_all_dry_run()` methods
    - `resolve_all(application_names: set[str]) -> dict[str, ResolvedName]` — raises on first failure
    - `resolve_all_dry_run(application_names: set[str]) -> DryRunReport` — accumulates all errors
    - Implement `DryRunReport` dataclass with `resolved`, `errors`, `total_count`, `resolvable_count`, `unresolvable_count`
    - Implement `DryRunReport.format_table()` for CLI output
    - _Requirements: 2.5, 7.1, 7.2, 7.3_

  - [x] 2.3 Write property test for Name Resolution Correctness (Property 1)
    - **Property 1: Name Resolution Correctness (Ordered Search, First-Match)**
    - Generate random Application_Names with registered prefixes, random filesystem states
    - Assert that the resolver returns the first match in registry-defined order
    - Assert FATAL error when no source exists at any search position
    - **Validates: Requirements 2.1, 2.2, 2.3, 2.4, 2.5, 5.3**

  - [x] 2.4 Write property test for Backward Compatibility (Property 6)
    - **Property 6: Backward Compatibility**
    - Generate Workflow_YAMLs where `jjob:` values match files directly in `dev/jobs/`
    - Assert pass-through (no rename) for shared names
    - Assert mixed-mode YAMLs process both types correctly
    - **Validates: Requirements 8.1, 8.2, 8.3**

  - [x] 2.5 Write property test for Dry-Run Completeness (Property 7)
    - **Property 7: Dry-Run Completeness**
    - Generate sets of N application names (mix of resolvable and unresolvable)
    - Assert dry-run report lists all N entries
    - Assert all unresolvable names reported (not halting on first)
    - Assert `resolvable_count + unresolvable_count == N`
    - **Validates: Requirements 7.1, 7.2, 7.3**

- [x] 3. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 4. Extend DAG Filter with Name Resolver integration
  - [x] 4.1 Modify `DAGFilter` in `dev/workflow/deployment/dag_filter.py` to accept and use `NameResolver`
    - Add `name_resolver: NameResolver | None = None` parameter to `__init__`
    - Add `jjob_source_map: dict[str, str]` field to `DAGReachabilitySet`
    - Implement `resolve_jjobs(app_names: set[str]) -> dict[str, ResolvedName]`
    - Modify `compute_reachability()` to call `resolve_jjobs()` after `extract_jjobs_from_yaml()`
    - Pass resolved `source_names` (not application_names) to `extract_ex_scripts()`
    - Store Application_Names in `DAGReachabilitySet.jjobs` (for EXPDIR staging)
    - When `name_resolver` is None, fall back to direct lookup (backward compat)
    - _Requirements: 4.1, 4.2, 4.3, 4.4_

  - [x] 4.2 Write property test for DAG Filter Resolution Integration (Property 4)
    - **Property 4: DAG Filter Resolution Integration**
    - Generate Workflow_YAMLs with Application_Names and corresponding source files
    - Assert DAG_Filter collects Application_Names from YAML
    - Assert resolution to Shared_Source_Names via Name_Resolver
    - Assert source file (not application-named file) parsed for dependencies
    - Assert both Application_Name and source_name in reachability set
    - **Validates: Requirements 4.1, 4.2, 4.3**

- [x] 5. Extend File Stager with rename-on-copy and unconditional artifact staging
  - [x] 5.1 Implement `stage_jjobs_with_rename()` in `dev/workflow/deployment/file_stager.py`
    - Accept `resolution_map: dict[str, ResolvedName]` parameter
    - For each entry: copy `dev/jobs/{source_name}` → `EXPDIR/jobs/{application_name}`
    - Deduplicate: same application_name staged exactly once
    - Distinct: two different application_names resolving to same source produce two files
    - Pass-through names (is_passthrough=True) copied without rename
    - Return `StagingResult` with count of files staged
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5_

  - [x] 5.2 Implement `stage_unconditional_artifacts()` in `dev/workflow/deployment/file_stager.py`
    - Copy `sorc/link_workflow.sh` → `EXPDIR/sorc/link_workflow.sh`
    - Copy `sorc/ufs_utils.fd/fix/link_fixdirs.sh` → `EXPDIR/sorc/ufs_utils.fd/fix/link_fixdirs.sh`
    - Preserve executable permission bits (mode 0755) using `shutil.copy2` or explicit `os.chmod`
    - Raise `StagingError` if source files are missing
    - _Requirements: 9.1, 9.2, 9.5, 9.6_

  - [x] 5.3 Write property test for Content Preservation on Rename (Property 3)
    - **Property 3: Content Preservation on Rename**
    - Generate random file content and random application/source name pairs
    - Stage via rename-on-copy
    - Assert byte content of destination == byte content of source
    - **Validates: Requirements 3.1, 6.2**

  - [x] 5.4 Write property test for Deduplication and Distinction (Property 5)
    - **Property 5: Deduplication and Distinction**
    - Generate YAMLs with duplicate application_names and shared-source pairs
    - Assert same application_name staged exactly once
    - Assert two different application_names resolving to same source produce two distinct files
    - **Validates: Requirements 3.4, 3.5**

  - [x] 5.5 Write property test for EXPDIR Naming Invariants (Property 2)
    - **Property 2: EXPDIR Naming Invariants**
    - Generate workflow deployments with application naming
    - Assert all filenames in `EXPDIR/jobs/` match `^J[A-Z][A-Z0-9_]*$`
    - Assert no file in `EXPDIR/jobs/` has `JGLOBAL_` prefix
    - **Validates: Requirements 3.2, 3.3, 6.1**

  - [x] 5.6 Write property test for Unconditional Linking Script Staging (Property 8)
    - **Property 8: Unconditional Linking Script Staging**
    - Generate various DAG-filter configurations (enabled/disabled, different YAML inputs)
    - Assert EXPDIR always contains `sorc/link_workflow.sh` and `sorc/ufs_utils.fd/fix/link_fixdirs.sh`
    - Assert executable permission bits preserved
    - **Validates: Requirements 9.1, 9.2, 9.5, 9.6**

- [x] 6. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 7. Integrate Name Resolver into pipeline and add dry-run reporting
  - [x] 7.1 Modify `dev/workflow/deployment/pipeline.py` to load PrefixRegistry and wire NameResolver
    - In `_stage_build_context()` (Stage 2): load `prefix_registry.yaml` and instantiate `PrefixRegistry`
    - Instantiate `NameResolver(dev_root, registry)` and pass to `DAGFilter`
    - In `_stage_validate()` (Stage 1): add existence check for `prefix_registry.yaml`
    - Wire `resolution_map` from DAG_Filter result into File_Stager's `stage_jjobs_with_rename()`
    - Call `stage_unconditional_artifacts()` after `stage_jjobs_with_rename()` regardless of `--dag-filter`
    - _Requirements: 2.7, 5.3, 5.4, 9.5_

  - [x] 7.2 Implement dry-run name resolution report in `dev/workflow/deployment/pipeline.py`
    - When `--dry-run` flag is active, call `resolve_all_dry_run()` instead of `resolve_all()`
    - Print `DryRunReport.format_table()` output showing application → source mappings
    - Report all unresolvable names (non-halting) and emit summary counts
    - Exit with non-zero status if any names are unresolvable
    - _Requirements: 7.1, 7.2, 7.3_

  - [x] 7.3 Write unit tests for pipeline integration
    - Test PrefixRegistry loading in validate stage
    - Test dry-run report output format
    - Test that missing prefix_registry.yaml raises FATAL in validate
    - Test end-to-end pipeline with application naming produces correct EXPDIR
    - _Requirements: 5.5, 7.1, 7.2, 7.3_

- [x] 8. Update Workflow YAML to use application-specific names
  - [x] 8.1 Update `dev/parm/workflow/gcafs.yaml` `jjob:` fields to use application-specific names
    - Tasks under the `gcdas` cycle: replace `JGLOBAL_*` with `JGCDAS_*` prefixes (e.g., `JGLOBAL_FORECAST` → `JGCDAS_FORECAST`)
    - Tasks under the `gcafs` cycle: replace `JGLOBAL_*` with `JGCAFS_*` prefixes (e.g., `JGLOBAL_FORECAST` → `JGCAFS_FORECAST`)
    - Keep `JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX` as-is (Direct_Match_Source already in `dev/jobs/`)
    - Verify all `jjob:` values conform to JAAAAA_Convention (all uppercase, starts with `J`, no extension)
    - _Requirements: 1.1, 1.2, 1.3, 1.4_

  - [x] 8.2 Write unit tests verifying gcafs.yaml migration correctness
    - Load gcafs.yaml and check all jjob values under `gcdas` cycle use `JGCDAS_` prefix
    - Load gcafs.yaml and check all jjob values under `gcafs` cycle use `JGCAFS_` prefix
    - Verify every jjob value matches `^J[A-Z][A-Z0-9_]*$` regex
    - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [x] 9. Document EE2 Scanner compatibility and add integration test
  - [x] 9.1 Add documentation comments to `dev/workflow/deployment/ee2_scanner.py` confirming application-name support
    - Document that `_JJOB_PATTERN = re.compile(r"^J[A-Z][A-Z0-9_]*$")` already accepts application-named files
    - Add module-level docstring note referencing application-jjob-naming spec
    - Confirm content validation (shebang, jjob_header, ex-script invocation) is filename-independent
    - _Requirements: 6.1, 6.2, 6.3_

  - [x] 9.2 Write integration test for end-to-end pipeline with application naming
    - Run full pipeline with gcafs.yaml and verify EXPDIR contains only application-named J-Jobs
    - Verify EE2 scan passes on the application-named EXPDIR
    - Verify `sorc/link_workflow.sh` and `sorc/ufs_utils.fd/fix/link_fixdirs.sh` are present with 0755
    - Verify backward compatibility: run pipeline with a shared-name YAML and confirm no rename
    - _Requirements: 6.1, 6.2, 6.3, 9.1, 9.2, 9.5, 9.6_

- [x] 10. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate universal correctness properties from the design document (8 properties)
- Unit tests validate specific examples and edge cases
- Test runner: `.venv/bin/python -m pytest tests/ -q` from `dev/workflow`
- Property test file: `dev/workflow/tests/test_application_naming_properties.py`
- Unit test file: `dev/workflow/tests/test_name_resolver.py`
- The EE2_Scanner requires no structural changes — only documentation updates

## Task Dependency Graph

```json
{
  "waves": [
    { "id": 0, "tasks": ["1.1", "1.2"] },
    { "id": 1, "tasks": ["2.1"] },
    { "id": 2, "tasks": ["2.2", "2.3", "2.4"] },
    { "id": 3, "tasks": ["2.5", "4.1"] },
    { "id": 4, "tasks": ["4.2", "5.1", "5.2"] },
    { "id": 5, "tasks": ["5.3", "5.4", "5.5", "5.6"] },
    { "id": 6, "tasks": ["7.1", "8.1"] },
    { "id": 7, "tasks": ["7.2", "7.3", "8.2", "9.1"] },
    { "id": 8, "tasks": ["9.2"] }
  ]
}
```

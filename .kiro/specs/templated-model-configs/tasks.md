# Implementation Plan: Templated Model Configs

## Overview

Convert static UFS model configuration files into Jinja2 templates rendered at deployment time. Implementation follows a bottom-up approach: schema and validators first, then templates, then pipeline integration, then migration cleanup, and finally comprehensive testing.

All code is Python. Tests use `pytest` + `hypothesis`. Package management uses `uv`.

## Tasks

- [x] 1. Model_Context schema, validators, and component YAML infrastructure
  - [x] 1.1 Create Model_Context schema validation module
    - Create `dev/workflow/deployment/model_context.py`
    - Define the `ModelContextSchema` class with required keys: `resolution`, `physics_suite`, `coupling_mode`, `dt_atmos`, `output_grid`, `output_fields`
    - Implement `validate(model_context: dict) -> list[str]` that returns FATAL ERROR messages for missing/invalid keys
    - Implement `merge_resolution_defaults(model_context: dict) -> dict` ensuring explicit `model.fv3` values override `model.defaults[resolution]`
    - Implement supported value enums: `SUPPORTED_RESOLUTIONS`, `SUPPORTED_PHYSICS_SUITES`, `SUPPORTED_COUPLING_MODES`
    - _Requirements: 4.1, 4.2, 4.3, 4.5, 4.6, 4.7_

  - [x] 1.2 Create format validators package
    - Create `dev/workflow/deployment/validators/__init__.py` exporting all validators
    - Create `dev/workflow/deployment/validators/model_configure.py` with `ModelConfigureValidator`
    - Create `dev/workflow/deployment/validators/namelist.py` with `NamelistValidator`
    - Create `dev/workflow/deployment/validators/diag_table.py` with `DiagTableValidator`
    - Create `dev/workflow/deployment/validators/esmf_config.py` with `ESMFConfigValidator`
    - Create `dev/workflow/deployment/validators/field_table.py` with `FieldTableValidator`
    - Each validator implements `validate(content: str, filepath: str) -> list[str]` returning error messages
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5, 7.6_

  - [x] 1.3 Create component YAML files
    - Create `dev/parm/components/atmos.yaml` with `model.fv3` subsection and atmosphere families
    - Create `dev/parm/components/ocean.yaml` with `model.ocean` subsection and ocean families
    - Create `dev/parm/components/ice.yaml` with `model.ice` subsection and ice families
    - Create `dev/parm/components/wave.yaml` with `model.wave` subsection and wave families
    - Create `dev/parm/components/gocart.yaml` with `model.aerosol` subsection and aerosol families
    - Each component YAML declares its own `model`, `families`, and `tasks` sections per the design schema
    - _Requirements: 10.1, 10.2, 10.5, 10.6, 10.10_

  - [x] 1.4 Write unit tests for Model_Context schema validation
    - Create `dev/test/test_model_configs/test_model_context.py`
    - Test required key validation (missing keys produce FATAL ERROR)
    - Test resolution defaults merge logic (explicit overrides defaults)
    - Test unsupported value detection (invalid physics_suite, coupling_mode, resolution)
    - Test type validation for all schema fields
    - _Requirements: 4.1, 4.5, 4.7_

  - [x] 1.5 Write unit tests for format validators
    - Create `dev/test/test_model_configs/test_validators.py`
    - Test each validator with known-good inputs (no errors returned)
    - Test each validator with known-bad inputs (correct error messages)
    - Test edge cases: empty files, comment-only files, partial content
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

- [x] 2. Checkpoint - Ensure schema and validator tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 3. FV3 template creation (field_table, model_configure, input.nml, diag_table)
  - [x] 3.1 Create `field_table.j2` template
    - Create `dev/parm/ufs/fv3/field_table.j2`
    - Implement tracer selection logic based on `model.physics_suite`, `model.pbl_scheme`, `model.progsigma`
    - Include base tracers (sphum, liq_wat), suite-specific tracers (rainwat, ice_wat, snowwat, graupel for gfdl/wsm6/thompson), Thompson-specific (ice_nc, rain_nc), ozone (all suites), TKE (satmedmf), prognostic sigma, cloud amount (gfdl)
    - Ensure rendered output matches legacy `field_table_*` variants exactly in name, order, and attributes
    - _Requirements: 1.2, 1.5, 1.6, 1.7, 1.8, 1.10, 6.1_

  - [x] 3.2 Create `model_configure.j2` template
    - Create `dev/parm/ufs/fv3/model_configure.j2`
    - Render key-value pairs from `model.fv3` context (total_tasks, start_date, dt_atmos, restart_interval, quilting, write_groups, output_grid, etc.)
    - Preserve `${FHMAX}` and other runtime shell variables via Template_Renderer's shell variable protection
    - Implement `fortran_logical` filter usage for boolean values
    - _Requirements: 1.3, 5.1, 5.5_

  - [x] 3.3 Create `input.nml.j2` template
    - Create `dev/parm/ufs/fv3/input.nml.j2`
    - Render Fortran namelist groups: `amip_interp_nml`, `atmos_model_nml`, `fv_core_nml`, `gfs_physics_nml`
    - Use proper `&group` / `/` formatting with correct whitespace
    - Include conditionals for hydrostatic vs non-hydrostatic mode
    - Map `model.fv3` keys to namelist variables with defaults
    - _Requirements: 1.4, 5.3_

  - [x] 3.4 Create `diag_table.j2` template
    - Create `dev/parm/ufs/fv3/diag_table.j2`
    - Render file entries and field entries based on `model.output_fields` and `model.active_components`
    - Include conditional ocean output fields when ocean component is active
    - Maintain column alignment using Jinja2 whitespace control
    - _Requirements: 1.1, 8.1_

- [x] 4. Coupling and GOCART template creation
  - [x] 4.1 Create `ufs.configure.j2` template
    - Create `dev/parm/ufs/ufs.configure.j2`
    - Implement component list generation from `model.active_components`
    - Implement PET list bounds calculation for each component (ATM, OCN, ICE, WAV, CHM, MED)
    - Implement `runSeq::` coupling sequence generation for all supported coupling modes: `atm`, `atmaero`, `s2s`, `s2sa`, `s2sw`, `s2swa`
    - Convert all `@[VAR]` atparse patterns to Jinja2 `{{ var }}` syntax
    - _Requirements: 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 6.2, 8.2_

  - [x] 4.2 Create GOCART resource config templates
    - Create `dev/parm/ufs/gocart/AERO_HISTORY.rc.j2` with collection and grid label rendering
    - Create `dev/parm/ufs/gocart/ExtData.j2` with emission dataset selection (`qfed`, `gbbepx`, `none`)
    - Create `dev/parm/ufs/gocart/collections/` directory with per-collection field definition templates (e.g., `inst_aod.j2`)
    - Convert all `@[VAR]` atparse patterns to Jinja2 syntax
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5, 8.3_

- [x] 5. Pipeline integration (model_config_renderer.py wiring into Stage 3)
  - [x] 5.1 Create `model_config_renderer.py` orchestration module
    - Create `dev/workflow/deployment/model_config_renderer.py`
    - Implement `ModelConfigRenderer` class with `render_all(model_context: dict, expdir: Path) -> list[RenderedFile]`
    - Implement context assembly: extract `model` section, merge resolution defaults, validate schema
    - Implement template discovery: find all `.j2` files under `dev/parm/ufs/`
    - Implement format validation dispatch: route each rendered file to its format-specific validator
    - Implement output placement: write validated files to `<EXPDIR>/parm/ufs/` with correct subdirectory structure
    - Implement fallback resolution: prefer `.j2` template over static file, copy static if no template exists
    - Implement `template_overrides` support for incremental migration
    - _Requirements: 9.1, 9.2, 9.3, 11.1, 11.2, 11.3_

  - [x] 5.2 Wire `model_config_renderer.py` into pipeline Stage 3
    - Modify `dev/workflow/deployment/pipeline.py` to add model config rendering as a sub-stage within Stage 3 (Render Templates)
    - Call `ModelConfigRenderer.render_all()` after existing template rendering
    - Ensure rendered files are included in the EXPDIR manifest with SHA-256 hashes
    - Halt pipeline with FATAL ERROR if any format validation fails
    - _Requirements: 7.7, 9.5_

  - [x] 5.3 Implement component composition logic
    - Add component YAML loading and merge logic to `model_config_renderer.py` or `workflow_config.py`
    - Implement active component filtering based on `components:` list
    - Implement model section merge (union of each component's `model.<component>` section)
    - Implement family merge (append each component's families to top-level)
    - Implement cross-component trigger resolution and dangling reference removal with warnings
    - _Requirements: 10.3, 10.4, 10.7, 10.8, 10.9_

- [x] 6. Checkpoint - Ensure pipeline integration tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 7. atparse migration and legacy file cleanup
  - [x] 7.1 Create atparse-to-Jinja2 migration utility
    - Create `dev/workflow/deployment/atparse_migration.py`
    - Implement `atparse_to_jinja2(content: str, var_mapping: dict[str, str]) -> str` conversion function
    - Define the variable mapping table (atparse uppercase names → Model_Context Jinja2 expressions)
    - Ensure shell variables (`${VAR}`) are preserved through conversion
    - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5_

  - [x] 7.2 Update forecast ex-scripts to use pre-rendered configs
    - Modify `ush/forecast_postdet.sh` to remove calls to `parsing_model_configure_FV3.sh`, `parsing_ufs_configure.sh`, `parsing_namelists_FV3.sh`
    - Modify `scripts/exglobal_forecast.sh` to copy pre-rendered files from `${EXPDIR}/parm/ufs/` to `${DATA}/`
    - Ensure ex-scripts use `${EXPDIR}/parm/ufs/<subpath>` paths and do NOT fall back to `${HOMEglobal}/parm/ufs/`
    - _Requirements: 5.1, 5.2, 5.3, 5.4, 9.4_

  - [x] 7.3 Remove legacy static file variants
    - Delete `parm/ufs/fv3/field_table_*` (20 files) replaced by `field_table.j2`
    - Delete `parm/ufs/ufs.configure.*.IN` (7 files) replaced by `ufs.configure.j2`
    - Delete `parm/ufs/gocart/ExtData.qfed`, `ExtData.gbbepx`, `ExtData.none` replaced by `ExtData.j2`
    - Delete `parm/ufs/fv3/diag_table_aod`, `diag_table_da`, `diag_table.aero` replaced by `diag_table.j2`
    - _Requirements: 6.3_

- [x] 8. Property-based tests and integration tests
  - [x] 8.1 Write property test: Template Equivalence (field_table)
    - **Property 1: Template Equivalence (field_table)**
    - Create `dev/test/test_model_configs/test_properties.py`
    - Use `hypothesis` to generate all supported (physics_suite, pbl_scheme, progsigma) combinations
    - Assert rendered `field_table.j2` output matches corresponding legacy static file in tracer name, order, and attributes
    - Minimum 100 iterations
    - **Validates: Requirements 1.5, 1.6, 1.7, 1.8, 1.10, 6.1**

  - [x] 8.2 Write property test: Template Equivalence (ufs.configure)
    - **Property 2: Template Equivalence (ufs.configure)**
    - Use `hypothesis` to generate all supported coupling_mode values with valid component task counts
    - Assert rendered `ufs.configure.j2` output is functionally equivalent to atparse-rendered legacy `.IN` file
    - Minimum 100 iterations
    - **Validates: Requirements 2.2, 2.3, 2.4, 2.5, 2.6, 6.2, 8.4**

  - [x] 8.3 Write property test: Format Validity
    - **Property 3: Format Validity**
    - Use `hypothesis` to generate valid Model_Context values (resolution × physics_suite × coupling_mode × component set)
    - Assert every rendered UFS_Model_Config file passes its format-specific validator without errors
    - Minimum 100 iterations
    - **Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5**

  - [x] 8.4 Write property test: No Legacy atparse Tokens
    - **Property 4: No Legacy atparse Tokens**
    - Use `hypothesis` to generate valid Model_Context values
    - Assert no rendered file contains `@[...]` atparse substitution patterns
    - Minimum 100 iterations
    - **Validates: Requirements 8.1, 8.2, 8.3**

  - [x] 8.5 Write property test: Component Composition Validity
    - **Property 5: Component Composition Validity**
    - Use `hypothesis` to generate non-empty subsets of supported components
    - Assert merged Model_Context contains exactly the union of included components' model sections
    - Assert no dangling trigger references in the resulting DAG
    - Assert no FATAL ERROR for template variables belonging to excluded components
    - Minimum 100 iterations
    - **Validates: Requirements 10.3, 10.4, 10.7, 10.9**

  - [x] 8.6 Write property test: Schema Validation and Default Override
    - **Property 6: Schema Validation and Default Override**
    - Use `hypothesis` to generate Model_Context dicts with randomly removed required keys
    - Assert FATAL ERROR is emitted for each missing required key
    - Use `hypothesis` to generate contexts with both explicit `model.fv3` values and `model.defaults[resolution]` values
    - Assert explicit values always override defaults in merged context
    - Minimum 100 iterations
    - **Validates: Requirements 4.1, 4.5, 4.7**

  - [x] 8.7 Write property test: Shell Variable Preservation
    - **Property 7: Shell Variable Preservation**
    - Use `hypothesis` to generate templates containing `${VAR}` patterns
    - Assert all shell variable patterns appear verbatim in rendered output
    - Minimum 100 iterations
    - **Validates: Requirements 5.5, 11.4**

  - [x] 8.8 Write integration tests for full rendering pipeline
    - Create `dev/test/test_model_configs/test_integration.py`
    - Test full rendering pipeline for each supported physics_suite × coupling_mode combination
    - Test component add/remove scenarios (verify DAG validity)
    - Test fallback mechanism (static file copy when no template exists)
    - Test `template_overrides` behavior during incremental migration
    - _Requirements: 11.1, 11.2, 11.3, 9.1, 9.2, 9.3_

- [x] 9. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate universal correctness properties from the design document
- Unit tests validate specific examples and edge cases
- All Python code uses `uv` for package management, `pytest` as test runner, `hypothesis` for property-based tests
- The existing `Template_Renderer` at `dev/workflow/deployment/template_renderer.py` handles Jinja2 rendering with strict undefined detection and shell variable preservation — reuse it directly
- The existing `pipeline.py` at `dev/workflow/deployment/pipeline.py` needs a new sub-stage in Stage 3 for model config rendering
- Component YAMLs go under `dev/parm/components/`; template files go under `dev/parm/ufs/`

## Task Dependency Graph

```json
{
  "waves": [
    { "id": 0, "tasks": ["1.1", "1.2", "1.3"] },
    { "id": 1, "tasks": ["1.4", "1.5", "3.1", "3.2", "3.3", "3.4"] },
    { "id": 2, "tasks": ["4.1", "4.2"] },
    { "id": 3, "tasks": ["5.1"] },
    { "id": 4, "tasks": ["5.2", "5.3"] },
    { "id": 5, "tasks": ["7.1"] },
    { "id": 6, "tasks": ["7.2", "7.3"] },
    { "id": 7, "tasks": ["8.1", "8.2", "8.3", "8.4", "8.5", "8.6", "8.7", "8.8"] }
  ]
}
```

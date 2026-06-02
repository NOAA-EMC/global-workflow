# Implementation Plan: Coupled Model Configs

## Overview

Convert coupled-model configuration files (MOM6 ocean, CICE6 ice, WW3 wave, FV3 nested grid, UPP post) from legacy `@[VAR]` atparse templates into Jinja2 templates rendered at deployment time. Implementation extends the parent "templated-model-configs" infrastructure with new templates, a MOM6-specific format validator, ocean resolution defaults merge logic, and submodule copy handling.

All code is Python + Jinja2 templates. Tests use `pytest` + `hypothesis`. Package management uses `uv`.

## Tasks

- [ ] 1. Schema extension, MOM6 validator, and ocean resolution defaults
  - [x] 1.1 Extend Model_Context schema for coupled components
    - Modify `dev/workflow/deployment/model_context.py`
    - Add `REQUIRED_KEYS` dict for `ocean`, `ice`, `wave`, and `post` sections per the design schema
    - Implement `validate_coupled_model_context(model_context: dict) -> list[str]` returning FATAL ERROR messages for missing/invalid keys
    - Add enum constraints: `ocean.resolution` in `{025, 050, 100, 500}`, `wave.ice_input` in `{YES, CPL}`, `wave.current_input` in `{YES, CPL}`, `post.system` in `{gfs, gcafs, gefs, sfs}`
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5_

  - [x] 1.2 Implement ocean resolution defaults merge logic
    - Add `merge_ocean_resolution_defaults(model_context: dict) -> dict` to `dev/workflow/deployment/model_context.py`
    - Merge `model.ocean.defaults[resolution]` into `model.ocean`; explicit values always override defaults
    - Raise `FatalDeploymentError` for unsupported resolution values
    - Define default mappings for all 4 resolutions (025, 050, 100, 500) with `nx_glb`, `ny_glb`, `dt_ocean`, `dt_therm`, mixing params
    - _Requirements: 12.1, 12.2, 12.3, 12.4_

  - [x] 1.3 Create MOM6ParameterValidator
    - Create `dev/workflow/deployment/validators/mom6_parameter.py`
    - Implement `MOM6ParameterValidator.validate(content: str, filepath: str) -> list[str]`
    - Validate MOM6 format: `! section` comment headers, `PARAM = VALUE` assignments, no stray lines
    - Return error messages with line numbers for invalid lines
    - Register in `dev/workflow/deployment/validators/__init__.py`
    - _Requirements: 10.1, 10.5, 10.6_

  - [-] 1.4 Write unit tests for coupled schema validation
    - Create `dev/test/test_coupled_model_configs/test_schema_validation.py`
    - Test required key validation for ocean, ice, wave, post sections (missing keys produce FATAL ERROR)
    - Test enum constraint validation (invalid resolution, ice_input, current_input, post.system)
    - Test ocean resolution defaults merge (explicit overrides defaults for all 4 resolutions)
    - Test unsupported resolution raises FatalDeploymentError
    - _Requirements: 7.1, 7.2, 7.3, 7.4, 7.5, 12.1, 12.2_

  - [-] 1.5 Write unit tests for MOM6ParameterValidator
    - Create `dev/test/test_coupled_model_configs/test_mom6_validator.py`
    - Test known-good MOM6 parameter file inputs (no errors)
    - Test known-bad inputs: missing `=`, invalid section headers, stray characters
    - Test edge cases: empty file, comment-only file, shell variables in values
    - _Requirements: 10.1, 10.5_

- [~] 2. Checkpoint - Ensure schema and validator tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 3. Ocean templates (MOM6)
  - [~] 3.1 Create `ocean/MOM_input.j2` template
    - Create `dev/parm/ufs/ocean/MOM_input.j2`
    - Implement single template with resolution-conditional blocks for grid dims, timesteps, mixing coefficients
    - Use MOM6 format: `! section` headers, `PARAM = VALUE` assignments
    - Include conditionals for `use_waves`, `river_runoff`, `oda_incupd`, `do_sppt`
    - Preserve shell variables: `${TOPOEDITS}`, `${CHLCLIM}`
    - Trigger `undefined_resolution_error` for unsupported resolution values
    - _Requirements: 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8_

  - [~] 3.2 Create `ocean/MOM6_data_table.j2` template
    - Create `dev/parm/ufs/ocean/MOM6_data_table.j2`
    - Render data override table with conditional river runoff entry based on `model.ocean.river_runoff`
    - Reference `model.ocean.frunoff` for runoff forcing file path
    - _Requirements: 2.1, 2.2, 2.3, 2.4_

  - [~] 3.3 Write property test: Template Equivalence (MOM_input)
    - **Property 1: Template Equivalence (Coupled-Model Configs)**
    - Use `hypothesis` to generate all 4 ocean resolutions × valid Model_Context variable combinations
    - Assert rendered `MOM_input.j2` output matches legacy `MOM_input_*.IN` atparse output for same variable values
    - Minimum 100 iterations
    - **Validates: Requirements 1.2, 1.3, 1.4, 1.5, 2.3, 11.5**

  - [~] 3.4 Write unit tests for MOM_input rendering
    - Create `dev/test/test_coupled_model_configs/test_mom_input.py`
    - Test each resolution (025, 050, 100, 500) produces correct grid dims and mixing params
    - Test conditional blocks: use_waves, river_runoff, oda_incupd, do_sppt
    - Test shell variable preservation (`${TOPOEDITS}`, `${CHLCLIM}`)
    - Test unsupported resolution triggers undefined variable error
    - _Requirements: 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8_

- [ ] 4. Ice template (CICE6)
  - [~] 4.1 Create `ice/ice_in.j2` template
    - Create `dev/parm/ufs/ice/ice_in.j2`
    - Render all CICE6 namelist groups: `&setup_nml`, `&grid_nml`, `&domain_nml`, `&tracer_nml`, `&thermo_nml`, `&dynamics_nml`, `&shortwave_nml`, `&ponds_nml`, `&snow_nml`, `&forcing_nml`, `&icefields_nml`
    - Implement warm start conditional: `runtype = 'continue'` / `'initial'`, `use_restart_time` via `fortran_logical` filter
    - Preserve shell variables: `${SYEAR}`, `${SMONTH}`, `${SDAY}`, `${FHMAX}`
    - Use `model.ice.*` context keys for decomposition, grid, output settings
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7_

  - [~] 4.2 Write property test: Warm Start Conditional Rendering
    - **Property 7: Warm Start Conditional Rendering**
    - Use `hypothesis` to generate valid ice Model_Context with `warm_start` as True/False
    - Assert `warm_start=True` → `runtype = 'continue'` and `use_restart_time = .true.`
    - Assert `warm_start=False` → `runtype = 'initial'` and `use_restart_time = .false.`
    - Minimum 100 iterations
    - **Validates: Requirements 3.3, 3.4**

  - [~] 4.3 Write unit tests for ice_in rendering
    - Create `dev/test/test_coupled_model_configs/test_ice_in.py`
    - Test warm start conditional (both True and False)
    - Test decomposition parameters (nprocs, block_size_x/y, processor_shape)
    - Test shell variable preservation (`${SYEAR}`, `${SMONTH}`, `${SDAY}`, `${FHMAX}`)
    - Test Fortran namelist format validity (NamelistValidator passes)
    - _Requirements: 3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7_

- [ ] 5. Wave template (WW3) and FV3 nested grid template
  - [~] 5.1 Create `wave/ww3_shel.nml.j2` template
    - Create `dev/parm/ufs/wave/ww3_shel.nml.j2`
    - Implement forcing mode mapping: `ice_input == "CPL"` → `'C'`, `"YES"` → `'T'`; same for `current_input`
    - Render output parameters, time steps, and directory paths from `model.wave.*`
    - Preserve shell variables: `${FHMAX_WAV}`
    - Use Fortran namelist format (`&group` / `/`)
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7_

  - [~] 5.2 Write property test: WW3 Forcing Mode Mapping
    - **Property 8: WW3 Forcing Mode Mapping**
    - Use `hypothesis` to generate valid wave Model_Context with `ice_input` ∈ {CPL, YES} × `current_input` ∈ {CPL, YES}
    - Assert correct flag character mapping in rendered output
    - Minimum 100 iterations
    - **Validates: Requirements 4.2, 4.3, 4.4**

  - [~] 5.3 Create `fv3/input_global_nest.nml.j2` template
    - Create `dev/parm/ufs/fv3/input_global_nest.nml.j2`
    - Render model_configure format with nest-specific parameters (`NEST_IMO`, `NEST_JMO`) when `model.fv3.do_nest` is true
    - Preserve shell variables: `${FHMAX}`, `${PDY}`, `${cyc}`
    - _Requirements: 5.1, 5.2, 5.3, 5.4, 5.5_

  - [~] 5.4 Create `post/post_itag.j2` template
    - Create `dev/parm/ufs/post/post_itag.j2`
    - Implement system-specific parameter selection based on `model.post.system` (gfs, gcafs, gefs, sfs)
    - Produce valid UPP iteration control parameters
    - _Requirements: 6.1, 6.2, 6.3, 6.4_

  - [~] 5.5 Write unit tests for WW3 and nested grid templates
    - Create `dev/test/test_coupled_model_configs/test_ww3_shel.py`
    - Test forcing mode mapping (CPL→C, YES→T) for both ice_input and current_input
    - Test output parameter rendering and shell variable preservation
    - Test Fortran namelist format validity (NamelistValidator passes)
    - Create `dev/test/test_coupled_model_configs/test_nested_grid.py`
    - Test nest-specific parameters included when `do_nest=True`
    - Test model_configure format validity (ModelConfigureValidator passes)
    - _Requirements: 4.2, 4.3, 4.4, 4.5, 5.1, 5.2, 5.3_

- [~] 6. Checkpoint - Ensure all template tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 7. Pipeline integration and submodule copy handling
  - [~] 7.1 Extend ModelConfigRenderer for coupled-model templates
    - Modify `dev/workflow/deployment/model_config_renderer.py`
    - Add template discovery for `ocean/`, `ice/`, `wave/`, `post/` subdirectories
    - Add ocean resolution defaults merge call before rendering ocean templates
    - Route `MOM_input` to `MOM6ParameterValidator`, `ice_in` and `ww3_shel.nml` to `NamelistValidator`, `input_global_nest.nml` to `ModelConfigureValidator`
    - Add coupled schema validation before rendering
    - _Requirements: 9.1, 9.2, 9.3, 9.4, 9.5, 9.6, 10.1, 10.2, 10.3, 10.4, 11.1, 11.2, 11.3, 11.4_

  - [~] 7.2 Implement submodule file copy (Stage 4)
    - Add submodule copy manifest to pipeline: NEXUS configs from `sorc/nexus.fd/config/gocart/` → `<EXPDIR>/parm/chem/nexus/gocart/`
    - Add UPP parm files from `sorc/upp.fd/parm/` → `<EXPDIR>/parm/post/`
    - Use `cp -rp` semantics (preserve permissions, no Jinja2 rendering)
    - Emit FATAL ERROR if source file not found
    - _Requirements: 13.1, 13.2, 13.3, 13.4, 13.5_

  - [~] 7.3 Update forecast ex-scripts for coupled-model configs
    - Modify `scripts/exglobal_forecast.sh` to remove calls to `parsing_namelists_MOM6.sh`, `parsing_namelists_CICE.sh`, `parsing_namelists_WW3.sh`
    - Replace with `cpreq` from `${EXPDIR}/parm/ufs/{ocean,ice,wave}/` to `${DATA}/`
    - Ensure no fallback to `${HOMEglobal}/parm/ufs/` or symlinks into `sorc/`
    - _Requirements: 8.1, 8.2, 8.3, 8.4, 8.5, 9.7_

  - [~] 7.4 Remove legacy coupled-model `.IN` files and update link_workflow.sh
    - Delete `parm/ufs/MOM_input_025.IN`, `MOM_input_050.IN`, `MOM_input_100.IN`, `MOM_input_500.IN`
    - Delete `parm/ufs/MOM6_data_table.IN`, `ice_in.IN`, `ww3_shel.nml.IN`, `input_global_nest.nml.IN`
    - Delete `parm/ufs/post_itag_gfs`, `post_itag_gcafs`
    - Modify `sorc/link_workflow.sh` to remove coupled-model `.IN` files from `ufs_templates` array
    - _Requirements: 11.7, 14.1, 14.2, 14.3_

- [~] 8. Checkpoint - Ensure pipeline integration tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 9. Property-based tests and integration tests
  - [x] 9.1 Write property test: Format Validity (All Rendered Configs)
    - **Property 2: Format Validity (All Rendered Configs)**
    - Use `hypothesis` to generate valid Model_Context (ocean resolution × ice decomposition × wave coupling mode × post system)
    - Assert every rendered coupled-model config passes its format-specific validator
    - Minimum 100 iterations
    - **Validates: Requirements 3.5, 4.5, 10.1, 10.2, 10.3, 10.4**

  - [x] 9.2 Write property test: Shell Variable Preservation
    - **Property 3: Shell Variable Preservation**
    - Use `hypothesis` to generate valid Model_Context values
    - Assert all `${VAR}` shell variable patterns in templates appear verbatim in rendered output
    - Minimum 100 iterations
    - **Validates: Requirements 1.8, 2.4, 3.7, 4.7, 5.5, 8.5**

  - [x] 9.3 Write property test: No Legacy atparse Tokens
    - **Property 4: No Legacy atparse Tokens**
    - Use `hypothesis` to generate valid Model_Context values
    - Assert no rendered coupled-model config contains `@[...]` atparse substitution patterns
    - Minimum 100 iterations
    - **Validates: Requirements 11.1, 11.2, 11.3, 11.4**

  - [x] 9.4 Write property test: Schema Validation (Missing Keys Cause FATAL ERROR)
    - **Property 5: Schema Validation (Missing Keys Cause FATAL ERROR)**
    - Use `hypothesis` to generate Model_Context dicts with randomly removed required keys from ocean/ice/wave/post
    - Assert FATAL ERROR emitted for each missing required key
    - Test unsupported `ocean.resolution` values produce FATAL ERROR
    - Minimum 100 iterations
    - **Validates: Requirements 1.6, 7.1, 7.2, 7.3, 7.4, 7.5**

  - [x] 9.5 Write property test: Ocean Resolution Default Override
    - **Property 6: Ocean Resolution Default Override**
    - Use `hypothesis` to generate contexts with both explicit `model.ocean` values and `model.ocean.defaults[resolution]` values
    - Assert explicit values always override defaults in merged context
    - Assert default-only keys appear in merged context
    - Minimum 100 iterations
    - **Validates: Requirements 12.1, 12.2, 12.3**

  - [x] 9.6 Write property test: Submodule Copy Integrity
    - **Property 9: Submodule Copy Integrity**
    - Use `hypothesis` to generate file content (arbitrary bytes)
    - Assert copied file is byte-identical to source
    - Assert no Jinja2 rendering attempted on submodule files
    - Minimum 100 iterations
    - **Validates: Requirements 13.3, 13.4, 13.5**

  - [x] 9.7 Write property test: No Symlinks in EXPDIR
    - **Property 10: No Symlinks in EXPDIR**
    - Use `hypothesis` to generate valid deployment configurations
    - Assert EXPDIR contains no symlinks to `sorc/ufs_model.fd/tests/parm/` for coupled-model configs
    - Assert all config files are regular files
    - Minimum 100 iterations
    - **Validates: Requirements 14.1, 14.2**

  - [x] 9.8 Write integration tests for coupled-model rendering pipeline
    - Create `dev/test/test_coupled_model_configs/test_integration.py`
    - Test full rendering pipeline for each ocean resolution (025, 050, 100, 500)
    - Test coupled-model rendering with all component combinations
    - Test submodule copy (NEXUS, UPP files copied verbatim)
    - Test EXPDIR manifest includes all rendered coupled-model configs with SHA-256 hashes
    - Test no symlinks in EXPDIR after deployment
    - _Requirements: 9.1, 9.2, 9.3, 9.4, 9.5, 9.6, 9.8, 13.3, 14.1_

- [x] 10. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

## Notes

- Tasks marked with `*` are optional and can be skipped for faster MVP
- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation
- Property tests validate universal correctness properties from the design document
- Unit tests validate specific examples and edge cases
- All Python code uses `uv` for package management, `pytest` as test runner, `hypothesis` for property-based tests
- This spec extends the parent "templated-model-configs" infrastructure — reuse `Template_Renderer`, `NamelistValidator`, `ModelConfigureValidator`, `ModelConfigRenderer`, and `pipeline.py` directly
- The new `MOM6ParameterValidator` handles the MOM6-specific `PARAM = VALUE` format distinct from Fortran namelists
- Ocean resolution defaults merge ensures switching resolution requires only changing `model.ocean.resolution`
- Submodule files (NEXUS, UPP) are copied verbatim by Stage 4, never templated
- Shell variables (`${VAR}`) are preserved through Jinja2 rendering for runtime expansion

## Task Dependency Graph

```json
{
  "waves": [
    { "id": 0, "tasks": ["1.1", "1.2", "1.3"] },
    { "id": 1, "tasks": ["1.4", "1.5"] },
    { "id": 2, "tasks": ["3.1", "3.2", "4.1", "5.1", "5.3", "5.4"] },
    { "id": 3, "tasks": ["3.3", "3.4", "4.2", "4.3", "5.2", "5.5"] },
    { "id": 4, "tasks": ["7.1", "7.2"] },
    { "id": 5, "tasks": ["7.3", "7.4"] },
    { "id": 6, "tasks": ["9.1", "9.2", "9.3", "9.4", "9.5", "9.6", "9.7", "9.8"] }
  ]
}
```

# Requirements Document

## Introduction

This feature converts the remaining coupled-model configuration files — currently linked as legacy `@[VAR]` atparse templates from `sorc/ufs_model.fd/tests/parm/` — into Jinja2 templates rendered at deployment time by the Deployment_Tool's Template_Renderer. It is a companion to the "templated-model-configs" spec, which handled FV3 atmosphere configs (input.nml, model_configure, field_table, diag_table, ufs.configure) and GOCART configs.

Files in scope:
- `MOM_input_025.IN`, `MOM_input_050.IN`, `MOM_input_100.IN`, `MOM_input_500.IN` — MOM6 ocean model namelists (resolution-dependent)
- `MOM6_data_table.IN` — MOM6 data override table for prescribed fields
- `ice_in.IN` — CICE6 sea ice model namelist
- `ww3_shel.nml.IN` — WW3 wave model runtime namelist
- `input_global_nest.nml.IN` — FV3 nested grid model_configure variant
- `post_itag_gfs`, `post_itag_gcafs` — UPP inline post-processing iteration control tags

The converted templates reside under `dev/parm/ufs/` and are rendered into `<EXPDIR>/parm/ufs/` at deploy time. This eliminates runtime `atparse` rendering, removes symlinks to `sorc/` submodules, and makes the EXPDIR fully self-contained for NCO production.

The existing infrastructure from the parent spec applies without modification:
- Template_Renderer with shell variable preservation and `fortran_logical` filter
- ModelConfigRenderer orchestration (Stage 3b of the deployment pipeline)
- Format validators (NamelistValidator for Fortran namelists)
- Component YAML architecture (`ocean.yaml`, `ice.yaml`, `wave.yaml`)
- `atparse_migration.py` utility for `@[VAR]` → `{{ var }}` conversion

## Glossary

- **Template_Renderer**: The wxflow-backed component responsible for resolving Jinja2 templates against a deployment-time context. Uses `parse_j2yaml` with strict undefined detection and shell variable preservation.
- **Deployment_Tool**: The component that consumes `dev/` sources plus a Workflow_Configuration YAML and produces a rendered, self-contained EXPDIR.
- **Workflow_Configuration**: The declarative YAML document describing all workflow parameters including the `model.ocean`, `model.ice`, and `model.wave` sections that provide coupled-model template context.
- **Model_Context**: The subsection of the Workflow_Configuration YAML that provides template variables for UFS model configuration rendering.
- **EXPDIR**: An immutable, versioned deployment directory containing all rendered artifacts required to run the workflow.
- **MOM6_Namelist**: The `MOM_input` parameter file consumed by the MOM6 ocean model at runtime, controlling ocean dynamics, mixing, diagnostics, and I/O.
- **CICE6_Namelist**: The `ice_in` Fortran namelist file consumed by the CICE6 sea ice model at runtime, controlling ice dynamics, thermodynamics, grid decomposition, and output.
- **WW3_Namelist**: The `ww3_shel.nml` Fortran namelist file consumed by the WW3 wave model at runtime, controlling wave physics, time stepping, and output.
- **Ocean_Resolution**: The MOM6 ocean grid resolution identifier (`025`, `050`, `100`, `500`) that determines grid dimensions, timestep, and mixing parameters.
- **atparse**: The legacy shell-based template substitution utility (`@[VAR]` syntax) currently used for runtime config generation; replaced by Jinja2 in this feature.
- **Component_YAML**: A composable YAML file under `dev/parm/components/` that defines one UFS component's model parameters. Included into the top-level Workflow_Configuration via `!INC` or `{% include %}`.
- **Shell_Variable_Preservation**: The Template_Renderer behavior that passes `${VAR}` shell variable references through without resolution, allowing runtime expansion by the forecast job.
- **ModelConfigRenderer**: The orchestration component within Stage 3b of the deployment pipeline that iterates over registered model config templates, renders each against the Model_Context, and invokes format validators.
- **UPP_Itag**: The inline post-processing iteration control tag file consumed by UPP when `WRITE_DOPOST` is enabled, specifying post-processing parameters for inline execution.
- **Nested_Grid_Config**: The `input_global_nest.nml` variant of model_configure used for global-nest FV3 configurations, containing additional nest-specific output grid parameters.

## Requirements

### Requirement 1: Jinja2 Template Creation for MOM6 Ocean Namelists

**User Story:** As a workflow developer, I want a single parameterized Jinja2 template for MOM6 ocean namelists, so that ocean resolution selection is driven by configuration rather than by choosing among four resolution-specific `.IN` files.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/ocean/MOM_input.j2` into `<EXPDIR>/parm/ufs/ocean/MOM_input` using the Model_Context from the Workflow_Configuration.
2. WHEN the Ocean_Resolution is `025` in the Model_Context, THE Template_Renderer SHALL produce a `MOM_input` whose parameter values (grid dimensions, timestep, mixing coefficients) are identical to those produced by rendering the legacy `MOM_input_025.IN` with `atparse` given the same variable values.
3. WHEN the Ocean_Resolution is `050` in the Model_Context, THE Template_Renderer SHALL produce a `MOM_input` whose parameter values are identical to those produced by rendering the legacy `MOM_input_050.IN` with `atparse` given the same variable values.
4. WHEN the Ocean_Resolution is `100` in the Model_Context, THE Template_Renderer SHALL produce a `MOM_input` whose parameter values are identical to those produced by rendering the legacy `MOM_input_100.IN` with `atparse` given the same variable values.
5. WHEN the Ocean_Resolution is `500` in the Model_Context, THE Template_Renderer SHALL produce a `MOM_input` whose parameter values are identical to those produced by rendering the legacy `MOM_input_500.IN` with `atparse` given the same variable values.
6. IF the Model_Context specifies an Ocean_Resolution value that is not one of the supported values (`025`, `050`, `100`, `500`), THEN THE Template_Renderer SHALL emit a FATAL ERROR identifying the unsupported Ocean_Resolution value and the template file.
7. THE `MOM_input.j2` template SHALL accept the Model_Context keys `model.ocean.resolution`, `model.ocean.dt_ocean`, `model.ocean.dt_therm`, `model.ocean.use_waves`, `model.ocean.oda_incupd`, and `model.ocean.do_sppt` to select the correct ocean configuration parameters.
8. THE Template_Renderer SHALL preserve `${VAR}` shell variable references in the rendered `MOM_input` that must expand at runtime (e.g., `${TOPOEDITS}`, `${CHLCLIM}`).

### Requirement 2: Jinja2 Template Creation for MOM6 Data Table

**User Story:** As a workflow developer, I want the MOM6 data override table to be a Jinja2 template, so that prescribed field sources (river runoff paths) are driven by configuration rather than hardcoded in a static file.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/ocean/MOM6_data_table.j2` into `<EXPDIR>/parm/ufs/ocean/MOM6_data_table` using the Model_Context from the Workflow_Configuration.
2. WHEN the Model_Context provides a `model.ocean.frunoff` path, THE Template_Renderer SHALL produce a `MOM6_data_table` that references the specified river runoff forcing file.
3. FOR ALL valid Model_Context configurations, THE rendered `MOM6_data_table` SHALL be identical to what `atparse` would have produced given the same variable values applied to `MOM6_data_table.IN` (functional equivalence property).
4. THE Template_Renderer SHALL preserve `${VAR}` shell variable references in the rendered `MOM6_data_table` that must expand at runtime.

### Requirement 3: Jinja2 Template Creation for CICE6 Sea Ice Namelist

**User Story:** As a workflow developer, I want the CICE6 namelist to be a Jinja2 template, so that ice model decomposition, restart settings, and output frequencies are driven by configuration rather than computed at runtime by a shell script.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/ice/ice_in.j2` into `<EXPDIR>/parm/ufs/ice/ice_in` using the Model_Context from the Workflow_Configuration.
2. WHEN the Model_Context specifies `model.ice.nprocs` and `model.ice.decomposition`, THE Template_Renderer SHALL produce an `ice_in` with correct `nprocs`, `processor_shape`, `block_size_x`, and `block_size_y` values consistent with the CICE6 domain decomposition algorithm.
3. WHEN the Model_Context specifies `model.ice.warm_start` as true, THE Template_Renderer SHALL produce an `ice_in` with `runtype = 'continue'` and `use_restart_time = .true.`.
4. WHEN the Model_Context specifies `model.ice.warm_start` as false, THE Template_Renderer SHALL produce an `ice_in` with `runtype = 'initial'` and `use_restart_time = .false.`.
5. FOR ALL valid Model_Context configurations, THE rendered `ice_in` SHALL be a syntactically valid Fortran namelist file that the NamelistValidator accepts without error.
6. THE `ice_in.j2` template SHALL accept the Model_Context keys `model.ice.nprocs`, `model.ice.decomposition`, `model.ice.dt_ice`, `model.ice.grid`, `model.ice.mask`, `model.ice.nx_glb`, `model.ice.ny_glb`, `model.ice.histfreq_n`, and `model.ice.hist_avg` to configure the ice model.
7. THE Template_Renderer SHALL preserve `${VAR}` shell variable references in the rendered `ice_in` that must expand at runtime (e.g., `${SYEAR}`, `${SMONTH}`, `${SDAY}`, `${FHMAX}`).

### Requirement 4: Jinja2 Template Creation for WW3 Wave Namelist

**User Story:** As a workflow developer, I want the WW3 wave namelist to be a Jinja2 template, so that wave model coupling inputs, output parameters, and time stepping are driven by configuration rather than computed at runtime by a shell script.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/wave/ww3_shel.nml.j2` into `<EXPDIR>/parm/ufs/wave/ww3_shel.nml` using the Model_Context from the Workflow_Configuration.
2. WHEN the Model_Context specifies `model.wave.ice_input` as `CPL`, THE Template_Renderer SHALL produce a `ww3_shel.nml` with the ice forcing field set to coupled mode (`C`).
3. WHEN the Model_Context specifies `model.wave.ice_input` as `YES`, THE Template_Renderer SHALL produce a `ww3_shel.nml` with the ice forcing field set to file input mode (`T`).
4. WHEN the Model_Context specifies `model.wave.current_input` as `CPL`, THE Template_Renderer SHALL produce a `ww3_shel.nml` with the current forcing field set to coupled mode (`C`).
5. FOR ALL valid Model_Context configurations, THE rendered `ww3_shel.nml` SHALL be a syntactically valid Fortran namelist file that the NamelistValidator accepts without error.
6. THE `ww3_shel.nml.j2` template SHALL accept the Model_Context keys `model.wave.ice_input`, `model.wave.current_input`, `model.wave.output_params`, `model.wave.dt_field_output`, `model.wave.dt_point_output`, `model.wave.grid_output_dir`, `model.wave.point_output_dir`, and `model.wave.restart_output_dir` to configure the wave model.
7. THE Template_Renderer SHALL preserve `${VAR}` shell variable references in the rendered `ww3_shel.nml` that must expand at runtime (e.g., `${FHMAX_WAV}`).

### Requirement 5: Jinja2 Template Creation for FV3 Nested Grid Configuration

**User Story:** As a workflow developer, I want the FV3 nested grid model_configure variant to be a Jinja2 template, so that nest-specific output grid parameters are driven by configuration rather than requiring a separate `.IN` file.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/fv3/input_global_nest.nml.j2` into `<EXPDIR>/parm/ufs/fv3/input_global_nest.nml` using the Model_Context from the Workflow_Configuration.
2. WHEN the Model_Context specifies `model.fv3.do_nest` as true, THE Template_Renderer SHALL produce an `input_global_nest.nml` that includes nest-specific output grid parameters (`NEST_IMO`, `NEST_JMO`) in addition to the standard model_configure parameters.
3. FOR ALL valid Model_Context configurations with nesting enabled, THE rendered `input_global_nest.nml` SHALL be identical to what `atparse` would have produced given the same variable values applied to `input_global_nest.nml.IN` (functional equivalence property).
4. THE `input_global_nest.nml.j2` template SHALL accept the Model_Context keys `model.fv3.npx_nest`, `model.fv3.npy_nest`, `model.fv3.do_nest`, and all standard model_configure keys to produce the nested grid configuration.
5. THE Template_Renderer SHALL preserve `${VAR}` shell variable references in the rendered `input_global_nest.nml` that must expand at runtime (e.g., `${FHMAX}`, `${PDY}`, `${cyc}`).

### Requirement 6: Jinja2 Template Creation for UPP Post-Processing Itags

**User Story:** As a workflow developer, I want the UPP inline post-processing iteration tags to be Jinja2 templates, so that post-processing parameters for different model systems (GFS, GCAFS) are driven by configuration.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/post/post_itag.j2` into `<EXPDIR>/parm/ufs/post/post_itag` using the Model_Context from the Workflow_Configuration.
2. WHEN the Model_Context specifies `model.post.system` as `gfs`, THE Template_Renderer SHALL produce a `post_itag` with parameters matching the legacy `post_itag_gfs` file.
3. WHEN the Model_Context specifies `model.post.system` as `gcafs`, THE Template_Renderer SHALL produce a `post_itag` with parameters matching the legacy `post_itag_gcafs` file.
4. FOR ALL supported `model.post.system` values, THE rendered `post_itag` SHALL contain valid UPP iteration control parameters that the UPP executable accepts without error.

### Requirement 7: Model_Context Schema Extension for Coupled Components

**User Story:** As a workflow developer, I want the `model.ocean`, `model.ice`, and `model.wave` sections of the Workflow_Configuration to be well-defined, so that all coupled-model template variables are declared in one place and validated at deployment time.

#### Acceptance Criteria

1. THE Workflow_Configuration SHALL include a `model.ocean` section containing at minimum the keys `resolution`, `dt_ocean`, `dt_therm`, `nx_glb`, `ny_glb`, `use_waves`, `oda_incupd`, `oda_incupd_nhours`, `do_sppt`, `river_runoff`, `diag_coord_def_z_file`, and `frunoff`, where `resolution` is a supported Ocean_Resolution value (`025`, `050`, `100`, `500`), `dt_ocean` is a positive integer representing the ocean dynamic timestep in seconds, and `dt_therm` is a positive integer representing the ocean thermodynamic timestep in seconds.
2. THE Workflow_Configuration SHALL include a `model.ice` section containing at minimum the keys `nprocs`, `decomposition`, `dt_ice`, `grid`, `mask`, `nx_glb`, `ny_glb`, `warm_start`, `histfreq_n`, `hist_avg`, `dumpfreq`, `dumpfreq_n`, `ktherm`, and `tr_pond_lvl`, where `nprocs` is a positive integer representing the CICE6 task count, `decomposition` is a supported decomposition method (e.g., `slenderX2`), and `dt_ice` is a positive integer representing the ice timestep in seconds.
3. THE Workflow_Configuration SHALL include a `model.wave` section containing at minimum the keys `ice_input`, `current_input`, `output_params`, `dt_field_output`, `dt_point_output`, `grid_output_dir`, `point_output_dir`, and `restart_output_dir`, where `ice_input` is one of `YES` or `CPL`, and `current_input` is one of `YES` or `CPL`.
4. IF a required key is missing from the `model.ocean`, `model.ice`, or `model.wave` section during template rendering, THEN THE Template_Renderer SHALL emit a FATAL ERROR identifying the missing key, the template file, and the line number, and SHALL NOT produce a rendered output file.
5. THE Workflow_Configuration SHALL include a `model.post` section containing at minimum the key `system` (one of `gfs`, `gcafs`, `gefs`, `sfs`) for UPP inline post-processing configuration.

### Requirement 8: Elimination of Runtime Coupled-Model Config Generation Scripts

**User Story:** As a maintainer, I want the runtime shell scripts that generate coupled-model configuration files to be replaced by deploy-time templates, so that runtime execution reads only pre-rendered, immutable files.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL produce all coupled-model config files at deployment time, and THE rendered EXPDIR SHALL NOT require `ush/parsing_namelists_MOM6.sh` at runtime for `MOM_input` or `data_table` generation.
2. THE Deployment_Tool SHALL produce all coupled-model config files at deployment time, and THE rendered EXPDIR SHALL NOT require `ush/parsing_namelists_CICE.sh` at runtime for `ice_in` generation.
3. THE Deployment_Tool SHALL produce all coupled-model config files at deployment time, and THE rendered EXPDIR SHALL NOT require `ush/parsing_namelists_WW3.sh` at runtime for `ww3_shel.nml` generation.
4. WHEN the forecast ex-script executes, THE ex-script SHALL read the pre-rendered coupled-model config files from `<EXPDIR>/parm/ufs/` and SHALL NOT invoke `atparse` to generate them.
5. THE Deployment_Tool SHALL preserve `${VAR}` shell variable references that must expand at runtime (e.g., `${PDY}`, `${cyc}`, `${FHMAX}`, `${FHMAX_WAV}`) by passing them through the Template_Renderer without resolution.

### Requirement 9: Rendered Output Placement for Coupled-Model Configs

**User Story:** As an operator, I want all rendered coupled-model configuration files to reside in predictable locations within the EXPDIR, so that the forecast job finds them without path-guessing logic.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL place the rendered MOM6 namelist at `<EXPDIR>/parm/ufs/ocean/MOM_input`.
2. THE Deployment_Tool SHALL place the rendered MOM6 data table at `<EXPDIR>/parm/ufs/ocean/MOM6_data_table`.
3. THE Deployment_Tool SHALL place the rendered CICE6 namelist at `<EXPDIR>/parm/ufs/ice/ice_in`.
4. THE Deployment_Tool SHALL place the rendered WW3 namelist at `<EXPDIR>/parm/ufs/wave/ww3_shel.nml`.
5. THE Deployment_Tool SHALL place the rendered nested grid config at `<EXPDIR>/parm/ufs/fv3/input_global_nest.nml`.
6. THE Deployment_Tool SHALL place the rendered UPP itag at `<EXPDIR>/parm/ufs/post/post_itag`.
7. WHEN the forecast ex-script references a coupled-model config file, THE ex-script SHALL use the path `${EXPDIR}/parm/ufs/<component>/<filename>` and SHALL NOT fall back to `${HOMEglobal}/parm/ufs/` or symlinks into `sorc/`.
8. THE Deployment_Tool SHALL include all rendered coupled-model config files in the EXPDIR Manifest with their SHA-256 hashes.

### Requirement 10: Format Validation for Coupled-Model Configs

**User Story:** As a CI engineer, I want automated validation that rendered coupled-model configs are syntactically correct, so that template errors are caught at deployment time rather than at model runtime.

#### Acceptance Criteria

1. WHEN the Deployment_Tool renders `MOM_input.j2`, THE Deployment_Tool SHALL validate that the rendered output conforms to the MOM6 parameter file format (section headers with `!` comments, parameter assignments as `PARAM = VALUE`, and valid section groupings).
2. WHEN the Deployment_Tool renders `ice_in.j2`, THE Deployment_Tool SHALL validate that the rendered output is a syntactically valid Fortran namelist file using the NamelistValidator (ampersand-delimited groups, valid variable assignments, and proper group termination with `/`).
3. WHEN the Deployment_Tool renders `ww3_shel.nml.j2`, THE Deployment_Tool SHALL validate that the rendered output is a syntactically valid Fortran namelist file using the NamelistValidator.
4. WHEN the Deployment_Tool renders `input_global_nest.nml.j2`, THE Deployment_Tool SHALL validate that the rendered output conforms to the FV3 model_configure format (one key-value pair per line, where values are parseable as string, integer, float, logical, or ISO-8601 date types).
5. IF a rendered coupled-model config file fails format validation, THEN THE Deployment_Tool SHALL emit a FATAL ERROR identifying the file, the line number, and the validation rule that was violated, and SHALL halt the deployment pipeline without sealing the EXPDIR.
6. THE Deployment_Tool SHALL run format validation on every rendered coupled-model config file as part of the deployment pipeline, after template rendering and before EXPDIR sealing.

### Requirement 11: atparse-to-Jinja2 Migration for Coupled-Model Templates

**User Story:** As a developer, I want the existing `@[VAR]` atparse syntax in coupled-model configuration templates to be converted to Jinja2 `{{ var }}` syntax, so that all templating uses a single engine.

#### Acceptance Criteria

1. THE `MOM_input.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `MOM_input_*.IN` files.
2. THE `ice_in.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `ice_in.IN` file.
3. THE `ww3_shel.nml.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `ww3_shel.nml.IN` file.
4. THE `input_global_nest.nml.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `input_global_nest.nml.IN` file.
5. FOR ALL converted templates, THE Template_Renderer SHALL produce output identical to what `atparse` would have produced given the same variable values (functional equivalence property).
6. THE `atparse_migration.py` utility SHALL be used to perform the initial `@[VAR]` → `{{ var }}` conversion, followed by manual review to apply Jinja2 conditionals for resolution-dependent parameters.
7. THE Deployment_Tool SHALL NOT depend on the `atparse` utility or `ush/atparse.bash` for rendering any coupled-model configuration file that has been converted to Jinja2.

### Requirement 12: Resolution-Dependent Ocean Parameter Defaults

**User Story:** As a workflow developer, I want resolution-dependent ocean parameters (grid dimensions, timesteps, mixing coefficients) to be declared as defaults in the Workflow_Configuration, so that switching ocean resolution requires changing only the `model.ocean.resolution` key.

#### Acceptance Criteria

1. THE Workflow_Configuration SHALL support ocean resolution-dependent defaults via a `model.ocean.defaults` mapping keyed by Ocean_Resolution (e.g., `025`, `050`, `100`, `500`) that provides default values for `nx_glb`, `ny_glb`, `dt_ocean`, `dt_therm`, and resolution-specific mixing parameters.
2. WHEN the `model.ocean.defaults` section provides a default value for a parameter and the `model.ocean` section also provides an explicit value for the same parameter, THEN THE Template_Renderer SHALL use the explicit `model.ocean` value, overriding the resolution-dependent default.
3. WHEN the Ocean_Resolution changes from `025` to `100`, THE Deployment_Tool SHALL automatically apply the correct grid dimensions, timesteps, and mixing parameters from the `model.ocean.defaults.100` section without requiring manual edits to individual parameters.
4. THE `model.ocean.defaults` section SHALL include entries for all four supported resolutions (`025`, `050`, `100`, `500`) with values derived from the corresponding legacy `MOM_input_*.IN` files.

### Requirement 13: Exclusion of Submodule-Owned Config Files from Templating

**User Story:** As a developer, I want config files owned by external submodules (NEXUS, UPP) to be copied rather than templated, so that the deployment pipeline does not duplicate or diverge from upstream sources.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL NOT create Jinja2 templates for NEXUS config files (`HEMCO_sa_*.j2`, `NEXUS_Config.rc.j2`) that reside in `sorc/nexus.fd/config/`.
2. THE Deployment_Tool SHALL NOT create Jinja2 templates for UPP parm files (`params_grib2_tbl_new`, `postxconfig-NT-*.txt`) that reside in `sorc/upp.fd/parm/`.
3. THE Deployment_Tool's Stage 4 (Stage Files) SHALL copy NEXUS config files from `sorc/nexus.fd/config/gocart/` into `<EXPDIR>/parm/chem/nexus/gocart/` without modification.
4. THE Deployment_Tool's Stage 4 (Stage Files) SHALL copy UPP parm files from `sorc/upp.fd/parm/` into `<EXPDIR>/parm/post/` without modification.
5. WHEN a file is designated as submodule-owned in the deployment manifest, THE Deployment_Tool SHALL copy the file verbatim and SHALL NOT attempt Jinja2 rendering on the file.

### Requirement 14: Elimination of sorc/ Symlinks for Coupled-Model Configs

**User Story:** As an NCO operator, I want the EXPDIR to contain no symlinks into `sorc/` submodules for coupled-model configs, so that the deployment artifact is portable and does not depend on the build tree.

#### Acceptance Criteria

1. WHEN the Deployment_Tool produces an EXPDIR, THE EXPDIR SHALL NOT contain symlinks to `sorc/ufs_model.fd/tests/parm/` for any coupled-model configuration file.
2. THE Deployment_Tool SHALL render or copy all coupled-model config files as regular files in the EXPDIR, replacing the symlink-based approach used by `sorc/link_workflow.sh`.
3. WHEN `sorc/link_workflow.sh` is executed after the Deployment_Tool has produced an EXPDIR, THE link script SHALL skip files that already exist as rendered templates in the EXPDIR (idempotent behavior).
4. THE rendered EXPDIR SHALL be deployable to an NCO production system without access to the `sorc/` directory tree.

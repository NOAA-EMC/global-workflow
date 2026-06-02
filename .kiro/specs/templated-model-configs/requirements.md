# Requirements Document

## Introduction

This feature converts the static UFS model configuration files under `parm/ufs/` in the global-workflow repository into Jinja2 templates rendered at deployment time by the Deployment_Tool's Template_Renderer. The goal is to replace the current approach of maintaining multiple static file variants (e.g., `field_table_gfdl`, `field_table_thompson`, `field_table_wsm6`) and runtime shell-based generation scripts (e.g., `ush/parsing_model_configure_FV3.sh`, `ush/parsing_ufs_configure.sh`) with a single parameterized template per configuration type.

The rendered output lands in `<EXPDIR>/parm/ufs/` as part of the immutable deployment snapshot, consistent with the deploy-time templating principle established by the parent spec (immutable-dag-workflow-modernization). This eliminates runtime template resolution, ensures reproducibility, and reduces the combinatorial explosion of static file variants.

Files in scope:
- `parm/ufs/fv3/diag_table` (and variants `diag_table_aod`, `diag_table_da`, `diag_table.aero`)
- `parm/ufs/fv3/field_table` (and 16+ physics-suite variants)
- `model_configure` (currently generated at runtime by `ush/parsing_model_configure_FV3.sh`)
- `input.nml` (FV3 namelist, currently generated at runtime)
- `ufs.configure` (ESMF/NUOPC coupling config, 6 coupling-mode variants)
- `parm/ufs/gocart/*.rc` (GOCART resource configs including `AERO_HISTORY.rc`, `ExtData.*`)

## Glossary

- **Template_Renderer**: The wxflow-backed component (from the parent spec) responsible for resolving Jinja2 templates against a deployment-time context. Uses `parse_j2yaml` with strict undefined detection and shell variable preservation.
- **Deployment_Tool**: The component that consumes `dev/` sources plus a configuration YAML and produces a rendered, self-contained EXPDIR.
- **Workflow_Configuration**: The declarative YAML document describing all workflow parameters including the `model` section that provides UFS model template context.
- **UFS_Model_Config**: A rendered configuration file consumed by the UFS Weather Model at runtime (diag_table, field_table, model_configure, input.nml, ufs.configure, or GOCART resource config).
- **Model_Context**: The subsection of the Workflow_Configuration YAML that provides template variables for UFS model configuration rendering (resolution, physics_suite, coupling_mode, output_fields, etc.).
- **EXPDIR**: An immutable, versioned deployment directory containing all rendered artifacts required to run the workflow.
- **Physics_Suite**: A named collection of physics parameterizations (e.g., `gfdl`, `thompson`, `wsm6`, `zhaocarr`) that determines tracer lists, field tables, and namelist settings.
- **Coupling_Mode**: The component coupling configuration for the UFS model (e.g., `atm`, `atmaero`, `s2s`, `s2sa`, `s2sw`, `s2swa`, `leapfrog_atm_wav`).
- **Resolution**: The cubed-sphere grid resolution identifier (e.g., `C48`, `C96`, `C384`, `C768`) that determines grid-dependent parameters.
- **GOCART_Config**: A MAPL History or ExtData resource configuration file (`.rc`) used by the GOCART aerosol component.
- **atparse**: The legacy shell-based template substitution utility (`@[VAR]` syntax) currently used for runtime config generation; replaced by Jinja2 in this feature.
- **Component_YAML**: A composable YAML file under `dev/parm/components/` that defines one UFS component's model parameters, families, and tasks. Included into the top-level Workflow_Configuration via `!INC` or `{% include %}`.
- **Nested_Include**: The wxflow `!INC` tag or Jinja2 `{% include %}` directive that allows a parent YAML to incorporate a child YAML at deployment time, resolved via the Template_Renderer's searchpath.

## Requirements

### Requirement 1: Jinja2 Template Creation for FV3 Configuration Files

**User Story:** As a workflow developer, I want a single Jinja2 template for each FV3 configuration file type, so that I no longer maintain multiple static variants per physics suite or resolution.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/fv3/diag_table.j2` into `<EXPDIR>/parm/ufs/fv3/diag_table` using the Model_Context from the Workflow_Configuration.
2. THE Deployment_Tool SHALL render `dev/parm/ufs/fv3/field_table.j2` into `<EXPDIR>/parm/ufs/fv3/field_table` using the Model_Context from the Workflow_Configuration.
3. THE Deployment_Tool SHALL render `dev/parm/ufs/fv3/model_configure.j2` into `<EXPDIR>/parm/ufs/fv3/model_configure` using the Model_Context from the Workflow_Configuration.
4. THE Deployment_Tool SHALL render `dev/parm/ufs/fv3/input.nml.j2` into `<EXPDIR>/parm/ufs/fv3/input.nml` using the Model_Context from the Workflow_Configuration.
5. WHEN the Physics_Suite is set to `gfdl` in the Model_Context, THE Template_Renderer SHALL produce a `field_table` whose tracer list is identical in name, order, and attributes to the tracer list in the corresponding legacy static file `parm/ufs/fv3/field_table_gfdl`.
6. WHEN the Physics_Suite is set to `thompson` in the Model_Context, THE Template_Renderer SHALL produce a `field_table` whose tracer list is identical in name, order, and attributes to the tracer list in the corresponding legacy static file `parm/ufs/fv3/field_table_thompson`.
7. WHEN the Physics_Suite is set to `wsm6` in the Model_Context, THE Template_Renderer SHALL produce a `field_table` whose tracer list is identical in name, order, and attributes to the tracer list in the corresponding legacy static file `parm/ufs/fv3/field_table_wsm6`.
8. WHEN the Physics_Suite is set to `zhaocarr` in the Model_Context, THE Template_Renderer SHALL produce a `field_table` whose tracer list is identical in name, order, and attributes to the tracer list in the corresponding legacy static file `parm/ufs/fv3/field_table_zhaocarr`.
9. IF the Model_Context specifies a Physics_Suite value that is not one of the supported values (`gfdl`, `thompson`, `wsm6`, `zhaocarr`), THEN THE Template_Renderer SHALL emit a FATAL ERROR identifying the unsupported Physics_Suite value and the template file.
10. THE `field_table.j2` template SHALL accept the Model_Context keys `physics_suite`, `pbl_scheme`, and `progsigma` to select the correct tracer configuration, such that the rendered output for any supported combination is identical in name, order, and attributes to the corresponding legacy static variant file (e.g., `field_table_gfdl_satmedmf_progsigma`).

### Requirement 2: Jinja2 Template Creation for ufs.configure

**User Story:** As a workflow developer, I want a single Jinja2 template for `ufs.configure`, so that coupling-mode selection is driven by configuration rather than by choosing among six static `.IN` files.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/ufs.configure.j2` into `<EXPDIR>/parm/ufs/ufs.configure` using the Model_Context from the Workflow_Configuration.
2. WHEN the Coupling_Mode is `atm`, THE Template_Renderer SHALL produce a `ufs.configure` that configures atmosphere-only execution with no ocean, ice, or wave components.
3. WHEN the Coupling_Mode is `s2s`, THE Template_Renderer SHALL produce a `ufs.configure` that configures atmosphere, ocean, and ice coupling via CMEPS.
4. WHEN the Coupling_Mode is `s2sw`, THE Template_Renderer SHALL produce a `ufs.configure` that configures atmosphere, ocean, ice, and wave coupling via CMEPS.
5. WHEN the Coupling_Mode is `atmaero`, THE Template_Renderer SHALL produce a `ufs.configure` that configures atmosphere with inline GOCART aerosol coupling.
6. FOR ALL supported Coupling_Mode values, THE rendered `ufs.configure` SHALL contain valid ESMF/NUOPC run sequence directives that the UFS model accepts without error.

### Requirement 3: Jinja2 Template Creation for GOCART Resource Configs

**User Story:** As a workflow developer, I want GOCART resource configuration files to be Jinja2 templates, so that aerosol output fields, grid labels, and emission source selections are driven by the Workflow_Configuration.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL render `dev/parm/ufs/gocart/AERO_HISTORY.rc.j2` into `<EXPDIR>/parm/ufs/gocart/AERO_HISTORY.rc` using the Model_Context from the Workflow_Configuration.
2. THE Deployment_Tool SHALL render each `dev/parm/ufs/gocart/*.rc.j2` file into the corresponding `<EXPDIR>/parm/ufs/gocart/*.rc` file.
3. THE Deployment_Tool SHALL render `dev/parm/ufs/gocart/ExtData.j2` into `<EXPDIR>/parm/ufs/gocart/ExtData` using the Model_Context, selecting the emission dataset (e.g., `qfed`, `gbbepx`, `none`) based on the `model.aerosol.emission_dataset` configuration value.
4. WHEN the Model_Context specifies a grid resolution, THE rendered `AERO_HISTORY.rc` SHALL contain grid label dimensions consistent with that resolution.
5. WHEN the Model_Context specifies active GOCART collections, THE rendered `AERO_HISTORY.rc` SHALL list those collections in the `COLLECTIONS` directive and include their field definitions.

### Requirement 4: Model_Context Schema in Workflow_Configuration

**User Story:** As a workflow developer, I want a well-defined `model` section in the Workflow_Configuration YAML, so that all UFS model template variables are declared in one place and validated at deployment time.

#### Acceptance Criteria

1. THE Workflow_Configuration SHALL include a `model` section containing at minimum the keys `resolution`, `physics_suite`, `coupling_mode`, `dt_atmos`, `output_grid`, and `output_fields`, where `resolution` is a supported Resolution value (e.g., `C48`, `C96`, `C384`, `C768`, `C1152`), `physics_suite` is a supported Physics_Suite value, `coupling_mode` is a supported Coupling_Mode value, and `dt_atmos` is a positive integer representing the atmospheric timestep in seconds.
2. THE Workflow_Configuration SHALL include a `model.fv3` subsection containing FV3-specific parameters including `npx`, `npy`, `npz`, `layout`, `io_layout`, `quilting`, `write_group`, `wrttask_per_group`, and `restart_interval`, where `npx` and `npy` are positive integers representing grid points per tile edge, `npz` is a positive integer representing vertical levels, `layout` is a two-element list of positive integers `[layout_x, layout_y]`, `io_layout` is a two-element list of non-negative integers, `quilting` is a boolean, `write_group` is a positive integer, `wrttask_per_group` is a positive integer, and `restart_interval` is a non-negative integer representing hours between restart file writes.
3. THE Workflow_Configuration SHALL include a `model.aerosol` subsection containing GOCART-specific parameters including `emission_dataset`, `active_collections`, and `grid_label`, where `emission_dataset` is one of the supported emission source identifiers (e.g., `qfed`, `gbbepx`, `none`), `active_collections` is a list of one or more GOCART collection names, and `grid_label` is a string identifying the output grid dimensions.
4. WHEN the Template_Renderer resolves a UFS model template, THE Template_Renderer SHALL use the `model` section of the Workflow_Configuration as the sole source of template variables for UFS model configuration rendering, and SHALL NOT fall back to environment variables or other configuration sections for variables defined within the `model` schema.
5. IF a required key is missing from the `model` section during template rendering, THEN THE Template_Renderer SHALL emit a FATAL ERROR identifying the missing key, the template file, and the line number, and SHALL NOT produce a rendered output file.
6. THE Workflow_Configuration SHALL support resolution-dependent defaults via a `model.defaults` mapping keyed by Resolution (e.g., `C48`, `C96`, `C384`, `C768`) that provides default values for `npx`, `npy`, `layout`, `write_group`, and `wrttask_per_group`.
7. WHEN the `model.defaults` section provides a default value for a parameter and the `model.fv3` section also provides an explicit value for the same parameter, THEN THE Template_Renderer SHALL use the explicit `model.fv3` value, overriding the resolution-dependent default.

### Requirement 5: Elimination of Runtime Config Generation Scripts

**User Story:** As a maintainer, I want the runtime shell scripts that generate model configuration files to be replaced by deploy-time templates, so that runtime execution reads only pre-rendered, immutable files.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL produce all UFS_Model_Config files at deployment time, and THE rendered EXPDIR SHALL NOT require `ush/parsing_model_configure_FV3.sh` at runtime.
2. THE Deployment_Tool SHALL produce all UFS_Model_Config files at deployment time, and THE rendered EXPDIR SHALL NOT require `ush/parsing_ufs_configure.sh` at runtime.
3. THE Deployment_Tool SHALL produce all UFS_Model_Config files at deployment time, and THE rendered EXPDIR SHALL NOT require `ush/parsing_namelists_FV3.sh` at runtime for `input.nml` generation.
4. WHEN the forecast ex-script executes, THE ex-script SHALL read the pre-rendered UFS_Model_Config files from `<EXPDIR>/parm/ufs/` and SHALL NOT invoke any template parsing function to generate them.
5. THE Deployment_Tool SHALL preserve `${VAR}` shell variable references that must expand at runtime (e.g., `${PDY}`, `${cyc}`, `${FHMAX}`) by passing them through the Template_Renderer without resolution, consistent with the shell variable preservation rule from the parent spec.

### Requirement 6: Consolidation of Static File Variants

**User Story:** As a developer, I want the 16+ `field_table_*` variants and 6 `ufs.configure.*.IN` variants replaced by parameterized templates, so that adding a new physics suite or coupling mode requires only a configuration change rather than a new static file.

#### Acceptance Criteria

1. WHEN the `field_table.j2` template is rendered for each supported Physics_Suite (`gfdl`, `thompson`, `wsm6`, `zhaocarr`) combined with each supported PBL scheme (`satmedmf`, `default`) and prognostic sigma option (`progsigma`, `default`), THE rendered output SHALL be functionally equivalent to the corresponding legacy static file.
2. WHEN the `ufs.configure.j2` template is rendered for each supported Coupling_Mode (`atm`, `atmaero`, `s2s`, `s2sa`, `s2sw`, `s2swa`, `leapfrog_atm_wav`), THE rendered output SHALL be functionally equivalent to the corresponding legacy `.IN` file after `atparse` substitution.
3. THE Deployment_Tool SHALL delete the legacy static variant files (`parm/ufs/fv3/field_table_*`, `parm/ufs/ufs.configure.*.IN`) from the source tree after the templates are validated.
4. WHEN a developer adds a new Physics_Suite, THE developer SHALL only need to add a new entry to the `model.physics_suites` configuration section and update the `field_table.j2` template conditionals, without creating a new static file.

### Requirement 7: Template Rendering Validation

**User Story:** As a CI engineer, I want automated validation that rendered UFS model configs are syntactically correct, so that template errors are caught at deployment time rather than at model runtime.

#### Acceptance Criteria

1. WHEN the Deployment_Tool renders `model_configure.j2`, THE Deployment_Tool SHALL validate that the rendered output conforms to the FV3 model_configure format (one key-value pair per line, where values are parseable as string, integer, float, logical, or ISO-8601 date types).
2. WHEN the Deployment_Tool renders `input.nml.j2`, THE Deployment_Tool SHALL validate that the rendered output is a syntactically valid Fortran namelist file (ampersand-delimited groups, valid variable assignments, and proper group termination with `/`).
3. WHEN the Deployment_Tool renders `diag_table.j2`, THE Deployment_Tool SHALL validate that the rendered output conforms to the FV3 diag_table format (title line, base-date line, file entries with 6 columns, and field entries with 8 columns).
4. WHEN the Deployment_Tool renders `ufs.configure.j2`, THE Deployment_Tool SHALL validate that the rendered output contains valid ESMF configuration syntax (label-colon-value attributes and properly nested `runSeq::` / `::` blocks).
5. WHEN the Deployment_Tool renders `field_table.j2`, THE Deployment_Tool SHALL validate that the rendered output conforms to the FV3 field_table format (valid field_type declarations, tracer blocks with `name` attributes, and matching open/close block structure).
6. IF a rendered UFS_Model_Config file fails format validation, THEN THE Deployment_Tool SHALL emit a FATAL ERROR identifying the file, the line number, and the validation rule that was violated, and SHALL halt the deployment pipeline without sealing the EXPDIR.
7. THE Deployment_Tool SHALL run format validation on every rendered UFS_Model_Config file as part of the deployment pipeline, after template rendering and before EXPDIR sealing.

### Requirement 8: atparse-to-Jinja2 Migration for Existing Templates

**User Story:** As a developer, I want the existing `@[VAR]` atparse syntax in UFS configuration templates to be converted to Jinja2 `{{ var }}` syntax, so that all templating uses a single engine.

#### Acceptance Criteria

1. THE `diag_table.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `diag_table`.
2. THE `ufs.configure.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `ufs.configure.*.IN` files.
3. THE `AERO_HISTORY.rc.j2` template SHALL use Jinja2 `{{ var }}` syntax in place of the legacy `@[VAR]` atparse syntax found in the current `AERO_HISTORY.rc`.
4. FOR ALL converted templates, THE Template_Renderer SHALL produce output identical to what `atparse` would have produced given the same variable values (functional equivalence property).
5. THE Deployment_Tool SHALL NOT depend on the `atparse` utility for rendering any UFS model configuration file that has been converted to Jinja2.

### Requirement 9: Rendered Output Placement in EXPDIR

**User Story:** As an operator, I want all rendered UFS model configuration files to reside in a predictable location within the EXPDIR, so that the forecast job finds them without path-guessing logic.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL place rendered FV3 configuration files at `<EXPDIR>/parm/ufs/fv3/` (specifically `diag_table`, `field_table`, `model_configure`, `input.nml`).
2. THE Deployment_Tool SHALL place the rendered coupling configuration at `<EXPDIR>/parm/ufs/ufs.configure`.
3. THE Deployment_Tool SHALL place rendered GOCART resource configs at `<EXPDIR>/parm/ufs/gocart/` (specifically `AERO_HISTORY.rc`, `ExtData`, and all species-specific `.rc` files).
4. WHEN the forecast ex-script references a UFS_Model_Config file, THE ex-script SHALL use the path `${EXPDIR}/parm/ufs/<subpath>` and SHALL NOT fall back to `${HOMEglobal}/parm/ufs/`.
5. THE Deployment_Tool SHALL include all rendered UFS_Model_Config files in the EXPDIR Manifest with their SHA-256 hashes.

### Requirement 10: Composable Nested Component Architecture

**User Story:** As a workflow developer, I want the Workflow_Configuration YAML to compose UFS components via nested includes, so that adding or removing a component (e.g., going from ATM-only to S2SW) is a matter of including/excluding component YAML files rather than editing a monolithic config.

#### Acceptance Criteria

1. THE Workflow_Configuration SHALL support nested component includes via the wxflow `!INC` tag or Jinja2 `{% include %}` directive, resolved at deployment time by the Template_Renderer's searchpath mechanism.
2. THE Workflow_Configuration SHALL define each UFS component (atmosphere, ocean, ice, wave, aerosol) in its own composable YAML file under `dev/parm/components/` (e.g., `atmos.yaml`, `ocean.yaml`, `ice.yaml`, `wave.yaml`, `gocart.yaml`).
3. THE top-level Workflow_Configuration SHALL compose components by including their YAML files, such that the resolved configuration merges each component's `model`, `families`, and `tasks` sections into the unified DAG.
4. WHEN a component YAML is excluded from the top-level include list, THE Deployment_Tool SHALL omit that component's tasks from the DAG, omit its model configuration templates from rendering, and SHALL NOT emit FATAL ERROR for missing component-specific template variables.
5. EACH component YAML SHALL declare its own `model` subsection (e.g., `model.ocean`, `model.ice`, `model.wave`) containing component-specific template variables (grid resolution, timestep, output fields, restart interval).
6. EACH component YAML SHALL declare its own `families` subsection listing the ecFlow families and tasks that belong to that component, using the same schema as the top-level `families` key.
7. WHEN two component YAMLs declare tasks with inter-component dependencies (e.g., wave depends on atmosphere forecast completion), THE Parser SHALL resolve cross-component trigger references using fully qualified family paths.
8. THE Deployment_Tool SHALL support a `components` key in the top-level Workflow_Configuration that lists the active components, and SHALL use this list to determine which component YAMLs to include:
   ```yaml
   components:
     - atmosphere
     - ocean
     - ice
     - wave
     - aerosol
   ```
9. WHEN the `components` list changes (e.g., removing `wave`), THE DAG_Generator SHALL produce a valid DAG that excludes wave tasks and removes any dangling trigger references to wave family paths, emitting a warning for each removed dependency.
10. THE component YAML files SHALL support Jinja2 templating within their own content, allowing component-level conditionals (e.g., `{% if model.ocean.resolution == '025' %}`) that are resolved against the merged Model_Context at deployment time.

### Requirement 11: Backward Compatibility During Migration

**User Story:** As a developer running experiments, I want the migration to be incremental so that partially-converted configurations still produce valid forecasts.

#### Acceptance Criteria

1. WHERE a UFS_Model_Config template has not yet been created for a given file, THE Deployment_Tool SHALL fall back to copying the corresponding static file from `parm/ufs/` into the EXPDIR verbatim.
2. WHILE the migration is in progress, THE Deployment_Tool SHALL support a `model.template_overrides` list in the Workflow_Configuration that specifies which config files use the new Jinja2 templates versus the legacy static files.
3. WHEN both a `.j2` template and a legacy static file exist for the same config, THE Deployment_Tool SHALL prefer the `.j2` template and SHALL emit a warning that the static file is deprecated.
4. THE rendered UFS_Model_Config files SHALL be byte-compatible with what the UFS model expects, including whitespace-sensitive formats (Fortran namelists, diag_table column alignment).

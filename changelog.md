# Changelog

All notable changes to global-workflow will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- CATChem interactive atmospheric chemistry for GCAFS via a new `APP=ATMC`
  option (atmosphere + CATChem), mirroring the existing `ATMA`
  (atmosphere + GOCART) forecast-only workflow:
  - `dev/workflow/setup_expt.py` accepts `--app ATMC` for GCAFS.
  - `dev/parm/config/gcafs/config.base.j2` recognizes `ATMC`, exports a
    `CHEM_MODEL` selector (`none`/`gocart`/`catchem`), and widens the EnKF
    APP suffix strip to `+([WAC])`.
  - `dev/parm/config/gcafs/config.aero.j2` adds `CATCHEM_CONFIG` (default
    `gcafs_aero`) and `CATCHEM_CONFIG_DIR`, mirroring `NEXUS_CONFIG`.
  - `dev/parm/config/gcafs/config.fcst.j2` passes `--catchem` to `config.ufs`
    and selects `FCSTEXEC="ufs_model_gcafs_catchem.x"` when
    `CHEM_MODEL="catchem"`.
  - `dev/parm/config/gcafs/config.ufs` parses `--catchem`, setting
    `cplchm=".true."`, `cplcat=".true."`, and `chm_model="catchem"` while
    reusing the `ufs.configure.atmaero` template.
  - `ush/parsing_ufs_configure.sh` renders `chm_model`/`cplcat` into the UFS
    configure file; `ush/parsing_namelists_fv3.sh` renders `cplcat` into
    `global_control.nml`.
  - `ush/forecast_predet.sh` and `ush/forecast_postdet.sh` gain
    `CATCHEM_predet`, `CATCHEM_rc`, `CATCHEM_postdet`, and `CATCHEM_out`:
    the CATChem runtime YAML set (`CATChem_config/species/emissions/
    field_mapping_${CATCHEM_CONFIG}.yaml`) is copied and renamed into `DATA`
    under the fixed names the CATChem NUOPC cap expects
    (`CATChem_new_config.yml`, `CATChem_species.yml`, `CATChem_emission.yml`,
    `CATChem_field_mapping.yml`), `ExtData` is linked from
    `AERO_INPUTS_DIR`, and `catchem*` diagnostics are copied to
    `$COM/model/chem/history`.
  - `dev/scripts/exglobal_forecast.sh` branches the chemistry steps onto the
    CATChem functions when `cplcat=".true."`.
  - `parm/archive/chem.yaml.j2` archives `{{ CHEM_MODEL }}*` history and
    `parm/archive/master_gcafs.yaml.j2` includes the `chem` tarball for
    `CHEM_MODEL == "catchem"` (GCAFS ATMA archiving is unchanged).
  - `dev/scripts/exgfs_aero_init_aerosol.py` uses
    `parm/chem/catchem/catchem_tracer.list` when `CHEM_MODEL == "catchem"`.
  - `dev/scripts/exglobal_prep_emissions.py` validates the selected CATChem
    emissions configuration set when `CHEM_MODEL == "catchem"`.
  - Build support: `sorc/build_opts.yaml` adds the `gcafs_catchem_model`
    entry (`./build_ufs.sh -a CATCHEM -e gcafs_catchem_model.x`) to the GCAFS
    system; `sorc/build_ufs.sh` registers the `COMPILE_ID`;
    `sorc/link_workflow.sh` links `exec/ufs_model_gcafs_catchem.x`.
  - Docs: `docs/source/user_guide/setup.rst`, `gcafs.rst`, and `clone.rst`
    describe the `ATMC` option.

### Changed

- `sorc/ufs_model.fd` pinned to ufs-weather-model
  `6dc36481b6d1347f3c5332905ad48e7e9da8675e`, the first revision exposing the
  `CATCHEM` application (`-DAPP=CATCHEM`) and the `cplcat` entry in
  `global_control.nml.IN`.

### Compatibility

- No behavior change for existing `APP=ATMA`/`ATM`/`S2S*` configurations: the
  GOCART code paths, GCAFS archive contents, and NEXUS/fire emissions
  preprocessing are untouched; all new logic is gated on `APP=ATMC` /
  `CHEM_MODEL=="catchem"` / `cplcat==".true."`.

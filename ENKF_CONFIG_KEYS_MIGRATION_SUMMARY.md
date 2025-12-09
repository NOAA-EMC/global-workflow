# EnKF Configuration Keys Migration Summary

## Overview
Migrated all configuration keys from `exglobal_enkf_earc_tars.py` scripts to a centralized method in `archive_tar_vars.py`, improving code maintainability and consistency.

## Changes Made

### 1. Created `ArchiveTar.add_enkf_config_vars()` method
**File**: `ush/python/pygfs/task/archive_tar_vars.py`

**New Method** (lines 191-278):
```python
@staticmethod
@logit(logger)
def add_enkf_config_vars(config_dict: AttrDict) -> Dict[str, Any]:
    """Collect configuration variables specific to EnKF archive operations."""
```

**Collects 45 configuration keys organized by category**:
- **Basic configuration** (6 keys): ATARDIR, current_cycle, IAUFHRS, RUN, PDY, PSLOT
- **Archive control** (7 keys): DO_ARCHCOM, ARCHCOM_TO, ROTDIR, PARMgfs, ARCDIR, SDATE, MODE
- **Ensemble configuration** (4 keys): ENSGRP, NMEM_EARCGRP, NMEM_ENS, NMEM_ENS_GFS
- **EnKF-specific operations** (3 keys): DO_CALC_INCREMENT_ENKF_GFS, DO_JEDIATMENS, lobsdiag_forenkf
- **Forecast configuration** (5 keys): FHMIN_ENKF, FHMAX_ENKF_GFS, FHOUT_ENKF_GFS, FHMAX_ENKF, FHOUT_ENKF
- **EnKF settings** (4 keys): ENKF_SPREAD, DOIAU_ENKF, IAU_OFFSET, IAUFHRS_ENKF
- **Restart intervals** (2 keys): restart_interval_enkfgdas, restart_interval_enkfgfs
- **Hybrid and data assimilation** (6 keys): DOHYBVAR, DOIAU, DO_CA, DO_CALC_INCREMENT, assim_freq, DO_JEDISNOWDA
- **Archive timing** (3 keys): ARCH_CYC, ARCH_WARMICFREQ, ARCH_FCSTICFREQ
- **Ocean and ice DA** (2 keys): DOHYBVAR_OCN, DOLETKF_OCN
- **Other** (3 keys): NET, DO_GSISOILDA, DO_LAND_IAU

**Also collects**: All COM*, COMIN*, COMOUT* template variables

### 2. Updated `exglobal_enkf_earc_tars.py` scripts
**Files**:
- `dev/scripts/exglobal_enkf_earc_tars.py`
- `scripts/exglobal_enkf_earc_tars.py`

**Before** (~35 lines for key management):
```python
# Pull out all the configuration keys needed to run the rest of archive steps
keys = ['ATARDIR', 'current_cycle', 'IAUFHRS', 'RUN', 'PDY',
        'PSLOT', 'DO_ARCHCOM', 'ARCHCOM_TO', 'ROTDIR', 'PARMgfs',
        'ARCDIR', 'SDATE', 'MODE', 'ENSGRP', 'NMEM_EARCGRP',
        'NMEM_ENS', 'DO_CALC_INCREMENT_ENKF_GFS', 'DO_JEDIATMENS',
        'lobsdiag_forenkf', 'FHMIN_ENKF', 'FHMAX_ENKF_GFS',
        'FHOUT_ENKF_GFS', 'FHMAX_ENKF', 'FHOUT_ENKF', 'ENKF_SPREAD',
        'restart_interval_enkfgdas', 'restart_interval_enkfgfs',
        'DOHYBVAR', 'DOIAU_ENKF', 'IAU_OFFSET', 'DOIAU', 'DO_CA',
        'DO_CALC_INCREMENT', 'assim_freq', 'ARCH_CYC', 'DO_JEDISNOWDA',
        'ARCH_WARMICFREQ', 'ARCH_FCSTICFREQ', 'DOHYBVAR_OCN',
        'DOLETKF_OCN', 'IAUFHRS_ENKF', 'NET', 'NMEM_ENS_GFS', 'DO_GSISOILDA', 'DO_LAND_IAU']

archive_dict = AttrDict()
for key in keys:
    archive_dict[key] = archive.task_config.get(key)
    if archive_dict[key] is None:
        print(f"Warning: key ({key}) not found in task_config!")

# Also import all COMIN* directory and template variables
for key in archive.task_config.keys():
    if key.startswith(("COM_", "COMIN_")):
        archive_dict[key] = archive.task_config.get(key)
```

**After** (3 lines):
```python
# Collect all EnKF-specific configuration keys and COM variables
# Uses centralized method from ArchiveTar utility class
archive_dict = ArchiveTar.add_enkf_config_vars(archive.task_config)
```

**Added import**:
```python
from pygfs.task.archive_tar_vars import ArchiveTar
```

## Benefits

### 1. **Single Source of Truth**
- Configuration keys defined once in `archive_tar_vars.py`
- No duplication between scripts
- Easy to add/remove keys in one place

### 2. **Better Documentation**
- Comprehensive docstring with all 45 keys documented
- Keys organized by category for clarity
- Inline comments explain purpose of each group

### 3. **Consistency**
- Matches pattern used in other archive scripts
- Same logging and error handling approach
- Follows utility class pattern (static methods)

### 4. **Maintainability**
- Easier to update key lists
- Centralized location for EnKF configuration
- Reduced code duplication (~35 lines → 3 lines per script)

### 5. **Error Handling**
- Uses logger.warning() instead of print()
- Consistent with other archive methods
- Proper logging of collected variables

## Architecture

### Before (Duplicated Keys)
```
dev/scripts/exglobal_enkf_earc_tars.py
  └─> keys = [45 keys listed inline]

scripts/exglobal_enkf_earc_tars.py
  └─> keys = [45 keys listed inline]
```

### After (Centralized Keys)
```
ush/python/pygfs/task/archive_tar_vars.py
  └─> ArchiveTar.add_enkf_config_vars() [45 keys defined once]
      ↑
      ├─ dev/scripts/exglobal_enkf_earc_tars.py
      └─ scripts/exglobal_enkf_earc_tars.py
```

## Key Categories in Detail

### Basic Configuration
Essential workflow parameters:
- `ATARDIR`: Archive directory path
- `current_cycle`: Current forecast cycle timestamp
- `IAUFHRS`: IAU hours for incremental analysis update
- `RUN`: Run type (enkfgdas, enkfgfs)
- `PDY`: Processing date (YYYYMMDD)
- `PSLOT`: Experiment name

### Ensemble Configuration
Ensemble member and grouping parameters:
- `ENSGRP`: Current ensemble group number
- `NMEM_EARCGRP`: Number of members per archive group
- `NMEM_ENS`: Total number of ensemble members (GDAS)
- `NMEM_ENS_GFS`: Total number of ensemble members (GFS)

### EnKF-Specific Operations
EnKF data assimilation settings:
- `DO_CALC_INCREMENT_ENKF_GFS`: Calculate EnKF increments for GFS
- `DO_JEDIATMENS`: Use JEDI for atmospheric ensemble DA
- `lobsdiag_forenkf`: Long observation diagnostics for EnKF

### Forecast Configuration
Forecast timing parameters:
- `FHMIN_ENKF`: Minimum forecast hour for EnKF
- `FHMAX_ENKF`: Maximum forecast hour for EnKF (GDAS)
- `FHMAX_ENKF_GFS`: Maximum forecast hour for EnKF (GFS)
- `FHOUT_ENKF`: Forecast output interval for EnKF (GDAS)
- `FHOUT_ENKF_GFS`: Forecast output interval for EnKF (GFS)

### Archive Timing
Control when to archive:
- `ARCH_CYC`: Cycle hour for archiving
- `ARCH_WARMICFREQ`: Frequency for warm initial condition archiving (days)
- `ARCH_FCSTICFREQ`: Frequency for forecast initial condition archiving (days)

## Testing Recommendations

1. **Unit Test**: Test `add_enkf_config_vars()` with sample EnKF config
2. **Integration Test**: Run EnKF archive job on test experiment
3. **Validation**: Compare archived tarballs before/after refactoring
4. **Key Coverage**: Verify all 45 keys are collected correctly

## Usage Example

```python
from pygfs.task.archive_tar_vars import ArchiveTar
from wxflow import AttrDict

# In any script that needs EnKF archive configuration
config_dict = AttrDict({...})  # From Archive.task_config
archive_dict = ArchiveTar.add_enkf_config_vars(config_dict)

# archive_dict now contains all 45 EnKF keys + all COM* variables
```

## Related Changes

This change complements the earlier ensemble member loop migration:
- **Loop migration**: Moved member loops from YAML to Python driver
- **Key centralization**: Moved configuration keys from scripts to utility module

Both changes follow the same principle: **centralize logic in utility modules, keep scripts simple**.

## Future Enhancements

Consider creating similar methods for other archive types:
- `add_gfs_config_vars()` - For GFS-specific archiving
- `add_gefs_config_vars()` - For GEFS-specific archiving
- `add_gcafs_config_vars()` - For GCAFS-specific archiving

This would further centralize configuration management across all archive operations.

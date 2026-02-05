# NET Variable Conversion Scripts

## Overview

These scripts facilitate the conversion between development and operational variable naming conventions for NOAA's global-workflow system, in accordance with EE2 standards.

## Variable Mapping

Development (global-workflow) → Operational (NCO):
- `HOMEglobal` → `HOME${NET}` (e.g., `HOMEglobal`)
- `PARMglobal` → `PARM${NET}` (e.g., `PARMglobal`)
- `USHglobal` → `USH${NET}` (e.g., `USHglobal`)
- `SCRglobal` → `SCR${NET}` (e.g., `SCRglobal`)
- `EXECglobal` → `EXEC${NET}` (e.g., `EXECglobal`)
- `FIXglobal` → `FIX${NET}` (e.g., `FIXglobal`)

## Scripts

### convert_to_net.sh

Converts development variables (global) to NET-specific variables for operational handoff.

**Usage:**
```bash
./convert_to_net.sh <NET_value> [target_directory]
```

**Example:**
```bash
# Convert for GFS operational deployment
./convert_to_net.sh gfs /path/to/deployment

# Convert for GEFS
./convert_to_net.sh gefs /path/to/deployment
```

### convert_from_net.sh

Converts NET-specific variables back to development variables (global).

**Usage:**
```bash
./convert_from_net.sh <NET_value> [target_directory]
```

**Example:**
```bash
# Restore development variables
./convert_from_net.sh gfs /path/to/code
```

## Scope

The scripts process the following file types:
- Shell scripts (*.sh, *.bash)
- Python files (*.py)
- Environment files (*.env)
- Configuration files (*.config)
- ecFlow files (*.ecf)
- Job scripts (J*)
- Execution scripts (ex*)

The scripts **exclude** these directories:
- .git (version control)
- sorc/ (source code - uses build-time substitution)
- exec/ (compiled executables)
- lib/ (libraries)
- fix/ (static data files)

## Verification

After conversion, always verify:

1. **Check file changes:**
   ```bash
   git diff | head -100
   ```

2. **Syntax validation:**
   ```bash
   bash -n dev/jobs/JGLOBAL_FORECAST
   bash -n scripts/exglobal_forecast.sh
   ```

3. **Count changes:**
   ```bash
   git diff --shortstat
   ```

## Workflow Integration

### Pre-Operational Handoff (Development → NCO)

1. Complete all development and testing in global-workflow
2. Create a deployment branch
3. Run convert_to_net.sh on deployment directory
4. Verify all syntax checks pass
5. Test the converted package
6. Hand off to NCO SPAs for installation

### Post-Operational Update (NCO → Development)

1. Receive operational code from NCO
2. Run convert_from_net.sh to restore development variables
3. Integrate changes back into global-workflow repository

## CI/CD Integration

These scripts support continuous integration by:
- Using word boundary matching (`\b`) to prevent partial replacements
- Excluding compiled/static content
- Providing verification guidance
- Supporting automated testing workflows

## EE2 Compliance

This approach aligns with EE2 standards by:
- Separating development from operational naming
- Providing clear conversion paths
- Maintaining consistency across the codebase
- Supporting multiple NET values (gfs, gefs, sfs, gcafs)

## Notes

- The conversion is **selective**, not a blanket search/replace
- Word boundaries ensure `globalworkflow` is NOT changed to `gfsworkflow`
- Scripts are idempotent - running twice produces the same result
- Always create backups before conversion

## Support

For issues or questions, contact the global-workflow development team.

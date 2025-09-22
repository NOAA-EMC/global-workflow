# Global Workflow CI/CD Pipeline Architecture

This document describes the GitLab CI/CD pipeline architecture for the global-workflow project, including the different testing modalities, configuration methods, and operational details.

## Overview

The CI/CD system supports multiple testing approaches across different computing platforms, with flexible triggering mechanisms from both GitHub Actions and GitLab scheduled pipelines. The architecture is designed to be easily extensible to new computing hosts and testing scenarios.

## Pipeline Modalities

The pipeline supports two primary testing modalities controlled by the `PIPELINE_TYPE` variable:

### 1. PR Cases (`pr_cases`)
- **Purpose**: Comprehensive end-to-end testing of experiment workflows
- **Default**: This is the default modality when no override is specified
- **Test Scope**: Full experiment cases that validate complete workflow functionality
- **Duration**: Longer running tests (typically hours)
- **Use Cases**: 
  - Pull request validation
  - Nightly regression testing
  - Full system integration testing

### 2. CTests (`ctests`)
- **Purpose**: Fast, focused testing of individual Rocoto jobs
- **Test Scope**: Unit-level tests with predefined input data
- **Duration**: Shorter running tests (typically minutes to an hour)
- **Use Cases**:
  - Quick PR validation
  - Developer feedback during development
  - Targeted testing of specific components

## Configuration Methods

### GitLab Scheduled Pipelines

GitLab scheduled pipelines can override pipeline behavior using environment variable settings:

#### Setting Pipeline Type
```bash
# In GitLab Project > Build > Pipelines schedules
# Set variables in the pipeline schedule configuration:

PIPELINE_TYPE=ctests          # Override to run CTests
PIPELINE_TYPE=pr_cases        # Explicit setting for PR cases (default behavior)
```

#### Setting Run Type for Nightly Operations
```bash
GFS_CI_RUN_TYPE=nightly      # Enables special nightly behaviors
                             # - Creates date-based directories on success
                             # - Establishes 'stable' symlinks
                             # - Triggers cleanup of old directories
```

#### Machine Selection
```bash
RUN_ON_MACHINES=hera         # Run only on Hera
RUN_ON_MACHINES=gaeac6       # Run only on Gaea C6
RUN_ON_MACHINES=all          # Run on all available machines (default)
RUN_ON_MACHINES="hera gaeac6" # Run on multiple specific machines
```

### GitHub Actions Trigger Interface

The GitHub Actions workflow provides a user-friendly interface for triggering GitLab pipelines:

#### Workflow Dispatch Inputs
- **PR Number**: Target PR to test (use `0` for develop branch)
- **Pipeline Type**: Dropdown selection between "CTests" and "PR Cases"
- **Machine Selection**: Individual checkboxes for each supported machine
  - Hera (default: enabled)
  - Gaea C6 (default: disabled)
  - Additional machines can be added as boolean inputs

## Directory Structure and Naming

### Standard Pipeline Runs
```
${CI_BUILDS_DIR}/
├── pr_cases_${CI_COMMIT_SHORT_SHA}_${CI_PIPELINE_ID}/
├── ctests_${CI_COMMIT_SHORT_SHA}_${CI_PIPELINE_ID}/
└── ...
```

### Nightly Pipeline Success Structure
When `GFS_CI_RUN_TYPE=nightly` and the pipeline succeeds:

```
${CI_BUILDS_DIR}/
├── nightly_${CI_COMMIT_SHORT_SHA}_${MMDDYY}/    # Date-based directory
├── stable -> nightly_${CI_COMMIT_SHORT_SHA}_${MMDDYY}/  # Symlink to latest successful nightly
└── ...
```

#### Nightly Directory Behavior
1. **During Execution**: Uses pipeline ID naming (`nightly_${SHA}_${PIPELINE_ID}`)
2. **On Success**: Renamed to date-based format (`nightly_${SHA}_${MMDDYY}`)
3. **Stable Link**: `stable` symlink always points to the most recent successful nightly
4. **Cleanup**: Old nightly directories are automatically removed (except the stable target)

### Directory Components
- `${CI_COMMIT_SHORT_SHA}`: Git commit short hash (8 characters)
- `${CI_PIPELINE_ID}`: GitLab pipeline unique identifier
- `${MMDDYY}`: Date format (month/day/year) for successful nightlies

## Host Configuration

### Supported Platforms
- **Hera**: NOAA HPC system
- **Gaea C6**: NOAA/NASA supercomputer

### Extending to New Hosts
To add a new computing platform:

1. **Add to GitHub Actions**: Add new boolean input in `.github/workflows/trigger-gitlab-piplines.yml`
2. **Create Host Section**: Add configuration block in `dev/ci/gitlab-ci-hosts.yml`
3. **Define Test Matrix**: Specify which test cases run on the new host
4. **Set Runner Tags**: Configure GitLab runner tags for job routing

## File Architecture

### Core Configuration Files
```
.gitlab-ci.yml                    # Main pipeline orchestration
dev/ci/
├── gitlab-ci-hosts.yml          # Host-specific configurations and test matrices
├── gitlab-ci-cases.yml          # Templates for standard experiment cases  
├── gitlab-ci-ctests.yml         # CTest framework configuration
└── cases/pr/                    # Individual test case definitions
    ├── C48_ATM.yaml
    ├── C96_atm3DVar.yaml
    └── ...
```

### GitHub Integration
```
.github/workflows/
└── trigger-gitlab-piplines.yml  # GitHub Actions trigger interface
```

## Usage Examples

### Running CTests on Hera via GitHub
1. Navigate to GitHub Actions tab
2. Select "Trigger GitLab Pipelines" workflow
3. Configure:
   - PR Number: `1234` (or `0` for develop)
   - Pipeline Type: `CTests`
   - Hera: ✅ (checked)
   - Gaea C6: ❌ (unchecked)

### Setting Up Nightly Regression Testing
In GitLab scheduled pipeline configuration:
```bash
GFS_CI_RUN_TYPE=nightly
PIPELINE_TYPE=pr_cases
RUN_ON_MACHINES=all
```

### Targeted PR Testing
For quick validation during development:
```bash
PIPELINE_TYPE=ctests
RUN_ON_MACHINES=hera
PR_NUMBER=1234  # Set via GitHub trigger
```

## Pipeline Flow

### Standard Flow
1. **Build Stage**: Compile and setup on selected machines
2. **Setup Tests**: Prepare experiment directories or CTest environment
3. **Run Tests**: Execute test cases or CTests
4. **Finalize**: Cleanup and directory management (nightly only)

### Conditional Execution
- Pipeline jobs use GitLab rules to conditionally execute based on:
  - `PIPELINE_TYPE` (ctests vs pr_cases)
  - `RUN_ON_MACHINES` (machine selection)
  - `CI_PIPELINE_SOURCE` (trigger vs schedule)

## Security and Access Control

### GitHub Actions
- Restricted to authorized users via `AUTHORIZED_GITLAB_TRIGGER_USERS` variable
- Requires manual approval through workflow_dispatch

### GitLab Integration
- Uses GitLab pipeline trigger tokens
- Environment-protected secrets for cross-platform communication

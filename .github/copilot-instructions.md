# AI Coding Agent Instructions for Global Workflow

**CRITICAL: This is a production weather forecasting system supporting NOAA's operational Global Forecast System (GFS), Global Ensemble Forecast System (GEFS), and Seasonal Forecast System (SFS). All changes must be thoroughly tested and must not disrupt operational workflows.**

This document provides comprehensive guidance for AI agents working on the NOAA Global Workflow system — a complex weather forecasting framework supporting multiple operational and research workflows.

> **Note:** If an EIB MCP-RAG server is connected, additional tool-specific guidance loads automatically via `.github/instructions/mcp.instructions.md`. No action needed — the agent will see those tools when they are available.

## System Architecture Overview

### Core Components
- **Global Workflow**: NOAA's operational weather forecasting framework
- **UFS Weather Model**: Unified Forecast System components (GFS, GEFS, SFS, GCAFS)
- **GSI/GDAS**: Global Data Assimilation System with GSI analysis
- **Job Control System**: Production job scripts calling execution scripts
- **wxflow**: Python workflow execution library with Executable class integration

### Production System Structure (GFS Operational Underpinnings)
```
jobs/                         # Production Job Control Language (JCL) scripts (89 files)
├── JGDAS_*                   # GDAS (Global Data Assimilation System) jobs
├── JGFS_*                    # GFS (Global Forecast System) jobs
├── JGLOBAL_*                 # Cross-system global jobs
├── Analysis Jobs (41)        # Data assimilation and analysis
├── Forecast Jobs (13)        # Model forecast execution
├── Post-Processing (10)      # Output product generation
└── Archive/Cleanup (7)       # Data management and cleanup

scripts/                     # Execution scripts called by jobs (83 files)
├── exgdas_*.{sh,py}         # GDAS execution scripts
├── exgfs_*.{sh,py}          # GFS execution scripts
├── exglobal_*.{sh,py}       # Global system execution scripts
├── Analysis Scripts         # Data assimilation implementations
├── Forecast Scripts         # Model execution implementations
└── Post-Processing Scripts  # Product generation implementations

ush/                        # Utility shell scripts and functions (78 files)
├── detect_machine.sh       # HPC platform detection and configuration
├── jjob_header.sh          # Standard job initialization
├── bash_utils.sh           # Common shell utilities
├── global_*.sh             # Global system utilities
├── wave_*.sh               # Wave model utilities
├── *_functions.sh          # Specialized function libraries
└── python/                 # Python utility modules

parm/                       # Parameter files and configuration templates
├── archive/                # Archive configuration templates
├── gdas/                   # GDAS system parameters
├── post/                   # Post-processing configurations
├── ufs/                    # UFS model configuration templates
├── wave/                   # Wave model parameters
└── product/                # Product generation configurations

sorc/                       # Source code and build infrastructure
├── build_all.sh            # Master build orchestration script
├── build_*.sh              # Component-specific build scripts
├── ufs_model.fd/           # UFS Weather Model source
├── gfs_utils.fd/           # GFS utility programs
├── gsi_*.fd/               # GSI data assimilation source
├── wxflow/                 # Python workflow execution library
└── CMakeLists.txt          # CMake build configuration

env/                        # HPC platform environment configurations
├── WCOSS2.env              # NOAA operational system
├── HERA.env                # NOAA RDHPCS research system
├── HERCULES.env            # MSU research system
└── *.env                   # Platform-specific settings
```

### System Execution Flow
1. **Jobs (`jobs/J*`)** - Entry points defining environment and calling execution scripts
2. **Scripts (`scripts/ex*.{sh,py}`)** - Implementation logic for each operational component
3. **Utilities (`ush/`)** - Shared functions and platform-specific utilities
4. **Parameters (`parm/`)** - Configuration templates for all system components
5. **Build System (`sorc/`)** - Source code compilation and dependency management

### Job-to-Script-to-Utility Pattern
```bash
# Example execution chain:
JGLOBAL_FORECAST              # Job sets environment, calls script
└── exglobal_forecast.py      # Script implements forecast logic
    └── forecast_det.sh       # Utility handles deterministic forecast
        └── ush/python/       # Python modules for specific tasks
```

## Workflow Orchestration System

### Workflow Management Components
- **ecFlow**: The sole orchestration engine for the global-workflow (ecFlow Python API for suite definition generation)
- **Applications Framework**: Factory pattern for different forecast systems

### Workflow Directory Structure
```
dev/workflow/              # Core workflow orchestration system
├── applications/          # Application-specific configurations (GFS, GEFS, SFS, GCAFS)
├── deployment/            # Deployment pipeline (renderer, DAG generator, stager, manifest, seal)
├── ecflow/                # ecFlow suite definition and template generation
├── hosts/                 # Host-specific configurations and settings
└── tests/                 # Workflow test suite
├── gefs_*.py              # GEFS-specific implementations
├── sfs_*.py               # SFS-specific implementations
└── gcafs_*.py             # GCAFS-specific implementations

ush/                       # Utility scripts and environment setup
├── gw_setup.sh            # Main environment setup with PYTHONPATH configuration
└── detect_machine.sh      # Machine detection and module loading
```

## Essential Developer Workflows

### Build System Commands
```bash
# Build all components (from sorc/)
./build_all.sh                     # Default build
./build_all.sh -d                  # Debug mode
./build_all.sh -v                  # Verbose output
./build_all.sh -c -A <HPC_ACCOUNT> # Compute node build with HPC account

# Build specific systems
./build_all.sh gfs               # GFS forecast system
./build_all.sh gefs              # GEFS ensemble system
./build_all.sh sfs               # Seasonal forecast system
./build_all.sh gcafs             # Climate analysis system
./build_all.sh gsi               # GSI data assimilation
./build_all.sh gdas              # GDAS system
./build_all.sh all               # All systems
```

### Experiment Setup Workflow
```bash
# 1. Environment setup (CRITICAL - must be done first)
source ush/detect_machine.sh
module use modulefiles
module load module_gwsetup.${MACHINE_ID}
source dev/workflow/gw_setup.sh

# 2. Create experiment
cd dev/workflow
python setup_expt.py gfs forecast-only \
  --pslot EXPERIMENT_NAME \
  --configdir parm/config/gfs \
  --comroot /path/to/data \
  --expdir /path/to/experiment

# 3. Generate ecFlow workflow
python setup_workflow.py /path/to/experiment ecflow
```

### Platform-Specific Development
```bash
# Supported platforms (use detect_machine.sh)
WCOSS2    # Tier 1 - Full operational support
Hercules  # Tier 1 - MSU, no TC Tracker
Hera      # Tier 2 - NOAA RDHPCS
Orion     # Tier 2 - MSU, GSI runs slowly
Gaea-C6   # Tier 1 - Fully supported platform capable of running retrospectives
Ursa      # Tier 1 - Fully supported, but cannot run high resolution or GCAFS cases
```

## Key Architectural Patterns

### Factory Pattern Usage
The system uses factory patterns for creating workflow components:

```python
# Example from application_factory.py
from wxflow import Factory
application_factory = Factory('Application')
application_factory.register('gfs_cycled', GFSCycled)
application_factory.register('gefs_forecast-only', GEFS)
```

**When to use factories:**
- Creating different workflow types (GFS, GEFS, SFS, GCAFS)
- Task generation based on application type
- Host-specific configurations

### Abstract Base Classes (ABC)
Core classes use ABC pattern for extensibility:

```python
class Application(ABC):
    @abstractmethod
    def get_tasks(self):
        pass
```

**When extending:**
- Always inherit from appropriate base classes
- Implement all abstract methods
- Follow naming conventions: `{Application}{WorkflowType}`

### Configuration Management
Configuration flows through AppConfig objects:

```python
class Tasks:
    def __init__(self, app_config: AppConfig, run: str):
        self._configs = self.app_config.configs[run]
        self._base = self._configs['base']
```

**Configuration hierarchy:**
1. `app_config.configs[run]['base']` - Base configuration
2. `app_config.run_options[run]` - Runtime options
3. Host-specific overlays from `hosts/` directory

## Workflow Task System

### Task Categories
```python
SERVICE_TASKS = ['arch_vrfy', 'earc_vrfy', 'stage_ic', 'cleanup', 'globus']
DTN_TASKS = ['arch_tars', 'earc_tars', 'fetch']
VALID_TASKS = ['prep', 'anal', 'fcst', 'upp', 'atmos_products', ...]
```

### Task Dependencies and Scheduling
- Tasks use ecFlow trigger expressions for dependency management
- Dependencies resolved through the DAG (Directed Acyclic Graph) structure
- ecFlow supports trigger, complete, event, meter, time, date, and cron primitives
- Boolean compositions (and, or, not) for complex dependency logic

### Task Resource Management
```python
def get_resource(self, task_name):
    # Resources defined per task: wallclock, cores, queue, etc.
```

## wxflow Integration Patterns

### Environment Setup
```bash
# From gw_setup.sh - CRITICAL for Python imports
if [[ -d "${HOMEglobal}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEglobal}/sorc/wxflow/src"
  export PYTHONPATH
fi
```

### Template Usage
```python
from wxflow import Template, TemplateConstants
# Templates used extensively for cyclestring substitution
template = Template(template_str)
```

### Executable Integration
- Use `wxflow.Executable` for subprocess management
- Integration points in task scripts via `SCRIPTS_PYTHONPATH`

## ecFlow Workflow Engine

### Suite Definition Generation
1. **Parse Workflow Configuration**: Read YAML workflow config
2. **Build DAG**: Construct directed acyclic graph of task dependencies
3. **Emit Suite Definition**: Generate `.def` file via ecFlow Python API
4. **Generate ecf Scripts**: Render per-task `.ecf` scripts from templates
5. **Validate**: Run EE2 compliance scan and DAG acyclicity check

### ecFlow Task Management
```python
# ecFlow suite/family/task hierarchy
from ecflow import Defs, Suite, Family, Task, Trigger, Event, Meter
```

### Job State Management
- States: unknown, queued, submitted, active, complete, aborted, suspended
- Retry logic with `ECF_TRIES` parameter
- Dependencies via trigger expressions
- Resource management via scheduler directives in `.ecf` scripts

## Development Guidelines

### Change Logging
- Each time you generate code, note the changes in changelog.md
- Follow semantic versioning guidelines
- Include date and description of changes
- Periodically perform git commits with clear messages when appropriate
- Never change the branch that we start with

### Code Style
- Follow the existing code style in the repository
- Use consistent indentation (2 spaces)
- Follow the BASH style already in code base especially "${variable}" for variables
- Never add extra whitespace at the end or beginning of lines
- Use pycodestyle for Python code
- Use shfmt where appropriate and shellcheck for linting

### Code Quality
- Ensure code is clean, well-commented, and follows best practices
- Use consistent naming conventions
- Avoid unnecessary complexity at all costs and make sure the code is easy to understand by average developers
- Avoid over-engineering solutions
- Use readable code that conveys intent and meaning over comments
- Write unit tests for new features and bug fixes
- Ensure code is modular and reusable

### Documentation
- Use numpy style docstrings for python functions and classes

## Application-Specific Patterns

### GFS (Global Forecast System)
- **Cycled**: Full data assimilation cycling
- **Forecast-only**: Forecast from existing initial conditions
- Classes: `GFSCycled`, `GFSForecastOnly`

### GEFS (Global Ensemble Forecast System)
- Ensemble forecasting system
- Special handling for ensemble members via `NMEM_ENS`
- Class: `GEFS`

### SFS (Standalone Forecast System)
- Simplified forecast-only workflow
- Class: `SFS`

### GCAFS (Global Climate Analysis Forecast System)
- Climate analysis and forecasting
- Both cycled and forecast-only modes
- Classes: `GCAFSCycled`, `GCAFSForecastOnly`

## Host Configuration

### Machine Detection
```bash
source "${HOMEglobal}/ush/detect_machine.sh"
# Sets MACHINE_ID for host-specific configurations
```

### Module Loading
```bash
module use "${HOMEglobal}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
```

### Supported Platforms
- HERA, ORION, HERCULES (Research systems)
- WCOSS2 (Operational system)
- AWS, Azure, Google Cloud (Cloud platforms)

### Throttling Configuration
```xml
<workflow cyclethrottle="1" taskthrottle="25">
  <!-- Prevent resource exhaustion -->
</workflow>
```

## Common Integration Points

### Environment Variables
```python
# Standard environment setup in tasks
envar_dict = {
    'RUN_ENVIR': 'emc',
    'HOMEglobal': self.HOMEglobal,
    'EXPDIR': self._base.get('EXPDIR'),
    'NET': self._base.get('NET'),
    'RUN': self.run,
    'CDATE': '<cyclestr>@Y@m@d@H</cyclestr>',
    'PDY': '<cyclestr>@Y@m@d</cyclestr>',
    'cyc': '<cyclestr>@H</cyclestr>',
}
```

### Cycle String Templates
```python
# ecFlow repeat date patterns for cycle management
# Cycles defined via RepeatDate in suite definition
```

### File Path Conventions
```python
# Standard directory structure
ROTDIR = f"{STMP}/RUNDIRS/{PSLOT}"
DATAROOT = f"{STMP}/RUNDIRS/{PSLOT}/{RUN}.<cyclestr>@Y@m@d@H</cyclestr>"
```

## Debugging and Troubleshooting

### Common Issues
1. **PYTHONPATH setup**: Ensure wxflow is in PYTHONPATH via `gw_setup.sh`
2. **Environment variables**: LSB vs SLURM variable mismatches
3. **Resource conflicts**: BatchQueueServer configuration for local testing
4. **ecFlow server**: Connection and authentication issues with ecflow_client

### Development Tools
- Use existing tasks: "Run Python Linting", "Run Shell Check"
- Performance analysis tools for workflow optimization
- ecflow_ui for workflow visualization and monitoring

### Testing Patterns
```python
# Unit test framework integration
def test_task_creation():
    # Test task generation and dependency resolution
```

## When Adding New Features

### New Applications
1. Create new classes in `dev/workflow/applications/`
2. Register in `application_factory.py`
3. Add ecFlow workflow configuration YAML in `dev/parm/workflow/`
4. Add host-specific configurations

### New Tasks
1. Add task definition to the workflow configuration YAML
2. Implement task generation logic
3. Define resource requirements
4. Set up dependencies via trigger expressions
5. Create corresponding job scripts

### New Hosts
1. Add machine detection in `detect_machine.sh`
2. Create host configuration in `hosts/` directory
3. Create modulefiles for environment setup
4. Update environment configurations in `env/` directory

---
Remember: This is a production weather forecasting system. Changes must be thoroughly tested and should not disrupt operational workflows. Always follow the existing patterns and conventions when extending the system

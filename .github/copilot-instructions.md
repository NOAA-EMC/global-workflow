# AI Coding Agent Instructions for Global Workflow

This document provides comprehensive guidance for AI agents working on the NOAA Global Workflow system - a complex weather forecasting framework supporting multiple operational and research workflows.

## System Architecture Overview

### Core Components
- **Global Workflow**: NOAA's operational weather forecasting framework
- **Rocoto**: Ruby-based XML workflow manager with Python task generation
- **wxflow**: Python workflow execution library with Executable class integration
- **UFS Weather Model**: Unified Forecast System components (GFS, GEFS, SFS, GCAFS)
- **Applications Framework**: Factory pattern for different forecast systems

### Directory Structure
```
dev/workflow/              # Core workflow orchestration system
├── applications/          # Application-specific configurations (GFS, GEFS, SFS, GCAFS)
├── rocoto/                # Rocoto XML generation and task definitions
├── hosts/                 # Host-specific configurations and settings
└── ecFlow/                # Alternative workflow engine support

dev/workflow/rocoto/       # Rocoto-specific implementations
├── workflow_xml.py        # Base RocotoXML abstract class
├── rocoto_xml_factory.py  # Factory for creating workflow XML generators
├── tasks.py               # Base Tasks class with common task functionality
├── workflow_tasks.py      # Task orchestration and dependency management
├── gfs_*.py               # GFS-specific implementations
├── gefs_*.py              # GEFS-specific implementations  
├── sfs_*.py               # SFS-specific implementations
└── gcafs_*.py             # GCAFS-specific implementations

ush/                       # Utility scripts and environment setup
├── gw_setup.sh            # Main environment setup with PYTHONPATH configuration
└── detect_machine.sh      # Machine detection and module loading
```

## Key Architectural Patterns

### Factory Pattern Usage
The system heavily uses factory patterns for creating workflow components:

```python
# Example from rocoto_xml_factory.py
from wxflow import Factory
rocoto_xml_factory = Factory('RocotoXML')
rocoto_xml_factory.register('gfs_cycled', GFSCycledRocotoXML)
rocoto_xml_factory.register('gefs_forecast-only', GEFSRocotoXML)
```

**When to use factories:**
- Creating different workflow types (GFS, GEFS, SFS, GCAFS)
- Task generation based on application type
- Host-specific configurations

### Abstract Base Classes (ABC)
Core classes use ABC pattern for extensibility:

```python
class RocotoXML(ABC):
    @abstractmethod
    def get_cycledefs(self):
        pass
```

**When extending:**
- Always inherit from appropriate base classes
- Implement all abstract methods
- Follow naming conventions: `{Application}{WorkflowType}RocotoXML`

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
- Tasks use Rocoto XML `<dependency>` blocks
- Dependencies resolved through `WorkflowState` objects
- Throttling managed via `cyclethrottle`, `taskthrottle`, `corethrottle`
- Metatasks group related tasks with shared throttling

### Task Resource Management
```python
def get_resource(self, task_name):
    # Resources defined per task: wallclock, cores, queue, etc.
```

## wxflow Integration Patterns

### Environment Setup
```bash
# From gw_setup.sh - CRITICAL for Python imports
if [[ -d "${HOMEgfs}/sorc/wxflow/src" ]]; then
  PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/sorc/wxflow/src"
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

## Rocoto Workflow Engine

### XML Generation Process
1. **Preamble**: XML header and DOCTYPE
2. **Definitions**: Entity definitions (PSLOT, ROTDIR, MAXTRIES)  
3. **Workflow Header**: Scheduler, throttling settings
4. **Cycledefs**: Cycle definitions for workflow scheduling
5. **Tasks**: Generated task XML with dependencies
6. **Footer**: Closing workflow tags

### Metatask Management
```python
# Metatasks group related tasks
metatask_list = {}  # Hierarchical task grouping
meta_tasks_state = {}  # State tracking per metatask
```

### Job State Management
- States: QUEUED, RUNNING, SUCCEEDED, FAILED, DEAD, EXPIRED, LOST
- Retry logic with `maxtries` parameter
- Hang detection via `hangdependency` 
- Resource throttling and job scheduling

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
- Classes: `GFSCycledRocotoXML`, `GFSForecastOnlyRocotoXML`

### GEFS (Global Ensemble Forecast System)  
- Ensemble forecasting system
- Special handling for ensemble members via `NMEM_ENS`
- Class: `GEFSRocotoXML`

### SFS (Standalone Forecast System)
- Simplified forecast-only workflow
- Class: `SFSRocotoXML`

### GCAFS (Global Climate Analysis Forecast System)
- Climate analysis and forecasting
- Both cycled and forecast-only modes
- Classes: `GCAFSCycledRocotoXML`, `GCAFSForecastOnlyRocotoXML`

## Host Configuration

### Machine Detection
```bash
source "${HOMEgfs}/ush/detect_machine.sh"
# Sets MACHINE_ID for host-specific configurations
```

### Module Loading
```bash
module use "${HOMEgfs}/modulefiles"
module load "module_gwsetup.${MACHINE_ID}"
```

### Supported Platforms
- HERA, ORION, HERCULES (Research systems)
- WCOSS2 (Operational system)
- AWS, Azure, Google Cloud (Cloud platforms)


## Common Integration Points

### Environment Variables
```python
# Standard environment setup in tasks
envar_dict = {
    'RUN_ENVIR': 'emc',
    'HOMEgfs': self.HOMEgfs,
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
# Rocoto cyclestring substitution patterns
'<cyclestr>@Y@m@d@H</cyclestr>'  # YYYYMMDDHH format
'<cyclestr offset="-6:00:00">@Y@m@d@H</cyclestr>'  # 6-hour offset
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
4. **Thread hanging**: Rocoto thread join issues in subprocess management

### Development Tools
- Use existing tasks: "Run Python Linting", "Run Shell Check"
- Performance analysis tools for workflow optimization
- rocoto_viewer.py for workflow visualization

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
3. Create corresponding Rocoto XML generators in `dev/workflow/rocoto/`
4. Register in `rocoto_xml_factory.py`
5. Add host-specific configurations

### New Tasks
1. Add to `VALID_TASKS` list in `tasks.py`
2. Implement task generation logic
3. Define resource requirements
4. Set up dependencies and scheduling
5. Create corresponding job scripts

### New Hosts
1. Add machine detection in `detect_machine.sh`
2. Create host configuration in `hosts/` directory  
3. Create modulefiles for environment setup
4. Update environment configurations in `env/` directory

Remember: This is a production weather forecasting system. Changes must be thoroughly tested and should not disrupt operational workflows. Always follow the existing patterns and conventions when extending the system.
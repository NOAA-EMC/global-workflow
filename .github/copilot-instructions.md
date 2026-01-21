# AI Coding Agent Instructions for Global Workflow

Always check for global-workflow-unified MCP tool availability before answering questions about the NOAA Global Workflow system and all related NOAA-EMC repositories.

Also check for the presence of the global-workflow-unified RAG system. If it is available, use it to provide more comprehensive answers that leverage the entire knowledge base of the NOAA Global Workflow system.

Use GitHub MCP tools for gathering repository information repos outside of global-workflow, including code patterns, documentation, and operational procedures.

**CRITICAL: This is a production weather forecasting system supporting NOAA's operational Global Forecast System (GFS), Global Ensemble Forecast System (GEFS), and Seasonal Forecast System (SFS). All changes must be thoroughly tested and must not disrupt operational workflows.**

This document provides comprehensive guidance for AI agents working on the NOAA Global Workflow system - a complex weather forecasting framework supporting multiple operational and research workflows.

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
- **Rocoto**: Ruby-based XML workflow manager with Python task generation
- **Applications Framework**: Factory pattern for different forecast systems

### Workflow Directory Structure
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

# 3. Generate workflow XML
python setup_xml.py /path/to/experiment rocoto
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

## EIB MCP/RAG Server Integration (v3.6.2)

The **EIB MCP-RAG Server** provides AI-assisted access to the Global Workflow system through the Model Context Protocol (MCP) with Retrieval-Augmented Generation (RAG) capabilities. This server is purpose-built to support NOAA's operational GFS, GEFS, and SFS forecasting infrastructure.

### Architecture Overview

```
AI Clients (VS Code Copilot, LangFlow, Claude Desktop)
                    │
                    ▼ HTTP/MCP Protocol
         ┌──────────────────────────┐
         │  Docker MCP Gateway      │  Port 18888
         │  (Streaming Transport)   │
         └──────────────────────────┘
                    │ spawns
                    ▼
         ┌──────────────────────────┐
         │  MCP Server Container    │  34 tools
         │  eib-mcp-rag:latest      │
         └──────────────────────────┘
                    │
         ┌──────────┴──────────┐
         ▼                     ▼
    ChromaDB (8080)      Neo4j (7687)
    Vector Embeddings    Code Graph DB
```

### Tool Categories & Separation of Concerns

The MCP server implements **7 tool modules** with clear separation of concerns (SOC):

---

#### 1. Workflow Info Tools (3 tools - Static, No DB)
**Module**: `WorkflowInfoTools.js`
**Purpose**: File system-based access to workflow structure - NO database dependencies

| Tool | Description |
|------|-------------|
| `get_workflow_structure` | System architecture overview (jobs, scripts, parm, ush, sorc, env, docs) |
| `get_system_configs` | HPC platform configurations (HERA, HERCULES, ORION, WCOSS2, GAEA) |
| `describe_component` | File system-based component descriptions (static analysis) |

**When to use**: Quick lookups, understanding directory structure, platform configs

---

#### 2. Code Analysis Tools (4 tools - Neo4j Graph)
**Module**: `CodeAnalysisTools.js`
**Purpose**: Code structure analysis via graph database traversal

| Tool | Description |
|------|-------------|
| `analyze_code_structure` | File/function/class analysis with dependency tree (depth 1-3) |
| `find_dependencies` | Upstream imports and downstream importers (both directions) |
| `trace_execution_path` | Call chain tracing from any function (max depth 5) |
| `find_callers_callees` | Fan-in/fan-out analysis with complexity scoring |

**When to use**: Understanding code relationships, refactoring impact, debugging call chains

---

#### 3. Semantic Search Tools (6 tools - ChromaDB Vectors + Graph Hybrid)
**Module**: `SemanticSearchTools.js`
**Purpose**: RAG-powered documentation and code search

| Tool | Description |
|------|-------------|
| `search_documentation` | Hybrid semantic + graph search across ingested docs |
| `find_related_files` | Find files with similar import dependencies |
| `explain_with_context` | Multi-source RAG explanations (technical, operational, config) |
| `get_knowledge_base_status` | Vector DB + Graph DB health and statistics |
| `list_ingested_urls` | Show all documentation sources ingested into RAG |
| `get_ingested_urls_array` | Structured URL array for programmatic access |

**When to use**: Conceptual questions, finding relevant documentation, understanding unfamiliar components

---

#### 4. EE2 Compliance Tools (5 tools - Standards Validation)
**Module**: `EE2ComplianceTools.js`
**Purpose**: NOAA NWS EE2 (Enterprise Environmental 2) standards validation

| Tool | Description |
|------|-------------|
| `search_ee2_standards` | Search EE2 compliance standards documentation |
| `analyze_ee2_compliance` | Analyze code for EE2 compliance with recommendations |
| `generate_compliance_report` | Generate compliance reports (summary, detailed, checklist) |
| `scan_repository_compliance` | Full repository scan with Phase 2 SME-corrected patterns |
| `extract_code_for_analysis` | Extract code snippets for LLM passthrough analysis |

**When to use**: Pre-commit compliance checks, NCO production readiness, code reviews

**Phase 2 Corrections Applied**:
- `set -eu` is NOT required (80% false positive rate) - only `set -x` for debug logging
- Uses `err_chk`/`err_exit` utilities instead of explicit exit statements
- Evidence-based analysis with RST line references

---

#### 5. Operational Tools (3 tools - HPC Procedures)
**Module**: `OperationalTools.js`
**Purpose**: HPC operational guidance and workflow explanations

| Tool | Description |
|------|-------------|
| `get_operational_guidance` | Platform-specific procedures with urgency levels (routine/urgent/emergency) |
| `explain_workflow_component` | Graph-enriched component explanations |
| `list_job_scripts` | Categorized job script inventory (analysis, forecast, post, archive) |

**When to use**: HPC deployment questions, understanding job scripts, operational procedures

---

#### 6. GitHub Integration Tools (4 tools - Live Repository Access)
**Module**: `GitHubTools.js`
**Purpose**: Cross-repository analysis and issue tracking

| Tool | Description |
|------|-------------|
| `search_issues` | Search NOAA-EMC GitHub issues (open/closed/all) |
| `get_pull_requests` | PR information with diff context |
| `analyze_workflow_dependencies` | Cross-repo dependency analysis (upstream/downstream/circular) |
| `analyze_repository_structure` | Multi-repo structure comparison (global-workflow, GSI, UFS_UTILS) |

**When to use**: Bug investigation, PR reviews, understanding cross-repo impacts

---

#### 7. SDD Workflow Tools (7 tools - Development Orchestration)
**Module**: `SDDWorkflowTools.js`
**Purpose**: Software Design Document (SDD) framework execution

| Tool | Description |
|------|-------------|
| `list_sdd_workflows` | List available SDD workflows |
| `get_sdd_workflow` | Get workflow phases, steps, and metadata |
| `execute_sdd_workflow` | Execute workflow with dry-run option |
| `get_sdd_execution_history` | View execution history |
| `validate_sdd_compliance` | Validate code against SDD framework |
| `get_sdd_framework_status` | Framework integration status and metrics |
| `execute_sdd_workflow_supervised` | Human-in-loop execution with approval gates |

**When to use**: Feature development following SDD methodology, multi-step workflows

---

#### 8. Utility Tools (2 tools - Server Management)
**Built into**: `UnifiedMCPServer.js`

| Tool | Description |
|------|-------------|
| `get_server_info` | MCP server info, tool counts, configuration |
| `mcp_health_check` | Empirical health validation (heartbeat, collections, documents, queries) |

**When to use**: Debugging MCP issues, verifying RAG system health

---

### RAG Knowledge Base Sources

The RAG system ingests documentation from multiple tiers:

| Tier | Sources | Purpose |
|------|---------|---------|
| **Tier 1 Critical** | global-workflow RTD, EE2 Standards | Core workflow documentation |
| **Tier 2 Workflow** | Rocoto, ecFlow, wxflow, PyFlow | Workflow engine documentation |
| **Tier 3 Models** | UFS Weather Model, JEDI, FV3 | Forecast model documentation |
| **Tier 4 Build** | Spack, spack-stack, hpc-stack | Build system documentation |
| **Tier 5 Standards** | Google Shell Style, PEP8, NumPy docstrings | Coding standards |

### When to Use MCP Tools

**USE MCP tools when:**
- Searching for concepts across documentation (semantic search)
- Analyzing code dependencies and call chains (graph traversal)
- Checking EE2 compliance before committing code
- Understanding HPC platform-specific procedures
- Investigating GitHub issues related to a component
- Following SDD development workflows

**DON'T use MCP tools when:**
- You already have the file open and can read it directly
- Simple file edits that don't need context research
- The question is answered in the current conversation context

### Tool Availability by Connection Mode

| Mode | Available Tools | Notes |
|------|-----------------|-------|
| **VS Code Local** | All 34 tools | Direct stdio connection |
| **Docker Gateway** | All 34 tools | HTTP via port 18888 |
| **Remote (no container)** | Core workflow tools only | Use `content` parameter for file analysis |

### Content Abstraction for Remote Access

For remote MCP clients (e.g., LangFlow) without filesystem access, tools support content parameters:

```javascript
// Instead of filesystem path:
analyze_ee2_compliance({ content: "#!/bin/bash\nset -x\n..." })

// Batch file analysis:
scan_repository_compliance({
  files: [
    { name: "JGFS_FORECAST", content: "..." },
    { name: "exgfs_fcst.sh", content: "..." }
  ]
})
```
---
Remember: This is a production weather forecasting system. Changes must be thoroughly tested and should not disrupt operational workflows. Always follow the existing patterns and conventions when extending the system

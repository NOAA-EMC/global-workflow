---
applyTo: "**"
excludeAgent: "code-review"
---

_Note:_ The YAML front matter in this file uses the GitHub Copilot instructions schema; `excludeAgent: "code-review"` ensures these instructions are not applied to the Copilot Code Review agent. See the official schema documentation at https://aka.ms/github-copilot-instructions-schema.
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

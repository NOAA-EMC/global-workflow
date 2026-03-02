---
applyWhen: hasActiveMCPServer("eib-mcp-rag-full")
excludeAgent: "code-review"
---

_Note:_ The YAML front matter in this file uses the GitHub Copilot instructions schema; `excludeAgent: "code-review"` ensures these instructions are not applied to the Copilot Code Review agent. See the official schema documentation at https://aka.ms/github-copilot-instructions-schema.
# EIB MCP/RAG Server — Tool Guide for Global Workflow (v7.21.0)

This file loads **only** when the EIB MCP-RAG server is connected. It provides tool selection guidance for AI agents working on global-workflow with MCP + RAG capabilities (48 tools across 9 modules backed by Neo4j graph DB and ChromaDB vector store).

## MCP-First Policy

**Prefer MCP tools over shell commands** for code analysis, documentation search, and compliance checking. Use `read_file`/`grep_search` only for exact line-level reads or literal string searches.

## Tool Modules (48 tools / 9 modules)

### 1. Workflow Info (3 tools — Filesystem only)
| Tool | Use For |
|------|---------|
| `get_workflow_structure` | System architecture overview |
| `get_system_configs` | HPC platform configurations |
| `describe_component` | Component documentation |

### 2. Code Analysis (6 tools — Neo4j)
| Tool | Use For |
|------|---------|
| `analyze_code_structure` | AST-level file/function analysis |
| `find_dependencies` | Import graph (upstream + downstream) |
| `trace_execution_path` | Call chain tracing |
| `find_callers_callees` | Fan-in/fan-out with complexity scoring |
| `trace_full_execution_chain` | End-to-end execution chain across files |
| `find_env_dependencies` | Environment variable lineage |

### 3. Semantic Search (6 tools — ChromaDB + Neo4j)
| Tool | Use For |
|------|---------|
| `search_documentation` | Semantic search across ingested docs |
| `find_related_files` | Vector similarity for related code/docs |
| `explain_with_context` | RAG-powered explanations with citations |
| `get_knowledge_base_status` | DB health and collection stats |
| `list_ingested_urls` | Documentation sources ingested into RAG |
| `get_ingested_urls_array` | Structured URL array for programmatic access |

### 4. EE2 Compliance (5 tools — ChromaDB)
| Tool | Use For |
|------|---------|
| `search_ee2_standards` | Search EE2 standards documentation |
| `analyze_ee2_compliance` | Check file against NCO standards |
| `generate_compliance_report` | Formatted compliance report |
| `scan_repository_compliance` | Bulk repo scan (Phase 2 SME-corrected) |
| `extract_code_for_analysis` | Extract code snippets for LLM analysis |

**Note**: `set -eu` is NOT required (80% false positive). Uses `err_chk`/`err_exit` utilities.

### 5. Operational (4 tools — ChromaDB)
| Tool | Use For |
|------|---------|
| `get_operational_guidance` | HPC platform-specific procedures |
| `explain_workflow_component` | Graph-enriched component explanations |
| `list_job_scripts` | Categorized job script inventory |
| `get_job_details` | Detailed job script analysis |

### 6. GraphRAG + Session State (9 tools — ChromaDB + Neo4j)
| Tool | Required Param | Use For |
|------|----------------|--------|
| `get_code_context` | `symbol` | GGSR neighborhood + community summary |
| `search_architecture` | `query` | Semantic search over community summaries |
| `find_similar_code` | `code_or_symbol` | Vector similarity + graph enrichment |
| `get_change_impact` | `symbol` | Blast radius with risk scoring |
| `trace_data_flow` | `from_symbol` | Data flow across codebase |
| `mark_as_modified` | `file_path` | Track file modifications in active session |
| `get_session_context` | *(none)* | Aggregated view of session work |
| `checkpoint_state` | `name` | Snapshot session state for recovery |
| `restore_checkpoint` | `checkpoint_id` | Roll back to a named checkpoint |

### 7. GitHub Integration (4 tools — GitHub API)
| Tool | Use For |
|------|---------|
| `search_issues` | Search NOAA-EMC GitHub issues |
| `get_pull_requests` | PR information with diff context |
| `analyze_workflow_dependencies` | Cross-repo dependency analysis |
| `analyze_repository_structure` | Multi-repo structure comparison |

### 8. SDD Workflows (9 tools — Filesystem)
| Tool | Use For |
|------|---------|
| `list_sdd_workflows` | List all workflow phase specs |
| `get_sdd_workflow` | Get specific phase details |
| `start_sdd_session` | Start a tracked session |
| `record_sdd_step` | Record step completion |
| `get_sdd_session` | Resume active session |
| `complete_sdd_session` | Complete and archive session |
| `get_sdd_execution_history` | View execution history |
| `validate_sdd_compliance` | Validate against SDD framework |
| `get_sdd_framework_status` | Framework status and metrics |

### 9. Utility (2 tools — Built-in)
| Tool | Use For |
|------|---------|
| `get_server_info` | Server version, tool counts |
| `mcp_health_check` | Full health validation |

## When to Use MCP vs Direct Access

| Task | Use MCP Tool | Use read_file/grep |
|------|-------------|-------------------|
| "What does this file do?" | `analyze_code_structure` | No |
| "What calls this function?" | `find_callers_callees` | No |
| "How does X subsystem work?" | `search_architecture` | No |
| "Is this code EE2-compliant?" | `analyze_ee2_compliance` | No |
| "Show me line 45-100" | No | `read_file` |
| "Search for literal 'FOO'" | No | `grep_search` |

**Best practice**: MCP tools for discovery, then `read_file` for specific line-level details.

## RAG Knowledge Base Tiers

| Tier | Sources | Purpose |
|------|---------|---------|
| 1 | global-workflow RTD, EE2 Standards | Core workflow docs |
| 2 | Rocoto, ecFlow, wxflow, PyFlow | Workflow engines |
| 3 | UFS Weather Model, JEDI, FV3 | Forecast models |
| 4 | Spack, spack-stack, hpc-stack | Build systems |
| 5 | Shell Style Guide, PEP8, NumPy | Coding standards |

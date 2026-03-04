---
applyWhen: hasActiveMCPServer("eib-mcp-gateway")
---

<!-- Regenerate tool tables with: cd mcp_server_node && node scripts/generate-tool-docs.js -->
# EIB MCP/RAG Server — Tool Guide for Global Workflow (v7.21.0)

This file loads **only** when the EIB MCP-RAG gateway is connected. It provides tool selection guidance for AI agents working on global-workflow with MCP + RAG capabilities (39 tools across 8 modules backed by Neo4j graph DB and ChromaDB vector store).

## MCP-First Policy

**Prefer MCP tools over shell commands** for code analysis, documentation search, and compliance checking. Use `read_file`/`grep_search` only for exact line-level reads or literal string searches.

## Tool Modules (39 tools / 8 modules)

### 1. Workflow Info (3 tools — Filesystem)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `get_workflow_structure` | — | `component`, `structure_data` | Get the structure and overview of the global workflow system |
| `get_system_configs` | — | `platform`, `config_type`, `content` | Get system configuration information for different HPC platforms |
| `describe_component` | `component` | `show_content`, `content`, `file_type` | Get basic description of a workflow component (file system only) |

### 2. Code Analysis (6 tools — Neo4j)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `analyze_code_structure` | `file_path` | `include_dependencies`, `depth`, `token_budget` | Analyze code structure, relationships, and dependencies for a specific file |
| `find_dependencies` | `target` | `direction`, `max_depth`, `token_budget` | Find all dependencies (imports) and dependents (importers) for a file or module |
| `trace_execution_path` | `function_name` | `file_path`, `max_depth`, `include_callers`, `include_weights`, `token_budget` | Trace the execution path from a starting function through call chains |
| `find_callers_callees` | `function_name` | `file_path`, `include_source`, `token_budget`, `cross_language` | Find all functions that call a target function and functions it calls |
| `trace_full_execution_chain` | `start` | `direction`, `max_depth`, `languages` | Trace complete execution chain across Shell, Python, and Fortran language boundaries |
| `find_env_dependencies` | `variable_name` | `show_exports`, `limit`, `token_budget` | Find all scripts that depend on or export a specific environment variable |

### 3. Semantic Search (6 tools — ChromaDB + Neo4j)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `search_documentation` | `query` | `collection`, `max_results`, `include_graph`, `similarity_threshold` | Hybrid semantic + graph search across workflow documentation and code |
| `find_related_files` | `file_path` | `max_results`, `include_documentation` | Find files with similar dependencies and import relationships |
| `explain_with_context` | `topic` | `context_type`, `detail_level` | Provide comprehensive explanations using hybrid search |
| `get_knowledge_base_status` | — | `include_graph`, `include_vector` | Get comprehensive knowledge base statistics |
| `list_ingested_urls` | — | `format`, `source_filter` | List all URLs that have been ingested into the RAG knowledge base |
| `get_ingested_urls_array` | — | `include_failed` | Get a structured array of all ingested URLs for programmatic access |

### 4. EE2 Compliance (5 tools — ChromaDB)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `search_ee2_standards` | `query` | `category`, `max_results`, `include_examples` | Search EE2 compliance standards and documentation |
| `analyze_ee2_compliance` | `content` | `analysis_type`, `include_recommendations` | Analyze code or documentation for EE2 compliance |
| `generate_compliance_report` | — | `scope`, `categories`, `format` | Generate comprehensive EE2 compliance report |
| `scan_repository_compliance` | `name`, `content` | `files`, `path`, `repository_path`, `file_patterns`, `sample_size`, `categories` | Scan repository for EE2 compliance (Phase 2 SME-corrected) |
| `extract_code_for_analysis` | `name`, `content` | `files`, `path`, `content_type`, `categories`, `file_pattern`, `max_files` | Extract code snippets from files for EE2 compliance analysis |

**Note**: `set -eu` is NOT required (80% false positive). Uses `err_chk`/`err_exit` utilities.

### 5. Operational (4 tools — ChromaDB)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `get_operational_guidance` | `operation` | `platform`, `urgency` | Get operational guidance and best practices for HPC operations |
| `explain_workflow_component` | `component` | `detail_level` | Get detailed explanation of a workflow component with graph context |
| `list_job_scripts` | — | `category`, `search`, `format`, `job_list`, `files`, `name`, `content` | List and categorize job scripts in the workflow |
| `get_job_details` | `job_name` | `include_content`, `include_config`, `include_chromadb` | Get comprehensive details about a J-Job including inputs, outputs, dependencies |

### 6. GraphRAG (9 tools — ChromaDB + Neo4j)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `get_code_context` | `symbol` | `depth`, `include_community`, `token_budget` | Get comprehensive context for a code symbol including graph neighborhood and community summaries |
| `search_architecture` | `query` | `max_results` | Search the codebase architecture for high-level understanding via community summaries |
| `find_similar_code` | `code_or_symbol` | `similarity_threshold`, `max_results` | Find code patterns semantically similar to a given symbol or description |
| `get_change_impact` | `symbol` | `change_type`, `include_indirect` | Analyze the blast radius of changing a code symbol with risk scoring |
| `trace_data_flow` | `from_symbol` | `to_symbol`, `max_depth` | Trace execution flow from a source symbol through the codebase |
| `mark_as_modified` | `file_path` | `change_type`, `description` | Record a file modification in the active session |
| `get_session_context` | — | `include_dirty` | Get aggregated view of the active session: examined symbols, file modifications |
| `checkpoint_state` | `name` | `description` | Snapshot current session state to a checkpoint for recovery |
| `restore_checkpoint` | `checkpoint_id` | — | Roll back session state to a previously created checkpoint |

### 7. GitHub Integration (4 tools — GitHub API)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `search_issues` | `query` | `repository`, `state`, `labels` | Search GitHub issues across workflow repositories |
| `get_pull_requests` | — | `repository`, `state`, `limit` | Get pull request information and changes |
| `analyze_workflow_dependencies` | `component` | `analysis_type`, `include_external` | Analyze dependencies and relationships between workflow components |
| `analyze_repository_structure` | — | `repositories`, `analysis_depth` | Analyze structure and components across multiple repositories |

### 8. Utility (2 tools — Built-in)

| Tool | Required | Optional | Description |
|------|----------|----------|-------------|
| `get_server_info` | — | `include_capabilities` | Get information about the MCP server and available tools |
| `mcp_health_check` | — | `detailed`, `deep`, `functional` | Check the health status of all MCP server components |

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

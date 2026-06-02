# Requirements Document

## Introduction

This feature is a **completion-and-verification ("definition of done") feature**, not a from-scratch build. Its sole purpose is to close the gap between *"the parent spec's tasks are marked complete"* and *"the parent goal is provably realized"*.

The parent spec — **immutable-dag-workflow-modernization** (`.kiro/specs/immutable-dag-workflow-modernization/`) — defines 14 requirements and 14 correctness Properties for turning the NOAA EMC global-workflow into an immutable, ecFlow-only, DAG-based, deployment-time-templated system that produces a self-contained, versioned EXPDIR artifact. Two child specs (`templated-model-configs`, `coupled-model-configs`) cover the UFS model-config templating slice, and a large implementation already exists under `dev/workflow/deployment/` with a ~575-test suite.

However, the parent `tasks.md` marks all 65 tasks "complete" while objective verification proves the goal is **not** actually realized. Running the actual repository surfaced the following authoritative gaps (treated here as ground truth):

1. **Runtime atparse is still live.** `ush/forecast_postdet.sh` still sources `parsing_namelists_WW3.sh`, `parsing_namelists_MOM6.sh`, `parsing_namelists_CICE.sh`, and `parsing_namelists_GOCART.sh`, which run `@[VAR]` substitution at run time even though deploy-time Jinja2 templates already exist under `dev/parm/ufs/{ocean,ice,wave,gocart}/`. Legacy `@[...]` files remain on disk (`parm/ufs/fv3/diag_table`, `parm/ufs/gocart/AERO_HISTORY.rc`). Additional non-UFS runtime atparse usages remain (`parm/prep_sfc/snow2mdl.nml.tmpl` via `ush/prep_sfc_snow.sh`, `ush/regrid_gsiSfcIncr_to_tile.sh`, `scripts/exgfs_wave_nawips.sh` via `gempak.parm.tmpl`), and `ush/atparse.bash` is still present and sourced. This violates parent Req 4.6 / Req 8 / Property 14.
2. **Rocoto decommission is incomplete.** `dev/workflow/setup_workflow.py` still contains 7 "rocoto" references; parent Req 14.3 requires the rocoto subparser and all rocoto-conditioned branches to be removed (a deprecation guard may remain). The directory trees and `rocoto_viewer.py` are already absent (Req 14.1, 14.2, 14.4 satisfied).
3. **Dependency pinning is not enforced in the environment.** `dev/workflow/requirements.txt` pins `wxflow==0.3.0` and `uwtools==2.16.0`, but `wxflow` is not installed in the active environment, so `tests/test_configuration.py` and `tests/test_hosts.py` fail to import. The Deployment_Tool's FATAL-ERROR-on-mismatch guard is therefore not effective. This violates parent Req 9.4 / 9.5.
4. **The parent correctness Properties do not all pass.** Running the 575-test suite yields **6 failed, 25 errors, 2 import errors, 569 passed, 2 skipped**. A dominant root cause is that the deployment pipeline FATALs on missing submodule-owned sources (e.g., `sorc/nexus.fd/config/gocart`), which blocks clean deployment fixtures for Properties 1, 3, 4, 8, and 14.
5. **There is no single end-to-end "goal realization" gate** that asserts all 14 parent Properties pass against a freshly deployed EXPDIR, and no reconciliation that the parent `tasks.md` "complete" status reflects verified reality.

Success for this feature is defined objectively as: **all 14 parent Properties green, the full `dev/workflow` test suite running with zero failures, zero errors, and zero collection/import errors, plus the specific remediations above completed**, all gated in CI and traceable back to the parent requirement and Property numbers. This document does not restate the parent spec; it references it and adds the precise, machine-verifiable acceptance criteria needed to declare the parent goal "done".

## Glossary

This feature reuses the parent spec's defined terms. The following are referenced unchanged from `immutable-dag-workflow-modernization`:

- **Global_Workflow**, **EXPDIR**, **Snapshot_ID**, **Manifest**, **Deployment_Tool**, **Template_Renderer**, **DAG_Generator**, **Universal_Wrapper**, **Atomic_Publish**, **JJob**, **Ex_Script**, **Workflow_Configuration**, **Suite_Definition**, **Platform**, **wxflow**, **uwtools**, **NCO**, **EE2** — as defined in the parent Requirements Document.
- **Property N** — the parent spec's correctness Property number N (1–14), as defined in the parent Design Document:
  1. Deployment Determinism, 2. Manifest Integrity, 3. Immutability, 4. Self-Containment, 5. Atomicity, 6. Idempotence, 7. Statelessness, 8. Platform Isolation, 9. Parser Round-Trip, 10. Printer Round-Trip, 11. ecFlow Round-Trip, 12. DAG Acyclicity, 13. Definition Fidelity, 14. No Unresolved Tokens.

Terms defined newly by this feature:

- **Goal_Realization_Gate**: A single authoritative CI gate that executes the full `dev/workflow` test suite (including the proving tests for all 14 parent Properties) against a freshly deployed EXPDIR and passes only when there are zero failures, zero errors, and zero collection/import errors.
- **Property_Suite**: The set of automated tests that prove parent Properties 1 through 14, located under `dev/workflow/tests/`.
- **Token_Scan**: The automated scan that searches rendered EXPDIR files (and, where in scope, repository runtime scripts) for unresolved templating tokens, specifically the atparse pattern `@[...]` and the Jinja2 patterns `{{`, `{%`, and `{#`.
- **Atparse_Exemption_Registry**: A version-controlled file that enumerates every file path permitted to retain runtime atparse/`@[...]` templating, each with a recorded justification, that the Token_Scan reads to distinguish allowed exemptions from violations.
- **Verification_Environment**: The declared, reproducible software environment (defined by `dev/workflow/requirements.txt` and the project's environment setup) in which the Deployment_Tool and the full test suite are intended to run.
- **Submodule_Source**: A file owned by a git submodule checkout under `sorc/` (for example `sorc/nexus.fd/`, `sorc/upp.fd/`) that the deployment pipeline copies verbatim into the EXPDIR.
- **Submodule_Fixture**: A documented, deterministic test fixture that provides the Submodule_Source files (or stand-ins) required for the Deployment_Tool to complete a clean deployment during verification.
- **Traceability_Matrix**: A machine-readable, version-controlled artifact that maps every parent requirement and every parent Property to the proving test(s) and their current pass status.
- **Verification_Report**: The machine-readable output (for example a JUnit XML or JSON summary) produced by a Goal_Realization_Gate run that records per-test and per-Property pass/fail status.
- **RAG_EE2_Compliance_Scan**: The authoritative EE2 compliance check performed via the agentcore MCP RAG server's EE2 tooling (`scan_repository_compliance` / `analyze_ee2_compliance` / `extract_code_for_analysis`), backed by NCEP WCOSS EE2 v11 standards with Phase 2 SME-corrected patterns. Distinct from, and authoritative over, the in-repo `dev/workflow/deployment/ee2_scanner.py` heuristic scanner. **Availability constraint:** the agentcore MCP RAG server is reachable ONLY inside this development environment; it is NOT available in CI or any other environment. Therefore the RAG_EE2_Compliance_Scan is a *development-time authoring and verification authority*: it is run by a developer (or a dev-environment hook) to validate created/modified scripts and to produce committed baseline recordings, and it is NEVER invoked live by CI.
- **EE2_Baseline_Recording**: A version-controlled artifact (under `dev/workflow/tests/fixtures/ee2/`) capturing the authoritative RAG_EE2_Compliance_Scan result (per file, per category) for the scripts this feature creates or modifies. It is produced in the development environment and consumed offline by CI and by unit tests so that EE2 authority is reproducible without a live RAG connection.

## Requirements

### Requirement 1: Eliminate UFS Runtime Templating in the Forecast Path

**User Story:** As an NCO implementer, I want the coupled-model forecast path to consume pre-rendered configs from the sealed EXPDIR instead of generating them at run time, so that the EXPDIR is truly sealed and no atparse substitution executes during a production run.

**Traces to parent:** Req 4.6, Req 8, Property 14.

#### Acceptance Criteria

1. THE Global_Workflow SHALL modify `ush/forecast_postdet.sh` so that the WW3 configuration is staged by copying the pre-rendered file from `${EXPDIR}/parm/ufs/wave/` into the run directory using `cpreq`, and SHALL NOT source `parsing_namelists_WW3.sh`.
2. THE Global_Workflow SHALL modify `ush/forecast_postdet.sh` so that the MOM6 configuration is staged by copying the pre-rendered file from `${EXPDIR}/parm/ufs/ocean/` into the run directory using `cpreq`, and SHALL NOT source `parsing_namelists_MOM6.sh`.
3. THE Global_Workflow SHALL modify `ush/forecast_postdet.sh` so that the CICE configuration is staged by copying the pre-rendered file from `${EXPDIR}/parm/ufs/ice/` into the run directory using `cpreq`, and SHALL NOT source `parsing_namelists_CICE.sh`.
4. THE Global_Workflow SHALL modify `ush/forecast_postdet.sh` so that the GOCART configuration is staged by copying the pre-rendered file from `${EXPDIR}/parm/ufs/gocart/` into the run directory using `cpreq`, and SHALL NOT source `parsing_namelists_GOCART.sh`.
5. WHEN the Token_Scan inspects `ush/forecast_postdet.sh`, THE Token_Scan SHALL report zero `source` references to `parsing_namelists_WW3.sh`, `parsing_namelists_MOM6.sh`, `parsing_namelists_CICE.sh`, and `parsing_namelists_GOCART.sh`.
6. WHEN a forecast task executes against a sealed EXPDIR, THE forecast path SHALL read each coupled-model configuration file as a fully-rendered file containing zero `@[...]` atparse tokens.

### Requirement 2: Remove Obsolete Atparse Scripts and Legacy Token Files

**User Story:** As a maintainer, I want the obsolete runtime parsing scripts, the atparse engine, and the legacy token-bearing config files removed once they are unreferenced, so that no developer can reintroduce runtime templating and the repository reflects the sealed-EXPDIR model.

**Traces to parent:** Req 4.6, Req 8, Property 14.

#### Acceptance Criteria

1. WHEN no script in the repository references `ush/atparse.bash`, THE Global_Workflow SHALL delete `ush/atparse.bash`.
2. WHEN no script in the repository sources a `parsing_namelists_*.sh` script that has been superseded by a deploy-time template, THE Global_Workflow SHALL delete that superseded `parsing_namelists_*.sh` script.
3. THE Global_Workflow SHALL delete the legacy token-bearing file `parm/ufs/fv3/diag_table`, which is superseded by the deploy-time template `dev/parm/ufs/fv3/diag_table.j2`.
4. THE Global_Workflow SHALL delete the legacy token-bearing file `parm/ufs/gocart/AERO_HISTORY.rc`, which is superseded by the deploy-time template `dev/parm/ufs/gocart/AERO_HISTORY.rc.j2`.
5. IF a file scheduled for deletion under this requirement is still referenced by any retained script, THEN THE Global_Workflow SHALL retain the file and SHALL emit a verification error identifying the referencing script, so the deletion is blocked until the reference is removed.
6. WHEN the Token_Scan runs over the repository's retained runtime scripts and config files, THE Token_Scan SHALL report zero `@[...]` atparse tokens except in files listed in the Atparse_Exemption_Registry.

### Requirement 3: Resolve or Explicitly Exempt Non-UFS Atparse Usages

**User Story:** As an NCO implementer, I want every remaining non-UFS atparse usage either converted to deploy-time rendering or explicitly recorded as an enforced exemption, so that the "no runtime templating" policy has no silent exceptions.

**Traces to parent:** Req 4.6, Req 8, Property 14.

#### Acceptance Criteria

1. THE Global_Workflow SHALL maintain an Atparse_Exemption_Registry file under version control that lists every file path permitted to retain runtime `@[...]` templating, with a recorded justification for each entry.
2. FOR EACH of the non-UFS atparse usages `parm/prep_sfc/snow2mdl.nml.tmpl` (rendered by `ush/prep_sfc_snow.sh`), `ush/regrid_gsiSfcIncr_to_tile.sh`, and `scripts/exgfs_wave_nawips.sh` (rendering `gempak.parm.tmpl`), THE Global_Workflow SHALL either convert the usage to a deploy-time rendered file consumed via `cpreq` from the EXPDIR, or record the file in the Atparse_Exemption_Registry with a justification.
3. WHEN the Token_Scan encounters a file containing `@[...]` tokens, THE Token_Scan SHALL pass only if that file path is present in the Atparse_Exemption_Registry, and SHALL emit a verification error naming the file otherwise.
4. IF a file path appears in the Atparse_Exemption_Registry but no longer contains `@[...]` tokens, THEN THE Token_Scan SHALL emit a warning identifying the stale exemption entry so it can be removed.
5. THE Atparse_Exemption_Registry SHALL be consumed by the EE2 compliance scan and the Token_Scan as the single authoritative source of permitted atparse exemptions.

### Requirement 4: Complete Rocoto Decommission in setup_workflow.py

**User Story:** As a maintainer, I want the Rocoto subparser and all Rocoto-conditioned branches removed from `dev/workflow/setup_workflow.py`, so that the ecFlow-only policy is fully enforced in code and no Rocoto code path remains reachable.

**Traces to parent:** Req 1.4, Req 14.3.

#### Acceptance Criteria

1. THE Global_Workflow SHALL remove the `rocoto` subparser from `dev/workflow/setup_workflow.py`.
2. THE Global_Workflow SHALL remove every Rocoto-conditioned code branch and `rocoto_xml_factory` reference from `dev/workflow/setup_workflow.py`.
3. WHERE a deprecation guard that raises a Rocoto-decommissioned FATAL ERROR is retained in `dev/workflow/setup_workflow.py`, THE Global_Workflow SHALL allow that guard to remain, and the guard SHALL be the only permitted residual reference to the term "rocoto" in that file.
4. WHEN a static scan counts case-insensitive occurrences of "rocoto" in `dev/workflow/setup_workflow.py`, THE scan SHALL pass only when every residual occurrence matches the documented deprecation-guard structure — a guard that is structurally identifiable by containing multiple "rocoto" references (such as a guard class or function name together with its FATAL-ERROR message) or by matching a documented allowlist pattern — and SHALL fail when a single lone "rocoto" occurrence does not match the documented deprecation-guard structure.
5. IF a developer invokes a decommissioned Rocoto code path through `dev/workflow/setup_workflow.py`, THEN THE Deployment_Tool SHALL emit a FATAL ERROR that references the ecFlow-only policy.

### Requirement 5: Enforce wxflow and uwtools Version Pinning as a Hard Precondition

**User Story:** As an NCO implementer, I want the Deployment_Tool to fatally refuse to run unless the pinned wxflow and uwtools versions are importable and matching, and I want the declared development environment to actually provide those versions, so that the version guard is effective and the full test suite imports and runs.

**Traces to parent:** Req 9.4, Req 9.5.

#### Acceptance Criteria

1. THE Deployment_Tool SHALL verify that `wxflow` and `uwtools` are importable and that their installed versions match the versions pinned in `dev/workflow/requirements.txt` before writing any file into an EXPDIR.
2. IF `wxflow` or `uwtools` is not importable, or its installed version does not match the pinned version, THEN THE Deployment_Tool SHALL emit a FATAL ERROR identifying the package, the expected version, and the found state, and SHALL NOT write any EXPDIR file.
3. THE Verification_Environment SHALL provide the pinned `wxflow` and `uwtools` versions such that importing `wxflow` and `uwtools` succeeds.
4. WHEN the full `dev/workflow` test suite is collected in the Verification_Environment, THE test suite SHALL produce zero import errors, including for `tests/test_configuration.py` and `tests/test_hosts.py`.
5. THE Deployment_Tool SHALL record the resolved `wxflow` and `uwtools` versions in the Manifest of every successfully produced EXPDIR.

### Requirement 6: Produce a Clean, Self-Contained EXPDIR End-to-End with Deterministic Submodule Handling

**User Story:** As a workflow developer, I want the deployment pipeline to complete a clean, self-contained EXPDIR in a verifiable environment without fataling on missing submodule-owned sources, so that the determinism, self-containment, immutability, and platform-isolation Properties can actually be exercised.

**Traces to parent:** Req 3.1, Req 3.8, Property 1, Property 4.

#### Acceptance Criteria

1. THE Global_Workflow SHALL define a deterministic handling of Submodule_Source files such that a verification deployment either requires the relevant submodules to be present and fetched, or supplies them through a documented Submodule_Fixture.
2. WHEN the Deployment_Tool runs in the Verification_Environment with the Submodule_Fixture (or fetched submodules) present, THE Deployment_Tool SHALL complete a full deployment without emitting a "Submodule source not found" FATAL ERROR.
3. WHEN a verification deployment completes, THE produced EXPDIR SHALL be self-contained such that no file under `<EXPDIR>/jobs/`, `<EXPDIR>/scripts/`, or `<EXPDIR>/ush/` references a path under `dev/`.
4. WHEN the Deployment_Tool is run twice in the Verification_Environment against the same git commit, the same input configuration, and the same Platform, THE two produced EXPDIRs SHALL have Manifests listing identical file hashes for every rendered file.
5. WHEN the proving test for Property 1 (`test_deployment_determinism_property`) executes in the Verification_Environment, THE test SHALL pass.
6. WHEN the proving test for Property 4 (`test_integration_self_containment`) executes in the Verification_Environment, THE test SHALL pass.
7. THE Submodule_Fixture, if used, SHALL be documented in the spec or repository such that any developer can reproduce the clean deployment.

### Requirement 7: Single Authoritative Goal Realization Gate

**User Story:** As an NCO implementer, I want one authoritative CI gate that proves all 14 parent Properties pass against a freshly deployed EXPDIR with zero test failures, errors, or collection errors, so that "goal realized" is an objective, machine-verifiable status rather than a claim.

**Traces to parent:** Properties 1–14, and all parent requirements.

#### Acceptance Criteria

1. THE Goal_Realization_Gate SHALL execute the full `dev/workflow` test suite, including the Property_Suite proving parent Properties 1 through 14, against a freshly deployed EXPDIR in the Verification_Environment, and SHALL report a satisfied status only when all 14 parent Properties pass, such that executing the Property_Suite without all 14 parent Properties passing is insufficient to satisfy the gate.
2. WHEN the Goal_Realization_Gate runs, THE Property_Suite SHALL prove all 14 parent Properties — Determinism (1), Manifest Integrity (2), Immutability (3), Self-Containment (4), Atomicity (5), Idempotence (6), Statelessness (7), Platform Isolation (8), Parser Round-Trip (9), Printer Round-Trip (10), ecFlow Round-Trip (11), DAG Acyclicity (12), Definition Fidelity (13), and No Unresolved Tokens (14) — with each corresponding test passing.
3. WHEN the Goal_Realization_Gate runs the full `dev/workflow` test suite, THE run SHALL report zero failed tests, zero errored tests, and zero collection or import errors.
4. THE Goal_Realization_Gate SHALL resolve the currently observed 6 failed tests, 25 errors, and 2 import errors so that none remain.
5. WHEN the Token_Scan executes as part of the Goal_Realization_Gate over a freshly deployed EXPDIR, THE Token_Scan SHALL report zero `{{`, `{%`, or `{#` Jinja2 tokens in any rendered file and zero `@[...]` atparse tokens in any rendered file.
6. THE Goal_Realization_Gate SHALL run in CI and SHALL produce a Verification_Report recording the per-Property and per-test pass status.
7. IF any test in the Goal_Realization_Gate fails, errors, or fails to collect, THEN THE Goal_Realization_Gate SHALL report a non-passing overall status.

### Requirement 8: Reconciliation and Traceability of Completion Claims

**User Story:** As an NCO Senior Production Analyst, I want a traceability matrix that maps every parent requirement and Property to its proving test and current pass status, and I want the parent tasks.md completion claims backed by passing verification, so that no task is marked done without a green proving test.

**Traces to parent:** all parent requirements and Properties 1–14.

#### Acceptance Criteria

1. THE Global_Workflow SHALL maintain a Traceability_Matrix that maps every parent requirement and every parent Property (1–14) to the proving test or tests that verify it.
2. THE Traceability_Matrix SHALL record, for each mapped parent requirement and Property, the current pass status derived from the most recent Goal_Realization_Gate run.
3. WHEN the Goal_Realization_Gate completes, THE Traceability_Matrix pass-status entries SHALL be consistent with the Verification_Report produced by that run.
4. IF a parent requirement or Property has no proving test in the Traceability_Matrix, THEN the reconciliation check SHALL emit a verification error identifying the unmapped parent item.
5. THE Global_Workflow SHALL ensure that every parent `tasks.md` task marked complete is backed by at least one passing proving test recorded in the Traceability_Matrix.
6. IF a parent `tasks.md` task is marked complete but its mapped proving test is not passing, THEN the reconciliation check SHALL emit a verification error identifying the task and its non-passing test.

### Requirement 9: EE2 Compliance Gate on the Rendered EXPDIR

**User Story:** As an NCO implementer, I want the EE2 compliance scan to pass on the rendered EXPDIR across the required categories with zero violations, so that the artifact can be accepted into the production suite without exception requests.

**Traces to parent:** Req 11.6.

#### Acceptance Criteria

1. WHEN the Deployment_Tool runs the EE2 compliance scan over every rendered J-Job, Ex_Script, and ush script in a freshly deployed EXPDIR, THE EE2 compliance scan SHALL report zero violations in the `error_handling` category.
2. WHEN the EE2 compliance scan runs over the rendered EXPDIR, THE scan SHALL report zero violations in the `environment_variables` category.
3. WHEN the EE2 compliance scan runs over the rendered EXPDIR, THE scan SHALL report zero violations in the `file_naming` category.
4. WHEN the EE2 compliance scan runs over the rendered EXPDIR, THE scan SHALL report zero violations in the `shebang_compliance` category.
5. IF the EE2 compliance scan reports any violation in any of the categories `error_handling`, `environment_variables`, `file_naming`, or `shebang_compliance`, THEN THE Deployment_Tool SHALL emit a FATAL ERROR identifying the file and the violated category.
6. THE EE2 compliance gate SHALL execute as part of the Goal_Realization_Gate so that EE2 compliance is verified on every gated run.

### Requirement 10: RAG-Backed EE2 Compliance as an Authoritative Gate

**User Story:** As an NCO implementer, I want every script this feature creates or modifies, together with the rendered EXPDIR scripts, validated by the authoritative agentcore RAG EE2 compliance tooling rather than only the in-repo heuristic scanner, so that compliance is judged against the official NCEP WCOSS EE2 v11 standards with Phase 2 SME-corrected patterns and no false pass or false fail slips through.

**Traces to parent:** Req 11.6 (EE2 compliance scan). Complements this spec's Requirement 9 (EE2 gate on the rendered EXPDIR).

#### Acceptance Criteria

1. WHEN this feature creates or modifies any shell, J-Job, Ex_Script, or ush file (for example the modified `ush/forecast_postdet.sh` and any new scripts), THE developer SHALL run the RAG_EE2_Compliance_Scan in the development environment across the categories `error_handling`, `environment_variables`, `file_naming`, `shebang_compliance`, and `production_utilities`, and the result SHALL report zero violations.
2. WHEN the RAG_EE2_Compliance_Scan evaluates a created or modified script, THE developer SHALL additionally run `extract_code_for_analysis` for the categories `output_file_naming`, `shebang_compliance`, and `env_var_validation`, and any finding SHALL be resolved or recorded with an explicit written justification.
3. WHEN a RAG_EE2_Compliance_Scan is run in the development environment, THE result SHALL be captured as a version-controlled EE2_Baseline_Recording so that the authoritative outcome is reproducible offline by CI and by unit tests without a live RAG connection.
4. WHERE the RAG_EE2_Compliance_Scan result and the in-repo `dev/workflow/deployment/ee2_scanner.py` result disagree for the same file and category, THE RAG_EE2_Compliance_Scan result SHALL be treated as authoritative, and the in-repo `ee2_scanner.py` SHALL be reconciled to produce the same result, so that the offline scanner faithfully reproduces the authoritative judgment.
5. THE cpreq-based config staging added to `ush/forecast_postdet.sh` SHALL conform to the EE2 essential-file pattern by emitting a message beginning with `FATAL ERROR:` and aborting the job when a required pre-rendered file is absent, and by using `cpreq` rather than plain `cp` for the copy.
6. BECAUSE the agentcore MCP RAG server is available only in the development environment and never in CI, THE Goal_Realization_Gate SHALL enforce EE2 compliance offline using the reconciled in-repo `ee2_scanner.py` together with the committed EE2_Baseline_Recording, and SHALL NOT invoke the RAG server at gate time; IF the offline EE2 check reports any unresolved violation or diverges from the committed EE2_Baseline_Recording, THEN THE Goal_Realization_Gate SHALL report a non-passing status.
7. THE feature SHALL rely on the `err_chk`, `err_exit`, and `cpreq` patterns for EE2 error-handling compliance, and SHALL NOT introduce `set -eu` or `set -e` solely to satisfy error handling, per the Phase 2 SME correction.

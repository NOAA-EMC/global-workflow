"""Integration tests for end-to-end pipeline with application naming.

Exercises the full flow: NameResolver + FileStager + EE2Scanner
to verify that application-specific J-Job naming works end-to-end.

Traces to: Requirements 6.1, 6.2, 6.3, 9.1, 9.2, 9.5, 9.6
"""

from __future__ import annotations

import os
import stat
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.ee2_scanner import scan_expdir
from deployment.file_stager import FileStager
from deployment.name_resolver import NameResolver, PrefixRegistry, ResolvedName


# ---------------------------------------------------------------------------
# EE2-compliant J-Job content template
# ---------------------------------------------------------------------------

_JJOB_CONTENT = """\
#!/bin/bash
source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"

export DATA="${DATAROOT}/${RUN}fcst.${PDY:-}${cyc}"
export jobid="${job}.${pid:-$$}"

${HOMEglobal}/scripts/exglobal_forecast.sh
err_chk
"""

_LINK_WORKFLOW_CONTENT = """\
#!/bin/bash
# link_workflow.sh — links executables and fix directories
HOMEglobal="${HOMEglobal:-$(dirname $(readlink -f $0))/..}"
echo "Linking workflow artifacts for HOMEglobal=$HOMEglobal"
"""

_LINK_FIXDIRS_CONTENT = """\
#!/bin/bash
# link_fixdirs.sh — links UFS utils fix directories
echo "Linking fix directories"
"""


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def project_root(tmp_path: Path) -> Path:
    """Create a minimal project structure with J-Job sources and linking scripts."""
    # dev/jobs/ with shared-name source files
    jobs_dir = tmp_path / "dev" / "jobs"
    jobs_dir.mkdir(parents=True)

    # Create 3 shared-name J-Jobs
    (jobs_dir / "JGLOBAL_FORECAST").write_text(_JJOB_CONTENT)
    (jobs_dir / "JGLOBAL_STAGE_IC").write_text(_JJOB_CONTENT)
    (jobs_dir / "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX").write_text(_JJOB_CONTENT)

    # sorc/link_workflow.sh
    sorc_dir = tmp_path / "sorc"
    sorc_dir.mkdir(parents=True)
    link_workflow = sorc_dir / "link_workflow.sh"
    link_workflow.write_text(_LINK_WORKFLOW_CONTENT)
    os.chmod(link_workflow, 0o755)

    # sorc/ufs_utils.fd/fix/link_fixdirs.sh
    fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
    fix_dir.mkdir(parents=True)
    link_fixdirs = fix_dir / "link_fixdirs.sh"
    link_fixdirs.write_text(_LINK_FIXDIRS_CONTENT)
    os.chmod(link_fixdirs, 0o755)

    return tmp_path


@pytest.fixture
def expdir(tmp_path: Path) -> Path:
    """Create an empty EXPDIR destination."""
    exp = tmp_path / "EXPDIR"
    exp.mkdir()
    return exp


# ---------------------------------------------------------------------------
# Integration test: full pipeline with application naming
# ---------------------------------------------------------------------------


class TestEndToEndApplicationNaming:
    """Integration test for the full application-naming pipeline.

    Exercises: NameResolver → FileStager.stage_jjobs_with_rename()
    → FileStager.stage_unconditional_artifacts() → EE2Scanner.

    Traces to: Requirements 6.1, 6.2, 6.3, 9.1, 9.2, 9.5, 9.6
    """

    def test_full_pipeline_produces_application_named_expdir(
        self, project_root: Path, expdir: Path
    ):
        """Run full pipeline with application names and verify EXPDIR contents.

        Flow:
        1. Build resolution_map using NameResolver with application names
        2. Stage J-Jobs with FileStager.stage_jjobs_with_rename()
        3. Stage unconditional artifacts
        4. Run EE2Scanner on the resulting EXPDIR
        5. Assert EXPDIR/jobs/ contains only application-named files (no JGLOBAL_)
        6. Assert EE2 scan passes
        7. Assert sorc/ linking scripts exist with 0755

        Traces to: Requirements 6.1, 6.2, 6.3, 9.1, 9.2, 9.5, 9.6
        """
        # --- Step 1: Resolve application names ---
        registry = PrefixRegistry.default()
        dev_root = project_root / "dev"
        resolver = NameResolver(dev_root, registry)

        application_names = {
            "JGCAFS_FORECAST",
            "JGCAFS_STAGE_IC",
            "JGCDAS_FORECAST",
            "JGCDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        }

        resolution_map = resolver.resolve_all(application_names)

        # Verify resolution correctness
        assert resolution_map["JGCAFS_FORECAST"].source_name == "JGLOBAL_FORECAST"
        assert resolution_map["JGCAFS_STAGE_IC"].source_name == "JGLOBAL_STAGE_IC"
        assert resolution_map["JGCDAS_FORECAST"].source_name == "JGLOBAL_FORECAST"
        assert resolution_map["JGCDAS_AERO_ANALYSIS_GENERATE_BMATRIX"].source_name == (
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"
        )

        # --- Step 2: Stage J-Jobs with rename ---
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        staging_result = stager.stage_jjobs_with_rename(resolution_map)
        assert staging_result.files_copied == 4

        # --- Step 3: Stage unconditional artifacts ---
        artifact_result = stager.stage_unconditional_artifacts()
        assert artifact_result.files_copied == 2

        # --- Step 4: Run EE2 scan on EXPDIR ---
        scan_result = scan_expdir(expdir)

        # The scan should pass. We use EE2-compliant content (shebang + jjob_header
        # + err_chk), so there should be no violations.
        assert scan_result.passed, (
            f"EE2 scan found {len(scan_result.violations)} violation(s):\n"
            + "\n".join(v.format() for v in scan_result.violations)
        )

        # --- Step 5: Assert EXPDIR/jobs/ contains only application-named files ---
        jobs_dir = expdir / "jobs"
        assert jobs_dir.is_dir()

        staged_files = sorted(f.name for f in jobs_dir.iterdir() if f.is_file())
        expected_files = sorted([
            "JGCAFS_FORECAST",
            "JGCAFS_STAGE_IC",
            "JGCDAS_FORECAST",
            "JGCDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        ])
        assert staged_files == expected_files

        # No JGLOBAL_ files in EXPDIR
        for filename in staged_files:
            assert not filename.startswith("JGLOBAL_"), (
                f"Found shared-name file '{filename}' in EXPDIR — "
                f"expected only application-named files"
            )

        # --- Step 6: Verify file content preservation ---
        # JGCAFS_FORECAST should have identical content to dev/jobs/JGLOBAL_FORECAST
        source_content = (dev_root / "jobs" / "JGLOBAL_FORECAST").read_text()
        staged_content = (jobs_dir / "JGCAFS_FORECAST").read_text()
        assert staged_content == source_content, (
            "Content mismatch: staged application-named file differs from source"
        )

        # --- Step 7: Assert sorc/ linking scripts exist with 0755 ---
        link_workflow = expdir / "sorc" / "link_workflow.sh"
        assert link_workflow.exists(), "sorc/link_workflow.sh not found in EXPDIR"
        assert link_workflow.read_text() == _LINK_WORKFLOW_CONTENT
        mode = stat.S_IMODE(link_workflow.stat().st_mode)
        assert mode == 0o755, (
            f"Expected 0755 permissions on link_workflow.sh, got {oct(mode)}"
        )

        link_fixdirs = expdir / "sorc" / "ufs_utils.fd" / "fix" / "link_fixdirs.sh"
        assert link_fixdirs.exists(), (
            "sorc/ufs_utils.fd/fix/link_fixdirs.sh not found in EXPDIR"
        )
        assert link_fixdirs.read_text() == _LINK_FIXDIRS_CONTENT
        mode = stat.S_IMODE(link_fixdirs.stat().st_mode)
        assert mode == 0o755, (
            f"Expected 0755 permissions on link_fixdirs.sh, got {oct(mode)}"
        )

    def test_ee2_scan_passes_on_application_named_expdir(
        self, project_root: Path, expdir: Path
    ):
        """EE2 Scanner validates application-named J-Jobs using the same rules.

        The scanner checks filenames match ^J[A-Z][A-Z0-9_]*$ (which accepts
        both JGLOBAL_FORECAST and JGCAFS_FORECAST), and checks content
        (shebang, jjob_header, error handling) which is filename-independent.

        Traces to: Requirements 6.1, 6.2, 6.3
        """
        # Resolve and stage
        registry = PrefixRegistry.default()
        dev_root = project_root / "dev"
        resolver = NameResolver(dev_root, registry)

        resolution_map = resolver.resolve_all({
            "JGCAFS_FORECAST",
            "JGCDAS_FORECAST",
        })

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        stager.stage_jjobs_with_rename(resolution_map)

        # Scan
        scan_result = scan_expdir(expdir)
        assert scan_result.passed, (
            "EE2 scan should pass on application-named J-Jobs.\n"
            f"Violations: {[v.format() for v in scan_result.violations]}"
        )

    def test_linking_scripts_staged_with_executable_permissions(
        self, project_root: Path, expdir: Path
    ):
        """Unconditional artifacts are staged with 0755 permissions.

        Traces to: Requirements 9.1, 9.2, 9.5, 9.6
        """
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        result = stager.stage_unconditional_artifacts()
        assert result.files_copied == 2

        # Verify link_workflow.sh
        link_workflow = expdir / "sorc" / "link_workflow.sh"
        assert link_workflow.exists()
        mode = stat.S_IMODE(link_workflow.stat().st_mode)
        assert mode == 0o755, f"link_workflow.sh has mode {oct(mode)}, expected 0755"

        # Verify link_fixdirs.sh
        link_fixdirs = expdir / "sorc" / "ufs_utils.fd" / "fix" / "link_fixdirs.sh"
        assert link_fixdirs.exists()
        mode = stat.S_IMODE(link_fixdirs.stat().st_mode)
        assert mode == 0o755, f"link_fixdirs.sh has mode {oct(mode)}, expected 0755"


# ---------------------------------------------------------------------------
# Integration test: backward compatibility with shared-name YAML
# ---------------------------------------------------------------------------


class TestBackwardCompatibilityIntegration:
    """Integration test: shared-name YAML produces EXPDIR without renaming.

    When a Workflow_YAML uses shared names (e.g., JGLOBAL_FORECAST), the
    pipeline should copy files without renaming — the EXPDIR/jobs/ should
    contain the same shared names as in dev/jobs/.

    Traces to: Requirements 8.1, 8.2, 8.3
    """

    def test_shared_name_yaml_no_rename(
        self, project_root: Path, expdir: Path
    ):
        """Run pipeline with shared-name YAML and confirm no rename.

        When jjob: values use shared names that exist directly in dev/jobs/,
        the resolver treats them as pass-through and the stager copies
        without renaming.

        Traces to: Requirements 8.1, 8.2, 8.3
        """
        # --- Resolve using shared names (backward compat mode) ---
        registry = PrefixRegistry.default()
        dev_root = project_root / "dev"
        resolver = NameResolver(dev_root, registry)

        # These names already exist in dev/jobs/ — should be pass-through
        shared_names = {
            "JGLOBAL_FORECAST",
            "JGLOBAL_STAGE_IC",
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        }

        resolution_map = resolver.resolve_all(shared_names)

        # Verify pass-through
        for name in shared_names:
            assert resolution_map[name].is_passthrough is True, (
                f"Shared name '{name}' should be pass-through"
            )
            assert resolution_map[name].source_name == name, (
                f"Shared name '{name}' source should equal itself"
            )

        # --- Stage with the resolution map ---
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        staging_result = stager.stage_jjobs_with_rename(resolution_map)
        assert staging_result.files_copied == 3

        # --- Verify EXPDIR contains the original shared names ---
        jobs_dir = expdir / "jobs"
        staged_files = sorted(f.name for f in jobs_dir.iterdir() if f.is_file())
        expected_files = sorted([
            "JGLOBAL_FORECAST",
            "JGLOBAL_STAGE_IC",
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        ])
        assert staged_files == expected_files

        # Content should be identical to source
        for name in shared_names:
            source_content = (dev_root / "jobs" / name).read_text()
            staged_content = (jobs_dir / name).read_text()
            assert staged_content == source_content, (
                f"Content mismatch for passthrough file '{name}'"
            )

    def test_mixed_mode_yaml(self, project_root: Path, expdir: Path):
        """Mixed YAML with both application and shared names works correctly.

        Traces to: Requirements 8.3
        """
        registry = PrefixRegistry.default()
        dev_root = project_root / "dev"
        resolver = NameResolver(dev_root, registry)

        # Mix of application names and shared names
        all_names = {
            "JGCAFS_FORECAST",           # application name → resolves to JGLOBAL_FORECAST
            "JGLOBAL_STAGE_IC",          # shared name → pass-through
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",  # direct match → pass-through
        }

        resolution_map = resolver.resolve_all(all_names)

        # Verify resolution types
        assert resolution_map["JGCAFS_FORECAST"].is_passthrough is False
        assert resolution_map["JGCAFS_FORECAST"].source_name == "JGLOBAL_FORECAST"
        assert resolution_map["JGLOBAL_STAGE_IC"].is_passthrough is True
        assert resolution_map["JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"].is_passthrough is True

        # Stage
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        staging_result = stager.stage_jjobs_with_rename(resolution_map)
        assert staging_result.files_copied == 3

        # Verify EXPDIR contents
        jobs_dir = expdir / "jobs"
        staged_files = sorted(f.name for f in jobs_dir.iterdir() if f.is_file())
        expected_files = sorted([
            "JGCAFS_FORECAST",
            "JGLOBAL_STAGE_IC",
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        ])
        assert staged_files == expected_files

    def test_ee2_scan_passes_on_shared_named_expdir(
        self, project_root: Path, expdir: Path
    ):
        """EE2 scan also passes when EXPDIR contains shared names.

        Traces to: Requirements 6.1, 6.3
        """
        registry = PrefixRegistry.default()
        dev_root = project_root / "dev"
        resolver = NameResolver(dev_root, registry)

        resolution_map = resolver.resolve_all({
            "JGLOBAL_FORECAST",
            "JGLOBAL_STAGE_IC",
        })

        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )
        stager.stage_jjobs_with_rename(resolution_map)

        scan_result = scan_expdir(expdir)
        assert scan_result.passed, (
            "EE2 scan should pass on shared-name J-Jobs.\n"
            f"Violations: {[v.format() for v in scan_result.violations]}"
        )

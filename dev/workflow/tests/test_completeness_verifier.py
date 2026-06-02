"""Unit tests for the CompletenessVerifier class.

Tests cross-reference integrity verification of a staged EXPDIR:
1. Every J-Job in jobs/ references an ex-script present in scripts/
2. Every ush script sourced by staged ex-scripts exists in ush/
3. FATAL ERROR message format includes missing file and referencing script

Validates: Requirements 8.1, 8.2, 8.3, 8.4
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.completeness_verifier import CompletenessResult, CompletenessVerifier
from deployment.pipeline import PipelineError


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def complete_expdir(tmp_path: Path) -> Path:
    """Create a complete EXPDIR where all cross-references resolve.

    Structure:
        jobs/JGLOBAL_FORECAST  → references exglobal_forecast.sh
        jobs/JGFS_ATMOS_POST   → references exgfs_atmos_post.sh
        scripts/exglobal_forecast.sh → sources forecast_predet.sh
        scripts/exgfs_atmos_post.sh  → sources post_utils.sh
        ush/forecast_predet.sh
        ush/post_utils.sh
        parm/config/gfs/config.base.j2
        parm/config/gfs/config.fcst.j2
    """
    # jobs/
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"\n'
        ': "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"\n'
        '"${FORECASTSH}" && true\n'
    )
    (jobs_dir / "JGFS_ATMOS_POST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "post" -c "base"\n'
        '${SCRglobal}/exgfs_atmos_post.sh\n'
    )

    # scripts/
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    (scripts_dir / "exglobal_forecast.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/forecast_predet.sh"\n'
    )
    (scripts_dir / "exgfs_atmos_post.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/post_utils.sh"\n'
    )

    # ush/
    ush_dir = tmp_path / "ush"
    ush_dir.mkdir()
    (ush_dir / "forecast_predet.sh").write_text("#!/bin/bash\necho predet\n")
    (ush_dir / "post_utils.sh").write_text("#!/bin/bash\necho utils\n")

    # parm/config/
    config_dir = tmp_path / "parm" / "config" / "gfs"
    config_dir.mkdir(parents=True)
    (config_dir / "config.base.j2").write_text("# base\n")
    (config_dir / "config.fcst.j2").write_text("# fcst\n")

    return tmp_path


@pytest.fixture
def expdir_missing_ex_script(tmp_path: Path) -> Path:
    """Create an EXPDIR where a J-Job references a missing ex-script.

    JGLOBAL_FORECAST references exglobal_forecast.sh but it is NOT
    present in scripts/.
    """
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        ': "${FORECASTSH:=${SCRglobal}/exglobal_forecast.sh}"\n'
        '"${FORECASTSH}" && true\n'
    )

    # scripts/ exists but does NOT contain exglobal_forecast.sh
    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()

    # ush/ (empty, no references from scripts)
    ush_dir = tmp_path / "ush"
    ush_dir.mkdir()

    return tmp_path


@pytest.fixture
def expdir_missing_ush_script(tmp_path: Path) -> Path:
    """Create an EXPDIR where an ex-script sources a missing ush script.

    exglobal_forecast.sh sources forecast_predet.sh but it is NOT
    present in ush/.
    """
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        '${SCRglobal}/exglobal_forecast.sh\n'
    )

    scripts_dir = tmp_path / "scripts"
    scripts_dir.mkdir()
    (scripts_dir / "exglobal_forecast.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/forecast_predet.sh"\n'
        '. "${USHglobal}/forecast_det.sh"\n'
    )

    # ush/ exists but does NOT contain the referenced scripts
    ush_dir = tmp_path / "ush"
    ush_dir.mkdir()

    return tmp_path


# ---------------------------------------------------------------------------
# Tests for passing verification with complete EXPDIR
# ---------------------------------------------------------------------------


class TestCompleteExpdir:
    """Tests that verification passes with a complete EXPDIR fixture."""

    def test_verify_returns_result_on_complete_expdir(
        self, complete_expdir: Path
    ):
        """verify() returns CompletenessResult (not raises) when all refs resolve."""
        verifier = CompletenessVerifier(complete_expdir)
        result = verifier.verify()
        assert isinstance(result, CompletenessResult)

    def test_verify_passed_is_true(self, complete_expdir: Path):
        """Result.passed is True when all cross-references are satisfied."""
        verifier = CompletenessVerifier(complete_expdir)
        result = verifier.verify()
        assert result.passed is True

    def test_no_missing_ex_scripts(self, complete_expdir: Path):
        """No missing ex-scripts reported for complete EXPDIR."""
        verifier = CompletenessVerifier(complete_expdir)
        result = verifier.verify()
        assert result.missing_ex_scripts == []

    def test_no_missing_ush_scripts(self, complete_expdir: Path):
        """No missing ush scripts reported for complete EXPDIR."""
        verifier = CompletenessVerifier(complete_expdir)
        result = verifier.verify()
        assert result.missing_ush_scripts == []

    def test_no_missing_configs(self, complete_expdir: Path):
        """No missing configs reported for complete EXPDIR."""
        verifier = CompletenessVerifier(complete_expdir)
        result = verifier.verify()
        assert result.missing_configs == []


# ---------------------------------------------------------------------------
# Tests for detection of missing ex-script referenced by a J-Job
# ---------------------------------------------------------------------------


class TestMissingExScript:
    """Tests that missing ex-scripts are detected and reported."""

    def test_raises_pipeline_error(self, expdir_missing_ex_script: Path):
        """verify() raises PipelineError when ex-script is missing."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError):
            verifier.verify()

    def test_error_stage_is_completeness(self, expdir_missing_ex_script: Path):
        """PipelineError stage is 'completeness'."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert exc_info.value.stage == "completeness"

    def test_error_message_names_missing_script(
        self, expdir_missing_ex_script: Path
    ):
        """Error message includes the name of the missing ex-script."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "exglobal_forecast.sh" in str(exc_info.value)

    def test_error_message_names_referencing_jjob(
        self, expdir_missing_ex_script: Path
    ):
        """Error message includes the J-Job that references the missing script."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "JGLOBAL_FORECAST" in str(exc_info.value)

    def test_check_jjob_ex_script_refs_returns_tuples(
        self, expdir_missing_ex_script: Path
    ):
        """_check_jjob_ex_script_refs returns list of (jjob, script) tuples."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        missing = verifier._check_jjob_ex_script_refs()
        assert len(missing) > 0
        jjob, script = missing[0]
        assert jjob == "JGLOBAL_FORECAST"
        assert script == "exglobal_forecast.sh"


# ---------------------------------------------------------------------------
# Tests for detection of missing ush script sourced by an ex-script
# ---------------------------------------------------------------------------


class TestMissingUshScript:
    """Tests that missing ush scripts are detected and reported."""

    def test_raises_pipeline_error(self, expdir_missing_ush_script: Path):
        """verify() raises PipelineError when ush script is missing."""
        verifier = CompletenessVerifier(expdir_missing_ush_script)
        with pytest.raises(PipelineError):
            verifier.verify()

    def test_error_message_names_missing_ush(
        self, expdir_missing_ush_script: Path
    ):
        """Error message includes the name of the missing ush script."""
        verifier = CompletenessVerifier(expdir_missing_ush_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "forecast_predet.sh" in str(exc_info.value)

    def test_error_message_names_referencing_script(
        self, expdir_missing_ush_script: Path
    ):
        """Error message includes the ex-script that sources the missing ush."""
        verifier = CompletenessVerifier(expdir_missing_ush_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "exglobal_forecast.sh" in str(exc_info.value)

    def test_detects_multiple_missing_ush_scripts(
        self, expdir_missing_ush_script: Path
    ):
        """Detects all missing ush scripts, not just the first one."""
        verifier = CompletenessVerifier(expdir_missing_ush_script)
        missing = verifier._check_ex_script_ush_refs()
        # exglobal_forecast.sh sources both forecast_predet.sh and forecast_det.sh
        missing_names = [ush for _, ush in missing]
        assert "forecast_predet.sh" in missing_names
        assert "forecast_det.sh" in missing_names

    def test_check_ex_script_ush_refs_returns_tuples(
        self, expdir_missing_ush_script: Path
    ):
        """_check_ex_script_ush_refs returns list of (script, ush) tuples."""
        verifier = CompletenessVerifier(expdir_missing_ush_script)
        missing = verifier._check_ex_script_ush_refs()
        assert len(missing) > 0
        ref_script, ush_name = missing[0]
        assert ref_script == "exglobal_forecast.sh"
        assert ush_name in ("forecast_predet.sh", "forecast_det.sh")


# ---------------------------------------------------------------------------
# Tests for FATAL ERROR message format
# ---------------------------------------------------------------------------


class TestFatalErrorFormat:
    """Tests that FATAL ERROR messages include required information.

    Validates Requirement 8.3: THE Deployment_Pipeline SHALL emit a FATAL
    ERROR naming the missing file and the referencing script.
    """

    def test_error_starts_with_fatal_error(
        self, expdir_missing_ex_script: Path
    ):
        """PipelineError string starts with 'FATAL ERROR'."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert str(exc_info.value).startswith("FATAL ERROR")

    def test_error_includes_stage_name(self, expdir_missing_ex_script: Path):
        """Error includes the stage name 'completeness'."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "[completeness]" in str(exc_info.value)

    def test_error_includes_missing_file_path_context(
        self, expdir_missing_ex_script: Path
    ):
        """Error includes path context for the missing file."""
        verifier = CompletenessVerifier(expdir_missing_ex_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        # Should mention the scripts directory where the file was expected
        assert "scripts" in str(exc_info.value)

    def test_ush_error_includes_ush_directory(
        self, expdir_missing_ush_script: Path
    ):
        """Error for missing ush script includes the ush directory path."""
        verifier = CompletenessVerifier(expdir_missing_ush_script)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "ush" in str(exc_info.value)

    def test_multiple_missing_deps_all_reported(self, tmp_path: Path):
        """When multiple dependencies are missing, all are reported in the error."""
        # Create EXPDIR with two J-Jobs referencing missing ex-scripts
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        (jobs_dir / "JGLOBAL_FORECAST").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exglobal_forecast.sh\n'
        )
        (jobs_dir / "JGFS_ATMOS_POST").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exgfs_atmos_post.sh\n'
        )
        scripts_dir = tmp_path / "scripts"
        scripts_dir.mkdir()
        ush_dir = tmp_path / "ush"
        ush_dir.mkdir()

        verifier = CompletenessVerifier(tmp_path)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        error_msg = str(exc_info.value)
        assert "exglobal_forecast.sh" in error_msg
        assert "exgfs_atmos_post.sh" in error_msg


# ---------------------------------------------------------------------------
# Tests for edge cases
# ---------------------------------------------------------------------------


class TestEdgeCases:
    """Tests for edge cases and boundary conditions."""

    def test_empty_expdir_passes(self, tmp_path: Path):
        """An EXPDIR with no jobs/ directory passes (nothing to check)."""
        verifier = CompletenessVerifier(tmp_path)
        result = verifier.verify()
        assert result.passed is True

    def test_empty_jobs_dir_passes(self, tmp_path: Path):
        """An EXPDIR with empty jobs/ directory passes."""
        (tmp_path / "jobs").mkdir()
        (tmp_path / "scripts").mkdir()
        (tmp_path / "ush").mkdir()
        verifier = CompletenessVerifier(tmp_path)
        result = verifier.verify()
        assert result.passed is True

    def test_jjob_with_no_ex_script_ref_passes(self, tmp_path: Path):
        """A J-Job that doesn't reference any ex-script passes verification."""
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        (jobs_dir / "JGLOBAL_CLEANUP").write_text(
            '#!/bin/bash\n'
            'echo "no ex-script reference"\n'
        )
        scripts_dir = tmp_path / "scripts"
        scripts_dir.mkdir()
        ush_dir = tmp_path / "ush"
        ush_dir.mkdir()

        verifier = CompletenessVerifier(tmp_path)
        result = verifier.verify()
        assert result.passed is True

    def test_comment_lines_in_scripts_ignored(self, tmp_path: Path):
        """Source references in comment lines are not checked."""
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        (jobs_dir / "JGLOBAL_FORECAST").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exglobal_forecast.sh\n'
        )
        scripts_dir = tmp_path / "scripts"
        scripts_dir.mkdir()
        (scripts_dir / "exglobal_forecast.sh").write_text(
            '#!/bin/bash\n'
            '# source "${USHglobal}/commented_out.sh"\n'
            'echo "no real ush source"\n'
        )
        ush_dir = tmp_path / "ush"
        ush_dir.mkdir()

        verifier = CompletenessVerifier(tmp_path)
        result = verifier.verify()
        assert result.passed is True

    def test_scripts_dir_missing_still_checks_jobs(self, tmp_path: Path):
        """If scripts/ doesn't exist, J-Job ex-script refs are still flagged."""
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir()
        (jobs_dir / "JGLOBAL_FORECAST").write_text(
            '#!/bin/bash\n'
            '${SCRglobal}/exglobal_forecast.sh\n'
        )
        # No scripts/ directory at all

        verifier = CompletenessVerifier(tmp_path)
        with pytest.raises(PipelineError) as exc_info:
            verifier.verify()
        assert "exglobal_forecast.sh" in str(exc_info.value)

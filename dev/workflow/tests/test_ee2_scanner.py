"""Unit tests for EE2_Compliance_Scanner.

Tests the EE2 compliance scanner with known-good and known-bad scripts
for each category: error_handling, environment_variables, file_naming,
shebang_compliance.

Traces to: Requirements 11.6
"""

import os
import sys
import tempfile

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from pathlib import Path

from deployment.ee2_scanner import (
    ScanResult,
    Violation,
    check_error_handling,
    check_environment_variables,
    check_file_naming,
    check_shebang_compliance,
    run_compliance_scan,
    scan_file,
    scan_expdir,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_file(tmp_path: Path, relpath: str, content: str) -> Path:
    """Create a file at tmp_path/relpath with given content."""
    filepath = tmp_path / relpath
    filepath.parent.mkdir(parents=True, exist_ok=True)
    filepath.write_text(content, encoding="utf-8")
    return filepath


# ---------------------------------------------------------------------------
# Tests: error_handling category
# ---------------------------------------------------------------------------


class TestErrorHandling:
    """Tests for error_handling compliance checks."""

    def test_captures_err_without_err_chk_fails(self, tmp_path):
        """Script that captures err=$? but never calls err_chk should fail."""
        content = """\
#!/bin/bash
set -x

${EXECgfs}/gfs_forecast.x
err=$?

echo "Forecast completed with status $err"
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_forecast.sh", content)
        result = ScanResult()
        check_error_handling(filepath, content, result)

        assert not result.passed
        assert len(result.violations) >= 1
        violation = result.violations[0]
        assert violation.category == "error_handling"
        assert "err_chk" in violation.description or "err_exit" in violation.description

    def test_uses_err_chk_passes(self, tmp_path):
        """Script that properly uses err_chk after capturing exit status should pass."""
        content = """\
#!/bin/bash
set -x

${EXECgfs}/gfs_forecast.x
err=$?
err_chk

echo "Forecast completed successfully"
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_forecast.sh", content)
        result = ScanResult()
        check_error_handling(filepath, content, result)

        assert result.passed

    def test_uses_err_exit_passes(self, tmp_path):
        """Script that uses err_exit for error handling should pass."""
        content = """\
#!/bin/bash
set -x

${EXECgfs}/gfs_post.x
err=$?
if [ $err -ne 0 ]; then
    err_exit "Post processing failed"
fi
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_post.sh", content)
        result = ScanResult()
        check_error_handling(filepath, content, result)

        assert result.passed

    def test_no_executables_no_violation(self, tmp_path):
        """Script with no executable invocations should not trigger violation."""
        content = """\
#!/bin/bash
set -x

export DATA=${DATAROOT}/${jobid}
mkdir -p $DATA
cd $DATA
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_setup.sh", content)
        result = ScanResult()
        check_error_handling(filepath, content, result)

        assert result.passed

    def test_python_script_skipped(self, tmp_path):
        """Python scripts should not be checked for shell error handling."""
        content = """\
#!/usr/bin/env python3
import sys
err = subprocess.run(["gfs_forecast.x"]).returncode
# No err_chk needed in Python
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_forecast.py", content)
        result = ScanResult()
        check_error_handling(filepath, content, result)

        assert result.passed


# ---------------------------------------------------------------------------
# Tests: environment_variables category
# ---------------------------------------------------------------------------


class TestEnvironmentVariables:
    """Tests for environment_variables compliance checks."""

    def test_jjob_missing_required_vars_fails(self, tmp_path):
        """J-Job that doesn't set required EE2 vars should fail."""
        content = """\
#!/bin/bash
set -x

# Missing all required EE2 environment variables
echo "Running job"
${HOMEgfs}/scripts/exgfs_forecast.sh
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = ScanResult()
        check_environment_variables(filepath, content, result)

        assert not result.passed
        assert len(result.violations) == 1
        violation = result.violations[0]
        assert violation.category == "environment_variables"
        assert "DATA" in violation.description
        assert "PDY" in violation.description

    def test_jjob_sources_jjob_header_passes(self, tmp_path):
        """J-Job that sources jjob_header.sh should pass (header sets vars)."""
        content = """\
#!/bin/bash
set -x

. ${HOMEgfs}/ush/jjob_header.sh -e "forecast" -c "base forecast"

echo "Running forecast"
${HOMEgfs}/scripts/exgfs_forecast.sh
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = ScanResult()
        check_environment_variables(filepath, content, result)

        assert result.passed

    def test_jjob_sources_standard_vars_passes(self, tmp_path):
        """J-Job that sources jjob_standard_vars.sh should pass."""
        content = """\
#!/bin/bash
set -x

. ${HOMEgfs}/ush/jjob_standard_vars.sh

echo "Running analysis"
${HOMEgfs}/scripts/exgfs_analysis.sh
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_ANALYSIS", content)
        result = ScanResult()
        check_environment_variables(filepath, content, result)

        assert result.passed

    def test_jjob_sets_all_vars_explicitly_passes(self, tmp_path):
        """J-Job that explicitly sets all required vars should pass."""
        content = """\
#!/bin/bash
set -x

export DATA=${DATAROOT}/${jobid}
export cycle=t${cyc}z
export PDY=${PDY:-$(date +%Y%m%d)}
export NET=gfs
export RUN=gfs
export COMIN=${COMROOT}/${NET}/${model_ver}/${RUN}.${PDY}/${cyc}/atmos
export COMOUT=${COMROOT}/${NET}/${model_ver}/${RUN}.${PDY}/${cyc}/atmos
export pgmout=OUTPUT.$$
export jobid=${jobid:-$$}

${HOMEgfs}/scripts/exgfs_forecast.sh
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = ScanResult()
        check_environment_variables(filepath, content, result)

        assert result.passed

    def test_non_jjob_not_checked(self, tmp_path):
        """Non-J-Job files (ex-scripts) should not be checked for env vars."""
        content = """\
#!/bin/bash
set -x
# This is an ex-script, not a J-Job
echo "Running"
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_forecast.sh", content)
        result = ScanResult()
        check_environment_variables(filepath, content, result)

        assert result.passed


# ---------------------------------------------------------------------------
# Tests: file_naming category
# ---------------------------------------------------------------------------


class TestFileNaming:
    """Tests for file_naming compliance checks."""

    def test_jjob_lowercase_name_fails(self, tmp_path):
        """J-Job with lowercase name violates JAAAAA pattern."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "jobs/jgfs_forecast", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert not result.passed
        assert len(result.violations) == 1
        violation = result.violations[0]
        assert violation.category == "file_naming"
        assert "JAAAAA" in violation.description

    def test_jjob_with_extension_fails(self, tmp_path):
        """J-Job with file extension violates JAAAAA pattern."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST.sh", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert not result.passed
        assert len(result.violations) == 1
        violation = result.violations[0]
        assert violation.category == "file_naming"
        assert "JAAAAA" in violation.description

    def test_jjob_valid_name_passes(self, tmp_path):
        """J-Job with valid JAAAAA name should pass."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "jobs/JGFS_ATMOS_FORECAST", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert result.passed

    def test_jjob_valid_name_with_digits_passes(self, tmp_path):
        """J-Job with digits in name should pass."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "jobs/JGFS_ATMOS_POST_F006", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert result.passed

    def test_exscript_invalid_name_fails(self, tmp_path):
        """Ex-script with invalid name violates exaaaaa.sh pattern."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "scripts/GFS_FORECAST.sh", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert not result.passed
        assert len(result.violations) == 1
        violation = result.violations[0]
        assert violation.category == "file_naming"
        assert "exaaaaa" in violation.description

    def test_exscript_no_extension_fails(self, tmp_path):
        """Ex-script without extension violates exaaaaa.sh pattern."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "scripts/exgfs_forecast", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert not result.passed
        violation = result.violations[0]
        assert violation.category == "file_naming"

    def test_exscript_valid_name_passes(self, tmp_path):
        """Ex-script with valid exaaaaa.sh name should pass."""
        content = "#!/bin/bash\necho hello\n"
        filepath = _make_file(tmp_path, "scripts/exgfs_atmos_forecast.sh", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert result.passed

    def test_exscript_python_extension_passes(self, tmp_path):
        """Ex-script with .py extension should pass."""
        content = "#!/usr/bin/env python3\nprint('hello')\n"
        filepath = _make_file(tmp_path, "scripts/exgfs_analysis.py", content)
        result = ScanResult()
        check_file_naming(filepath, content, result)

        assert result.passed


# ---------------------------------------------------------------------------
# Tests: shebang_compliance category
# ---------------------------------------------------------------------------


class TestShebangCompliance:
    """Tests for shebang_compliance checks."""

    def test_missing_shebang_fails(self, tmp_path):
        """Script without shebang line should fail."""
        content = """\
# This is a comment, not a shebang
set -x
echo "hello"
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert not result.passed
        assert len(result.violations) == 1
        violation = result.violations[0]
        assert violation.category == "shebang_compliance"
        assert "Missing shebang" in violation.description

    def test_invalid_shebang_fails(self, tmp_path):
        """Script with invalid shebang should fail."""
        content = """\
#!/usr/bin/perl
use strict;
print "hello\\n";
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_process.sh", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert not result.passed
        assert len(result.violations) == 1
        violation = result.violations[0]
        assert violation.category == "shebang_compliance"
        assert "Invalid shebang" in violation.description

    def test_valid_bash_shebang_passes(self, tmp_path):
        """Script with #!/bin/bash should pass."""
        content = """\
#!/bin/bash
set -x
echo "hello"
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert result.passed

    def test_valid_env_python3_shebang_passes(self, tmp_path):
        """Script with #!/usr/bin/env python3 should pass."""
        content = """\
#!/usr/bin/env python3
import sys
print("hello")
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_analysis.py", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert result.passed

    def test_valid_env_bash_shebang_passes(self, tmp_path):
        """Script with #!/usr/bin/env bash should pass."""
        content = """\
#!/usr/bin/env bash
set -x
echo "hello"
"""
        filepath = _make_file(tmp_path, "scripts/exgfs_post.sh", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert result.passed

    def test_empty_file_fails(self, tmp_path):
        """Empty file should fail shebang check."""
        content = ""
        filepath = _make_file(tmp_path, "jobs/JGFS_EMPTY", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert not result.passed
        violation = result.violations[0]
        assert violation.category == "shebang_compliance"
        assert "empty" in violation.description.lower()

    def test_shebang_with_space_normalized(self, tmp_path):
        """Shebang with space after #! should be normalized and pass."""
        content = """\
#! /bin/bash
set -x
echo "hello"
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = ScanResult()
        check_shebang_compliance(filepath, content, result)

        assert result.passed


# ---------------------------------------------------------------------------
# Tests: scan_file integration
# ---------------------------------------------------------------------------


class TestScanFile:
    """Tests for the scan_file function that runs all checks."""

    def test_scan_file_all_categories(self, tmp_path):
        """scan_file runs all categories by default."""
        content = """\
#!/bin/bash
set -x

${EXECgfs}/gfs_forecast.x
err=$?
err_chk
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = scan_file(filepath)

        # Should check env vars (J-Job without jjob_header.sh)
        env_violations = [
            v for v in result.violations if v.category == "environment_variables"
        ]
        assert len(env_violations) > 0

    def test_scan_file_specific_category(self, tmp_path):
        """scan_file can be limited to specific categories."""
        content = """\
#!/bin/bash
set -x
echo "hello"
"""
        filepath = _make_file(tmp_path, "jobs/JGFS_FORECAST", content)
        result = scan_file(filepath, categories=["shebang_compliance"])

        # Only shebang should be checked - and it passes
        assert result.passed

    def test_scan_file_unreadable(self, tmp_path):
        """scan_file handles unreadable files gracefully."""
        filepath = tmp_path / "nonexistent" / "file.sh"
        result = scan_file(filepath)

        assert not result.passed
        assert result.violations[0].description == "Unable to read file for compliance scanning"


# ---------------------------------------------------------------------------
# Tests: scan_expdir integration
# ---------------------------------------------------------------------------


class TestScanExpdir:
    """Tests for the scan_expdir function that scans an entire EXPDIR."""

    def test_scan_expdir_finds_violations(self, tmp_path):
        """scan_expdir finds violations across multiple files."""
        # Create a J-Job with bad naming
        _make_file(tmp_path, "jobs/jgfs_bad_name", "#!/bin/bash\necho hi\n")
        # Create a valid J-Job
        _make_file(
            tmp_path,
            "jobs/JGFS_GOOD",
            "#!/bin/bash\n. jjob_header.sh\necho hi\n",
        )

        result = scan_expdir(tmp_path)

        naming_violations = [
            v for v in result.violations if v.category == "file_naming"
        ]
        assert len(naming_violations) >= 1

    def test_scan_expdir_nonexistent_raises(self):
        """scan_expdir raises FileNotFoundError for missing directory."""
        with pytest.raises(FileNotFoundError):
            scan_expdir(Path("/nonexistent/expdir"))

    def test_scan_expdir_empty_passes(self, tmp_path):
        """scan_expdir on empty directory passes (no files to check)."""
        result = scan_expdir(tmp_path)
        assert result.passed


# ---------------------------------------------------------------------------
# Tests: Violation formatting
# ---------------------------------------------------------------------------


class TestViolationFormat:
    """Tests for violation message formatting."""

    def test_format_includes_category_and_file(self):
        """Violation format includes FATAL ERROR, category, file, and description."""
        v = Violation(
            category="error_handling",
            file="jobs/JGFS_FORECAST",
            description="Missing err_chk",
        )
        formatted = v.format()
        assert "FATAL ERROR" in formatted
        assert "error_handling" in formatted
        assert "jobs/JGFS_FORECAST" in formatted
        assert "Missing err_chk" in formatted

    def test_format_uses_ee2_violation_prefix(self):
        """Violation format uses the standard 'EE2 violation' prefix."""
        v = Violation(
            category="shebang_compliance",
            file="scripts/exgfs_post.sh",
            description="Invalid shebang",
        )
        formatted = v.format()
        assert "FATAL ERROR: EE2 violation [shebang_compliance]:" in formatted


# ---------------------------------------------------------------------------
# Tests: run_compliance_scan (raises SystemExit on violations)
# ---------------------------------------------------------------------------


class TestRunComplianceScan:
    """Tests for run_compliance_scan which raises SystemExit on violations."""

    def test_raises_system_exit_on_violations(self, tmp_path):
        """run_compliance_scan raises SystemExit when violations are found."""
        # Create a J-Job with bad naming to trigger a violation
        _make_file(tmp_path, "jobs/jgfs_bad_name", "#!/bin/bash\necho hi\n")

        with pytest.raises(SystemExit) as exc_info:
            run_compliance_scan(tmp_path)

        # SystemExit message should contain violation count and FATAL ERROR
        exit_msg = str(exc_info.value)
        assert "FAILED" in exit_msg
        assert "violation" in exit_msg

    def test_no_exit_when_clean(self, tmp_path):
        """run_compliance_scan does not raise when no violations found."""
        # Create a compliant J-Job
        content = """\
#!/bin/bash
set -x
. ${HOMEgfs}/ush/jjob_header.sh -e "forecast" -c "base forecast"
${HOMEgfs}/scripts/exgfs_forecast.sh
"""
        _make_file(tmp_path, "jobs/JGFS_FORECAST", content)

        # Should not raise
        run_compliance_scan(tmp_path)

    def test_exit_message_contains_fatal_error_format(self, tmp_path):
        """SystemExit message contains FATAL ERROR lines for each violation."""
        # Create files with multiple violations
        _make_file(tmp_path, "jobs/jgfs_bad", "#!/bin/bash\necho hi\n")
        _make_file(tmp_path, "scripts/BAD_SCRIPT.sh", "#!/bin/bash\necho hi\n")

        with pytest.raises(SystemExit) as exc_info:
            run_compliance_scan(tmp_path)

        exit_msg = str(exc_info.value)
        assert "FATAL ERROR" in exit_msg

    def test_raises_file_not_found_for_missing_expdir(self):
        """run_compliance_scan raises FileNotFoundError for missing directory."""
        with pytest.raises(FileNotFoundError):
            run_compliance_scan(Path("/nonexistent/expdir"))

    def test_specific_categories_only(self, tmp_path):
        """run_compliance_scan can be limited to specific categories."""
        # Create a J-Job with bad naming but valid shebang
        _make_file(tmp_path, "jobs/jgfs_bad_name", "#!/bin/bash\necho hi\n")

        # Only check shebang_compliance — should pass since shebang is valid
        run_compliance_scan(tmp_path, categories=["shebang_compliance"])


# ---------------------------------------------------------------------------
# Tests: scan_expdir with mock EXPDIR structure
# ---------------------------------------------------------------------------


class TestScanExpdirStructure:
    """Tests for scan_expdir with a realistic EXPDIR directory structure."""

    def test_scans_jobs_scripts_ush_directories(self, tmp_path):
        """scan_expdir scans jobs/, scripts/, and ush/ subdirectories."""
        # Create a compliant EXPDIR structure
        _make_file(
            tmp_path,
            "jobs/JGFS_FORECAST",
            "#!/bin/bash\n. jjob_header.sh\necho hi\n",
        )
        _make_file(
            tmp_path,
            "scripts/exgfs_forecast.sh",
            "#!/bin/bash\nset -x\necho hi\n",
        )
        _make_file(
            tmp_path,
            "ush/helper_util.sh",
            "#!/bin/bash\necho utility\n",
        )

        result = scan_expdir(tmp_path)
        # All files are compliant
        assert result.passed

    def test_detects_violations_across_directories(self, tmp_path):
        """scan_expdir detects violations in jobs/, scripts/, and ush/."""
        # Bad J-Job naming
        _make_file(tmp_path, "jobs/jgfs_bad", "#!/bin/bash\necho hi\n")
        # Bad ex-script naming
        _make_file(tmp_path, "scripts/BAD_SCRIPT.sh", "#!/bin/bash\necho hi\n")
        # Bad shebang in ush
        _make_file(tmp_path, "ush/helper.sh", "#!/usr/bin/perl\necho hi\n")

        result = scan_expdir(tmp_path)
        assert not result.passed

        categories_found = {v.category for v in result.violations}
        assert "file_naming" in categories_found
        assert "shebang_compliance" in categories_found

    def test_skips_hidden_files(self, tmp_path):
        """scan_expdir skips hidden files (starting with .)."""
        _make_file(tmp_path, "jobs/.hidden_file", "no shebang here\n")
        _make_file(
            tmp_path,
            "jobs/JGFS_GOOD",
            "#!/bin/bash\n. jjob_header.sh\necho hi\n",
        )

        result = scan_expdir(tmp_path)
        # Hidden file should be skipped, only JGFS_GOOD checked
        hidden_violations = [
            v for v in result.violations if ".hidden" in v.file
        ]
        assert len(hidden_violations) == 0

    def test_skips_binary_files(self, tmp_path):
        """scan_expdir skips binary/data files (.nc, .grb, etc.)."""
        _make_file(tmp_path, "scripts/data.nc", "binary content\n")
        _make_file(
            tmp_path,
            "scripts/exgfs_post.sh",
            "#!/bin/bash\nset -x\necho hi\n",
        )

        result = scan_expdir(tmp_path)
        nc_violations = [v for v in result.violations if "data.nc" in v.file]
        assert len(nc_violations) == 0

    def test_missing_subdirectories_handled(self, tmp_path):
        """scan_expdir handles missing jobs/, scripts/, or ush/ gracefully."""
        # Only create jobs/ — scripts/ and ush/ don't exist
        _make_file(
            tmp_path,
            "jobs/JGFS_FORECAST",
            "#!/bin/bash\n. jjob_header.sh\necho hi\n",
        )

        # Should not raise, just scan what exists
        result = scan_expdir(tmp_path)
        assert result.passed

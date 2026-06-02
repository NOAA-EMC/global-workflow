"""
Tests for backward-compatibility shims (Task 11.2).

Verifies that jjob_header.sh, jjob_standard_vars.sh, and jjob_shell_setup.sh
in dev/ush/ function as thin shims that fall back to the original implementation
when universal_wrapper.sh is not present, ensuring existing J-Jobs continue to
work without per-job edits.

Validates: Requirements 6.9
"""

import os
import subprocess
import stat
import pytest

# Path to the project root
PROJECT_ROOT = os.path.abspath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..")
)
DEV_USH = os.path.join(PROJECT_ROOT, "dev", "ush")


class TestShimFilesExist:
    """Verify all three shim files exist in dev/ush/."""

    def test_jjob_header_exists(self):
        """jjob_header.sh shim exists in dev/ush/."""
        path = os.path.join(DEV_USH, "jjob_header.sh")
        assert os.path.isfile(path), f"Missing: {path}"

    def test_jjob_standard_vars_exists(self):
        """jjob_standard_vars.sh shim exists in dev/ush/."""
        path = os.path.join(DEV_USH, "jjob_standard_vars.sh")
        assert os.path.isfile(path), f"Missing: {path}"

    def test_jjob_shell_setup_exists(self):
        """jjob_shell_setup.sh shim exists in dev/ush/."""
        path = os.path.join(DEV_USH, "jjob_shell_setup.sh")
        assert os.path.isfile(path), f"Missing: {path}"


class TestShimSyntax:
    """Verify all shim files have valid bash syntax."""

    @pytest.mark.parametrize("script", [
        "jjob_header.sh",
        "jjob_standard_vars.sh",
        "jjob_shell_setup.sh",
    ])
    def test_bash_syntax_valid(self, script):
        """Each shim passes bash -n syntax check."""
        path = os.path.join(DEV_USH, script)
        result = subprocess.run(
            ["bash", "-n", path],
            capture_output=True, text=True
        )
        assert result.returncode == 0, (
            f"Syntax error in {script}: {result.stderr}"
        )


class TestShimDelegation:
    """Verify shims check for universal_wrapper.sh and fall back correctly."""

    @pytest.mark.parametrize("script", [
        "jjob_header.sh",
        "jjob_standard_vars.sh",
        "jjob_shell_setup.sh",
    ])
    def test_shim_references_universal_wrapper(self, script):
        """Each shim references universal_wrapper.sh for delegation."""
        path = os.path.join(DEV_USH, script)
        with open(path, "r") as f:
            content = f.read()
        assert "universal_wrapper.sh" in content, (
            f"{script} does not reference universal_wrapper.sh"
        )

    @pytest.mark.parametrize("script", [
        "jjob_header.sh",
        "jjob_standard_vars.sh",
        "jjob_shell_setup.sh",
    ])
    def test_shim_has_fallback_path(self, script):
        """Each shim has a fallback path when universal_wrapper.sh is absent."""
        path = os.path.join(DEV_USH, script)
        with open(path, "r") as f:
            content = f.read()
        # The fallback is indicated by an else branch
        assert "else" in content, (
            f"{script} does not have a fallback (else) branch"
        )


class TestStandardVarsFallback:
    """Test jjob_standard_vars.sh fallback sets expected variables."""

    def test_standard_vars_sets_expected_exports(self, tmp_path):
        """When universal_wrapper.sh is absent, standard vars are set."""
        # Create a minimal environment where the shim can run in fallback mode
        # We point HOMEglobal to a temp dir without universal_wrapper.sh
        fake_home = tmp_path / "home"
        fake_ush = fake_home / "ush"
        fake_ush.mkdir(parents=True)

        # Create a test script that sources the shim and prints variables
        test_script = tmp_path / "test_vars.sh"
        test_script.write_text(f"""#!/bin/bash
export HOMEglobal="{fake_home}"
export DATAROOT="{tmp_path}/data"
export jobid="test_job_123"
export cyc="06"
source "{DEV_USH}/jjob_standard_vars.sh"
echo "USHglobal=${{USHglobal}}"
echo "FIXglobal=${{FIXglobal}}"
echo "PARMglobal=${{PARMglobal}}"
echo "SCRIPTSglobal=${{SCRIPTSglobal}}"
echo "pgmout=${{pgmout}}"
echo "DATA=${{DATA}}"
echo "cycle=${{cycle}}"
echo "KEEPDATA=${{KEEPDATA}}"
echo "envir=${{envir}}"
""")
        test_script.chmod(test_script.stat().st_mode | stat.S_IEXEC)

        result = subprocess.run(
            ["bash", str(test_script)],
            capture_output=True, text=True,
            env={**os.environ, "PATH": os.environ.get("PATH", "/usr/bin:/bin")}
        )

        assert result.returncode == 0, f"Script failed: {result.stderr}"
        output = result.stdout

        # Verify expected variables are set
        assert f"USHglobal={fake_home}/ush" in output
        assert f"FIXglobal={fake_home}/fix" in output
        assert f"PARMglobal={fake_home}/parm" in output
        assert f"SCRIPTSglobal={fake_home}/scripts" in output
        assert "pgmout=OUTPUT." in output
        assert f"DATA={tmp_path}/data/test_job_123" in output
        assert "cycle=t06z" in output
        assert "KEEPDATA=NO" in output
        assert "envir=prod" in output


class TestHeaderFallback:
    """Test jjob_header.sh fallback handles -e and -c options."""

    def test_header_requires_env_job(self, tmp_path):
        """When -e is not provided, the shim emits an error."""
        # Create a minimal environment
        fake_home = tmp_path / "home"
        fake_ush = fake_home / "ush"
        fake_ush.mkdir(parents=True)

        # Create a minimal err_exit.sh
        err_exit = fake_ush / "err_exit.sh"
        err_exit.write_text("""#!/bin/bash
err_exit() {
    echo "FATAL ERROR: $1" >&2
    exit 1
}
""")

        test_script = tmp_path / "test_header.sh"
        test_script.write_text(f"""#!/bin/bash
export HOMEglobal="{fake_home}"
export DATAROOT="{tmp_path}/data"
export jobid="test_job_123"
export PDY="20250115"
export cyc="06"
export machine="hera"
source "{DEV_USH}/jjob_header.sh"
""")
        test_script.chmod(test_script.stat().st_mode | stat.S_IEXEC)

        result = subprocess.run(
            ["bash", str(test_script)],
            capture_output=True, text=True,
            env={**os.environ, "PATH": os.environ.get("PATH", "/usr/bin:/bin")}
        )

        # Should fail because -e was not provided
        assert result.returncode != 0
        assert "Must specify a job name with -e" in result.stderr

    def test_header_sources_config_and_env(self, tmp_path):
        """When -e and -c are provided, configs and env are sourced."""
        fake_home = tmp_path / "home"
        fake_ush = fake_home / "ush"
        fake_ush.mkdir(parents=True)
        fake_env = fake_home / "env"
        fake_env.mkdir(parents=True)
        fake_parm = fake_home / "dev" / "parm" / "config"
        fake_parm.mkdir(parents=True)

        # Create a minimal err_exit.sh
        err_exit = fake_ush / "err_exit.sh"
        err_exit.write_text("""#!/bin/bash
err_exit() {
    echo "FATAL ERROR: $1" >&2
    exit 1
}
""")

        # Create a config file
        config_base = fake_parm / "config.base"
        config_base.write_text('export MY_CONFIG_VAR="loaded_from_config"\n')

        # Create a machine env file
        machine_env = fake_env / "hera.env"
        machine_env.write_text('export MY_ENV_VAR="loaded_from_env"\n')

        test_script = tmp_path / "test_header_full.sh"
        test_script.write_text(f"""#!/bin/bash
export HOMEglobal="{fake_home}"
export DATAROOT="{tmp_path}/data"
export jobid="test_job_123"
export PDY="20250115"
export cyc="06"
export machine="hera"
source "{DEV_USH}/jjob_header.sh" -e "fcst" -c "base"
echo "MY_CONFIG_VAR=${{MY_CONFIG_VAR}}"
echo "MY_ENV_VAR=${{MY_ENV_VAR}}"
""")
        test_script.chmod(test_script.stat().st_mode | stat.S_IEXEC)

        result = subprocess.run(
            ["bash", str(test_script)],
            capture_output=True, text=True,
            env={**os.environ, "PATH": os.environ.get("PATH", "/usr/bin:/bin")}
        )

        assert result.returncode == 0, f"Script failed: {result.stderr}"
        assert "MY_CONFIG_VAR=loaded_from_config" in result.stdout
        assert "MY_ENV_VAR=loaded_from_env" in result.stdout


class TestShimRequirementTraceability:
    """Verify shims document their requirement traceability."""

    @pytest.mark.parametrize("script", [
        "jjob_header.sh",
        "jjob_standard_vars.sh",
        "jjob_shell_setup.sh",
    ])
    def test_shim_references_requirement_6_9(self, script):
        """Each shim references Requirement 6.9 in its header."""
        path = os.path.join(DEV_USH, script)
        with open(path, "r") as f:
            content = f.read()
        assert "6.9" in content, (
            f"{script} does not reference Requirement 6.9"
        )

"""Property-based test: Platform Isolation (Property 8).

Deploys the same Workflow_Configuration (gfs_forecast_only.yaml) for two
different platforms (HERA and WCOSS2), compares the resulting EXPDIR file
trees, and asserts that differences exist ONLY in platform-conditioned paths:
  - env/ (platform-specific env files)
  - parm/config/<app>/config.resources.* (platform-specific resources)
  - modulefiles/ (platform-specific modules)
  - .ecf scheduler directives (PBS vs Slurm)

J-Jobs, ex-scripts, and ush utilities MUST be IDENTICAL across platforms.

**Validates: Requirements 12.3**

Traces to: Design Document - Correctness Property 8
  "EXPDIRs deployed for two different platforms differ only in env/,
   parm/config/<app>/config.resources.*, modulefiles/, and .ecf
   scheduler directives."
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import pytest
import yaml
from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import run, PipelineError, SubmodulePolicy


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Committed Submodule_Fixture tree (Req 6.2, 6.7). Resolved relative to this
# test file so it works regardless of the current working directory.
FIXTURE_ROOT = (Path(__file__).resolve().parent / "fixtures" / "submodules")

# Platform pairs to test: each pair should produce identical non-platform files
PLATFORM_PAIRS = [
    ("HERA", "WCOSS2"),
    ("HERA", "ORION"),
    ("WCOSS2", "HERCULES"),
]

# Paths that are ALLOWED to differ between platforms
PLATFORM_CONDITIONED_PREFIXES = (
    "env/",
    "modulefiles/",
    "workflow/provenance.yaml",
    "workflow/state.db",
    "manifest.yaml",
)

# File patterns that are allowed to differ (checked by full relative path)
PLATFORM_CONDITIONED_PATTERNS = (
    "config.resources.",
    ".ecf",
)


def _is_platform_conditioned(rel_path: str, platform_a: str, platform_b: str) -> bool:
    """Return True if a file path is expected to differ between platforms.

    Platform-conditioned paths include:
      - env/ directory (platform-specific env files)
      - parm/config/<app>/config.resources.${PLATFORM}
      - modulefiles/ directory (platform-specific modules)
      - .ecf scripts (contain platform-specific scheduler directives)
      - manifest.yaml (contains deployment metadata including platform)
      - workflow/provenance.yaml (contains platform in metadata)
      - workflow/state.db (empty placeholder, may differ in timestamp)
    """
    # Check prefix-based patterns
    for prefix in PLATFORM_CONDITIONED_PREFIXES:
        if rel_path.startswith(prefix):
            return True

    # Check if it's a platform-specific resource file
    if "config.resources." in rel_path:
        # config.resources.HERA, config.resources.WCOSS2, etc.
        basename = os.path.basename(rel_path)
        if basename.startswith("config.resources."):
            suffix = basename.replace("config.resources.", "")
            if suffix.upper() in (platform_a.upper(), platform_b.upper()):
                return True

    # Check if it's an .ecf file (scheduler directives differ)
    if rel_path.endswith(".ecf"):
        return True

    return False


def _create_minimal_dev_tree(base_path: Path) -> dict:
    """Create a minimal dev/ tree with platform-specific files for testing.

    Creates a project structure that includes:
      - A workflow config (gfs_forecast_only.yaml)
      - Platform env files (HERA.env, WCOSS2.env, ORION.env, HERCULES.env)
      - Platform resource files (config.resources.HERA, config.resources.WCOSS2, etc.)
      - Platform modulefiles (gw_run.hera.lua, gw_run.wcoss2.lua, etc.)
      - J-Jobs, ex-scripts, and ush utilities (platform-independent)
      - ecFlow templates

    Returns a dict with 'project_root', 'dev_root', 'config_path' keys.
    """
    project_root = base_path / "global-workflow"
    project_root.mkdir(parents=True, exist_ok=True)

    dev_root = project_root / "dev"
    dev_root.mkdir(exist_ok=True)

    # Create .git directory to mark repo root
    (project_root / ".git").mkdir(exist_ok=True)

    # --- Platform env files ---
    env_dir = project_root / "env"
    env_dir.mkdir(exist_ok=True)
    (env_dir / "HERA.env").write_text(
        "#!/bin/bash\n# HERA environment\nexport MACHINE=HERA\n"
        "export SCHEDULER=slurm\nexport PARTITION=hera\n"
    )
    (env_dir / "WCOSS2.env").write_text(
        "#!/bin/bash\n# WCOSS2 environment\nexport MACHINE=WCOSS2\n"
        "export SCHEDULER=pbs\nexport QUEUE=dev\n"
    )
    (env_dir / "ORION.env").write_text(
        "#!/bin/bash\n# ORION environment\nexport MACHINE=ORION\n"
        "export SCHEDULER=slurm\nexport PARTITION=orion\n"
    )
    (env_dir / "HERCULES.env").write_text(
        "#!/bin/bash\n# HERCULES environment\nexport MACHINE=HERCULES\n"
        "export SCHEDULER=slurm\nexport PARTITION=hercules\n"
    )

    # --- J-Jobs (platform-independent) ---
    # J-Jobs source jjob_header.sh which sets all required EE2 variables
    # (DATA, cycle, PDY, NET, RUN, COMIN, COMOUT, pgmout, jobid)
    jobs_dir = dev_root / "jobs"
    jobs_dir.mkdir(exist_ok=True)

    jjob_template = (
        "#!/bin/bash\n"
        "# J-Job: {description}\n"
        "source jjob_header.sh\n"
        "source ${{EXPDIR}}/parm/config/gfs/config.base\n"
        "echo '{description}'\n"
    )

    (jobs_dir / "JGLOBAL_STAGE_IC").write_text(
        jjob_template.format(description="Stage initial conditions")
    )
    (jobs_dir / "JGLOBAL_FORECAST").write_text(
        jjob_template.format(description="Run GFS forecast")
    )
    (jobs_dir / "JGFS_ATMOS_POST").write_text(
        jjob_template.format(description="Post-processing")
    )
    (jobs_dir / "JGLOBAL_ARCHIVE").write_text(
        jjob_template.format(description="Archive products")
    )

    # --- Ex-scripts (platform-independent) ---
    scripts_dir = dev_root / "scripts"
    scripts_dir.mkdir(exist_ok=True)
    (scripts_dir / "exglobal_stage_ic.sh").write_text(
        "#!/bin/bash\n# Ex-script: Stage IC\necho 'stage_ic done'\n"
    )
    (scripts_dir / "exglobal_forecast.sh").write_text(
        "#!/bin/bash\n# Ex-script: Forecast\necho 'forecast done'\n"
    )

    # --- ush utilities (platform-independent) ---
    ush_dir = dev_root / "ush"
    ush_dir.mkdir(exist_ok=True)
    (ush_dir / "detect_machine.sh").write_text(
        "#!/bin/bash\n# Detect the current HPC platform\n"
        "echo ${MACHINE:-UNKNOWN}\n"
    )
    (ush_dir / "err_exit.sh").write_text(
        "#!/bin/bash\n# Error exit handler\nexit 1\n"
    )

    # --- parm/config/<app>/ with platform-specific resources ---
    gfs_config = dev_root / "parm" / "config" / "gfs"
    gfs_config.mkdir(parents=True, exist_ok=True)
    (gfs_config / "config.resources").write_text(
        "#!/bin/bash\n# Base resources (shared across platforms)\n"
        "export ntasks=24\nexport walltime='02:00:00'\n"
    )
    (gfs_config / "config.resources.HERA").write_text(
        "#!/bin/bash\n# HERA-specific resources\n"
        "export ntasks=48\nexport partition=hera\n"
    )
    (gfs_config / "config.resources.WCOSS2").write_text(
        "#!/bin/bash\n# WCOSS2-specific resources\n"
        "export ntasks=96\nexport queue=dev\n"
    )
    (gfs_config / "config.resources.ORION").write_text(
        "#!/bin/bash\n# ORION-specific resources\n"
        "export ntasks=64\nexport partition=orion\n"
    )
    (gfs_config / "config.resources.HERCULES").write_text(
        "#!/bin/bash\n# HERCULES-specific resources\n"
        "export ntasks=80\nexport partition=hercules\n"
    )

    # --- parm/workflow/ with the workflow config ---
    workflow_dir = dev_root / "parm" / "workflow"
    workflow_dir.mkdir(parents=True, exist_ok=True)

    config = {
        "suite": {
            "name": "gfs_v17_fcst_only",
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {
            "ECF_TRIES": 2,
        },
        "families": [
            {
                "path": "gfs/atmos/stage",
                "tasks": [
                    {
                        "name": "stage_ic",
                        "trigger": "",
                        "jjob": "JGLOBAL_STAGE_IC",
                    },
                ],
            },
            {
                "path": "gfs/atmos/forecast",
                "tasks": [
                    {
                        "name": "fcst",
                        "trigger": "gfs/atmos/stage/stage_ic == complete",
                        "jjob": "JGLOBAL_FORECAST",
                    },
                ],
            },
            {
                "path": "gfs/atmos/archive",
                "tasks": [
                    {
                        "name": "arch",
                        "trigger": "gfs/atmos/forecast/fcst == complete",
                        "jjob": "JGLOBAL_ARCHIVE",
                    },
                ],
            },
        ],
        "inter_cycle_dependencies": [],
    }

    config_path = workflow_dir / "gfs_forecast_only.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # --- ecFlow templates ---
    ecflow_templates = dev_root / "workflow" / "ecflow" / "templates"
    ecflow_templates.mkdir(parents=True, exist_ok=True)
    (ecflow_templates / "task.ecf.j2").write_text(
        "%include <head.h>\n"
        "%include <envsetup.h>\n"
        "# Task: {{ task.name }} | JJob: {{ task.jjob }}\n"
        "${EXPDIR}/ush/universal_wrapper.sh {{ task.jjob }}\n"
        "%include <tail.h>\n"
    )

    # --- modulefiles (platform-specific) ---
    modulefiles_dir = project_root / "modulefiles"
    modulefiles_dir.mkdir(exist_ok=True)
    (modulefiles_dir / "gw_run.hera.lua").write_text(
        '-- Hera run module\nload("intel/2022.1.2")\n'
    )
    (modulefiles_dir / "gw_setup.hera.lua").write_text(
        '-- Hera setup module\nload("cmake/3.23")\n'
    )
    (modulefiles_dir / "gw_run.wcoss2.lua").write_text(
        '-- WCOSS2 run module\nload("intel/19.1.3.304")\n'
    )
    (modulefiles_dir / "gw_setup.wcoss2.lua").write_text(
        '-- WCOSS2 setup module\nload("cmake/3.20")\n'
    )
    (modulefiles_dir / "gw_run.orion.lua").write_text(
        '-- Orion run module\nload("intel/2022.1.2")\n'
    )
    (modulefiles_dir / "gw_setup.orion.lua").write_text(
        '-- Orion setup module\nload("cmake/3.22")\n'
    )
    (modulefiles_dir / "gw_run.hercules.lua").write_text(
        '-- Hercules run module\nload("intel/2023.1")\n'
    )
    (modulefiles_dir / "gw_setup.hercules.lua").write_text(
        '-- Hercules setup module\nload("cmake/3.25")\n'
    )
    (modulefiles_dir / "gw_run.common.lua").write_text(
        "-- Common run module (shared)\n"
    )

    # --- Unconditional artifacts (Req 9.1, 9.2) ---
    sorc_dir = project_root / "sorc"
    sorc_dir.mkdir(exist_ok=True)
    link_workflow = sorc_dir / "link_workflow.sh"
    link_workflow.write_text("#!/bin/bash\n# link_workflow.sh placeholder\n")
    os.chmod(link_workflow, 0o755)
    ufs_fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
    ufs_fix_dir.mkdir(parents=True, exist_ok=True)
    link_fixdirs = ufs_fix_dir / "link_fixdirs.sh"
    link_fixdirs.write_text("#!/bin/bash\n# link_fixdirs.sh placeholder\n")
    os.chmod(link_fixdirs, 0o755)

    return {
        "project_root": project_root,
        "dev_root": dev_root,
        "config_path": config_path,
    }


def _unseal_expdir(expdir: Path) -> None:
    """Remove read-only permissions from a sealed EXPDIR for cleanup."""
    if not expdir.exists():
        return
    for dirpath, dirnames, filenames in os.walk(expdir):
        dp = Path(dirpath)
        try:
            os.chmod(dp, 0o755)
        except OSError:
            pass
        for fn in filenames:
            try:
                os.chmod(dp / fn, 0o644)
            except OSError:
                pass
    try:
        os.chmod(expdir, 0o755)
    except OSError:
        pass


def _collect_file_tree(expdir: Path) -> dict[str, bytes]:
    """Collect all files in an EXPDIR as a dict of relative_path -> content.

    Returns a dict mapping relative file paths (as strings) to their
    binary content.
    """
    files: dict[str, bytes] = {}
    for filepath in sorted(expdir.rglob("*")):
        if filepath.is_file():
            rel_path = str(filepath.relative_to(expdir))
            files[rel_path] = filepath.read_bytes()
    return files


def _diff_file_trees(
    tree_a: dict[str, bytes],
    tree_b: dict[str, bytes],
) -> dict[str, str]:
    """Compare two file trees and return differences.

    Returns a dict mapping relative paths to a description of the difference:
      - "only_in_a": file exists only in tree_a
      - "only_in_b": file exists only in tree_b
      - "content_differs": file exists in both but content differs
    """
    diffs: dict[str, str] = {}

    all_paths = set(tree_a.keys()) | set(tree_b.keys())

    for path in sorted(all_paths):
        if path not in tree_a:
            diffs[path] = "only_in_b"
        elif path not in tree_b:
            diffs[path] = "only_in_a"
        elif tree_a[path] != tree_b[path]:
            diffs[path] = "content_differs"

    return diffs


# ---------------------------------------------------------------------------
# Property Test: Platform Isolation (Property 8)
# ---------------------------------------------------------------------------


@given(
    platform_pair_idx=st.integers(min_value=0, max_value=len(PLATFORM_PAIRS) - 1),
    version=st.from_regex(r"v[0-9]{1,2}\.[0-9]{1,2}\.[0-9]{1,2}", fullmatch=True),
)
@settings(
    max_examples=5,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.function_scoped_fixture],
)
def test_platform_isolation_property(platform_pair_idx, version):
    """Property 8: Platform Isolation.

    **Validates: Requirements 12.3**

    Deploy the same Workflow_Configuration for two different platforms,
    diff the file trees, and assert that differences exist ONLY in
    platform-conditioned paths:
      - env/ (platform-specific env files)
      - parm/config/<app>/config.resources.* (platform-specific resources)
      - modulefiles/ (platform-specific modules)
      - .ecf scheduler directives (PBS vs Slurm)

    J-Jobs, ex-scripts, and ush utilities MUST be IDENTICAL across platforms.
    """
    platform_a, platform_b = PLATFORM_PAIRS[platform_pair_idx]

    with tempfile.TemporaryDirectory() as tmpdir:
        base_path = Path(tmpdir)

        # Create the shared dev tree
        tree = _create_minimal_dev_tree(base_path)

        expdir_a = base_path / f"expdir_{platform_a.lower()}"
        expdir_b = base_path / f"expdir_{platform_b.lower()}"

        try:
            # Deploy for platform A
            run(
                config=str(tree["config_path"]),
                platform=platform_a,
                expdir=str(expdir_a),
                version=version,
                submodule_policy=SubmodulePolicy.FIXTURE,
                fixture_root=str(FIXTURE_ROOT),
            )
            _unseal_expdir(expdir_a)

            # Deploy for platform B
            run(
                config=str(tree["config_path"]),
                platform=platform_b,
                expdir=str(expdir_b),
                version=version,
                submodule_policy=SubmodulePolicy.FIXTURE,
                fixture_root=str(FIXTURE_ROOT),
            )
            _unseal_expdir(expdir_b)

            # Collect file trees
            files_a = _collect_file_tree(expdir_a)
            files_b = _collect_file_tree(expdir_b)

            # Compute differences
            diffs = _diff_file_trees(files_a, files_b)

            # Classify differences
            unexpected_diffs: dict[str, str] = {}
            for path, diff_type in diffs.items():
                if not _is_platform_conditioned(path, platform_a, platform_b):
                    unexpected_diffs[path] = diff_type

            # Assert: no unexpected differences
            assert not unexpected_diffs, (
                f"Platform isolation violated!\n"
                f"Platforms: {platform_a} vs {platform_b}\n"
                f"Version: {version}\n"
                f"Files that differ but should be identical:\n"
                + "\n".join(
                    f"  {path}: {diff_type}"
                    for path, diff_type in sorted(unexpected_diffs.items())
                )
            )

            # Additional assertion: J-Jobs are identical
            jobs_a = {
                k: v for k, v in files_a.items() if k.startswith("jobs/")
            }
            jobs_b = {
                k: v for k, v in files_b.items() if k.startswith("jobs/")
            }
            assert jobs_a == jobs_b, (
                f"J-Jobs differ between {platform_a} and {platform_b}! "
                f"J-Jobs must be platform-independent."
            )

            # Additional assertion: ex-scripts are identical
            scripts_a = {
                k: v for k, v in files_a.items() if k.startswith("scripts/")
            }
            scripts_b = {
                k: v for k, v in files_b.items() if k.startswith("scripts/")
            }
            assert scripts_a == scripts_b, (
                f"Ex-scripts differ between {platform_a} and {platform_b}! "
                f"Ex-scripts must be platform-independent."
            )

            # Additional assertion: ush utilities are identical
            ush_a = {
                k: v for k, v in files_a.items() if k.startswith("ush/")
            }
            ush_b = {
                k: v for k, v in files_b.items() if k.startswith("ush/")
            }
            assert ush_a == ush_b, (
                f"ush utilities differ between {platform_a} and {platform_b}! "
                f"ush utilities must be platform-independent."
            )

            # Assert that platform-conditioned differences DO exist
            # (confirms the platforms actually produce different content)
            assert len(diffs) > 0, (
                f"No differences found between {platform_a} and {platform_b} "
                f"deployments. Expected at least env/ and modulefiles/ to differ."
            )

        finally:
            _unseal_expdir(expdir_a)
            _unseal_expdir(expdir_b)


# ---------------------------------------------------------------------------
# Focused unit-style tests for platform isolation
# ---------------------------------------------------------------------------


class TestPlatformIsolationHERAvsWCOSS2:
    """Focused tests for HERA vs WCOSS2 platform isolation."""

    @pytest.fixture
    def deployed_pair(self, tmp_path):
        """Deploy for both HERA and WCOSS2, return file trees."""
        tree = _create_minimal_dev_tree(tmp_path)

        expdir_hera = tmp_path / "expdir_hera"
        expdir_wcoss2 = tmp_path / "expdir_wcoss2"

        run(
            config=str(tree["config_path"]),
            platform="HERA",
            expdir=str(expdir_hera),
            version="v1.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )
        _unseal_expdir(expdir_hera)

        run(
            config=str(tree["config_path"]),
            platform="WCOSS2",
            expdir=str(expdir_wcoss2),
            version="v1.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )
        _unseal_expdir(expdir_wcoss2)

        files_hera = _collect_file_tree(expdir_hera)
        files_wcoss2 = _collect_file_tree(expdir_wcoss2)

        yield {
            "files_hera": files_hera,
            "files_wcoss2": files_wcoss2,
            "expdir_hera": expdir_hera,
            "expdir_wcoss2": expdir_wcoss2,
        }

        _unseal_expdir(expdir_hera)
        _unseal_expdir(expdir_wcoss2)

    def test_jobs_identical(self, deployed_pair):
        """J-Jobs are identical across HERA and WCOSS2."""
        jobs_hera = {
            k: v
            for k, v in deployed_pair["files_hera"].items()
            if k.startswith("jobs/")
        }
        jobs_wcoss2 = {
            k: v
            for k, v in deployed_pair["files_wcoss2"].items()
            if k.startswith("jobs/")
        }
        assert jobs_hera == jobs_wcoss2

    def test_scripts_identical(self, deployed_pair):
        """Ex-scripts are identical across HERA and WCOSS2."""
        scripts_hera = {
            k: v
            for k, v in deployed_pair["files_hera"].items()
            if k.startswith("scripts/")
        }
        scripts_wcoss2 = {
            k: v
            for k, v in deployed_pair["files_wcoss2"].items()
            if k.startswith("scripts/")
        }
        assert scripts_hera == scripts_wcoss2

    def test_ush_identical(self, deployed_pair):
        """ush utilities are identical across HERA and WCOSS2."""
        ush_hera = {
            k: v
            for k, v in deployed_pair["files_hera"].items()
            if k.startswith("ush/")
        }
        ush_wcoss2 = {
            k: v
            for k, v in deployed_pair["files_wcoss2"].items()
            if k.startswith("ush/")
        }
        assert ush_hera == ush_wcoss2

    def test_env_files_differ(self, deployed_pair):
        """env/ files differ between HERA and WCOSS2."""
        env_hera = {
            k: v
            for k, v in deployed_pair["files_hera"].items()
            if k.startswith("env/")
        }
        env_wcoss2 = {
            k: v
            for k, v in deployed_pair["files_wcoss2"].items()
            if k.startswith("env/")
        }
        # They should have different env files
        assert env_hera != env_wcoss2

    def test_modulefiles_differ(self, deployed_pair):
        """modulefiles/ differ between HERA and WCOSS2."""
        mods_hera = {
            k: v
            for k, v in deployed_pair["files_hera"].items()
            if k.startswith("modulefiles/")
        }
        mods_wcoss2 = {
            k: v
            for k, v in deployed_pair["files_wcoss2"].items()
            if k.startswith("modulefiles/")
        }
        assert mods_hera != mods_wcoss2

    def test_ecf_scripts_differ(self, deployed_pair):
        """ecf scripts differ (scheduler directives: Slurm vs PBS)."""
        ecf_hera = {
            k: v
            for k, v in deployed_pair["files_hera"].items()
            if k.endswith(".ecf")
        }
        ecf_wcoss2 = {
            k: v
            for k, v in deployed_pair["files_wcoss2"].items()
            if k.endswith(".ecf")
        }
        # Same set of .ecf files should exist
        assert set(ecf_hera.keys()) == set(ecf_wcoss2.keys()), (
            "Different .ecf files generated for HERA vs WCOSS2"
        )
        # But content should differ (scheduler directives)
        any_differ = any(
            ecf_hera[k] != ecf_wcoss2[k] for k in ecf_hera
        )
        assert any_differ, (
            ".ecf scripts should differ between HERA (Slurm) and WCOSS2 (PBS)"
        )

    def test_only_platform_conditioned_paths_differ(self, deployed_pair):
        """Only platform-conditioned paths differ between deployments."""
        diffs = _diff_file_trees(
            deployed_pair["files_hera"],
            deployed_pair["files_wcoss2"],
        )

        unexpected = {
            path: diff_type
            for path, diff_type in diffs.items()
            if not _is_platform_conditioned(path, "HERA", "WCOSS2")
        }

        assert not unexpected, (
            f"Unexpected differences between HERA and WCOSS2:\n"
            + "\n".join(f"  {p}: {d}" for p, d in sorted(unexpected.items()))
        )

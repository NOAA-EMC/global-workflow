"""Integration test for Self-Containment (Property 4).

Verifies that a deployed EXPDIR is fully self-contained and does not
depend on the dev/ source tree at runtime. After deployment, the dev/
tree is made inaccessible and the EXPDIR is validated to ensure:

  - All files referenced in the .def file exist within the EXPDIR
  - .ecf scripts do not reference paths back to dev/
  - The EXPDIR has all required NCO layout directories
  - No symlinks point outside the EXPDIR

Validates: Requirements 3.1
"""

from __future__ import annotations

import os
import re
import stat
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import run, SubmodulePolicy


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Committed Submodule_Fixture tree (Req 6.2, 6.7). Resolved relative to this
# test file so it works regardless of the current working directory.
FIXTURE_ROOT = (Path(__file__).resolve().parent / "fixtures" / "submodules")

# NCO production layout directories required for self-containment (Req 3.2)
NCO_REQUIRED_DIRS = [
    "jobs",
    "scripts",
    "ush",
    "ecf",
    "versions",
]

# Directories that should exist if the workflow has content for them
NCO_OPTIONAL_DIRS = [
    "parm",
    "sorc",
    "fix",
    "modulefiles",
]


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def minimal_dev_tree(tmp_path):
    """Create a minimal dev/ tree that mirrors the real structure.

    This fixture creates a self-contained dev/ tree with just enough
    content to exercise the full pipeline without depending on the
    full repository content (which may have missing optional dirs).
    """
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

    # Create required subdirectories
    (dev_root / "jobs").mkdir()
    (dev_root / "scripts").mkdir()
    (dev_root / "ush").mkdir()
    (dev_root / "parm" / "workflow").mkdir(parents=True)
    (dev_root / "parm" / "config" / "gfs_forecast_only").mkdir(parents=True)
    (dev_root / "sorc").mkdir()
    (dev_root / "versions").mkdir()
    (dev_root / "modulefiles").mkdir()
    (dev_root / "env").mkdir()
    (dev_root / "workflow" / "ecflow" / "templates").mkdir(parents=True)
    (dev_root / "workflow" / "ecflow" / "include").mkdir(parents=True)

    # Create the gfs_forecast_only.yaml config
    config = {
        "suite": {
            "name": "gfs_v17_fcst_only",
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {
            "ECF_TRIES": 2,
            "ECF_JOB_CMD": "uwtools submit %ECF_JOB% %ECF_JOBOUT%",
        },
        "cycles": [
            {
                "name": "gfs",
                "repeat": {
                    "type": "date",
                    "variable": "YMD",
                    "start": "{{ idate }}",
                    "end": "{{ edate }}",
                    "step": 1,
                },
                "time": "00:00 06:00 12:00 18:00",
            }
        ],
        "families": [
            {
                "path": "gfs/atmos/stage",
                "tasks": [
                    {
                        "name": "stage_ic",
                        "trigger": "",
                        "jjob": "JGLOBAL_STAGE_IC",
                    }
                ],
            },
            {
                "path": "gfs/atmos/forecast",
                "tasks": [
                    {
                        "name": "fcst",
                        "trigger": "gfs/atmos/stage/stage_ic == complete",
                        "jjob": "JGLOBAL_FORECAST",
                        "events": ["forecast_hour"],
                        "meters": [
                            {"name": "forecast_hour", "min": 0, "max": 120}
                        ],
                    }
                ],
            },
            {
                "path": "gfs/atmos/post",
                "tasks": [
                    {
                        "name": "post_f000",
                        "trigger": "gfs/atmos/forecast/fcst:forecast_hour ge 0",
                        "jjob": "JGFS_ATMOS_POST",
                        "variables": {"FHOUR": "0"},
                    },
                    {
                        "name": "post_f006",
                        "trigger": "gfs/atmos/forecast/fcst:forecast_hour ge 6",
                        "jjob": "JGFS_ATMOS_POST",
                        "variables": {"FHOUR": "6"},
                    },
                ],
            },
            {
                "path": "gfs/atmos/archive",
                "tasks": [
                    {
                        "name": "arch",
                        "trigger": "gfs/atmos/post/post_f006 == complete",
                        "jjob": "JGLOBAL_ARCHIVE",
                    }
                ],
            },
        ],
        "inter_cycle_dependencies": [],
    }

    config_path = dev_root / "parm" / "workflow" / "gfs_forecast_only.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # Create a minimal task.ecf.j2 template
    template = """\
%include <head.h>
%include <envsetup.h>
# Task: {{ task.name }} | JJob: {{ task.jjob }}
${EXPDIR}/ush/universal_wrapper.sh {{ task.jjob }}
%include <tail.h>
"""
    (dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2").write_text(
        template
    )

    # Create ecFlow include files
    (dev_root / "workflow" / "ecflow" / "include" / "head.h").write_text(
        "# head.h - ecFlow header\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "tail.h").write_text(
        "# tail.h - ecFlow tail\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "envsetup.h").write_text(
        "# envsetup.h - environment setup\n"
    )

    # Create sample J-Jobs (EE2 compliant)
    jjob_template = (
        "#!/bin/bash\n"
        "# J-Job: {name}\n"
        ". ${{HOMEgfs}}/ush/jjob_header.sh\n"
        "export DATA=${{DATAROOT}}/${{jobid}}\n"
        "export cycle=t${{cyc}}z\n"
        "export PDY=${{PDY}}\n"
        "export NET=${{NET}}\n"
        "export RUN=${{RUN}}\n"
        "export COMIN=${{COMROOT}}/${{NET}}/${{model_ver}}/${{RUN}}.${{PDY}}/${{cyc}}/atmos\n"
        "export COMOUT=${{COMROOT}}/${{NET}}/${{model_ver}}/${{RUN}}.${{PDY}}/${{cyc}}/atmos\n"
        "export pgmout=OUTPUT.$$\n"
        "export jobid=${{job}}.$$\n"
        "exit 0\n"
    )
    for jjob_name in [
        "JGLOBAL_FORECAST",
        "JGLOBAL_STAGE_IC",
        "JGFS_ATMOS_POST",
        "JGLOBAL_ARCHIVE",
    ]:
        (dev_root / "jobs" / jjob_name).write_text(
            jjob_template.format(name=jjob_name)
        )

    # Create a sample ex-script
    (dev_root / "scripts" / "exglobal_forecast.sh").write_text(
        "#!/bin/bash\n# Ex-script: exglobal_forecast.sh\nexit 0\n"
    )

    # Create a sample ush utility
    (dev_root / "ush" / "detect_machine.sh").write_text(
        "#!/bin/bash\n# detect_machine.sh\nexport MACHINE=HERA\n"
    )

    # Create a versions file
    (dev_root / "versions" / "run.ver").write_text(
        "export gfs_ver=v17.0.0\n"
    )

    # Create a .git directory to mark repo root
    (tmp_path / ".git").mkdir()

    # Create unconditional artifacts (Req 9.1, 9.2)
    sorc_dir = tmp_path / "sorc"
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
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


@pytest.fixture
def deployed_expdir(minimal_dev_tree):
    """Deploy a workflow and return the EXPDIR path along with dev_root.

    This fixture runs the full pipeline and returns both the sealed
    EXPDIR and the dev_root so tests can manipulate dev/ accessibility.
    """
    info = minimal_dev_tree
    expdir = info["tmp_path"] / "EXPDIR"

    run(
        config=str(info["config_path"]),
        platform="HERA",
        expdir=str(expdir),
        version="v17.0.0",
        submodule_policy=SubmodulePolicy.FIXTURE,
        fixture_root=str(FIXTURE_ROOT),
    )

    # Unseal the EXPDIR so we can inspect it (sealing is tested separately)
    _unseal(expdir)

    return {
        "expdir": expdir,
        "dev_root": info["dev_root"],
        "tmp_path": info["tmp_path"],
    }


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _unseal(expdir: Path) -> None:
    """Restore write permissions on a sealed EXPDIR for inspection."""
    os.chmod(expdir, 0o755)
    for item in expdir.rglob("*"):
        if item.is_dir():
            os.chmod(item, 0o755)
        elif item.is_file():
            os.chmod(item, 0o644)


def _make_inaccessible(path: Path) -> None:
    """Make a directory tree inaccessible (chmod 000)."""
    # First make all contents inaccessible bottom-up
    for item in sorted(path.rglob("*"), reverse=True):
        if item.is_dir():
            os.chmod(item, 0o000)
        elif item.is_file():
            os.chmod(item, 0o000)
    os.chmod(path, 0o000)


def _restore_accessible(path: Path) -> None:
    """Restore accessibility to a directory tree."""
    os.chmod(path, 0o755)
    for item in path.rglob("*"):
        if item.is_dir():
            os.chmod(item, 0o755)
        elif item.is_file():
            os.chmod(item, 0o644)


# ---------------------------------------------------------------------------
# Integration Tests: Self-Containment (Property 4)
# ---------------------------------------------------------------------------


class TestSelfContainment:
    """Verify that the deployed EXPDIR is fully self-contained.

    Property 4 states: The EXPDIR executes without reading any file
    from dev/.

    Since we cannot run ecflow_server in this environment, we verify
    self-containment structurally by checking that:
      1. All files referenced in the .def file exist within the EXPDIR
      2. .ecf scripts do not reference paths back to dev/
      3. The EXPDIR has all required NCO layout directories
      4. No symlinks point outside the EXPDIR

    Validates: Requirements 3.1
    """

    def test_def_file_references_exist_in_expdir(self, deployed_expdir):
        """All ecf_home/ecf_files/ecf_include paths in .def resolve within EXPDIR.

        The .def file declares ECF_HOME, ECF_FILES, and ECF_INCLUDE
        variables that ecFlow uses to locate scripts. These must all
        point to directories within the EXPDIR.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]

        # Find the .def file
        def_dir = expdir / "ecf" / "defs"
        assert def_dir.exists(), "ecf/defs/ directory not found in EXPDIR"

        def_files = list(def_dir.glob("*.def"))
        assert len(def_files) > 0, "No .def files found in EXPDIR"

        for def_file in def_files:
            content = def_file.read_text()

            # Extract ECF_HOME, ECF_FILES, ECF_INCLUDE variable definitions
            # These appear as: edit ECF_HOME '/path/to/ecf'
            ecf_var_pattern = re.compile(
                r"edit\s+(ECF_HOME|ECF_FILES|ECF_INCLUDE)\s+'([^']+)'"
            )
            matches = ecf_var_pattern.findall(content)

            for var_name, var_value in matches:
                # The value may contain ${EXPDIR} or the literal EXPDIR path
                # Replace ${EXPDIR} with the actual path for validation
                resolved = var_value.replace("${EXPDIR}", str(expdir))
                resolved = resolved.replace("$EXPDIR", str(expdir))

                # If the path is absolute and references EXPDIR, check it exists
                if str(expdir) in resolved:
                    resolved_path = Path(resolved)
                    assert resolved_path.exists(), (
                        f".def file references {var_name}='{var_value}' "
                        f"which resolves to '{resolved}' but does not exist "
                        f"within the EXPDIR"
                    )

    def test_ecf_scripts_no_dev_references(self, deployed_expdir):
        """No .ecf script references paths back to dev/.

        After deployment, .ecf scripts should only reference paths
        within the EXPDIR (via ${EXPDIR} or relative paths). Any
        hardcoded reference to dev/ would break self-containment.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]
        dev_root = deployed_expdir["dev_root"]

        ecf_scripts_dir = expdir / "ecf" / "scripts"
        if not ecf_scripts_dir.exists():
            pytest.skip("No ecf/scripts/ directory in EXPDIR")

        ecf_files = list(ecf_scripts_dir.rglob("*.ecf"))
        assert len(ecf_files) > 0, "No .ecf scripts found"

        # Pattern to detect references to the dev/ source tree
        dev_path_str = str(dev_root)
        dev_patterns = [
            dev_path_str,           # Absolute path to dev/
            "/dev/jobs/",           # Common dev/ subpaths
            "/dev/scripts/",
            "/dev/ush/",
            "/dev/parm/",
            "/dev/workflow/",
        ]

        violations = []
        for ecf_file in ecf_files:
            content = ecf_file.read_text()
            rel_name = ecf_file.relative_to(expdir)

            for pattern in dev_patterns:
                if pattern in content:
                    violations.append(
                        f"{rel_name}: contains reference to '{pattern}'"
                    )

        assert not violations, (
            "Self-containment violation: .ecf scripts reference dev/ paths:\n"
            + "\n".join(f"  - {v}" for v in violations)
        )

    def test_expdir_has_nco_layout_directories(self, deployed_expdir):
        """EXPDIR contains all required NCO layout directories.

        A self-contained EXPDIR must have the standard NCO production
        layout directories so that J-Jobs and scripts can find their
        dependencies without reaching back to dev/.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]

        existing_dirs = {d.name for d in expdir.iterdir() if d.is_dir()}

        missing = []
        for required_dir in NCO_REQUIRED_DIRS:
            if required_dir not in existing_dirs:
                missing.append(required_dir)

        assert not missing, (
            f"EXPDIR is missing required NCO layout directories: {missing}. "
            f"Found: {sorted(existing_dirs)}"
        )

    def test_no_symlinks_outside_expdir(self, deployed_expdir):
        """No symlinks in the EXPDIR point to targets outside the EXPDIR.

        Symlinks pointing outside the EXPDIR would break self-containment
        because they depend on external filesystem state.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]

        external_symlinks = []
        for item in expdir.rglob("*"):
            if item.is_symlink():
                target = item.resolve()
                # Check if the symlink target is outside the EXPDIR
                try:
                    target.relative_to(expdir)
                except ValueError:
                    rel_link = item.relative_to(expdir)
                    external_symlinks.append(
                        f"{rel_link} -> {target}"
                    )

        assert not external_symlinks, (
            "Self-containment violation: symlinks point outside EXPDIR:\n"
            + "\n".join(f"  - {s}" for s in external_symlinks)
        )

    def test_jobs_do_not_reference_dev_paths(self, deployed_expdir):
        """Staged J-Jobs do not contain hardcoded references to dev/.

        J-Jobs should use ${HOMEgfs} or ${EXPDIR} variables, not
        hardcoded paths to the dev/ source tree.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]
        dev_root = deployed_expdir["dev_root"]

        jobs_dir = expdir / "jobs"
        if not jobs_dir.exists():
            pytest.skip("No jobs/ directory in EXPDIR")

        job_files = list(jobs_dir.iterdir())
        assert len(job_files) > 0, "No J-Job files found"

        dev_path_str = str(dev_root)
        violations = []

        for job_file in job_files:
            if not job_file.is_file():
                continue
            content = job_file.read_text()
            rel_name = job_file.relative_to(expdir)

            if dev_path_str in content:
                violations.append(
                    f"{rel_name}: contains hardcoded dev/ path '{dev_path_str}'"
                )

        assert not violations, (
            "Self-containment violation: J-Jobs reference dev/ paths:\n"
            + "\n".join(f"  - {v}" for v in violations)
        )

    def test_scripts_do_not_reference_dev_paths(self, deployed_expdir):
        """Staged scripts do not contain hardcoded references to dev/.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]
        dev_root = deployed_expdir["dev_root"]

        scripts_dir = expdir / "scripts"
        if not scripts_dir.exists():
            pytest.skip("No scripts/ directory in EXPDIR")

        script_files = list(scripts_dir.rglob("*"))
        dev_path_str = str(dev_root)
        violations = []

        for script_file in script_files:
            if not script_file.is_file():
                continue
            content = script_file.read_text()
            rel_name = script_file.relative_to(expdir)

            if dev_path_str in content:
                violations.append(
                    f"{rel_name}: contains hardcoded dev/ path"
                )

        assert not violations, (
            "Self-containment violation: scripts reference dev/ paths:\n"
            + "\n".join(f"  - {v}" for v in violations)
        )

    def test_dev_inaccessible_expdir_still_valid(self, deployed_expdir):
        """EXPDIR remains structurally valid after dev/ is made inaccessible.

        This is the core self-containment test: after chmod 000 on dev/,
        verify that all files in the EXPDIR are still readable and that
        the manifest can be validated without accessing dev/.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]
        dev_root = deployed_expdir["dev_root"]

        # Make dev/ completely inaccessible
        _make_inaccessible(dev_root)

        try:
            # Verify dev/ is truly inaccessible
            assert not os.access(dev_root, os.R_OK), (
                "dev/ should not be readable after chmod 000"
            )

            # Verify all EXPDIR files are still readable
            unreadable = []
            for item in expdir.rglob("*"):
                if item.is_file():
                    try:
                        item.read_bytes()
                    except PermissionError:
                        unreadable.append(str(item.relative_to(expdir)))

            assert not unreadable, (
                f"Files in EXPDIR became unreadable after dev/ was removed: "
                f"{unreadable[:10]}"
            )

            # Verify manifest.yaml is readable and parseable
            manifest_path = expdir / "manifest.yaml"
            assert manifest_path.exists(), "manifest.yaml not found"
            manifest = yaml.safe_load(manifest_path.read_text())
            assert "snapshot_id" in manifest
            assert "files" in manifest
            assert len(manifest["files"]) > 0

            # Verify the .def file is readable
            def_dir = expdir / "ecf" / "defs"
            def_files = list(def_dir.glob("*.def"))
            assert len(def_files) > 0, "No .def files found"
            for def_file in def_files:
                content = def_file.read_text()
                assert len(content) > 0, f"{def_file.name} is empty"

            # Verify ecf scripts are readable
            ecf_scripts_dir = expdir / "ecf" / "scripts"
            if ecf_scripts_dir.exists():
                ecf_files = list(ecf_scripts_dir.rglob("*.ecf"))
                for ecf_file in ecf_files:
                    content = ecf_file.read_text()
                    assert len(content) > 0, f"{ecf_file.name} is empty"

        finally:
            # Always restore dev/ permissions for cleanup
            _restore_accessible(dev_root)

    def test_manifest_files_all_exist_in_expdir(self, deployed_expdir):
        """Every file listed in manifest.yaml exists within the EXPDIR.

        The manifest is the authoritative list of what the EXPDIR
        contains. If any listed file is missing, the deployment is
        not self-contained.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]

        manifest_path = expdir / "manifest.yaml"
        assert manifest_path.exists(), "manifest.yaml not found"

        manifest = yaml.safe_load(manifest_path.read_text())
        files_section = manifest.get("files", {})

        missing_files = []
        for rel_path in files_section:
            full_path = expdir / rel_path
            if not full_path.exists():
                missing_files.append(rel_path)

        assert not missing_files, (
            f"Manifest lists {len(missing_files)} file(s) not found in EXPDIR:\n"
            + "\n".join(f"  - {f}" for f in missing_files[:20])
        )

    def test_ecf_include_files_present(self, deployed_expdir):
        """ecFlow include files referenced by .ecf scripts exist in EXPDIR.

        .ecf scripts use %include <file.h> directives. The referenced
        include files must exist under ecf/include/ in the EXPDIR.

        Validates: Requirements 3.1
        """
        expdir = deployed_expdir["expdir"]

        ecf_scripts_dir = expdir / "ecf" / "scripts"
        include_dir = expdir / "ecf" / "include"

        if not ecf_scripts_dir.exists():
            pytest.skip("No ecf/scripts/ directory")

        ecf_files = list(ecf_scripts_dir.rglob("*.ecf"))
        if not ecf_files:
            pytest.skip("No .ecf scripts found")

        # Extract %include <filename> references
        include_pattern = re.compile(r"%include\s+<([^>]+)>")
        referenced_includes = set()

        for ecf_file in ecf_files:
            content = ecf_file.read_text()
            matches = include_pattern.findall(content)
            referenced_includes.update(matches)

        # Verify each referenced include file exists
        missing_includes = []
        for include_name in referenced_includes:
            include_path = include_dir / include_name
            if not include_path.exists():
                missing_includes.append(include_name)

        assert not missing_includes, (
            f"ecf scripts reference include files not found in "
            f"ecf/include/:\n"
            + "\n".join(f"  - {f}" for f in missing_includes)
        )

"""Property-based test: No Unresolved Tokens (Property 14).

Deploys a workflow configuration via pipeline.run() to a temporary EXPDIR,
then walks all rendered files in the EXPDIR and asserts that none contain
unresolved Jinja2 tokens (``{{``, ``{%``, ``{#``).

Per Requirement 4.6: "THE EXPDIR SHALL NOT contain any unresolved Jinja2
tokens in files that the Deployment_Tool has marked as rendered."

Files excluded from the scan (not rendered templates):
- ``parm/workflow/*.yaml`` — the Workflow_Configuration source (staged verbatim)
- ``ecf/defs/*.def`` — ecFlow definition files (generated, contain ecFlow
  variable references like ``{{ EXPDIR }}`` that ecFlow resolves at runtime)
- ``workflow/provenance.yaml`` — deployment metadata recording raw config values
- ``manifest.yaml`` — deployment manifest (generated metadata)

**Validates: Requirements 4.6**

Traces to: Design Document - Correctness Property 14
  "No rendered file in the EXPDIR contains ``{{``, ``{%``, or ``{#`` sequences."
"""

from __future__ import annotations

import os
import stat
import sys
import tempfile
from pathlib import Path

import pytest
import yaml
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import run, SubmodulePolicy

# Committed Submodule_Fixture tree (Req 6.2, 6.7). Resolved relative to this
# test file so it works regardless of the current working directory.
FIXTURE_ROOT = (Path(__file__).resolve().parent / "fixtures" / "submodules")


# ---------------------------------------------------------------------------
# Jinja2 token patterns that must NOT appear in rendered files
# ---------------------------------------------------------------------------

UNRESOLVED_TOKENS = ("{{", "{%", "{#")

# Files/patterns that are NOT rendered templates and may legitimately
# contain Jinja2-like syntax (ecFlow variable references, raw config, metadata)
EXCLUDED_PATTERNS = (
    "parm/workflow/",       # Workflow config YAML (staged verbatim, not rendered)
    "ecf/defs/",           # ecFlow .def files (generated, contain ecFlow vars)
    "workflow/provenance",  # Provenance metadata (records raw config values)
    "manifest.yaml",       # Deployment manifest (generated metadata)
)


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating valid workflow configurations
# ---------------------------------------------------------------------------

# Valid identifiers for task/family names
_identifier = st.from_regex(r"[a-z][a-z0-9_]{0,7}", fullmatch=True)


@st.composite
def _task_strategy(draw, family_path: str, prev_task: str | None = None):
    """Generate a valid task entry for a workflow config."""
    name = draw(_identifier)
    jjob = "J" + name.upper()
    trigger = f"{family_path}/{prev_task} == complete" if prev_task else ""
    return {
        "name": name,
        "jjob": jjob,
        "trigger": trigger,
    }


@st.composite
def _workflow_config_strategy(draw):
    """Generate a valid Workflow_Configuration YAML structure.

    Produces configs with:
    - A suite name
    - 1-3 families, each with 1-3 tasks
    - No Jinja2 tokens in task-level values (all concrete)
    """
    suite_name = draw(_identifier) + "_suite"
    num_families = draw(st.integers(min_value=1, max_value=3))

    families = []
    used_family_paths = set()

    for i in range(num_families):
        # Generate unique family path
        segments = draw(st.lists(_identifier, min_size=1, max_size=2))
        family_path = "/".join(segments)
        # Ensure uniqueness
        if family_path in used_family_paths:
            family_path = f"{family_path}/f{i}"
        used_family_paths.add(family_path)

        num_tasks = draw(st.integers(min_value=1, max_value=3))
        tasks = []
        prev_task = None

        for _ in range(num_tasks):
            task = draw(_task_strategy(family_path, prev_task))
            # Ensure unique task names within family
            existing_names = {t["name"] for t in tasks}
            while task["name"] in existing_names:
                task["name"] = task["name"] + "x"
                task["jjob"] = "J" + task["name"].upper()
            tasks.append(task)
            prev_task = task["name"]

        families.append({"path": family_path, "tasks": tasks})

    config = {
        "suite": {
            "name": suite_name,
            "ecf_home": "{{ EXPDIR }}/ecf",
            "ecf_files": "{{ EXPDIR }}/ecf/scripts",
            "ecf_include": "{{ EXPDIR }}/ecf/include",
        },
        "defaults": {
            "ECF_TRIES": 2,
        },
        "families": families,
    }

    return config


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _create_dev_tree(tmp_path: Path, config: dict) -> dict:
    """Create a minimal dev/ tree suitable for pipeline.run().

    Returns a dict with 'dev_root', 'config_path', and 'tmp_path'.
    """
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

    # Create required subdirectories
    (dev_root / "jobs").mkdir()
    (dev_root / "scripts").mkdir()
    (dev_root / "ush").mkdir()
    (dev_root / "parm" / "workflow").mkdir(parents=True)
    (dev_root / "parm" / "config" / "gfs").mkdir(parents=True)
    (dev_root / "workflow" / "ecflow" / "templates").mkdir(parents=True)

    # Write the workflow config
    config_path = dev_root / "parm" / "workflow" / "test_config.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # Create a minimal task.ecf.j2 template (no unresolved tokens after render)
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


def _unseal_expdir(expdir: Path) -> None:
    """Remove read-only permissions from a sealed EXPDIR for cleanup."""
    for item in expdir.rglob("*"):
        if item.is_file():
            os.chmod(item, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP | stat.S_IROTH)
        elif item.is_dir():
            os.chmod(item, stat.S_IRWXU | stat.S_IRGRP | stat.S_IXGRP)
    os.chmod(expdir, stat.S_IRWXU)


def _is_excluded(rel_path: str) -> bool:
    """Check if a file path matches an excluded pattern.

    Excluded files are not rendered templates and may legitimately
    contain Jinja2-like syntax.
    """
    for pattern in EXCLUDED_PATTERNS:
        if rel_path.startswith(pattern) or rel_path == pattern:
            return True
    return False


def _scan_for_unresolved_tokens(expdir: Path) -> list[tuple[str, str, int]]:
    """Scan rendered files in EXPDIR for unresolved Jinja2 tokens.

    Skips files that are not rendered templates (config YAML, .def files,
    provenance metadata, manifest).

    Returns a list of (file_path, token, line_number) tuples for any
    unresolved tokens found.
    """
    violations = []
    for filepath in sorted(expdir.rglob("*")):
        if not filepath.is_file():
            continue

        rel_path = str(filepath.relative_to(expdir))

        # Skip files that are not rendered templates
        if _is_excluded(rel_path):
            continue

        try:
            content = filepath.read_text(encoding="utf-8")
        except (UnicodeDecodeError, PermissionError):
            # Skip binary files or unreadable files
            continue

        for line_num, line in enumerate(content.splitlines(), start=1):
            for token in UNRESOLVED_TOKENS:
                if token in line:
                    violations.append((rel_path, token, line_num))

    return violations


# ---------------------------------------------------------------------------
# Property Test: No Unresolved Tokens (Property 14)
# ---------------------------------------------------------------------------


@given(config=_workflow_config_strategy())
@settings(
    max_examples=50,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_no_unresolved_tokens_property(config: dict):
    """Property 14: No rendered file in EXPDIR contains unresolved Jinja2 tokens.

    **Validates: Requirements 4.6**

    Steps:
    1. Generate a random valid Workflow_Configuration
    2. Deploy it via pipeline.run() to a temporary EXPDIR
    3. Walk all rendered files in the EXPDIR (excluding non-rendered metadata)
    4. Assert none contain ``{{``, ``{%``, or ``{#``
    """
    with tempfile.TemporaryDirectory() as tmp_dir:
        tmp_path = Path(tmp_dir)
        info = _create_dev_tree(tmp_path, config)
        expdir = tmp_path / "expdir"

        # Deploy via the pipeline
        result = run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v1.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )

        assert result["dry_run"] is False
        assert expdir.exists()

        # Unseal so we can read files
        _unseal_expdir(expdir)

        # Scan all rendered files for unresolved Jinja2 tokens
        violations = _scan_for_unresolved_tokens(expdir)

        assert violations == [], (
            "Unresolved Jinja2 tokens found in rendered EXPDIR files:\n"
            + "\n".join(
                f"  {path}:{line} contains '{token}'"
                for path, token, line in violations
            )
        )


# ---------------------------------------------------------------------------
# Deterministic test: deploy a concrete config and scan
# ---------------------------------------------------------------------------


def test_no_unresolved_tokens_forecast_only():
    """Deploy a forecast-only config and verify no unresolved tokens.

    **Validates: Requirements 4.6**

    This is a concrete example test that complements the property test above.
    It uses a realistic multi-family config with post-processing tasks.
    """
    with tempfile.TemporaryDirectory() as tmp_dir:
        tmp_path = Path(tmp_dir)

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
                        }
                    ],
                },
                {
                    "path": "gfs/atmos/post",
                    "tasks": [
                        {
                            "name": "post_f000",
                            "trigger": "gfs/atmos/forecast/fcst == complete",
                            "jjob": "JGFS_ATMOS_POST",
                            "variables": {"FHOUR": "0"},
                        }
                    ],
                },
            ],
        }

        info = _create_dev_tree(tmp_path, config)
        expdir = tmp_path / "expdir"

        # Deploy via the pipeline
        result = run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            submodule_policy=SubmodulePolicy.FIXTURE,
            fixture_root=str(FIXTURE_ROOT),
        )

        assert result["dry_run"] is False
        assert expdir.exists()

        # Unseal so we can read files
        _unseal_expdir(expdir)

        # Scan all rendered files for unresolved Jinja2 tokens
        violations = _scan_for_unresolved_tokens(expdir)

        assert violations == [], (
            "Unresolved Jinja2 tokens found in rendered EXPDIR files:\n"
            + "\n".join(
                f"  {path}:{line} contains '{token}'"
                for path, token, line in violations
            )
        )

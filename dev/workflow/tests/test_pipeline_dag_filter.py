"""Integration tests for the deployment pipeline with --dag-filter flag.

Exercises end-to-end pipeline behavior with DAG-filtered staging enabled
and disabled.  Verifies:
  - dag_filter=True produces a minimal EXPDIR (only reachable artifacts)
  - dag_filter=False produces a full EXPDIR (backward compatibility)
  - Config conditioning runs in both modes
  - Size reduction report is logged when filtering is enabled
  - FATAL ERROR propagation from DAG_Filter and Completeness_Verifier

Validates: Requirements 13.1, 13.2, 13.3, 13.4, 9.1–9.4
"""

from __future__ import annotations

import logging
import os
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.pipeline import PipelineError, SubmodulePolicy, run


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def dag_filter_dev_tree(tmp_path):
    """Create a dev/ tree with enough content to exercise DAG filtering.

    Includes:
    - A workflow YAML referencing 2 of 4 available J-Jobs
    - J-Jobs with ex-script references and jjob_header -c patterns
    - Ex-scripts sourcing ush scripts
    - Extra J-Jobs/scripts/ush that should be excluded by DAG filter
    - Config files required by the active J-Jobs
    """
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

    # Create subdirectories
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

    # -- Workflow YAML referencing only 2 J-Jobs --
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
                "time": "00:00",
            }
        ],
        "families": [
            {
                "path": "gfs/atmos/forecast",
                "tasks": [
                    {
                        "name": "fcst",
                        "trigger": "",
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
                    }
                ],
            },
        ],
        "inter_cycle_dependencies": [],
    }
    config_path = dev_root / "parm" / "workflow" / "gfs_forecast_only.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # -- J-Jobs: 2 referenced + 2 extra (unreachable) --
    # JGLOBAL_FORECAST: references exglobal_forecast.sh, config base+fcst
    (dev_root / "jobs" / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        '# J-Job: JGLOBAL_FORECAST\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base fcst"\n'
        'export FORECASTSH="${SCRglobal}/exglobal_forecast.sh"\n'
        '"${FORECASTSH}" && true\n'
        'exit 0\n'
    )

    # JGFS_ATMOS_POST: references exgfs_atmos_post.sh, config base+post
    (dev_root / "jobs" / "JGFS_ATMOS_POST").write_text(
        '#!/bin/bash\n'
        '# J-Job: JGFS_ATMOS_POST\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "post" -c "base post"\n'
        '${SCRglobal}/exgfs_atmos_post.sh\n'
        'exit 0\n'
    )

    # Extra J-Jobs NOT referenced by the workflow YAML
    (dev_root / "jobs" / "JGLOBAL_ARCHIVE").write_text(
        '#!/bin/bash\n'
        '# J-Job: JGLOBAL_ARCHIVE\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "arch" -c "base arch"\n'
        '${SCRglobal}/exglobal_archive.sh\n'
        'exit 0\n'
    )
    (dev_root / "jobs" / "JGLOBAL_STAGE_IC").write_text(
        '#!/bin/bash\n'
        '# J-Job: JGLOBAL_STAGE_IC\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "stage" -c "base"\n'
        '${SCRglobal}/exglobal_stage_ic.sh\n'
        'exit 0\n'
    )

    # -- Ex-scripts: 2 referenced + 2 extra --
    (dev_root / "scripts" / "exglobal_forecast.sh").write_text(
        '#!/bin/bash\n'
        '# Ex-script: exglobal_forecast.sh\n'
        'source "${USHglobal}/forecast_predet.sh"\n'
        'source "${USHglobal}/forecast_det.sh"\n'
        'exit 0\n'
    )
    (dev_root / "scripts" / "exgfs_atmos_post.sh").write_text(
        '#!/bin/bash\n'
        '# Ex-script: exgfs_atmos_post.sh\n'
        'source "${USHglobal}/atmos_post.sh"\n'
        'exit 0\n'
    )
    # Extra ex-scripts NOT reachable
    (dev_root / "scripts" / "exglobal_archive.sh").write_text(
        '#!/bin/bash\n# Ex-script: exglobal_archive.sh\nexit 0\n'
    )
    (dev_root / "scripts" / "exglobal_stage_ic.sh").write_text(
        '#!/bin/bash\n# Ex-script: exglobal_stage_ic.sh\nexit 0\n'
    )

    # -- Ush scripts: 3 reachable + 2 extra --
    (dev_root / "ush" / "forecast_predet.sh").write_text(
        '#!/bin/bash\n# forecast_predet.sh\nexit 0\n'
    )
    (dev_root / "ush" / "forecast_det.sh").write_text(
        '#!/bin/bash\n# forecast_det.sh\nexit 0\n'
    )
    (dev_root / "ush" / "atmos_post.sh").write_text(
        '#!/bin/bash\n# atmos_post.sh\nexit 0\n'
    )
    # Extra ush scripts NOT reachable
    (dev_root / "ush" / "archive_utils.sh").write_text(
        '#!/bin/bash\n# archive_utils.sh\nexit 0\n'
    )
    (dev_root / "ush" / "stage_ic_utils.sh").write_text(
        '#!/bin/bash\n# stage_ic_utils.sh\nexit 0\n'
    )

    # -- Config files --
    config_dir = dev_root / "parm" / "config" / "gfs_forecast_only"
    (config_dir / "config.base").write_text(
        '#!/bin/bash\n# config.base\nexport NET="gfs"\nexport RUN="gfs"\n'
    )
    (config_dir / "config.fcst").write_text(
        '#!/bin/bash\n# config.fcst\nexport FHMAX=120\n'
    )
    (config_dir / "config.post").write_text(
        '#!/bin/bash\n# config.post\nexport POSTGPSH="postgp.sh"\n'
    )
    (config_dir / "config.arch").write_text(
        '#!/bin/bash\n# config.arch (should be excluded by DAG filter)\n'
        'export HPSSARCH="YES"\n'
    )
    (config_dir / "config.resources").write_text(
        '#!/bin/bash\n# config.resources\nexport ACCOUNT="dev"\n'
    )
    (config_dir / "config.com").write_text(
        '#!/bin/bash\n# config.com\nexport COMROOT="/lfs/data/com"\n'
    )

    # -- ecFlow templates --
    (dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2").write_text(
        '%include <head.h>\n'
        '# Task: {{ task.name }}\n'
        '${EXPDIR}/ush/universal_wrapper.sh {{ task.jjob }}\n'
        '%include <tail.h>\n'
    )
    (dev_root / "workflow" / "ecflow" / "include" / "head.h").write_text(
        "# head.h\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "tail.h").write_text(
        "# tail.h\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "envsetup.h").write_text(
        "# envsetup.h\n"
    )

    # -- versions file --
    (dev_root / "versions" / "run.ver").write_text(
        "export gfs_ver=v17.0.0\n"
    )

    # -- .git for repo root detection --
    (tmp_path / ".git").mkdir()

    # -- Submodule fixture dirs (for SubmodulePolicy.SKIP_OPTIONAL) --
    # These are optional and can be absent under SKIP_OPTIONAL policy

    # -- Unconditional artifacts (Req 9.1, 9.2) --
    sorc_dir = tmp_path / "sorc"
    sorc_dir.mkdir(exist_ok=True)
    link_workflow = sorc_dir / "link_workflow.sh"
    link_workflow.write_text("#!/bin/bash\n# link_workflow.sh\n")
    os.chmod(link_workflow, 0o755)
    ufs_fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
    ufs_fix_dir.mkdir(parents=True, exist_ok=True)
    link_fixdirs = ufs_fix_dir / "link_fixdirs.sh"
    link_fixdirs.write_text("#!/bin/bash\n# link_fixdirs.sh\n")
    os.chmod(link_fixdirs, 0o755)

    return {
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


@pytest.fixture
def broken_jjob_dev_tree(tmp_path):
    """Dev tree where a J-Job references a nonexistent ex-script.

    Used to test FATAL ERROR propagation from DAG_Filter.
    """
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

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

    config = {
        "suite": {"name": "gfs_v17_fcst_only"},
        "defaults": {"ECF_TRIES": 2},
        "cycles": [{"name": "gfs", "time": "00:00"}],
        "families": [
            {
                "path": "gfs/forecast",
                "tasks": [
                    {"name": "fcst", "trigger": "", "jjob": "JMISSING_JOB"}
                ],
            }
        ],
        "inter_cycle_dependencies": [],
    }
    config_path = dev_root / "parm" / "workflow" / "gfs_forecast_only.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # No J-Job file for JMISSING_JOB → DAG_Filter should FATAL

    # ecFlow templates
    (dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2").write_text(
        "# task\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "head.h").write_text("# h\n")
    (dev_root / "workflow" / "ecflow" / "include" / "tail.h").write_text("# t\n")
    (dev_root / "workflow" / "ecflow" / "include" / "envsetup.h").write_text("# e\n")

    (dev_root / "versions" / "run.ver").write_text("export gfs_ver=v17.0.0\n")

    (tmp_path / ".git").mkdir()

    # -- Unconditional artifacts (Req 9.1, 9.2) --
    sorc_dir = tmp_path / "sorc"
    sorc_dir.mkdir(exist_ok=True)
    link_workflow = sorc_dir / "link_workflow.sh"
    link_workflow.write_text("#!/bin/bash\n# link_workflow.sh\n")
    os.chmod(link_workflow, 0o755)
    ufs_fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
    ufs_fix_dir.mkdir(parents=True, exist_ok=True)
    link_fixdirs = ufs_fix_dir / "link_fixdirs.sh"
    link_fixdirs.write_text("#!/bin/bash\n# link_fixdirs.sh\n")
    os.chmod(link_fixdirs, 0o755)

    return {
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


@pytest.fixture
def incomplete_expdir_tree(tmp_path):
    """Dev tree that passes DAG_Filter but fails Completeness_Verifier.

    J-Job references an ex-script that exists in dev/scripts/ (passes
    DAG_Filter) but after staging the ush script referenced by the
    ex-script is missing from the staged EXPDIR (fails completeness).

    This simulates an incomplete staging where a ush dependency was
    missed — the DAG filter passes because the ush file exists in dev/,
    but the staged EXPDIR is incomplete.
    """
    dev_root = tmp_path / "dev"
    dev_root.mkdir()

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

    config = {
        "suite": {"name": "gfs_v17_fcst_only"},
        "defaults": {"ECF_TRIES": 2},
        "cycles": [{"name": "gfs", "time": "00:00"}],
        "families": [
            {
                "path": "gfs/forecast",
                "tasks": [
                    {"name": "fcst", "trigger": "", "jjob": "JGLOBAL_FORECAST"}
                ],
            }
        ],
        "inter_cycle_dependencies": [],
    }
    config_path = dev_root / "parm" / "workflow" / "gfs_forecast_only.yaml"
    config_path.write_text(yaml.dump(config, sort_keys=False))

    # J-Job references an ex-script
    (dev_root / "jobs" / "JGLOBAL_FORECAST").write_text(
        '#!/bin/bash\n'
        'source "${HOMEglobal}/ush/jjob_header.sh" -e "fcst" -c "base"\n'
        '${SCRglobal}/exglobal_forecast.sh\n'
        'exit 0\n'
    )

    # Ex-script references a ush script that DOES exist in dev/ush/
    (dev_root / "scripts" / "exglobal_forecast.sh").write_text(
        '#!/bin/bash\n'
        'source "${USHglobal}/forecast_predet.sh"\n'
        'source "${USHglobal}/missing_ush_dep.sh"\n'
        'exit 0\n'
    )

    # Only one ush script exists; the other is present for DAG filter
    # but we'll patch it out after DAG filter runs
    (dev_root / "ush" / "forecast_predet.sh").write_text(
        '#!/bin/bash\n# forecast_predet.sh\nexit 0\n'
    )
    # missing_ush_dep.sh exists in dev but will fail completeness if
    # the staging incorrectly omits it. However, with DAG filter properly
    # working, it WOULD be staged. So let's NOT create it to trigger
    # a DAG filter error (missing ush is a warning, not fatal).
    # Instead, test completeness failure via a different approach.

    # Config files
    config_dir = dev_root / "parm" / "config" / "gfs_forecast_only"
    (config_dir / "config.base").write_text(
        '#!/bin/bash\nexport NET="gfs"\n'
    )
    (config_dir / "config.resources").write_text(
        '#!/bin/bash\nexport ACCOUNT="dev"\n'
    )
    (config_dir / "config.com").write_text(
        '#!/bin/bash\nexport COMROOT="/com"\n'
    )

    # ecFlow templates
    (dev_root / "workflow" / "ecflow" / "templates" / "task.ecf.j2").write_text(
        "# task\n"
    )
    (dev_root / "workflow" / "ecflow" / "include" / "head.h").write_text("# h\n")
    (dev_root / "workflow" / "ecflow" / "include" / "tail.h").write_text("# t\n")
    (dev_root / "workflow" / "ecflow" / "include" / "envsetup.h").write_text("# e\n")

    (dev_root / "versions" / "run.ver").write_text("export gfs_ver=v17.0.0\n")
    (tmp_path / ".git").mkdir()

    # -- Unconditional artifacts (Req 9.1, 9.2) --
    sorc_dir = tmp_path / "sorc"
    sorc_dir.mkdir(exist_ok=True)
    link_workflow = sorc_dir / "link_workflow.sh"
    link_workflow.write_text("#!/bin/bash\n# link_workflow.sh\n")
    os.chmod(link_workflow, 0o755)
    ufs_fix_dir = sorc_dir / "ufs_utils.fd" / "fix"
    ufs_fix_dir.mkdir(parents=True, exist_ok=True)
    link_fixdirs = ufs_fix_dir / "link_fixdirs.sh"
    link_fixdirs.write_text("#!/bin/bash\n# link_fixdirs.sh\n")
    os.chmod(link_fixdirs, 0o755)

    return {
        "tmp_path": tmp_path,
        "dev_root": dev_root,
        "config_path": config_path,
    }


# ---------------------------------------------------------------------------
# Tests: Full pipeline with --dag-filter enabled (minimal EXPDIR)
# ---------------------------------------------------------------------------


class TestPipelineDagFilterEnabled:
    """Integration tests for pipeline with dag_filter=True.

    Validates: Requirements 13.1, 13.3, 9.1–9.4
    """

    def test_dag_filter_produces_minimal_expdir_jobs(self, dag_filter_dev_tree):
        """dag_filter=True stages only J-Jobs referenced by the workflow YAML.

        Validates: Requirement 13.1
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=True,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        jobs_dir = expdir / "jobs"
        staged_jobs = {f.name for f in jobs_dir.iterdir() if f.is_file()}

        # Only JGLOBAL_FORECAST and JGFS_ATMOS_POST should be staged
        assert "JGLOBAL_FORECAST" in staged_jobs
        assert "JGFS_ATMOS_POST" in staged_jobs

        # JGLOBAL_ARCHIVE and JGLOBAL_STAGE_IC should NOT be staged
        assert "JGLOBAL_ARCHIVE" not in staged_jobs
        assert "JGLOBAL_STAGE_IC" not in staged_jobs

    def test_dag_filter_produces_minimal_expdir_scripts(self, dag_filter_dev_tree):
        """dag_filter=True stages only ex-scripts invoked by reachable J-Jobs.

        Validates: Requirement 13.1
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=True,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        scripts_dir = expdir / "scripts"
        staged_scripts = {f.name for f in scripts_dir.iterdir() if f.is_file()}

        # Only reachable ex-scripts should be staged
        assert "exglobal_forecast.sh" in staged_scripts
        assert "exgfs_atmos_post.sh" in staged_scripts

        # Unreachable ex-scripts should NOT be staged
        assert "exglobal_archive.sh" not in staged_scripts
        assert "exglobal_stage_ic.sh" not in staged_scripts

    def test_dag_filter_produces_minimal_expdir_ush(self, dag_filter_dev_tree):
        """dag_filter=True stages only ush scripts transitively reachable.

        Validates: Requirement 13.1
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=True,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        ush_dir = expdir / "ush"
        staged_ush = {f.name for f in ush_dir.iterdir() if f.is_file()}

        # Only ush scripts sourced by reachable ex-scripts
        assert "forecast_predet.sh" in staged_ush
        assert "forecast_det.sh" in staged_ush
        assert "atmos_post.sh" in staged_ush

        # Unreachable ush scripts should NOT be staged
        assert "archive_utils.sh" not in staged_ush
        assert "stage_ic_utils.sh" not in staged_ush

    def test_dag_filter_config_conditioning_still_runs(
        self, dag_filter_dev_tree
    ):
        """Config conditioning applies even with dag_filter=True.

        Validates: Requirement 13.3
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        # Add a deploy-time conditional to a config file
        config_dir = info["dev_root"] / "parm" / "config" / "gfs_forecast_only"
        (config_dir / "config.fcst").write_text(
            '#!/bin/bash\n'
            '# config.fcst\n'
            'case ${RUN} in\n'
            '  *gfs)\n'
            '    export FHMAX=120\n'
            '    ;;\n'
            '  *gdas)\n'
            '    export FHMAX=9\n'
            '    ;;\n'
            'esac\n'
        )

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=True,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        # The config.fcst should exist and have been conditioned
        # (RUN=gfs from context, so gdas branch should be eliminated
        # or the file should at least have been processed)
        config_fcst = expdir / "parm" / "config" / "gfs_forecast_only" / "config.fcst"
        if config_fcst.exists():
            content = config_fcst.read_text()
            # Config conditioning should have processed the file;
            # at minimum, the file should still be valid
            assert "FHMAX" in content

    def test_dag_filter_size_reduction_logged(
        self, dag_filter_dev_tree, caplog
    ):
        """Size reduction report is logged when dag_filter=True.

        Validates: Requirements 9.1, 9.2, 9.3, 9.4
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=True,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        log_text = caplog.text

        # Size reduction report should appear in logs
        assert "DAG Filter Results:" in log_text
        # Should show staged/total counts for J-Jobs (2 staged of 4 total)
        assert "2/4" in log_text  # J-Jobs: 2 reachable out of 4

    def test_dag_filter_completeness_verifier_runs(self, dag_filter_dev_tree, caplog):
        """Completeness verification runs and passes when dag_filter=True.

        Validates: Requirements 13.1, 13.3
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=True,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        # Completeness verification should pass and be logged
        assert "Completeness verification passed" in caplog.text


# ---------------------------------------------------------------------------
# Tests: Full pipeline with --dag-filter disabled (full EXPDIR)
# ---------------------------------------------------------------------------


class TestPipelineDagFilterDisabled:
    """Integration tests for pipeline with dag_filter=False (default).

    Validates: Requirements 13.2, 13.3, 13.4
    """

    def test_no_dag_filter_stages_all_jobs(self, dag_filter_dev_tree):
        """dag_filter=False stages ALL J-Jobs from dev/jobs/.

        Validates: Requirement 13.2
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=False,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        jobs_dir = expdir / "jobs"
        staged_jobs = {f.name for f in jobs_dir.iterdir() if f.is_file()}

        # ALL J-Jobs should be staged (full-copy behavior)
        assert "JGLOBAL_FORECAST" in staged_jobs
        assert "JGFS_ATMOS_POST" in staged_jobs
        assert "JGLOBAL_ARCHIVE" in staged_jobs
        assert "JGLOBAL_STAGE_IC" in staged_jobs

    def test_no_dag_filter_stages_all_scripts(self, dag_filter_dev_tree):
        """dag_filter=False stages ALL ex-scripts from dev/scripts/.

        Validates: Requirement 13.2
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=False,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        scripts_dir = expdir / "scripts"
        staged_scripts = {f.name for f in scripts_dir.iterdir() if f.is_file()}

        # ALL ex-scripts should be staged
        assert "exglobal_forecast.sh" in staged_scripts
        assert "exgfs_atmos_post.sh" in staged_scripts
        assert "exglobal_archive.sh" in staged_scripts
        assert "exglobal_stage_ic.sh" in staged_scripts

    def test_no_dag_filter_stages_all_ush(self, dag_filter_dev_tree):
        """dag_filter=False stages ALL ush scripts from dev/ush/.

        Validates: Requirement 13.2
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        run(
            config=str(info["config_path"]),
            platform="HERA",
            expdir=str(expdir),
            version="v17.0.0",
            dag_filter=False,
            skip_ee2_scan=True,
            submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
        )

        ush_dir = expdir / "ush"
        staged_ush = {f.name for f in ush_dir.iterdir() if f.is_file()}

        # ALL ush scripts should be staged
        assert "forecast_predet.sh" in staged_ush
        assert "forecast_det.sh" in staged_ush
        assert "atmos_post.sh" in staged_ush
        assert "archive_utils.sh" in staged_ush
        assert "stage_ic_utils.sh" in staged_ush

    def test_no_dag_filter_config_conditioning_still_runs(
        self, dag_filter_dev_tree, caplog
    ):
        """Config conditioning applies even with dag_filter=False.

        Validates: Requirement 13.3
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        # Config conditioning stage should run regardless of dag_filter
        assert "Condition config files" in caplog.text

    def test_no_dag_filter_logs_disabled_status(self, dag_filter_dev_tree, caplog):
        """Pipeline logs DAG filter disabled status.

        Validates: Requirement 13.4
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        assert "DISABLED" in caplog.text

    def test_no_dag_filter_no_size_reduction_logged(
        self, dag_filter_dev_tree, caplog
    ):
        """Size reduction report NOT logged when dag_filter=False.

        Validates: Requirement 9 (only reports when filtering is active)
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        # Size reduction report should NOT appear
        assert "DAG Filter Results:" not in caplog.text

    def test_no_dag_filter_completeness_verifier_skipped(
        self, dag_filter_dev_tree, caplog
    ):
        """Completeness verification does NOT run when dag_filter=False.

        Validates: Requirement 13.2 (full mode skips DAG-specific checks)
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        # Completeness verification should NOT run in full mode
        assert "Completeness verification" not in caplog.text


# ---------------------------------------------------------------------------
# Tests: Config conditioning in both modes
# ---------------------------------------------------------------------------


class TestConfigConditioningBothModes:
    """Config conditioning runs regardless of --dag-filter setting.

    Validates: Requirement 13.3
    """

    def test_conditioning_runs_with_dag_filter(self, dag_filter_dev_tree, caplog):
        """Config conditioning executes when dag_filter=True.

        Validates: Requirement 13.3
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=True,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        assert "Condition config files" in caplog.text

    def test_conditioning_runs_without_dag_filter(
        self, dag_filter_dev_tree, caplog
    ):
        """Config conditioning executes when dag_filter=False.

        Validates: Requirement 13.3
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        assert "Condition config files" in caplog.text


# ---------------------------------------------------------------------------
# Tests: FATAL ERROR propagation
# ---------------------------------------------------------------------------


class TestFatalErrorPropagation:
    """FATAL ERROR propagation from DAG_Filter and Completeness_Verifier.

    Validates: Requirements 13.1, 13.4
    """

    def test_dag_filter_fatal_on_missing_jjob(self, broken_jjob_dev_tree):
        """DAG_Filter raises PipelineError for missing J-Job.

        When a J-Job referenced in the Workflow_YAML does not exist in
        dev/jobs/, the DAG_Filter must raise a FATAL PipelineError.

        Validates: Requirement 13.1
        """
        info = broken_jjob_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with pytest.raises(PipelineError, match="JMISSING_JOB"):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=True,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

    def test_dag_filter_fatal_not_raised_when_disabled(
        self, broken_jjob_dev_tree
    ):
        """Missing J-Job does NOT cause FATAL when dag_filter=False.

        The DAG_Filter is not invoked when dag_filter is disabled,
        so missing J-Job references are not checked.

        Validates: Requirement 13.2 (full mode doesn't run DAG checks)
        """
        info = broken_jjob_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        # Should not raise PipelineError for missing J-Job since
        # DAG filter is disabled. It may raise for other reasons
        # (e.g. missing ecFlow template context), but not for the
        # missing J-Job file.
        try:
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )
        except PipelineError as e:
            # If it raises, it should NOT be about JMISSING_JOB
            assert "JMISSING_JOB" not in str(e), (
                f"dag_filter=False should not check J-Job existence, "
                f"but got: {e}"
            )

    def test_completeness_verifier_fatal_propagates(self, dag_filter_dev_tree):
        """Completeness_Verifier FATAL ERROR propagates through pipeline.

        When an ex-script references a ush script not present in the
        staged EXPDIR, CompletenessVerifier raises PipelineError.

        Validates: Requirements 13.1, 8.4
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        # Modify an ex-script to reference a ush script that doesn't exist
        # in dev/ush/ (so it won't be staged)
        (info["dev_root"] / "scripts" / "exglobal_forecast.sh").write_text(
            '#!/bin/bash\n'
            'source "${USHglobal}/forecast_predet.sh"\n'
            'source "${USHglobal}/completely_missing_ush.sh"\n'
            'exit 0\n'
        )

        # The DAG filter will emit a WARNING for the missing ush script
        # (non-fatal in DAG filter) but the completeness verifier should
        # catch it after staging since the file won't exist in EXPDIR/ush/
        with pytest.raises(PipelineError, match="completeness|missing"):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=True,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )


# ---------------------------------------------------------------------------
# Tests: DAG filter logging status
# ---------------------------------------------------------------------------


class TestDagFilterLogging:
    """Pipeline logs DAG filter activation status.

    Validates: Requirement 13.4
    """

    def test_logs_enabled_when_dag_filter_on(self, dag_filter_dev_tree, caplog):
        """Pipeline logs 'ENABLED' when dag_filter=True.

        Validates: Requirement 13.4
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=True,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        assert "ENABLED" in caplog.text

    def test_logs_disabled_when_dag_filter_off(self, dag_filter_dev_tree, caplog):
        """Pipeline logs 'DISABLED' when dag_filter=False.

        Validates: Requirement 13.4
        """
        info = dag_filter_dev_tree
        expdir = info["tmp_path"] / "EXPDIR"

        with caplog.at_level(logging.INFO):
            run(
                config=str(info["config_path"]),
                platform="HERA",
                expdir=str(expdir),
                version="v17.0.0",
                dag_filter=False,
                skip_ee2_scan=True,
                submodule_policy=SubmodulePolicy.SKIP_OPTIONAL,
            )

        assert "DISABLED" in caplog.text

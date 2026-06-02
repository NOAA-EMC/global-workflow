"""Unit tests for generate_ecf_scripts function.

Tests the per-task .ecf script generation including:
- Correct file output paths (<output_dir>/<family_path>/<task_name>.ecf)
- Platform-specific scheduler directives (PBS for WCOSS2, Slurm for others)
- Template rendering with task context (name, family_path, jjob)
- Handling of multiple tasks across different families
- Error handling for missing templates

Traces to: Requirements 1.2, 12.5
"""

import os
import sys
import tempfile

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_generator import generate_ecf_scripts
from deployment.workflow_config import DAG, TaskNode


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Path to the actual task.ecf.j2 template
_TEMPLATE_PATH = os.path.join(
    os.path.dirname(__file__),
    "..",
    "ecflow",
    "templates",
    "task.ecf.j2",
)


def _make_simple_dag() -> DAG:
    """Create a simple DAG with tasks in different families."""
    dag = DAG(suite_name="test_suite")
    dag.nodes["gdas/atmos/prep/prep"] = TaskNode(
        name="prep",
        family_path="gdas/atmos/prep",
        jjob="JGDAS_ATMOS_PREP",
    )
    dag.nodes["gdas/atmos/analysis/anal"] = TaskNode(
        name="anal",
        family_path="gdas/atmos/analysis",
        jjob="JGDAS_ATMOS_ANALYSIS",
    )
    dag.nodes["gdas/atmos/post/post_f000"] = TaskNode(
        name="post_f000",
        family_path="gdas/atmos/post",
        jjob="JGDAS_ATMOS_POST",
    )
    return dag


def _make_single_task_dag() -> DAG:
    """Create a DAG with a single task for focused testing."""
    dag = DAG(suite_name="single")
    dag.nodes["app/run/task1"] = TaskNode(
        name="task1",
        family_path="app/run",
        jjob="JAPP_TASK1",
    )
    return dag


# ---------------------------------------------------------------------------
# Tests: File output paths
# ---------------------------------------------------------------------------


class TestEcfScriptPaths:
    """Tests that .ecf files are written to the correct paths."""

    def test_single_task_output_path(self):
        """Verify a single task produces the correct output file path."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            expected_path = os.path.join(output_dir, "app", "run", "task1.ecf")
            assert len(result) == 1
            assert str(result[0]) == expected_path
            assert os.path.exists(expected_path)

    def test_multiple_tasks_output_paths(self):
        """Verify multiple tasks produce files in correct family directories."""
        dag = _make_simple_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            assert len(result) == 3

            # Check each expected path exists
            expected_paths = [
                os.path.join(output_dir, "gdas", "atmos", "prep", "prep.ecf"),
                os.path.join(output_dir, "gdas", "atmos", "analysis", "anal.ecf"),
                os.path.join(output_dir, "gdas", "atmos", "post", "post_f000.ecf"),
            ]
            for expected in expected_paths:
                assert os.path.exists(expected), f"Missing: {expected}"

    def test_creates_nested_directories(self):
        """Verify that deeply nested family paths create the directory tree."""
        dag = DAG(suite_name="deep")
        dag.nodes["a/b/c/d/task"] = TaskNode(
            name="task",
            family_path="a/b/c/d",
            jjob="JTASK",
        )
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            expected = os.path.join(output_dir, "a", "b", "c", "d", "task.ecf")
            assert os.path.exists(expected)


# ---------------------------------------------------------------------------
# Tests: Platform-specific scheduler directives
# ---------------------------------------------------------------------------


class TestSchedulerDirectives:
    """Tests that platform-specific scheduler directives are correct."""

    def test_wcoss2_uses_pbs_directives(self):
        """WCOSS2 platform should produce PBS directives."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "WCOSS2")

            content = result[0].read_text()
            assert "#PBS -N task1" in content
            assert "#PBS -j oe" in content
            assert "#PBS -q %ECF_JOB_QUEUE%" in content
            assert "#PBS -A %ACCOUNT%" in content
            assert "#PBS -l walltime=%WALLTIME%" in content
            assert "#PBS -l select=%SELECT%" in content
            # Should NOT have Slurm directives
            assert "#SBATCH" not in content

    def test_hera_uses_slurm_directives(self):
        """HERA platform should produce Slurm directives."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            content = result[0].read_text()
            assert "#SBATCH --job-name=task1" in content
            assert "#SBATCH --output=%ECF_JOBOUT%" in content
            assert "#SBATCH --account=%ACCOUNT%" in content
            assert "#SBATCH --qos=%QOS%" in content
            assert "#SBATCH --time=%WALLTIME%" in content
            assert "#SBATCH --nodes=%NODES%" in content
            assert "#SBATCH --ntasks=%NTASKS%" in content
            # Should NOT have PBS directives
            assert "#PBS" not in content

    def test_hercules_uses_slurm_directives(self):
        """HERCULES platform should produce Slurm directives."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERCULES")

            content = result[0].read_text()
            assert "#SBATCH --job-name=task1" in content
            assert "#PBS" not in content

    def test_orion_uses_slurm_directives(self):
        """ORION platform should produce Slurm directives."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "ORION")

            content = result[0].read_text()
            assert "#SBATCH --job-name=task1" in content
            assert "#PBS" not in content

    def test_wcoss2_case_insensitive(self):
        """Platform matching for WCOSS2 should be case-insensitive."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "wcoss2")

            content = result[0].read_text()
            assert "#PBS -N task1" in content


# ---------------------------------------------------------------------------
# Tests: Template rendering with task context
# ---------------------------------------------------------------------------


class TestTemplateRendering:
    """Tests that the Jinja2 template is rendered with correct task context."""

    def test_task_name_rendered(self):
        """Task name should appear in the rendered .ecf content."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            content = result[0].read_text()
            assert "Task: task1" in content

    def test_task_family_path_rendered(self):
        """Task family_path should appear in the rendered .ecf content."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            content = result[0].read_text()
            assert "Family: app/run" in content

    def test_task_jjob_rendered(self):
        """Task jjob should appear in the rendered .ecf content."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            content = result[0].read_text()
            assert "JAPP_TASK1" in content

    def test_universal_wrapper_invocation(self):
        """The rendered script should invoke universal_wrapper.sh with the jjob."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            content = result[0].read_text()
            assert "${EXPDIR}/ush/universal_wrapper.sh JAPP_TASK1" in content

    def test_ecflow_includes_preserved(self):
        """ecFlow %include directives should be preserved (not resolved by Jinja2)."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            content = result[0].read_text()
            assert "%include <head.h>" in content
            assert "%include <tail.h>" in content

    def test_each_task_gets_unique_content(self):
        """Each task should have its own name/jjob rendered in its .ecf file."""
        dag = _make_simple_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            # Check prep task
            prep_path = os.path.join(
                output_dir, "gdas", "atmos", "prep", "prep.ecf"
            )
            prep_content = open(prep_path).read()
            assert "Task: prep" in prep_content
            assert "JGDAS_ATMOS_PREP" in prep_content

            # Check anal task
            anal_path = os.path.join(
                output_dir, "gdas", "atmos", "analysis", "anal.ecf"
            )
            anal_content = open(anal_path).read()
            assert "Task: anal" in anal_content
            assert "JGDAS_ATMOS_ANALYSIS" in anal_content


# ---------------------------------------------------------------------------
# Tests: Error handling
# ---------------------------------------------------------------------------


class TestErrorHandling:
    """Tests for error conditions."""

    def test_missing_template_raises_error(self):
        """Should raise FileNotFoundError if template doesn't exist."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            with pytest.raises(FileNotFoundError):
                generate_ecf_scripts(
                    dag, output_dir, "/nonexistent/template.ecf.j2", "HERA"
                )

    def test_empty_dag_produces_no_files(self):
        """An empty DAG should produce no .ecf files."""
        dag = DAG(suite_name="empty")
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")
            assert result == []


# ---------------------------------------------------------------------------
# Tests: Integration with full pipeline
# ---------------------------------------------------------------------------


class TestIntegration:
    """Integration tests using the actual template and parsed DAGs."""

    def test_with_parsed_forecast_only_dag(self):
        """Generate ecf scripts from the gfs_forecast_only.yaml config."""
        from deployment.workflow_config import parse

        sample_dir = os.path.join(
            os.path.dirname(__file__), "..", "..", "parm", "workflow"
        )
        path = os.path.join(sample_dir, "gfs_forecast_only.yaml")
        dag = parse(path)

        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = os.path.join(tmpdir, "ecf", "scripts")
            result = generate_ecf_scripts(dag, output_dir, _TEMPLATE_PATH, "HERA")

            # Should generate one .ecf per task in the DAG
            assert len(result) == len(dag.nodes)

            # Each file should exist and contain valid content
            for ecf_path in result:
                assert ecf_path.exists()
                content = ecf_path.read_text()
                # Should have Slurm directives (HERA)
                assert "#SBATCH" in content
                # Should have ecFlow includes
                assert "%include <head.h>" in content
                assert "%include <tail.h>" in content
                # Should invoke universal_wrapper
                assert "universal_wrapper.sh" in content

    def test_wcoss2_vs_hera_differ_only_in_directives(self):
        """Same DAG on different platforms should differ only in directives."""
        dag = _make_single_task_dag()
        with tempfile.TemporaryDirectory() as tmpdir:
            hera_dir = os.path.join(tmpdir, "hera", "ecf", "scripts")
            wcoss2_dir = os.path.join(tmpdir, "wcoss2", "ecf", "scripts")

            generate_ecf_scripts(dag, hera_dir, _TEMPLATE_PATH, "HERA")
            generate_ecf_scripts(dag, wcoss2_dir, _TEMPLATE_PATH, "WCOSS2")

            hera_content = open(
                os.path.join(hera_dir, "app", "run", "task1.ecf")
            ).read()
            wcoss2_content = open(
                os.path.join(wcoss2_dir, "app", "run", "task1.ecf")
            ).read()

            # Both should have the same template body
            assert "Task: task1" in hera_content
            assert "Task: task1" in wcoss2_content
            assert "JAPP_TASK1" in hera_content
            assert "JAPP_TASK1" in wcoss2_content

            # But different scheduler directives
            assert "#SBATCH" in hera_content
            assert "#PBS" in wcoss2_content
            assert "#PBS" not in hera_content
            assert "#SBATCH" not in wcoss2_content

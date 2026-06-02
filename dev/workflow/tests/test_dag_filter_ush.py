"""Unit tests for DAGFilter Layer 3 — transitive ush script resolution.

Tests extract_ush_scripts() BFS transitive closure, circular dependency
handling, missing ush script warnings, and _parse_source_refs() helper.

Validates: Requirements 3.1, 3.2, 3.3, 3.4, 3.5
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGFilter, _USH_SOURCE_PATTERNS


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def tmp_dev_root(tmp_path: Path) -> Path:
    """Create a minimal dev/ directory structure for testing."""
    (tmp_path / "scripts").mkdir()
    (tmp_path / "ush").mkdir()
    (tmp_path / "jobs").mkdir()
    return tmp_path


@pytest.fixture
def dag_filter(tmp_dev_root: Path) -> DAGFilter:
    """Create a DAGFilter instance with a minimal workflow YAML."""
    workflow_yaml = {"families": []}
    return DAGFilter(tmp_dev_root, workflow_yaml, "HERA")


# ---------------------------------------------------------------------------
# Tests for _USH_SOURCE_PATTERNS
# ---------------------------------------------------------------------------


class TestUshSourcePatterns:
    """Tests for the _USH_SOURCE_PATTERNS regex list."""

    def test_matches_source_with_quotes(self):
        """Matches: source "${USHglobal}/forecast_predet.sh" """
        line = 'source "${USHglobal}/forecast_predet.sh"'
        matches = []
        for pattern in _USH_SOURCE_PATTERNS:
            m = pattern.search(line)
            if m:
                matches.append(m.group("script"))
        assert "forecast_predet.sh" in matches

    def test_matches_source_without_quotes(self):
        """Matches: source ${USHglobal}/forecast_predet.sh"""
        line = "source ${USHglobal}/forecast_predet.sh"
        matches = []
        for pattern in _USH_SOURCE_PATTERNS:
            m = pattern.search(line)
            if m:
                matches.append(m.group("script"))
        assert "forecast_predet.sh" in matches

    def test_matches_dot_source_with_quotes(self):
        """Matches: . "${USHgfs}/load_modules.sh" """
        line = '. "${USHgfs}/load_modules.sh"'
        matches = []
        for pattern in _USH_SOURCE_PATTERNS:
            m = pattern.search(line)
            if m:
                matches.append(m.group("script"))
        assert "load_modules.sh" in matches

    def test_matches_dot_source_without_quotes(self):
        """Matches: . ${USHglobal}/jjob_header.sh"""
        line = ". ${USHglobal}/jjob_header.sh"
        matches = []
        for pattern in _USH_SOURCE_PATTERNS:
            m = pattern.search(line)
            if m:
                matches.append(m.group("script"))
        assert "jjob_header.sh" in matches

    def test_matches_various_ush_prefixes(self):
        """Matches USHglobal, USHgfs, USHobsproc, etc."""
        lines = [
            'source "${USHglobal}/forecast_predet.sh"',
            'source "${USHgfs}/load_modules.sh"',
            'source "${USHobsproc}/helper.sh"',
        ]
        for line in lines:
            found = False
            for pattern in _USH_SOURCE_PATTERNS:
                if pattern.search(line):
                    found = True
                    break
            assert found, f"Pattern did not match: {line}"

    def test_does_not_match_comment_lines(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Comment lines are skipped by _parse_source_refs."""
        script = tmp_dev_root / "scripts" / "extest.sh"
        script.write_text('# source "${USHglobal}/commented_out.sh"\n')
        refs = dag_filter._parse_source_refs(script)
        assert refs == []

    def test_does_not_match_non_ush_source(self):
        """Does not match: source "${HOMEglobal}/scripts/something.sh" """
        line = 'source "${HOMEglobal}/scripts/something.sh"'
        matches = []
        for pattern in _USH_SOURCE_PATTERNS:
            m = pattern.search(line)
            if m:
                matches.append(m.group("script"))
        assert matches == []


# ---------------------------------------------------------------------------
# Tests for _parse_source_refs
# ---------------------------------------------------------------------------


class TestParseSourceRefs:
    """Tests for DAGFilter._parse_source_refs helper."""

    def test_extracts_multiple_refs(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Extracts multiple ush script references from a single file."""
        script = tmp_dev_root / "scripts" / "exglobal_forecast.sh"
        script.write_text(
            '#!/bin/bash\n'
            'source "${USHglobal}/forecast_predet.sh"\n'
            'source "${USHglobal}/forecast_det.sh"\n'
            'source "${USHglobal}/forecast_postdet.sh"\n'
        )
        refs = dag_filter._parse_source_refs(script)
        assert refs == ["forecast_predet.sh", "forecast_det.sh", "forecast_postdet.sh"]

    def test_skips_comments(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Skips lines that are comments."""
        script = tmp_dev_root / "scripts" / "extest.sh"
        script.write_text(
            '#!/bin/bash\n'
            '# source "${USHglobal}/commented.sh"\n'
            'source "${USHglobal}/active.sh"\n'
            '  # . "${USHglobal}/also_commented.sh"\n'
        )
        refs = dag_filter._parse_source_refs(script)
        assert refs == ["active.sh"]

    def test_handles_nonexistent_file(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Returns empty list for a file that doesn't exist."""
        path = tmp_dev_root / "scripts" / "nonexistent.sh"
        refs = dag_filter._parse_source_refs(path)
        assert refs == []

    def test_handles_empty_file(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Returns empty list for an empty file."""
        script = tmp_dev_root / "scripts" / "empty.sh"
        script.write_text("")
        refs = dag_filter._parse_source_refs(script)
        assert refs == []

    def test_handles_dot_source(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Extracts dot-source references."""
        script = tmp_dev_root / "ush" / "helper.sh"
        script.write_text('. "${USHglobal}/sub_helper.sh"\n')
        refs = dag_filter._parse_source_refs(script)
        assert refs == ["sub_helper.sh"]


# ---------------------------------------------------------------------------
# Tests for extract_ush_scripts
# ---------------------------------------------------------------------------


class TestExtractUshScripts:
    """Tests for DAGFilter.extract_ush_scripts BFS transitive closure."""

    def test_simple_single_level(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Resolves ush scripts directly sourced by an ex-script."""
        # Create ex-script that sources two ush scripts
        ex_script = tmp_dev_root / "scripts" / "exglobal_forecast.sh"
        ex_script.write_text(
            '#!/bin/bash\n'
            'source "${USHglobal}/forecast_predet.sh"\n'
            'source "${USHglobal}/forecast_det.sh"\n'
        )
        # Create the ush scripts (no further dependencies)
        (tmp_dev_root / "ush" / "forecast_predet.sh").write_text("#!/bin/bash\n")
        (tmp_dev_root / "ush" / "forecast_det.sh").write_text("#!/bin/bash\n")

        result = dag_filter.extract_ush_scripts({"exglobal_forecast.sh"})
        assert result == {"forecast_predet.sh", "forecast_det.sh"}

    def test_transitive_resolution(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Resolves transitive ush dependencies (A -> B -> C)."""
        # ex-script sources A
        ex_script = tmp_dev_root / "scripts" / "extest.sh"
        ex_script.write_text('source "${USHglobal}/a.sh"\n')

        # A sources B
        (tmp_dev_root / "ush" / "a.sh").write_text('source "${USHglobal}/b.sh"\n')
        # B sources C
        (tmp_dev_root / "ush" / "b.sh").write_text('source "${USHglobal}/c.sh"\n')
        # C has no further deps
        (tmp_dev_root / "ush" / "c.sh").write_text("#!/bin/bash\n")

        result = dag_filter.extract_ush_scripts({"extest.sh"})
        assert result == {"a.sh", "b.sh", "c.sh"}

    def test_circular_dependency_no_infinite_loop(
        self, dag_filter: DAGFilter, tmp_dev_root: Path
    ):
        """Handles circular dependencies without infinite loop."""
        # ex-script sources A
        ex_script = tmp_dev_root / "scripts" / "extest.sh"
        ex_script.write_text('source "${USHglobal}/a.sh"\n')

        # A sources B, B sources A (circular)
        (tmp_dev_root / "ush" / "a.sh").write_text('source "${USHglobal}/b.sh"\n')
        (tmp_dev_root / "ush" / "b.sh").write_text('source "${USHglobal}/a.sh"\n')

        result = dag_filter.extract_ush_scripts({"extest.sh"})
        assert result == {"a.sh", "b.sh"}
        # Should have a circular dependency warning
        assert any("Circular dependency" in w for w in dag_filter._warnings)

    def test_missing_ush_script_warning(
        self, dag_filter: DAGFilter, tmp_dev_root: Path
    ):
        """Emits WARNING for missing ush scripts (non-fatal)."""
        ex_script = tmp_dev_root / "scripts" / "extest.sh"
        ex_script.write_text('source "${USHglobal}/missing_script.sh"\n')

        result = dag_filter.extract_ush_scripts({"extest.sh"})
        # The missing script is still in the visited set
        assert "missing_script.sh" in result
        # Should have a warning about the missing script
        assert any("missing_script.sh" in w for w in dag_filter._warnings)
        assert any("not found" in w for w in dag_filter._warnings)

    def test_missing_ex_script_skipped(
        self, dag_filter: DAGFilter, tmp_dev_root: Path
    ):
        """Skips ex-scripts that don't exist (handled by Layer 2)."""
        result = dag_filter.extract_ush_scripts({"nonexistent_ex.sh"})
        assert result == set()

    def test_multiple_ex_scripts(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Resolves ush scripts from multiple ex-scripts."""
        # Two ex-scripts sourcing different ush scripts
        (tmp_dev_root / "scripts" / "ex_a.sh").write_text(
            'source "${USHglobal}/helper_a.sh"\n'
        )
        (tmp_dev_root / "scripts" / "ex_b.sh").write_text(
            'source "${USHglobal}/helper_b.sh"\n'
        )
        (tmp_dev_root / "ush" / "helper_a.sh").write_text("#!/bin/bash\n")
        (tmp_dev_root / "ush" / "helper_b.sh").write_text("#!/bin/bash\n")

        result = dag_filter.extract_ush_scripts({"ex_a.sh", "ex_b.sh"})
        assert result == {"helper_a.sh", "helper_b.sh"}

    def test_shared_dependency_counted_once(
        self, dag_filter: DAGFilter, tmp_dev_root: Path
    ):
        """Shared dependencies are only counted once in the result."""
        # Two ex-scripts both source the same ush script
        (tmp_dev_root / "scripts" / "ex_a.sh").write_text(
            'source "${USHglobal}/shared.sh"\n'
        )
        (tmp_dev_root / "scripts" / "ex_b.sh").write_text(
            'source "${USHglobal}/shared.sh"\n'
        )
        (tmp_dev_root / "ush" / "shared.sh").write_text("#!/bin/bash\n")

        result = dag_filter.extract_ush_scripts({"ex_a.sh", "ex_b.sh"})
        assert result == {"shared.sh"}

    def test_diamond_dependency(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Handles diamond dependencies (A->B, A->C, B->D, C->D)."""
        ex_script = tmp_dev_root / "scripts" / "extest.sh"
        ex_script.write_text(
            'source "${USHglobal}/b.sh"\n'
            'source "${USHglobal}/c.sh"\n'
        )
        (tmp_dev_root / "ush" / "b.sh").write_text('source "${USHglobal}/d.sh"\n')
        (tmp_dev_root / "ush" / "c.sh").write_text('source "${USHglobal}/d.sh"\n')
        (tmp_dev_root / "ush" / "d.sh").write_text("#!/bin/bash\n")

        result = dag_filter.extract_ush_scripts({"extest.sh"})
        assert result == {"b.sh", "c.sh", "d.sh"}

    def test_empty_ex_scripts_set(self, dag_filter: DAGFilter):
        """Returns empty set when no ex-scripts are provided."""
        result = dag_filter.extract_ush_scripts(set())
        assert result == set()

    def test_deep_transitive_chain(self, dag_filter: DAGFilter, tmp_dev_root: Path):
        """Resolves a deep chain of transitive dependencies."""
        ex_script = tmp_dev_root / "scripts" / "extest.sh"
        ex_script.write_text('source "${USHglobal}/level1.sh"\n')

        # Create a chain: level1 -> level2 -> level3 -> level4
        for i in range(1, 4):
            (tmp_dev_root / "ush" / f"level{i}.sh").write_text(
                f'source "${{USHglobal}}/level{i+1}.sh"\n'
            )
        (tmp_dev_root / "ush" / "level4.sh").write_text("#!/bin/bash\n")

        result = dag_filter.extract_ush_scripts({"extest.sh"})
        assert result == {"level1.sh", "level2.sh", "level3.sh", "level4.sh"}

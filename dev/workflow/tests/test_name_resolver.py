"""Unit tests for the NameResolver and ResolvedName classes.

Tests the 5-step resolution algorithm:
1. Direct check (pass-through)
2. Prefix identification (longest match)
3. Ordered search through shared prefixes
4. Direct fallback
5. FATAL error

Traces to: Requirements 2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 8.1, 8.2
"""

from __future__ import annotations

import re

import pytest
import yaml
from pathlib import Path

from deployment.name_resolver import NameResolver, PrefixRegistry, ResolvedName
from deployment.name_resolver import DryRunReport
from deployment.pipeline import PipelineError


@pytest.fixture
def tmp_dev_root(tmp_path: Path) -> Path:
    """Create a temporary dev/ directory with a jobs/ subdirectory."""
    jobs_dir = tmp_path / "jobs"
    jobs_dir.mkdir()
    return tmp_path


@pytest.fixture
def default_registry() -> PrefixRegistry:
    """Return the default PrefixRegistry."""
    return PrefixRegistry.default()


class TestResolvedName:
    """Tests for the ResolvedName frozen dataclass."""

    def test_fields(self):
        rn = ResolvedName(
            application_name="JGCAFS_FORECAST",
            source_name="JGLOBAL_FORECAST",
            is_passthrough=False,
        )
        assert rn.application_name == "JGCAFS_FORECAST"
        assert rn.source_name == "JGLOBAL_FORECAST"
        assert rn.is_passthrough is False

    def test_frozen(self):
        rn = ResolvedName(
            application_name="JGCAFS_FORECAST",
            source_name="JGLOBAL_FORECAST",
            is_passthrough=False,
        )
        with pytest.raises(AttributeError):
            rn.application_name = "SOMETHING_ELSE"  # type: ignore[misc]

    def test_passthrough(self):
        rn = ResolvedName(
            application_name="JGLOBAL_FORECAST",
            source_name="JGLOBAL_FORECAST",
            is_passthrough=True,
        )
        assert rn.is_passthrough is True
        assert rn.application_name == rn.source_name

    def test_equality(self):
        rn1 = ResolvedName("JGCAFS_FORECAST", "JGLOBAL_FORECAST", False)
        rn2 = ResolvedName("JGCAFS_FORECAST", "JGLOBAL_FORECAST", False)
        assert rn1 == rn2


class TestNameResolverInit:
    """Tests for NameResolver initialization."""

    def test_init(self, tmp_dev_root: Path, default_registry: PrefixRegistry):
        resolver = NameResolver(tmp_dev_root, default_registry)
        assert resolver._dev_root == tmp_dev_root
        assert resolver._registry == default_registry
        assert resolver._jobs_dir == tmp_dev_root / "jobs"


class TestNameResolverDirectCheck:
    """Step 1: Direct check — file exists directly in dev/jobs/."""

    def test_direct_match_passthrough(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """A file that exists directly should be a pass-through."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGLOBAL_FORECAST")

        assert result.application_name == "JGLOBAL_FORECAST"
        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.is_passthrough is True

    def test_direct_match_application_name_exists(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """An application-named file that exists directly is pass-through."""
        (tmp_dev_root / "jobs" / "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX")

        assert result.application_name == "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"
        assert result.source_name == "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"
        assert result.is_passthrough is True


class TestNameResolverPrefixResolution:
    """Steps 2-3: Prefix identification and ordered search."""

    def test_gcafs_resolves_to_global(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """JGCAFS_FORECAST should resolve to JGLOBAL_FORECAST."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGCAFS_FORECAST")

        assert result.application_name == "JGCAFS_FORECAST"
        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.is_passthrough is False

    def test_gcdas_resolves_to_global_first(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """JGCDAS_ searches JGLOBAL_ first, then JGDAS_."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        (tmp_dev_root / "jobs" / "JGDAS_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGCDAS_FORECAST")

        # Should pick JGLOBAL_ first since it's first in the search list
        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.is_passthrough is False

    def test_gcdas_falls_back_to_gdas(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """JGCDAS_ falls back to JGDAS_ when JGLOBAL_ doesn't exist."""
        (tmp_dev_root / "jobs" / "JGDAS_AERO_ANALYSIS_INITIALIZE").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGCDAS_AERO_ANALYSIS_INITIALIZE")

        assert result.source_name == "JGDAS_AERO_ANALYSIS_INITIALIZE"
        assert result.is_passthrough is False

    def test_gfs_resolves_to_global_first(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """JGFS_ searches JGLOBAL_ first, then JGFS_."""
        (tmp_dev_root / "jobs" / "JGLOBAL_STAGE_IC").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGFS_STAGE_IC")

        assert result.source_name == "JGLOBAL_STAGE_IC"
        assert result.is_passthrough is False

    def test_gefs_resolves_to_global_first(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """JGEFS_ searches JGLOBAL_ first."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGEFS_FORECAST")

        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.is_passthrough is False

    def test_longest_prefix_match(self, tmp_dev_root: Path):
        """Longest prefix should be matched to avoid false matches.

        For example, JGDAS_ should not match JGDAS_X when JGDAS_ prefix
        is registered (and not confuse with shorter prefixes).
        """
        # Create a registry with both JGDAS_ and JGDA_ to test longest match
        registry = PrefixRegistry(registry={
            "JGD_": ["JGLOBAL_"],
            "JGDAS_": ["JGLOBAL_", "JGDAS_"],
        })
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, registry)

        result = resolver.resolve("JGDAS_FORECAST")

        # Should match JGDAS_ (longer) not JGD_ (shorter)
        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.application_name == "JGDAS_FORECAST"


class TestNameResolverDirectFallback:
    """Step 4: Direct fallback — application_name exists (but wasn't found in step 1)."""

    def test_direct_fallback_after_prefix_search_fails(
        self, tmp_dev_root: Path,
    ):
        """When shared prefix search fails but direct name exists -> pass-through.

        Note: In practice, step 1 catches this. This tests the fallback
        logic for completeness (step 4 in algorithm).
        """
        # Use a custom registry where the search prefix won't find anything
        registry = PrefixRegistry(registry={
            "JTEST_": ["JSHARED_"],
        })
        # Don't create JSHARED_FOO, but do create JTEST_FOO
        (tmp_dev_root / "jobs" / "JTEST_FOO").touch()
        resolver = NameResolver(tmp_dev_root, registry)

        # Step 1 finds JTEST_FOO directly → pass-through
        result = resolver.resolve("JTEST_FOO")
        assert result.is_passthrough is True
        assert result.source_name == "JTEST_FOO"


class TestNameResolverFatalError:
    """Step 5: FATAL error when no source file can be found."""

    def test_unknown_prefix_raises(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """An unknown prefix should raise PipelineError."""
        resolver = NameResolver(tmp_dev_root, default_registry)

        with pytest.raises(PipelineError) as exc_info:
            resolver.resolve("JUNKNOWN_FORECAST")

        assert "Unknown prefix" in str(exc_info.value)
        assert "JUNKNOWN_FORECAST" in str(exc_info.value)

    def test_no_source_found_raises(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """A known prefix but no matching source raises PipelineError."""
        resolver = NameResolver(tmp_dev_root, default_registry)

        with pytest.raises(PipelineError) as exc_info:
            resolver.resolve("JGCAFS_NONEXISTENT")

        assert "Cannot resolve" in str(exc_info.value)
        assert "JGCAFS_NONEXISTENT" in str(exc_info.value)
        assert "JGLOBAL_NONEXISTENT" in str(exc_info.value)

    def test_error_includes_searched_paths(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """FATAL error message lists all searched candidates."""
        resolver = NameResolver(tmp_dev_root, default_registry)

        with pytest.raises(PipelineError) as exc_info:
            resolver.resolve("JGCDAS_MISSING")

        msg = str(exc_info.value)
        assert "JGLOBAL_MISSING" in msg
        assert "JGDAS_MISSING" in msg
        assert "JGCDAS_MISSING" in msg


class TestNameResolverBackwardCompat:
    """Backward compatibility: shared names pass through (Req 8.1, 8.2)."""

    def test_shared_name_passthrough(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """A shared name like JGLOBAL_FORECAST passes through directly."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve("JGLOBAL_FORECAST")

        assert result.is_passthrough is True
        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.application_name == "JGLOBAL_FORECAST"

    def test_mixed_mode_works(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """Both application names and shared names resolve correctly."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        (tmp_dev_root / "jobs" / "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        # Application name resolves via prefix
        result_app = resolver.resolve("JGCAFS_FORECAST")
        assert result_app.source_name == "JGLOBAL_FORECAST"
        assert result_app.is_passthrough is False

        # Shared name passes through
        result_shared = resolver.resolve("JGLOBAL_FORECAST")
        assert result_shared.source_name == "JGLOBAL_FORECAST"
        assert result_shared.is_passthrough is True

        # Direct match source passes through
        result_direct = resolver.resolve("JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX")
        assert result_direct.source_name == "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"
        assert result_direct.is_passthrough is True


class TestResolveAll:
    """Tests for NameResolver.resolve_all() — production mode (fail-fast)."""

    def test_resolve_all_success(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """All names resolvable → returns complete dict."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        (tmp_dev_root / "jobs" / "JGLOBAL_STAGE_IC").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve_all({"JGCAFS_FORECAST", "JGCAFS_STAGE_IC"})

        assert len(result) == 2
        assert result["JGCAFS_FORECAST"].source_name == "JGLOBAL_FORECAST"
        assert result["JGCAFS_STAGE_IC"].source_name == "JGLOBAL_STAGE_IC"

    def test_resolve_all_raises_on_first_failure(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """Raises PipelineError on the first unresolvable name."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        # JGLOBAL_NONEXISTENT does NOT exist
        resolver = NameResolver(tmp_dev_root, default_registry)

        with pytest.raises(PipelineError) as exc_info:
            resolver.resolve_all({"JGCAFS_FORECAST", "JGCAFS_NONEXISTENT"})

        assert "Cannot resolve" in str(exc_info.value)
        assert "JGCAFS_NONEXISTENT" in str(exc_info.value)

    def test_resolve_all_empty_set(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """Empty input → empty dict."""
        resolver = NameResolver(tmp_dev_root, default_registry)
        result = resolver.resolve_all(set())
        assert result == {}

    def test_resolve_all_mixed_passthrough_and_resolved(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """Mix of passthrough and prefix-resolved names."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        (tmp_dev_root / "jobs" / "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        result = resolver.resolve_all({
            "JGCAFS_FORECAST",
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        })

        assert result["JGCAFS_FORECAST"].is_passthrough is False
        assert result["JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"].is_passthrough is True


class TestResolveAllDryRun:
    """Tests for NameResolver.resolve_all_dry_run() — accumulates all errors."""

    def test_dry_run_all_resolvable(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """All resolvable → no errors."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        (tmp_dev_root / "jobs" / "JGLOBAL_STAGE_IC").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({"JGCAFS_FORECAST", "JGCAFS_STAGE_IC"})

        assert report.total_count == 2
        assert report.resolvable_count == 2
        assert report.unresolvable_count == 0
        assert len(report.errors) == 0
        assert "JGCAFS_FORECAST" in report.resolved
        assert "JGCAFS_STAGE_IC" in report.resolved

    def test_dry_run_accumulates_errors(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """Unresolvable names are accumulated, not raised."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({
            "JGCAFS_FORECAST",
            "JGCAFS_NONEXISTENT",
            "JGCAFS_ALSO_MISSING",
        })

        assert report.total_count == 3
        assert report.resolvable_count == 1
        assert report.unresolvable_count == 2
        assert len(report.errors) == 2
        assert "JGCAFS_FORECAST" in report.resolved

    def test_dry_run_counts_invariant(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """resolvable_count + unresolvable_count == total_count."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({
            "JGCAFS_FORECAST",
            "JGCAFS_MISSING1",
            "JGCAFS_MISSING2",
        })

        assert report.resolvable_count + report.unresolvable_count == report.total_count

    def test_dry_run_empty_set(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """Empty input → empty report with zeroes."""
        resolver = NameResolver(tmp_dev_root, default_registry)
        report = resolver.resolve_all_dry_run(set())

        assert report.total_count == 0
        assert report.resolvable_count == 0
        assert report.unresolvable_count == 0
        assert report.resolved == {}
        assert report.errors == []

    def test_dry_run_all_unresolvable(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """All names fail → all in errors, none resolved."""
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({
            "JGCAFS_MISSING1",
            "JGCAFS_MISSING2",
        })

        assert report.total_count == 2
        assert report.resolvable_count == 0
        assert report.unresolvable_count == 2
        assert len(report.errors) == 2
        assert report.resolved == {}


class TestDryRunReport:
    """Tests for DryRunReport dataclass and format_table()."""

    def test_format_table_basic(self):
        """format_table produces expected structure."""
        resolved = {
            "JGCAFS_FORECAST": ResolvedName("JGCAFS_FORECAST", "JGLOBAL_FORECAST", False),
        }
        report = DryRunReport(
            resolved=resolved,
            errors=[],
            total_count=1,
            resolvable_count=1,
            unresolvable_count=0,
        )

        table = report.format_table()

        assert "Name Resolution Report:" in table
        assert "JGCAFS_FORECAST" in table
        assert "JGLOBAL_FORECAST" in table
        assert "resolved" in table
        assert "Summary: 1 resolvable, 0 unresolvable (1 total)" in table

    def test_format_table_with_errors(self):
        """format_table shows ERROR entries for unresolvable names."""
        resolved = {
            "JGCAFS_FORECAST": ResolvedName("JGCAFS_FORECAST", "JGLOBAL_FORECAST", False),
        }
        errors = ["Cannot resolve 'JGCAFS_NONEXISTENT': searched [JGLOBAL_NONEXISTENT, JGCAFS_NONEXISTENT] in dev/jobs/"]
        report = DryRunReport(
            resolved=resolved,
            errors=errors,
            total_count=2,
            resolvable_count=1,
            unresolvable_count=1,
        )

        table = report.format_table()

        assert "JGCAFS_FORECAST" in table
        assert "JGCAFS_NONEXISTENT" in table
        assert "ERROR" in table
        assert "\u2014" in table  # em-dash for missing source
        assert "Summary: 1 resolvable, 1 unresolvable (2 total)" in table

    def test_format_table_empty_report(self):
        """format_table works with empty report."""
        report = DryRunReport(
            resolved={},
            errors=[],
            total_count=0,
            resolvable_count=0,
            unresolvable_count=0,
        )

        table = report.format_table()

        assert "Name Resolution Report:" in table
        assert "Summary: 0 resolvable, 0 unresolvable (0 total)" in table

    def test_format_table_contains_box_drawing_chars(self):
        """format_table uses box-drawing characters for the table."""
        resolved = {
            "JGCAFS_FORECAST": ResolvedName("JGCAFS_FORECAST", "JGLOBAL_FORECAST", False),
        }
        report = DryRunReport(
            resolved=resolved,
            errors=[],
            total_count=1,
            resolvable_count=1,
            unresolvable_count=0,
        )

        table = report.format_table()

        # Check for box-drawing characters
        assert "┌" in table
        assert "┐" in table
        assert "└" in table
        assert "┘" in table
        assert "│" in table
        assert "─" in table
        assert "├" in table
        assert "┤" in table
        assert "┼" in table
        assert "┬" in table
        assert "┴" in table

    def test_format_table_column_headers(self):
        """format_table includes correct column headers."""
        report = DryRunReport(
            resolved={"JGCAFS_X": ResolvedName("JGCAFS_X", "JGLOBAL_X", False)},
            errors=[],
            total_count=1,
            resolvable_count=1,
            unresolvable_count=0,
        )

        table = report.format_table()

        assert "Application_Name" in table
        assert "Shared_Source_Name" in table
        assert "Status" in table



class TestGcafsYamlMigration:
    """Unit tests verifying gcafs.yaml migration correctness.

    Ensures that the gcafs.yaml Workflow_YAML uses correct application-specific
    jjob prefixes per cycle and that all jjob values conform to the JAAAAA_Convention.

    Traces to: Requirements 1.1, 1.2, 1.3, 1.4
    """

    GCAFS_YAML_PATH = Path(__file__).parent.parent.parent / "parm" / "workflow" / "gcafs.yaml"
    JAAAAA_PATTERN = re.compile(r"^J[A-Z][A-Z0-9_]*$")

    # This jjob is a Direct_Match_Source that lives in dev/jobs/ under its own name.
    # It appears under a gcdas/ path but uses the JGDAS_ prefix (not JGCDAS_).
    GCDAS_EXCEPTIONS = {"JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"}

    @pytest.fixture(autouse=True)
    def load_yaml(self):
        """Load gcafs.yaml once for all tests in this class."""
        assert self.GCAFS_YAML_PATH.exists(), (
            f"gcafs.yaml not found at {self.GCAFS_YAML_PATH}"
        )
        with open(self.GCAFS_YAML_PATH) as f:
            self.yaml_data = yaml.safe_load(f)

    def _collect_jjobs_for_cycle(self, cycle_prefix: str) -> list[str]:
        """Collect all jjob values from families whose path starts with cycle_prefix."""
        jjobs = []
        for family in self.yaml_data["families"]:
            path = family["path"]
            if path.startswith(cycle_prefix):
                for task in family["tasks"]:
                    jjobs.append(task["jjob"])
        return jjobs

    def test_gcdas_tasks_use_jgcdas_prefix(self):
        """All jjob values under gcdas/ paths use JGCDAS_ prefix (with exceptions)."""
        gcdas_jjobs = self._collect_jjobs_for_cycle("gcdas/")

        assert len(gcdas_jjobs) > 0, "Expected at least one jjob under gcdas/ paths"

        for jjob in gcdas_jjobs:
            if jjob in self.GCDAS_EXCEPTIONS:
                continue
            assert jjob.startswith("JGCDAS_"), (
                f"jjob '{jjob}' under gcdas/ path does not use JGCDAS_ prefix"
            )

    def test_gcafs_tasks_use_jgcafs_prefix(self):
        """All jjob values under gcafs/ paths use JGCAFS_ prefix."""
        gcafs_jjobs = self._collect_jjobs_for_cycle("gcafs/")

        assert len(gcafs_jjobs) > 0, "Expected at least one jjob under gcafs/ paths"

        for jjob in gcafs_jjobs:
            assert jjob.startswith("JGCAFS_"), (
                f"jjob '{jjob}' under gcafs/ path does not use JGCAFS_ prefix"
            )

    def test_all_jjobs_match_jaaaaa_convention(self):
        """Every jjob value in gcafs.yaml matches ^J[A-Z][A-Z0-9_]*$ regex."""
        all_jjobs = []
        for family in self.yaml_data["families"]:
            for task in family["tasks"]:
                all_jjobs.append(task["jjob"])

        assert len(all_jjobs) > 0, "Expected at least one jjob in gcafs.yaml"

        for jjob in all_jjobs:
            assert self.JAAAAA_PATTERN.match(jjob), (
                f"jjob '{jjob}' does not match JAAAAA_Convention "
                f"(^J[A-Z][A-Z0-9_]*$)"
            )


# ---------------------------------------------------------------------------
# Task 7.3: Pipeline Integration Tests
# Traces to: Requirements 5.5, 7.1, 7.2, 7.3
# ---------------------------------------------------------------------------


class TestPrefixRegistryLoadInValidateStage:
    """Test PrefixRegistry loading with the actual prefix_registry.yaml file.

    Verifies that PrefixRegistry.load() can parse the real registry file
    in the deployment directory, and that _load_name_resolver wires it
    correctly into the pipeline.

    Traces to: Requirements 5.5
    """

    def test_load_actual_registry_file(self):
        """PrefixRegistry.load() with the actual prefix_registry.yaml succeeds."""
        registry_path = Path(__file__).parent.parent / "deployment" / "prefix_registry.yaml"
        registry = PrefixRegistry.load(registry_path)

        # Verify all 6 expected prefixes are present
        expected_prefixes = {"JGCAFS_", "JGCDAS_", "JGFS_", "JGDAS_", "JGEFS_", "JSFS_"}
        assert registry.known_prefixes() == expected_prefixes

    def test_load_actual_registry_search_lists(self):
        """Actual registry has correct search lists per Requirement 5.2."""
        registry_path = Path(__file__).parent.parent / "deployment" / "prefix_registry.yaml"
        registry = PrefixRegistry.load(registry_path)

        assert registry.get_search_prefixes("JGCAFS_") == ["JGLOBAL_"]
        assert registry.get_search_prefixes("JGCDAS_") == ["JGLOBAL_", "JGDAS_"]
        assert registry.get_search_prefixes("JGFS_") == ["JGLOBAL_", "JGFS_"]
        assert registry.get_search_prefixes("JGDAS_") == ["JGLOBAL_", "JGDAS_"]
        assert registry.get_search_prefixes("JGEFS_") == ["JGLOBAL_", "JGEFS_"]
        assert registry.get_search_prefixes("JSFS_") == ["JGLOBAL_", "JSFS_"]

    def test_load_name_resolver_uses_actual_registry(self, tmp_dev_root: Path):
        """_load_name_resolver() loads the actual prefix_registry.yaml and builds a NameResolver."""
        from deployment.pipeline import _load_name_resolver

        resolver = _load_name_resolver(tmp_dev_root)
        # It should be a NameResolver that knows about the standard prefixes
        # Verify by resolving a name that exists as a passthrough
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        result = resolver.resolve("JGCAFS_FORECAST")
        assert result.source_name == "JGLOBAL_FORECAST"
        assert result.application_name == "JGCAFS_FORECAST"
        assert result.is_passthrough is False


class TestMissingPrefixRegistryRaisesFatal:
    """Test that missing prefix_registry.yaml raises FATAL in validate stage.

    Traces to: Requirements 5.5
    """

    def test_missing_registry_in_validate_stage(self, tmp_path: Path):
        """PrefixRegistry.load() raises PipelineError when path doesn't exist.

        This mirrors the behavior in _stage_validate which checks the file
        exists before attempting to load it, raising FATAL if absent.
        """
        nonexistent = tmp_path / "nonexistent" / "prefix_registry.yaml"
        with pytest.raises(PipelineError) as exc_info:
            PrefixRegistry.load(nonexistent)

        assert "Prefix registry not found at" in str(exc_info.value)

    def test_missing_registry_via_load(self, tmp_path: Path):
        """PrefixRegistry.load() with a nonexistent path raises PipelineError."""
        bogus_path = tmp_path / "does_not_exist" / "prefix_registry.yaml"
        with pytest.raises(PipelineError, match="Prefix registry not found"):
            PrefixRegistry.load(bogus_path)

    def test_malformed_registry_raises(self, tmp_path: Path):
        """PrefixRegistry.load() with invalid YAML raises PipelineError."""
        bad_yaml = tmp_path / "prefix_registry.yaml"
        bad_yaml.write_text("not: a: valid: yaml: [[[")

        with pytest.raises(PipelineError, match="Failed to parse prefix registry"):
            PrefixRegistry.load(bad_yaml)

    def test_registry_missing_registry_key_raises(self, tmp_path: Path):
        """PrefixRegistry.load() without 'registry' key raises PipelineError."""
        bad_yaml = tmp_path / "prefix_registry.yaml"
        bad_yaml.write_text("something_else:\n  key: value\n")

        with pytest.raises(PipelineError, match="missing 'registry' key"):
            PrefixRegistry.load(bad_yaml)


class TestDryRunReportOutputFormat:
    """Test dry-run report output format matches the design spec.

    The dry-run report should produce a formatted table with columns for
    Application_Name, Shared_Source_Name, and Status, plus a summary line.

    Traces to: Requirements 7.1, 7.2, 7.3
    """

    def test_dry_run_report_with_known_names(self, tmp_dev_root: Path, default_registry: PrefixRegistry):
        """Dry-run produces table with all names resolved."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        (tmp_dev_root / "jobs" / "JGLOBAL_STAGE_IC").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({"JGCAFS_FORECAST", "JGCAFS_STAGE_IC"})
        table = report.format_table()

        # Verify structure
        assert "Name Resolution Report:" in table
        assert "Application_Name" in table
        assert "Shared_Source_Name" in table
        assert "Status" in table
        assert "JGCAFS_FORECAST" in table
        assert "JGLOBAL_FORECAST" in table
        assert "JGCAFS_STAGE_IC" in table
        assert "JGLOBAL_STAGE_IC" in table
        assert "resolved" in table
        assert "Summary: 2 resolvable, 0 unresolvable (2 total)" in table

    def test_dry_run_report_with_errors(self, tmp_dev_root: Path, default_registry: PrefixRegistry):
        """Dry-run reports all unresolvable names without halting."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({
            "JGCAFS_FORECAST",
            "JGCAFS_MISSING_ONE",
            "JGCAFS_MISSING_TWO",
        })
        table = report.format_table()

        # Resolved name should be in the table
        assert "JGCAFS_FORECAST" in table
        assert "JGLOBAL_FORECAST" in table
        assert "resolved" in table

        # Errors should be in the table with ERROR status
        assert "JGCAFS_MISSING_ONE" in table
        assert "JGCAFS_MISSING_TWO" in table
        assert "ERROR" in table

        # Summary should account for all names
        assert "Summary: 1 resolvable, 2 unresolvable (3 total)" in table

    def test_dry_run_report_counts_invariant(self, tmp_dev_root: Path, default_registry: PrefixRegistry):
        """resolvable + unresolvable == total in dry-run report."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        names = {"JGCAFS_FORECAST", "JGCAFS_A", "JGCAFS_B", "JGCAFS_C"}
        report = resolver.resolve_all_dry_run(names)

        assert report.resolvable_count + report.unresolvable_count == report.total_count
        assert report.total_count == len(names)

    def test_dry_run_format_table_has_box_drawing(self, tmp_dev_root: Path, default_registry: PrefixRegistry):
        """format_table uses box-drawing characters for structured output."""
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").touch()
        resolver = NameResolver(tmp_dev_root, default_registry)

        report = resolver.resolve_all_dry_run({"JGCAFS_FORECAST"})
        table = report.format_table()

        # Verify box-drawing characters
        assert "\u250c" in table  # ┌
        assert "\u2510" in table  # ┐
        assert "\u2514" in table  # └
        assert "\u2518" in table  # ┘
        assert "\u2502" in table  # │
        assert "\u2500" in table  # ─


class TestEndToEndPipelineApplicationNaming:
    """Test end-to-end pipeline wiring with application naming.

    Verifies that the pipeline correctly wires the PrefixRegistry and
    NameResolver to produce application-named J-Jobs in the EXPDIR.

    Traces to: Requirements 5.5, 7.1, 7.2, 7.3
    """

    def test_name_resolver_wiring_produces_correct_resolution_map(
        self, tmp_dev_root: Path, default_registry: PrefixRegistry
    ):
        """NameResolver produces a resolution_map suitable for FileStager."""
        # Set up source files
        (tmp_dev_root / "jobs" / "JGLOBAL_FORECAST").write_text("#!/bin/bash\n# forecast")
        (tmp_dev_root / "jobs" / "JGLOBAL_STAGE_IC").write_text("#!/bin/bash\n# stage ic")
        (tmp_dev_root / "jobs" / "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX").write_text(
            "#!/bin/bash\n# aero bmat"
        )

        resolver = NameResolver(tmp_dev_root, default_registry)

        # Application names as they would appear in a workflow YAML
        app_names = {
            "JGCAFS_FORECAST",
            "JGCAFS_STAGE_IC",
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX",
        }

        resolution_map = resolver.resolve_all(app_names)

        # Verify resolution
        assert resolution_map["JGCAFS_FORECAST"].source_name == "JGLOBAL_FORECAST"
        assert resolution_map["JGCAFS_FORECAST"].is_passthrough is False
        assert resolution_map["JGCAFS_STAGE_IC"].source_name == "JGLOBAL_STAGE_IC"
        assert resolution_map["JGCAFS_STAGE_IC"].is_passthrough is False
        # Direct match: JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX exists directly
        assert resolution_map["JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"].source_name == (
            "JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"
        )
        assert resolution_map["JGDAS_AERO_ANALYSIS_GENERATE_BMATRIX"].is_passthrough is True

    def test_file_stager_with_resolution_map_produces_application_named_expdir(
        self, tmp_path: Path
    ):
        """FileStager.stage_jjobs_with_rename produces EXPDIR with application names."""
        from deployment.file_stager import FileStager

        # Create dev/jobs source files
        project_root = tmp_path / "project"
        dev_root = project_root / "dev"
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(parents=True)

        (jobs_dir / "JGLOBAL_FORECAST").write_text("#!/bin/bash\n# forecast job")
        (jobs_dir / "JGLOBAL_STAGE_IC").write_text("#!/bin/bash\n# stage ic job")

        # Create EXPDIR
        expdir = tmp_path / "expdir"
        expdir.mkdir()

        stager = FileStager(project_root=project_root, expdir=expdir, use_uwtools=False)

        # Build a resolution_map as the pipeline would
        resolution_map = {
            "JGCAFS_FORECAST": ResolvedName("JGCAFS_FORECAST", "JGLOBAL_FORECAST", False),
            "JGCAFS_STAGE_IC": ResolvedName("JGCAFS_STAGE_IC", "JGLOBAL_STAGE_IC", False),
        }

        result = stager.stage_jjobs_with_rename(resolution_map)

        # Verify files staged with application names
        assert result.files_copied == 2
        assert (expdir / "jobs" / "JGCAFS_FORECAST").exists()
        assert (expdir / "jobs" / "JGCAFS_STAGE_IC").exists()

        # Verify content is preserved (not modified)
        assert (expdir / "jobs" / "JGCAFS_FORECAST").read_text() == "#!/bin/bash\n# forecast job"
        assert (expdir / "jobs" / "JGCAFS_STAGE_IC").read_text() == "#!/bin/bash\n# stage ic job"

        # Verify no JGLOBAL_ files in EXPDIR
        expdir_jobs = list((expdir / "jobs").iterdir())
        for f in expdir_jobs:
            assert not f.name.startswith("JGLOBAL_"), (
                f"EXPDIR should not contain shared-prefix files, found {f.name}"
            )

    def test_end_to_end_resolve_and_stage(self, tmp_path: Path):
        """Full end-to-end: load registry → resolve names → stage with rename."""
        from deployment.file_stager import FileStager

        # Setup project structure
        project_root = tmp_path / "project"
        dev_root = project_root / "dev"
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(parents=True)

        # Create source J-Job files
        forecast_content = "#!/bin/bash\nset -eu\n. ${EXPDIR}/jobs/jjob_header\nexglobal_forecast.sh"
        (jobs_dir / "JGLOBAL_FORECAST").write_text(forecast_content)

        # Use the actual registry
        registry_path = Path(__file__).parent.parent / "deployment" / "prefix_registry.yaml"
        registry = PrefixRegistry.load(registry_path)
        resolver = NameResolver(dev_root, registry)

        # Resolve application names
        app_names = {"JGCAFS_FORECAST", "JGCDAS_FORECAST"}
        resolution_map = resolver.resolve_all(app_names)

        # Stage to EXPDIR
        expdir = tmp_path / "expdir"
        expdir.mkdir()
        stager = FileStager(project_root=project_root, expdir=expdir, use_uwtools=False)
        result = stager.stage_jjobs_with_rename(resolution_map)

        # Verify both application-named files were produced from same source
        assert result.files_copied == 2
        assert (expdir / "jobs" / "JGCAFS_FORECAST").exists()
        assert (expdir / "jobs" / "JGCDAS_FORECAST").exists()

        # Both should have the same content (from JGLOBAL_FORECAST)
        assert (expdir / "jobs" / "JGCAFS_FORECAST").read_text() == forecast_content
        assert (expdir / "jobs" / "JGCDAS_FORECAST").read_text() == forecast_content

        # Verify all filenames match JAAAAA_Convention
        import re
        jjob_pattern = re.compile(r"^J[A-Z][A-Z0-9_]*$")
        for f in (expdir / "jobs").iterdir():
            assert jjob_pattern.match(f.name), f"File {f.name} doesn't match JAAAAA convention"

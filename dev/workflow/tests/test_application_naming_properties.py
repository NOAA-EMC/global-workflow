"""Property-based tests for application-specific J-Job naming.

Tests the correctness properties defined in the design document for
the application-jjob-naming feature.

Traces to: Design Document - Correctness Properties
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import pytest
from hypothesis import given, settings, HealthCheck, assume
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.name_resolver import DryRunReport, NameResolver, PrefixRegistry, ResolvedName
from deployment.pipeline import PipelineError


# ---------------------------------------------------------------------------
# Shared Strategies
# ---------------------------------------------------------------------------

# Valid suffixes that could appear after a prefix (uppercase letters, digits, underscores)
_SUFFIX_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

# Known shared prefixes that exist as source files
_SHARED_PREFIXES = ["JGLOBAL_", "JGDAS_", "JGFS_", "JGEFS_", "JSFS_"]

# Known application prefixes from the registry
_APPLICATION_PREFIXES = ["JGCAFS_", "JGCDAS_", "JGFS_", "JGDAS_", "JGEFS_", "JSFS_"]


@st.composite
def _jjob_suffix(draw):
    """Generate a valid J-Job suffix (e.g., FORECAST, AERO_ANALYSIS_INITIALIZE)."""
    # Generate 1-3 underscore-separated parts
    num_parts = draw(st.integers(min_value=1, max_value=3))
    parts = []
    for _ in range(num_parts):
        part = draw(st.text(
            alphabet="ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
            min_size=2,
            max_size=12,
        ))
        # Ensure part starts with a letter
        if part[0].isdigit():
            part = "A" + part[1:]
        parts.append(part)
    return "_".join(parts)


# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 6: Backward Compatibility
# ---------------------------------------------------------------------------


@st.composite
def _shared_source_names(draw):
    """Generate a set of shared source names (e.g., JGLOBAL_FORECAST).

    These names exist directly in dev/jobs/ and should pass through
    without renaming.
    """
    prefix = draw(st.sampled_from(_SHARED_PREFIXES))
    suffix = draw(_jjob_suffix())
    return prefix + suffix


@st.composite
def _application_names_with_source(draw):
    """Generate an application name and its expected source.

    Returns (application_name, source_name) where source_name uses
    the first shared prefix in the registry search list.
    """
    # Pick an application prefix that maps to JGLOBAL_ first
    app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_"]))
    suffix = draw(_jjob_suffix())

    application_name = app_prefix + suffix
    source_name = "JGLOBAL_" + suffix

    return (application_name, source_name)


@st.composite
def _mixed_mode_names(draw):
    """Generate a mix of shared names and application names for backward compat testing.

    Returns:
        tuple of (shared_names, app_name_pairs) where:
        - shared_names: list of names that exist directly in dev/jobs/
        - app_name_pairs: list of (application_name, source_name) tuples
    """
    # Generate unique suffixes upfront to avoid deduplication loops
    num_total = draw(st.integers(min_value=2, max_value=4))
    num_shared = draw(st.integers(min_value=1, max_value=max(1, num_total - 1)))
    num_app = num_total - num_shared

    # Generate distinct suffixes
    suffixes = draw(st.lists(
        _jjob_suffix(),
        min_size=num_total,
        max_size=num_total,
        unique=True,
    ))

    # Split suffixes between shared and application names
    shared_suffixes = suffixes[:num_shared]
    app_suffixes = suffixes[num_shared:]

    shared_names = []
    for suffix in shared_suffixes:
        prefix = draw(st.sampled_from(_SHARED_PREFIXES))
        shared_names.append(prefix + suffix)

    app_pairs = []
    for suffix in app_suffixes:
        app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_"]))
        app_pairs.append((app_prefix + suffix, "JGLOBAL_" + suffix))

    return (shared_names, app_pairs)


class TestBackwardCompatibilityProperty:
    """Property 6: Backward Compatibility.

    For any Workflow_YAML where a jjob: value matches a file directly in
    dev/jobs/ (i.e., uses a Shared_Source_Name like JGLOBAL_FORECAST), the
    pipeline SHALL copy that file without renaming. Mixed-mode YAMLs containing
    both Application_Names and Shared_Source_Names SHALL process both types
    correctly in the same run.

    Validates: Requirements 8.1, 8.2, 8.3
    """

    @given(shared_name=_shared_source_names())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_shared_names_passthrough(self, shared_name: str, tmp_path: Path):
        """Shared names that exist directly in dev/jobs/ pass through without rename.

        # Feature: application-jjob-naming, Property 6: Backward Compatibility
        **Validates: Requirements 8.1, 8.2**
        """
        # Setup: create the shared name file in dev/jobs/
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir(exist_ok=True)
        (jobs_dir / shared_name).touch()

        registry = PrefixRegistry.default()
        resolver = NameResolver(tmp_path, registry)

        # Act
        result = resolver.resolve(shared_name)

        # Assert: pass-through — no rename
        assert result.is_passthrough is True, (
            f"Shared name '{shared_name}' should be pass-through but got "
            f"is_passthrough={result.is_passthrough}"
        )
        assert result.source_name == shared_name, (
            f"Shared name '{shared_name}' source_name should equal "
            f"application_name but got source_name='{result.source_name}'"
        )
        assert result.application_name == shared_name, (
            f"application_name should be '{shared_name}' but got "
            f"'{result.application_name}'"
        )

    @given(data=_application_names_with_source())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_application_names_resolve_via_prefix(self, data: tuple, tmp_path: Path):
        """Application names resolve correctly via prefix search.

        # Feature: application-jjob-naming, Property 6: Backward Compatibility
        **Validates: Requirements 8.1, 8.2, 8.3**
        """
        application_name, source_name = data

        # Setup: create the source file (JGLOBAL_*) in dev/jobs/
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir(exist_ok=True)
        (jobs_dir / source_name).touch()

        # Ensure the application_name does NOT exist directly
        # (so it must resolve via prefix search)
        app_path = jobs_dir / application_name
        if app_path.exists():
            app_path.unlink()

        registry = PrefixRegistry.default()
        resolver = NameResolver(tmp_path, registry)

        # Act
        result = resolver.resolve(application_name)

        # Assert: resolved via prefix, not pass-through
        assert result.is_passthrough is False, (
            f"Application name '{application_name}' should resolve via prefix "
            f"(is_passthrough=False) but got is_passthrough=True"
        )
        assert result.source_name == source_name, (
            f"Application name '{application_name}' should resolve to "
            f"'{source_name}' but got '{result.source_name}'"
        )
        assert result.application_name == application_name

    @given(mixed=_mixed_mode_names())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture, HealthCheck.large_base_example])
    def test_mixed_mode_processes_both_types(self, mixed: tuple, tmp_path: Path):
        """Mixed-mode YAMLs with both shared and application names resolve correctly.

        # Feature: application-jjob-naming, Property 6: Backward Compatibility
        **Validates: Requirements 8.1, 8.2, 8.3**
        """
        shared_names, app_pairs = mixed

        # Setup: create filesystem state
        jobs_dir = tmp_path / "jobs"
        jobs_dir.mkdir(exist_ok=True)

        # Create shared name files directly
        for name in shared_names:
            (jobs_dir / name).touch()

        # Create source files for application names (JGLOBAL_* sources)
        for app_name, source_name in app_pairs:
            (jobs_dir / source_name).touch()
            # Ensure app_name does NOT exist directly so it goes through prefix resolution
            app_path = jobs_dir / app_name
            if app_path.exists():
                app_path.unlink()

        registry = PrefixRegistry.default()
        resolver = NameResolver(tmp_path, registry)

        # Act: resolve all names (simulating resolve_all batch behavior)
        all_names = set(shared_names) | {app_name for app_name, _ in app_pairs}
        results = {}
        for name in all_names:
            results[name] = resolver.resolve(name)

        # Assert: shared names are pass-through
        for name in shared_names:
            assert results[name].is_passthrough is True, (
                f"Shared name '{name}' should be pass-through in mixed mode"
            )
            assert results[name].source_name == name, (
                f"Shared name '{name}' source should equal itself in mixed mode"
            )

        # Assert: application names resolve via prefix
        for app_name, expected_source in app_pairs:
            assert results[app_name].is_passthrough is False, (
                f"App name '{app_name}' should NOT be pass-through in mixed mode"
            )
            assert results[app_name].source_name == expected_source, (
                f"App name '{app_name}' should resolve to '{expected_source}' "
                f"but got '{results[app_name].source_name}'"
            )

        # Assert: total results equals total input names (no loss)
        assert len(results) == len(all_names), (
            f"Expected {len(all_names)} results but got {len(results)}"
        )

# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 1: Name Resolution Correctness
# ---------------------------------------------------------------------------

# Strategies specific to Property 1


@st.composite
def _valid_suffix_p1(draw) -> str:
    """Generate a valid J-Job suffix matching [A-Z][A-Z0-9_]*.

    Suffixes must start with an uppercase letter and may contain uppercase
    letters, digits, and underscores. No trailing underscore (to avoid
    confusion with prefixes).
    """
    first_char = draw(st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    rest = draw(
        st.text(
            alphabet=st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"),
            min_size=0,
            max_size=20,
        )
    )
    suffix = first_char + rest
    # Strip trailing underscores to avoid prefix-like endings
    suffix = suffix.rstrip("_")
    assume(len(suffix) >= 1)
    return suffix


@st.composite
def _random_registry_entry_p1(draw) -> tuple[str, list[str]]:
    """Generate a random (app_prefix, search_prefixes) pair from the default registry."""
    known_entries = [
        ("JGCAFS_", ["JGLOBAL_"]),
        ("JGCDAS_", ["JGLOBAL_", "JGDAS_"]),
        ("JGFS_", ["JGLOBAL_", "JGFS_"]),
        ("JGDAS_", ["JGLOBAL_", "JGDAS_"]),
        ("JGEFS_", ["JGLOBAL_", "JGEFS_"]),
        ("JSFS_", ["JGLOBAL_", "JSFS_"]),
    ]
    return draw(st.sampled_from(known_entries))


@st.composite
def _filesystem_state_p1(draw, search_prefixes: list[str], suffix: str) -> set[str]:
    """Generate a random subset of candidate source files that exist.

    For a given suffix and list of search_prefixes, randomly decide which
    candidate files exist in the filesystem.
    """
    candidates = [prefix + suffix for prefix in search_prefixes]
    existing = set()
    for candidate in candidates:
        if draw(st.booleans()):
            existing.add(candidate)
    return existing


@given(data=st.data())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_name_resolution_ordered_search(data):
    """Property 1: Name Resolution Correctness (Ordered Search, First-Match).

    For any Application_Name with a prefix registered in the Prefix_Registry,
    and for any filesystem state of dev/jobs/, the Name_Resolver SHALL return
    the first source file found by searching Shared_Prefixes in registry-defined
    order, and SHALL raise a FATAL error if no source exists at any search
    position.

    # Feature: application-jjob-naming, Property 1: Name Resolution Correctness
    **Validates: Requirements 2.1, 2.2, 2.3, 2.4, 2.5, 5.3**
    """
    # Generate a random registry entry (app_prefix, search_prefixes)
    app_prefix, search_prefixes = data.draw(_random_registry_entry_p1())

    # Generate a random valid suffix
    suffix = data.draw(_valid_suffix_p1())

    # Construct the application name
    application_name = app_prefix + suffix

    # Generate a random filesystem state: which candidates exist
    existing_files = data.draw(_filesystem_state_p1(search_prefixes, suffix))

    # Also decide whether the application_name itself exists directly
    # (which would trigger step 1: direct check → pass-through)
    direct_exists = data.draw(st.booleans())

    # Set up the filesystem using tempfile (compatible with Hypothesis)
    with tempfile.TemporaryDirectory() as tmpdir:
        dev_root = Path(tmpdir)
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(parents=True, exist_ok=True)

        if direct_exists:
            (jobs_dir / application_name).touch()

        for filename in existing_files:
            (jobs_dir / filename).touch()

        # Determine whether the application_name exists on disk.
        # This can happen explicitly (direct_exists=True) OR implicitly
        # when a candidate in existing_files has the same name as
        # application_name (e.g., JGFS_A with prefix JGFS_ and suffix A
        # produces candidate JGFS_A which IS the application_name).
        app_name_on_disk = (jobs_dir / application_name).exists()

        # Create the registry and resolver
        registry = PrefixRegistry.default()
        resolver = NameResolver(dev_root, registry)

        # Determine expected behavior based on the 5-step algorithm:
        # 1. If application_name exists directly → pass-through
        # 2. Prefix identification (already known from our generation)
        # 3. Ordered search through shared prefixes → first match wins
        # 4. Direct fallback (already handled by step 1)
        # 5. FATAL error if no match

        if app_name_on_disk:
            # Step 1: Direct check succeeds → pass-through
            result = resolver.resolve(application_name)
            assert result.application_name == application_name
            assert result.source_name == application_name
            assert result.is_passthrough is True

        elif existing_files:
            # Step 3: Ordered search — find the FIRST match in registry order
            expected_source = None
            for shared_prefix in search_prefixes:
                candidate = shared_prefix + suffix
                if candidate in existing_files:
                    expected_source = candidate
                    break

            if expected_source is not None:
                # A match was found in the ordered search
                result = resolver.resolve(application_name)
                assert result.application_name == application_name
                assert result.source_name == expected_source, (
                    f"Expected first-match '{expected_source}' from search "
                    f"order {search_prefixes}, but got '{result.source_name}'. "
                    f"Existing files: {existing_files}"
                )
                assert result.is_passthrough is False
            else:
                # existing_files has entries but none match our search prefixes
                # for this suffix — should raise FATAL
                with pytest.raises(PipelineError) as exc_info:
                    resolver.resolve(application_name)
                assert "Cannot resolve" in str(exc_info.value)

        else:
            # No candidates exist → FATAL error (Step 5)
            with pytest.raises(PipelineError) as exc_info:
                resolver.resolve(application_name)

            error_msg = str(exc_info.value)
            assert "Cannot resolve" in error_msg
            assert application_name in error_msg


# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 7: Dry-Run Completeness
# ---------------------------------------------------------------------------


@st.composite
def _dry_run_name_sets(draw):
    """Generate a mix of resolvable and unresolvable application names.

    Returns:
        tuple of (resolvable_names, unresolvable_names) where:
        - resolvable_names: list of (application_name, source_name) pairs
          where source_name will exist in dev/jobs/
        - unresolvable_names: list of application_names that have no
          corresponding source file
    """
    # Generate unique suffixes to avoid collisions
    num_resolvable = draw(st.integers(min_value=0, max_value=5))
    num_unresolvable = draw(st.integers(min_value=0, max_value=5))
    total = num_resolvable + num_unresolvable
    assume(total >= 1)

    suffixes = draw(st.lists(
        _jjob_suffix(),
        min_size=total,
        max_size=total,
        unique=True,
    ))

    resolvable_suffixes = suffixes[:num_resolvable]
    unresolvable_suffixes = suffixes[num_resolvable:]

    # Resolvable: application names whose JGLOBAL_ source will exist
    resolvable = []
    for suffix in resolvable_suffixes:
        app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_"]))
        app_name = app_prefix + suffix
        source_name = "JGLOBAL_" + suffix
        resolvable.append((app_name, source_name))

    # Unresolvable: application names with NO corresponding source file
    unresolvable = []
    for suffix in unresolvable_suffixes:
        app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_"]))
        app_name = app_prefix + suffix
        unresolvable.append(app_name)

    return (resolvable, unresolvable)


class TestDryRunCompletenessProperty:
    """Property 7: Dry-Run Completeness.

    For any Workflow_YAML with N total jjob: references (some resolvable,
    some not), the dry-run report SHALL list all N entries, report all
    unresolvable names (not halt on the first), and the sum of
    resolvable_count + unresolvable_count SHALL equal N.

    # Feature: application-jjob-naming, Property 7: Dry-Run Completeness
    **Validates: Requirements 7.1, 7.2, 7.3**
    """

    @given(name_sets=_dry_run_name_sets())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dry_run_reports_all_entries(self, name_sets: tuple):
        """Dry-run report lists all N entries regardless of resolvability.

        # Feature: application-jjob-naming, Property 7: Dry-Run Completeness
        **Validates: Requirements 7.1, 7.2, 7.3**
        """
        resolvable_pairs, unresolvable_names = name_sets

        with tempfile.TemporaryDirectory() as tmpdir:
            dev_root = Path(tmpdir)
            jobs_dir = dev_root / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create only resolvable source files
            for app_name, source_name in resolvable_pairs:
                (jobs_dir / source_name).touch()

            registry = PrefixRegistry.default()
            resolver = NameResolver(dev_root, registry)

            # Collect all names
            all_names = {app_name for app_name, _ in resolvable_pairs} | set(unresolvable_names)
            n = len(all_names)

            # Act: dry-run resolution (should NOT raise)
            report = resolver.resolve_all_dry_run(all_names)

            # Assert: total_count equals N
            assert report.total_count == n, (
                f"Expected total_count={n} but got {report.total_count}"
            )

            # Assert: resolvable_count + unresolvable_count == N
            assert report.resolvable_count + report.unresolvable_count == n, (
                f"resolvable_count({report.resolvable_count}) + "
                f"unresolvable_count({report.unresolvable_count}) != total({n})"
            )

    @given(name_sets=_dry_run_name_sets())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dry_run_reports_all_unresolvable(self, name_sets: tuple):
        """Dry-run reports ALL unresolvable names (does not halt on first error).

        # Feature: application-jjob-naming, Property 7: Dry-Run Completeness
        **Validates: Requirements 7.1, 7.2, 7.3**
        """
        resolvable_pairs, unresolvable_names = name_sets

        with tempfile.TemporaryDirectory() as tmpdir:
            dev_root = Path(tmpdir)
            jobs_dir = dev_root / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create only resolvable source files
            for app_name, source_name in resolvable_pairs:
                (jobs_dir / source_name).touch()

            registry = PrefixRegistry.default()
            resolver = NameResolver(dev_root, registry)

            all_names = {app_name for app_name, _ in resolvable_pairs} | set(unresolvable_names)

            # Act: dry-run should NOT raise (unlike resolve_all)
            report = resolver.resolve_all_dry_run(all_names)

            # Assert: all unresolvable names are reported in errors
            assert report.unresolvable_count == len(unresolvable_names), (
                f"Expected {len(unresolvable_names)} unresolvable but got "
                f"{report.unresolvable_count}"
            )

            # Each unresolvable name must appear in the error messages
            for name in unresolvable_names:
                found = any(name in error for error in report.errors)
                assert found, (
                    f"Unresolvable name '{name}' not found in error list: {report.errors}"
                )

    @given(name_sets=_dry_run_name_sets())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dry_run_reports_all_resolvable(self, name_sets: tuple):
        """Dry-run correctly tracks all resolvable names in resolved dict.

        # Feature: application-jjob-naming, Property 7: Dry-Run Completeness
        **Validates: Requirements 7.1, 7.2, 7.3**
        """
        resolvable_pairs, unresolvable_names = name_sets

        with tempfile.TemporaryDirectory() as tmpdir:
            dev_root = Path(tmpdir)
            jobs_dir = dev_root / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create only resolvable source files
            for app_name, source_name in resolvable_pairs:
                (jobs_dir / source_name).touch()

            registry = PrefixRegistry.default()
            resolver = NameResolver(dev_root, registry)

            all_names = {app_name for app_name, _ in resolvable_pairs} | set(unresolvable_names)

            # Act
            report = resolver.resolve_all_dry_run(all_names)

            # Assert: all resolvable names appear in report.resolved
            assert report.resolvable_count == len(resolvable_pairs), (
                f"Expected {len(resolvable_pairs)} resolvable but got "
                f"{report.resolvable_count}"
            )
            for app_name, expected_source in resolvable_pairs:
                assert app_name in report.resolved, (
                    f"Resolvable name '{app_name}' not found in resolved dict"
                )
                assert report.resolved[app_name].source_name == expected_source, (
                    f"Expected source '{expected_source}' for '{app_name}' but "
                    f"got '{report.resolved[app_name].source_name}'"
                )

    @given(name_sets=_dry_run_name_sets())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dry_run_does_not_raise(self, name_sets: tuple):
        """Dry-run never raises PipelineError, unlike resolve_all.

        # Feature: application-jjob-naming, Property 7: Dry-Run Completeness
        **Validates: Requirements 7.1, 7.2, 7.3**
        """
        resolvable_pairs, unresolvable_names = name_sets
        assume(len(unresolvable_names) > 0)

        with tempfile.TemporaryDirectory() as tmpdir:
            dev_root = Path(tmpdir)
            jobs_dir = dev_root / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create only resolvable source files
            for app_name, source_name in resolvable_pairs:
                (jobs_dir / source_name).touch()

            registry = PrefixRegistry.default()
            resolver = NameResolver(dev_root, registry)

            all_names = {app_name for app_name, _ in resolvable_pairs} | set(unresolvable_names)

            # Act: dry-run should NOT raise even with unresolvable names
            # (unlike resolve_all which would raise PipelineError)
            try:
                report = resolver.resolve_all_dry_run(all_names)
            except PipelineError:
                pytest.fail(
                    "resolve_all_dry_run raised PipelineError — it should "
                    "accumulate errors instead of raising"
                )

            # Verify it accumulated errors gracefully
            assert report.unresolvable_count > 0, (
                "Expected at least 1 unresolvable name but got 0"
            )


# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 4: DAG Filter Resolution Integration
# ---------------------------------------------------------------------------


@st.composite
def _dag_filter_workflow_data(draw):
    """Generate Workflow_YAML with Application_Names and corresponding source files.

    Returns a dict with:
        - workflow_yaml: dict in the expected format
        - app_names: list of Application_Names used in the YAML
        - source_map: dict mapping app_name → source_name
        - ex_scripts: list of ex-script names to embed in source files
        - suffixes: the generated suffixes (for uniqueness)
    """
    # Generate 1-3 unique suffixes for J-Jobs
    num_tasks = draw(st.integers(min_value=1, max_value=3))
    suffixes = draw(st.lists(
        _jjob_suffix(),
        min_size=num_tasks,
        max_size=num_tasks,
        unique=True,
    ))

    # Use JGCAFS_ prefix so they resolve to JGLOBAL_ sources
    app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_"]))

    app_names = []
    source_map = {}
    ex_scripts = []

    for i, suffix in enumerate(suffixes):
        app_name = app_prefix + suffix
        source_name = "JGLOBAL_" + suffix
        app_names.append(app_name)
        source_map[app_name] = source_name

        # Generate a unique ex-script name for each source file.
        # The DAG_Filter regex expects ex[a-z_]+\.(sh|py) — only lowercase
        # letters and underscores after "ex", no digits allowed.
        # Use an index-based alphabetic discriminator for uniqueness.
        alpha_id = chr(ord("a") + i)  # a, b, c, ...
        clean_suffix = "".join(c for c in suffix.lower() if c.isalpha() or c == "_")
        if not clean_suffix:
            clean_suffix = "task"
        ex_script_name = f"ex{clean_suffix}_{alpha_id}.sh"
        ex_scripts.append(ex_script_name)

    # Build Workflow_YAML in expected format
    tasks = [{"jjob": name} for name in app_names]
    workflow_yaml = {"families": [{"tasks": tasks}]}

    return {
        "workflow_yaml": workflow_yaml,
        "app_names": app_names,
        "source_map": source_map,
        "ex_scripts": ex_scripts,
        "suffixes": suffixes,
    }


class TestDAGFilterResolutionIntegration:
    """Property 4: DAG Filter Resolution Integration.

    For any Workflow_YAML containing Application_Names in jjob: fields, the
    DAG_Filter SHALL (a) collect the Application_Names from the YAML,
    (b) resolve each to its Shared_Source_Name via the Name_Resolver,
    (c) parse the source file (not the application-named file) for ex-script
    and config dependencies, and (d) include both Application_Name and
    source_name in the reachability set.

    # Feature: application-jjob-naming, Property 4: DAG Filter Resolution Integration
    **Validates: Requirements 4.1, 4.2, 4.3**
    """

    @given(data=_dag_filter_workflow_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dag_filter_collects_application_names_from_yaml(self, data: dict, tmp_path: Path):
        """DAG_Filter collects Application_Names (not source names) from Workflow_YAML.

        # Feature: application-jjob-naming, Property 4: DAG Filter Resolution Integration
        **Validates: Requirements 4.1, 4.2, 4.3**
        """
        from deployment.dag_filter import DAGFilter, DAGReachabilitySet
        from deployment.name_resolver import NameResolver, PrefixRegistry

        workflow_yaml = data["workflow_yaml"]
        app_names = data["app_names"]
        source_map = data["source_map"]
        ex_scripts = data["ex_scripts"]

        # Setup filesystem
        dev_root = tmp_path
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(exist_ok=True)
        scripts_dir = dev_root / "scripts"
        scripts_dir.mkdir(exist_ok=True)
        ush_dir = dev_root / "ush"
        ush_dir.mkdir(exist_ok=True)
        config_dir = dev_root / "parm" / "config" / "gfs"
        config_dir.mkdir(parents=True, exist_ok=True)
        # Create unconditional config files
        (config_dir / "config.base").touch()
        (config_dir / "config.base.j2").touch()
        (config_dir / "config.com").touch()

        # Create source J-Job files with ex-script invocations
        for i, app_name in enumerate(app_names):
            source_name = source_map[app_name]
            ex_script = ex_scripts[i]
            content = f'#!/bin/bash\n${{SCRglobal}}/{ex_script}\n'
            (jobs_dir / source_name).write_text(content)
            # Create the ex-script file (to pass existence check)
            (scripts_dir / ex_script).touch()

        # Create registry and resolver
        registry = PrefixRegistry.default()
        resolver = NameResolver(dev_root, registry)

        # Create DAGFilter with name_resolver
        dag_filter = DAGFilter(
            dev_root=dev_root,
            workflow_yaml=workflow_yaml,
            platform="HERA",
            name_resolver=resolver,
        )

        # Act: extract jjobs from YAML (Layer 1)
        extracted = dag_filter.extract_jjobs_from_yaml()

        # Assert: extracted names are Application_Names, not source names
        for app_name in app_names:
            assert app_name in extracted, (
                f"Application_Name '{app_name}' should be collected from YAML "
                f"but was not found in extracted set: {extracted}"
            )

        # Assert: source names are NOT directly in the extracted set
        # (unless they happen to also be valid application names)
        for app_name in app_names:
            source_name = source_map[app_name]
            if source_name != app_name:
                assert source_name not in extracted, (
                    f"Source name '{source_name}' should NOT be in extracted "
                    f"set — only Application_Names should be collected"
                )

    @given(data=_dag_filter_workflow_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dag_filter_resolves_to_source_names(self, data: dict, tmp_path: Path):
        """DAG_Filter resolves Application_Names to Shared_Source_Names via Name_Resolver.

        # Feature: application-jjob-naming, Property 4: DAG Filter Resolution Integration
        **Validates: Requirements 4.1, 4.2, 4.3**
        """
        from deployment.dag_filter import DAGFilter, DAGReachabilitySet
        from deployment.name_resolver import NameResolver, PrefixRegistry

        workflow_yaml = data["workflow_yaml"]
        app_names = data["app_names"]
        source_map = data["source_map"]
        ex_scripts = data["ex_scripts"]

        # Setup filesystem
        dev_root = tmp_path
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(exist_ok=True)
        scripts_dir = dev_root / "scripts"
        scripts_dir.mkdir(exist_ok=True)
        ush_dir = dev_root / "ush"
        ush_dir.mkdir(exist_ok=True)
        config_dir = dev_root / "parm" / "config" / "gfs"
        config_dir.mkdir(parents=True, exist_ok=True)
        (config_dir / "config.base").touch()
        (config_dir / "config.base.j2").touch()
        (config_dir / "config.com").touch()

        # Create source J-Job files with ex-script invocations
        for i, app_name in enumerate(app_names):
            source_name = source_map[app_name]
            ex_script = ex_scripts[i]
            content = f'#!/bin/bash\n${{SCRglobal}}/{ex_script}\n'
            (jobs_dir / source_name).write_text(content)
            (scripts_dir / ex_script).touch()

        registry = PrefixRegistry.default()
        resolver = NameResolver(dev_root, registry)

        dag_filter = DAGFilter(
            dev_root=dev_root,
            workflow_yaml=workflow_yaml,
            platform="HERA",
            name_resolver=resolver,
        )

        # Act: compute full reachability
        result = dag_filter.compute_reachability()

        # Assert: jjob_source_map maps each app_name to its source_name
        for app_name in app_names:
            expected_source = source_map[app_name]
            assert app_name in result.jjob_source_map, (
                f"Application_Name '{app_name}' not found in jjob_source_map"
            )
            assert result.jjob_source_map[app_name] == expected_source, (
                f"Expected '{app_name}' → '{expected_source}' but got "
                f"'{result.jjob_source_map[app_name]}'"
            )

    @given(data=_dag_filter_workflow_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dag_filter_parses_source_file_for_dependencies(self, data: dict, tmp_path: Path):
        """DAG_Filter parses the source file (not the app-named file) for dependencies.

        # Feature: application-jjob-naming, Property 4: DAG Filter Resolution Integration
        **Validates: Requirements 4.1, 4.2, 4.3**
        """
        from deployment.dag_filter import DAGFilter, DAGReachabilitySet
        from deployment.name_resolver import NameResolver, PrefixRegistry

        workflow_yaml = data["workflow_yaml"]
        app_names = data["app_names"]
        source_map = data["source_map"]
        ex_scripts = data["ex_scripts"]

        # Setup filesystem
        dev_root = tmp_path
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(exist_ok=True)
        scripts_dir = dev_root / "scripts"
        scripts_dir.mkdir(exist_ok=True)
        ush_dir = dev_root / "ush"
        ush_dir.mkdir(exist_ok=True)
        config_dir = dev_root / "parm" / "config" / "gfs"
        config_dir.mkdir(parents=True, exist_ok=True)
        (config_dir / "config.base").touch()
        (config_dir / "config.base.j2").touch()
        (config_dir / "config.com").touch()

        # Create source J-Job files with KNOWN ex-script invocations
        for i, app_name in enumerate(app_names):
            source_name = source_map[app_name]
            ex_script = ex_scripts[i]
            # Source file has the ex-script reference
            source_content = f'#!/bin/bash\n${{SCRglobal}}/{ex_script}\n'
            (jobs_dir / source_name).write_text(source_content)
            (scripts_dir / ex_script).touch()

        # DO NOT create application-named files — this proves the DAG_Filter
        # is reading the SOURCE file, not the application-named file
        for app_name in app_names:
            app_path = jobs_dir / app_name
            assert not app_path.exists(), (
                f"Application-named file '{app_name}' should NOT exist on disk"
            )

        registry = PrefixRegistry.default()
        resolver = NameResolver(dev_root, registry)

        dag_filter = DAGFilter(
            dev_root=dev_root,
            workflow_yaml=workflow_yaml,
            platform="HERA",
            name_resolver=resolver,
        )

        # Act
        result = dag_filter.compute_reachability()

        # Assert: ex-scripts found by parsing SOURCE files are in the result
        for ex_script in ex_scripts:
            assert ex_script in result.ex_scripts, (
                f"Ex-script '{ex_script}' from source file should be in "
                f"reachability set but was not found. "
                f"result.ex_scripts = {result.ex_scripts}"
            )

    @given(data=_dag_filter_workflow_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_dag_filter_includes_both_names_in_reachability(self, data: dict, tmp_path: Path):
        """Both Application_Name and source_name are in the reachability set.

        # Feature: application-jjob-naming, Property 4: DAG Filter Resolution Integration
        **Validates: Requirements 4.1, 4.2, 4.3**
        """
        from deployment.dag_filter import DAGFilter, DAGReachabilitySet
        from deployment.name_resolver import NameResolver, PrefixRegistry

        workflow_yaml = data["workflow_yaml"]
        app_names = data["app_names"]
        source_map = data["source_map"]
        ex_scripts = data["ex_scripts"]

        # Setup filesystem
        dev_root = tmp_path
        jobs_dir = dev_root / "jobs"
        jobs_dir.mkdir(exist_ok=True)
        scripts_dir = dev_root / "scripts"
        scripts_dir.mkdir(exist_ok=True)
        ush_dir = dev_root / "ush"
        ush_dir.mkdir(exist_ok=True)
        config_dir = dev_root / "parm" / "config" / "gfs"
        config_dir.mkdir(parents=True, exist_ok=True)
        (config_dir / "config.base").touch()
        (config_dir / "config.base.j2").touch()
        (config_dir / "config.com").touch()

        # Create source J-Job files
        for i, app_name in enumerate(app_names):
            source_name = source_map[app_name]
            ex_script = ex_scripts[i]
            content = f'#!/bin/bash\n${{SCRglobal}}/{ex_script}\n'
            (jobs_dir / source_name).write_text(content)
            (scripts_dir / ex_script).touch()

        registry = PrefixRegistry.default()
        resolver = NameResolver(dev_root, registry)

        dag_filter = DAGFilter(
            dev_root=dev_root,
            workflow_yaml=workflow_yaml,
            platform="HERA",
            name_resolver=resolver,
        )

        # Act
        result = dag_filter.compute_reachability()

        # Assert: Application_Names are in result.jjobs (for EXPDIR staging)
        for app_name in app_names:
            assert app_name in result.jjobs, (
                f"Application_Name '{app_name}' should be in result.jjobs "
                f"but was not found: {result.jjobs}"
            )

        # Assert: source_names are accessible via jjob_source_map
        for app_name in app_names:
            source_name = source_map[app_name]
            assert app_name in result.jjob_source_map, (
                f"Application_Name '{app_name}' should have entry in "
                f"jjob_source_map"
            )
            assert result.jjob_source_map[app_name] == source_name, (
                f"jjob_source_map['{app_name}'] should be '{source_name}' "
                f"but got '{result.jjob_source_map[app_name]}'"
            )

        # Assert: the reachability set is valid (at least one J-Job)
        assert result.is_valid, "Reachability set should be valid with J-Jobs"



# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 3: Content Preservation on Rename
# ---------------------------------------------------------------------------


@st.composite
def _content_preservation_data(draw):
    """Generate random file content and application/source name pairs.

    Returns a dict with:
        - content: random binary content (1–10000 bytes)
        - application_name: a valid application-named J-Job
        - source_name: the corresponding source file name
        - suffix: the generated suffix
    """
    # Generate a valid suffix
    suffix = draw(_jjob_suffix())

    # Pick an application prefix that maps to JGLOBAL_ first
    app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_", "JGFS_", "JGDAS_", "JGEFS_", "JSFS_"]))

    application_name = app_prefix + suffix
    source_name = "JGLOBAL_" + suffix

    # Generate random binary content
    content = draw(st.binary(min_size=1, max_size=10000))

    return {
        "content": content,
        "application_name": application_name,
        "source_name": source_name,
        "suffix": suffix,
    }


class TestContentPreservationOnRename:
    """Property 3: Content Preservation on Rename.

    For any J-Job staged via rename-on-copy, the byte content of the
    destination file (EXPDIR/jobs/{application_name}) SHALL be identical
    to the byte content of the source file (dev/jobs/{source_name}).

    # Feature: application-jjob-naming, Property 3: Content Preservation on Rename
    **Validates: Requirements 3.1, 6.2**
    """

    @given(data=_content_preservation_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_staged_file_content_equals_source_content(self, data: dict, tmp_path: Path):
        """Byte content of destination file equals byte content of source file.

        # Feature: application-jjob-naming, Property 3: Content Preservation on Rename
        **Validates: Requirements 3.1, 6.2**
        """
        from deployment.file_stager import FileStager

        source_content = data["content"]
        application_name = data["application_name"]
        source_name = data["source_name"]

        # Setup project structure: project_root/dev/jobs/{source_name}
        project_root = tmp_path / "project"
        jobs_dir = project_root / "dev" / "jobs"
        jobs_dir.mkdir(parents=True, exist_ok=True)

        # Write random binary content to source file
        (jobs_dir / source_name).write_bytes(source_content)

        # Setup EXPDIR as empty destination
        expdir = tmp_path / "expdir"
        expdir.mkdir(parents=True, exist_ok=True)

        # Create FileStager instance
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )

        # Build resolution map: application_name → ResolvedName
        resolution_map = {
            application_name: ResolvedName(
                application_name=application_name,
                source_name=source_name,
                is_passthrough=False,
            ),
        }

        # Act: stage via rename-on-copy
        result = stager.stage_jjobs_with_rename(resolution_map)

        # Assert: file was staged
        assert result.files_copied == 1, (
            f"Expected 1 file copied but got {result.files_copied}"
        )

        # Assert: byte content of destination == byte content of source
        dst_file = expdir / "jobs" / application_name
        assert dst_file.exists(), (
            f"Destination file {dst_file} should exist after staging"
        )
        assert dst_file.read_bytes() == source_content, (
            f"Byte content of destination '{application_name}' does not match "
            f"source '{source_name}'. Content was not preserved during rename-on-copy."
        )

    @given(data=_content_preservation_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_passthrough_also_preserves_content(self, data: dict, tmp_path: Path):
        """Passthrough names (is_passthrough=True) also preserve byte content.

        # Feature: application-jjob-naming, Property 3: Content Preservation on Rename
        **Validates: Requirements 3.1, 6.2**
        """
        from deployment.file_stager import FileStager

        source_content = data["content"]
        # For passthrough, application_name == source_name
        source_name = data["source_name"]
        application_name = source_name  # passthrough: same name

        # Setup project structure
        project_root = tmp_path / "project"
        jobs_dir = project_root / "dev" / "jobs"
        jobs_dir.mkdir(parents=True, exist_ok=True)

        # Write random binary content to source file
        (jobs_dir / source_name).write_bytes(source_content)

        # Setup EXPDIR
        expdir = tmp_path / "expdir"
        expdir.mkdir(parents=True, exist_ok=True)

        # Create FileStager instance
        stager = FileStager(
            project_root=project_root,
            expdir=expdir,
            use_uwtools=False,
        )

        # Build resolution map: passthrough case
        resolution_map = {
            application_name: ResolvedName(
                application_name=application_name,
                source_name=source_name,
                is_passthrough=True,
            ),
        }

        # Act
        result = stager.stage_jjobs_with_rename(resolution_map)

        # Assert: content preserved for passthrough
        dst_file = expdir / "jobs" / application_name
        assert dst_file.exists(), (
            f"Destination file {dst_file} should exist after staging"
        )
        assert dst_file.read_bytes() == source_content, (
            f"Byte content of passthrough destination '{application_name}' "
            f"does not match source. Content was not preserved."
        )



# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 8: Unconditional Linking Script Staging
# ---------------------------------------------------------------------------


@st.composite
def _unconditional_staging_scenario(draw):
    """Generate various DAG-filter configurations for unconditional staging tests.

    Returns a dict with:
        - dag_filter_enabled: bool (whether DAG filtering is conceptually active)
        - jjob_names: set of J-Job names (to prove staging is independent of DAG content)
    """
    dag_filter_enabled = draw(st.booleans())

    # Generate a random set of J-Job names (could be empty, proving independence)
    num_jobs = draw(st.integers(min_value=0, max_value=5))
    jjob_names = set()
    for _ in range(num_jobs):
        prefix = draw(st.sampled_from(_APPLICATION_PREFIXES + _SHARED_PREFIXES))
        suffix = draw(_jjob_suffix())
        jjob_names.add(prefix + suffix)

    return {
        "dag_filter_enabled": dag_filter_enabled,
        "jjob_names": jjob_names,
    }


class TestUnconditionalLinkingScriptStaging:
    """Property 8: Unconditional Linking Script Staging.

    For any deployment (with or without --dag-filter enabled, and regardless of
    which Application_Names are in the YAML), the EXPDIR SHALL contain
    sorc/link_workflow.sh and sorc/ufs_utils.fd/fix/link_fixdirs.sh with
    executable permission bits preserved.

    # Feature: application-jjob-naming, Property 8: Unconditional Linking Script Staging
    **Validates: Requirements 9.1, 9.2, 9.5, 9.6**
    """

    @given(scenario=_unconditional_staging_scenario())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_unconditional_artifacts_always_staged(self, scenario: dict):
        """Unconditional artifacts are always staged regardless of DAG-filter config.

        # Feature: application-jjob-naming, Property 8: Unconditional Linking Script Staging
        **Validates: Requirements 9.1, 9.2, 9.5, 9.6**
        """
        from deployment.file_stager import FileStager

        dag_filter_enabled = scenario["dag_filter_enabled"]
        jjob_names = scenario["jjob_names"]

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup project root with unconditional artifacts
            project_root = tmp_path / "project"
            project_root.mkdir()

            # Create sorc/link_workflow.sh with executable content and 0755 perms
            link_workflow = project_root / "sorc" / "link_workflow.sh"
            link_workflow.parent.mkdir(parents=True, exist_ok=True)
            link_workflow.write_text("#!/bin/bash\n# Link workflow script\necho 'linking'\n")
            os.chmod(link_workflow, 0o755)

            # Create sorc/ufs_utils.fd/fix/link_fixdirs.sh with executable content and 0755
            link_fixdirs = project_root / "sorc" / "ufs_utils.fd" / "fix" / "link_fixdirs.sh"
            link_fixdirs.parent.mkdir(parents=True, exist_ok=True)
            link_fixdirs.write_text("#!/bin/bash\n# Link fixdirs script\necho 'fixing'\n")
            os.chmod(link_fixdirs, 0o755)

            # Create dev/jobs/ for completeness (with random J-Job files if any)
            jobs_dir = project_root / "dev" / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)
            for name in jjob_names:
                (jobs_dir / name).write_text("#!/bin/bash\n# placeholder\n")

            # Create empty destination EXPDIR
            expdir = tmp_path / "expdir"
            expdir.mkdir()

            # Instantiate FileStager
            stager = FileStager(project_root=project_root, expdir=expdir, use_uwtools=False)

            # Act: call stage_unconditional_artifacts (always called regardless of DAG)
            stager.stage_unconditional_artifacts()

            # Assert: link_workflow.sh exists in EXPDIR
            staged_link_workflow = expdir / "sorc" / "link_workflow.sh"
            assert staged_link_workflow.exists(), (
                f"EXPDIR should contain sorc/link_workflow.sh but it does not. "
                f"dag_filter_enabled={dag_filter_enabled}, jjobs={jjob_names}"
            )

            # Assert: link_fixdirs.sh exists in EXPDIR
            staged_link_fixdirs = expdir / "sorc" / "ufs_utils.fd" / "fix" / "link_fixdirs.sh"
            assert staged_link_fixdirs.exists(), (
                f"EXPDIR should contain sorc/ufs_utils.fd/fix/link_fixdirs.sh but "
                f"it does not. dag_filter_enabled={dag_filter_enabled}, jjobs={jjob_names}"
            )

            # Assert: executable permission bits preserved on link_workflow.sh
            mode_lw = staged_link_workflow.stat().st_mode
            assert mode_lw & 0o111 != 0, (
                f"sorc/link_workflow.sh should have executable bits but mode={oct(mode_lw)}"
            )

            # Assert: executable permission bits preserved on link_fixdirs.sh
            mode_lf = staged_link_fixdirs.stat().st_mode
            assert mode_lf & 0o111 != 0, (
                f"sorc/ufs_utils.fd/fix/link_fixdirs.sh should have executable "
                f"bits but mode={oct(mode_lf)}"
            )



# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 2: EXPDIR Naming Invariants
# ---------------------------------------------------------------------------


@st.composite
def _expdir_naming_deployment_data(draw):
    """Generate a set of application names from registered prefixes for staging.

    Returns:
        dict with:
        - app_names: list of application names (e.g., JGCAFS_FORECAST)
        - source_names: dict mapping app_name → source_name (JGLOBAL_*)
        - file_content: dict mapping source_name → bytes content
    """
    # Pick how many J-Jobs to stage (1 to 5)
    num_jobs = draw(st.integers(min_value=1, max_value=5))

    # Generate unique suffixes
    suffixes = draw(st.lists(
        _jjob_suffix(),
        min_size=num_jobs,
        max_size=num_jobs,
        unique=True,
    ))

    # For each suffix, pick an application prefix from the registered set
    app_prefixes = ["JGCAFS_", "JGCDAS_", "JGFS_", "JGDAS_", "JGEFS_", "JSFS_"]

    app_names = []
    source_names = {}
    file_content = {}

    for suffix in suffixes:
        app_prefix = draw(st.sampled_from(app_prefixes))
        app_name = app_prefix + suffix
        source_name = "JGLOBAL_" + suffix  # Source always uses JGLOBAL_

        app_names.append(app_name)
        source_names[app_name] = source_name

        # Generate random file content for the source
        content = draw(st.binary(min_size=10, max_size=200))
        file_content[source_name] = content

    return {
        "app_names": app_names,
        "source_names": source_names,
        "file_content": file_content,
    }


class TestEXPDIRNamingInvariantsProperty:
    """Property 2: EXPDIR Naming Invariants.

    For any workflow deployment that uses application naming, all files in
    the EXPDIR jobs/ directory SHALL have filenames that (a) conform to the
    ^J[A-Z][A-Z0-9_]*$ pattern and (b) contain no file with the JGLOBAL_
    prefix.

    # Feature: application-jjob-naming, Property 2: EXPDIR Naming Invariants
    **Validates: Requirements 3.2, 3.3, 6.1**
    """

    @given(data=_expdir_naming_deployment_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_expdir_filenames_match_jjob_pattern(self, data: dict):
        """All filenames in EXPDIR/jobs/ match ^J[A-Z][A-Z0-9_]*$ after staging.

        # Feature: application-jjob-naming, Property 2: EXPDIR Naming Invariants
        **Validates: Requirements 3.2, 3.3, 6.1**
        """
        import re
        from deployment.file_stager import FileStager

        app_names = data["app_names"]
        source_names = data["source_names"]
        file_content = data["file_content"]

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup: project root with dev/jobs/ containing source files
            project_root = tmp_path / "project"
            jobs_dir = project_root / "dev" / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create source files (JGLOBAL_*) in dev/jobs/
            for source_name, content in file_content.items():
                (jobs_dir / source_name).write_bytes(content)

            # Setup EXPDIR
            expdir = tmp_path / "expdir"
            expdir.mkdir(parents=True, exist_ok=True)

            # Build resolution map
            resolution_map = {}
            for app_name in app_names:
                source_name = source_names[app_name]
                resolution_map[app_name] = ResolvedName(
                    application_name=app_name,
                    source_name=source_name,
                    is_passthrough=False,
                )

            # Stage via FileStager
            stager = FileStager(
                project_root=project_root,
                expdir=expdir,
                use_uwtools=False,
            )
            stager.stage_jjobs_with_rename(resolution_map)

            # Assert: all filenames in EXPDIR/jobs/ match the pattern
            jjob_pattern = re.compile(r"^J[A-Z][A-Z0-9_]*$")
            staged_jobs_dir = expdir / "jobs"
            assert staged_jobs_dir.exists(), "EXPDIR/jobs/ should exist after staging"

            for filepath in staged_jobs_dir.iterdir():
                filename = filepath.name
                assert jjob_pattern.match(filename), (
                    f"File '{filename}' in EXPDIR/jobs/ does not match "
                    f"^J[A-Z][A-Z0-9_]*$ pattern"
                )

    @given(data=_expdir_naming_deployment_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_expdir_no_jglobal_prefix_files(self, data: dict):
        """No file in EXPDIR/jobs/ has the JGLOBAL_ prefix after application naming.

        # Feature: application-jjob-naming, Property 2: EXPDIR Naming Invariants
        **Validates: Requirements 3.2, 3.3, 6.1**
        """
        from deployment.file_stager import FileStager

        app_names = data["app_names"]
        source_names = data["source_names"]
        file_content = data["file_content"]

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup: project root with dev/jobs/ containing source files
            project_root = tmp_path / "project"
            jobs_dir = project_root / "dev" / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create source files (JGLOBAL_*) in dev/jobs/
            for source_name, content in file_content.items():
                (jobs_dir / source_name).write_bytes(content)

            # Setup EXPDIR
            expdir = tmp_path / "expdir"
            expdir.mkdir(parents=True, exist_ok=True)

            # Build resolution map — NO pass-through for JGLOBAL_ sources
            resolution_map = {}
            for app_name in app_names:
                source_name = source_names[app_name]
                resolution_map[app_name] = ResolvedName(
                    application_name=app_name,
                    source_name=source_name,
                    is_passthrough=False,
                )

            # Stage via FileStager
            stager = FileStager(
                project_root=project_root,
                expdir=expdir,
                use_uwtools=False,
            )
            stager.stage_jjobs_with_rename(resolution_map)

            # Assert: NO file in EXPDIR/jobs/ starts with JGLOBAL_
            staged_jobs_dir = expdir / "jobs"
            assert staged_jobs_dir.exists(), "EXPDIR/jobs/ should exist after staging"

            for filepath in staged_jobs_dir.iterdir():
                filename = filepath.name
                assert not filename.startswith("JGLOBAL_"), (
                    f"File '{filename}' in EXPDIR/jobs/ has JGLOBAL_ prefix — "
                    f"application naming should replace JGLOBAL_ with "
                    f"application-specific prefixes"
                )

    @given(data=_expdir_naming_deployment_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_expdir_naming_both_invariants_together(self, data: dict):
        """Combined invariant: all files match pattern AND none have JGLOBAL_ prefix.

        # Feature: application-jjob-naming, Property 2: EXPDIR Naming Invariants
        **Validates: Requirements 3.2, 3.3, 6.1**
        """
        import re
        from deployment.file_stager import FileStager

        app_names = data["app_names"]
        source_names = data["source_names"]
        file_content = data["file_content"]

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup: project root with dev/jobs/ containing source files
            project_root = tmp_path / "project"
            jobs_dir = project_root / "dev" / "jobs"
            jobs_dir.mkdir(parents=True, exist_ok=True)

            # Create source files (JGLOBAL_*) in dev/jobs/
            for source_name, content in file_content.items():
                (jobs_dir / source_name).write_bytes(content)

            # Setup EXPDIR
            expdir = tmp_path / "expdir"
            expdir.mkdir(parents=True, exist_ok=True)

            # Build resolution map
            resolution_map = {}
            for app_name in app_names:
                source_name = source_names[app_name]
                resolution_map[app_name] = ResolvedName(
                    application_name=app_name,
                    source_name=source_name,
                    is_passthrough=False,
                )

            # Stage via FileStager
            stager = FileStager(
                project_root=project_root,
                expdir=expdir,
                use_uwtools=False,
            )
            stager.stage_jjobs_with_rename(resolution_map)

            # Enumerate all files in EXPDIR/jobs/
            staged_jobs_dir = expdir / "jobs"
            assert staged_jobs_dir.exists(), "EXPDIR/jobs/ should exist after staging"

            jjob_pattern = re.compile(r"^J[A-Z][A-Z0-9_]*$")
            staged_filenames = [f.name for f in staged_jobs_dir.iterdir()]

            # Assert we staged the expected number of files
            assert len(staged_filenames) == len(app_names), (
                f"Expected {len(app_names)} files but found {len(staged_filenames)}: "
                f"{staged_filenames}"
            )

            for filename in staged_filenames:
                # Invariant (a): conforms to JAAAAA pattern
                assert jjob_pattern.match(filename), (
                    f"File '{filename}' in EXPDIR/jobs/ does not match "
                    f"^J[A-Z][A-Z0-9_]*$ pattern"
                )
                # Invariant (b): no JGLOBAL_ prefix
                assert not filename.startswith("JGLOBAL_"), (
                    f"File '{filename}' in EXPDIR/jobs/ has JGLOBAL_ prefix — "
                    f"application naming should produce application-specific names"
                )


# ---------------------------------------------------------------------------
# Feature: application-jjob-naming, Property 5: Deduplication and Distinction
# ---------------------------------------------------------------------------


@st.composite
def _dedup_resolution_map(draw):
    """Generate a resolution_map where a single application_name maps to a source.

    Since the resolution_map is a dict keyed on application_name, the same
    application_name naturally appears exactly once (dict deduplication).
    This strategy generates 1-4 unique application names, each resolving
    to a JGLOBAL_ source, to verify that the file stager produces exactly
    one file per application_name.

    Returns:
        tuple of (resolution_map, source_content_map) where:
        - resolution_map: dict[str, ResolvedName]
        - source_content_map: dict[source_name, bytes] for creating source files
    """
    num_entries = draw(st.integers(min_value=1, max_value=4))
    suffixes = draw(st.lists(
        _jjob_suffix(),
        min_size=num_entries,
        max_size=num_entries,
        unique=True,
    ))

    resolution_map = {}
    source_content_map = {}

    for suffix in suffixes:
        app_prefix = draw(st.sampled_from(["JGCAFS_", "JGCDAS_", "JGFS_"]))
        app_name = app_prefix + suffix
        source_name = "JGLOBAL_" + suffix

        resolution_map[app_name] = ResolvedName(
            application_name=app_name,
            source_name=source_name,
            is_passthrough=False,
        )

        # Generate unique content per source file
        content = draw(st.binary(min_size=10, max_size=200))
        source_content_map[source_name] = content

    return (resolution_map, source_content_map)


@st.composite
def _shared_source_distinction_data(draw):
    """Generate two different application_names that resolve to the same source.

    This tests the distinction property: two app names → same source → two
    distinct files with identical content.

    Returns:
        tuple of (resolution_map, shared_source_name, content) where:
        - resolution_map: dict with two entries pointing to the same source
        - shared_source_name: the common source name
        - content: bytes content for the shared source file
    """
    suffix = draw(_jjob_suffix())

    # Pick two DIFFERENT application prefixes
    prefixes = draw(st.lists(
        st.sampled_from(["JGCAFS_", "JGCDAS_", "JGFS_", "JGDAS_", "JGEFS_"]),
        min_size=2,
        max_size=2,
        unique=True,
    ))

    app_name_1 = prefixes[0] + suffix
    app_name_2 = prefixes[1] + suffix
    shared_source = "JGLOBAL_" + suffix

    # Generate file content
    content = draw(st.binary(min_size=10, max_size=200))

    resolution_map = {
        app_name_1: ResolvedName(
            application_name=app_name_1,
            source_name=shared_source,
            is_passthrough=False,
        ),
        app_name_2: ResolvedName(
            application_name=app_name_2,
            source_name=shared_source,
            is_passthrough=False,
        ),
    }

    return (resolution_map, shared_source, content)


class TestDeduplicationAndDistinctionProperty:
    """Property 5: Deduplication and Distinction.

    For any Workflow_YAML, (a) if the same Application_Name appears in multiple
    tasks, the EXPDIR SHALL contain exactly one file with that name; and (b) if
    two different Application_Names resolve to the same Shared_Source_Name, the
    EXPDIR SHALL contain two distinct files (one per Application_Name) with
    identical content.

    # Feature: application-jjob-naming, Property 5: Deduplication and Distinction
    **Validates: Requirements 3.4, 3.5**
    """

    @given(data=_dedup_resolution_map())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_same_application_name_staged_exactly_once(self, data: tuple):
        """Same application_name in the resolution_map produces exactly one file.

        The resolution_map is a dict keyed on application_name, so duplicate
        application_names are naturally deduplicated. The file stager must
        produce exactly one file per key in the EXPDIR.

        # Feature: application-jjob-naming, Property 5: Deduplication and Distinction
        **Validates: Requirements 3.4, 3.5**
        """
        resolution_map, source_content_map = data

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup project structure
            project_root = tmp_path / "project"
            dev_jobs = project_root / "dev" / "jobs"
            dev_jobs.mkdir(parents=True, exist_ok=True)
            expdir = tmp_path / "expdir"
            expdir.mkdir(exist_ok=True)

            # Create source files
            for source_name, content in source_content_map.items():
                (dev_jobs / source_name).write_bytes(content)

            # Create FileStager
            from deployment.file_stager import FileStager

            stager = FileStager(
                project_root=project_root,
                expdir=expdir,
                use_uwtools=False,
            )

            # Act: stage with rename
            result = stager.stage_jjobs_with_rename(resolution_map)

            # Assert: exactly one file per application_name in EXPDIR/jobs/
            jobs_target = expdir / "jobs"
            staged_files = list(jobs_target.iterdir()) if jobs_target.exists() else []
            staged_names = {f.name for f in staged_files}

            for app_name in resolution_map:
                assert app_name in staged_names, (
                    f"Application_Name '{app_name}' should be staged in "
                    f"EXPDIR/jobs/ but was not found. Staged: {staged_names}"
                )

            # Assert: number of files equals number of unique application_names
            assert len(staged_files) == len(resolution_map), (
                f"Expected exactly {len(resolution_map)} files (one per "
                f"application_name) but found {len(staged_files)}. "
                f"Staged: {staged_names}"
            )

            # Assert: files_copied count matches
            assert result.files_copied == len(resolution_map), (
                f"Expected files_copied={len(resolution_map)} but got "
                f"{result.files_copied}"
            )

    @given(data=_shared_source_distinction_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_different_app_names_same_source_produce_distinct_files(
        self, data: tuple
    ):
        """Two different application_names resolving to same source produce two files.

        When JGCAFS_FORECAST and JGCDAS_FORECAST both resolve to
        JGLOBAL_FORECAST, the EXPDIR SHALL contain both files with
        identical content.

        # Feature: application-jjob-naming, Property 5: Deduplication and Distinction
        **Validates: Requirements 3.4, 3.5**
        """
        resolution_map, shared_source, content = data

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup project structure
            project_root = tmp_path / "project"
            dev_jobs = project_root / "dev" / "jobs"
            dev_jobs.mkdir(parents=True, exist_ok=True)
            expdir = tmp_path / "expdir"
            expdir.mkdir(exist_ok=True)

            # Create the single shared source file
            (dev_jobs / shared_source).write_bytes(content)

            # Create FileStager
            from deployment.file_stager import FileStager

            stager = FileStager(
                project_root=project_root,
                expdir=expdir,
                use_uwtools=False,
            )

            # Act: stage with rename
            result = stager.stage_jjobs_with_rename(resolution_map)

            # Assert: two distinct files produced
            jobs_target = expdir / "jobs"
            assert jobs_target.exists(), "EXPDIR/jobs/ should exist after staging"

            staged_files = list(jobs_target.iterdir())
            staged_names = {f.name for f in staged_files}

            app_names = list(resolution_map.keys())
            assert len(app_names) == 2, "Test should generate exactly 2 app names"

            # Assert: both files exist
            for app_name in app_names:
                assert app_name in staged_names, (
                    f"Application_Name '{app_name}' should be in EXPDIR/jobs/ "
                    f"but found: {staged_names}"
                )

            # Assert: exactly 2 files (distinct)
            assert len(staged_files) == 2, (
                f"Expected 2 distinct files but found {len(staged_files)}: "
                f"{staged_names}"
            )

            # Assert: both files have identical content (same source)
            content_1 = (jobs_target / app_names[0]).read_bytes()
            content_2 = (jobs_target / app_names[1]).read_bytes()
            assert content_1 == content_2, (
                f"Two app names resolving to same source should have identical "
                f"content, but they differ"
            )

            # Assert: content matches original source
            assert content_1 == content, (
                f"Staged content should match source content but differs"
            )

            # Assert: files_copied is 2
            assert result.files_copied == 2, (
                f"Expected files_copied=2 but got {result.files_copied}"
            )

    @given(data=_shared_source_distinction_data())
    @settings(max_examples=100, suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_distinct_files_have_different_names(self, data: tuple):
        """Two files from same source must have different filenames (one per app name).

        This validates that the file stager uses the application_name as the
        destination filename, NOT the source_name.

        # Feature: application-jjob-naming, Property 5: Deduplication and Distinction
        **Validates: Requirements 3.4, 3.5**
        """
        resolution_map, shared_source, content = data

        with tempfile.TemporaryDirectory() as tmpdir:
            tmp_path = Path(tmpdir)

            # Setup project structure
            project_root = tmp_path / "project"
            dev_jobs = project_root / "dev" / "jobs"
            dev_jobs.mkdir(parents=True, exist_ok=True)
            expdir = tmp_path / "expdir"
            expdir.mkdir(exist_ok=True)

            # Create the shared source file
            (dev_jobs / shared_source).write_bytes(content)

            # Create FileStager
            from deployment.file_stager import FileStager

            stager = FileStager(
                project_root=project_root,
                expdir=expdir,
                use_uwtools=False,
            )

            # Act
            stager.stage_jjobs_with_rename(resolution_map)

            # Assert: the filenames in EXPDIR are the APPLICATION_NAMES, not source
            jobs_target = expdir / "jobs"
            staged_names = {f.name for f in jobs_target.iterdir()}

            # Source name should NOT appear (unless it coincides with an app name)
            app_names = set(resolution_map.keys())
            assert staged_names == app_names, (
                f"Staged filenames should be application names {app_names} "
                f"but got {staged_names}. Source name '{shared_source}' should "
                f"not appear as a filename unless it's also an app name."
            )

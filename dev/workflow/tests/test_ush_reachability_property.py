"""Property-based test: Transitive Ush Reachability (Property 3).

Generates random dependency graphs (including cycles) among shell scripts,
creates a temporary filesystem with ex-scripts and ush scripts containing
source statements, runs DAGFilter.extract_ush_scripts(), and verifies the
result matches an independently computed transitive closure.

**Validates: Requirements 3.1, 3.2, 3.3, 3.4**

Traces to: Design Document - Correctness Property 3
  "For any dependency graph of source relationships among shell scripts,
   the DAG_Filter's ush script extraction SHALL produce exactly the transitive
   closure of scripts reachable from the seed ex-scripts, terminating correctly
   even in the presence of cycles."
"""

from __future__ import annotations

import os
import sys
import tempfile
from collections import deque
from pathlib import Path

from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.dag_filter import DAGFilter


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating random dependency graphs
# ---------------------------------------------------------------------------


@st.composite
def _random_ush_dependency_graph(draw):
    """Generate a random dependency graph of ush scripts sourced by ex-scripts.

    Produces:
    - A list of ex-script names (seeds)
    - A list of ush script names (potential targets)
    - Edges from ex-scripts to ush scripts (seed edges)
    - Edges among ush scripts (inter-ush edges, may include cycles)

    Strategy:
    - Generate 1-5 ex-scripts
    - Generate 1-10 ush scripts
    - Generate seed edges: each ex-script sources 0-4 ush scripts
    - Generate inter-ush edges: 0-15 edges between ush scripts (allows cycles)
    """
    num_ex = draw(st.integers(min_value=1, max_value=5))
    num_ush = draw(st.integers(min_value=1, max_value=10))

    ex_names = [f"exglobal_task_{i}.sh" for i in range(num_ex)]
    ush_names = [f"ush_util_{i}.sh" for i in range(num_ush)]

    # Seed edges: ex-scripts -> ush scripts
    seed_edges: list[tuple[str, str]] = []
    for ex in ex_names:
        num_deps = draw(st.integers(min_value=0, max_value=min(4, num_ush)))
        targets = draw(
            st.lists(
                st.integers(min_value=0, max_value=num_ush - 1),
                min_size=num_deps,
                max_size=num_deps,
                unique=True,
            )
        )
        for t in targets:
            seed_edges.append((ex, ush_names[t]))

    # Inter-ush edges (may include cycles)
    num_inter_edges = draw(st.integers(min_value=0, max_value=15))
    inter_edges: list[tuple[str, str]] = []
    for _ in range(num_inter_edges):
        src_idx = draw(st.integers(min_value=0, max_value=num_ush - 1))
        tgt_idx = draw(st.integers(min_value=0, max_value=num_ush - 1))
        inter_edges.append((ush_names[src_idx], ush_names[tgt_idx]))

    return ex_names, ush_names, seed_edges, inter_edges


def _compute_expected_transitive_closure(
    ush_names: list[str],
    seed_edges: list[tuple[str, str]],
    inter_edges: list[tuple[str, str]],
) -> set[str]:
    """Independently compute the transitive closure from seed edges.

    Uses BFS starting from ush scripts directly referenced by ex-scripts,
    following inter-ush edges transitively. Handles cycles via a visited set.

    Returns:
        Set of all ush script names reachable from any ex-script.
    """
    # Build adjacency list for inter-ush edges
    adj: dict[str, set[str]] = {name: set() for name in ush_names}
    for src, tgt in inter_edges:
        adj[src].add(tgt)

    # Collect initial seeds (ush scripts directly sourced by ex-scripts)
    seeds: set[str] = set()
    for _, ush in seed_edges:
        seeds.add(ush)

    # BFS from seeds through the inter-ush graph
    visited: set[str] = set(seeds)
    queue: deque[str] = deque(seeds)

    while queue:
        current = queue.popleft()
        for dep in adj.get(current, set()):
            if dep not in visited:
                visited.add(dep)
                queue.append(dep)

    return visited


def _create_temp_filesystem(
    tmp_dir: Path,
    ex_names: list[str],
    ush_names: list[str],
    seed_edges: list[tuple[str, str]],
    inter_edges: list[tuple[str, str]],
) -> Path:
    """Create a temporary filesystem structure with ex-scripts and ush scripts.

    Creates:
    - tmp_dir/scripts/  with ex-script files containing source statements
    - tmp_dir/ush/      with ush script files containing source statements
    - tmp_dir/jobs/     (empty, required by DAGFilter)

    Returns:
        Path to the dev_root (tmp_dir).
    """
    scripts_dir = tmp_dir / "scripts"
    ush_dir = tmp_dir / "ush"
    jobs_dir = tmp_dir / "jobs"
    scripts_dir.mkdir(parents=True, exist_ok=True)
    ush_dir.mkdir(parents=True, exist_ok=True)
    jobs_dir.mkdir(parents=True, exist_ok=True)

    # Build edge maps: ex -> list of ush deps, ush -> list of ush deps
    ex_deps: dict[str, list[str]] = {ex: [] for ex in ex_names}
    for ex, ush in seed_edges:
        ex_deps[ex].append(ush)

    ush_deps: dict[str, list[str]] = {ush: [] for ush in ush_names}
    for src, tgt in inter_edges:
        if tgt not in ush_deps[src]:  # avoid duplicate lines
            ush_deps[src].append(tgt)

    # Write ex-script files
    for ex_name, deps in ex_deps.items():
        lines = ["#!/bin/bash\n"]
        for i, dep in enumerate(deps):
            # Alternate between source and dot-source patterns
            if i % 2 == 0:
                lines.append(f'source "${{USHglobal}}/{dep}"\n')
            else:
                lines.append(f'. "${{USHglobal}}/{dep}"\n')
        (scripts_dir / ex_name).write_text("".join(lines))

    # Write ush script files
    for ush_name, deps in ush_deps.items():
        lines = ["#!/bin/bash\n"]
        for i, dep in enumerate(deps):
            if i % 2 == 0:
                lines.append(f'source "${{USHglobal}}/{dep}"\n')
            else:
                lines.append(f'. "${{USHglobal}}/{dep}"\n')
        (ush_dir / ush_name).write_text("".join(lines))

    return tmp_dir


# ---------------------------------------------------------------------------
# Property Test: Transitive Ush Reachability (Property 3)
# ---------------------------------------------------------------------------


@given(graph=_random_ush_dependency_graph())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_ush_transitive_reachability_property(graph):
    """Property 3: extract_ush_scripts() produces exactly the transitive closure.

    **Validates: Requirements 3.1, 3.2, 3.3, 3.4**

    For any random dependency graph of ush scripts (including cycles):
    - The result must equal the expected transitive closure computed independently
    - The function must terminate (no infinite loops from cycles)
    - Circular dependencies must be handled gracefully (warnings emitted)
    """
    ex_names, ush_names, seed_edges, inter_edges = graph

    with tempfile.TemporaryDirectory() as tmp_str:
        tmp_dir = Path(tmp_str)
        dev_root = _create_temp_filesystem(
            tmp_dir, ex_names, ush_names, seed_edges, inter_edges
        )

        # Compute expected result independently
        expected = _compute_expected_transitive_closure(
            ush_names, seed_edges, inter_edges
        )

        # Run the DAGFilter's extract_ush_scripts
        dag_filter = DAGFilter(
            dev_root=dev_root,
            workflow_yaml={"families": []},
            platform="HERA",
        )
        actual = dag_filter.extract_ush_scripts(set(ex_names))

        # Property: actual result must equal the expected transitive closure
        assert actual == expected, (
            f"Transitive closure mismatch.\n"
            f"  Expected: {sorted(expected)}\n"
            f"  Actual:   {sorted(actual)}\n"
            f"  Missing from actual: {sorted(expected - actual)}\n"
            f"  Extra in actual:     {sorted(actual - expected)}\n"
            f"  Ex-scripts: {ex_names}\n"
            f"  Ush-scripts: {ush_names}\n"
            f"  Seed edges: {seed_edges}\n"
            f"  Inter edges: {inter_edges}"
        )


@given(graph=_random_ush_dependency_graph())
@settings(
    max_examples=100,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_ush_cycles_terminate_with_warnings(graph):
    """Property 3 (cycle handling): cycles don't cause infinite loops and emit warnings.

    **Validates: Requirements 3.1, 3.2, 3.3, 3.4**

    For any random dependency graph containing cycles:
    - The function must terminate within the test deadline
    - If cycles exist among reachable scripts, warnings must be emitted
    """
    ex_names, ush_names, seed_edges, inter_edges = graph

    with tempfile.TemporaryDirectory() as tmp_str:
        tmp_dir = Path(tmp_str)
        dev_root = _create_temp_filesystem(
            tmp_dir, ex_names, ush_names, seed_edges, inter_edges
        )

        dag_filter = DAGFilter(
            dev_root=dev_root,
            workflow_yaml={"families": []},
            platform="HERA",
        )
        # The function must terminate (no infinite loop) — if it doesn't,
        # the test will be killed by the deadline/timeout
        result = dag_filter.extract_ush_scripts(set(ex_names))

        # Verify result is a set of strings
        assert isinstance(result, set)
        for item in result:
            assert isinstance(item, str)

        # Check for cycle warnings if cycles exist among reachable nodes
        expected_reachable = _compute_expected_transitive_closure(
            ush_names, seed_edges, inter_edges
        )

        # Detect if there are cycles among reachable ush scripts
        # A cycle exists if any inter-edge goes to an already-visited node
        # that is in the reachable set
        reachable_with_back_edges = _has_cycles_in_reachable(
            ush_names, seed_edges, inter_edges, expected_reachable
        )

        if reachable_with_back_edges:
            # If cycles exist among reachable scripts, warnings should be present
            circular_warnings = [
                w for w in dag_filter._warnings
                if "Circular dependency" in w
            ]
            assert len(circular_warnings) > 0, (
                f"Expected circular dependency warnings but got none.\n"
                f"Warnings: {dag_filter._warnings}\n"
                f"Inter edges among reachable: "
                f"{[(s, t) for s, t in inter_edges if s in expected_reachable and t in expected_reachable]}"
            )


def _has_cycles_in_reachable(
    ush_names: list[str],
    seed_edges: list[tuple[str, str]],
    inter_edges: list[tuple[str, str]],
    reachable: set[str],
) -> bool:
    """Detect if there are back-edges (cycles) among reachable ush scripts.

    Simulates the BFS that extract_ush_scripts performs and checks if any
    edge leads to a node that has already been visited (which would trigger
    the circular dependency warning).
    """
    # Build adjacency list for inter-ush edges (deduplicated, matching
    # _create_temp_filesystem which writes each target only once per source)
    adj: dict[str, list[str]] = {name: [] for name in ush_names}
    for src, tgt in inter_edges:
        if tgt not in adj[src]:
            adj[src].append(tgt)

    # Collect initial seeds
    seeds: set[str] = set()
    for _, ush in seed_edges:
        seeds.add(ush)

    # Simulate BFS and detect back-edges
    visited: set[str] = set(seeds)
    queue: deque[str] = deque(seeds)

    while queue:
        current = queue.popleft()
        # Only process if the file would "exist" on disk
        if current not in set(ush_names):
            continue
        for dep in adj.get(current, []):
            if dep in visited:
                # This is a back-edge — would trigger circular warning
                return True
            visited.add(dep)
            queue.append(dep)

    return False

"""Unit tests for DAG.validate_acyclic().

Tests the acyclicity validation including:
- Valid acyclic DAGs pass without error
- Cyclic DAGs raise CycleDetectedError with the cycle path
- Empty DAGs and single-node DAGs are valid

Traces to: Requirements 2.2
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import (
    CycleDetectedError,
    DAG,
    Edge,
    TaskNode,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_dag(nodes: list[str], edges: list[tuple[str, str]]) -> DAG:
    """Create a DAG with the given node names and edge pairs."""
    dag = DAG(suite_name="test")
    for name in nodes:
        dag.nodes[name] = TaskNode(
            name=name.split("/")[-1],
            family_path="/".join(name.split("/")[:-1]) or "root",
            jjob=f"J_{name.upper().replace('/', '_')}",
        )
    for src, tgt in edges:
        dag.edges.append(Edge(source=src, target=tgt))
    return dag


# ---------------------------------------------------------------------------
# Tests: Acyclic DAGs (should pass)
# ---------------------------------------------------------------------------


class TestValidateAcyclicPass:
    """Tests for DAGs that are valid (no cycles)."""

    def test_empty_dag(self):
        """An empty DAG has no cycles."""
        dag = DAG(suite_name="empty")
        dag.validate_acyclic()  # Should not raise

    def test_single_node_no_edges(self):
        """A single node with no edges is acyclic."""
        dag = _make_dag(["root/task"], [])
        dag.validate_acyclic()  # Should not raise

    def test_linear_chain(self):
        """A linear chain A -> B -> C is acyclic."""
        dag = _make_dag(
            ["app/a", "app/b", "app/c"],
            [("app/a", "app/b"), ("app/b", "app/c")],
        )
        dag.validate_acyclic()  # Should not raise

    def test_diamond_dag(self):
        """A diamond shape A -> B, A -> C, B -> D, C -> D is acyclic."""
        dag = _make_dag(
            ["app/a", "app/b", "app/c", "app/d"],
            [
                ("app/a", "app/b"),
                ("app/a", "app/c"),
                ("app/b", "app/d"),
                ("app/c", "app/d"),
            ],
        )
        dag.validate_acyclic()  # Should not raise

    def test_disconnected_components(self):
        """Disconnected acyclic components are valid."""
        dag = _make_dag(
            ["comp1/a", "comp1/b", "comp2/x", "comp2/y"],
            [("comp1/a", "comp1/b"), ("comp2/x", "comp2/y")],
        )
        dag.validate_acyclic()  # Should not raise


# ---------------------------------------------------------------------------
# Tests: Cyclic DAGs (should raise CycleDetectedError)
# ---------------------------------------------------------------------------


class TestValidateAcyclicFail:
    """Tests for DAGs that contain cycles."""

    def test_self_loop(self):
        """A self-loop A -> A is a cycle."""
        dag = _make_dag(["app/a"], [("app/a", "app/a")])
        with pytest.raises(CycleDetectedError) as exc_info:
            dag.validate_acyclic()
        assert "app/a" in exc_info.value.cycle_path

    def test_two_node_cycle(self):
        """A -> B -> A is a cycle."""
        dag = _make_dag(
            ["app/a", "app/b"],
            [("app/a", "app/b"), ("app/b", "app/a")],
        )
        with pytest.raises(CycleDetectedError) as exc_info:
            dag.validate_acyclic()
        # The cycle path should contain both nodes
        assert len(exc_info.value.cycle_path) >= 2

    def test_three_node_cycle(self):
        """A -> B -> C -> A is a cycle."""
        dag = _make_dag(
            ["app/a", "app/b", "app/c"],
            [("app/a", "app/b"), ("app/b", "app/c"), ("app/c", "app/a")],
        )
        with pytest.raises(CycleDetectedError) as exc_info:
            dag.validate_acyclic()
        cycle = exc_info.value.cycle_path
        assert len(cycle) >= 2
        # All nodes in the cycle should be from our graph
        for node in cycle:
            assert node in ["app/a", "app/b", "app/c"]

    def test_cycle_in_larger_graph(self):
        """A cycle embedded in a larger graph is detected."""
        dag = _make_dag(
            ["app/start", "app/a", "app/b", "app/c", "app/end"],
            [
                ("app/start", "app/a"),
                ("app/a", "app/b"),
                ("app/b", "app/c"),
                ("app/c", "app/a"),  # cycle: a -> b -> c -> a
                ("app/c", "app/end"),
            ],
        )
        with pytest.raises(CycleDetectedError) as exc_info:
            dag.validate_acyclic()
        cycle = exc_info.value.cycle_path
        # The cycle should involve a, b, c
        cycle_set = set(cycle)
        assert cycle_set.issubset({"app/a", "app/b", "app/c"})

    def test_error_message_contains_cycle(self):
        """The error message shows the cycle path with arrows."""
        dag = _make_dag(
            ["app/x", "app/y"],
            [("app/x", "app/y"), ("app/y", "app/x")],
        )
        with pytest.raises(CycleDetectedError) as exc_info:
            dag.validate_acyclic()
        msg = str(exc_info.value)
        assert "Cycle detected" in msg
        assert "→" in msg

    def test_cycle_path_attribute(self):
        """CycleDetectedError exposes cycle_path as a list."""
        dag = _make_dag(
            ["app/a", "app/b", "app/c"],
            [("app/a", "app/b"), ("app/b", "app/c"), ("app/c", "app/a")],
        )
        with pytest.raises(CycleDetectedError) as exc_info:
            dag.validate_acyclic()
        assert isinstance(exc_info.value.cycle_path, list)
        assert len(exc_info.value.cycle_path) >= 2

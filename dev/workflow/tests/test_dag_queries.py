"""Unit tests for DAG.downstream() and DAG.upstream() query functions.

Tests with known DAG topologies:
- Linear chain (A→B→C)
- Diamond (A→B, A→C, B→D, C→D)
- Disconnected components
- Single node (no edges)

Traces to: Requirements 2.8
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import DAG, Edge, TaskNode


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
# Tests: Linear Chain (A → B → C)
# ---------------------------------------------------------------------------


class TestLinearChain:
    """Tests for downstream/upstream on a linear chain A → B → C."""

    @pytest.fixture
    def dag(self):
        return _make_dag(
            ["app/a", "app/b", "app/c"],
            [("app/a", "app/b"), ("app/b", "app/c")],
        )

    def test_downstream_from_root(self, dag):
        """downstream(A) returns {B, C} — all nodes reachable forward."""
        result = dag.downstream("app/a")
        assert result == {"app/b", "app/c"}

    def test_downstream_from_middle(self, dag):
        """downstream(B) returns {C} — only the tail."""
        result = dag.downstream("app/b")
        assert result == {"app/c"}

    def test_downstream_from_leaf(self, dag):
        """downstream(C) returns empty set — no successors."""
        result = dag.downstream("app/c")
        assert result == set()

    def test_upstream_from_leaf(self, dag):
        """upstream(C) returns {A, B} — all nodes reachable backward."""
        result = dag.upstream("app/c")
        assert result == {"app/a", "app/b"}

    def test_upstream_from_middle(self, dag):
        """upstream(B) returns {A} — only the root."""
        result = dag.upstream("app/b")
        assert result == {"app/a"}

    def test_upstream_from_root(self, dag):
        """upstream(A) returns empty set — no predecessors."""
        result = dag.upstream("app/a")
        assert result == set()


# ---------------------------------------------------------------------------
# Tests: Diamond (A → B, A → C, B → D, C → D)
# ---------------------------------------------------------------------------


class TestDiamond:
    """Tests for downstream/upstream on a diamond DAG."""

    @pytest.fixture
    def dag(self):
        return _make_dag(
            ["app/a", "app/b", "app/c", "app/d"],
            [
                ("app/a", "app/b"),
                ("app/a", "app/c"),
                ("app/b", "app/d"),
                ("app/c", "app/d"),
            ],
        )

    def test_downstream_from_root(self, dag):
        """downstream(A) returns {B, C, D} — all reachable nodes."""
        result = dag.downstream("app/a")
        assert result == {"app/b", "app/c", "app/d"}

    def test_downstream_from_branch(self, dag):
        """downstream(B) returns {D} — converges at D."""
        result = dag.downstream("app/b")
        assert result == {"app/d"}

    def test_downstream_from_sink(self, dag):
        """downstream(D) returns empty set — D is the sink."""
        result = dag.downstream("app/d")
        assert result == set()

    def test_upstream_from_sink(self, dag):
        """upstream(D) returns {A, B, C} — all ancestors."""
        result = dag.upstream("app/d")
        assert result == {"app/a", "app/b", "app/c"}

    def test_upstream_from_branch(self, dag):
        """upstream(B) returns {A} — only the root."""
        result = dag.upstream("app/b")
        assert result == {"app/a"}

    def test_upstream_from_root(self, dag):
        """upstream(A) returns empty set — A is the source."""
        result = dag.upstream("app/a")
        assert result == set()


# ---------------------------------------------------------------------------
# Tests: Disconnected Components
# ---------------------------------------------------------------------------


class TestDisconnectedComponents:
    """Tests for downstream/upstream on disconnected graph components."""

    @pytest.fixture
    def dag(self):
        """Two disconnected chains: (X→Y→Z) and (P→Q)."""
        return _make_dag(
            ["comp1/x", "comp1/y", "comp1/z", "comp2/p", "comp2/q"],
            [
                ("comp1/x", "comp1/y"),
                ("comp1/y", "comp1/z"),
                ("comp2/p", "comp2/q"),
            ],
        )

    def test_downstream_stays_in_component(self, dag):
        """downstream(X) only returns nodes in its own component."""
        result = dag.downstream("comp1/x")
        assert result == {"comp1/y", "comp1/z"}
        # No nodes from comp2
        assert "comp2/p" not in result
        assert "comp2/q" not in result

    def test_upstream_stays_in_component(self, dag):
        """upstream(Z) only returns nodes in its own component."""
        result = dag.upstream("comp1/z")
        assert result == {"comp1/x", "comp1/y"}
        # No nodes from comp2
        assert "comp2/p" not in result
        assert "comp2/q" not in result

    def test_downstream_other_component(self, dag):
        """downstream(P) returns {Q} — only its own component."""
        result = dag.downstream("comp2/p")
        assert result == {"comp2/q"}

    def test_upstream_other_component(self, dag):
        """upstream(Q) returns {P} — only its own component."""
        result = dag.upstream("comp2/q")
        assert result == {"comp2/p"}

    def test_isolated_node_not_in_other_results(self, dag):
        """Nodes in one component never appear in another's queries."""
        downstream_x = dag.downstream("comp1/x")
        upstream_z = dag.upstream("comp1/z")
        downstream_p = dag.downstream("comp2/p")
        upstream_q = dag.upstream("comp2/q")

        comp1_nodes = {"comp1/x", "comp1/y", "comp1/z"}
        comp2_nodes = {"comp2/p", "comp2/q"}

        # comp2 nodes never in comp1 results
        assert downstream_x.isdisjoint(comp2_nodes)
        assert upstream_z.isdisjoint(comp2_nodes)

        # comp1 nodes never in comp2 results
        assert downstream_p.isdisjoint(comp1_nodes)
        assert upstream_q.isdisjoint(comp1_nodes)


# ---------------------------------------------------------------------------
# Tests: Single Node (no edges)
# ---------------------------------------------------------------------------


class TestSingleNode:
    """Tests for downstream/upstream on a single isolated node."""

    @pytest.fixture
    def dag(self):
        return _make_dag(["solo/task"], [])

    def test_downstream_empty(self, dag):
        """downstream of a single node with no edges returns empty set."""
        result = dag.downstream("solo/task")
        assert result == set()

    def test_upstream_empty(self, dag):
        """upstream of a single node with no edges returns empty set."""
        result = dag.upstream("solo/task")
        assert result == set()


# ---------------------------------------------------------------------------
# Tests: Task Not In Graph (KeyError handling)
# ---------------------------------------------------------------------------


class TestTaskNotInGraph:
    """Tests that downstream/upstream raise KeyError for unknown tasks."""

    @pytest.fixture
    def dag(self):
        return _make_dag(
            ["app/a", "app/b"],
            [("app/a", "app/b")],
        )

    def test_downstream_raises_key_error(self, dag):
        """downstream raises KeyError when task is not in the DAG."""
        with pytest.raises(KeyError, match="nonexistent/task"):
            dag.downstream("nonexistent/task")

    def test_upstream_raises_key_error(self, dag):
        """upstream raises KeyError when task is not in the DAG."""
        with pytest.raises(KeyError, match="nonexistent/task"):
            dag.upstream("nonexistent/task")

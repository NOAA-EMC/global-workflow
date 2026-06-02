"""Property-based test: DAG Acyclicity (Property 12).

Generates random directed graphs using hypothesis, builds a DAG object from
the generated nodes and edges, then asserts that `validate_acyclic()` raises
`CycleDetectedError` if and only if `networkx.is_directed_acyclic_graph()`
returns False for the same graph structure.

**Validates: Requirements 2.2**

Traces to: Design Document - Correctness Property 12
  "The dependency graph contains no cycles."
  "validate_acyclic() runs topological sort; raises CycleDetectedError on failure."
"""

from __future__ import annotations

import os
import sys

import networkx as nx
from hypothesis import given, settings, HealthCheck
from hypothesis import strategies as st

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.workflow_config import CycleDetectedError, DAG, Edge, TaskNode


# ---------------------------------------------------------------------------
# Hypothesis Strategies for generating random directed graphs
# ---------------------------------------------------------------------------


@st.composite
def _random_directed_graph(draw):
    """Generate a random directed graph as (nodes, edges).

    Produces a list of node names and a list of (source, target) edge tuples.
    The graph may or may not contain cycles — the property test checks both cases.

    Strategy:
    - Generate 1-15 nodes with unique names
    - Generate 0-30 directed edges between those nodes (allowing self-loops)
    - This covers acyclic graphs, cyclic graphs, disconnected components,
      self-loops, and dense/sparse topologies
    """
    num_nodes = draw(st.integers(min_value=1, max_value=15))
    node_names = [f"family/task_{i}" for i in range(num_nodes)]

    # Generate edges as pairs of indices into the node list
    edges = draw(
        st.lists(
            st.tuples(
                st.integers(min_value=0, max_value=num_nodes - 1),
                st.integers(min_value=0, max_value=num_nodes - 1),
            ),
            min_size=0,
            max_size=30,
        )
    )

    # Convert index pairs to named edge tuples (deduplicate)
    edge_pairs = list({(node_names[src], node_names[tgt]) for src, tgt in edges})

    return node_names, edge_pairs


def _build_dag(node_names: list[str], edge_pairs: list[tuple[str, str]]) -> DAG:
    """Build a DAG object from node names and edge pairs."""
    dag = DAG(suite_name="test_acyclicity")
    for name in node_names:
        task_name = name.split("/")[-1]
        family_path = "/".join(name.split("/")[:-1]) or "root"
        dag.nodes[name] = TaskNode(
            name=task_name,
            family_path=family_path,
            jjob=f"J_{task_name.upper()}",
        )
    for src, tgt in edge_pairs:
        dag.edges.append(Edge(source=src, target=tgt))
    return dag


def _build_nx_digraph(node_names: list[str], edge_pairs: list[tuple[str, str]]) -> nx.DiGraph:
    """Build a networkx DiGraph from node names and edge pairs (independent of DAG)."""
    g = nx.DiGraph()
    for name in node_names:
        g.add_node(name)
    for src, tgt in edge_pairs:
        g.add_edge(src, tgt)
    return g


# ---------------------------------------------------------------------------
# Property Test: DAG Acyclicity (Property 12)
# ---------------------------------------------------------------------------


@given(graph=_random_directed_graph())
@settings(
    max_examples=200,
    deadline=None,
    suppress_health_check=[HealthCheck.too_slow],
)
def test_dag_acyclicity_property(graph):
    """Property 12: validate_acyclic() raises CycleDetectedError iff the graph has a cycle.

    **Validates: Requirements 2.2**

    For any random directed graph:
    - If networkx determines the graph is acyclic, validate_acyclic() must NOT raise
    - If networkx determines the graph has a cycle, validate_acyclic() MUST raise CycleDetectedError
    """
    node_names, edge_pairs = graph

    # Build the DAG object under test
    dag = _build_dag(node_names, edge_pairs)

    # Build an independent networkx DiGraph as the oracle
    nx_graph = _build_nx_digraph(node_names, edge_pairs)
    is_acyclic = nx.is_directed_acyclic_graph(nx_graph)

    if is_acyclic:
        # The graph has no cycles — validate_acyclic() should NOT raise
        dag.validate_acyclic()
    else:
        # The graph has a cycle — validate_acyclic() MUST raise CycleDetectedError
        try:
            dag.validate_acyclic()
            assert False, (
                "validate_acyclic() did not raise CycleDetectedError "
                "for a graph that networkx identifies as cyclic.\n"
                f"Nodes: {node_names}\n"
                f"Edges: {edge_pairs}"
            )
        except CycleDetectedError as e:
            # Verify the error contains a valid cycle path
            assert isinstance(e.cycle_path, list), "cycle_path should be a list"
            assert len(e.cycle_path) >= 1, "cycle_path should have at least one node"
            # All nodes in the reported cycle should be from our graph
            for node in e.cycle_path:
                assert node in node_names, (
                    f"Cycle path contains unknown node '{node}'. "
                    f"Known nodes: {node_names}"
                )

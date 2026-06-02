"""Unit tests for the deploy-time variable registry.

Tests that the registry contains the required 15 variables, that
get_deploy_time_values() correctly extracts only registered variables
from a context dict, and that missing keys are silently skipped.

Validates: Requirements 11.1, 11.3, 11.4
"""

from __future__ import annotations

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.deploy_time_vars import (
    DEPLOY_TIME_REGISTRY,
    DeployTimeVariable,
    get_deploy_time_values,
)


# ---------------------------------------------------------------------------
# Required variable names per Requirement 11.3
# ---------------------------------------------------------------------------

REQUIRED_VARIABLES = {
    "RUN",
    "NET",
    "CASE",
    "CASE_ENS",
    "MACHINE",
    "CDUMP",
    "NMEM_ENS",
    "APP",
    "CCPP_SUITE",
    "DO_COUPLED",
    "DO_WAVE",
    "DO_OCN",
    "DO_ICE",
    "DO_AERO",
    "REPLAY_ICS",
}


# ---------------------------------------------------------------------------
# Tests for DEPLOY_TIME_REGISTRY
# ---------------------------------------------------------------------------


class TestDeployTimeRegistry:
    """Tests for the DEPLOY_TIME_REGISTRY list."""

    def test_registry_contains_all_15_required_variables(self):
        """All 15 required deploy-time variables are present in the registry."""
        registry_names = {var.name for var in DEPLOY_TIME_REGISTRY}
        assert REQUIRED_VARIABLES.issubset(registry_names), (
            f"Missing variables: {REQUIRED_VARIABLES - registry_names}"
        )

    def test_registry_has_exactly_15_entries(self):
        """The registry contains exactly 15 entries (no extras, no gaps)."""
        assert len(DEPLOY_TIME_REGISTRY) == 15

    def test_registry_entries_are_deploy_time_variable_instances(self):
        """Every entry in the registry is a DeployTimeVariable dataclass."""
        for var in DEPLOY_TIME_REGISTRY:
            assert isinstance(var, DeployTimeVariable)

    def test_registry_entries_have_valid_sources(self):
        """Every entry has a source of 'workflow_yaml', 'platform', or 'derived'."""
        valid_sources = {"workflow_yaml", "platform", "derived"}
        for var in DEPLOY_TIME_REGISTRY:
            assert var.source in valid_sources, (
                f"{var.name} has invalid source: {var.source}"
            )

    def test_registry_is_deterministic(self):
        """Repeated access to the registry yields the same order."""
        names_first = [var.name for var in DEPLOY_TIME_REGISTRY]
        names_second = [var.name for var in DEPLOY_TIME_REGISTRY]
        assert names_first == names_second

    def test_registry_names_are_unique(self):
        """No duplicate variable names in the registry."""
        names = [var.name for var in DEPLOY_TIME_REGISTRY]
        assert len(names) == len(set(names)), "Duplicate names found in registry"


# ---------------------------------------------------------------------------
# Tests for get_deploy_time_values()
# ---------------------------------------------------------------------------


class TestGetDeployTimeValues:
    """Tests for the get_deploy_time_values() function."""

    def test_extracts_only_registered_variables(self):
        """Only variables in the registry are extracted from the context."""
        context = {
            "RUN": "gfs",
            "NET": "gfs",
            "MACHINE": "HERA",
            "PDY": "20250101",       # runtime variable — should be excluded
            "cyc": "00",             # runtime variable — should be excluded
            "SOME_OTHER": "value",   # not in registry — should be excluded
        }
        result = get_deploy_time_values(context)
        assert "RUN" in result
        assert "NET" in result
        assert "MACHINE" in result
        assert "PDY" not in result
        assert "cyc" not in result
        assert "SOME_OTHER" not in result

    def test_missing_context_keys_silently_skipped(self):
        """Missing keys in the context do not raise KeyError."""
        context = {"RUN": "gfs"}  # Only one of 15 variables present
        result = get_deploy_time_values(context)
        assert result == {"RUN": "gfs"}

    def test_empty_context_returns_empty_dict(self):
        """An empty context dict returns an empty result."""
        result = get_deploy_time_values({})
        assert result == {}

    def test_full_context_returns_all_15(self):
        """A context with all 15 variables returns all 15."""
        context = {var.name: f"value_{var.name}" for var in DEPLOY_TIME_REGISTRY}
        result = get_deploy_time_values(context)
        assert len(result) == 15
        for var in DEPLOY_TIME_REGISTRY:
            assert var.name in result

    def test_values_converted_to_string(self):
        """Non-string values in the context are converted to strings."""
        context = {
            "NMEM_ENS": 80,
            "DO_COUPLED": True,
            "CASE": "C384",
        }
        result = get_deploy_time_values(context)
        assert result["NMEM_ENS"] == "80"
        assert result["DO_COUPLED"] == "True"
        assert result["CASE"] == "C384"

    def test_result_preserves_registry_order(self):
        """The result dict preserves insertion order matching the registry."""
        context = {var.name: f"val_{i}" for i, var in enumerate(DEPLOY_TIME_REGISTRY)}
        result = get_deploy_time_values(context)
        result_keys = list(result.keys())
        registry_names = [var.name for var in DEPLOY_TIME_REGISTRY]
        assert result_keys == registry_names

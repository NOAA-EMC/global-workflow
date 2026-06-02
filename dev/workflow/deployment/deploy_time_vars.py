"""Deploy-time variable registry — single source of truth.

Defines the authoritative set of variables whose values are fully determined
at deployment time from the Workflow_YAML and platform selection.  Both the
Config_Conditioner and Model_Input_Renderer consume this registry to know
which variables can be resolved during deployment (as opposed to runtime
variables like PDY, cyc, DATA, COMOUT).

Traces to: Requirements 11.1, 11.2, 11.3, 11.4
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class DeployTimeVariable:
    """A variable resolvable at deployment time.

    Attributes:
        name: The variable name as it appears in config files (e.g. "RUN").
        source: Origin of the value — one of "workflow_yaml", "platform",
            or "derived".
        description: Human-readable explanation of what the variable represents.
    """

    name: str
    source: str  # "workflow_yaml" | "platform" | "derived"
    description: str


# The authoritative registry (Req 11.3)
DEPLOY_TIME_REGISTRY: list[DeployTimeVariable] = [
    DeployTimeVariable("RUN", "workflow_yaml", "Primary run identifier from cycles[0].name"),
    DeployTimeVariable("NET", "workflow_yaml", "Model network from suite.name prefix"),
    DeployTimeVariable("CASE", "workflow_yaml", "Atmosphere resolution (e.g. C384)"),
    DeployTimeVariable("CASE_ENS", "workflow_yaml", "Ensemble resolution"),
    DeployTimeVariable("MACHINE", "platform", "Target HPC platform"),
    DeployTimeVariable("CDUMP", "derived", "Cycle dump identifier (alias for RUN)"),
    DeployTimeVariable("NMEM_ENS", "workflow_yaml", "Number of ensemble members"),
    DeployTimeVariable("APP", "workflow_yaml", "Application identifier"),
    DeployTimeVariable("CCPP_SUITE", "workflow_yaml", "CCPP physics suite name"),
    DeployTimeVariable("DO_COUPLED", "workflow_yaml", "Coupled model flag"),
    DeployTimeVariable("DO_WAVE", "workflow_yaml", "Wave component flag"),
    DeployTimeVariable("DO_OCN", "workflow_yaml", "Ocean component flag"),
    DeployTimeVariable("DO_ICE", "workflow_yaml", "Ice component flag"),
    DeployTimeVariable("DO_AERO", "workflow_yaml", "Aerosol component flag"),
    DeployTimeVariable("REPLAY_ICS", "workflow_yaml", "Replay initial conditions flag"),
]


def get_deploy_time_values(context: dict[str, Any]) -> dict[str, str]:
    """Extract deploy-time variable values from the pipeline context.

    Iterates over the registry and pulls matching keys from the provided
    context dict.  Missing keys are silently skipped — this allows partial
    contexts (e.g. when only platform variables are available).

    Used by both Config_Conditioner and Model_Input_Renderer.

    Args:
        context: Pipeline context dict (typically built in Stage 2).

    Returns:
        Dict mapping variable name to its string value for all registry
        entries present in the context.
    """
    values: dict[str, str] = {}
    for var in DEPLOY_TIME_REGISTRY:
        if var.name in context:
            values[var.name] = str(context[var.name])
    return values

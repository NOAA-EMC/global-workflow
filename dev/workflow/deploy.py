#!/usr/bin/env python3
"""deploy_workflow CLI — Deployment_Tool entry point.

Parses command-line arguments and invokes the 8-stage deployment pipeline
to produce an immutable, versioned EXPDIR from dev/ sources.

Usage:
    deploy_workflow \
        --config dev/parm/workflow/gfs_cycled.yaml \
        --platform HERA \
        --expdir /path/to/EXPDIR \
        --version v17.0.0 \
        [--allowlist dev/ctests/] \
        [--dry-run]

Traces to: Requirements 1.5, 3.1, 12.1
"""

from __future__ import annotations

import argparse
import logging
import sys

from deployment.pipeline import SUPPORTED_PLATFORMS, PipelineError, SubmodulePolicy, run

logger = logging.getLogger(__name__)

# Sorted platform list for help text and error messages
_PLATFORM_CHOICES = sorted(SUPPORTED_PLATFORMS)


def _build_parser() -> argparse.ArgumentParser:
    """Build the argument parser for deploy_workflow."""
    parser = argparse.ArgumentParser(
        prog="deploy_workflow",
        description=(
            "Deploy the global-workflow into an immutable, versioned EXPDIR. "
            "Renders templates, generates the ecFlow DAG, runs EE2 compliance "
            "checks, and seals the deployment artifact."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "Supported platforms:\n"
            f"  {', '.join(_PLATFORM_CHOICES)}\n\n"
            "Examples:\n"
            "  deploy_workflow --config dev/parm/workflow/gfs_cycled.yaml \\\n"
            "                  --platform HERA \\\n"
            "                  --expdir /scratch/EXPDIR/gfs_v17 \\\n"
            "                  --version v17.0.0\n\n"
            "  deploy_workflow --config dev/parm/workflow/gfs_cycled.yaml \\\n"
            "                  --platform HERA \\\n"
            "                  --expdir /scratch/EXPDIR/gfs_v17 \\\n"
            "                  --version v17.0.0 \\\n"
            "                  --dry-run\n"
        ),
    )

    parser.add_argument(
        "--config",
        required=True,
        help=(
            "Path to the Workflow_Configuration YAML file "
            "(e.g. dev/parm/workflow/gfs_cycled.yaml)."
        ),
    )

    parser.add_argument(
        "--platform",
        required=True,
        help=(
            "Target HPC platform. "
            f"Must be one of: {', '.join(_PLATFORM_CHOICES)}."
        ),
    )

    parser.add_argument(
        "--expdir",
        required=True,
        help="Destination EXPDIR path for the immutable deployment.",
    )

    parser.add_argument(
        "--version",
        required=True,
        help=(
            "Semantic version string for the Snapshot_ID "
            "(e.g. 'v17.0.0')."
        ),
    )

    parser.add_argument(
        "--allowlist",
        default=None,
        help=(
            "Comma-separated list of dev/ paths to include that are "
            "normally excluded (e.g. 'dev/ctests/,dev/ci/')."
        ),
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        default=False,
        help="Validate inputs without writing any files to EXPDIR.",
    )

    parser.add_argument(
        "--dag-filter",
        action="store_true",
        default=False,
        help=(
            "Enable DAG-filtered staging. When active, only artifacts "
            "transitively reachable from the Workflow_YAML task DAG are "
            "staged into the EXPDIR. When disabled (default), all files "
            "from dev/ are staged using full-copy behavior."
        ),
    )

    parser.add_argument(
        "--submodule-policy",
        choices=["require", "fixture", "skip"],
        default="require",
        help=(
            "How to handle missing submodule source files. "
            "'require' (default) aborts on missing submodules. "
            "'fixture' resolves from committed fixture data. "
            "'skip' skips optional submodule copies with a warning."
        ),
    )

    return parser


def _validate_platform(platform: str) -> str:
    """Validate and normalize the platform argument.

    Args:
        platform: The user-provided platform string.

    Returns:
        The normalized (uppercased) platform string.

    Raises:
        SystemExit: If the platform is not in the supported set.
    """
    normalized = platform.upper()
    if normalized not in SUPPORTED_PLATFORMS:
        logger.error(
            "FATAL ERROR [cli]: Unsupported platform '%s'. "
            "Supported platforms: %s",
            platform,
            ", ".join(_PLATFORM_CHOICES),
        )
        sys.exit(1)
    return normalized


def _check_rocoto_invocation(args: argparse.Namespace) -> None:
    """Emit FATAL ERROR if a Rocoto code path is invoked.

    Checks for any indication that the user is attempting to use a
    decommissioned Rocoto workflow path.

    Traces to: Requirement 1.5
    """
    # Check if the config path references rocoto
    config_lower = args.config.lower()
    if "rocoto" in config_lower:
        logger.error(
            "FATAL ERROR [cli]: Rocoto is decommissioned (Requirement 1). "
            "The global-workflow uses ecFlow exclusively for orchestration. "
            "Please use a Workflow_Configuration YAML file instead of "
            "Rocoto XML. See dev/workflow/README.md for migration guidance."
        )
        sys.exit(1)


def _parse_allowlist(allowlist_str: str | None) -> list[str] | None:
    """Parse the comma-separated allowlist into a list.

    Args:
        allowlist_str: Comma-separated string of dev/ paths, or None.

    Returns:
        List of path strings, or None if no allowlist provided.
    """
    if allowlist_str is None:
        return None
    # Split on commas, strip whitespace, filter empty strings
    items = [item.strip() for item in allowlist_str.split(",")]
    return [item for item in items if item]


def main(argv: list[str] | None = None) -> int:
    """Main entry point for the deploy_workflow CLI.

    Args:
        argv: Command-line arguments (defaults to sys.argv[1:]).

    Returns:
        Exit code: 0 on success, 1 on failure.
    """
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%Y-%m-%dT%H:%M:%S",
    )

    parser = _build_parser()
    args = parser.parse_args(argv)

    # Check for Rocoto invocation (Req 1.5)
    _check_rocoto_invocation(args)

    # Validate platform (Req 12.1)
    platform = _validate_platform(args.platform)

    # Parse allowlist
    allowlist = _parse_allowlist(args.allowlist)

    # Run the deployment pipeline (Req 3.1)
    try:
        # Map CLI string to SubmodulePolicy enum
        policy_map = {
            "require": SubmodulePolicy.REQUIRE,
            "fixture": SubmodulePolicy.FIXTURE,
            "skip": SubmodulePolicy.SKIP_OPTIONAL,
        }
        submodule_policy = policy_map[args.submodule_policy]

        result = run(
            config=args.config,
            platform=platform,
            expdir=args.expdir,
            version=args.version,
            allowlist=allowlist,
            dry_run=args.dry_run,
            dag_filter=args.dag_filter,
            submodule_policy=submodule_policy,
        )
    except PipelineError as e:
        logger.error(str(e))
        return 1
    except Exception as e:
        logger.error("FATAL ERROR [unexpected]: %s", e)
        return 1

    # Report results
    if result.get("dry_run"):
        logger.info("Dry-run complete. Validation passed.")
    else:
        snapshot_id = result.get("snapshot_id", "unknown")
        logger.info("Deployment complete.")
        logger.info("  Snapshot_ID:    %s", snapshot_id)
        logger.info("  EXPDIR:         %s", result.get("expdir"))
        logger.info("  Files rendered: %d", result.get("files_rendered", 0))
        logger.info("  Files staged:   %d", result.get("files_staged", 0))
        logger.info("  DAG tasks:      %d", result.get("tasks", 0))
        logger.info("  Duration:       %.2fs", result.get("duration_seconds", 0))

    return 0


if __name__ == "__main__":
    sys.exit(main())

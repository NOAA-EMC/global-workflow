"""Platform-conditioned rendering for the Deployment_Tool.

Handles the rendering and staging of platform-specific files:
  - env/${PLATFORM}.env
  - parm/config/<app>/config.resources.${PLATFORM}
  - modulefiles/${PLATFORM}/ (platform-specific modulefiles)

Non-platform files (J-Jobs, ex-scripts, ush) are identical across platforms.

Traces to: Requirements 12.2, 12.3
"""

from __future__ import annotations

import logging
import re
import shutil
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Supported platforms (mirrors pipeline.SUPPORTED_PLATFORMS)
SUPPORTED_PLATFORMS = frozenset({
    "WCOSS2",
    "HERA",
    "HERCULES",
    "ORION",
    "GAEAC6",
    "DERECHO",
    "URSA",
    "AWSPW",
    "AZUREPW",
    "GOOGLEPW",
    "CONTAINER",
})

# Mapping from platform name to modulefile platform suffix.
# Modulefiles use lowercase platform names in their filenames
# (e.g., gw_run.hera.lua, gw_setup.wcoss2.lua).
_PLATFORM_TO_MODULEFILE_SUFFIX: dict[str, str] = {
    "WCOSS2": "wcoss2",
    "HERA": "hera",
    "HERCULES": "hercules",
    "ORION": "orion",
    "GAEAC6": "gaeac6",
    "DERECHO": "derecho",
    "URSA": "ursa",
    "AWSPW": "noaacloud",
    "AZUREPW": "noaacloud",
    "GOOGLEPW": "noaacloud",
    "CONTAINER": "container",
}


# ---------------------------------------------------------------------------
# Errors
# ---------------------------------------------------------------------------


class PlatformConditionError(Exception):
    """Raised when platform-conditioned rendering fails."""

    def __init__(self, message: str) -> None:
        self.message = message
        super().__init__(f"FATAL ERROR [platform_conditioner]: {message}")


# ---------------------------------------------------------------------------
# Result dataclass
# ---------------------------------------------------------------------------


@dataclass
class PlatformRenderResult:
    """Result of platform-conditioned rendering."""

    env_file: Optional[Path] = None
    resource_files: list[Path] = field(default_factory=list)
    modulefile_paths: list[Path] = field(default_factory=list)
    common_modulefiles: list[Path] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Platform-conditioned rendering
# ---------------------------------------------------------------------------


def render_platform_env(
    project_root: Path,
    expdir: Path,
    platform: str,
    renderer: Optional[Any] = None,
) -> Optional[Path]:
    """Render the platform-specific env file to EXPDIR.

    Copies (or renders if it's a .j2 template) the env file for the
    specified platform from the source tree to <EXPDIR>/env/${PLATFORM}.env.

    The source env file is looked up in the following order:
      1. <project_root>/env/${PLATFORM}.env
      2. <project_root>/dev/env/${PLATFORM}.env

    Args:
        project_root: Root of the global-workflow repository.
        expdir: Destination EXPDIR path.
        platform: Target platform name (e.g. 'HERA', 'WCOSS2').
        renderer: Optional TemplateRenderer instance for .j2 files.

    Returns:
        Path to the rendered env file in EXPDIR, or None if not found.

    Raises:
        PlatformConditionError: If the env file cannot be found.
    """
    platform = platform.upper()
    env_filename = f"{platform}.env"

    # Search for the env file in known locations
    candidates = [
        project_root / "env" / env_filename,
        project_root / "dev" / "env" / env_filename,
    ]

    src_path: Optional[Path] = None
    for candidate in candidates:
        if candidate.exists():
            src_path = candidate
            break

    if src_path is None:
        logger.warning(
            f"Platform env file not found for {platform}. "
            f"Searched: {[str(c) for c in candidates]}"
        )
        return None

    # Destination path
    dst_dir = expdir / "env"
    dst_dir.mkdir(parents=True, exist_ok=True)
    dst_path = dst_dir / env_filename

    # If it's a .j2 template, render it; otherwise copy verbatim
    if src_path.suffix == ".j2":
        if renderer is None:
            raise PlatformConditionError(
                f"Env file {src_path} is a Jinja2 template but no renderer "
                f"was provided."
            )
        renderer.render_file(src_path, dst_path)
    else:
        shutil.copy2(src_path, dst_path)

    logger.info(f"  ✓ Platform env: {src_path.name} → env/{env_filename}")
    return dst_path


def render_platform_resources(
    project_root: Path,
    expdir: Path,
    platform: str,
    app: str,
    renderer: Optional[Any] = None,
) -> list[Path]:
    """Render platform-specific config.resources files to EXPDIR.

    Copies (or renders) the config.resources.${PLATFORM} file for the
    specified platform from dev/parm/config/<app>/ to
    <EXPDIR>/parm/config/<app>/config.resources.${PLATFORM}.

    Also copies the base config.resources file (non-platform-specific)
    which is shared across all platforms.

    Args:
        project_root: Root of the global-workflow repository.
        expdir: Destination EXPDIR path.
        platform: Target platform name (e.g. 'HERA', 'WCOSS2').
        app: Application name (e.g. 'gfs', 'gefs', 'gcafs').
        renderer: Optional TemplateRenderer instance for .j2 files.

    Returns:
        List of paths to rendered resource files in EXPDIR.
    """
    platform = platform.upper()
    rendered_files: list[Path] = []

    # Source directory for config files
    src_config_dir = project_root / "dev" / "parm" / "config" / app

    if not src_config_dir.is_dir():
        logger.debug(
            f"  Config directory not found: {src_config_dir}. "
            f"Skipping platform resources for app '{app}'."
        )
        return rendered_files

    # Destination directory
    dst_config_dir = expdir / "parm" / "config" / app
    dst_config_dir.mkdir(parents=True, exist_ok=True)

    # 1. Copy the base config.resources (shared across platforms)
    base_resources = src_config_dir / "config.resources"
    if base_resources.exists():
        dst_base = dst_config_dir / "config.resources"
        if not dst_base.exists():
            shutil.copy2(base_resources, dst_base)
            rendered_files.append(dst_base)
            logger.debug(f"  Staged base config.resources for {app}")

    # 2. Copy/render the platform-specific config.resources.${PLATFORM}
    platform_resources = src_config_dir / f"config.resources.{platform}"
    if platform_resources.exists():
        dst_platform = dst_config_dir / f"config.resources.{platform}"
        if platform_resources.suffix == ".j2":
            if renderer is None:
                raise PlatformConditionError(
                    f"Resource file {platform_resources} is a Jinja2 template "
                    f"but no renderer was provided."
                )
            renderer.render_file(platform_resources, dst_platform)
        else:
            shutil.copy2(platform_resources, dst_platform)
        rendered_files.append(dst_platform)
        logger.info(
            f"  ✓ Platform resources: config.resources.{platform} "
            f"→ parm/config/{app}/"
        )
    else:
        logger.debug(
            f"  No platform-specific config.resources.{platform} "
            f"found for app '{app}'."
        )

    return rendered_files


def stage_platform_modulefiles(
    project_root: Path,
    expdir: Path,
    platform: str,
) -> list[Path]:
    """Copy platform-specific modulefiles to EXPDIR.

    Modulefiles are organized by platform. The naming convention is:
      gw_<purpose>.<platform_suffix>.lua

    For example:
      gw_run.hera.lua, gw_setup.wcoss2.lua

    This function copies:
      1. Platform-specific modulefiles (matching the platform suffix)
      2. Common modulefiles (e.g., gw_run.common.lua)

    The destination is <EXPDIR>/modulefiles/${PLATFORM}/ for platform-
    specific files and <EXPDIR>/modulefiles/ for common files.

    Args:
        project_root: Root of the global-workflow repository.
        expdir: Destination EXPDIR path.
        platform: Target platform name (e.g. 'HERA', 'WCOSS2').

    Returns:
        List of paths to staged modulefiles in EXPDIR.
    """
    platform = platform.upper()
    staged_files: list[Path] = []

    # Determine the modulefile suffix for this platform
    platform_suffix = _PLATFORM_TO_MODULEFILE_SUFFIX.get(platform)
    if platform_suffix is None:
        logger.warning(
            f"No modulefile suffix mapping for platform {platform}. "
            f"Skipping modulefile staging."
        )
        return staged_files

    # Search for modulefiles in known locations
    modulefile_dirs = [
        project_root / "modulefiles",
        project_root / "dev" / "modulefiles",
    ]

    src_dir: Optional[Path] = None
    for candidate in modulefile_dirs:
        if candidate.is_dir():
            src_dir = candidate
            break

    if src_dir is None:
        logger.debug("  No modulefiles directory found. Skipping.")
        return staged_files

    # Destination: <EXPDIR>/modulefiles/${PLATFORM}/
    dst_platform_dir = expdir / "modulefiles" / platform
    dst_platform_dir.mkdir(parents=True, exist_ok=True)

    # Also create a common modulefiles directory
    dst_common_dir = expdir / "modulefiles"
    dst_common_dir.mkdir(parents=True, exist_ok=True)

    # Pattern to match platform-specific modulefiles
    # e.g., gw_run.hera.lua, gw_setup.hera.lua
    platform_pattern = re.compile(
        rf"^.*\.{re.escape(platform_suffix)}\.lua$", re.IGNORECASE
    )
    common_pattern = re.compile(r"^.*\.common\.lua$", re.IGNORECASE)

    for src_file in sorted(src_dir.iterdir()):
        if not src_file.is_file():
            continue

        filename = src_file.name

        if platform_pattern.match(filename):
            # Platform-specific modulefile → modulefiles/${PLATFORM}/
            dst_file = dst_platform_dir / filename
            shutil.copy2(src_file, dst_file)
            staged_files.append(dst_file)
        elif common_pattern.match(filename):
            # Common modulefile → modulefiles/ (shared)
            dst_file = dst_common_dir / filename
            if not dst_file.exists():
                shutil.copy2(src_file, dst_file)
                staged_files.append(dst_file)

    if staged_files:
        logger.info(
            f"  ✓ Platform modulefiles: {len(staged_files)} file(s) "
            f"→ modulefiles/{platform}/"
        )
    else:
        logger.debug(
            f"  No modulefiles found for platform suffix '{platform_suffix}'."
        )

    return staged_files


def render_all_platform_conditioned(
    project_root: Path,
    expdir: Path,
    platform: str,
    app: str,
    renderer: Optional[Any] = None,
) -> PlatformRenderResult:
    """Render all platform-conditioned files for a deployment.

    This is the main entry point for platform-conditioned rendering.
    It handles:
      1. Platform env file (env/${PLATFORM}.env)
      2. Platform resource files (parm/config/<app>/config.resources.${PLATFORM})
      3. Platform modulefiles (modulefiles/${PLATFORM}/)

    Non-platform files (J-Jobs, ex-scripts, ush) are handled by the
    general file staging stage and are identical across platforms.

    Args:
        project_root: Root of the global-workflow repository.
        expdir: Destination EXPDIR path.
        platform: Target platform name (e.g. 'HERA', 'WCOSS2').
        app: Application name (e.g. 'gfs', 'gefs', 'gcafs').
        renderer: Optional TemplateRenderer instance for .j2 files.

    Returns:
        PlatformRenderResult with paths to all rendered platform files.

    Raises:
        PlatformConditionError: If a required platform file is missing
            or rendering fails.
    """
    platform = platform.upper()
    result = PlatformRenderResult()

    logger.info(f"  Platform-conditioned rendering for {platform}:")

    # 1. Render platform env file
    result.env_file = render_platform_env(
        project_root=project_root,
        expdir=expdir,
        platform=platform,
        renderer=renderer,
    )

    # 2. Render platform resource files
    result.resource_files = render_platform_resources(
        project_root=project_root,
        expdir=expdir,
        platform=platform,
        app=app,
        renderer=renderer,
    )

    # 3. Stage platform modulefiles
    result.modulefile_paths = stage_platform_modulefiles(
        project_root=project_root,
        expdir=expdir,
        platform=platform,
    )

    total = (
        (1 if result.env_file else 0)
        + len(result.resource_files)
        + len(result.modulefile_paths)
    )
    logger.info(
        f"  ✓ Platform-conditioned rendering complete: "
        f"{total} file(s) for {platform}"
    )

    return result


def get_platform_conditioned_paths(platform: str) -> list[str]:
    """Return the list of path patterns that are platform-conditioned.

    These paths are expected to differ between platforms. All other
    files in the EXPDIR should be identical across platforms.

    This is used by the Platform Isolation property test to determine
    which files are allowed to differ.

    Args:
        platform: Target platform name.

    Returns:
        List of glob patterns for platform-conditioned paths.
    """
    platform = platform.upper()
    platform_suffix = _PLATFORM_TO_MODULEFILE_SUFFIX.get(platform, platform.lower())

    return [
        f"env/{platform}.env",
        f"parm/config/*/config.resources.{platform}",
        f"modulefiles/{platform}/*",
        f"modulefiles/{platform}/",
        f"modulefiles/*.{platform_suffix}.lua",
        # ecf scripts may contain platform-specific scheduler directives
        "ecf/scripts/**/*.ecf",
    ]

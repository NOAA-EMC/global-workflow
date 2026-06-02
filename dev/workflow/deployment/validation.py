"""Input validation stage for the deployment pipeline.

Implements Stage 1 of the pipeline:
  - Check git state (clean working tree)
  - Verify wxflow/uwtools versions match pinned versions in requirements.txt
  - Refuse if EXPDIR already exists with a manifest (immutability guard)
  - Emit FATAL ERROR referencing existing Snapshot_ID if write attempted to sealed EXPDIR

Traces to: Requirements 3.5, 9.5
"""

import re
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


class ValidationError(Exception):
    """Raised when input validation fails with a FATAL ERROR."""

    pass


@dataclass
class ValidationResult:
    """Result of the input validation stage."""

    passed: bool = True
    warnings: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)

    def add_warning(self, msg: str) -> None:
        self.warnings.append(msg)

    def add_error(self, msg: str) -> None:
        self.passed = False
        self.errors.append(msg)


def check_expdir_immutability(expdir: Path) -> None:
    """Check if EXPDIR already exists and contains a manifest.yaml.

    If a manifest exists, the EXPDIR is considered sealed/published and
    must not be overwritten. Emits FATAL ERROR referencing the existing
    Snapshot_ID.

    Args:
        expdir: Path to the target EXPDIR.

    Raises:
        ValidationError: If EXPDIR already contains a manifest.yaml.
    """
    manifest_path = expdir / "manifest.yaml"
    if not manifest_path.exists():
        return

    # Try to extract the Snapshot_ID from the existing manifest
    snapshot_id = _extract_snapshot_id(manifest_path)
    if snapshot_id:
        raise ValidationError(
            f"FATAL ERROR: EXPDIR already published with Snapshot_ID {snapshot_id}. "
            f"Path: {expdir}"
        )
    else:
        raise ValidationError(
            f"FATAL ERROR: EXPDIR already contains a manifest.yaml and is sealed. "
            f"Path: {expdir}"
        )


def _extract_snapshot_id(manifest_path: Path) -> Optional[str]:
    """Extract the snapshot_id field from a manifest.yaml file.

    Args:
        manifest_path: Path to the manifest.yaml file.

    Returns:
        The snapshot_id string if found, None otherwise.
    """
    try:
        content = manifest_path.read_text(encoding="utf-8")
        # Simple regex extraction to avoid requiring yaml import for validation
        match = re.search(r'^snapshot_id:\s*["\']?([^"\'\n]+)["\']?', content, re.MULTILINE)
        if match:
            return match.group(1).strip()
    except (OSError, IOError):
        pass
    return None


def check_pinned_versions(requirements_path: Path) -> ValidationResult:
    """Verify installed wxflow/uwtools versions match pinned versions.

    Reads pinned versions from requirements.txt and compares against
    installed package versions. Emits FATAL ERROR on mismatch.
    Gracefully skips if packages are not installed (e.g., in CI without
    full environment).

    Args:
        requirements_path: Path to dev/workflow/requirements.txt.

    Returns:
        ValidationResult with any errors or warnings.
    """
    result = ValidationResult()

    if not requirements_path.exists():
        result.add_error(
            f"FATAL ERROR: Requirements file not found: {requirements_path}"
        )
        return result

    pinned = _parse_pinned_versions(requirements_path)

    for package, pinned_version in pinned.items():
        installed_version = _get_installed_version(package)
        if installed_version is None:
            result.add_warning(
                f"Package '{package}' is not installed; skipping version check"
            )
        elif installed_version != pinned_version:
            result.add_error(
                f"FATAL ERROR: {package} {installed_version} != pinned {pinned_version} "
                f"(from {requirements_path})"
            )

    return result


def _parse_pinned_versions(requirements_path: Path) -> dict[str, str]:
    """Parse pinned (==) versions from a requirements.txt file.

    Only extracts packages with exact version pins (==).

    Args:
        requirements_path: Path to the requirements.txt file.

    Returns:
        Dict mapping package name to pinned version string.
    """
    pinned = {}
    try:
        content = requirements_path.read_text(encoding="utf-8")
        for line in content.splitlines():
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            # Match exact pins: package==version
            match = re.match(r'^([a-zA-Z0-9_-]+)==([^\s#]+)', line)
            if match:
                package = match.group(1).lower()
                version = match.group(2)
                pinned[package] = version
    except (OSError, IOError):
        pass
    return pinned


def _get_installed_version(package: str) -> Optional[str]:
    """Get the installed version of a Python package.

    Args:
        package: Package name to check.

    Returns:
        Version string if installed, None otherwise.
    """
    try:
        from importlib.metadata import version, PackageNotFoundError
        return version(package)
    except Exception:
        return None


def check_git_state(repo_path: Optional[Path] = None) -> ValidationResult:
    """Check if the git working tree is clean.

    A clean working tree means no uncommitted changes (staged or unstaged)
    and no untracked files in tracked directories.

    Args:
        repo_path: Path to the git repository root. If None, uses cwd.

    Returns:
        ValidationResult with warnings for dirty state.
    """
    result = ValidationResult()

    try:
        cmd_args = ["git", "status", "--porcelain"]
        kwargs = {}
        if repo_path:
            kwargs["cwd"] = str(repo_path)

        proc = subprocess.run(
            cmd_args,
            capture_output=True,
            text=True,
            timeout=30,
            **kwargs,
        )

        if proc.returncode != 0:
            result.add_warning(
                f"Unable to check git state: {proc.stderr.strip()}"
            )
            return result

        if proc.stdout.strip():
            dirty_files = proc.stdout.strip().splitlines()
            n_dirty = len(dirty_files)
            result.add_warning(
                f"Git working tree is not clean ({n_dirty} modified/untracked files). "
                f"Deployment from a dirty tree may not be reproducible."
            )

    except FileNotFoundError:
        result.add_warning("git command not found; skipping git state check")
    except subprocess.TimeoutExpired:
        result.add_warning("git status timed out; skipping git state check")
    except Exception as e:
        result.add_warning(f"Unable to check git state: {e}")

    return result


def validate_inputs(
    expdir: Path,
    requirements_path: Path,
    repo_path: Optional[Path] = None,
) -> ValidationResult:
    """Run the full input validation stage.

    This is the main entry point for Stage 1 of the deployment pipeline.
    It performs all validation checks in order:
      1. EXPDIR immutability guard (FATAL if manifest exists)
      2. Pinned version verification (FATAL on mismatch)
      3. Git state check (warning if dirty)

    Args:
        expdir: Path to the target EXPDIR.
        requirements_path: Path to dev/workflow/requirements.txt.
        repo_path: Path to the git repository root. If None, uses cwd.

    Returns:
        ValidationResult aggregating all checks.

    Raises:
        ValidationError: If EXPDIR is already sealed (immediate fatal).
    """
    # Check 1: EXPDIR immutability (raises immediately on failure)
    check_expdir_immutability(expdir)

    # Check 2: Pinned versions
    result = check_pinned_versions(requirements_path)

    # Check 3: Git state
    git_result = check_git_state(repo_path)
    result.warnings.extend(git_result.warnings)
    # Git state issues are warnings, not errors (deployment can proceed)

    return result

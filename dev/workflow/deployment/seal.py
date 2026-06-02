"""EXPDIR sealing module.

Sets all regular files to mode 0444, all directories to mode 0555,
and writes workflow/provenance.yaml with deployment metadata.

Traces to: Requirements 3.4, 13.4
"""

import getpass
import os
import platform
import stat
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Optional

import yaml


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

FILE_MODE = 0o444  # read-only for all
DIR_MODE = 0o555   # read + execute for all, no write

PROVENANCE_FILENAME = "workflow/provenance.yaml"


# ---------------------------------------------------------------------------
# Git metadata helpers
# ---------------------------------------------------------------------------


def _git_remote(repo_path: Optional[Path] = None) -> str:
    """Get the git remote URL (origin) for the repository."""
    try:
        cmd = ["git", "remote", "get-url", "origin"]
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=str(repo_path) if repo_path else None,
            timeout=10,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass
    return ""


def _git_commit(repo_path: Optional[Path] = None) -> str:
    """Get the current git commit hash."""
    try:
        cmd = ["git", "rev-parse", "HEAD"]
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=str(repo_path) if repo_path else None,
            timeout=10,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass
    return ""


def _git_branch(repo_path: Optional[Path] = None) -> str:
    """Get the current git branch name."""
    try:
        cmd = ["git", "rev-parse", "--abbrev-ref", "HEAD"]
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            cwd=str(repo_path) if repo_path else None,
            timeout=10,
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass
    return ""


# ---------------------------------------------------------------------------
# Sealing functions
# ---------------------------------------------------------------------------


def seal_permissions(expdir: Path) -> None:
    """Set all regular files to 0444 and all directories to 0555.

    Walks the EXPDIR tree bottom-up so that directories are sealed
    after their contents (otherwise we'd lose write permission to
    modify children).

    Parameters
    ----------
    expdir : Path
        Root of the EXPDIR to seal.

    Raises
    ------
    OSError
        If permission changes fail.
    """
    expdir = Path(expdir)

    # Walk bottom-up so we seal leaf directories before parents
    for dirpath, dirnames, filenames in os.walk(str(expdir), topdown=False):
        # Seal regular files
        for filename in filenames:
            filepath = os.path.join(dirpath, filename)
            # Only seal regular files (skip symlinks)
            if os.path.isfile(filepath) and not os.path.islink(filepath):
                os.chmod(filepath, FILE_MODE)

        # Seal the directory itself
        os.chmod(dirpath, DIR_MODE)


def write_provenance(
    expdir: Path,
    config: Optional[dict[str, Any]] = None,
    repo_path: Optional[Path] = None,
) -> Path:
    """Write workflow/provenance.yaml with deployment metadata.

    Parameters
    ----------
    expdir : Path
        Root of the EXPDIR.
    config : dict, optional
        Configuration values used for deployment. If None, an empty
        dict is written.
    repo_path : Path, optional
        Path to the git repository for metadata extraction. If None,
        the current working directory is used.

    Returns
    -------
    Path
        Path to the written provenance.yaml file.
    """
    expdir = Path(expdir)

    provenance = {
        "git_remote": _git_remote(repo_path),
        "git_commit": _git_commit(repo_path),
        "git_branch": _git_branch(repo_path),
        "deployed_by": getpass.getuser(),
        "deployed_on": platform.node(),
        "deployed_at": datetime.now(timezone.utc).isoformat(),
        "config": config if config is not None else {},
    }

    provenance_path = expdir / PROVENANCE_FILENAME

    # Ensure the workflow/ directory exists
    provenance_path.parent.mkdir(parents=True, exist_ok=True)

    with open(provenance_path, "w") as f:
        yaml.dump(
            provenance,
            f,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
        )

    return provenance_path


def seal_expdir(
    expdir: Path,
    config: Optional[dict[str, Any]] = None,
    repo_path: Optional[Path] = None,
) -> Path:
    """Seal an EXPDIR: write provenance and set permissions.

    This is the main entry point for Stage 8 of the deployment pipeline.
    It writes the provenance.yaml file first, then seals all file and
    directory permissions.

    Parameters
    ----------
    expdir : Path
        Root of the EXPDIR to seal.
    config : dict, optional
        Configuration values used for deployment.
    repo_path : Path, optional
        Path to the git repository for metadata extraction.

    Returns
    -------
    Path
        Path to the written provenance.yaml file.

    Raises
    ------
    FileNotFoundError
        If expdir does not exist.
    OSError
        If permission changes fail.
    """
    expdir = Path(expdir)

    if not expdir.is_dir():
        raise FileNotFoundError(
            f"EXPDIR does not exist: {expdir}"
        )

    # Write provenance before sealing (need write permission)
    provenance_path = write_provenance(expdir, config=config, repo_path=repo_path)

    # Seal all permissions
    seal_permissions(expdir)

    return provenance_path

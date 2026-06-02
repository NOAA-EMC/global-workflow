"""File stager using uwtools uw fs copy API.

Stages non-template files from dev/ to EXPDIR following the
source-to-target mapping defined in the design document.

Falls back to shutil.copytree/copy2 if uwtools is not available.

Traces to: Requirements 8.2, 8.7, 8.8, 9.2
"""

from __future__ import annotations

import logging
import os
import shutil
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from .name_resolver import ResolvedName

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Default source-to-target mapping (design table)
# ---------------------------------------------------------------------------

#: Default mapping of dev/ subdirectories to EXPDIR subdirectories.
#: Keys are relative to the project root (containing dev/), values are
#: relative to the EXPDIR destination.
DEFAULT_SOURCE_TARGET_MAP: dict[str, str] = {
    "dev/jobs": "jobs",
    "dev/scripts": "scripts",
    "dev/ush": "ush",
    "dev/sorc": "sorc",
    "dev/versions": "versions",
    "dev/modulefiles": "modulefiles",
}

#: Directories excluded from staging by default (Req 8.7).
DEFAULT_EXCLUDES: list[str] = [
    "dev/ci",
    "dev/ctests",
]


# ---------------------------------------------------------------------------
# Errors
# ---------------------------------------------------------------------------


class StagingError(Exception):
    """Raised when file staging fails."""

    def __init__(self, message: str, source: Optional[str] = None):
        self.message = message
        self.source = source
        super().__init__(f"FATAL ERROR: {message}")


# ---------------------------------------------------------------------------
# Result dataclass
# ---------------------------------------------------------------------------


@dataclass
class StagingResult:
    """Result of a file staging operation."""

    files_copied: int = 0
    directories_created: int = 0
    skipped_excludes: list[str] = field(default_factory=list)
    staged_paths: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# uwtools availability check
# ---------------------------------------------------------------------------


def _uwtools_available() -> bool:
    """Check if uwtools is importable."""
    try:
        import uwtools  # noqa: F401
        return True
    except ImportError:
        return False


# ---------------------------------------------------------------------------
# Copy backends
# ---------------------------------------------------------------------------


def _copy_with_uwtools(src: Path, dst: Path) -> None:
    """Copy a file using the uwtools ``uw fs copy`` API.

    Reconciled to the pinned uwtools 2.16.0 API (Req 5): ``fs.copy`` takes a
    ``config`` mapping of destination-relative path -> source path together with
    a ``target_dir`` base, and returns a report listing ready / not-ready
    copies. The earlier ``source=``/``target=`` keyword form was removed
    upstream. The destination parent is ensured first so a single-file copy
    lands deterministically.
    """
    from uwtools.api import fs as uwfs

    dst.parent.mkdir(parents=True, exist_ok=True)
    report = uwfs.copy(config={dst.name: str(src)}, target_dir=str(dst.parent))
    if str(dst) not in report.get("ready", []):
        raise OSError(
            f"uwtools fs.copy did not stage {src} -> {dst}; report={report}"
        )


def _copy_with_shutil(src: Path, dst: Path) -> None:
    """Copy a file using shutil.copy2 (preserves metadata)."""
    dst.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(src, dst)


# ---------------------------------------------------------------------------
# Template detection
# ---------------------------------------------------------------------------


def _is_template(path: Path) -> bool:
    """Return True if the file is a Jinja2 template (handled by renderer)."""
    return path.suffix == ".j2"


# ---------------------------------------------------------------------------
# FileStager class
# ---------------------------------------------------------------------------


@dataclass
class FileStager:
    """Stages non-template files from dev/ to EXPDIR.

    Parameters
    ----------
    project_root : Path
        Root of the global-workflow repository (parent of dev/).
    expdir : Path
        Destination EXPDIR path.
    source_target_map : dict[str, str] | None
        Mapping of source dirs (relative to project_root) to target dirs
        (relative to expdir). Defaults to DEFAULT_SOURCE_TARGET_MAP.
    excludes : list[str] | None
        Directories to exclude (relative to project_root).
        Defaults to DEFAULT_EXCLUDES.
    allowlist : list[str] | None
        Directories from excludes to re-include (e.g. ["dev/ctests/"]).
        Overrides excludes for the specified paths.
    use_uwtools : bool | None
        If True, use uwtools for copying. If None, auto-detect.
    """

    project_root: Path
    expdir: Path
    source_target_map: Optional[dict[str, str]] = None
    excludes: Optional[list[str]] = None
    allowlist: Optional[list[str]] = None
    use_uwtools: Optional[bool] = None

    def __post_init__(self) -> None:
        self.project_root = Path(self.project_root)
        self.expdir = Path(self.expdir)

        if self.source_target_map is None:
            self.source_target_map = dict(DEFAULT_SOURCE_TARGET_MAP)

        if self.excludes is None:
            self.excludes = list(DEFAULT_EXCLUDES)

        if self.allowlist is None:
            self.allowlist = []

        if self.use_uwtools is None:
            self.use_uwtools = _uwtools_available()

    def _get_copy_fn(self):
        """Return the appropriate copy function."""
        if self.use_uwtools:
            return _copy_with_uwtools
        return _copy_with_shutil

    def _is_excluded(self, rel_path: str) -> bool:
        """Check if a relative path falls under an excluded directory.

        A path is excluded if it starts with any exclude prefix, UNLESS
        it also starts with an allowlist prefix.
        """
        # Normalize path separators
        normalized = rel_path.replace(os.sep, "/")

        # Check allowlist first — allowlist overrides excludes
        for allowed in self.allowlist:
            allowed_norm = allowed.rstrip("/").replace(os.sep, "/")
            if normalized.startswith(allowed_norm + "/") or normalized == allowed_norm:
                return False

        # Check excludes
        for excluded in self.excludes:
            excluded_norm = excluded.rstrip("/").replace(os.sep, "/")
            if normalized.startswith(excluded_norm + "/") or normalized == excluded_norm:
                return True

        return False

    def stage(self) -> StagingResult:
        """Execute the file staging operation.

        Copies non-template files from each source directory in the
        source_target_map to the corresponding target directory under EXPDIR.

        Returns
        -------
        StagingResult
            Summary of the staging operation.

        Raises
        ------
        StagingError
            If a source directory in the map does not exist and is not optional.
        """
        result = StagingResult()
        copy_fn = self._get_copy_fn()

        for source_rel, target_rel in self.source_target_map.items():
            source_dir = self.project_root / source_rel
            target_dir = self.expdir / target_rel

            if not source_dir.exists():
                logger.info(
                    "Source directory %s does not exist, skipping.", source_dir
                )
                continue

            if not source_dir.is_dir():
                logger.warning(
                    "Source path %s is not a directory, skipping.", source_dir
                )
                continue

            # Walk the source directory tree
            for root, dirs, files in os.walk(source_dir):
                root_path = Path(root)
                # Compute relative path from project root for exclusion check
                rel_from_root = root_path.relative_to(self.project_root)
                rel_str = str(rel_from_root).replace(os.sep, "/")

                if self._is_excluded(rel_str):
                    result.skipped_excludes.append(rel_str)
                    # Prune subdirectories to avoid walking into excluded trees
                    dirs.clear()
                    continue

                for filename in files:
                    src_file = root_path / filename
                    # Check file-level exclusion
                    file_rel = str(
                        src_file.relative_to(self.project_root)
                    ).replace(os.sep, "/")

                    if self._is_excluded(file_rel):
                        result.skipped_excludes.append(file_rel)
                        continue

                    # Skip templates — they are handled by Template_Renderer
                    if _is_template(src_file):
                        logger.debug("Skipping template: %s", src_file)
                        continue

                    # Compute destination path
                    rel_to_source = src_file.relative_to(source_dir)
                    dst_file = target_dir / rel_to_source

                    # Ensure parent directory exists
                    if not dst_file.parent.exists():
                        dst_file.parent.mkdir(parents=True, exist_ok=True)
                        result.directories_created += 1

                    # Copy the file
                    try:
                        copy_fn(src_file, dst_file)
                        result.files_copied += 1
                        result.staged_paths.append(str(dst_file))
                        logger.debug("Staged: %s -> %s", src_file, dst_file)
                    except (OSError, IOError) as e:
                        raise StagingError(
                            f"Failed to copy {src_file} to {dst_file}: {e}",
                            source=str(src_file),
                        ) from e

        logger.info(
            "File staging complete: %d files copied, %d directories created.",
            result.files_copied,
            result.directories_created,
        )
        return result

    def stage_unconditional_artifacts(self) -> StagingResult:
        """Stage artifacts that are always deployed regardless of DAG filter.

        Stages:
        - sorc/link_workflow.sh → EXPDIR/sorc/link_workflow.sh
        - sorc/ufs_utils.fd/fix/link_fixdirs.sh →
            EXPDIR/sorc/ufs_utils.fd/fix/link_fixdirs.sh

        Preserves executable permission bits (mode 0755).

        Returns:
            StagingResult for the unconditional artifacts.

        Raises:
            StagingError: If source files are missing.
        """
        # Define unconditional artifacts (relative to project_root)
        artifacts = [
            "sorc/link_workflow.sh",
            "sorc/ufs_utils.fd/fix/link_fixdirs.sh",
        ]

        result = StagingResult()

        for rel_path in artifacts:
            src = self.project_root / rel_path
            dst = self.expdir / rel_path

            if not src.exists():
                raise StagingError(
                    f"Unconditional artifact not found: {rel_path}",
                    source=str(src),
                )

            # Ensure destination directory exists
            if not dst.parent.exists():
                dst.parent.mkdir(parents=True, exist_ok=True)
                result.directories_created += 1

            # Copy preserving metadata
            shutil.copy2(src, dst)

            # Ensure executable permission bits
            os.chmod(dst, 0o755)

            result.files_copied += 1
            result.staged_paths.append(str(dst))
            logger.debug(
                "Staged unconditional artifact: %s -> %s", src, dst
            )

        logger.info(
            "Unconditional artifact staging complete: %d files copied.",
            result.files_copied,
        )
        return result

    def stage_single(self, source_rel: str, target_rel: str) -> Path:
        """Stage a single file from source to target.

        Parameters
        ----------
        source_rel : str
            Source path relative to project_root.
        target_rel : str
            Target path relative to expdir.

        Returns
        -------
        Path
            The destination path of the staged file.

        Raises
        ------
        StagingError
            If the source file does not exist or copy fails.
        """
        src = self.project_root / source_rel
        dst = self.expdir / target_rel

        if not src.exists():
            raise StagingError(
                f"Source file not found: {src}", source=str(src)
            )

        if self._is_excluded(source_rel):
            raise StagingError(
                f"Source file is excluded: {source_rel}. "
                f"Add to allowlist to include.",
                source=source_rel,
            )

        copy_fn = self._get_copy_fn()
        dst.parent.mkdir(parents=True, exist_ok=True)

        try:
            copy_fn(src, dst)
        except (OSError, IOError) as e:
            raise StagingError(
                f"Failed to copy {src} to {dst}: {e}", source=str(src)
            ) from e

        return dst

    def stage_jjobs_with_rename(
        self,
        resolution_map: dict[str, ResolvedName],
    ) -> StagingResult:
        """Stage J-Jobs with application-specific renaming.

        For each resolved pair:
        - Source: dev/jobs/{source_name}
        - Destination: EXPDIR/jobs/{application_name}

        Deduplication: if the same application_name appears multiple times
        in the YAML (duplicate task references), it is staged exactly once.
        This is naturally enforced by the dict keyed on application_name.

        Distinct files: if two application_names resolve to the same source,
        both destination files are produced (with identical content).

        Passthrough names (is_passthrough=True) are copied without rename
        (source_name == application_name, so copy is still src → dst).

        Args:
            resolution_map: Dict mapping application_name → ResolvedName.

        Returns:
            StagingResult with count of files staged.

        Raises:
            StagingError: If a source file cannot be read/copied.

        Traces to: Requirements 3.1, 3.2, 3.3, 3.4, 3.5
        """
        from .name_resolver import ResolvedName  # noqa: F811

        result = StagingResult()
        copy_fn = self._get_copy_fn()
        jobs_dir = self.project_root / "dev" / "jobs"
        target_dir = self.expdir / "jobs"

        # Ensure the target jobs directory exists
        if not target_dir.exists():
            target_dir.mkdir(parents=True, exist_ok=True)
            result.directories_created += 1

        for application_name, resolved in resolution_map.items():
            src_file = jobs_dir / resolved.source_name
            dst_file = target_dir / application_name

            if not src_file.exists():
                raise StagingError(
                    f"Failed to copy {src_file} to {dst_file}: "
                    f"source file not found",
                    source=str(src_file),
                )

            try:
                copy_fn(src_file, dst_file)
                result.files_copied += 1
                result.staged_paths.append(str(dst_file))
                logger.debug(
                    "Staged J-Job: %s -> %s (source: %s)",
                    resolved.source_name,
                    application_name,
                    "passthrough" if resolved.is_passthrough else "renamed",
                )
            except (OSError, IOError) as e:
                raise StagingError(
                    f"Failed to copy {src_file} to {dst_file}: {e}",
                    source=str(src_file),
                ) from e

        logger.info(
            "J-Job staging complete: %d files staged with application naming.",
            result.files_copied,
        )
        return result


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------


def stage_files(
    project_root: str | Path,
    expdir: str | Path,
    *,
    source_target_map: Optional[dict[str, str]] = None,
    excludes: Optional[list[str]] = None,
    allowlist: Optional[list[str]] = None,
) -> StagingResult:
    """Stage non-template files from dev/ to EXPDIR.

    This is the primary entry point for the file staging step of the
    deployment pipeline.

    Parameters
    ----------
    project_root : str or Path
        Root of the global-workflow repository.
    expdir : str or Path
        Destination EXPDIR path.
    source_target_map : dict[str, str] | None
        Custom source-to-target mapping. Defaults to design table.
    excludes : list[str] | None
        Directories to exclude. Defaults to dev/ci/, dev/ctests/.
    allowlist : list[str] | None
        Excluded directories to re-include (e.g. ["dev/ctests/"]).

    Returns
    -------
    StagingResult
        Summary of the staging operation.
    """
    stager = FileStager(
        project_root=Path(project_root),
        expdir=Path(expdir),
        source_target_map=source_target_map,
        excludes=excludes,
        allowlist=allowlist,
    )
    return stager.stage()

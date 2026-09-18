#!/usr/bin/env python3

"""
Build a per-experiment fix directory that overlays user-supplied files on top of
the installed ``${HOMEglobal}/fix`` tree.

The overlay is described by the ``fix:`` section of the experiment YAML as a flat
``dst: src`` map, where ``dst`` is a path relative to the fix root and ``src`` is
an absolute path.  Two behaviours are selected by the shape of ``src``:

* no glob characters -> **replace**: ``dst`` becomes a symlink to ``src``
  (``src`` may be a file or a directory; ``dst`` need not exist in the base tree).
* glob characters    -> **merge**: each match of ``src`` is linked into the
  directory ``dst`` under its own basename, alongside whatever the base tree
  already provides there.

The resulting tree is built lazily: any directory that no entry touches is a
single symlink back to the base tree, so an overlay of one file under
``mom6/008/`` expands only ``mom6`` and ``mom6/008``, and every other component
remains a single link.
"""

import glob
import os
import shutil
from logging import getLogger
from pathlib import Path, PurePosixPath
from typing import Dict, List, Tuple

from wxflow import save_as_yaml

__all__ = ['build_fix_overlay', 'FixOverlayError', 'MANIFEST_NAME']

logger = getLogger(__name__)

MANIFEST_NAME = '.fix_overlay.yaml'
_GLOB_CHARS = set('*?[')


class FixOverlayError(ValueError):
    """Raised for an invalid or unsatisfiable ``fix:`` section."""


def _is_glob(path: str) -> bool:
    return any(ch in _GLOB_CHARS for ch in path)


def _normalize_dst(dst: str) -> PurePosixPath:
    """
    Validate a destination and return it as a relative, normalized path.

    Raises
    ------
    FixOverlayError
        If ``dst`` is empty, absolute, or would escape the fix root.
    """
    if not isinstance(dst, str) or not dst.strip():
        raise FixOverlayError(f"fix: destination must be a non-empty relative path, got {dst!r}")

    dst = dst.strip()
    if dst.startswith('/'):
        raise FixOverlayError(f"fix: destination '{dst}' must be relative to the fix directory, not absolute")

    parts = [p for p in PurePosixPath(dst).parts if p not in ('', '.')]
    if not parts:
        raise FixOverlayError(f"fix: destination '{dst}' resolves to the fix root itself; "
                              "override individual components instead")
    if '..' in parts:
        raise FixOverlayError(f"fix: destination '{dst}' may not contain '..'")

    return PurePosixPath(*parts)


def _expand_entries(overlays: Dict[str, str]) -> Tuple[Dict[PurePosixPath, str], List[Dict[str, str]]]:
    """
    Turn the user's ``dst: src`` map into a map of leaf destinations to sources.

    Glob sources are expanded to one leaf per match under ``dst/<basename>``;
    plain sources are a single leaf at ``dst``.  Entries are applied in order,
    so a later entry that lands on the same leaf as an earlier one wins; this
    is how a glob merge can be followed by a single-file exception.

    Returns
    -------
    links : dict
        Leaf destination -> source.
    overrides : list
        One record per leaf that a later entry replaced, for the log and manifest.

    Raises
    ------
    FixOverlayError
        On a missing source, an empty glob, or a relative source.
    """
    links: Dict[PurePosixPath, str] = {}
    origin: Dict[PurePosixPath, str] = {}  # leaf -> user entry that produced it, for messages
    overrides: List[Dict[str, str]] = []

    for raw_dst, raw_src in overlays.items():
        dst = _normalize_dst(raw_dst)

        if not isinstance(raw_src, str) or not raw_src.strip():
            raise FixOverlayError(f"fix: source for '{raw_dst}' must be a non-empty path, got {raw_src!r}")
        src = os.path.expanduser(raw_src.strip())
        if not os.path.isabs(src):
            raise FixOverlayError(f"fix: source for '{raw_dst}' must be an absolute path, got '{raw_src}'")

        if _is_glob(src):
            matches = sorted(glob.glob(src))
            if not matches:
                raise FixOverlayError(f"fix: glob for '{raw_dst}' matched nothing: '{src}'")
            leaves: List[Tuple[PurePosixPath, str]] = [(dst / os.path.basename(m), m) for m in matches]
        else:
            if not os.path.exists(src):
                raise FixOverlayError(f"fix: source for '{raw_dst}' does not exist: '{src}'")
            leaves = [(dst, src)]

        for leaf, leaf_src in leaves:
            if leaf in links:
                logger.info(f"fix: '{leaf}' from '{origin[leaf]}' overridden by '{raw_dst}'")
                overrides.append({'dst': str(leaf), 'from': origin[leaf], 'by': raw_dst,
                                  'was': links[leaf], 'now': leaf_src})
            links[leaf] = leaf_src
            origin[leaf] = raw_dst

    # A leaf that sits inside another leaf would be written through the
    # replacing symlink into the user's source tree; refuse that.
    for leaf in links:
        for ancestor in leaf.parents:
            if ancestor in links:
                raise FixOverlayError(f"fix: '{leaf}' (from '{origin[leaf]}') lies inside "
                                      f"'{ancestor}' (from '{origin[ancestor]}'), which is replaced wholesale")

    return links, overrides


def _materialize(base: Path, dest: Path, rel: PurePosixPath,
                 links: Dict[PurePosixPath, str], expanded: set) -> None:
    """
    Create ``dest/rel`` as a real directory whose children are either overlay
    links, further expanded directories, or links back to ``base/rel/<child>``.
    """
    base_here = base / rel
    dest_here = dest / rel
    dest_here.mkdir(parents=True, exist_ok=False)

    if base_here.exists() and not base_here.is_dir():
        raise FixOverlayError(f"fix: cannot place entries under '{rel}': '{base_here}' is not a directory")

    children = set(os.listdir(base_here)) if base_here.is_dir() else set()
    children |= {leaf.relative_to(rel).parts[0] for leaf in links if rel in leaf.parents}

    for child in sorted(children):
        child_rel = rel / child
        target = dest_here / child
        if child_rel in links:
            os.symlink(links[child_rel], target)
        elif child_rel in expanded:
            _materialize(base, dest, child_rel, links, expanded)
        else:
            os.symlink(base_here / child, target)


def build_fix_overlay(base_dir: str, overlays: Dict[str, str], dest_dir: str) -> str:
    """
    Build an overlay fix tree at ``dest_dir`` from ``base_dir`` and ``overlays``.

    Parameters
    ----------
    base_dir : str
        The installed fix tree, normally ``${HOMEglobal}/fix``.  Untouched
        entries link back here, so the overlay follows re-links of the install.
    overlays : dict
        ``dst: src`` map from the experiment YAML ``fix:`` section.
    dest_dir : str
        Where to build the overlay, normally ``${EXPDIR}/fix``.  If it already
        holds an overlay (identified by its manifest) it is rebuilt; anything
        else at that path is an error.

    Returns
    -------
    str
        ``dest_dir`` as an absolute path, suitable for ``FIXglobal``.
    """
    if not isinstance(overlays, dict):
        raise FixOverlayError(f"fix: section must be a mapping of destination: source, got {type(overlays).__name__}")

    base = Path(base_dir).absolute()
    dest = Path(dest_dir).absolute()

    if not base.is_dir():
        raise FixOverlayError(f"fix: base fix directory does not exist: '{base}'")
    if dest == base or base in dest.parents:
        raise FixOverlayError(f"fix: overlay destination '{dest}' may not be inside the base fix directory")

    links, overrides = _expand_entries(overlays)

    if dest.is_symlink() or dest.exists():
        if dest.is_dir() and not dest.is_symlink() and (dest / MANIFEST_NAME).exists():
            logger.info(f"Removing previous fix overlay at {dest}")
            shutil.rmtree(dest)
        else:
            raise FixOverlayError(f"fix: '{dest}' exists and is not a fix overlay; remove it first")

    # Every proper ancestor of a leaf must be a real directory in the overlay.
    expanded = {ancestor for leaf in links for ancestor in leaf.parents if ancestor != PurePosixPath('.')}

    _materialize(base, dest, PurePosixPath('.'), links, expanded)

    manifest = {
        'base': str(base),
        'entries': {str(k): v for k, v in overlays.items()},
        'links': {str(k): v for k, v in sorted(links.items())},
        'overrides': overrides,
    }
    save_as_yaml(manifest, str(dest / MANIFEST_NAME))

    logger.info(f"Built fix overlay at {dest} ({len(links)} link(s) over {base})")
    for leaf, src in sorted(links.items()):
        logger.info(f"  {leaf} -> {src}")

    return str(dest)

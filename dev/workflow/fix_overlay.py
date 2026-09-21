#!/usr/bin/env python3

"""
Build a per-experiment fix directory that overlays user-supplied files on top of
the installed ``${HOMEglobal}/fix`` tree.

The overlay is described by the ``fix_files:`` section of the experiment YAML.
It has two sub-sections, ``replace:`` and ``add:``.  Each is a map of
``<path under fix/>: <your path>``, where ``<your path>`` is an absolute file,
directory or glob, or a list of those.

``replace:``
    Every path named, or matched by a glob, must already exist in the fix tree
    and is swapped for yours.  The source must be the same kind of thing as
    what it replaces (file for file, directory for directory).
``add:``
    Every path named, or matched by a glob, must *not* exist in the fix tree
    yet and is linked in.

A single plain source is linked *at* the destination path.  A glob, or a
list of sources, means the destination is a directory: each match, or each
listed file or directory, is linked into it under its own name, and each is
checked individually against the rule of its sub-section.  Under ``replace``
the destination directory must already exist; under ``add`` it may exist (new
files go beside the existing ones) or not (it is created).

Any path that disagrees with the sub-section it is under is an error, so a
misspelled name can neither create a stray file nor clobber a system one.
Two entries that land on the same path are an error, as is an entry nested
inside a directory that another entry links wholesale.

The resulting tree is built lazily: any directory that no entry touches is a
single symlink back to the base tree, so an overlay of one file under
``mom6/008/`` expands only ``mom6`` and ``mom6/008``, and every other component
remains a single link.

Two kinds of ``Path`` are used below and must not be mixed up:

* ``base`` and ``dest`` are absolute and refer to real directories.
* every other ``Path`` (``dst``, ``leaf``, ``rel``, ``ancestor``) is *relative
  to the fix root* and is only ever used as a key: joined onto ``base``/``dest``,
  compared, or walked with ``.parents``.  Never call ``.exists()``, ``.is_dir()``,
  ``.glob()``, ``.mkdir()`` etc. on one of these; a relative ``Path`` resolves
  against the current working directory, so such a call would silently answer
  about the wrong tree instead of failing.
"""

import glob
import os
import shutil
from logging import getLogger
from pathlib import Path
from typing import Dict, List, Tuple, Union

from wxflow import save_as_yaml

__all__ = ['build_fix_overlay', 'FixOverlayError', 'MANIFEST_NAME', 'MODES']

logger = getLogger(__name__)

MANIFEST_NAME = '.fix_overlay.yaml'
MODES = ('replace', 'add')
_GLOB_CHARS = set('*?[')


class FixOverlayError(ValueError):
    """Raised for an invalid or unsatisfiable ``fix_files:`` section."""


def _is_glob(path: str) -> bool:
    return any(ch in _GLOB_CHARS for ch in path)


def _kind(path: str) -> str:
    return 'directory' if os.path.isdir(path) else 'file'


def _normalize_dst(mode: str, dst: str) -> Path:
    """
    Validate a destination and return it as a relative, normalized path.

    Raises
    ------
    FixOverlayError
        If ``dst`` is empty, absolute, or would escape the fix root.
    """
    if not isinstance(dst, str) or not dst.strip():
        raise FixOverlayError(f"fix_files: {mode}: path must be a non-empty path under fix/, got {dst!r}")

    dst = dst.strip()
    if dst.startswith('/'):
        raise FixOverlayError(f"fix_files: {mode}: '{dst}' must be relative to fix/, not absolute")

    parts = [p for p in Path(dst).parts if p not in ('', '.')]
    if not parts:
        raise FixOverlayError(f"fix_files: {mode}: '{dst}' is the fix root itself; "
                              "name individual components instead")
    if '..' in parts:
        raise FixOverlayError(f"fix_files: {mode}: '{dst}' may not contain '..'")

    return Path(*parts)


def _normalize_srcs(mode: str, dst: str, raw: Union[str, List[str]]) -> List[str]:
    """Return the entry's source(s) as a list of absolute paths."""
    srcs = raw if isinstance(raw, list) else [raw]
    if not srcs:
        raise FixOverlayError(f"fix_files: {mode}: '{dst}' has an empty list of sources")
    out = []
    for src in srcs:
        if not isinstance(src, str) or not src.strip():
            raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' must be a non-empty path, got {src!r}")
        src = os.path.expanduser(src.strip())
        if not os.path.isabs(src):
            raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' must be an absolute path, got '{src}'")
        out.append(src)
    return out


def _check_leaf(base: Path, mode: str, leaf: Path, src: str, label: str) -> None:
    """Enforce the sub-section's rule for one destination path."""
    in_base = (base / leaf).exists()
    if mode == 'replace':
        if not in_base:
            raise FixOverlayError(f"fix_files: replace: '{leaf}' ({label}) is not in the fix tree; "
                                  "use 'add' for a new file or directory")
        have, want = _kind(str(base / leaf)), _kind(src)
        if have != want:
            raise FixOverlayError(f"fix_files: replace: '{leaf}' ({label}) is a {have} in the fix tree "
                                  f"but the source is a {want}: '{src}'")
    elif in_base:
        hint = "use 'replace' to swap it for yours"
        if (base / leaf).is_dir() and not os.path.isdir(src):
            hint += f"; to add a file into that directory, name the full path '{leaf / os.path.basename(src)}'"
        raise FixOverlayError(f"fix_files: add: '{leaf}' ({label}) already exists in the fix tree; {hint}")


def _expand_entries(base: Path, fix_files: Dict[str, Dict[str, Union[str, List[str]]]]) \
        -> Tuple[Dict[Path, str], List[Dict[str, object]]]:
    """
    Turn the ``fix_files:`` section into a map of leaf destinations to sources,
    checking every leaf against the base tree per the module docstring.

    Returns
    -------
    links : dict
        Leaf destination -> source.
    applied : list
        One record per (entry, source), for the summary: mode, dst, src, number of files.

    Raises
    ------
    FixOverlayError
        On a malformed section, a missing source, an empty glob, a path that
        disagrees with its sub-section, or two entries landing on one path.
    """
    unknown = set(fix_files) - set(MODES)
    if unknown:
        raise FixOverlayError(f"fix_files: unknown sub-section(s) {sorted(unknown)}; "
                              f"expected one or more of {list(MODES)}")

    links: Dict[Path, str] = {}
    origin: Dict[Path, str] = {}  # leaf -> "mode: dst" that produced it, for messages
    applied: List[Dict[str, object]] = []

    for mode in MODES:
        entries = fix_files.get(mode) or {}
        if not isinstance(entries, dict):
            raise FixOverlayError(f"fix_files: {mode}: must be a mapping of <path under fix/>: <your path>, "
                                  f"got {type(entries).__name__}")

        for raw_dst, raw_src in entries.items():
            dst = _normalize_dst(mode, raw_dst)
            entry = f"{mode}: {raw_dst}"

            # A list, or a glob, means dst is a directory that the sources go into.
            into_dir = isinstance(raw_src, list)
            for src in _normalize_srcs(mode, raw_dst, raw_src):
                if into_dir or _is_glob(src):
                    if (base / dst).exists() and not (base / dst).is_dir():
                        raise FixOverlayError(f"fix_files: {mode}: '{dst}' is a file in the fix tree; "
                                              f"a glob or a list of sources needs a directory destination: '{src}'")
                    if mode == 'replace' and not (base / dst).exists():
                        raise FixOverlayError(f"fix_files: replace: '{dst}' is not in the fix tree; "
                                              "use 'add' to create a new directory")

                if _is_glob(src):
                    matches = sorted(glob.glob(src))
                    if not matches:
                        raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' matched nothing: '{src}'")
                    leaves = [(dst / os.path.basename(m), m) for m in matches]
                    label = f"matched by '{entry}'"
                else:
                    if not os.path.exists(src):
                        raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' does not exist: '{src}'")
                    leaves = [(dst / os.path.basename(src) if into_dir else dst, src)]
                    label = f"from '{entry}'"

                for leaf, leaf_src in leaves:
                    for ancestor in leaf.parents:
                        if (base / ancestor).exists() and not (base / ancestor).is_dir():
                            raise FixOverlayError(f"fix_files: {mode}: cannot place '{leaf}' ({label}) under "
                                                  f"'{ancestor}', which is a file in the fix tree")
                    _check_leaf(base, mode, leaf, leaf_src, label)
                    if leaf in links:
                        raise FixOverlayError(f"fix_files: '{leaf}' is set by both '{origin[leaf]}' and '{entry}'")
                    links[leaf] = leaf_src
                    origin[leaf] = entry
                applied.append({'mode': mode, 'dst': str(dst), 'src': src, 'files': len(leaves)})

    # A leaf that sits inside another leaf would be written through the
    # linking symlink into the user's source tree; refuse that.
    for leaf in links:
        for ancestor in leaf.parents:
            if ancestor in links:
                raise FixOverlayError(f"fix_files: '{leaf}' (from '{origin[leaf]}') lies inside "
                                      f"'{ancestor}' (from '{origin[ancestor]}'), which is linked wholesale")

    return links, applied


def _materialize(base: Path, dest: Path, rel: Path,
                 links: Dict[Path, str], expanded: set) -> None:
    """
    Create ``dest/rel`` as a real directory whose children are either overlay
    links, further expanded directories, or links back to ``base/rel/<child>``.
    """
    base_here = base / rel
    dest_here = dest / rel
    dest_here.mkdir(parents=True, exist_ok=False)

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


def _summary(base: Path, dest: Path, applied: List[Dict[str, object]]) -> str:
    width = max((len(str(a['dst'])) for a in applied), default=0)
    lines = [f"Fix files: {dest}  (overlay on {base})"]
    for a in applied:
        count = f"  ({a['files']} files)" if a['files'] != 1 or _is_glob(str(a['src'])) else ''
        lines.append(f"  {a['mode']:<8} {str(a['dst']):<{width}}{count}  <- {a['src']}")
    return '\n'.join(lines)


def build_fix_overlay(base_dir: str, fix_files: Dict[str, Dict[str, Union[str, List[str]]]], dest_dir: str) -> str:
    """
    Build an overlay fix tree at ``dest_dir`` from ``base_dir`` and ``fix_files``.

    Parameters
    ----------
    base_dir : str
        The installed fix tree, normally ``${HOMEglobal}/fix``.  Untouched
        entries link back here, so the overlay follows re-links of the install.
    fix_files : dict
        The ``fix_files:`` section of the experiment YAML: maps under
        ``replace`` and/or ``add``.
    dest_dir : str
        Where to build the overlay, normally ``${EXPDIR}/fix``.  If it already
        holds an overlay (identified by its manifest) it is rebuilt; anything
        else at that path is an error.

    Returns
    -------
    str
        ``dest_dir`` as an absolute path, suitable for ``FIXglobal``.
    """
    if not isinstance(fix_files, dict):
        raise FixOverlayError(f"fix_files: must be a mapping with 'replace' and/or 'add' sub-sections, "
                              f"got {type(fix_files).__name__}")

    base = Path(base_dir).absolute()
    dest = Path(dest_dir).absolute()

    if not base.is_dir():
        raise FixOverlayError(f"fix_files: base fix directory does not exist: '{base}'")
    if dest == base or base in dest.parents:
        raise FixOverlayError(f"fix_files: overlay destination '{dest}' may not be inside the base fix directory")

    links, applied = _expand_entries(base, fix_files)

    if dest.is_symlink() or dest.exists():
        if dest.is_dir() and not dest.is_symlink() and (dest / MANIFEST_NAME).exists():
            logger.info(f"Removing previous fix overlay at {dest}")
            shutil.rmtree(dest)
        else:
            raise FixOverlayError(f"fix_files: '{dest}' exists and is not a fix overlay; remove it first")

    # Every proper ancestor of a leaf must be a real directory in the overlay.
    expanded = {ancestor for leaf in links for ancestor in leaf.parents if ancestor != Path('.')}

    _materialize(base, dest, Path('.'), links, expanded)

    manifest = {
        'base': str(base),
        'entries': {mode: dict(fix_files.get(mode) or {}) for mode in MODES if fix_files.get(mode)},
        'links': {str(k): v for k, v in sorted(links.items())},
    }
    save_as_yaml(manifest, str(dest / MANIFEST_NAME))

    logger.info(_summary(base, dest, applied))

    return str(dest)

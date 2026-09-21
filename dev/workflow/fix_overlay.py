#!/usr/bin/env python3

"""
Build a per-experiment fix directory that overlays user-supplied files on top of
the installed ``${HOMEglobal}/fix`` tree.

The overlay is described by the ``fix_files:`` section of the experiment YAML.
It has three sub-sections, each a flat map of ``<path under fix/>: <your path>``:

``replace:``
    The path already exists in the fix tree and is swapped for yours.  The
    source must be the same kind of thing (file for file, directory for
    directory).  An entry whose path is *not* in the fix tree is an error.
``add:``
    The path does not exist in the fix tree yet and is linked in.  An entry
    whose path *is* already in the fix tree is an error.
``merge:``
    The path is an existing directory in the fix tree; every file in the
    source directory (or every match of a source glob) is linked into it beside
    the files already there.  Merging into a directory that is not in the fix
    tree is an error.

``replace``/``add`` entries always take precedence over a ``merge`` match for
the same file, so "everything in this directory, except this one file" is
written as a ``merge`` plus one ``replace``.

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
from typing import Dict, List, Tuple

from wxflow import save_as_yaml

__all__ = ['build_fix_overlay', 'FixOverlayError', 'MANIFEST_NAME', 'MODES']

logger = getLogger(__name__)

MANIFEST_NAME = '.fix_overlay.yaml'
# Order of application: explicit single-path entries win over merge matches.
MODES = ('merge', 'add', 'replace')
_GLOB_CHARS = set('*?[')


class FixOverlayError(ValueError):
    """Raised for an invalid or unsatisfiable ``fix_files:`` section."""


def _is_glob(path: str) -> bool:
    return any(ch in _GLOB_CHARS for ch in path)


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


def _normalize_src(mode: str, dst: str, src: str) -> str:
    if not isinstance(src, str) or not src.strip():
        raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' must be a non-empty path, got {src!r}")
    src = os.path.expanduser(src.strip())
    if not os.path.isabs(src):
        raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' must be an absolute path, got '{src}'")
    return src


def _kind(path: str) -> str:
    return 'directory' if os.path.isdir(path) else 'file'


def _expand_entries(base: Path, fix_files: Dict[str, Dict[str, str]]) \
        -> Tuple[Dict[Path, str], List[Dict[str, str]], List[Dict[str, object]]]:
    """
    Turn the ``fix_files:`` section into a map of leaf destinations to sources.

    Each mode is validated against what the base tree actually contains (see
    the module docstring).  Modes are applied in ``MODES`` order so that
    ``replace``/``add`` entries override ``merge`` matches on the same leaf.

    Returns
    -------
    links : dict
        Leaf destination -> source.
    overrides : list
        One record per leaf that a later entry replaced.
    applied : list
        One record per user entry, for the summary: mode, dst, src, number of files.

    Raises
    ------
    FixOverlayError
        On a malformed section, a missing source, an empty glob, or an entry
        whose mode disagrees with the base tree.
    """
    unknown = set(fix_files) - set(MODES)
    if unknown:
        raise FixOverlayError(f"fix_files: unknown sub-section(s) {sorted(unknown)}; "
                              f"expected one or more of {list(MODES)}")

    links: Dict[Path, str] = {}
    origin: Dict[Path, str] = {}  # leaf -> "mode: dst" that produced it, for messages
    overrides: List[Dict[str, str]] = []
    applied: List[Dict[str, object]] = []

    for mode in MODES:
        entries = fix_files.get(mode) or {}
        if not isinstance(entries, dict):
            raise FixOverlayError(f"fix_files: {mode}: must be a mapping of <path under fix/>: <your path>, "
                                  f"got {type(entries).__name__}")

        for raw_dst, raw_src in entries.items():
            dst = _normalize_dst(mode, raw_dst)
            src = _normalize_src(mode, raw_dst, raw_src)
            in_base = (base / dst).exists()

            if mode == 'merge':
                if not in_base or not (base / dst).is_dir():
                    raise FixOverlayError(f"fix_files: merge: '{dst}' is not a directory in the fix tree; "
                                          "use 'add' to create a new directory")
                pattern = src if _is_glob(src) else os.path.join(src, '*')
                if not _is_glob(src) and not os.path.isdir(src):
                    raise FixOverlayError(f"fix_files: merge: source for '{dst}' must be a directory or a glob, "
                                          f"got '{src}'")
                matches = sorted(glob.glob(pattern))
                if not matches:
                    raise FixOverlayError(f"fix_files: merge: source for '{dst}' matched nothing: '{pattern}'")
                leaves: List[Tuple[Path, str]] = [(dst / os.path.basename(m), m) for m in matches]

            else:  # replace / add: a single path
                if _is_glob(src):
                    raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' may not be a glob; "
                                          "use 'merge' to link several files into a directory")
                if not os.path.exists(src):
                    raise FixOverlayError(f"fix_files: {mode}: source for '{dst}' does not exist: '{src}'")
                if mode == 'replace':
                    if not in_base:
                        raise FixOverlayError(f"fix_files: replace: '{dst}' is not in the fix tree; "
                                              "use 'add' for a new file or directory")
                    if _kind(str(base / dst)) != _kind(src):
                        raise FixOverlayError(f"fix_files: replace: '{dst}' is a {_kind(str(base / dst))} in the "
                                              f"fix tree but the source is a {_kind(src)}: '{src}'")
                elif in_base:
                    raise FixOverlayError(f"fix_files: add: '{dst}' already exists in the fix tree; "
                                          "use 'replace' to swap it for yours")
                leaves = [(dst, src)]

            label = f"{mode}: {raw_dst}"
            for leaf, leaf_src in leaves:
                if leaf in links:
                    logger.info(f"fix_files: '{leaf}' from '{origin[leaf]}' overridden by '{label}'")
                    overrides.append({'dst': str(leaf), 'from': origin[leaf], 'by': label,
                                      'was': links[leaf], 'now': leaf_src})
                links[leaf] = leaf_src
                origin[leaf] = label
            applied.append({'mode': mode, 'dst': str(dst), 'src': src, 'files': len(leaves)})

    # A leaf that sits inside another leaf would be written through the
    # replacing symlink into the user's source tree; refuse that.
    for leaf in links:
        for ancestor in leaf.parents:
            if ancestor in links:
                raise FixOverlayError(f"fix_files: '{leaf}' (from '{origin[leaf]}') lies inside "
                                      f"'{ancestor}' (from '{origin[ancestor]}'), which is linked wholesale")

    return links, overrides, applied


def _materialize(base: Path, dest: Path, rel: Path,
                 links: Dict[Path, str], expanded: set) -> None:
    """
    Create ``dest/rel`` as a real directory whose children are either overlay
    links, further expanded directories, or links back to ``base/rel/<child>``.
    """
    base_here = base / rel
    dest_here = dest / rel
    dest_here.mkdir(parents=True, exist_ok=False)

    if base_here.exists() and not base_here.is_dir():
        raise FixOverlayError(f"fix_files: cannot place entries under '{rel}': '{base_here}' is not a directory")

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
        count = f"  ({a['files']} files)" if a['mode'] == 'merge' else ''
        lines.append(f"  {a['mode']:<8} {str(a['dst']):<{width}}{count}  <- {a['src']}")
    return '\n'.join(lines)


def build_fix_overlay(base_dir: str, fix_files: Dict[str, Dict[str, str]], dest_dir: str) -> str:
    """
    Build an overlay fix tree at ``dest_dir`` from ``base_dir`` and ``fix_files``.

    Parameters
    ----------
    base_dir : str
        The installed fix tree, normally ``${HOMEglobal}/fix``.  Untouched
        entries link back here, so the overlay follows re-links of the install.
    fix_files : dict
        The ``fix_files:`` section of the experiment YAML: up to three maps
        under ``replace``, ``add`` and ``merge``.
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
        raise FixOverlayError(f"fix_files: must be a mapping with 'replace', 'add' and/or 'merge' sub-sections, "
                              f"got {type(fix_files).__name__}")

    base = Path(base_dir).absolute()
    dest = Path(dest_dir).absolute()

    if not base.is_dir():
        raise FixOverlayError(f"fix_files: base fix directory does not exist: '{base}'")
    if dest == base or base in dest.parents:
        raise FixOverlayError(f"fix_files: overlay destination '{dest}' may not be inside the base fix directory")

    links, overrides, applied = _expand_entries(base, fix_files)

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
        'overrides': overrides,
    }
    save_as_yaml(manifest, str(dest / MANIFEST_NAME))

    logger.info(_summary(base, dest, applied))

    return str(dest)

#!/usr/bin/env python3
"""
global_net_converter.py
Convert between HOMEglobal-style and HOME${NET}-style variable names.

Can be used as a standalone CLI tool or imported as a module:

For programmatic usage, create a script path/to/example_convert.py:

    from global_net_converter import GlobalToNetConverter, NetToGlobalConverter

    REPO_ROOT = '/path/to/global-workflow/'
    TARGET_PATH = 'dev/'

    # Convert HOMEglobal -> HOMEgfs
    GlobalToNetConverter().convert(REPO_ROOT, TARGET_PATH, 'gfs')

    # Convert HOMEgfs -> HOMEglobal
    NetToGlobalConverter().convert(REPO_ROOT, TARGET_PATH, 'gfs')

Then run it:
    python path/to/example_convert.py

CLI usage:
    # Convert all HOMEglobal-style variables to HOMEgfs-style variables in the target directory,
    # excluding any files or directories named 'sorc'.
    global_net_converter.py to-net gfs /path/to/repo_root relative/path/to/target --exclude sorc
    # Convert all HOMEgfs-style variables HOMEglobal-style variables in the target directory,
    # excluding any files or directories named 'sorc'.
    global_net_converter.py to-global gfs /path/to/repo_root relative/path/to/target --exclude sorc
    # Convert all HOME${NET}-style variables (for all NET values: gfs, gefs, sfs, gcafs)
    # back to HOMEglobal-style variables in the target directory.
    global_net_converter.py to-global all /path/to/repo_root relative/path/to/target
"""

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Union


VALID_NET_VALUES = ('gfs', 'gefs', 'sfs', 'gcafs')
ALL_NET_VALUES = ('gefs', 'gfs', 'gcafs', 'sfs')

_SELF_PATH = Path(__file__).resolve()
_SELF_SCRIPTS = frozenset({
    'global_net_converter.py',
    'example_convert.py',
})


@dataclass
class ConversionResult:
    """Result of a conversion operation.

    Attributes
    ----------
    converted : int
        Number of files that were modified.
    failed : int
        Number of files that could not be processed.
    skipped : int
        Number of files that contained no matching patterns.
    """

    converted: int = 0
    failed: int = 0
    skipped: int = 0

    @property
    def total_processed(self) -> int:
        return self.converted + self.failed + self.skipped

    @property
    def success(self) -> bool:
        return self.failed == 0


# ---------------------------------------------------------------------------
# Shared file-processing helpers
# ---------------------------------------------------------------------------

def _process_file(filepath: Path, patterns: dict):
    """Apply word-boundary replacements to a file.

    Returns
    -------
    tuple[bool, bool]
        (modified, failed)
    """
    try:
        content = filepath.read_text(errors='replace')
    except OSError as exc:
        print(f"ERROR: Could not read {filepath}: {exc}", file=sys.stderr)
        return False, True

    new_content = content
    for pattern, replacement in patterns.items():
        new_content = re.sub(rf'\b{re.escape(pattern)}\b', replacement, new_content)

    if new_content == content:
        return False, False

    try:
        filepath.write_text(new_content)
    except OSError as exc:
        print(f"ERROR: Could not write {filepath}: {exc}", file=sys.stderr)
        return False, True

    return True, False


def _iter_files(dirpath: Path, exclude_names):
    for path in dirpath.rglob('*'):
        if path.is_dir():
            continue
        if path.resolve() == _SELF_PATH:
            continue
        if path.name in exclude_names:
            continue
        if any(p.name in exclude_names for p in path.parents):
            continue
        yield path


def _validate_target(target: Path) -> None:
    if not target.exists():
        raise FileNotFoundError(f"Target path does not exist: {target}")


# ---------------------------------------------------------------------------
# GlobalToNetConverter
# ---------------------------------------------------------------------------

class GlobalToNetConverter:
    """Convert HOMEglobal-style variables to HOME${NET}-specific variables.

    Parameters
    ----------
    verbose : bool
        Print progress to stdout (default True).

    Examples
    --------
    Convert a single file:

    >>> converter = GlobalToNetConverter()
    >>> result = converter.convert_file('/path/to/script.sh', 'gfs')

    Convert an entire directory, excluding some sub-paths:

    >>> result = converter.convert_directory(
    ...     '/path/to/repo', 'gefs', exclude=['sorc', 'parm/archive']
    ... )

    Convert a file or directory (auto-detected):

    >>> result = converter.convert('/path/to/target', 'sfs')
    """

    VALID_NET_VALUES = VALID_NET_VALUES

    def __init__(self, verbose: bool = True) -> None:
        self.verbose = verbose

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def convert(
        self,
        base_path: Union[str, Path],
        relative_path: str,
        net: str,
        exclude: Optional[List[str]] = None,
    ) -> ConversionResult:
        """Convert a file or directory (auto-detected).

        Parameters
        ----------
        base_path : str or Path
            Absolute base path (e.g. repository root).
        relative_path : str
            Relative path to the target file or directory within base_path.
        net : str
            NET value to convert to. Must be one of VALID_NET_VALUES.
        exclude : list of str, optional
            Directory/file basenames to exclude when target is a directory.

        Returns
        -------
        ConversionResult

        Raises
        ------
        ValueError
            If net is not a valid NET value.
        FileNotFoundError
            If target does not exist.
        """
        target = Path(base_path) / relative_path.strip('/')
        self._validate_net(net)
        _validate_target(target)

        if target.is_file():
            return self.convert_file(target, net)
        return self.convert_directory(target, net, exclude=exclude)

    def convert_file(
        self,
        filepath: Union[str, Path],
        net: str,
    ) -> ConversionResult:
        """Convert a single file in-place.

        Parameters
        ----------
        filepath : str or Path
            Path to the file to convert.
        net : str
            NET value to convert to. Must be one of VALID_NET_VALUES.

        Returns
        -------
        ConversionResult
        """
        filepath = Path(filepath)
        self._validate_net(net)

        patterns = self._get_patterns(net)
        modified, failed = _process_file(filepath, patterns)

        result = ConversionResult(
            converted=1 if modified else 0,
            failed=1 if failed else 0,
            skipped=0 if (modified or failed) else 1,
        )

        if self.verbose:
            if failed:
                print(f"ERROR: Failed to process {filepath}", file=sys.stderr)
            elif modified:
                print(f"Processed 1 file for NET={net}")
            else:
                print(f"No patterns found in {filepath} for NET={net}")

        return result

    def convert_directory(
        self,
        dirpath: Union[str, Path],
        net: str,
        exclude: Optional[List[str]] = None,
    ) -> ConversionResult:
        """Convert all files in a directory tree in-place.

        Parameters
        ----------
        dirpath : str or Path
            Root directory to process.
        net : str
            NET value to convert to. Must be one of VALID_NET_VALUES.
        exclude : list of str, optional
            Directory/file basenames to skip (matched by basename).

        Returns
        -------
        ConversionResult
        """
        dirpath = Path(dirpath)
        self._validate_net(net)
        _validate_target(dirpath)

        exclude = exclude or []
        exclude_names = {Path(e).name for e in exclude} | _SELF_SCRIPTS
        display_exclude = [e for e in exclude if Path(e).name not in _SELF_SCRIPTS]
        patterns = self._get_patterns(net)

        if self.verbose:
            print("=========================================")
            print(f"Converting global-workflow variables to {net}-specific variables")
            print(f"Target: {dirpath}")
            if display_exclude:
                print(f"Excluding: {' '.join(display_exclude)}")
            print("=========================================")
            print(f"Converting: global -> {net}")

        files = list(_iter_files(dirpath, exclude_names))
        result = ConversionResult()

        if not files:
            if self.verbose:
                print(f"No files to convert for NET={net}")
            return result

        if self.verbose:
            print(f"Processing {len(files)} files...")

        for f in files:
            modified, failed = _process_file(f, patterns)
            if failed:
                result.failed += 1
            elif modified:
                result.converted += 1
            else:
                result.skipped += 1

        if self.verbose:
            self._print_summary(result, net)

        return result

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _get_patterns(net: str) -> dict:
        return {
            'HOMEglobal': f'HOME{net}',
            'PARMglobal': f'PARM{net}',
            'USHglobal': f'USH{net}',
            'SCRglobal': f'SCR{net}',
            'EXECglobal': f'EXEC{net}',
            'FIXglobal': f'FIX{net}',
        }

    @staticmethod
    def _validate_net(net: str) -> None:
        if net not in VALID_NET_VALUES:
            raise ValueError(
                f"Invalid NET value '{net}'. Must be one of: {', '.join(VALID_NET_VALUES)}"
            )

    @staticmethod
    def _print_summary(result: ConversionResult, net: str) -> None:
        print()
        if result.converted == 0:
            print(f"No files to convert for NET={net}")
        elif result.failed > 0:
            print(f"Converted {result.converted} files ({result.failed} failed) for NET={net}")
        else:
            print(f"Converted {result.converted} files for NET={net}")
        print()
        print("=========================================")
        print("Conversion completed successfully!")
        print("=========================================")


# ---------------------------------------------------------------------------
# NetToGlobalConverter
# ---------------------------------------------------------------------------

class NetToGlobalConverter:
    """Convert HOME${NET}-style variables back to HOMEglobal-style variables.

    Parameters
    ----------
    verbose : bool
        Print progress to stdout (default True).

    Examples
    --------
    Convert a single file:

    >>> converter = NetToGlobalConverter()
    >>> result = converter.convert_file('/path/to/script.sh', 'gfs')

    Convert an entire directory for all NET values:

    >>> result = converter.convert_directory(
    ...     '/path/to/repo', 'all', exclude=['sorc', 'parm/archive']
    ... )

    Convert a file or directory (auto-detected):

    >>> result = converter.convert('/path/to/target', 'gefs')
    """

    VALID_NET_VALUES = VALID_NET_VALUES
    ALL_NET_VALUES = ALL_NET_VALUES

    def __init__(self, verbose: bool = True) -> None:
        self.verbose = verbose

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def convert(
        self,
        base_path: Union[str, Path],
        relative_path: str,
        net: str,
        exclude: Optional[List[str]] = None,
    ) -> ConversionResult:
        """Convert a file or directory (auto-detected).

        Parameters
        ----------
        base_path : str or Path
            Absolute base path (e.g. repository root).
        relative_path : str
            Relative path to the target file or directory within base_path.
        net : str
            NET value to revert. One of VALID_NET_VALUES or 'all'.
        exclude : list of str, optional
            Directory/file basenames to exclude when target is a directory.

        Returns
        -------
        ConversionResult

        Raises
        ------
        ValueError
            If net is not a valid NET value.
        FileNotFoundError
            If target does not exist.
        """
        target = Path(base_path) / relative_path.strip('/')
        self._validate_net(net)
        _validate_target(target)

        if target.is_file():
            return self.convert_file(target, net)
        return self.convert_directory(target, net, exclude=exclude)

    def convert_file(
        self,
        filepath: Union[str, Path],
        net: str,
    ) -> ConversionResult:
        """Convert a single file in-place.

        Parameters
        ----------
        filepath : str or Path
            Path to the file to convert.
        net : str
            NET value to revert. One of VALID_NET_VALUES or 'all'.

        Returns
        -------
        ConversionResult
        """
        filepath = Path(filepath)
        self._validate_net(net)

        net_list = ALL_NET_VALUES if net == 'all' else (net,)
        result = ConversionResult()

        for current_net in net_list:
            patterns = self._get_patterns(current_net)
            modified, failed = _process_file(filepath, patterns)
            if failed:
                result.failed += 1
            elif modified:
                result.converted += 1
            else:
                result.skipped += 1

        if self.verbose:
            if result.failed:
                print(f"ERROR: Failed to process {filepath}", file=sys.stderr)
            elif result.converted:
                print(f"Processed {filepath}")
            else:
                print(f"No patterns found in {filepath}")

        return result

    def convert_directory(
        self,
        dirpath: Union[str, Path],
        net: str,
        exclude: Optional[List[str]] = None,
    ) -> ConversionResult:
        """Convert all files in a directory tree in-place.

        Parameters
        ----------
        dirpath : str or Path
            Root directory to process.
        net : str
            NET value to revert. One of VALID_NET_VALUES or 'all'.
        exclude : list of str, optional
            Directory/file basenames to skip (matched by basename).

        Returns
        -------
        ConversionResult
        """
        dirpath = Path(dirpath)
        self._validate_net(net)
        _validate_target(dirpath)

        exclude = exclude or []
        exclude_names = {Path(e).name for e in exclude} | _SELF_SCRIPTS
        display_exclude = [e for e in exclude if Path(e).name not in _SELF_SCRIPTS]
        net_list = ALL_NET_VALUES if net == 'all' else (net,)

        if self.verbose:
            print("=========================================")
            if net == 'all':
                print(f"Converting NET-specific variables to global-workflow variables "
                      f"from: {' '.join(net_list)}")
            else:
                print(f"Converting {net}-specific variables to global-workflow variables")
            print(f"Target: {dirpath}")
            if display_exclude:
                print(f"Excluding: {' '.join(display_exclude)}")
            print("=========================================")

        files = list(_iter_files(dirpath, exclude_names))
        combined = ConversionResult()

        for current_net in net_list:
            if self.verbose:
                print(f"Converting: {current_net} -> global")

            patterns = self._get_patterns(current_net)
            result = ConversionResult()

            if not files:
                if self.verbose:
                    print("No files to convert")
                continue

            if self.verbose:
                print(f"Processing {len(files)} files...")

            for f in files:
                modified, failed = _process_file(f, patterns)
                if failed:
                    result.failed += 1
                elif modified:
                    result.converted += 1
                else:
                    result.skipped += 1

            combined.converted += result.converted
            combined.failed += result.failed
            combined.skipped += result.skipped

            if self.verbose:
                self._print_net_summary(result, current_net)

        if self.verbose:
            print()
            print("=========================================")
            print("All conversions completed successfully!")
            print("=========================================")

        return combined

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _get_patterns(net: str) -> dict:
        return {
            f'HOME{net}': 'HOMEglobal',
            f'PARM{net}': 'PARMglobal',
            f'USH{net}': 'USHglobal',
            f'SCR{net}': 'SCRglobal',
            f'EXEC{net}': 'EXECglobal',
            f'FIX{net}': 'FIXglobal',
        }

    @staticmethod
    def _validate_net(net: str) -> None:
        if net != 'all' and net not in VALID_NET_VALUES:
            raise ValueError(
                f"Invalid NET value '{net}'. Must be one of: {', '.join(VALID_NET_VALUES)}, or all"
            )

    @staticmethod
    def _print_net_summary(result: ConversionResult, net: str) -> None:
        if result.converted == 0:
            print(f"No files to convert for NET={net}")
        elif result.failed > 0:
            print(f"Converted {result.converted} files ({result.failed} failed) for NET={net}")
        else:
            print(f"Converted {result.converted} files for NET={net}")
        print()


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description='Convert between HOMEglobal-style and HOME${NET}-style variable names.'
    )
    subparsers = parser.add_subparsers(dest='direction', required=True)

    to_net = subparsers.add_parser(
        'to-net',
        help='Convert HOMEglobal etc. to HOME${NET} etc.'
    )
    to_net.add_argument('net', metavar='NET_value',
                        help=f'One of: {", ".join(VALID_NET_VALUES)}')
    to_net.add_argument('base_path', help='Absolute base path (e.g. repository root)')
    to_net.add_argument('relative_path', help='Relative path to the target within base_path')
    to_net.add_argument('--exclude', nargs='+', default=[], metavar='dir',
                        help='Paths to exclude (matched by basename)')

    to_global = subparsers.add_parser(
        'to-global',
        help='Convert HOME${NET} etc. back to HOMEglobal etc.'
    )
    to_global.add_argument('net', metavar='NET_value',
                           help=f'One of: {", ".join(VALID_NET_VALUES)}, or all')
    to_global.add_argument('base_path', help='Absolute base path (e.g. repository root)')
    to_global.add_argument('relative_path', help='Relative path to the target within base_path')
    to_global.add_argument('--exclude', nargs='+', default=[], metavar='dir',
                           help='Paths to exclude (matched by basename)')

    return parser


def main() -> None:
    args = _build_parser().parse_args()

    if args.direction == 'to-net':
        converter = GlobalToNetConverter(verbose=True)
    elif args.direction == 'to-global':
        converter = NetToGlobalConverter(verbose=True)
    else:
        raise ValueError(f"Unknown direction '{args.direction}'. Must be 'to-net' or 'to-global'.")

    try:
        result = converter.convert(args.base_path, args.relative_path, args.net, exclude=args.exclude)
    except (ValueError, FileNotFoundError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)

    if not result.success:
        sys.exit(1)


if __name__ == '__main__':
    main()

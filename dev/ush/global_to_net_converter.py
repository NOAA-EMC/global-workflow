#!/usr/bin/env python3
"""
global_to_net_converter.py
Converts HOMEglobal, PARMglobal, etc. to HOME${NET}, PARM${NET}, etc.

Can be used as a standalone CLI tool or imported as a module:

    from global_to_net_converter import GlobalToNetConverter

    converter = GlobalToNetConverter()
    result = converter.convert_file('/path/to/file.sh', 'gfs')
    result = converter.convert_directory('/path/to/dir', 'gfs', exclude=['sorc'])
    result = converter.convert('/path/to/file_or_dir', 'gfs')
"""

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Union


VALID_NET_VALUES = ('gfs', 'gefs', 'sfs', 'gcafs')

_SELF_SCRIPTS = frozenset({
    'convert_from_net_to_global.sh', 'convert_from_global_to_net.sh',
    'convert_from_net_to_global.py', 'convert_from_global_to_net.py',
    'net_to_global_converter.py', 'global_to_net_converter.py',
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
        target: Union[str, Path],
        net: str,
        exclude: Optional[List[str]] = None,
    ) -> ConversionResult:
        """Convert a file or directory (auto-detected).

        Parameters
        ----------
        target : str or Path
            Path to a file or directory.
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
        target = Path(target)
        self._validate_net(net)
        self._validate_target(target)

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
        modified, failed = self._process_file(filepath, patterns)

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
        self._validate_target(dirpath)

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

        files = list(self._iter_files(dirpath, exclude_names))
        result = ConversionResult()

        if not files:
            if self.verbose:
                print(f"No files to convert for NET={net}")
            return result

        if self.verbose:
            print(f"Processing {len(files)} files...")

        for f in files:
            modified, failed = self._process_file(f, patterns)
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
            'USHglobal':  f'USH{net}',
            'SCRglobal':  f'SCR{net}',
            'EXECglobal': f'EXEC{net}',
            'FIXglobal':  f'FIX{net}',
        }

    @staticmethod
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

    @staticmethod
    def _iter_files(dirpath: Path, exclude_names):
        for path in dirpath.rglob('*'):
            if path.is_dir():
                continue
            if any(p.name in exclude_names for p in path.parents):
                continue
            yield path

    @staticmethod
    def _validate_net(net: str) -> None:
        if net not in VALID_NET_VALUES:
            raise ValueError(
                f"Invalid NET value '{net}'. Must be one of: {', '.join(VALID_NET_VALUES)}"
            )

    @staticmethod
    def _validate_target(target: Path) -> None:
        if not target.exists():
            raise FileNotFoundError(f"Target path does not exist: {target}")

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
# CLI entry point
# ---------------------------------------------------------------------------

def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description='Convert HOMEglobal etc. to HOME${NET} etc.'
    )
    parser.add_argument('net', metavar='NET_value',
                        help=f'One of: {", ".join(VALID_NET_VALUES)}')
    parser.add_argument('target_path', help='File or directory to process')
    parser.add_argument('--exclude', nargs='+', default=[], metavar='dir',
                        help='Paths to exclude (matched by basename)')
    return parser


def main() -> None:
    args = _build_parser().parse_args()
    converter = GlobalToNetConverter(verbose=True)
    try:
        result = converter.convert(args.target_path, args.net, exclude=args.exclude)
    except (ValueError, FileNotFoundError) as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)

    if not result.success:
        sys.exit(1)


if __name__ == '__main__':
    main()

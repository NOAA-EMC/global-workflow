#!/usr/bin/env python3
"""
convert_from_net_to_global.py
Script to convert HOME${NET}, PARM${NET}, etc. back to HOMEglobal, PARMglobal, etc.
for development

Usage: convert_from_net_to_global.py <NET_value> <target_path> [--exclude dir1 dir2 ...]
NET_value can be: gfs, gefs, sfs, gcafs, or all (for all NET values)
target_path can be a file or directory

Example: convert_from_net_to_global.py gfs /path/to/development --exclude sorc dev parm/archive
Example: convert_from_net_to_global.py all /path/to/development
"""

import argparse
import re
import sys
from pathlib import Path

RED = '\033[0;31m'
GREEN = '\033[0;32m'
YELLOW = '\033[1;33m'
BLUE = '\033[0;34m'
CYAN = '\033[0;36m'
NC = '\033[0m'

ALL_NET_VALUES = ('gefs', 'gfs', 'gcafs', 'sfs')


def get_patterns(net):
    return {
        f'HOME{net}': 'HOMEglobal',
        f'PARM{net}': 'PARMglobal',
        f'USH{net}':  'USHglobal',
        f'SCR{net}':  'SCRglobal',
        f'EXEC{net}': 'EXECglobal',
        f'FIX{net}':  'FIXglobal',
    }


def process_file(filepath, patterns):
    """Replace all patterns in a file using word-boundary matching.

    Returns
    -------
    tuple[bool, bool]
        (modified, failed)
    """
    try:
        content = filepath.read_text(errors='replace')
    except OSError as exc:
        print(f"{RED}ERROR: Could not read {filepath}: {exc}{NC}", file=sys.stderr)
        return False, True

    new_content = content
    for pattern, replacement in patterns.items():
        new_content = re.sub(rf'\b{re.escape(pattern)}\b', replacement, new_content)

    if new_content == content:
        return False, False

    try:
        filepath.write_text(new_content)
    except OSError as exc:
        print(f"{RED}ERROR: Could not write {filepath}: {exc}{NC}", file=sys.stderr)
        return False, True

    return True, False


def get_files(target_path, exclude_names):
    """Yield all files under target_path, skipping dirs whose name is in exclude_names."""
    for path in target_path.rglob('*'):
        if path.is_dir():
            continue
        if any(p.name in exclude_names for p in path.parents):
            continue
        yield path


def main():
    parser = argparse.ArgumentParser(
        description='Convert HOME${NET} etc. back to HOMEglobal etc.'
    )
    parser.add_argument('net', metavar='NET_value',
                        help='One of: gfs, gefs, sfs, gcafs, or all')
    parser.add_argument('target_path',
                        help='File or directory to process')
    parser.add_argument('--exclude', nargs='+', default=[], metavar='dir',
                        help='Directories to exclude (matched by basename)')
    args = parser.parse_args()

    net = args.net
    if net == 'all':
        net_list = list(ALL_NET_VALUES)
    elif net in ALL_NET_VALUES:
        net_list = [net]
    else:
        print(f"ERROR: Invalid NET value '{net}'. "
              f"Must be one of: {', '.join(ALL_NET_VALUES)}, or all")
        sys.exit(1)

    target = Path(args.target_path)
    if not target.exists():
        print(f"{RED}ERROR: Target path {target} does not exist{NC}", file=sys.stderr)
        sys.exit(1)

    # Match shell behaviour: exclude by basename only
    exclude_names = {Path(e).name for e in args.exclude}

    # Filter self-scripts from the display list (mirrors the shell script)
    self_scripts = {'convert_from_net_to_global.sh', 'convert_from_global_to_net.sh',
                    'convert_from_net_to_global.py', 'convert_from_global_to_net.py'}
    display_exclude = [e for e in args.exclude if Path(e).name not in self_scripts]

    print(f"{CYAN}========================================={NC}")
    if net == 'all':
        print(f"{YELLOW}Processing: Converting NET-specific variables to "
              f"{GREEN}global{NC}{YELLOW}-workflow variables from: "
              f"{RED}{' '.join(net_list)}{NC}")
    else:
        print(f"{YELLOW}Processing: Converting {RED}{net}{NC}{YELLOW}-specific "
              f"variables to {GREEN}global{NC}{YELLOW}-workflow variables{NC}")
    print(f"{BLUE}Target: {target}{NC}")
    if display_exclude:
        print(f"{BLUE}Excluding directories: {' '.join(display_exclude)}{NC}")
    print(f"{CYAN}========================================={NC}")

    for current_net in net_list:
        print()
        print(f"{YELLOW}Converting for: {RED}{current_net}{NC} "
              f"{YELLOW}\u2192{NC} {GREEN}global{NC}")

        patterns = get_patterns(current_net)

        if target.is_file():
            modified, failed = process_file(target, patterns)
            if failed:
                sys.exit(1)
            if modified:
                print(f"{GREEN}\u2713 Processed 1 file for NET={current_net}{NC}")
            else:
                print(f"{YELLOW}No files to convert for NET={current_net}{NC}")
        else:
            files = list(get_files(target, exclude_names))
            if not files:
                print(f"{YELLOW}No files to convert{NC}")
            else:
                print(f"{BLUE}Processing {len(files)} files...{NC}")
                converted = 0
                failed_count = 0
                for f in files:
                    modified, failed = process_file(f, patterns)
                    if failed:
                        failed_count += 1
                    elif modified:
                        converted += 1

                if converted == 0:
                    print(f"{YELLOW}No files to convert for NET={current_net}{NC}")
                elif failed_count > 0:
                    print(f"{YELLOW}\u26a0 Converted {converted} files "
                          f"({failed_count} failed) for NET={current_net}{NC}")
                else:
                    print(f"{GREEN}\u2713 Converted {converted} files "
                          f"for NET={current_net}{NC}")

    print()
    print(f"{CYAN}========================================={NC}")
    print(f"{GREEN}All conversions completed successfully!{NC}")
    print(f"{CYAN}========================================={NC}")


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""
Utility module to find the HOMEgfs (repository root) directory by traversing up
the file system until finding a directory that contains the .github subdirectory.
"""

import os
from pathlib import Path


def find_homegfs(start_path=None):
    """
    Find the HOMEgfs directory by traversing up the file system until
    finding a directory that contains the .github subdirectory.

    Parameters
    ----------
    start_path : str or Path, optional
        The path to start searching from. If not provided, the current
        directory will be used.

    Returns
    -------
    Path
        The full path to the HOMEgfs directory.

    Raises
    ------
    ValueError
        If the HOMEgfs directory cannot be found.
    """
    # If start_path is not provided, use the directory of the calling script
    if start_path is None:
        # Get the path of the calling script
        start_path = os.getcwd()

    # Convert to Path object if it's a string
    if isinstance(start_path, str):
        start_path = Path(start_path)
    else:
        start_path = Path(start_path).resolve()

    # Start traversing up from the current directory
    current_dir = start_path

    # Traverse up until we find .github directory or reach the filesystem root
    while True:
        # Check if .github exists in the current directory
        if (current_dir / '.github').is_dir():
            return current_dir

        # Go up one level
        parent_dir = current_dir.parent

        # If we've reached the root directory and haven't found .github
        if parent_dir == current_dir:
            raise ValueError(
                "Could not find HOMEgfs directory. "
                "Traversed up to the root without finding a .github directory."
            )

        current_dir = parent_dir


if __name__ == '__main__':
    # Example usage when run as a script
    try:
        homegfs = find_homegfs()
        print(homegfs)
    except ValueError as e:
        print(f"Error: {e}")

"""
Compare two folders and produce a comprehensive report of which files
are unique to each folder and which are common. Creates a YAML file
listing the full paths of these files.
Usage:
    python compare_folders.py <folder1> <folder2> <output_yaml_path>
"""

import sys
import os
from pathlib import Path

try:
    import yaml
except ImportError:
    print("Please install pyyaml: pip install pyyaml")
    sys.exit(1)

def gather_files(folder):
    """Return a set of file paths (relative to 'folder') for all files within it."""
    folder = Path(folder).resolve()
    all_files = set()
    for root, dirs, files in os.walk(folder):
        for file_name in files:
            full_path = Path(root) / file_name
            # Store paths relative to 'folder'
            rel_path = full_path.relative_to(folder)
            all_files.add(rel_path)
    return all_files

def main(folder1, folder2, output_yaml):
    folder1 = Path(folder1).resolve()
    folder2 = Path(folder2).resolve()

    # Gather all files in each folder (relative paths)
    files_in_1 = gather_files(folder1)
    files_in_2 = gather_files(folder2)

    # Compute differences
    unique_to_folder1 = sorted(list(files_in_1 - files_in_2))
    unique_to_folder2 = sorted(list(files_in_2 - files_in_1))
    in_both = sorted(list(files_in_1 & files_in_2))

    # Build the output data
    comparison_result = {
        "folder1": str(folder1),
        "folder2": str(folder2),
        "unique_to_folder1": [str(folder1 / rel) for rel in unique_to_folder1],
        "unique_to_folder2": [str(folder2 / rel) for rel in unique_to_folder2],
        "common_files": {
            "count": len(in_both),
            "files": [ 
                {
                    "in_folder1": str(folder1 / rel),
                    "in_folder2": str(folder2 / rel)
                } 
                for rel in in_both
            ]
        }
    }

    # Write to YAML
    with open(output_yaml, "w", encoding="utf-8") as f:
        yaml.dump(comparison_result, f, sort_keys=False)

    print(f"Comparison complete. Results written to {output_yaml}")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python compare_folders.py <folder1> <folder2> <output_yaml_path>")
        sys.exit(1)

    folder1_arg = sys.argv[1]
    folder2_arg = sys.argv[2]
    output_yaml_arg = sys.argv[3]

    main(folder1_arg, folder2_arg, output_yaml_arg)
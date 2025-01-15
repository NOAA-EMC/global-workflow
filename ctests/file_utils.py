#!/usr/bin/env python3

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
from argparse import ArgumentParser
import shutil

try:
    import yaml
except ImportError:
    print("Please install pyyaml: pip install pyyaml")
    sys.exit(1)

def parse_args():
    parser = ArgumentParser()
    parser.add_argument("--cmp_folders", nargs=2, required=False, help="Paths to compare")
    parser.add_argument("--yaml", required=False, help="Path to output YAML file")
    parser.add_argument("--copy_files", help="Path to copy the common files to")
    return parser.parse_args()

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

def compair_folders(folder1, folder2, output_yaml):

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
        "unique_to_folder1": [str(rel) for rel in unique_to_folder1],
        "unique_to_folder2": [str(rel) for rel in unique_to_folder2],
        "common_files": {
            "count": len(in_both),
            "files": [ 
                {
                    "in_folder1": str(rel),
                    "in_folder2": str(rel)
                } 
                for rel in in_both
            ]
        }
    }

    # Write to YAML
    with open(output_yaml, "w", encoding="utf-8") as f:
        yaml.dump(comparison_result, f, sort_keys=False)

    print(f"Comparison complete. Results written to {output_yaml}")

def copy_common_files(common_files, folder1, dest_folder):
    dest_folder = Path(dest_folder)
    dest_folder.mkdir(parents=True, exist_ok=True)
    for rel_path in common_files:
        source_file = folder1 / rel_path
        target_file = dest_folder / rel_path
        target_file.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source_file, target_file)

def load_output_files(yaml_path):
    with open(yaml_path, "r") as f:
        data = yaml.safe_load(f)
    return data.get("output_files", [])

if __name__ == "__main__":
    args = parse_args()
    if args.cmp_folders:
        folder1_arg, folder2_arg = args.cmp_folders
        output_yaml = args.yaml
        compair_folders(folder1_arg, folder2_arg, output_yaml)
    if args.copy_files:
        input_yaml = args.yaml
        output_files = load_output_files(input_yaml)
        copy_common_files(output_files, Path.cwd(), args.copy_files)
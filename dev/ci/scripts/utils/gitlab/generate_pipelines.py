#!/usr/bin/env python3
"""
Script to generate GitLab CI pipeline configuration based on supported test cases
for each machine.

Overview
--------
This script automates the creation of a `.gitlab-ci.yml` file tailored for the
current repository and its supported test environments. Its main role is to
dynamically generate the GitLab CI pipeline configuration, ensuring that the
pipeline reflects the actual set of supported test cases for each target machine.

Process
-------
1. **Discovery of Supported Test Cases**:
   - The script imports and uses `get_host_case_list.py` (via `get_host_cases`) to
     determine which test cases are supported on each specified machine.
   - The list of machines is provided via command-line arguments.

2. **Template-Based Configuration**:
   - A template YAML file is provided (via `--template` argument) that contains
     the static, reusable parts of the pipeline configuration.
   - The script reads this template, extracting the relevant content below a
     demarcation marker.

3. **Dynamic Section Generation**:
   - For each machine, the script generates YAML configuration sections for
     build, setup, and test jobs, using the discovered test cases as a matrix.
   - These sections are appended to the template content.

4. **Output**:
   - The final, combined configuration is written to `.gitlab-ci.yml` in the
     repository's CI directory (or to a user-specified output path).
   - This output file is what GitLab CI will use to define and run the pipeline.

Role in Workflow
----------------
- This script is intended to be run whenever the set of supported test cases or
  machines changes, or when the pipeline template is updated.
- It ensures that the CI pipeline is always in sync with the actual capabilities
  of the codebase and test infrastructure, reducing manual maintenance and errors.
- The generated `.gitlab-ci.yml` is the authoritative pipeline definition for
  GitLab CI/CD in this repository.

Usage
-----
Run this script with the required arguments:
    python generate_pipelines.py --machines <machine1,machine2,...> --template <template_path> [--output <output_path>]

This will produce or update the `.gitlab-ci.yml` file for use by GitLab CI.

"""

import os
import sys
import argparse
from pathlib import Path

# update sys.path to include libs in the parent directory
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from get_host_case_list import get_host_cases
from find_homegfs import find_homegfs

# Get the path to the top directory of the repository
# to place the generated .gitlab-ci.yml file in the default location
_homegfs = find_homegfs(os.path.dirname(os.path.abspath(__file__)))


def get_case_list_for_machine(machine):
    """
    Get the list of supported cases for the given machine.

    Parameters
    ----------
    machine : str
        The name of the machine to get supported cases for.

    Returns
    -------
    list
        A list of test case names supported on the specified machine.
    """

    cases = get_host_cases(machine, homegfs=_homegfs)
    return cases


def generate_machine_config(machine, case_list):
    """
    Generate the machine-specific configuration sections for GitLab CI.

    Parameters
    ----------
    machine : str
        The name of the machine for which to generate configuration.
    case_list : list
        A list of test case names supported on the machine.

    Returns
    -------
    str
        YAML configuration for the machine's build, setup, and run_tests jobs.
    """

    case_str = '", "'.join(case_list)
    case_list_yaml = f'["{case_str}"]'

    machine_config = f'''
build-{machine}:
  extends: .build_template
  variables:
    mahine: {machine}
  tags: ["{machine}"]

setup_experiments-{machine}:
  extends: .setup_template
  variables:
    mahine: {machine}
  tags: ["{machine}"]
  parallel:
    matrix:
      - caseName: {case_list_yaml}
  dependencies:
    - build-{machine}

run_tests-{machine}:
  extends: .run_tests_template
  variables:
    mahine: {machine}
  tags: ["{machine}"]
  parallel:
    matrix:
      - caseName: {case_list_yaml}
  dependencies:
    - setup_experiments-{machine}
'''
    return machine_config


def read_template_file(template_path):
    """
    Read the template file and extract the content below the demarcation line.

    Parameters
    ----------
    template_path : str
       Path to the template file containing the base GitLab CI configuration.

    Returns
    -------
    str
        Content of the template file below the demarcation line.
    """
    with open(template_path, 'r') as f:
        lines = f.readlines()

    # Find the demarcation line that separates the header from the usable template
    marker_line = "# ------------------------------------------------------------"

    for i, line in enumerate(lines):
        if line.strip() == marker_line:
            return ''.join(lines[i + 1:])

    # If marker line not found, return an empty string
    return ''


def generate_pipeline_config(machines, template_file, output_file=None):
    """
    Generate the complete GitLab CI pipeline configuration.

    This function combines a template file with machine-specific configurations
    based on the supported test cases for each machine.

    Parameters
    ----------
    machines : list
        List of machine names to include in the pipeline configuration.
    template_file : str
        Path to the template file containing the base configuration.
        The template should end with a marker line: "# Machine-specific jobs generated from template:"
    output_file : str, optional
        Path where the generated configuration will be written.
        If not provided, defaults to ci/.gitlab-ci.yml in the repository root.

    Raises
    ------
    ValueError
        If the template file does not exist.
    """
    # Set default output file path if not specified
    output_file = output_file or os.path.join(_homegfs, 'dev/ci', '.gitlab-ci.yml')

    # Initialize with the template content
    if os.path.exists(template_file):
        config_content = read_template_file(template_file)
    else:
        raise ValueError(f"Template file {template_file} not found")

    # Add a comment to indicate the start of generated sections
    config_content += '\n# The following sections are generated for multiple hosts and dynamic case lists\n'

    # Generate machine-specific configurations
    for machine in machines:
        case_list = get_case_list_for_machine(machine)
        if not case_list:
            print(f"Warning: No supported cases found for machine {machine}", file=sys.stderr)
            continue

        machine_config = generate_machine_config(machine, case_list)
        config_content += machine_config

    # Add a comment to indicate the end of generated sections
    config_content += '\n# End of generated sections\n'

    # Ensure no blank line at the top of the resulting pipeline
    config_content = config_content.lstrip()

    # Write the complete configuration to the output file
    with open(output_file, 'w') as f:
        f.write(config_content)

    print(f"GitLab CI pipeline configuration generated at {output_file}")


def main():
    """
    Parse command line arguments and generate the GitLab CI pipeline configuration.

    This is the main entry point for the script when executed directly.
    It parses command line arguments and calls generate_pipeline_config()
    with the appropriate parameters.

    Command line arguments:
    --machines : str
        Comma-separated list of machines to include in the pipeline.
    --template : str
        Path to the template file containing the base configuration.
    --output : str, optional
        Path where the generated configuration will be written.

    Returns
    -------
    None
    """

    parser = argparse.ArgumentParser(description='Generate GitLab CI pipeline configuration.')
    parser.add_argument('--machines', required=True, help='Comma-separated list of machines to include in the pipeline')
    parser.add_argument('--template', required=True, help='Path to the template file for the pipeline configuration')
    parser.add_argument('--output', default=None, help='Path to the output file (default: ci/.gitlab-ci.yml)')

    args = parser.parse_args()

    machines = [machine.strip() for machine in args.machines.split(',')]
    generate_pipeline_config(machines, args.template, args.output)


if __name__ == '__main__':
    main()

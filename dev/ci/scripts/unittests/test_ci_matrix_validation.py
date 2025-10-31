"""
Unit tests for GitLab CI case matrix validation.

This module validates that the static case matrices defined in gitlab-ci-hosts.yml
remain consistent with the skip_ci_on_hosts tags specified in individual case YAML
files located in dev/ci/cases/pr/.

The validation ensures:
    - Each host matrix only includes cases that do not skip that host
    - Each host matrix includes all cases that should run on that host
    - No cases are incorrectly included or excluded from host matrices

Test scenarios:
    - test_matrices_are_valid: Validates current repository state
    - test_detect_incorrectly_included_case: Simulates case in matrix that should skip host
    - test_detect_missing_case: Simulates case missing from matrix that should be included
"""

import os
import re
import tempfile
from pathlib import Path
from typing import Dict, List, Set

import pytest
import yaml


class CIMatrixValidator:
    """
    Validator for GitLab CI case matrices.
    
    Compares static matrix definitions in gitlab-ci-hosts.yml against
    skip_ci_on_hosts tags in case YAML files to ensure consistency.
    
    Attributes:
        repo_root: Path to repository root directory
        gitlab_config_path: Path to gitlab-ci-hosts.yml
        cases_dir: Path to dev/ci/cases/pr directory
    """
    
    def __init__(self, repo_root: Path):
        """
        Initialize validator with repository paths.
        
        Args:
            repo_root: Path to the repository root directory
        """
        self.repo_root = Path(repo_root)
        self.gitlab_config_path = self.repo_root / 'dev' / 'ci' / 'gitlab-ci-hosts.yml'
        self.cases_dir = self.repo_root / 'dev' / 'ci' / 'cases' / 'pr'
        
        self._validate_paths()
    
    def _validate_paths(self):
        """Validate that required paths exist."""
        if not self.gitlab_config_path.exists():
            raise FileNotFoundError(f"GitLab config not found: {self.gitlab_config_path}")
        
        if not self.cases_dir.exists():
            raise FileNotFoundError(f"Cases directory not found: {self.cases_dir}")
    
    def extract_matrices_from_config(self) -> Dict[str, Set[str]]:
        """
        Extract case matrices for each host from gitlab-ci-hosts.yml.
        
        Parses YAML anchors that define matrices like:
            .hera_cases_matrix: &hera_cases
              - caseName: ["case1", "case2", ...]
        
        Returns:
            Dictionary mapping host names to sets of case names
        """
        with open(self.gitlab_config_path, 'r') as f:
            content = f.read()
        
        host_matrices = {}
        matrix_pattern = r'\.(\w+)_cases_matrix:\s*&\1_cases\s*\n\s*-\s*caseName:\s*\[(.*?)\]'
        
        for match in re.finditer(matrix_pattern, content, re.MULTILINE | re.DOTALL):
            host = match.group(1)
            cases_str = match.group(2)
            case_list = re.findall(r'"([^"]+)"', cases_str)
            host_matrices[host] = set(case_list)
        
        return host_matrices
    
    def get_all_case_files(self) -> List[Path]:
        """
        Get all YAML case files from the cases/pr directory.
        
        Returns:
            Sorted list of Path objects for each case YAML file
        """
        return sorted(self.cases_dir.glob('*.yaml'))
    
    def extract_skip_hosts(self, case_file: Path) -> Set[str]:
        """
        Extract the list of hosts to skip from a case YAML file.
        
        Handles Jinja2 templating in case files by using regex to extract
        only the skip_ci_on_hosts section, avoiding full YAML parsing errors.
        
        Args:
            case_file: Path to case YAML file
        
        Returns:
            Set of host names that should skip this case
        """
        with open(case_file, 'r') as f:
            content = f.read()
        
        skip_pattern = r'skip_ci_on_hosts:\s*\n((?:\s*-\s*\w+\s*\n)*)'
        match = re.search(skip_pattern, content)
        
        if match:
            skip_section = "skip_ci_on_hosts:\n" + match.group(1)
            try:
                parsed = yaml.safe_load(skip_section)
                skip_hosts = parsed.get('skip_ci_on_hosts', [])
                return set(skip_hosts) if skip_hosts else set()
            except yaml.YAMLError:
                return set()
        
        return set()
    
    def build_expected_matrices(self, known_hosts: Set[str]) -> Dict[str, Set[str]]:
        """
        Build expected matrices for each host based on skip_ci_on_hosts tags.
        
        For each case file, determines which hosts should run it by checking
        which hosts are NOT in the case's skip list.
        
        Args:
            known_hosts: Set of host names from gitlab-ci-hosts.yml
        
        Returns:
            Dictionary mapping host names to expected sets of case names
        """
        expected_matrices = {host: set() for host in known_hosts}
        
        for case_file in self.get_all_case_files():
            case_name = case_file.stem
            skip_hosts = self.extract_skip_hosts(case_file)
            
            for host in known_hosts:
                if host not in skip_hosts:
                    expected_matrices[host].add(case_name)
        
        return expected_matrices
    
    def validate(self) -> tuple[bool, Dict[str, List[str]]]:
        """
        Validate actual matrices against expected matrices.
        
        Compares the matrices defined in gitlab-ci-hosts.yml against what
        should exist based on skip_ci_on_hosts tags in case files.
        
        Returns:
            Tuple of (is_valid, errors_dict) where:
                - is_valid: True if all matrices are valid
                - errors_dict: Dictionary mapping hosts to lists of error messages
        """
        actual_matrices = self.extract_matrices_from_config()
        expected_matrices = self.build_expected_matrices(set(actual_matrices.keys()))
        
        errors = {}
        
        for host in sorted(actual_matrices.keys()):
            actual_cases = actual_matrices[host]
            expected_cases = expected_matrices.get(host, set())
            
            host_errors = []
            
            extra_cases = actual_cases - expected_cases
            if extra_cases:
                host_errors.append(
                    f"Matrix includes cases that should skip this host: {sorted(extra_cases)}"
                )
            
            missing_cases = expected_cases - actual_cases
            if missing_cases:
                host_errors.append(
                    f"Matrix missing cases that should run on this host: {sorted(missing_cases)}"
                )
            
            if host_errors:
                errors[host] = host_errors
        
        is_valid = len(errors) == 0
        return is_valid, errors


def get_repo_root() -> Path:
    """
    Find the repository root by looking for .github directory.
    
    Returns:
        Path to repository root
    """
    current = Path(__file__).resolve()
    
    for parent in [current] + list(current.parents):
        if (parent / '.github').exists():
            return parent
    
    raise FileNotFoundError("Could not find repository root (.github directory)")


@pytest.fixture
def validator():
    """Fixture providing a CIMatrixValidator instance."""
    repo_root = get_repo_root()
    return CIMatrixValidator(repo_root)


@pytest.fixture
def repo_root_path():
    """Fixture providing the repository root path."""
    return get_repo_root()


def test_matrices_are_valid(validator):
    """
    Test that current case matrices are valid.
    
    Validates that the matrices in gitlab-ci-hosts.yml are consistent
    with skip_ci_on_hosts tags in all case files.
    """
    is_valid, errors = validator.validate()
    
    if not is_valid:
        error_msg = "Matrix validation failed:\n"
        for host, host_errors in errors.items():
            error_msg += f"\nHost '{host}':\n"
            for error in host_errors:
                error_msg += f"  - {error}\n"
        pytest.fail(error_msg)


def test_detect_incorrectly_included_case(validator, repo_root_path, tmp_path):
    """
    Test that validator detects cases incorrectly included in host matrix.
    
    Simulates adding a skip tag to a case that is currently in a host's matrix,
    then verifies the validator catches this inconsistency.
    """
    test_case_file = repo_root_path / 'dev' / 'ci' / 'cases' / 'pr' / 'C48_ATM.yaml'
    
    with open(test_case_file, 'r') as f:
        original_content = f.read()
    
    modified_content = original_content.replace(
        'yaml: {{ HOMEgfs }}/dev/ci/cases/yamls/gfs_defaults_ci.yaml\n\nworkflow:',
        'yaml: {{ HOMEgfs }}/dev/ci/cases/yamls/gfs_defaults_ci.yaml\n\n'
        'skip_ci_on_hosts:\n  - hera\n\nworkflow:'
    )
    
    try:
        with open(test_case_file, 'w') as f:
            f.write(modified_content)
        
        is_valid, errors = validator.validate()
        
        assert not is_valid, "Validator should detect incorrectly included case"
        assert 'hera' in errors, "Validator should report error for hera"
        assert any('C48_ATM' in str(error) for error in errors['hera']), \
            "Error should mention C48_ATM case"
    
    finally:
        with open(test_case_file, 'w') as f:
            f.write(original_content)


def test_detect_missing_case(validator, repo_root_path):
    """
    Test that validator detects cases missing from host matrix.
    
    Simulates removing a case from gitlab-ci-hosts.yml matrix while
    the case file does not skip that host, then verifies the validator
    catches this inconsistency.
    """
    gitlab_config = validator.gitlab_config_path
    
    with open(gitlab_config, 'r') as f:
        original_content = f.read()
    
    modified_content = re.sub(
        r'(\.hera_cases_matrix:.*?caseName: \[)(.*?)(\"C48_ATM\".*?\])',
        r'\1\3',
        original_content,
        flags=re.DOTALL
    )
    modified_content = modified_content.replace('"C48_ATM", ', '')
    modified_content = modified_content.replace(', "C48_ATM"', '')
    
    try:
        with open(gitlab_config, 'w') as f:
            f.write(modified_content)
        
        validator_modified = CIMatrixValidator(repo_root_path)
        is_valid, errors = validator_modified.validate()
        
        assert not is_valid, "Validator should detect missing case"
        assert 'hera' in errors, "Validator should report error for hera"
        assert any('C48_ATM' in str(error) for error in errors['hera']), \
            "Error should mention C48_ATM case"
    
    finally:
        with open(gitlab_config, 'w') as f:
            f.write(original_content)


def test_validator_initialization_missing_paths(tmp_path):
    """
    Test that validator raises appropriate errors for missing paths.
    
    Verifies that CIMatrixValidator initialization fails gracefully
    when required directories or files do not exist.
    """
    with pytest.raises(FileNotFoundError):
        CIMatrixValidator(tmp_path)


def test_get_all_case_files(validator):
    """
    Test that case file discovery returns expected files.
    
    Verifies that the validator can find case YAML files in the
    expected directory and returns them in sorted order.
    """
    case_files = validator.get_all_case_files()
    
    assert len(case_files) > 0, "Should find at least one case file"
    assert all(f.suffix == '.yaml' for f in case_files), "All files should be YAML"
    assert case_files == sorted(case_files), "Files should be sorted"


def test_extract_skip_hosts_no_skip_section(validator, tmp_path):
    """
    Test skip host extraction for cases without skip_ci_on_hosts section.
    
    Verifies that extract_skip_hosts returns an empty set when a case
    file does not contain a skip_ci_on_hosts section.
    """
    test_case = tmp_path / "test_case.yaml"
    test_case.write_text("""
experiment:
  app: ATM
workflow:
  engine: rocoto
""")
    
    skip_hosts = validator.extract_skip_hosts(test_case)
    assert skip_hosts == set(), "Should return empty set for cases without skip section"


def test_extract_skip_hosts_with_skips(validator, tmp_path):
    """
    Test skip host extraction for cases with skip_ci_on_hosts section.
    
    Verifies that extract_skip_hosts correctly parses and returns
    the set of hosts listed in skip_ci_on_hosts.
    """
    test_case = tmp_path / "test_case.yaml"
    test_case.write_text("""
experiment:
  app: ATM

skip_ci_on_hosts:
  - hera
  - orion

workflow:
  engine: rocoto
""")
    
    skip_hosts = validator.extract_skip_hosts(test_case)
    assert skip_hosts == {'hera', 'orion'}, "Should return correct set of skip hosts"

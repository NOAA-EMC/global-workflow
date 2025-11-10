"""
Unit tests for GitLab CI case matrix validation.

Validates that static case matrices in gitlab-ci-hosts.yml remain consistent
with skip_ci_on_hosts tags in case YAML files (dev/ci/cases/pr/).

Ensures:
    - Host matrices only include cases that don't skip that host
    - Host matrices include all cases that should run on that host
    - No cases are incorrectly included or excluded from matrices
"""

import re
from pathlib import Path
from typing import Dict, List, Set

import pytest
import yaml


class CIMatrixValidator:
    """
    Validates GitLab CI case matrices against skip_ci_on_hosts tags.

    Compares static matrix definitions in gitlab-ci-hosts.yml against
    skip_ci_on_hosts tags in case files to ensure consistency.
    """

    def __init__(self, repo_root: Path):
        """Initialize validator with repository paths."""
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
        Extract host case matrices from gitlab-ci-hosts.yml.

        Parses YAML anchors like:
            .hera_cases_matrix: &hera_cases
              - caseName: ["case1", "case2", ...]

        Returns:
            Dictionary mapping host names to sets of case names
        """
        with open(self.gitlab_config_path, 'r') as f:
            content = f.read()

        host_matrices = {}
        pattern = r'\.(\w+)_cases_matrix:\s*&\1_cases\s*\n\s*-\s*caseName:\s*\[(.*?)\]'

        for match in re.finditer(pattern, content, re.MULTILINE | re.DOTALL):
            host = match.group(1)
            cases_str = match.group(2)
            host_matrices[host] = set(re.findall(r'"([^"]+)"', cases_str))

        return host_matrices

    def get_all_case_files(self) -> List[Path]:
        """Get all YAML case files from cases/pr directory."""
        return sorted(self.cases_dir.glob('*.yaml'))

    def extract_skip_hosts(self, case_file: Path) -> Set[str]:
        """
        Extract skip_ci_on_hosts list from a case YAML file.

        Uses regex to extract only the skip section, avoiding Jinja2
        templating issues with full YAML parsing.

        Returns:
            Set of host names that should skip this case
        """
        with open(case_file, 'r') as f:
            content = f.read()

        match = re.search(r'skip_ci_on_hosts:\s*\n((?:\s*-\s*\w+\s*\n)*)', content)
        if match:
            try:
                parsed = yaml.safe_load("skip_ci_on_hosts:\n" + match.group(1))
                skip_hosts = parsed.get('skip_ci_on_hosts', [])
                return set(skip_hosts) if skip_hosts else set()
            except yaml.YAMLError:
                pass
        return set()

    def build_expected_matrices(self, known_hosts: Set[str]) -> Dict[str, Set[str]]:
        """
        Build expected matrices based on skip_ci_on_hosts tags.

        For each case file, determines which hosts should run it by
        checking which hosts are NOT in the case's skip list.

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

        Compares matrices in gitlab-ci-hosts.yml against what should exist
        based on skip_ci_on_hosts tags in case files.

        Returns:
            Tuple of (is_valid, errors) where errors maps hosts to error messages
        """
        actual_matrices = self.extract_matrices_from_config()
        expected_matrices = self.build_expected_matrices(set(actual_matrices.keys()))
        errors = {}

        for host in sorted(actual_matrices.keys()):
            host_errors = []
            extra_cases = actual_matrices[host] - expected_matrices.get(host, set())
            missing_cases = expected_matrices.get(host, set()) - actual_matrices[host]

            if extra_cases:
                host_errors.append(f"Matrix includes cases that should skip this host: {sorted(extra_cases)}")
            if missing_cases:
                host_errors.append(f"Matrix missing cases that should run on this host: {sorted(missing_cases)}")

            if host_errors:
                errors[host] = host_errors

        return len(errors) == 0, errors


def get_repo_root() -> Path:
    """Find repository root by looking for .github directory."""
    for parent in [Path(__file__).resolve()] + list(Path(__file__).resolve().parents):
        if (parent / '.github').exists():
            return parent
    raise FileNotFoundError("Could not find repository root (.github directory)")


@pytest.fixture
def validator():
    """Fixture providing a CIMatrixValidator instance."""
    return CIMatrixValidator(get_repo_root())


@pytest.fixture
def repo_root_path():
    """Fixture providing the repository root path."""
    return get_repo_root()


def test_matrices_are_valid(validator):
    """
    Test that current case matrices are valid.

    Validates that matrices in gitlab-ci-hosts.yml are consistent
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


def test_detect_incorrectly_included_case(validator, repo_root_path):
    """
    Test validator detects cases incorrectly included in host matrix.

    Simulates adding a skip tag to a case that's currently in a host's
    matrix, then verifies the validator catches the inconsistency.
    """
    actual_matrices = validator.extract_matrices_from_config()

    if not actual_matrices:
        pytest.skip("No host matrices found")

    test_host = sorted(actual_matrices.keys())[0]
    host_cases = actual_matrices[test_host]

    if not host_cases:
        pytest.skip(f"No cases in matrix for {test_host}")

    test_case_name = sorted(host_cases)[0]
    test_case_file = repo_root_path / 'dev' / 'ci' / 'cases' / 'pr' / f'{test_case_name}.yaml'

    if not test_case_file.exists():
        pytest.skip(f"Case file not found: {test_case_file}")

    original_content = test_case_file.read_text()

    # Add skip tag for test host
    if 'skip_ci_on_hosts:' in original_content:
        modified_content = original_content.replace('skip_ci_on_hosts:', f'skip_ci_on_hosts:\n  - {test_host}')
    else:
        modified_content = original_content.replace('workflow:', f'skip_ci_on_hosts:\n  - {test_host}\n\nworkflow:')

    try:
        test_case_file.write_text(modified_content)
        is_valid, errors = validator.validate()

        assert not is_valid, "Validator should detect incorrectly included case"
        assert test_host in errors, f"Validator should report error for {test_host}"
        assert any(test_case_name in str(error) for error in errors[test_host]), \
            f"Error should mention {test_case_name} case"
    finally:
        test_case_file.write_text(original_content)


def test_detect_missing_case(validator, repo_root_path):
    """
    Test validator detects cases missing from host matrix.

    Simulates removing a case from a host's matrix while the case file
    doesn't skip that host, verifying the validator catches it.
    """
    gitlab_config = validator.gitlab_config_path
    original_content = gitlab_config.read_text()

    # Remove C48_ATM from hera's matrix
    modified_content = re.sub(
        r'(\.hera_cases_matrix:.*?caseName: \[)(.*?)(\"C48_ATM\".*?\])',
        r'\1\3', original_content, flags=re.DOTALL)
    modified_content = modified_content.replace('"C48_ATM", ', '').replace(', "C48_ATM"', '')

    try:
        gitlab_config.write_text(modified_content)
        validator_modified = CIMatrixValidator(repo_root_path)
        is_valid, errors = validator_modified.validate()

        assert not is_valid, "Validator should detect missing case"
        assert 'hera' in errors, "Validator should report error for hera"
        assert any('C48_ATM' in str(error) for error in errors['hera']), \
            "Error should mention C48_ATM case"
    finally:
        gitlab_config.write_text(original_content)


def test_validator_initialization_missing_paths(tmp_path):
    """Test validator raises error for missing paths."""
    with pytest.raises(FileNotFoundError):
        CIMatrixValidator(tmp_path)


def test_get_all_case_files(validator):
    """Test case file discovery returns sorted YAML files."""
    case_files = validator.get_all_case_files()
    assert len(case_files) > 0, "Should find at least one case file"
    assert all(f.suffix == '.yaml' for f in case_files), "All files should be YAML"
    assert case_files == sorted(case_files), "Files should be sorted"


def test_extract_skip_hosts_no_skip_section(validator, tmp_path):
    """Test skip host extraction returns empty set without skip_ci_on_hosts section."""
    test_case = tmp_path / "test_case.yaml"
    test_case.write_text("experiment:\n  app: ATM\nworkflow:\n  engine: rocoto\n")
    assert validator.extract_skip_hosts(test_case) == set()


def test_extract_skip_hosts_with_skips(validator, tmp_path):
    """Test skip host extraction correctly parses skip_ci_on_hosts section."""
    test_case = tmp_path / "test_case.yaml"
    test_case.write_text("experiment:\n  app: ATM\n\nskip_ci_on_hosts:\n  - hera\n  - orion\n\nworkflow:\n  engine: rocoto\n")
    assert validator.extract_skip_hosts(test_case) == {'hera', 'orion'}

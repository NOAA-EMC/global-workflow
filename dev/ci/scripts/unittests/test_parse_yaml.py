#!/usr/bin/env python3

import unittest
import os
import sys
import subprocess
from pathlib import Path

# Add parent directory to sys.path to import utils modules
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
UTILS_DIR = os.path.abspath(os.path.join(SCRIPT_DIR, '..', 'utils'))
sys.path.insert(0, UTILS_DIR)

# Path to the test data directory
TEST_DATA_DIR = os.path.join(SCRIPT_DIR, 'test_data')
TEST_CONFIG = os.path.join(TEST_DATA_DIR, 'test_config.yaml')
SCRIPT_PATH = os.path.join(SCRIPT_DIR, '..', 'utils', 'parse_yaml.py')


class TestParseYAML(unittest.TestCase):
    """Tests for the parse_yaml.py script"""

    @classmethod
    def setUpClass(cls):
        # Ensure test_data directory exists
        os.makedirs(TEST_DATA_DIR, exist_ok=True)
        # Create test yaml file if it doesn't exist
        if not os.path.exists(TEST_CONFIG):
            with open(TEST_CONFIG, 'w') as f:
                f.write('''# Test configuration file for parse_yaml.py unit tests
top_level: simple_value
nested:
  key1: value1
  key2: value2
  deeper:
    key3: value3
numbers:
  integer: 42
  float: 3.14
list_data:
  - item1
  - item2
  - item3
complex:
  nested_list:
    - name: first
      value: 1
    - name: second
      value: 2
template_value: "/path/to/homegfs/some/path"''')

    def test_cli_basic(self):
        """Test the command-line interface with basic options"""
        # Test retrieving a simple value
        cmd = [sys.executable, SCRIPT_PATH, '-y', TEST_CONFIG, '-k', 'top_level']
        result = subprocess.run(cmd, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, f"Command failed: {result.stderr}")
        self.assertEqual(result.stdout.strip(), 'simple_value')

        # Test retrieving a nested value
        cmd = [sys.executable, SCRIPT_PATH, '-y', TEST_CONFIG, '-k', 'nested.key2']
        result = subprocess.run(cmd, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, f"Command failed: {result.stderr}")
        self.assertEqual(result.stdout.strip(), 'value2')

    def test_cli_default_value(self):
        """Test the --default option"""
        # Test default value for non-existent key
        cmd = [sys.executable, SCRIPT_PATH, '-y', TEST_CONFIG, '-k', 'missing.key',
               '-d', 'default_value']
        result = subprocess.run(cmd, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, f"Command failed: {result.stderr}")
        self.assertEqual(result.stdout.strip(), 'default_value')

    def test_cli_fail_on_missing(self):
        """Test the --fail-on-missing option"""
        # Test that non-existent key with --fail-on-missing fails
        cmd = [sys.executable, SCRIPT_PATH, '-y', TEST_CONFIG, '-k', 'missing.key',
               '--fail-on-missing']
        result = subprocess.run(cmd, capture_output=True, text=True)
        self.assertEqual(result.returncode, 1, "Expected command to fail with code 1")
        self.assertIn("not found", result.stderr)

    # JSON formatting test has been removed

    def test_cli_string_option(self):
        """Test the --string option for list output"""
        # Test string output for a list
        cmd = [sys.executable, SCRIPT_PATH, '-y', TEST_CONFIG, '-k', 'list_data',
               '--string']
        result = subprocess.run(cmd, capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, f"Command failed: {result.stderr}")
        # Each list item should be on a separate line
        lines = result.stdout.strip().split('\n')
        self.assertEqual(lines, ['item1', 'item2', 'item3'])


if __name__ == '__main__':
    print("Starting TestParseYAML tests...")
    unittest.main(verbosity=2)

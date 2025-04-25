#!/usr/bin/env python3

import unittest
import os
import sys
import tempfile
import shutil
from pathlib import Path
from find_homegfs import find_homegfs

# Add parent directory to sys.path to import find_homegfs module
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
UTILS_DIR = os.path.abspath(os.path.join(SCRIPT_DIR, '..', 'utils'))
sys.path.insert(0, UTILS_DIR)


class TestFindHOMEgfs(unittest.TestCase):
    """Tests for the find_homegfs.py script"""

    def setUp(self):
        # Create a temporary directory structure for testing
        self.test_dir = tempfile.mkdtemp()
        # Create a fake repo structure with .github directory
        self.fake_repo_path = os.path.join(self.test_dir, "fake_repo")
        os.makedirs(os.path.join(self.fake_repo_path, ".github"))
        # Create a nested directory structure for testing
        self.nested_dir = os.path.join(self.fake_repo_path, "dir1", "dir2", "dir3")
        os.makedirs(self.nested_dir)

    def tearDown(self):
        # Clean up the temporary directory
        shutil.rmtree(self.test_dir)

    def test_find_homegfs_current_dir(self):
        """Test find_homegfs when starting from the repo root"""
        result = find_homegfs(self.fake_repo_path)
        self.assertEqual(str(result), str(Path(self.fake_repo_path)))

    def test_find_homegfs_nested_dir(self):
        """Test find_homegfs when starting from a nested directory"""
        result = find_homegfs(self.nested_dir)
        self.assertEqual(str(result), str(Path(self.fake_repo_path)))

    def test_find_homegfs_none_start_path(self):
        """Test find_homegfs with None start_path (should use cwd)"""
        # Save the current directory
        original_dir = os.getcwd()
        try:
            # Change to the fake repo directory
            os.chdir(self.fake_repo_path)
            result = find_homegfs(None)
            self.assertEqual(str(result), str(Path(self.fake_repo_path)))
        finally:
            # Restore the original directory
            os.chdir(original_dir)

    def test_find_homegfs_not_found(self):
        """Test find_homegfs when .github directory doesn't exist"""
        # Create a directory outside the fake repo
        outside_dir = os.path.join(self.test_dir, "outside")
        os.makedirs(outside_dir)

        # Patch os.path.dirname to ensure we don't traverse beyond our test directory
        real_dirname = os.path.dirname

        def mock_dirname(path):
            result = real_dirname(path)
            # If we're about to go above our test directory, return the same path
            # to simulate reaching the filesystem root
            if result == self.test_dir or os.path.dirname(result) == self.test_dir:
                return path
            return result

        original_dirname = os.path.dirname
        os.path.dirname = mock_dirname
        try:
            with self.assertRaises(ValueError):
                find_homegfs(outside_dir)
        finally:
            # Restore original function
            os.path.dirname = original_dirname

    def test_with_string_path(self):
        """Test find_homegfs with string path"""
        result = find_homegfs(str(self.fake_repo_path))
        self.assertEqual(str(result), str(Path(self.fake_repo_path)))

    def test_with_path_object(self):
        """Test find_homegfs with Path object"""
        result = find_homegfs(Path(self.fake_repo_path))
        self.assertEqual(str(result), str(Path(self.fake_repo_path)))


if __name__ == '__main__':
    print("Starting TestFindHOMEgfs tests...")
    unittest.main(verbosity=2)

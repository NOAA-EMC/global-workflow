#!/usr/bin/env python3
"""
Rocoto Testing Driver - Comprehensive Test Framework for rocotostat.py Development

This driver creates various test scenarios and compares the output of your
rocotostat.py implementation against the official rocotostat command.
"""

import os
import sys
import time
import subprocess
import json
import sqlite3
from pathlib import Path
from datetime import datetime
import argparse
import logging

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('rocoto_testing.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class RocotoTestDriver:
    """Main testing driver for rocotostat.py development"""
    
    def __init__(self, test_dir="/home/tmcguinness/GITHUB/COPILOT/rocoto_testing"):
        self.test_dir = Path(test_dir)
        self.rocoto_bin = "/home/tmcguinness/GITHUB/COPILOT/rocoto/bin"
        self.base_workflow_dir = "/home/tmcguinness/GITHUB/COPILOT/test_workflow"
        self.base_xml = "/home/tmcguinness/GITHUB/COPILOT/test_workflow_simple.xml"
        self.base_db = "/home/tmcguinness/GITHUB/COPILOT/test_workflow_simple.db"
        
        # Environment setup
        self.env = os.environ.copy()
        self.env['PATH'] = f"{self.rocoto_bin}:{self.env['PATH']}"
        
        # Test scenarios
        self.test_scenarios = []
        
    def create_test_scenario(self, name, xml_file, db_file, description=""):
        """Create a test scenario configuration"""
        scenario = {
            'name': name,
            'xml_file': xml_file,
            'db_file': db_file,
            'description': description,
            'timestamp': datetime.now().isoformat()
        }
        self.test_scenarios.append(scenario)
        return scenario
        
    def run_official_rocotostat(self, xml_file, db_file, args=None):
        """Run the official rocotostat command"""
        cmd = [f"{self.rocoto_bin}/rocotostat", "-w", str(xml_file), "-d", str(db_file)]
        if args:
            cmd.extend(args)
            
        try:
            result = subprocess.run(
                cmd, 
                capture_output=True, 
                text=True, 
                env=self.env,
                timeout=60
            )
            return {
                'success': result.returncode == 0,
                'stdout': result.stdout,
                'stderr': result.stderr,
                'returncode': result.returncode,
                'execution_time': time.time()
            }
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'stdout': '',
                'stderr': 'Command timed out after 60 seconds',
                'returncode': -1,
                'execution_time': time.time()
            }
        except Exception as e:
            return {
                'success': False,
                'stdout': '',
                'stderr': str(e),
                'returncode': -1,
                'execution_time': time.time()
            }
    
    def run_custom_rocotostat(self, rocotostat_py_path, xml_file, db_file, args=None):
        """Run the custom rocotostat.py implementation"""
        cmd = ["python3", str(rocotostat_py_path), "-w", str(xml_file), "-d", str(db_file)]
        if args:
            cmd.extend(args)
            
        try:
            result = subprocess.run(
                cmd, 
                capture_output=True, 
                text=True,
                timeout=60
            )
            return {
                'success': result.returncode == 0,
                'stdout': result.stdout,
                'stderr': result.stderr,
                'returncode': result.returncode,
                'execution_time': time.time()
            }
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'stdout': '',
                'stderr': 'Command timed out after 60 seconds',
                'returncode': -1,
                'execution_time': time.time()
            }
        except Exception as e:
            return {
                'success': False,
                'stdout': '',
                'stderr': str(e),
                'returncode': -1,
                'execution_time': time.time()
            }
    
    def compare_outputs(self, official_result, custom_result):
        """Compare outputs between official and custom implementations"""
        comparison = {
            'outputs_match': official_result['stdout'] == custom_result['stdout'],
            'return_codes_match': official_result['returncode'] == custom_result['returncode'],
            'both_successful': official_result['success'] and custom_result['success'],
            'official_output': official_result['stdout'],
            'custom_output': custom_result['stdout'],
            'official_stderr': official_result['stderr'],
            'custom_stderr': custom_result['stderr'],
            'differences': []
        }
        
        # Find differences in output
        if not comparison['outputs_match']:
            official_lines = official_result['stdout'].split('\n')
            custom_lines = custom_result['stdout'].split('\n')
            
            for i, (off_line, cust_line) in enumerate(zip(official_lines, custom_lines)):
                if off_line != cust_line:
                    comparison['differences'].append({
                        'line': i + 1,
                        'official': off_line,
                        'custom': cust_line
                    })
        
        return comparison
    
    def run_test_scenario(self, scenario, rocotostat_py_path, test_args=None):
        """Run a single test scenario"""
        logger.info(f"Running test scenario: {scenario['name']}")
        logger.info(f"Description: {scenario['description']}")
        
        # Run official rocotostat
        official_result = self.run_official_rocotostat(
            scenario['xml_file'], 
            scenario['db_file'], 
            test_args
        )
        
        # Run custom rocotostat.py
        custom_result = self.run_custom_rocotostat(
            rocotostat_py_path,
            scenario['xml_file'], 
            scenario['db_file'], 
            test_args
        )
        
        # Compare results
        comparison = self.compare_outputs(official_result, custom_result)
        
        # Prepare test result (convert Path objects to strings for JSON serialization)
        test_result = {
            'scenario': {
                'name': scenario['name'],
                'xml_file': str(scenario['xml_file']),
                'db_file': str(scenario['db_file']),
                'description': scenario['description'],
                'timestamp': scenario['timestamp']
            },
            'test_args': test_args,
            'official_result': official_result,
            'custom_result': custom_result,
            'comparison': comparison,
            'timestamp': datetime.now().isoformat()
        }
        
        # Log results
        if comparison['both_successful'] and comparison['outputs_match']:
            logger.info("✅ Test PASSED - Outputs match perfectly")
        elif comparison['both_successful'] and not comparison['outputs_match']:
            logger.warning("⚠️  Test PARTIAL - Both successful but outputs differ")
            for diff in comparison['differences']:
                logger.warning(f"  Line {diff['line']}: Official: '{diff['official']}' | Custom: '{diff['custom']}'")
        else:
            logger.error("❌ Test FAILED - Execution issues")
            if not official_result['success']:
                logger.error(f"  Official failed: {official_result['stderr']}")
            if not custom_result['success']:
                logger.error(f"  Custom failed: {custom_result['stderr']}")
        
        return test_result
    
    def create_failing_workflow_scenario(self):
        """Create a workflow scenario with failing tasks"""
        logger.info("Creating failing workflow scenario...")
        
        # Create a copy of the base workflow with a failing task
        failing_xml = self.test_dir / "failing_workflow.xml"
        failing_db = self.test_dir / "failing_workflow.db"
        
        # Read the base XML and modify it to create a failing task
        with open(self.base_xml, 'r') as f:
            xml_content = f.read()
        
        # Modify the stage_test script to fail
        xml_content = xml_content.replace(
            "&JOBS_DIR;/stage_test.sh",
            "&JOBS_DIR;/stage_test_fail.sh"
        )
        
        # Write the modified XML
        with open(failing_xml, 'w') as f:
            f.write(xml_content)
        
        # Create the failing script
        failing_script = Path(self.base_workflow_dir) / "scripts" / "stage_test_fail.sh"
        with open(failing_script, 'w') as f:
            f.write("""#!/bin/bash
echo "=== Failing Stage Test Task ==="
echo "This task is designed to fail for testing purposes"
exit 1
""")
        
        # Make it executable
        failing_script.chmod(0o755)
        
        # Initialize the workflow
        self.run_rocotorun(failing_xml, failing_db)
        
        return self.create_test_scenario(
            "failing_workflow",
            failing_xml,
            failing_db,
            "Workflow with intentionally failing stage_test task"
        )
    
    def create_multi_cycle_scenario(self):
        """Create a scenario with multiple cycles at different stages"""
        logger.info("Creating multi-cycle scenario...")
        
        multi_xml = self.test_dir / "multi_cycle_workflow.xml"
        multi_db = self.test_dir / "multi_cycle_workflow.db"
        
        # Read the base XML and modify it for more frequent cycles
        with open(self.base_xml, 'r') as f:
            xml_content = f.read()
        
        # Modify to have cycles every 30 minutes for 4 hours
        xml_content = xml_content.replace(
            '<cycledef>202507181200 202507181800 06:00:00</cycledef>',
            '<cycledef>202507181200 202507181800 00:30:00</cycledef>'
        )
        
        with open(multi_xml, 'w') as f:
            f.write(xml_content)
        
        # Initialize the workflow
        self.run_rocotorun(multi_xml, multi_db)
        
        return self.create_test_scenario(
            "multi_cycle",
            multi_xml,
            multi_db,
            "Workflow with multiple cycles (every 30 minutes)"
        )
    
    def run_rocotorun(self, xml_file, db_file):
        """Run rocotorun to initialize/update workflow"""
        cmd = [f"{self.rocoto_bin}/rocotorun", "-w", str(xml_file), "-d", str(db_file)]
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, env=self.env, timeout=30)
            return result.returncode == 0
        except:
            return False
    
    def clean_test_databases(self):
        """Clean up all test databases for fresh start"""
        logger.info("Cleaning up test databases for fresh start...")
        
        # List of databases to clean
        databases_to_clean = [
            self.base_db,
            self.test_dir / "failing_workflow.db",
            self.test_dir / "multi_cycle_workflow.db"
        ]
        
        for db_path in databases_to_clean:
            if Path(db_path).exists():
                try:
                    Path(db_path).unlink()
                    logger.info(f"Removed database: {db_path}")
                except Exception as e:
                    logger.warning(f"Could not remove {db_path}: {e}")
        
        # Also clean up lock files
        for db_path in databases_to_clean:
            lock_file = Path(str(db_path).replace('.db', '_lock.db'))
            if lock_file.exists():
                try:
                    lock_file.unlink()
                    logger.info(f"Removed lock file: {lock_file}")
                except Exception as e:
                    logger.warning(f"Could not remove {lock_file}: {e}")
    
    def setup_test_scenarios(self, fresh_start=False):
        """Setup all test scenarios"""
        if fresh_start:
            self.clean_test_databases()
        
        scenarios = []
        
        # Base scenario (current working workflow)
        scenarios.append(self.create_test_scenario(
            "base_workflow",
            self.base_xml,
            self.base_db,
            "Base working workflow with all tasks succeeding"
        ))
        
        # Failing workflow scenario
        scenarios.append(self.create_failing_workflow_scenario())
        
        # Multi-cycle scenario
        scenarios.append(self.create_multi_cycle_scenario())
        
        return scenarios
    
    def run_comprehensive_test(self, rocotostat_py_path, fresh_start=False):
        """Run comprehensive test suite"""
        logger.info("Starting comprehensive rocotostat.py test suite")
        
        # Setup test scenarios
        scenarios = self.setup_test_scenarios(fresh_start=fresh_start)
        
        # Test different argument combinations
        test_args_list = [
            None,  # Default output
            ["-v"],  # Verbose
            ["-t", "stage_test"],  # Specific task (correct format)
            ["-c", "202507181200"],  # Specific cycle
            ["-T"],  # Task sort
            ["-s"],  # Summary
        ]
        
        all_results = []
        
        for scenario in scenarios:
            for test_args in test_args_list:
                test_name = f"{scenario['name']}_{'_'.join(test_args) if test_args else 'default'}"
                logger.info(f"\n{'='*60}")
                logger.info(f"Running test: {test_name}")
                logger.info(f"{'='*60}")
                
                result = self.run_test_scenario(scenario, rocotostat_py_path, test_args)
                result['test_name'] = test_name
                all_results.append(result)
        
        # Generate summary report
        self.generate_test_report(all_results)
        
        return all_results
    
    def generate_test_report(self, results):
        """Generate a comprehensive test report"""
        report_file = self.test_dir / "test_report.json"
        summary_file = self.test_dir / "test_summary.md"
        
        # Save detailed JSON report
        with open(report_file, 'w') as f:
            json.dump(results, f, indent=2)
        
        # Generate markdown summary
        total_tests = len(results)
        passed_tests = sum(1 for r in results if r['comparison']['both_successful'] and r['comparison']['outputs_match'])
        partial_tests = sum(1 for r in results if r['comparison']['both_successful'] and not r['comparison']['outputs_match'])
        failed_tests = sum(1 for r in results if not r['comparison']['both_successful'])
        
        summary = f"""# Rocotostat.py Test Report

## Summary
- **Total Tests**: {total_tests}
- **Passed**: {passed_tests} ✅
- **Partial**: {partial_tests} ⚠️
- **Failed**: {failed_tests} ❌
- **Success Rate**: {(passed_tests/total_tests)*100:.1f}%

## Test Results

"""
        
        for result in results:
            status = "✅ PASSED" if result['comparison']['both_successful'] and result['comparison']['outputs_match'] else \
                    "⚠️ PARTIAL" if result['comparison']['both_successful'] else "❌ FAILED"
            
            summary += f"### {result['test_name']}\n"
            summary += f"**Status**: {status}\n"
            summary += f"**Scenario**: {result['scenario']['description']}\n"
            summary += f"**Args**: {result['test_args']}\n"
            
            if result['comparison']['differences']:
                summary += "**Differences**:\n"
                for diff in result['comparison']['differences']:
                    summary += f"- Line {diff['line']}: Official: `{diff['official']}` | Custom: `{diff['custom']}`\n"
            
            summary += "\n"
        
        with open(summary_file, 'w') as f:
            f.write(summary)
        
        logger.info(f"Test report saved to {report_file}")
        logger.info(f"Test summary saved to {summary_file}")
        logger.info(f"Total: {total_tests}, Passed: {passed_tests}, Partial: {partial_tests}, Failed: {failed_tests}")


def main():
    parser = argparse.ArgumentParser(description='Rocoto Testing Driver')
    parser.add_argument('rocotostat_py', help='Path to your rocotostat.py implementation')
    parser.add_argument('--test-dir', default='/home/tmcguinness/GITHUB/COPILOT/rocoto_testing', 
                       help='Directory for test files')
    parser.add_argument('--scenario', help='Run specific test scenario only')
    parser.add_argument('--args', help='Additional arguments to pass to rocotostat')
    parser.add_argument('--fresh', action='store_true', help='Start with fresh databases')
    
    args = parser.parse_args()
    
    # Validate rocotostat.py exists
    if not Path(args.rocotostat_py).exists():
        logger.error(f"rocotostat.py not found at: {args.rocotostat_py}")
        sys.exit(1)
    
    # Initialize test driver
    driver = RocotoTestDriver(args.test_dir)
    
    # Run comprehensive test
    results = driver.run_comprehensive_test(args.rocotostat_py, fresh_start=args.fresh)
    
    # Print final summary
    total = len(results)
    passed = sum(1 for r in results if r['comparison']['both_successful'] and r['comparison']['outputs_match'])
    
    if passed == total:
        logger.info("🎉 All tests passed! Your rocotostat.py implementation is working perfectly!")
    else:
        logger.info(f"📊 Test Results: {passed}/{total} tests passed. See test_report.json for details.")


if __name__ == "__main__":
    main()

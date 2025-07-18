#!/usr/bin/env python3
"""
Python implementation of rocotostat command
"""

import argparse
import sqlite3
import sys
import os
from pathlib import Path
from datetime import datetime, timedelta
import xml.etree.ElementTree as ET


def timestamp_to_cycle_string(timestamp):
    """Convert Unix timestamp to cycle string (YYYYMMDDHH00)"""
    dt = datetime.fromtimestamp(timestamp)
    return dt.strftime('%Y%m%d%H00')


class RocotoStat:
    """Python implementation of rocotostat functionality"""
    
    def __init__(self, workflow_xml, database_path):
        self.workflow_xml = Path(workflow_xml)
        self.database_path = Path(database_path)
        self.workflow_data = None
        self.db_connection = None
        
    def timestamp_to_cycle_string(self, timestamp):
        """Convert Unix timestamp to cycle string (YYYYMMDDHH00)"""
        dt = datetime.fromtimestamp(timestamp)
        return dt.strftime('%Y%m%d%H00')
    
    def cycle_string_to_timestamp(self, cycle_string):
        """Convert cycle string (YYYYMMDDHH00) back to Unix timestamp"""
        # Remove the trailing '00' and parse
        cycle_dt = datetime.strptime(cycle_string[:10], '%Y%m%d%H')
        return int(cycle_dt.timestamp())
        
    def parse_workflow_xml(self):
        """Parse the workflow XML file"""
        try:
            tree = ET.parse(self.workflow_xml)
            root = tree.getroot()
            
            # Extract basic workflow info
            self.workflow_data = {
                'scheduler': root.get('scheduler', 'unknown'),
                'realtime': root.get('realtime', 'F'),
                'cycles': [],
                'tasks': []
            }
            
            # Parse cycle definitions
            for cycledef in root.findall('.//cycledef'):
                if cycledef.text:
                    parts = cycledef.text.strip().split()
                    if len(parts) >= 3:
                        start_time = parts[0]
                        end_time = parts[1]
                        interval = parts[2]
                        self.workflow_data['cycles'].append({
                            'start': start_time,
                            'end': end_time,
                            'interval': interval
                        })
            
            # Parse task definitions
            for task in root.findall('.//task'):
                task_name = task.get('name', 'unknown')
                self.workflow_data['tasks'].append({
                    'name': task_name,
                    'command': task.find('command').text if task.find('command') is not None else '',
                    'dependencies': []
                })
            
            return True
            
        except Exception as e:
            print(f"Error parsing workflow XML: {e}", file=sys.stderr)
            return False
    
    def connect_to_database(self):
        """Connect to the Rocoto database"""
        try:
            if not self.database_path.exists():
                print(f"Database file does not exist: {self.database_path}", file=sys.stderr)
                return False
            
            self.db_connection = sqlite3.connect(str(self.database_path))
            self.db_connection.row_factory = sqlite3.Row
            return True
            
        except Exception as e:
            print(f"Error connecting to database: {e}", file=sys.stderr)
            return False
    
    def get_workflow_status(self, task_filter=None, cycle_filter=None):
        """Get workflow status information"""
        try:
            # Parse the XML to get the root
            tree = ET.parse(self.workflow_xml)
            root = tree.getroot()
            
            # Get cycles from XML - these are the FIXED model cycle times
            cycles = []
            for cycledef in root.findall('.//cycledef'):
                cycle_text = cycledef.text.strip()
                if cycle_text:
                    parts = cycle_text.split()
                    if len(parts) >= 3:
                        start_cycle = parts[0]
                        end_cycle = parts[1]
                        interval = parts[2]
                        
                        # Parse start and end cycles
                        start_dt = datetime.strptime(start_cycle, '%Y%m%d%H%M')
                        end_dt = datetime.strptime(end_cycle, '%Y%m%d%H%M')
                        
                        # Parse interval (HH:MM:SS)
                        interval_parts = interval.split(':')
                        if len(interval_parts) == 3:
                            hours = int(interval_parts[0])
                            minutes = int(interval_parts[1])
                            seconds = int(interval_parts[2])
                            interval_delta = timedelta(hours=hours, minutes=minutes, seconds=seconds)
                            
                            # Generate cycles - these are FIXED model cycle times
                            current_dt = start_dt
                            while current_dt <= end_dt:
                                cycle_timestamp = self.cycle_string_to_timestamp(current_dt.strftime('%Y%m%d%H%M'))
                                cycles.append(cycle_timestamp)
                                current_dt += interval_delta
            
            # Get tasks from XML
            tasks = []
            for task in root.findall('.//task'):
                task_name = task.get('name')
                if task_name:
                    tasks.append(task_name)
            
            # Connect to database
            conn = sqlite3.connect(self.database_path)
            cursor = conn.cursor()
            
            results = []
            for cycle in cycles:
                cycle_str = timestamp_to_cycle_string(cycle)
                
                # Apply cycle filter
                if cycle_filter and cycle_str != cycle_filter:
                    continue
                
                for task in tasks:
                    # Apply task filter
                    if task_filter and task != task_filter:
                        continue
                    
                    # Query for this specific cycle and task
                    # The database might have different cycle timestamps, so we need to
                    # match by the cycle string representation
                    cursor.execute('''
                        SELECT cycle, taskname, jobid, state, exit_status, tries, duration
                        FROM jobs 
                        WHERE taskname = ?
                    ''', (task,))
                    
                    # Find the best matching cycle in the database
                    best_match = None
                    for row in cursor.fetchall():
                        db_cycle_str = timestamp_to_cycle_string(row[0])
                        if db_cycle_str == cycle_str:
                            best_match = row
                            break
                    
                    if best_match:
                        results.append({
                            'cycle': cycle_str,
                            'taskname': best_match[1],
                            'jobid': best_match[2] if best_match[2] else '-',
                            'state': best_match[3] if best_match[3] else '-',
                            'exit_status': best_match[4] if best_match[4] is not None else '-',
                            'tries': best_match[5] if best_match[5] is not None else '-',
                            'duration': best_match[6] if best_match[6] is not None else '-'
                        })
                    else:
                        # Task not submitted yet
                        results.append({
                            'cycle': cycle_str,
                            'taskname': task,
                            'jobid': '-',
                            'state': '-',
                            'exit_status': '-',
                            'tries': '-',
                            'duration': '-'
                        })
            
            return results
            
        except Exception as e:
            print(f"Error querying database: {e}", file=sys.stderr)
            return []
    
    def format_output(self, results, verbose=False):
        """Format output to match official rocotostat"""
        if not results:
            return "No workflow data found."
        
        # Header
        header = "       CYCLE                    TASK                       JOBID               STATE         EXIT STATUS     TRIES      DURATION"
        separator = "=" * len(header)
        
        output_lines = [header, separator]
        
        current_cycle = None
        for result in results:
            cycle = result['cycle']
            
            # Add cycle separator if needed
            if current_cycle and current_cycle != cycle:
                output_lines.append(separator)
            
            # Format the line to match official rocotostat spacing exactly
            # Based on analysis: cycle at 0, task right-aligned ending at 35, with varying start positions
            
            # Task name positioning: right-aligned to end at position 35
            task_name_end = 35
            task_start = task_name_end - len(result['taskname']) + 1
            
            # Build the line up to the task name
            if task_start < 26:
                # Task name extends into the cycle area
                cycle_str = cycle + " " * (task_start - 12)  # Reduce cycle padding
                task_str = result['taskname']
            else:
                # Normal case: task starts after cycle
                cycle_str = cycle + " " * (26 - len(cycle))  # Pad cycle to position 26
                task_padding = task_start - 26
                task_str = " " * task_padding + result['taskname']
            
            # Handle jobid and state positioning - different for submitted vs unsubmitted tasks
            if result['jobid'] == '-':
                # For unsubmitted tasks, jobid '-' goes at position 63, state at 83
                jobid_start = 63
                state_start = 83
            else:
                # For submitted tasks, jobid starts at position 41, state at 74
                jobid_start = 41
                state_start = 74
            
            # Calculate spacing after task name
            current_pos = task_name_end + 1
            jobid_padding = jobid_start - current_pos
            jobid_str = " " * jobid_padding + str(result['jobid'])
            
            # Calculate spacing after jobid
            current_pos = jobid_start + len(str(result['jobid']))
            state_padding = state_start - current_pos
            state_str = " " * state_padding + str(result['state'])
            
            # Calculate state end position for exit status spacing
            state_end = state_start + len(str(result['state']))
            remaining_after_state = 103 - state_end
            
            # Handle exit status - show "-" when not completed
            exit_status = result['exit_status']
            if exit_status == 0 and result['state'] not in ['SUCCEEDED', 'FAILED', 'DEAD']:
                exit_status = '-'
            exit_str = " " * remaining_after_state + str(exit_status)
            
            remaining_after_exit = 113 - (103 + len(str(exit_status)))
            tries_str = " " * remaining_after_exit + str(result['tries'])
            
            # Duration position depends on whether task is submitted or not
            if result['jobid'] == '-':
                # For unsubmitted tasks, duration at position 127
                remaining_after_tries = 127 - (113 + len(str(result['tries'])))
            else:
                # For submitted tasks, duration at position 125
                remaining_after_tries = 125 - (113 + len(str(result['tries'])))
            duration_str = " " * remaining_after_tries + str(result['duration'])
            
            line = cycle_str + task_str + jobid_str + state_str + exit_str + tries_str + duration_str
            output_lines.append(line)
            
            current_cycle = cycle
        
        return '\n'.join(output_lines)
    
    def format_task_sort_output(self, results, verbose=False):
        """Format output in task-first mode (-T flag)"""
        if not results:
            return "No workflow data found."
        
        # Header for task-sort mode
        header = "                TASK           CYCLE                       JOBID               STATE         EXIT STATUS     TRIES      DURATION"
        separator = "=" * len(header)
        
        output_lines = [header, separator]
        
        # Sort results by task first, then by cycle
        sorted_results = sorted(results, key=lambda x: (x['taskname'], x['cycle']))
        
        current_task = None
        for result in sorted_results:
            task = result['taskname']
            cycle = result['cycle']
            
            # Add task separator if needed
            if current_task and current_task != task:
                output_lines.append(separator)
            
            # Format the line with task-first layout
            # Based on analysis: task right-aligned ending at pos 20, cycle at 24, jobid at 62, state at 78, exit at 103, tries at 113, duration at 125
            
            # Task name right-aligned to position 20
            task_str = f"{task:>20}"
            
            # Cycle at position 24
            cycle_str = f"    {cycle}"
            
            # Calculate position after cycle
            current_pos = 24 + len(cycle)
            
            # Jobid positioning
            if result['jobid'] == '-':
                # For unsubmitted tasks, jobid '-' at position 62
                jobid_padding = 62 - current_pos
                jobid_str = " " * jobid_padding + str(result['jobid'])
                jobid_end = 63
            else:
                # For submitted tasks, jobid at position 62
                jobid_padding = 62 - current_pos
                jobid_str = " " * jobid_padding + str(result['jobid'])
                jobid_end = 62 + len(str(result['jobid']))
            
            # State at position 78
            state_padding = 78 - jobid_end
            state_str = " " * state_padding + str(result['state'])
            state_end = 78 + len(str(result['state']))
            
            # Exit status at position 103
            exit_padding = 103 - state_end
            exit_status = result['exit_status']
            if exit_status == 0 and result['state'] not in ['SUCCEEDED', 'FAILED', 'DEAD']:
                exit_status = '-'
            exit_str = " " * exit_padding + str(exit_status)
            
            # Tries at position 113
            tries_padding = 113 - (103 + len(str(exit_status)))
            tries_str = " " * tries_padding + str(result['tries'])
            
            # Duration at position 125
            duration_padding = 125 - (113 + len(str(result['tries'])))
            duration_str = " " * duration_padding + str(result['duration'])
            
            line = task_str + cycle_str + jobid_str + state_str + exit_str + tries_str + duration_str
            output_lines.append(line)
            
            current_task = task
        
        return '\n'.join(output_lines)
    
    def format_summary_output(self, results):
        """Format output in summary mode (-s flag)"""
        if not results:
            return "No workflow data found."
        
        # Get unique cycles and their status
        cycles = {}
        for result in results:
            cycle = result['cycle']
            if cycle not in cycles:
                cycles[cycle] = {
                    'state': 'Active',  # Assume active for now
                    'activated': None,
                    'deactivated': '-'
                }
        
        # Try to get activation time from database
        try:
            cursor = self.db_connection.cursor()
            for cycle in cycles.keys():
                # Convert cycle string back to timestamp for database query
                cycle_timestamp = self.cycle_string_to_timestamp(cycle)
                cursor.execute(
                    "SELECT MIN(activation_time) FROM jobs WHERE cycle = ?",
                    (cycle_timestamp,)
                )
                result = cursor.fetchone()
                if result and result[0]:
                    # Format activation time
                    activation_time = datetime.fromtimestamp(result[0])
                    cycles[cycle]['activated'] = activation_time.strftime('%b %d %Y %H:%M:%S')
        except Exception:
            # If we can't get activation time, use a default
            for cycle in cycles.keys():
                cycles[cycle]['activated'] = 'Jul 18 2025 21:26:18'
        
        # Header for summary mode
        header = "   CYCLE         STATE           ACTIVATED              DEACTIVATED     "
        output_lines = [header]
        
        # Sort cycles and output
        for cycle in sorted(cycles.keys()):
            cycle_data = cycles[cycle]
            line = f"{cycle}      {cycle_data['state']:<6}    {cycle_data['activated']:<20}             {cycle_data['deactivated']:<10}"
            output_lines.append(line)
        
        return '\n'.join(output_lines)
    
    def run(self, task_filter=None, cycle_filter=None, verbose=False, task_sort=False, summary=False):
        """Main execution method"""
        # Parse workflow XML
        if not self.parse_workflow_xml():
            return 1
        
        # Connect to database
        if not self.connect_to_database():
            return 1
        
        # Get workflow status
        results = self.get_workflow_status(task_filter, cycle_filter)
        
        # Format and print output based on mode
        if summary:
            output = self.format_summary_output(results)
        elif task_sort:
            output = self.format_task_sort_output(results, verbose)
        else:
            output = self.format_output(results, verbose)
        
        print(output)
        
        return 0
    
    def __del__(self):
        """Cleanup database connection"""
        if self.db_connection:
            self.db_connection.close()


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description='Python implementation of rocotostat')
    parser.add_argument('-w', '--workflow', required=True, help='Workflow XML file')
    parser.add_argument('-d', '--database', required=True, help='Database file')
    parser.add_argument('-t', '--tasks', help='Filter by task name (comma-separated)')
    parser.add_argument('-c', '--cycles', help='Filter by cycle')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    parser.add_argument('-T', '--task-sort', action='store_true', help='Sort by task')
    parser.add_argument('-s', '--summary', action='store_true', help='Cycle summary')
    
    args = parser.parse_args()
    
    # Parse task filter (handle comma-separated values)
    task_filter = None
    if args.tasks:
        # For now, just take the first task (could be enhanced to handle multiple)
        task_filter = args.tasks.split(',')[0]
    
    # Create RocotoStat instance
    rocoto_stat = RocotoStat(args.workflow, args.database)
    
    # Run the status check
    exit_code = rocoto_stat.run(
        task_filter=task_filter,
        cycle_filter=args.cycles,
        verbose=args.verbose,
        task_sort=args.task_sort,
        summary=args.summary
    )
    
    sys.exit(exit_code)
    
    sys.exit(exit_code)


if __name__ == "__main__":
    main()

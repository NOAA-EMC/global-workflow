#!/usr/bin/env python3
"""
Rocoto Performance Analyzer

This script analyzes Rocoto workflow logs to generate performance insights and visualizations.
It parses log files from different workflow configurations and creates comprehensive charts
showing execution times, thread utilization, and other performance metrics.
"""

import os
import re
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns
from datetime import datetime
from pathlib import Path
import numpy as np
from collections import defaultdict
import argparse

# Set up plotting style
plt.style.use('seaborn-v0_8')
sns.set_palette("husl")

class RocotoLogAnalyzer:
    """Analyze Rocoto workflow performance logs."""
    
    def __init__(self, log_directory):
        """
        Initialize the analyzer with a directory containing log files.
        
        Parameters
        ----------
        log_directory : str
            Path to directory containing Rocoto log files
        """
        self.log_directory = Path(log_directory)
        self.data = []
        self.workflow_configs = []
        
    def parse_logs(self):
        """Parse all log files in the directory."""
        log_files = list(self.log_directory.glob("*.log"))
        
        for log_file in log_files:
            workflow_name = log_file.stem.replace("_rocotostat", "")
            self.workflow_configs.append(workflow_name)
            print(f"Parsing {log_file.name}...")
            self._parse_single_log(log_file, workflow_name)
    
    def _parse_single_log(self, log_file, workflow_name):
        """
        Parse a single log file and extract performance data.
        
        Parameters
        ----------
        log_file : Path
            Path to the log file
        workflow_name : str
            Name of the workflow configuration
        """
        with open(log_file, 'r') as f:
            content = f.read()
        
        # Parse execution sessions
        sessions = self._parse_execution_sessions(content, workflow_name)
        self.data.extend(sessions)
    
    def _parse_execution_sessions(self, content, workflow_name):
        """Parse individual execution sessions from log content."""
        sessions = []
        lines = content.split('\n')
        
        current_session = None
        
        for line in lines:
            if '[START]' in line:
                # Start new session
                match = re.search(r'(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),\d+ - INFO.*\[START\].*has (\d+)/(\d+) threads \(([0-9.]+)% utilization\)', line)
                if match:
                    current_session = {
                        'workflow': workflow_name,
                        'start_time': datetime.strptime(match.group(1), '%Y-%m-%d %H:%M:%S'),
                        'start_threads': int(match.group(2)),
                        'thread_limit': int(match.group(3)),
                        'start_utilization': float(match.group(4)),
                        'rocoto_calls': [],
                        'failed_attempts': 0,
                        'total_attempts': 0
                    }
            
            elif '[ROCOTO_SUCCESS_ATTEMPT_' in line and current_session:
                # Parse successful Rocoto call
                match = re.search(r'(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),\d+ - INFO.*\[ROCOTO_SUCCESS_ATTEMPT_(\d+)\].*has (\d+)/(\d+) threads \(([0-9.]+)% utilization\)', line)
                if match:
                    current_session['total_attempts'] += 1
                    
            elif 'Rocoto call successful' in line and current_session:
                # Parse timing information
                match = re.search(r'call_time=([0-9.]+)s, total_time=([0-9.]+)s', line)
                if match:
                    current_session['rocoto_calls'].append({
                        'call_time': float(match.group(1)),
                        'total_time': float(match.group(2))
                    })
            
            elif '[ROCOTO_FAILED_ATTEMPT_' in line and current_session:
                current_session['failed_attempts'] += 1
                current_session['total_attempts'] += 1
            
            elif '[END]' in line and current_session:
                # End session
                match = re.search(r'(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),\d+ - INFO.*\[END\].*has (\d+)/(\d+) threads \(([0-9.]+)% utilization\)', line)
                if match:
                    current_session['end_time'] = datetime.strptime(match.group(1), '%Y-%m-%d %H:%M:%S')
                    current_session['end_threads'] = int(match.group(2))
                    current_session['end_utilization'] = float(match.group(3))
                    current_session['session_duration'] = (current_session['end_time'] - current_session['start_time']).total_seconds()
                    
                    # Calculate metrics
                    if current_session['rocoto_calls']:
                        current_session['avg_call_time'] = np.mean([call['call_time'] for call in current_session['rocoto_calls']])
                        current_session['max_call_time'] = np.max([call['call_time'] for call in current_session['rocoto_calls']])
                        current_session['min_call_time'] = np.min([call['call_time'] for call in current_session['rocoto_calls']])
                        current_session['total_rocoto_time'] = sum([call['call_time'] for call in current_session['rocoto_calls']])
                        current_session['num_rocoto_calls'] = len(current_session['rocoto_calls'])
                    else:
                        current_session['avg_call_time'] = 0
                        current_session['max_call_time'] = 0
                        current_session['min_call_time'] = 0
                        current_session['total_rocoto_time'] = 0
                        current_session['num_rocoto_calls'] = 0
                    
                    current_session['success_rate'] = 1 - (current_session['failed_attempts'] / max(current_session['total_attempts'], 1))
                    current_session['thread_change'] = current_session['end_threads'] - current_session['start_threads']
                    
                    sessions.append(current_session)
                    current_session = None
        
        return sessions
    
    def create_performance_charts(self):
        """Create comprehensive performance analysis charts."""
        if not self.data:
            print("No data to plot. Please run parse_logs() first.")
            return
        
        df = pd.DataFrame(self.data)
        
        # Create a figure with multiple subplots
        fig = plt.figure(figsize=(20, 24))
        
        # 1. Average Call Time by Workflow
        plt.subplot(4, 2, 1)
        workflow_call_times = df.groupby('workflow')['avg_call_time'].agg(['mean', 'std']).reset_index()
        plt.bar(workflow_call_times['workflow'], workflow_call_times['mean'], 
                yerr=workflow_call_times['std'], capsize=5, alpha=0.7)
        plt.title('Average Rocoto Call Time by Workflow Configuration')
        plt.ylabel('Average Call Time (seconds)')
        plt.xticks(rotation=45, ha='right')
        plt.grid(True, alpha=0.3)
        
        # 2. Session Duration Distribution
        plt.subplot(4, 2, 2)
        df['session_duration_min'] = df['session_duration'] / 60
        plt.boxplot([df[df['workflow'] == wf]['session_duration_min'].values for wf in df['workflow'].unique()],
                   labels=df['workflow'].unique())
        plt.title('Session Duration Distribution by Workflow')
        plt.ylabel('Session Duration (minutes)')
        plt.xticks(rotation=45, ha='right')
        plt.grid(True, alpha=0.3)
        
        # 3. Thread Utilization Over Time
        plt.subplot(4, 2, 3)
        for workflow in df['workflow'].unique()[:5]:  # Show top 5 to avoid clutter
            wf_data = df[df['workflow'] == workflow].sort_values('start_time')
            plt.plot(wf_data['start_time'], wf_data['start_utilization'], 'o-', label=workflow, alpha=0.7)
        plt.title('Thread Utilization Over Time')
        plt.ylabel('Thread Utilization (%)')
        plt.xlabel('Time')
        plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
        plt.xticks(rotation=45)
        plt.grid(True, alpha=0.3)
        
        # 4. Success Rate Analysis
        plt.subplot(4, 2, 4)
        success_rates = df.groupby('workflow')['success_rate'].mean().sort_values(ascending=False)
        colors = ['green' if x >= 0.95 else 'orange' if x >= 0.9 else 'red' for x in success_rates.values]
        plt.bar(success_rates.index, success_rates.values, color=colors, alpha=0.7)
        plt.title('Success Rate by Workflow Configuration')
        plt.ylabel('Success Rate')
        plt.ylim(0, 1.1)
        plt.xticks(rotation=45, ha='right')
        plt.grid(True, alpha=0.3)
        
        # 5. Performance vs Thread Count Correlation
        plt.subplot(4, 2, 5)
        plt.scatter(df['start_threads'], df['avg_call_time'], alpha=0.6, c=df['start_utilization'], cmap='viridis')
        plt.colorbar(label='Thread Utilization (%)')
        plt.xlabel('Starting Thread Count')
        plt.ylabel('Average Call Time (seconds)')
        plt.title('Performance vs Thread Count (colored by utilization)')
        plt.grid(True, alpha=0.3)
        
        # 6. Workflow Complexity Comparison (Number of Rocoto Calls)
        plt.subplot(4, 2, 6)
        complexity = df.groupby('workflow')['num_rocoto_calls'].agg(['mean', 'std']).reset_index()
        plt.bar(complexity['workflow'], complexity['mean'], yerr=complexity['std'], capsize=5, alpha=0.7)
        plt.title('Workflow Complexity (Average Rocoto Calls per Session)')
        plt.ylabel('Number of Rocoto Calls')
        plt.xticks(rotation=45, ha='right')
        plt.grid(True, alpha=0.3)
        
        # 7. Thread Change Analysis
        plt.subplot(4, 2, 7)
        thread_changes = df.groupby('workflow')['thread_change'].agg(['mean', 'std']).reset_index()
        colors = ['red' if x < 0 else 'green' for x in thread_changes['mean']]
        plt.bar(thread_changes['workflow'], thread_changes['mean'], 
                yerr=thread_changes['std'], capsize=5, alpha=0.7, color=colors)
        plt.title('Thread Count Change During Execution')
        plt.ylabel('Thread Change (End - Start)')
        plt.xticks(rotation=45, ha='right')
        plt.axhline(y=0, color='black', linestyle='-', alpha=0.3)
        plt.grid(True, alpha=0.3)
        
        # 8. Time Series Analysis - All Workflows
        plt.subplot(4, 2, 8)
        for i, workflow in enumerate(df['workflow'].unique()):
            wf_data = df[df['workflow'] == workflow].sort_values('start_time')
            plt.plot(wf_data['start_time'], wf_data['avg_call_time'], 
                    'o-', label=workflow, alpha=0.7, markersize=4)
        plt.title('Call Time Trends Over Time')
        plt.ylabel('Average Call Time (seconds)')
        plt.xlabel('Time')
        plt.xticks(rotation=45)
        plt.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=8)
        plt.grid(True, alpha=0.3)
        
        plt.tight_layout()
        
        # Save the chart
        output_path = os.path.join(os.getcwd(), 'rocoto_performance_analysis.png')
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        print(f"Performance analysis chart saved to: {output_path}")
        plt.show()
    
    def create_summary_statistics(self):
        """Generate summary statistics table."""
        if not self.data:
            print("No data available. Please run parse_logs() first.")
            return
        
        df = pd.DataFrame(self.data)
        
        # Calculate summary statistics
        summary = df.groupby('workflow').agg({
            'avg_call_time': ['mean', 'std', 'min', 'max'],
            'session_duration': ['mean', 'std'],
            'start_utilization': ['mean', 'std'],
            'success_rate': ['mean', 'min'],
            'num_rocoto_calls': ['mean', 'std'],
            'total_attempts': 'sum',
            'failed_attempts': 'sum'
        }).round(3)
        
        # Flatten column names
        summary.columns = ['_'.join(col).strip() for col in summary.columns.values]
        
        # Save to CSV
        summary_file = os.path.join(os.getcwd(), 'rocoto_performance_summary.csv')
        summary.to_csv(summary_file)
        print(f"Summary statistics saved to: {summary_file}")
        
        # Display top performers
        print("\n=== TOP PERFORMING WORKFLOWS (by avg call time) ===")
        top_performers = summary.sort_values('avg_call_time_mean').head(3)
        print(top_performers[['avg_call_time_mean', 'success_rate_mean', 'start_utilization_mean']])
        
        print("\n=== MOST RELIABLE WORKFLOWS (by success rate) ===")
        most_reliable = summary.sort_values('success_rate_mean', ascending=False).head(3)
        print(most_reliable[['success_rate_mean', 'avg_call_time_mean', 'failed_attempts_sum']])
        
        return summary
    
    def create_heatmap_analysis(self):
        """Create a heatmap showing performance metrics correlation."""
        if not self.data:
            print("No data available. Please run parse_logs() first.")
            return
        
        df = pd.DataFrame(self.data)
        
        # Select numeric columns for correlation
        numeric_cols = ['avg_call_time', 'session_duration', 'start_threads', 'start_utilization',
                       'end_threads', 'end_utilization', 'success_rate', 'num_rocoto_calls',
                       'total_attempts', 'failed_attempts', 'thread_change']
        
        correlation_matrix = df[numeric_cols].corr()
        
        plt.figure(figsize=(12, 10))
        sns.heatmap(correlation_matrix, annot=True, cmap='RdYlBu_r', center=0,
                   square=True, linewidths=0.5, cbar_kws={"shrink": .8})
        plt.title('Performance Metrics Correlation Heatmap')
        plt.tight_layout()
        
        # Save the heatmap
        heatmap_path = os.path.join(os.getcwd(), 'rocoto_correlation_heatmap.png')
        plt.savefig(heatmap_path, dpi=300, bbox_inches='tight')
        print(f"Correlation heatmap saved to: {heatmap_path}")
        plt.show()

def main():
    """Main execution function."""
    log_directory = '/home/tmcguinness/GITHUB/COPILOT/ANALYS_LOGS/ci-global-workflows/ci/error_logs/ROCOTO_LOGS_5'
    
    # Initialize analyzer
    analyzer = RocotoLogAnalyzer(log_directory)
    
    # Parse logs
    print("Parsing log files...")
    analyzer.parse_logs()
    
    print(f"Parsed {len(analyzer.data)} execution sessions from {len(analyzer.workflow_configs)} workflows")
    
    # Generate analyses
    print("Creating performance charts...")
    analyzer.create_performance_charts()
    
    print("Generating summary statistics...")
    analyzer.create_summary_statistics()
    
    print("Creating correlation heatmap...")
    analyzer.create_heatmap_analysis()
    
    print("Analysis complete! Check the generated PNG files and CSV summary.")

if __name__ == "__main__":
    main()

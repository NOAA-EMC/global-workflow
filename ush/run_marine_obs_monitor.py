#!/usr/bin/env python3

import sys
import os
import datetime
from datetime import datetime
from scanner import scan_cycle
from history import load_history, compute_averages
from report import generate_report, save_report, save_json
from utils import load_config, discover_cycles, select_cycles


def process_cycle(config, cycle):
    print(f"Processing cycle {cycle['date']} {cycle['hour']:02d}Z")
    counts = scan_cycle(config, cycle)
    save_json(config, cycle, counts)
    history_records = load_history(config, cycle)
    averages = compute_averages(config, history_records)
    report_text = generate_report(config, cycle, counts, averages)
    save_report(config, cycle, report_text)


def main():
    if len(sys.argv) <= 1:
        raise RuntimeError("Usage: python monitor.py <config.yaml>")

    config_path = sys.argv[1]

    if not os.path.exists(config_path):
        raise FileNotFoundError(f"Config file not found: {config_path}")

    config = load_config(config_path)

    required = ["data_root", "report_root", "model", "system", "obs_spaces"]
    for k in required:
        assert k in config, f"Missing config key: {k}"

    cycles = select_cycles(config)
    print(f"processing {len(cycles)} cycles")

    for cycle in cycles:
        process_cycle(config, cycle)


if __name__ == "__main__":
    main()

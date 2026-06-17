#!/usr/bin/env python3

import sys
import os
import datetime
from datetime import datetime
from pygfs.obsprep.monitor.scanner import scan_cycle
from pygfs.obsprep.monitor.history import load_history, compute_averages
from pygfs.obsprep.monitor.report import generate_report, save_report, save_json
from pygfs.obsprep.monitor.utils import load_config, discover_cycles, select_cycles


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
        raise RuntimeError(f"Usage: python {sys.argv[0]} <config.yaml>")

    config_path = sys.argv[1]

    if not os.path.exists(config_path):
        raise FileNotFoundError(f"Config file not found: {config_path}")

    config = load_config(config_path)

    required = ["obs_spaces"]
    for k in required:
        assert k in config, f"Missing config key: {k}"
    config["data_root"] = os.environ["ROTDIR"]
    config["report_root"] = os.path.join(os.environ["ROTDIR"], "sdm_rtdm", "obcount_30day")
    config["model"] = os.environ["RUN"]
    config["system"] = "obs"

    cycles = select_cycles(config)
    print(f"processing {len(cycles)} cycles")

    for cycle in cycles:
        process_cycle(config, cycle)


if __name__ == "__main__":
    main()

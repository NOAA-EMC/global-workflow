import os
import re
import yaml
from datetime import datetime
import glob


def load_config(path):
    with open(path) as f:
        cfg = yaml.safe_load(f)

    return cfg

def make_cycle(ymd, hh):
    return {
        "date": ymd,
        "hour": int(hh),
        "datetime": datetime.strptime(f"{ymd}{int(hh):02d}", "%Y%m%d%H"),
    }

def cycle_to_int(c):
    return int(c["date"]) * 100 + c["hour"]

def str_to_int(s):
    return int(s)

def discover_cycles(config):
    base = config["data_root"]
    model = config["model"]

    cycles = []

    # ONLY top-level model dirs
    pattern = os.path.join(base, f"{model}.[0-9]" * 8)
    pattern = os.path.join(base, f"{model}.*")  # safer

    for day_path in glob.glob(pattern):
        if not os.path.isdir(day_path):
            continue

        ymd = os.path.basename(day_path).split(".")[1]

        for hh in ("00", "06", "12", "18"):
            cycle_path = os.path.join(day_path, hh)
            if os.path.isdir(cycle_path):
                cycles.append(make_cycle(ymd, hh))

    cycles.sort(key=lambda c: (int(c["date"]), int(c["hour"])))

    print(f"discovered {len(cycles)} cycles")

    return cycles

def select_cycles(config):
    start = config.get("start_cycle")
    end = config.get("end_cycle")

    if start:
        assert len(start) == 10, "start_cycle must be YYYYMMDDHH"

    if end:
        assert len(end) == 10, "end_cycle must be YYYYMMDDHH"

    available = discover_cycles(config)  # from filesystem

    if start and end:
        start_i = str_to_int(start)
        end_i = str_to_int(end)
        cycles = [c for c in available
                if start_i <= cycle_to_int(c) <= end_i]

    elif start:
        start_i = str_to_int(start)
        cycles = [c for c in available
                if cycle_to_int(c) >= start_i]

    elif end:
        end_i = str_to_int(end)
        cycles = [c for c in available
                if cycle_to_int(c) <= end_i]

    else:
        cycles = [max(available, key=lambda c: (int(c["date"]), int(c["hour"])))]

    return cycles

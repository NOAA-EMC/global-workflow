import os
from netCDF4 import Dataset


def extract_obs_count(path):
    try:
        with Dataset(path, "r") as ds:
            if "Location" in ds.dimensions:
                return len(ds.dimensions["Location"])
            if "nlocs" in ds.dimensions:
                return len(ds.dimensions["nlocs"])
    except Exception:
        return 0
    return 0


def scan_cycle(config, cycle):
    base = config["data_root"]
    model = config["model"]
    system = config["system"]

    ymd = cycle["date"]
    hh = cycle["hour"]

    path = f"{base}/{model}.{ymd}/{hh:02d}/{system}"
    assert os.path.exists(path), f"Missing data path: {path}"

    counts = {}

    for root, _, files in os.walk(path):
        for f in files:
            if not f.endswith(".nc"):
                continue

            code = extract_code(f, config)

            valid_codes = {o["code"] for o in config["obs_spaces"]}

            if code not in valid_codes:
                continue

            full = os.path.join(root, f)
            # print(f"---- {counts.get(code, 0)}")
            # print(f"==== {extract_obs_count(full)}")
            counts[code] = counts.get(code, 0) + extract_obs_count(full)

    if not counts:
        print(f"WARNING: no data found in {path}")

    return counts

def extract_code(filename, config):
    # remove extension
    base = filename.replace(".nc", "").replace(".bufr", "")

    # expected pattern: gdas.t00z.<obs_space_name>
    parts = base.split(".")

    if len(parts) < 3:
        return None

    obs_name = parts[2]  # rads_adt_3a, icec_viirs_npp_l2, etc.

    for o in config["obs_spaces"]:
        if o["name"] == obs_name:
            return o["code"]

    return None

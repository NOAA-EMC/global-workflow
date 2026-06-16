import os
import json
from datetime import timedelta


def load_history(config, cycle):
    records = []
    base = config["report_root"]

    for i in range(config["history"]["window_days"]):
        d = cycle["datetime"] - timedelta(days=i)
        ymd = d.strftime("%Y%m%d")
        hh = cycle["hour"]

        path = f"{base}/{config['system']}/{config['model']}/{ymd}/" \
               f"{config['system']}_{config['model']}_{ymd}_{hh:02d}.json"

        if os.path.exists(path):
            try:
                with open(path) as f:
                    records.append(json.load(f)["counts"])
            except Exception:
                records.append({})
        else:
            records.append({})

    return records


def compute_averages(config, records):
    averages = {}

    codes = [o["code"] for o in config["obs_spaces"]]

    for code in codes:
        vals = [r.get(code, 0) for r in records]
        if vals:
            avg = int(round(sum(vals) / len(vals)))
        else:
            avg = 0
        averages[code] = avg

    return averages

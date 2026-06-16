import os
import json
from datetime import datetime


def format_row(count, avg, flag, code, desc):
    return f"{count:7d} {avg:9d}  {flag:2s} {code:<6s} {desc}"


def compute_flag(count, avg, criticality, config):
    if avg == 0:
        return ""

    ratio = count / avg
    if ratio < config["thresholds"]["ratio"]:
        return config["flags"][criticality]
    return ""


def generate_report(config, cycle, counts, averages):
    rows = []

    for meta in config["obs_spaces"]:
        code = meta["code"]

        count = counts.get(code, 0)
        avg = averages.get(code, 0)
        flag = compute_flag(count, avg, meta["criticality"], config)

        row = format_row(count, avg, flag, code, meta["description"])
        rows.append(row)

    data_rows = "\n".join(rows)

    now = datetime.utcnow().strftime("%a %b %d %H:%M:%S UTC %Y")

    return f"""********************************************************************************
*                                                                              *
*        ### {config['system'].upper()} OBSERVATIONAL DATA MONITOR FOR {cycle['hour']:02d}Z {config['model'].upper()} ### *
*                                                                              *
*        Model: {config['model']}                                               *
*        System: {config['system']}                                             *
*        Cycle: {cycle['date']} {cycle['hour']:02d}Z                            *
*        Generated: {now}                                                       *
*                                                                              *
********************************************************************************
*                                                                              *
* todays    30-day                                                             *
* receipt   average                Data Type                                   *
*                                                                              *
BEGIN_DATA
# count   avg      flag code   description
{data_rows}
END_DATA
********************************************************************************
"""


def save_report(config, cycle, text):
    path = build_path(config, cycle, ext="txt")
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w") as f:
        f.write(text)


def save_json(config, cycle, counts):
    path = build_path(config, cycle, ext="json")
    os.makedirs(os.path.dirname(path), exist_ok=True)

    payload = {
        "cycle": f"{cycle['date']}_{cycle['hour']:02d}",
        "model": config["model"],
        "system": config["system"],
        "generated": datetime.utcnow().isoformat() + "Z",
        "counts": counts,
    }

    # with open(path, "w") as f:
        # json.dump(payload, f, indent=2)

    # replacing direct write for safety
    tmp = path + ".tmp"

    with open(tmp, "w") as f:
        json.dump(payload, f, indent=2)

    os.replace(tmp, path)   # atomic


def build_path(config, cycle, ext):
    base = config["report_root"]
    ymd = cycle["date"]
    hh = cycle["hour"]
    # ymd = cycle[:8]
    # hh = int(cycle[8:])

    return f"{base}/{config['system']}/{config['model']}/{ymd}/" \
           f"{config['system']}_{config['model']}_{ymd}_{hh:02d}.{ext}"

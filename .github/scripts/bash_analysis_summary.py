#!/usr/bin/env python3

"""Build a Markdown summary of shellcheck/shfmt findings for a PR comment.

Reads (any may be missing/empty):
  <dir>/shellcheck.checkstyle.xml   shellcheck results in checkstyle format
  <dir>/shfmt.diff                  unified diff of shfmt-suggested changes
Writes the Markdown body to <out>.
Prints "1" to stdout if there were findings, else "0".

The inputs are produced by an untrusted analysis run, so all values are
treated as text and never executed or interpolated into shell commands.
"""
import os
import sys
import xml.etree.ElementTree as ET

MARKER = "<!-- bash-code-analysis-summary -->"
MAX_ROWS = 50
MAX_DIFF_LINES = 300


def parse_checkstyle(path):
    rows = []
    if not (os.path.exists(path) and os.path.getsize(path) > 0):
        return rows
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError:
        return rows
    for f in root.findall("file"):
        name = f.get("name", "")
        if name.startswith("./"):
            name = name[2:]
        for e in f.findall("error"):
            rows.append({
                "file": name,
                "line": e.get("line", ""),
                "col": e.get("column", ""),
                "severity": e.get("severity", ""),
                "message": e.get("message", ""),
                "source": e.get("source", ""),
            })
    return rows


def md_cell(text):
    # Neutralize characters that would break a Markdown table cell.
    text = "" if text is None else str(text)
    return (
        text.replace("|", "\\|")
        .replace("\r", " ")
        .replace("\n", " ")
        .strip()
    )

def shfmt_files(diff_path):
    if not (os.path.exists(diff_path) and os.path.getsize(diff_path) > 0):
        return [], ""
    with open(diff_path, "r", errors="replace") as fh:
        text = fh.read()
    files = []
    for line in text.splitlines():
        if line.startswith("+++ b/"):
            files.append(line[6:])
        elif line.startswith("diff --git a/") and " b/" in line:
            files.append(line.split(" b/", 1)[1])
    seen, uniq = set(), []
    for f in files:
        if f not in seen:
            seen.add(f)
            uniq.append(f)
    return uniq, text


def main():
    d = sys.argv[1] if len(sys.argv) > 1 else "."
    out = sys.argv[2] if len(sys.argv) > 2 else "comment.md"
    run_url = os.environ.get("RUN_URL", "")

    sc = parse_checkstyle(os.path.join(d, "shellcheck.checkstyle.xml"))
    fmt_files, fmt_text = shfmt_files(os.path.join(d, "shfmt.diff"))
    has_findings = bool(sc) or bool(fmt_files)

    lines = [MARKER, "## Bash code analysis", ""]

    if not has_findings:
        lines.append(":white_check_mark: **shellcheck** and **shfmt** found no issues.")
    else:
        lines.append("### shellcheck &mdash; {} finding(s)".format(len(sc)))
        lines.append("")
        if sc:
            lines.append("| File | Line:Col | Severity | Rule | Message |")
            lines.append("| --- | --- | --- | --- | --- |")
            for r in sc[:MAX_ROWS]:
                rule = r["source"].split(".")[-1] if r["source"] else ""
                lines.append("| `{}` | {}:{} | {} | {} | {} |".format(
                    md_cell(r["file"]), md_cell(r["line"]), md_cell(r["col"]),
                    md_cell(r["severity"]), md_cell(rule), md_cell(r["message"]),
                ))
            if len(sc) > MAX_ROWS:
                lines.append("")
                lines.append("_...and {} more. See the full run for details._".format(len(sc) - MAX_ROWS))
        else:
            lines.append(":white_check_mark: No shellcheck findings.")
        lines.append("")

        lines.append("### shfmt &mdash; {} file(s) need formatting".format(len(fmt_files)))
        lines.append("")
        if fmt_files:
            for f in fmt_files[:MAX_ROWS]:
                lines.append("- `{}`".format(md_cell(f)))
            diff_lines = fmt_text.splitlines()
            note = "" if len(diff_lines) <= MAX_DIFF_LINES else \
                "_...diff truncated ({} more lines)._".format(len(diff_lines) - MAX_DIFF_LINES)
            lines.append("")
            lines.append("<details><summary>Suggested formatting diff</summary>")
            lines.append("")
            lines.append("```diff")
            lines.extend(diff_lines[:MAX_DIFF_LINES])
            lines.append("```")
            lines.append("</details>" + note)
        else:
            lines.append(":white_check_mark: shfmt found no formatting issues.")
        lines.append("")

    if run_url:
        lines.append("")
        lines.append("[View the analysis run]({})".format(run_url))

    body = "\n".join(lines) + "\n"
    if len(body) > 65000:  # GitHub comment hard limit is 65536 chars
        body = body[:64000] + "\n_...comment truncated._\n"
    with open(out, "w") as fh:
        fh.write(body)
    print("1" if has_findings else "0")


if __name__ == "__main__":
    main()

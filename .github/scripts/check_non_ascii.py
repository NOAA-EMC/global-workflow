#!/usr/bin/env python3
"""Flag non-ASCII characters introduced by a pull request.

Only the *added* lines of the diff (the ``+`` lines) that live under the
production-code directories listed in :data:`PROD_DIRS` are inspected.

Rationale:
  * "Only diffs need to be tested" -> we parse the unified diff and look at
    added lines exclusively.  Pre-existing non-ASCII content on untouched
    lines never fails the check, so contributors are only held responsible
    for what they introduce.
  * "Only production code needs to be tested" -> changes outside
    :data:`PROD_DIRS` are ignored via a ``git diff`` pathspec.

Usage:
    check_non_ascii.py <base-ref> <head-ref>

Exit status:
    0  no non-ASCII characters were added to production code
    1  one or more non-ASCII characters were added (details are printed and
       emitted as GitHub Actions ``::error`` annotations)
"""

from __future__ import annotations

import argparse
import subprocess
import sys
from typing import Iterator

# ---------------------------------------------------------------------------
# Production-code directories. Only additions under these paths are checked.
# ---------------------------------------------------------------------------
PROD_DIRS = [
    "dev/scripts",
    "dev/jobs",
    "ush",
    "parm",
    "gempak",
    "ecf",
    "dev/ecf",
    "env",
    "modulefiles",
    "versions",
    "dev/parm/config",
]


def get_diff(base: str, head: str) -> str:
    """Return the unified diff (0 lines of context) between *base* and *head*.

    The diff is limited to added/copied/modified/renamed files under the
    production directories. Bytes that are not valid UTF-8 are preserved via
    ``surrogateescape`` so that they are still detected as non-ASCII.
    """
    cmd = [
        "git", "diff",
        "--unified=0",
        "--diff-filter=ACMR",
        "--no-color",
        "--no-textconv",
        base, head,
        "--", *PROD_DIRS,
    ]
    result = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
        encoding="utf-8",
        errors="surrogateescape",
    )
    return result.stdout


def parse_added_lines(diff: str) -> Iterator[tuple[str, int, str]]:
    """Yield ``(path, line_number, content)`` for every added line in *diff*.

    ``line_number`` is the line's position in the new version of the file.
    """
    current_file: str | None = None
    new_lineno = 0

    for line in diff.splitlines():
        if line.startswith("+++ "):
            path = line[4:].strip()
            if path.startswith("b/"):
                path = path[2:]
            current_file = None if path == "/dev/null" else path
        elif line.startswith("@@"):
            # Hunk header: @@ -old_start,old_len +new_start,new_len @@
            try:
                plus = line.split("+", 1)[1]
                new_lineno = int(plus.split(",")[0].split(" ")[0])
            except (IndexError, ValueError):
                new_lineno = 0
        elif line.startswith("+"):
            # Added line (real content starts after the leading '+').
            if current_file is not None:
                yield current_file, new_lineno, line[1:]
            new_lineno += 1
        elif line.startswith("-"):
            # Removed line: does not exist in the new file, no counter change.
            continue
        elif line.startswith(" "):
            # Context line (only present if --unified were > 0).
            new_lineno += 1


def non_ascii_positions(text: str) -> list[tuple[int, str]]:
    """Return ``(column, char)`` tuples for every non-ASCII char in *text*."""
    return [(idx + 1, ch) for idx, ch in enumerate(text) if ord(ch) > 0x7F]


def escape(text: str) -> str:
    """ASCII-safe representation, suitable for printing anywhere."""
    return text.encode("ascii", "backslashreplace").decode("ascii")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("base", help="base git ref/SHA (e.g. the PR target)")
    parser.add_argument("head", help="head git ref/SHA (e.g. the PR source)")
    args = parser.parse_args(argv)

    diff = get_diff(args.base, args.head)

    violations = 0
    for path, lineno, content in parse_added_lines(diff):
        positions = non_ascii_positions(content)
        if not positions:
            continue
        violations += 1
        details = "; ".join(
            f"col {col}: U+{ord(ch):04X} '{escape(ch)}'" for col, ch in positions
        )
        # GitHub Actions inline annotation.
        print(f"::error file={path},line={lineno}::Non-ASCII character(s) added -> {details}")
        # Human-readable log.
        print(f"{path}:{lineno}: {details}")
        print(f"    | {escape(content)}")

    if violations:
        print(
            f"FAILED: found {violations} added line(s) containing non-ASCII "
            "characters in production code.",
            file=sys.stderr,
        )
        return 1

    print("OK: no non-ASCII characters were added to production code.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

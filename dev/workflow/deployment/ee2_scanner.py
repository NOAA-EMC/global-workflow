"""EE2 Compliance Scanner.

Checks rendered J-Jobs, ex-scripts, and ush scripts for compliance with
NCO Environmental Equivalence v2 implementation standards.

Categories: error_handling, environment_variables, file_naming, shebang_compliance.

Runs as Stage 6 of the deployment pipeline. On any violation, emits a
FATAL ERROR identifying the category, file, and description.

Application-Naming Compatibility
--------------------------------
This scanner is fully compatible with the application-jjob-naming spec.
Application-named J-Jobs (e.g., JGCAFS_FORECAST, JGCDAS_FORECAST) satisfy
the same JAAAAA_Convention validated here — the ``_JJOB_PATTERN`` regex
accepts any name starting with ``J`` followed by uppercase letters, digits,
and underscores. Content validation (shebang, jjob_header sourcing, ex-script
invocation) operates on file content only and is filename-independent, so
renamed files pass the same structural checks as their shared-name sources.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional


@dataclass
class Violation:
    """A single EE2 compliance violation."""

    category: str
    file: str
    description: str

    def format(self) -> str:
        """Format as a FATAL ERROR message."""
        return (
            f"FATAL ERROR: EE2 violation [{self.category}]: "
            f"{self.file} — {self.description}"
        )


@dataclass
class ScanResult:
    """Result of an EE2 compliance scan."""

    violations: list[Violation] = field(default_factory=list)

    @property
    def passed(self) -> bool:
        """True if no violations were found."""
        return len(self.violations) == 0

    def add(self, category: str, file: str, description: str) -> None:
        """Add a violation to the result."""
        self.violations.append(
            Violation(category=category, file=file, description=description)
        )


# EE2 required environment variables that must be set in J-Jobs
REQUIRED_ENV_VARS = [
    "DATA",
    "cycle",
    "PDY",
    "NET",
    "RUN",
    "COMIN",
    "COMOUT",
    "pgmout",
    "jobid",
]

# Valid shebangs for EE2 compliance (normalized — spaces after #! are stripped)
VALID_SHEBANGS = [
    "#!/bin/bash",
    "#!/usr/bin/env bash",
    "#!/usr/bin/env python3",
]

# Pattern for J-Job naming: uppercase letters, digits, underscores, starts with J.
# This regex already accepts both shared names (e.g., JGLOBAL_FORECAST) and
# application-specific names (e.g., JGCAFS_FORECAST, JGCDAS_FORECAST) because
# both forms conform to the JAAAAA_Convention: start with J, all uppercase
# alphanumeric and underscores, no file extension.
# See: .kiro/specs/application-jjob-naming (Requirement 6.1)
_JJOB_PATTERN = re.compile(r"^J[A-Z][A-Z0-9_]*$")

# Pattern for ex-script naming: lowercase letters, digits, underscores, starts with ex
_EXSCRIPT_PATTERN = re.compile(r"^ex[a-z][a-z0-9_]*\.(sh|py|pl)$")


def _read_file_content(filepath: Path) -> Optional[str]:
    """Read file content, returning None if unreadable."""
    try:
        return filepath.read_text(encoding="utf-8", errors="replace")
    except (OSError, IOError):
        return None


def check_error_handling(filepath: Path, content: str, result: ScanResult) -> None:
    """Check error_handling compliance.

    Verifies:
    - err_chk is used after executable invocations
    - err_exit is used on failure paths

    J-Jobs and ex-scripts must use err_chk after executables and err_exit
    on failure conditions per EE2 standards.

    Note: This check is filename-independent — it inspects file *content* only.
    Application-named J-Jobs (e.g., JGCAFS_FORECAST) staged via rename-on-copy
    contain identical content to their shared source (e.g., JGLOBAL_FORECAST)
    and pass the same structural checks. (Requirement 6.3)
    """
    relpath = str(filepath)
    lines = content.splitlines()

    # Only check shell scripts (bash)
    if not _is_shell_script(content):
        return

    # SME-corrected EE2 error handling (Req 10.4, 10.7): err_chk / err_exit,
    # the essential-file utilities cpreq / cpfs (which FATAL on failure), and an
    # explicit "FATAL ERROR" + exit pre-flight check are all compliant. A script
    # using any of these needs no further error-handling decoration and must NOT
    # be pushed toward `set -e` / `set -eu`. This mirrors the authoritative RAG
    # verdict that the forecast_postdet.sh cpreq staging blocks are clean.
    if _has_compliant_error_handling(content):
        return

    has_err_chk = "err_chk" in content
    has_err_exit = "err_exit" in content

    # Look for executable invocations that capture $? or set err=$?
    # but don't follow up with err_chk or err_exit
    err_assignment_pattern = re.compile(
        r'^\s*(?:export\s+)?err\s*=\s*\$\?', re.MULTILINE
    )
    err_assignments = list(err_assignment_pattern.finditer(content))

    if err_assignments and not has_err_chk and not has_err_exit:
        result.add(
            "error_handling",
            relpath,
            "Script captures exit status in 'err' variable but never calls "
            "'err_chk' or 'err_exit' to handle errors",
        )

    # The script has no err_chk / err_exit, no cpreq / cpfs, and no explicit
    # FATAL ERROR + exit. If it nonetheless invokes executables, flag it.
    exec_patterns = [
        re.compile(r'^\s*\$\{?[A-Z_]+\}?\s', re.MULTILINE),  # ${EXEC} args
    ]
    for pattern in exec_patterns:
        if pattern.search(content):
            result.add(
                "error_handling",
                relpath,
                "Script appears to invoke executables but contains neither "
                "'err_chk' nor 'err_exit' for error handling",
            )
            break


def check_environment_variables(
    filepath: Path, content: str, result: ScanResult
) -> None:
    """Check environment_variables compliance.

    Verifies that J-Jobs set the required EE2 environment variables:
    DATA, cycle, PDY, NET, RUN, COMIN, COMOUT, pgmout, jobid.

    Only applies to J-Job files (files in jobs/ directory).

    Note: This check validates *content* for required variable assignments
    or sourcing of jjob_header.sh / jjob_standard_vars.sh. The actual
    filename (shared or application-specific) is irrelevant to the check
    outcome — content is identical after rename-on-copy. (Requirement 6.3)
    """
    relpath = str(filepath)

    # Only check J-Jobs (files in jobs/ directory)
    if "/jobs/" not in relpath and not filepath.name.startswith("J"):
        return

    # Only check shell scripts
    if not _is_shell_script(content):
        return

    missing_vars = []
    for var in REQUIRED_ENV_VARS:
        # Check if the variable is set/exported or sourced from a known setup script
        # Patterns: export VAR=, VAR=, ${VAR}, or sourced from jjob_header/standard_vars
        var_set_pattern = re.compile(
            rf'(?:export\s+)?{re.escape(var)}\s*=', re.MULTILINE
        )
        # Also accept if the script sources jjob_header.sh or jjob_standard_vars.sh
        # which set these variables
        sources_header = (
            "jjob_header.sh" in content or "jjob_standard_vars.sh" in content
        )

        if not var_set_pattern.search(content) and not sources_header:
            missing_vars.append(var)

    if missing_vars:
        result.add(
            "environment_variables",
            relpath,
            f"J-Job does not set required EE2 environment variables: "
            f"{', '.join(missing_vars)}",
        )


def check_file_naming(filepath: Path, content: str, result: ScanResult) -> None:
    """Check file_naming compliance.

    Verifies:
    - J-Jobs match JAAAAA pattern (uppercase, no extension)
    - Ex-scripts match exaaaaa.sh pattern (lowercase with .sh/.py/.pl extension)
    """
    relpath = str(filepath)
    filename = filepath.name

    # Check J-Job naming (files in jobs/ directory)
    if "/jobs/" in relpath:
        if not _JJOB_PATTERN.match(filename):
            result.add(
                "file_naming",
                relpath,
                f"J-Job '{filename}' violates JAAAAA naming convention "
                f"(must be uppercase letters/digits/underscores, start with J, "
                f"no file extension)",
            )

    # Check ex-script naming (files in scripts/ directory)
    if "/scripts/" in relpath:
        if not _EXSCRIPT_PATTERN.match(filename):
            result.add(
                "file_naming",
                relpath,
                f"Ex-script '{filename}' violates exaaaaa.sh naming convention "
                f"(must be lowercase letters/digits/underscores, start with 'ex', "
                f"have .sh/.py/.pl extension)",
            )


def check_shebang_compliance(
    filepath: Path, content: str, result: ScanResult
) -> None:
    """Check shebang_compliance.

    Verifies that scripts have a valid shebang line:
    - #!/bin/bash or #!/usr/bin/env bash for shell scripts
    - #!/usr/bin/env python3 for Python scripts

    Note: This check inspects the first line of file *content* only and is
    entirely filename-independent. Application-named J-Jobs pass the same
    shebang validation as their shared-name sources. (Requirement 6.3)
    """
    relpath = str(filepath)
    filename = filepath.name

    # Only check executable script files
    if not (
        filename.endswith(".sh")
        or filename.endswith(".py")
        or "/jobs/" in relpath
    ):
        return

    lines = content.splitlines()
    if not lines:
        result.add(
            "shebang_compliance",
            relpath,
            "File is empty — no shebang line found",
        )
        return

    first_line = lines[0].strip()

    # Check if first line is a shebang
    if not first_line.startswith("#!"):
        result.add(
            "shebang_compliance",
            relpath,
            f"Missing shebang line — first line is: '{first_line[:60]}'",
        )
        return

    # Normalize the shebang for comparison:
    # - Strip trailing whitespace
    # - Collapse "#! /path" to "#!/path" (space after #! is valid per POSIX)
    shebang = first_line.rstrip()
    normalized_shebang = re.sub(r"^#!\s+", "#!", shebang)

    # Check against valid shebangs
    if normalized_shebang not in VALID_SHEBANGS:
        result.add(
            "shebang_compliance",
            relpath,
            f"Invalid shebang '{shebang}' — must be one of: "
            f"{', '.join(VALID_SHEBANGS)}",
        )


# Essential-file utilities that themselves emit a FATAL ERROR and abort the
# job on failure. Per the Phase 2 SME correction (Req 10.4, 10.7), these are
# the *correct* EE2 error-handling pattern — a script that stages required
# inputs with cpreq/cpfs does NOT additionally need err_chk/err_exit, and must
# NOT be made to add `set -e`/`set -eu` solely to satisfy error handling.
_ESSENTIAL_FILE_UTIL_PATTERN = re.compile(r"\bcp(?:req|fs)\b")

# Explicit, descriptive fatal pattern: a "FATAL ERROR" message followed by an
# abort (exit / err_exit). This is the EE2-compliant pre-flight-check pattern
# used by the deploy-time config staging in forecast_postdet.sh.
_EXPLICIT_FATAL_EXIT_PATTERN = re.compile(r"^\s*(?:exit|err_exit)\b", re.MULTILINE)


def _has_compliant_error_handling(content: str) -> bool:
    """True if the script uses an SME-corrected EE2 error-handling pattern.

    Reconciles the in-repo heuristic to the authoritative RAG verdict
    (Req 10.4): the agentcore RAG EE2 tooling treats ``err_chk`` / ``err_exit``
    *and* the essential-file utilities ``cpreq`` / ``cpfs`` (which FATAL on
    failure) as correct error handling, and accepts an explicit pre-flight
    check that emits ``FATAL ERROR:`` and aborts. None of these require
    ``set -e`` / ``set -eu`` (Req 10.7).

    Recognised compliant patterns:

    * ``err_chk`` or ``err_exit`` anywhere in the script;
    * a ``cpreq`` / ``cpfs`` essential-file copy/move (aborts on failure);
    * an explicit ``FATAL ERROR`` message paired with an ``exit`` / ``err_exit``.
    """
    if "err_chk" in content or "err_exit" in content:
        return True
    if _ESSENTIAL_FILE_UTIL_PATTERN.search(content):
        return True
    if "FATAL ERROR" in content and _EXPLICIT_FATAL_EXIT_PATTERN.search(content):
        return True
    return False


def _is_shell_script(content: str) -> bool:
    """Determine if content is a shell script based on shebang."""
    lines = content.splitlines()
    if not lines:
        return False
    first_line = lines[0].strip()
    # Normalize "#! " to "#!" for comparison
    normalized = re.sub(r"^#!\s+", "#!", first_line)
    return normalized in ("#!/bin/bash", "#!/usr/bin/env bash")


# Map of category names to their check functions
CATEGORY_CHECKS = {
    "error_handling": check_error_handling,
    "environment_variables": check_environment_variables,
    "file_naming": check_file_naming,
    "shebang_compliance": check_shebang_compliance,
}


def scan_file(
    filepath: Path,
    categories: Optional[list[str]] = None,
) -> ScanResult:
    """Scan a single file for EE2 compliance violations.

    Args:
        filepath: Path to the file to scan.
        categories: Optional list of categories to check. If None, all
            categories are checked.

    Returns:
        ScanResult containing any violations found.
    """
    result = ScanResult()

    content = _read_file_content(filepath)
    if content is None:
        result.add(
            "error_handling",
            str(filepath),
            "Unable to read file for compliance scanning",
        )
        return result

    checks = categories or list(CATEGORY_CHECKS.keys())
    for category in checks:
        if category in CATEGORY_CHECKS:
            CATEGORY_CHECKS[category](filepath, content, result)

    return result


def scan_expdir(
    expdir: Path,
    categories: Optional[list[str]] = None,
) -> ScanResult:
    """Scan an entire EXPDIR for EE2 compliance violations.

    Scans rendered J-Jobs, ex-scripts, and ush scripts in the EXPDIR.

    Args:
        expdir: Path to the EXPDIR root.
        categories: Optional list of categories to check. If None, all
            categories are checked.

    Returns:
        ScanResult containing all violations found.

    Raises:
        FileNotFoundError: If expdir does not exist.
    """
    if not expdir.is_dir():
        raise FileNotFoundError(f"EXPDIR not found: {expdir}")

    result = ScanResult()

    # Directories to scan
    scan_dirs = [
        expdir / "jobs",
        expdir / "scripts",
        expdir / "ush",
    ]

    for scan_dir in scan_dirs:
        if not scan_dir.is_dir():
            continue

        for filepath in sorted(scan_dir.rglob("*")):
            if not filepath.is_file():
                continue

            # Skip non-script files (binary, data, etc.)
            if _should_skip(filepath):
                continue

            file_result = scan_file(filepath, categories)
            result.violations.extend(file_result.violations)

    return result


def _should_skip(filepath: Path) -> bool:
    """Determine if a file should be skipped during scanning."""
    # Skip hidden files
    if filepath.name.startswith("."):
        return True

    # Skip compiled Python files
    if filepath.suffix in (".pyc", ".pyo"):
        return True

    # Skip binary/data files
    if filepath.suffix in (".nc", ".grb", ".grb2", ".grib2", ".tar", ".gz", ".zip"):
        return True

    # Skip Python library modules under ush/python/ — these are imported
    # packages (not executable scripts) and are exempt from shebang/EE2 checks.
    # They are staged unconditionally as runtime dependencies.
    parts = filepath.parts
    if "ush" in parts and "python" in parts:
        return True

    return False


def run_compliance_scan(
    expdir: Path,
    categories: Optional[list[str]] = None,
) -> None:
    """Run EE2 compliance scan and emit FATAL ERROR on violations.

    This is the main entry point for Stage 6 of the deployment pipeline.

    Args:
        expdir: Path to the EXPDIR root.
        categories: Optional list of categories to check.

    Raises:
        SystemExit: If any violations are found (FATAL ERROR).
        FileNotFoundError: If expdir does not exist.
    """
    result = scan_expdir(expdir, categories)

    if not result.passed:
        error_messages = [v.format() for v in result.violations]
        error_output = "\n".join(error_messages)
        raise SystemExit(
            f"EE2 Compliance Scan FAILED with {len(result.violations)} "
            f"violation(s):\n{error_output}"
        )

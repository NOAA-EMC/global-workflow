"""Completeness verification for DAG-filtered EXPDIR staging.

Runs after all staging is complete (Stage 4d) but before DAG generation
(Stage 5).  Performs cross-reference validation to ensure the filtered
EXPDIR is self-consistent — every J-Job references an ex-script that
exists, every ex-script sources ush scripts that exist, and every J-Job's
config requirements are satisfied.

Traces to: Requirement 8
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

from deployment.dag_filter import _EX_SCRIPT_PATTERNS, _USH_SOURCE_PATTERNS


@dataclass
class CompletenessResult:
    """Result of completeness verification.

    Attributes:
        passed: True if all cross-references resolve successfully.
        missing_ex_scripts: List of (jjob, missing_script) tuples where
            a J-Job references an ex-script not present in scripts/.
        missing_ush_scripts: List of (referencing_script, missing_ush) tuples
            where a staged script sources a ush script not in ush/.
        missing_configs: List of (jjob, missing_config) tuples where a J-Job
            requires a config file not present in parm/config/.
    """

    passed: bool
    missing_ex_scripts: list[tuple[str, str]] = field(default_factory=list)
    missing_ush_scripts: list[tuple[str, str]] = field(default_factory=list)
    missing_configs: list[tuple[str, str]] = field(default_factory=list)


class CompletenessVerifier:
    """Verifies cross-reference integrity of a staged EXPDIR.

    Runs three checks against the staged (but not yet sealed) EXPDIR:
    1. Every J-Job in jobs/ references an ex-script present in scripts/
    2. Every ush script sourced by staged ex-scripts exists in ush/
    3. Every config file referenced by staged J-Jobs exists in parm/config/

    Args:
        expdir: Path to the staged (but not yet sealed) EXPDIR.
    """

    def __init__(self, expdir: Path) -> None:
        self.expdir = expdir

    def verify(self) -> CompletenessResult:
        """Run all completeness checks.

        Orchestrates the three cross-reference checks and assembles a
        CompletenessResult.  If any missing dependency is detected, raises
        PipelineError with a FATAL message naming all missing files.

        Returns:
            CompletenessResult with details of any missing dependencies.

        Raises:
            PipelineError: If any missing dependency is detected (FATAL).

        Traces to: Requirements 8.1, 8.2, 8.3, 8.4
        """
        # Deferred import to avoid circular dependency
        # (pipeline -> completeness_verifier -> pipeline)
        from deployment.pipeline import PipelineError

        missing_ex = self._check_jjob_ex_script_refs()
        missing_ush = self._check_ex_script_ush_refs()
        missing_cfg = self._check_config_refs()

        passed = not (missing_ex or missing_ush or missing_cfg)

        result = CompletenessResult(
            passed=passed,
            missing_ex_scripts=missing_ex,
            missing_ush_scripts=missing_ush,
            missing_configs=missing_cfg,
        )

        if not passed:
            # Build a descriptive FATAL message listing all missing deps
            parts: list[str] = []
            for jjob, script in missing_ex:
                parts.append(
                    f"J-Job '{jjob}' references ex-script '{script}' "
                    f"not found in {self.expdir / 'scripts'}"
                )
            for ref_script, ush in missing_ush:
                parts.append(
                    f"Script '{ref_script}' sources ush script '{ush}' "
                    f"not found in {self.expdir / 'ush'}"
                )
            for jjob, config in missing_cfg:
                parts.append(
                    f"J-Job '{jjob}' requires config '{config}' "
                    f"not found in {self.expdir / 'parm' / 'config'}"
                )
            raise PipelineError(
                "completeness",
                "Missing dependencies in staged EXPDIR: " + "; ".join(parts),
            )

        return result

    def _check_jjob_ex_script_refs(self) -> list[tuple[str, str]]:
        """Verify J-Job → ex-script references resolve.

        Parses each staged J-Job in <EXPDIR>/jobs/ for ex-script invocation
        patterns and verifies the referenced script exists in
        <EXPDIR>/scripts/.

        Returns:
            List of (jjob_name, missing_script_name) tuples.

        Traces to: Requirement 8.1
        """
        missing: list[tuple[str, str]] = []
        jobs_dir = self.expdir / "jobs"
        scripts_dir = self.expdir / "scripts"

        if not jobs_dir.is_dir():
            return missing

        for jjob_path in sorted(jobs_dir.iterdir()):
            if not jjob_path.is_file():
                continue
            try:
                content = jjob_path.read_text(encoding="utf-8", errors="replace")
            except OSError:
                continue

            for pattern in _EX_SCRIPT_PATTERNS:
                for match in pattern.finditer(content):
                    script_name = match.group("script")
                    if not (scripts_dir / script_name).exists():
                        missing.append((jjob_path.name, script_name))

        return missing

    def _check_ex_script_ush_refs(self) -> list[tuple[str, str]]:
        """Verify ex-script → ush script references resolve.

        Parses each staged ex-script in <EXPDIR>/scripts/ for source/dot-source
        statements referencing ush scripts and verifies they exist in
        <EXPDIR>/ush/.

        Returns:
            List of (referencing_script_name, missing_ush_name) tuples.

        Traces to: Requirement 8.2
        """
        missing: list[tuple[str, str]] = []
        scripts_dir = self.expdir / "scripts"
        ush_dir = self.expdir / "ush"

        if not scripts_dir.is_dir():
            return missing

        for script_path in sorted(scripts_dir.iterdir()):
            if not script_path.is_file():
                continue
            try:
                content = script_path.read_text(encoding="utf-8", errors="replace")
            except OSError:
                continue

            for line in content.splitlines():
                stripped = line.lstrip()
                if stripped.startswith("#"):
                    continue
                for pattern in _USH_SOURCE_PATTERNS:
                    match = pattern.search(line)
                    if match:
                        ush_name = match.group("script")
                        if not (ush_dir / ush_name).exists():
                            missing.append((script_path.name, ush_name))

        return missing

    def _check_config_refs(self) -> list[tuple[str, str]]:
        """Verify J-Job → config file references resolve.

        Parses each staged J-Job for jjob_header.sh -c flags and verifies
        that the referenced config files exist somewhere under
        <EXPDIR>/parm/config/.

        Returns:
            List of (jjob_name, missing_config_name) tuples.

        Traces to: Requirement 8.3 (implied by completeness check)
        """
        from deployment.dag_filter import _JJOB_HEADER_PATTERN

        missing: list[tuple[str, str]] = []
        jobs_dir = self.expdir / "jobs"
        parm_config_dir = self.expdir / "parm" / "config"

        if not jobs_dir.is_dir():
            return missing

        # Collect all config files available in parm/config/ (recursive)
        available_configs: set[str] = set()
        if parm_config_dir.is_dir():
            for cfg_path in parm_config_dir.rglob("*"):
                if cfg_path.is_file():
                    available_configs.add(cfg_path.name)

        for jjob_path in sorted(jobs_dir.iterdir()):
            if not jjob_path.is_file():
                continue
            try:
                content = jjob_path.read_text(encoding="utf-8", errors="replace")
            except OSError:
                continue

            for match in _JJOB_HEADER_PATTERN.finditer(content):
                basenames = match.group("configs").split()
                for base in basenames:
                    # Check for config.<base>.j2 or config.<base>
                    candidates = [f"config.{base}.j2", f"config.{base}"]
                    found = any(c in available_configs for c in candidates)
                    if not found:
                        missing.append((jjob_path.name, f"config.{base}"))

        return missing

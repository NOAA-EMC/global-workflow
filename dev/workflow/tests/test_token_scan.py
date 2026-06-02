"""Unit tests for the Token_Scan (dev/workflow/deployment/token_scan.py).

Covers:
- atparse (``@[VAR]``) and Jinja2 (``{{`` / ``{%`` / ``{#``) detection;
- registry honoring — a repo runtime file with ``@[...]`` passes ONLY when its
  path is exempt (Req 3.3);
- stale-exemption detection as a warning that does NOT fail the scan (Req 3.4);
- rendered EXPDIR artifacts are NEVER exempt (Req 7.5, 9);
- ``forecast_postdet.sh`` parsing-namelists source detection (Req 1.5);
- binary files are skipped gracefully.

**Validates: Requirements 3.3, 3.4, 1.5, 2.6**

Traces to: Design Document - Component 3 (Token_Scan and
Atparse_Exemption_Registry); parent Req 4.6, Req 8, Property 14.
"""

from __future__ import annotations

import os
import sys
from pathlib import Path

import pytest
import yaml

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.token_scan import (
    ATPARSE_PATTERN,
    JINJA_PATTERNS,
    TokenScanResult,
    load_exemptions,
    scan_rendered_expdir,
    scan_repo_runtime,
)

# Repo root: tests -> workflow -> dev -> <repo root>
REPO_ROOT = Path(__file__).resolve().parents[3]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _write(path: Path, text: str) -> Path:
    """Write text to ``path`` creating parent dirs; return the path."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")
    return path


def _write_registry(path: Path, paths: list[str]) -> Path:
    """Write an Atparse_Exemption_Registry YAML listing ``paths``."""
    data = {
        "exemptions": [
            {"path": p, "justification": f"test exemption for {p}"} for p in paths
        ]
    }
    return _write(path, yaml.dump(data, sort_keys=False))


# ---------------------------------------------------------------------------
# Pattern-level detection
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "token",
    ["@[VAR]", "@[my_var]", "@[VARIABLE]", "@[_leading]", "@[A1_b2]"],
)
def test_atparse_pattern_matches_valid_tokens(token: str):
    """ATPARSE_PATTERN matches well-formed ``@[NAME]`` atparse tokens."""
    assert ATPARSE_PATTERN.search(token), f"expected match for {token!r}"


@pytest.mark.parametrize(
    "text",
    ["@[]", "@[1abc]", "@ [VAR]", "@VAR", "[VAR]", "@{VAR}"],
)
def test_atparse_pattern_rejects_non_tokens(text: str):
    """ATPARSE_PATTERN does not match malformed atparse-like text."""
    assert ATPARSE_PATTERN.search(text) is None, f"unexpected match for {text!r}"


@pytest.mark.parametrize(
    "token",
    ["{{ x }}", "{% if y %}", "{# comment #}"],
)
def test_jinja_patterns_match_openers(token: str):
    """At least one JINJA_PATTERN matches each Jinja2 opener form."""
    assert any(p.search(token) for p in JINJA_PATTERNS), token


# ---------------------------------------------------------------------------
# load_exemptions
# ---------------------------------------------------------------------------


def test_load_exemptions_reads_paths(tmp_path: Path):
    """load_exemptions returns the set of repo-relative exempt paths."""
    registry = _write_registry(
        tmp_path / "atparse_exemptions.yaml",
        ["a/b.tmpl", "ush/c.sh"],
    )
    assert load_exemptions(registry) == {"a/b.tmpl", "ush/c.sh"}


def test_load_exemptions_missing_file_returns_empty(tmp_path: Path):
    """A missing registry file yields an empty exemption set."""
    assert load_exemptions(tmp_path / "does_not_exist.yaml") == set()


def test_load_exemptions_empty_list(tmp_path: Path):
    """A registry with no entries yields an empty set."""
    registry = _write(tmp_path / "reg.yaml", "exemptions: []\n")
    assert load_exemptions(registry) == set()


# ---------------------------------------------------------------------------
# scan_repo_runtime — registry honoring (Req 3.3, 2.6)
# ---------------------------------------------------------------------------


def test_repo_runtime_unexempt_atparse_is_violation(tmp_path: Path):
    """A non-exempt runtime file with ``@[...]`` fails the scan (Req 3.3)."""
    _write(tmp_path / "ush" / "rogue.sh", "#!/bin/bash\nfoo=@[BAR]\n")
    result = scan_repo_runtime(tmp_path, registry=set())

    assert not result.passed
    paths = {p for p, _, _ in result.atparse_violations}
    assert "ush/rogue.sh" in paths


def test_repo_runtime_exempt_atparse_passes(tmp_path: Path):
    """A file with ``@[...]`` passes ONLY when its path is exempt (Req 3.3)."""
    _write(tmp_path / "ush" / "exempt.sh", "#!/bin/bash\nfoo=@[BAR]\n")
    result = scan_repo_runtime(tmp_path, registry={"ush/exempt.sh"})

    assert result.passed
    assert result.atparse_violations == []
    # The exempt file carried tokens, so it is NOT reported as stale.
    assert "ush/exempt.sh" not in result.stale_exemptions


def test_repo_runtime_mixed_exempt_and_violation(tmp_path: Path):
    """Exempt file is allowed while a sibling non-exempt file still fails."""
    _write(tmp_path / "ush" / "exempt.sh", "x=@[ONE]\n")
    _write(tmp_path / "scripts" / "bad.sh", "y=@[TWO]\n")
    result = scan_repo_runtime(tmp_path, registry={"ush/exempt.sh"})

    assert not result.passed
    paths = {p for p, _, _ in result.atparse_violations}
    assert paths == {"scripts/bad.sh"}


def test_repo_runtime_no_tokens_passes(tmp_path: Path):
    """A runtime tree with no atparse tokens and no registry passes cleanly."""
    _write(tmp_path / "ush" / "clean.sh", "#!/bin/bash\necho hello\n")
    result = scan_repo_runtime(tmp_path, registry=set())

    assert result.passed
    assert result.atparse_violations == []
    assert result.stale_exemptions == []


# ---------------------------------------------------------------------------
# scan_repo_runtime — stale exemptions (Req 3.4)
# ---------------------------------------------------------------------------


def test_stale_exemption_warns_but_passes(tmp_path: Path):
    """An exempt file with no ``@[...]`` is stale: warning only, scan passes."""
    _write(tmp_path / "ush" / "no_tokens.sh", "#!/bin/bash\necho clean\n")
    result = scan_repo_runtime(tmp_path, registry={"ush/no_tokens.sh"})

    assert result.passed  # stale exemptions do NOT fail the scan (Req 3.4)
    assert result.stale_exemptions == ["ush/no_tokens.sh"]


def test_missing_exempt_file_is_stale(tmp_path: Path):
    """A registry entry whose file does not exist is reported as stale."""
    _write(tmp_path / "ush" / "present.sh", "#!/bin/bash\necho hi\n")
    result = scan_repo_runtime(tmp_path, registry={"parm/gone.tmpl"})

    assert result.passed
    assert "parm/gone.tmpl" in result.stale_exemptions


def test_stale_and_active_exemptions_distinguished(tmp_path: Path):
    """Only the token-free exempt entry is stale; the active one is not."""
    _write(tmp_path / "ush" / "active.sh", "v=@[KEEP]\n")
    _write(tmp_path / "ush" / "stale.sh", "echo nothing\n")
    result = scan_repo_runtime(
        tmp_path, registry={"ush/active.sh", "ush/stale.sh"}
    )

    assert result.passed
    assert result.stale_exemptions == ["ush/stale.sh"]


# ---------------------------------------------------------------------------
# scan_repo_runtime — forecast_postdet parsing-source detection (Req 1.5)
# ---------------------------------------------------------------------------


def test_forecast_postdet_parsing_source_detected(tmp_path: Path):
    """Sourcing a parsing_namelists_*.sh in forecast_postdet.sh is a violation."""
    _write(
        tmp_path / "ush" / "forecast_postdet.sh",
        "#!/bin/bash\n"
        'source "${USHgfs}/parsing_namelists_WW3.sh"\n'
        "WW3_namelists\n",
    )
    result = scan_repo_runtime(tmp_path, registry=set())

    assert not result.passed
    assert result.parsing_source_violations == [
        ("ush/forecast_postdet.sh", "parsing_namelists_WW3.sh")
    ]


def test_forecast_postdet_parsing_source_comment_ignored(tmp_path: Path):
    """A commented-out parsing_namelists reference is not a violation."""
    _write(
        tmp_path / "ush" / "forecast_postdet.sh",
        "#!/bin/bash\n"
        '# legacy: source "${USHgfs}/parsing_namelists_MOM6.sh"\n'
        'cpreq "${EXPDIR}/parm/ufs/ocean/MOM_input" "${DATA}/INPUT/MOM_input"\n',
    )
    result = scan_repo_runtime(tmp_path, registry=set())

    assert result.parsing_source_violations == []
    assert result.passed


def test_real_forecast_postdet_has_no_parsing_sources():
    """The real (task-1 remediated) forecast_postdet.sh sources no parsing scripts.

    Task 1 already converted the WW3/MOM6/CICE/GOCART blocks to cpreq, so the
    live script must yield zero parsing-source violations (Req 1.5).
    """
    forecast_postdet = REPO_ROOT / "ush" / "forecast_postdet.sh"
    if not forecast_postdet.is_file():
        pytest.skip("ush/forecast_postdet.sh not present in this checkout")

    registry = load_exemptions(REPO_ROOT / "dev" / "parm" / "atparse_exemptions.yaml")
    # Scope the scan to ush/ so this unit test does not depend on the whole repo.
    result = scan_repo_runtime(REPO_ROOT, registry=registry, scan_dirs=("ush",))

    assert result.parsing_source_violations == [], (
        "forecast_postdet.sh still sources parsing_namelists_*.sh:\n"
        + "\n".join(f"  {s} -> {n}" for s, n in result.parsing_source_violations)
    )


# ---------------------------------------------------------------------------
# scan_rendered_expdir — sealed artifacts are NEVER exempt (Req 7.5, 9)
# ---------------------------------------------------------------------------


def test_expdir_clean_passes(tmp_path: Path):
    """A fully-rendered EXPDIR with no tokens passes."""
    expdir = tmp_path / "expdir"
    _write(expdir / "scripts" / "exfoo.sh", "#!/bin/bash\necho resolved value\n")
    _write(expdir / "parm" / "ufs" / "wave" / "ww3_shel.nml", "&ww3 dt=300 /\n")

    result = scan_rendered_expdir(expdir)
    assert result.passed
    assert result.atparse_violations == []
    assert result.jinja_violations == []


def test_expdir_atparse_is_violation(tmp_path: Path):
    """An ``@[...]`` token in a rendered EXPDIR file is always a violation."""
    expdir = tmp_path / "expdir"
    _write(expdir / "parm" / "ufs" / "ocean" / "MOM_input", "dt = @[DT_OCEAN]\n")

    result = scan_rendered_expdir(expdir)
    assert not result.passed
    paths = {p for p, _, _ in result.atparse_violations}
    assert "parm/ufs/ocean/MOM_input" in paths


def test_expdir_jinja_is_violation(tmp_path: Path):
    """Jinja2 tokens in a rendered EXPDIR file are violations (Req 7.5)."""
    expdir = tmp_path / "expdir"
    _write(
        expdir / "scripts" / "exbar.sh",
        "#!/bin/bash\necho {{ unresolved }}\n{% if x %}\n{# note #}\n",
    )

    result = scan_rendered_expdir(expdir)
    assert not result.passed
    tokens = {t for _, _, t in result.jinja_violations}
    assert {"{{", "{%", "{#"} <= tokens


def test_expdir_registry_does_not_exempt_artifacts(tmp_path: Path):
    """The registry does NOT exempt EXPDIR artifacts even at an exempt path.

    A path that would be exempt for repo runtime (e.g. an exgfs_wave_nawips
    rendition) must still fail when it carries ``@[...]`` inside a sealed
    EXPDIR — scan_rendered_expdir takes no registry (Req 7.5, 9).
    """
    expdir = tmp_path / "expdir"
    # Same basename/path shape as a registry-exempt repo file:
    _write(expdir / "scripts" / "exgfs_wave_nawips.sh", "g=@[GRID]\n")

    result = scan_rendered_expdir(expdir)
    assert not result.passed
    paths = {p for p, _, _ in result.atparse_violations}
    assert "scripts/exgfs_wave_nawips.sh" in paths


def test_expdir_excludes_non_rendered_metadata(tmp_path: Path):
    """Non-rendered metadata (ecf defs, workflow config, manifest) is skipped.

    These legitimately contain ecFlow ``{{ VAR }}`` references that ecFlow
    resolves at runtime, so they must not be flagged.
    """
    expdir = tmp_path / "expdir"
    _write(expdir / "ecf" / "defs" / "suite.def", "edit ECF_HOME {{ EXPDIR }}\n")
    _write(expdir / "parm" / "workflow" / "config.yaml", "ecf_home: {{ EXPDIR }}\n")
    _write(expdir / "manifest.yaml", "rendered: {{ count }}\n")

    result = scan_rendered_expdir(expdir)
    assert result.passed
    assert result.jinja_violations == []


# ---------------------------------------------------------------------------
# Binary-safety
# ---------------------------------------------------------------------------


def test_binary_files_skipped_gracefully(tmp_path: Path):
    """Undecodable/binary files are skipped without raising (binary-safe)."""
    expdir = tmp_path / "expdir"
    expdir.mkdir(parents=True)
    # A binary blob that happens to contain an atparse-looking byte sequence.
    (expdir / "data.bin").write_bytes(b"\x00\x01@[VAR]\xff\xfe")
    _write(expdir / "scripts" / "exok.sh", "#!/bin/bash\necho ok\n")

    result = scan_rendered_expdir(expdir)  # must not raise
    assert result.passed
    # The binary file's token must NOT be counted.
    assert result.atparse_violations == []


# ---------------------------------------------------------------------------
# TokenScanResult.passed semantics
# ---------------------------------------------------------------------------


def test_result_passed_true_when_only_stale():
    """passed is True when only stale_exemptions are present (Req 3.4)."""
    result = TokenScanResult(stale_exemptions=["parm/old.tmpl"])
    assert result.passed


def test_result_passed_false_on_any_hard_violation():
    """Any atparse/jinja/parsing-source violation makes passed False."""
    assert not TokenScanResult(
        atparse_violations=[("f", 1, "@[X]")]
    ).passed
    assert not TokenScanResult(jinja_violations=[("f", 1, "{{")]).passed
    assert not TokenScanResult(
        parsing_source_violations=[("ush/forecast_postdet.sh", "parsing_namelists_WW3.sh")]
    ).passed

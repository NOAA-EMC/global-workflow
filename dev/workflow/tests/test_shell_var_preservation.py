"""Test: Shell variable preservation during Jinja2 rendering.

Verifies that the Template_Renderer preserves ${VAR} shell-style references
verbatim while resolving Jinja2 {{ var }} expressions at deployment time.

**Validates: Requirements 4.10**

Traces to: Requirements 4.10
  "WHERE the Template_Renderer encounters ${VAR} shell-style references inside
   a Jinja2 string value, THE Template_Renderer SHALL preserve them verbatim
   and SHALL leave their expansion to the runtime shell."
"""

from __future__ import annotations

import os
import sys
import tempfile
from pathlib import Path

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from deployment.template_renderer import (
    TemplateRenderer,
    _protect_shell_vars,
    _restore_shell_vars,
)


class TestShellVariablePreservation:
    """Tests that shell variables (${VAR}) survive Jinja2 rendering intact."""

    def setup_method(self):
        """Set up a TemplateRenderer with a basic context and temp directory."""
        self.context = {
            "NET": "gfs",
            "RUN": "gdas",
            "model_ver": "v17.0.0",
            "MACHINE": "hera",
            "HOMEgfs": "/apps/ops/gfs",
        }
        self.tmpdir = tempfile.mkdtemp()
        self.renderer = TemplateRenderer(
            context=self.context,
            searchpath=[self.tmpdir],
            strict=True,
        )

    def test_shell_vars_preserved_jinja2_resolved(self):
        """Mixed template: Jinja2 vars resolved, shell vars preserved verbatim."""
        template = (
            "export NET={{ NET }}\n"
            "export RUN={{ RUN }}\n"
            "export COMOUT=${COMOUT}/${NET}.${PDY}/${cyc}/atmos\n"
            "export DATA=${DATAROOT}/${jobid}\n"
        )

        result = self.renderer.render_string(template)

        # Jinja2 variables should be resolved
        assert "export NET=gfs" in result
        assert "export RUN=gdas" in result
        # Shell variables should remain as literal ${VAR}
        assert "${COMOUT}" in result
        assert "${PDY}" in result
        assert "${cyc}" in result
        assert "${DATAROOT}" in result
        assert "${jobid}" in result
        # The full shell expression lines should be intact
        assert "${COMOUT}/${NET}.${PDY}/${cyc}/atmos" in result
        assert "${DATAROOT}/${jobid}" in result

    def test_shell_vars_only_template(self):
        """Template with only shell variables — all preserved verbatim."""
        template = (
            "cd ${DATA}\n"
            "cp ${COMOUT}/file.nc ${DATA}/input.nc\n"
            "export pgmout=${DATA}/${pgmout:-/dev/null}\n"
        )

        result = self.renderer.render_string(template)

        assert "${DATA}" in result
        assert "${COMOUT}" in result
        assert result == template  # Nothing should change

    def test_jinja2_only_template(self):
        """Template with only Jinja2 variables — all resolved."""
        template = "version={{ model_ver }}\nmachine={{ MACHINE }}\n"

        result = self.renderer.render_string(template)

        assert result == "version=v17.0.0\nmachine=hera\n"
        assert "{{" not in result

    def test_multiple_shell_vars_same_line(self):
        """Multiple shell variables on the same line are all preserved."""
        template = "path=${COMOUT}/${NET}.${PDY}/${cyc}/atmos\n"

        result = self.renderer.render_string(template)

        assert result == template

    def test_shell_var_pattern_uppercase_with_underscores(self):
        """Shell vars matching [A-Z_][A-Z0-9_]* pattern are preserved."""
        template = (
            "a=${HOME_DIR}\n"
            "b=${DATA_ROOT_2}\n"
            "c=${_PRIVATE}\n"
            "d=${A}\n"
        )

        result = self.renderer.render_string(template)

        assert "${HOME_DIR}" in result
        assert "${DATA_ROOT_2}" in result
        assert "${_PRIVATE}" in result
        assert "${A}" in result

    def test_shell_vars_adjacent_to_jinja2(self):
        """Shell vars and Jinja2 vars can appear adjacent in the same line."""
        template = "output={{ HOMEgfs }}/com/${NET}.${PDY}\n"

        result = self.renderer.render_string(template)

        # Jinja2 resolved
        assert "/apps/ops/gfs/com/" in result
        # Shell vars preserved
        assert "${NET}" in result
        assert "${PDY}" in result

    def test_render_file_preserves_shell_vars(self):
        """render_file() also preserves shell variables in the output file."""
        template_content = (
            "#!/bin/bash\n"
            "export HOMEgfs={{ HOMEgfs }}\n"
            "export COMOUT=${COMOUT}/${NET}.${PDY}/${cyc}/atmos\n"
            "export DATA=${DATAROOT}/${jobid}\n"
        )

        src = Path(self.tmpdir) / "test_template.sh"
        dst = Path(self.tmpdir) / "rendered_output.sh"
        src.write_text(template_content, encoding="utf-8")

        self.renderer.render_file(src, dst)

        rendered = dst.read_text(encoding="utf-8")
        # Jinja2 resolved
        assert "export HOMEgfs=/apps/ops/gfs" in rendered
        # Shell vars preserved
        assert "${COMOUT}" in rendered
        assert "${DATAROOT}" in rendered
        assert "${jobid}" in rendered
        assert "${NET}" in rendered
        assert "${PDY}" in rendered
        assert "${cyc}" in rendered


class TestProtectRestoreHelpers:
    """Unit tests for the _protect_shell_vars and _restore_shell_vars helpers."""

    def test_protect_replaces_shell_vars_with_placeholders(self):
        """_protect_shell_vars replaces ${VAR} with unique placeholders."""
        text = "hello ${WORLD} and ${FOO}"
        protected, replacements = _protect_shell_vars(text)

        assert "${WORLD}" not in protected
        assert "${FOO}" not in protected
        assert len(replacements) == 2

    def test_restore_reverses_protection(self):
        """_restore_shell_vars restores original ${VAR} from placeholders."""
        text = "hello ${WORLD} and ${FOO}"
        protected, replacements = _protect_shell_vars(text)
        restored = _restore_shell_vars(protected, replacements)

        assert restored == text

    def test_protect_ignores_lowercase_vars(self):
        """${lowercase} patterns are NOT matched (only uppercase)."""
        text = "hello ${lowercase} world"
        protected, replacements = _protect_shell_vars(text)

        # lowercase vars should not be protected
        assert len(replacements) == 0
        assert protected == text

    def test_protect_empty_string(self):
        """Empty string produces no replacements."""
        protected, replacements = _protect_shell_vars("")
        assert protected == ""
        assert replacements == {}

    def test_roundtrip_preserves_all_shell_vars(self):
        """Protect then restore is identity for any text with shell vars."""
        text = "${A} ${B_C} ${D2E} some text ${_UNDER}"
        protected, replacements = _protect_shell_vars(text)
        restored = _restore_shell_vars(protected, replacements)
        assert restored == text

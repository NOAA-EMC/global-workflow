"""Unit tests for strict undefined variable detection in TemplateRenderer.

Verifies that when a template references an undefined variable in strict mode,
a TemplateRenderError is raised with the correct file path, line number, and
variable name, formatted as a FATAL ERROR.

Traces to: Requirements 4.4
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from pathlib import Path

from deployment.template_renderer import TemplateRenderer, TemplateRenderError


class TestStrictUndefinedDetection:
    """Tests for strict undefined variable detection (Requirement 4.4)."""

    def test_undefined_variable_raises_template_render_error(self, tmp_path):
        """Rendering a template with an undefined variable raises TemplateRenderError."""
        # Create a template that references an undefined variable
        src = tmp_path / "config.yaml.j2"
        src.write_text("name: {{ app_name }}\nversion: {{ undefined_var }}\n")

        dst = tmp_path / "output" / "config.yaml"

        renderer = TemplateRenderer(
            context={"app_name": "gfs"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        err = exc_info.value
        assert err.variable == "undefined_var"
        assert err.file == str(src)
        assert err.line == 2
        assert "FATAL ERROR" in err.message

    def test_undefined_variable_includes_file_path(self, tmp_path):
        """Error message includes the file path of the template."""
        src = tmp_path / "subdir" / "template.conf.j2"
        src.parent.mkdir(parents=True, exist_ok=True)
        src.write_text("host: {{ hostname }}\n")

        dst = tmp_path / "output" / "template.conf"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        err = exc_info.value
        assert str(src) in err.message
        assert err.file == str(src)

    def test_undefined_variable_includes_line_number(self, tmp_path):
        """Error includes the line number where the undefined variable appears."""
        # Put the undefined variable on line 4
        template_content = (
            "line1: value1\n"
            "line2: value2\n"
            "line3: value3\n"
            "line4: {{ missing_var }}\n"
        )
        src = tmp_path / "multi_line.yaml.j2"
        src.write_text(template_content)

        dst = tmp_path / "output" / "multi_line.yaml"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        err = exc_info.value
        assert err.variable == "missing_var"
        assert err.line == 4

    def test_undefined_variable_includes_variable_name(self, tmp_path):
        """Error includes the exact name of the undefined variable."""
        src = tmp_path / "test.j2"
        src.write_text("value: {{ my_special_variable }}\n")

        dst = tmp_path / "output" / "test.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        err = exc_info.value
        assert err.variable == "my_special_variable"

    def test_strict_mode_disabled_allows_undefined(self, tmp_path):
        """With strict=False, undefined variables render as empty strings."""
        src = tmp_path / "lenient.j2"
        src.write_text("value: {{ undefined_var }}\n")

        dst = tmp_path / "output" / "lenient.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=False,
        )

        # Should not raise
        renderer.render_file(src, dst)
        assert dst.exists()

    def test_render_string_undefined_raises_error(self, tmp_path):
        """render_string also raises TemplateRenderError on undefined variables."""
        renderer = TemplateRenderer(
            context={"defined_var": "hello"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_string("{{ defined_var }} and {{ not_defined }}")

        err = exc_info.value
        assert err.variable == "not_defined"
        assert "FATAL ERROR" in err.message

    def test_all_variables_defined_renders_successfully(self, tmp_path):
        """When all variables are defined, rendering succeeds without error."""
        src = tmp_path / "complete.yaml.j2"
        src.write_text("name: {{ app_name }}\nversion: {{ version }}\n")

        dst = tmp_path / "output" / "complete.yaml"

        renderer = TemplateRenderer(
            context={"app_name": "gfs", "version": "17.0.0"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "gfs" in content
        assert "17.0.0" in content

    def test_fatal_error_format_matches_design(self, tmp_path):
        """Error message format matches design: FATAL ERROR: [file:line] message."""
        src = tmp_path / "format_test.j2"
        src.write_text("{{ bad_var }}\n")

        dst = tmp_path / "output" / "format_test.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        err = exc_info.value
        # Design specifies: FATAL ERROR: "Undefined variable 'VAR' in FILE:LINE"
        assert "FATAL ERROR" in err.message
        assert str(src) in err.message
        assert "Undefined variable" in err.message
        assert "'bad_var'" in err.message

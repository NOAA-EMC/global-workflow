"""Comprehensive unit tests for Template_Renderer.

Covers:
- Nested includes ({% include %})
- Template inheritance ({% extends %} / {% block %})
- render_tree() — renders all .j2 files in a directory tree, stripping .j2 suffix
- Unresolved token detection (_verify_no_unresolved)
- TemplateRenderer.create() factory method

**Validates: Requirements 4.1, 4.2, 4.3, 4.4, 4.10**
"""

from __future__ import annotations

import os
import sys

import pytest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from pathlib import Path

from deployment.template_renderer import TemplateRenderer, TemplateRenderError


# ---------------------------------------------------------------------------
# Nested Includes (Requirement 4.2)
# ---------------------------------------------------------------------------


class TestNestedIncludes:
    """Tests for nested {% include %} resolution via searchpath."""

    def test_include_resolves_child_template(self, tmp_path):
        """A parent template can include a child template from the searchpath."""
        # Create child template in searchpath
        child = tmp_path / "partials" / "db_config.j2"
        child.parent.mkdir(parents=True)
        child.write_text("host: {{ db_host }}\nport: {{ db_port }}\n")

        # Create parent template that includes the child
        src = tmp_path / "main_config.yaml.j2"
        src.write_text(
            "app: {{ app_name }}\n"
            "database:\n"
            "{% include 'partials/db_config.j2' %}"
        )

        dst = tmp_path / "output" / "main_config.yaml"

        renderer = TemplateRenderer(
            context={"app_name": "gfs", "db_host": "localhost", "db_port": "5432"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "app: gfs" in content
        assert "host: localhost" in content
        assert "port: 5432" in content

    def test_nested_include_chain(self, tmp_path):
        """Template A includes B, which includes C (multi-level nesting)."""
        # C — leaf template
        c_file = tmp_path / "level_c.j2"
        c_file.write_text("leaf_value: {{ leaf }}\n")

        # B — includes C
        b_file = tmp_path / "level_b.j2"
        b_file.write_text("mid_value: {{ mid }}\n{% include 'level_c.j2' %}")

        # A — includes B
        src = tmp_path / "level_a.j2"
        src.write_text("top_value: {{ top }}\n{% include 'level_b.j2' %}")

        dst = tmp_path / "output" / "result.txt"

        renderer = TemplateRenderer(
            context={"top": "alpha", "mid": "beta", "leaf": "gamma"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "top_value: alpha" in content
        assert "mid_value: beta" in content
        assert "leaf_value: gamma" in content

    def test_include_from_multiple_searchpath_dirs(self, tmp_path):
        """Includes resolve from multiple directories in the searchpath."""
        # Create two searchpath directories
        dir_a = tmp_path / "dir_a"
        dir_b = tmp_path / "dir_b"
        dir_a.mkdir()
        dir_b.mkdir()

        # Put different includes in each directory
        (dir_a / "header.j2").write_text("# Header from dir_a\n")
        (dir_b / "footer.j2").write_text("# Footer from dir_b\n")

        # Main template includes from both
        src = tmp_path / "combined.j2"
        src.write_text("{% include 'header.j2' %}body: {{ content }}\n{% include 'footer.j2' %}")

        dst = tmp_path / "output" / "combined.txt"

        renderer = TemplateRenderer(
            context={"content": "hello"},
            searchpath=[str(dir_a), str(dir_b)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "Header from dir_a" in content
        assert "body: hello" in content
        assert "Footer from dir_b" in content

    def test_include_not_found_raises_error(self, tmp_path):
        """Including a non-existent template raises TemplateRenderError."""
        src = tmp_path / "broken.j2"
        src.write_text("{% include 'nonexistent.j2' %}")

        dst = tmp_path / "output" / "broken.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        assert "not found" in exc_info.value.message.lower() or "nonexistent" in exc_info.value.message


# ---------------------------------------------------------------------------
# Template Inheritance (Requirement 4.3)
# ---------------------------------------------------------------------------


class TestTemplateInheritance:
    """Tests for {% extends %} and {% block %} template inheritance."""

    def test_child_extends_base_template(self, tmp_path):
        """A child template can extend a base template and override blocks."""
        # Base template with blocks
        base = tmp_path / "base_config.j2"
        base.write_text(
            "# Base Configuration\n"
            "{% block header %}default_header{% endblock %}\n"
            "common_setting: {{ common }}\n"
            "{% block body %}default_body{% endblock %}\n"
        )

        # Child template that extends base
        src = tmp_path / "child_config.j2"
        src.write_text(
            "{% extends 'base_config.j2' %}\n"
            "{% block header %}custom_header_for_{{ app }}{% endblock %}\n"
            "{% block body %}app_specific_body{% endblock %}\n"
        )

        dst = tmp_path / "output" / "child_config.txt"

        renderer = TemplateRenderer(
            context={"common": "shared_value", "app": "gfs"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "custom_header_for_gfs" in content
        assert "common_setting: shared_value" in content
        assert "app_specific_body" in content
        # Default values should NOT appear since they were overridden
        assert "default_header" not in content
        assert "default_body" not in content

    def test_inheritance_uses_default_block_when_not_overridden(self, tmp_path):
        """Blocks not overridden in child use the base template's default."""
        base = tmp_path / "base.j2"
        base.write_text(
            "{% block section_a %}default_a{% endblock %}\n"
            "{% block section_b %}default_b{% endblock %}\n"
        )

        # Child only overrides section_a
        src = tmp_path / "partial_child.j2"
        src.write_text(
            "{% extends 'base.j2' %}\n"
            "{% block section_a %}overridden_a{% endblock %}\n"
        )

        dst = tmp_path / "output" / "partial.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "overridden_a" in content
        assert "default_b" in content

    def test_multi_level_inheritance(self, tmp_path):
        """Three-level inheritance: grandchild extends child extends base."""
        base = tmp_path / "grandparent.j2"
        base.write_text("{% block content %}base_content{% endblock %}\n")

        child = tmp_path / "parent.j2"
        child.write_text(
            "{% extends 'grandparent.j2' %}\n"
            "{% block content %}parent_content{% endblock %}\n"
        )

        src = tmp_path / "grandchild.j2"
        src.write_text(
            "{% extends 'parent.j2' %}\n"
            "{% block content %}grandchild_content{% endblock %}\n"
        )

        dst = tmp_path / "output" / "result.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "grandchild_content" in content
        assert "parent_content" not in content
        assert "base_content" not in content


# ---------------------------------------------------------------------------
# render_tree() (Requirement 4.5 — renders all .j2 files in a tree)
# ---------------------------------------------------------------------------


class TestRenderTree:
    """Tests for render_tree() — batch rendering of .j2 files in a directory."""

    def test_renders_all_j2_files_stripping_suffix(self, tmp_path):
        """render_tree renders every .j2 file and strips the .j2 extension."""
        src_dir = tmp_path / "templates"
        src_dir.mkdir()

        (src_dir / "config.yaml.j2").write_text("name: {{ name }}\n")
        (src_dir / "settings.conf.j2").write_text("mode: {{ mode }}\n")

        dst_dir = tmp_path / "rendered"

        renderer = TemplateRenderer(
            context={"name": "gfs", "mode": "cycled"},
            searchpath=[str(src_dir)],
            strict=True,
        )

        rendered_files = renderer.render_tree(src_dir, dst_dir)

        assert len(rendered_files) == 2
        # Check output files exist without .j2 suffix
        assert (dst_dir / "config.yaml").exists()
        assert (dst_dir / "settings.conf").exists()
        # Check content
        assert "name: gfs" in (dst_dir / "config.yaml").read_text()
        assert "mode: cycled" in (dst_dir / "settings.conf").read_text()

    def test_render_tree_preserves_subdirectory_structure(self, tmp_path):
        """render_tree preserves the relative directory structure."""
        src_dir = tmp_path / "templates"
        (src_dir / "sub" / "deep").mkdir(parents=True)

        (src_dir / "top.txt.j2").write_text("top: {{ val }}\n")
        (src_dir / "sub" / "mid.txt.j2").write_text("mid: {{ val }}\n")
        (src_dir / "sub" / "deep" / "bottom.txt.j2").write_text("bottom: {{ val }}\n")

        dst_dir = tmp_path / "output"

        renderer = TemplateRenderer(
            context={"val": "ok"},
            searchpath=[str(src_dir)],
            strict=True,
        )

        rendered_files = renderer.render_tree(src_dir, dst_dir)

        assert len(rendered_files) == 3
        assert (dst_dir / "top.txt").exists()
        assert (dst_dir / "sub" / "mid.txt").exists()
        assert (dst_dir / "sub" / "deep" / "bottom.txt").exists()

    def test_render_tree_ignores_non_j2_files(self, tmp_path):
        """render_tree only processes .j2 files; other files are ignored."""
        src_dir = tmp_path / "templates"
        src_dir.mkdir()

        (src_dir / "template.yaml.j2").write_text("key: {{ val }}\n")
        (src_dir / "readme.md").write_text("# Not a template\n")
        (src_dir / "data.json").write_text('{"not": "rendered"}\n')

        dst_dir = tmp_path / "output"

        renderer = TemplateRenderer(
            context={"val": "rendered"},
            searchpath=[str(src_dir)],
            strict=True,
        )

        rendered_files = renderer.render_tree(src_dir, dst_dir)

        assert len(rendered_files) == 1
        assert (dst_dir / "template.yaml").exists()
        assert not (dst_dir / "readme.md").exists()
        assert not (dst_dir / "data.json").exists()

    def test_render_tree_returns_list_of_output_paths(self, tmp_path):
        """render_tree returns a list of Path objects for rendered files."""
        src_dir = tmp_path / "src"
        src_dir.mkdir()
        (src_dir / "a.txt.j2").write_text("a\n")
        (src_dir / "b.txt.j2").write_text("b\n")

        dst_dir = tmp_path / "dst"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(src_dir)],
            strict=True,
        )

        rendered_files = renderer.render_tree(src_dir, dst_dir)

        assert all(isinstance(p, Path) for p in rendered_files)
        assert all(p.exists() for p in rendered_files)

    def test_render_tree_empty_directory(self, tmp_path):
        """render_tree on an empty directory returns an empty list."""
        src_dir = tmp_path / "empty"
        src_dir.mkdir()
        dst_dir = tmp_path / "output"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(src_dir)],
            strict=True,
        )

        rendered_files = renderer.render_tree(src_dir, dst_dir)
        assert rendered_files == []


# ---------------------------------------------------------------------------
# Unresolved Token Detection (Requirement 4.6)
# ---------------------------------------------------------------------------


class TestUnresolvedTokenDetection:
    """Tests for _verify_no_unresolved catching leftover Jinja2 tokens."""

    def test_leftover_double_braces_raises_error(self, tmp_path):
        """A rendered file containing {{ raises TemplateRenderError."""
        src = tmp_path / "bad.txt.j2"
        # Use a raw block to sneak {{ through rendering
        src.write_text("{% raw %}leftover: {{ not_resolved }}{% endraw %}\n")

        dst = tmp_path / "output" / "bad.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        assert "Unresolved" in exc_info.value.message or "{{" in exc_info.value.message

    def test_leftover_block_tag_raises_error(self, tmp_path):
        """A rendered file containing {% raises TemplateRenderError."""
        src = tmp_path / "bad_block.txt.j2"
        src.write_text("{% raw %}{% if something %}{% endraw %}\n")

        dst = tmp_path / "output" / "bad_block.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        assert "Unresolved" in exc_info.value.message or "{%" in exc_info.value.message

    def test_leftover_comment_tag_raises_error(self, tmp_path):
        """A rendered file containing {# raises TemplateRenderError."""
        src = tmp_path / "bad_comment.txt.j2"
        src.write_text("{% raw %}{# leftover comment #}{% endraw %}\n")

        dst = tmp_path / "output" / "bad_comment.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        assert "Unresolved" in exc_info.value.message or "{#" in exc_info.value.message

    def test_clean_rendered_file_passes_verification(self, tmp_path):
        """A properly rendered file with no leftover tokens passes without error."""
        src = tmp_path / "clean.txt.j2"
        src.write_text("name: {{ name }}\nversion: {{ version }}\n")

        dst = tmp_path / "output" / "clean.txt"

        renderer = TemplateRenderer(
            context={"name": "gfs", "version": "17.0"},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        # Should not raise
        renderer.render_file(src, dst)
        content = dst.read_text()
        assert "{{" not in content
        assert "{%" not in content
        assert "{#" not in content

    def test_unresolved_token_reports_line_number(self, tmp_path):
        """The error for unresolved tokens includes the line number."""
        src = tmp_path / "multiline.txt.j2"
        src.write_text(
            "line1: ok\n"
            "line2: ok\n"
            "{% raw %}line3: {{ leftover }}{% endraw %}\n"
        )

        dst = tmp_path / "output" / "multiline.txt"

        renderer = TemplateRenderer(
            context={},
            searchpath=[str(tmp_path)],
            strict=True,
        )

        with pytest.raises(TemplateRenderError) as exc_info:
            renderer.render_file(src, dst)

        err = exc_info.value
        assert err.line == 3


# ---------------------------------------------------------------------------
# TemplateRenderer.create() Factory (Requirement 4.2 — searchpath)
# ---------------------------------------------------------------------------


class TestCreateFactory:
    """Tests for TemplateRenderer.create() factory method."""

    def test_create_builds_correct_searchpath(self, tmp_path):
        """create() builds searchpath: [config/<app>/, config/, parm/, workflow/]."""
        dev_root = tmp_path / "dev"
        # Create the expected directory structure
        (dev_root / "parm" / "config" / "gfs").mkdir(parents=True)
        (dev_root / "parm" / "config").mkdir(parents=True, exist_ok=True)
        (dev_root / "parm").mkdir(parents=True, exist_ok=True)
        (dev_root / "workflow").mkdir(parents=True)

        renderer = TemplateRenderer.create(
            context={"NET": "gfs"},
            dev_root=str(dev_root),
            app="gfs",
            strict=True,
        )

        expected = [
            str(dev_root / "parm" / "config" / "gfs"),
            str(dev_root / "parm" / "config"),
            str(dev_root / "parm"),
            str(dev_root / "workflow"),
        ]
        assert renderer.searchpath == expected

    def test_create_with_different_app(self, tmp_path):
        """create() uses the app parameter to build the app-specific path."""
        dev_root = tmp_path / "dev"
        (dev_root / "parm" / "config" / "gefs").mkdir(parents=True)
        (dev_root / "parm" / "config").mkdir(parents=True, exist_ok=True)
        (dev_root / "parm").mkdir(parents=True, exist_ok=True)
        (dev_root / "workflow").mkdir(parents=True)

        renderer = TemplateRenderer.create(
            context={},
            dev_root=str(dev_root),
            app="gefs",
            strict=True,
        )

        assert str(dev_root / "parm" / "config" / "gefs") in renderer.searchpath
        # gfs should NOT be in the path
        assert str(dev_root / "parm" / "config" / "gfs") not in renderer.searchpath

    def test_create_filters_nonexistent_directories(self, tmp_path):
        """create() only includes directories that actually exist."""
        dev_root = tmp_path / "dev"
        # Only create parm/ and workflow/, skip config/<app>/
        (dev_root / "parm").mkdir(parents=True)
        (dev_root / "workflow").mkdir(parents=True)

        renderer = TemplateRenderer.create(
            context={},
            dev_root=str(dev_root),
            app="gfs",
            strict=True,
        )

        # config/gfs/ and config/ don't exist, so they should be filtered out
        assert str(dev_root / "parm" / "config" / "gfs") not in renderer.searchpath
        assert str(dev_root / "parm" / "config") not in renderer.searchpath
        # These exist
        assert str(dev_root / "parm") in renderer.searchpath
        assert str(dev_root / "workflow") in renderer.searchpath

    def test_create_passes_context_and_strict(self, tmp_path):
        """create() correctly passes context and strict parameters."""
        dev_root = tmp_path / "dev"
        (dev_root / "parm").mkdir(parents=True)
        (dev_root / "workflow").mkdir(parents=True)

        context = {"NET": "gfs", "RUN": "gdas", "MACHINE": "hera"}
        renderer = TemplateRenderer.create(
            context=context,
            dev_root=str(dev_root),
            app="gfs",
            strict=False,
        )

        assert renderer.context == context
        assert renderer.strict is False

    def test_create_accepts_path_object(self, tmp_path):
        """create() accepts a Path object for dev_root."""
        dev_root = tmp_path / "dev"
        (dev_root / "parm" / "config" / "gfs").mkdir(parents=True)
        (dev_root / "parm" / "config").mkdir(parents=True, exist_ok=True)
        (dev_root / "parm").mkdir(parents=True, exist_ok=True)
        (dev_root / "workflow").mkdir(parents=True)

        # Pass as Path object (not string)
        renderer = TemplateRenderer.create(
            context={},
            dev_root=dev_root,
            app="gfs",
        )

        assert len(renderer.searchpath) == 4
        assert all(isinstance(p, str) for p in renderer.searchpath)

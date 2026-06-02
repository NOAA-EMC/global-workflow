"""Template renderer wrapping wxflow parse_j2yaml.

Provides strict undefined variable detection, shell variable preservation,
nested includes, and template inheritance support.

Since wxflow may not be available in all environments, this module implements
the same interface using Jinja2 directly (which wxflow wraps internally).

Traces to: Requirements 4.1, 4.2, 4.3, 9.1
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any

import yaml
from jinja2 import (
    Environment,
    FileSystemLoader,
    StrictUndefined,
    TemplateNotFound,
    TemplateSyntaxError,
    Undefined,
    UndefinedError,
)

# Pattern matching shell variables: ${VAR_NAME}
# These must be preserved verbatim for runtime shell expansion.
_SHELL_VAR_PATTERN = re.compile(r'\$\{[A-Z_][A-Z0-9_]*\}')

# Unique placeholder prefix used to protect shell variables from Jinja2
_SHELL_VAR_PLACEHOLDER = "__SHELL_VAR_PRESERVE__"


def fortran_logical(value: Any) -> str:
    """Jinja2 filter that converts a Python boolean to Fortran logical literal.

    Converts truthy values to '.true.' and falsy values to '.false.'.
    Used in UFS model configuration templates (model_configure, input.nml)
    where Fortran logical syntax is required.

    Args:
        value: A Python value to convert. Truthy -> '.true.', falsy -> '.false.'.

    Returns:
        '.true.' or '.false.' as a string.
    """
    return ".true." if value else ".false."


class TemplateRenderError(Exception):
    """Raised when template rendering fails.

    Attributes:
        file: Path to the template file that caused the error.
        line: Line number where the error occurred (if available).
        variable: Name of the undefined variable (if applicable).
        message: Human-readable error description.
    """

    def __init__(
        self,
        message: str,
        file: str | None = None,
        line: int | None = None,
        variable: str | None = None,
    ) -> None:
        self.file = file
        self.line = line
        self.variable = variable
        parts = ["FATAL ERROR:"]
        if file:
            loc = file
            if line is not None:
                loc += f":{line}"
            parts.append(f"[{loc}]")
        parts.append(message)
        self.message = " ".join(parts)
        super().__init__(self.message)


def _protect_shell_vars(text: str) -> tuple[str, dict[str, str]]:
    """Replace ${VAR} shell patterns with placeholders to prevent Jinja2 resolution.

    Returns the modified text and a mapping of placeholder -> original value.
    """
    replacements: dict[str, str] = {}
    counter = 0

    def _replace(match: re.Match) -> str:
        nonlocal counter
        placeholder = f"{_SHELL_VAR_PLACEHOLDER}{counter}__"
        replacements[placeholder] = match.group(0)
        counter += 1
        return placeholder

    protected = _SHELL_VAR_PATTERN.sub(_replace, text)
    return protected, replacements


def _restore_shell_vars(text: str, replacements: dict[str, str]) -> str:
    """Restore shell variable placeholders back to their original ${VAR} form."""
    result = text
    for placeholder, original in replacements.items():
        result = result.replace(placeholder, original)
    return result


class TemplateRenderer:
    """Renders Jinja2-templated files with strict undefined detection.

    Wraps Jinja2 with:
    - Configurable searchpath for includes and template inheritance
    - StrictUndefined mode (raises on undefined variables)
    - Shell variable preservation (${VAR} patterns left for runtime)
    - YAML round-trip support for config files

    The searchpath should be ordered most-specific first:
        [dev/parm/config/<app>/, dev/parm/config/, dev/parm/, dev/workflow/]

    This ensures that app-specific templates override general ones when
    using {% include %} or {% extends %}.

    Args:
        context: Deployment-time Jinja2 context dictionary containing
            variables like NET, RUN, MODE, MACHINE, model_ver, EXPDIR, etc.
        searchpath: List of directory paths to search for included/inherited
            templates. Most specific first (e.g. app-specific before general).
        strict: If True, use StrictUndefined to raise on undefined variables.
            Equivalent to wxflow's allow_missing=False. Defaults to True.
    """

    def __init__(
        self,
        context: dict[str, Any],
        searchpath: list[str],
        strict: bool = True,
    ) -> None:
        self.context = context
        self.searchpath = [str(p) for p in searchpath]
        self.strict = strict

        # Build the Jinja2 environment with FileSystemLoader for searchpath support.
        # FileSystemLoader enables {% include %} and {% extends %} to find
        # templates in any of the searchpath directories.
        undefined_cls = StrictUndefined if strict else Undefined
        self._env = Environment(
            loader=FileSystemLoader(self.searchpath, followlinks=True),
            undefined=undefined_cls,
            keep_trailing_newline=True,
            # Use default Jinja2 delimiters for {{ }}, {% %}, {# #}
        )

        # Register custom filters for UFS model configuration rendering
        self._env.filters["fortran_logical"] = fortran_logical

    @classmethod
    def create(
        cls,
        context: dict[str, Any],
        dev_root: str | Path,
        app: str = "gfs",
        strict: bool = True,
    ) -> "TemplateRenderer":
        """Factory method that builds the standard deployment searchpath.

        Constructs the searchpath as specified in the design:
            [dev/parm/config/<app>/, dev/parm/config/, dev/parm/, dev/workflow/]

        Args:
            context: Deployment-time Jinja2 context dictionary.
            dev_root: Path to the dev/ directory root.
            app: Application name (e.g. 'gfs', 'gefs', 'sfs'). Defaults to 'gfs'.
            strict: If True, use StrictUndefined. Defaults to True.

        Returns:
            A configured TemplateRenderer instance.
        """
        dev_root = Path(dev_root)
        searchpath = [
            str(dev_root / "parm" / "config" / app),
            str(dev_root / "parm" / "config"),
            str(dev_root / "parm"),
            str(dev_root / "workflow"),
        ]
        # Filter to only existing directories to avoid FileSystemLoader warnings
        searchpath = [p for p in searchpath if Path(p).is_dir()]
        return cls(context=context, searchpath=searchpath, strict=strict)

    def render_file(self, src: Path, dst: Path) -> None:
        """Render a single Jinja2 template file and write the result to dst.

        For YAML files (.yaml, .yml), the rendered output is parsed and
        re-serialized to ensure valid YAML. For all other files, the
        rendered text is written directly.

        Shell variable patterns (${VAR}) are preserved verbatim and not
        resolved by Jinja2.

        After rendering, verifies no unresolved Jinja2 tokens remain.

        Args:
            src: Path to the source template file.
            dst: Path where the rendered output will be written.

        Raises:
            TemplateRenderError: If strict mode is enabled and an
                undefined variable is encountered, or if unresolved
                tokens remain after rendering.
        """
        src = Path(src)
        dst = Path(dst)

        # Read the source template
        try:
            template_text = src.read_text(encoding="utf-8")
        except FileNotFoundError:
            raise TemplateRenderError(
                f"Template file not found: {src}",
                file=str(src),
            )

        # Protect shell variables from Jinja2 resolution
        protected_text, shell_replacements = _protect_shell_vars(template_text)

        # Render the template using Jinja2
        # Use from_string so we can handle templates that aren't in the searchpath
        # but still allow {% extends %} and {% include %} to resolve via the loader
        try:
            template = self._env.from_string(protected_text)
            rendered = template.render(self.context)
        except UndefinedError as e:
            # Extract variable name from the error message
            # Jinja2 UndefinedError messages are like "'var_name' is undefined"
            var_name = _extract_var_name(str(e))
            line_num = _find_undefined_line(protected_text, var_name)
            raise TemplateRenderError(
                f"Undefined variable '{var_name}'",
                file=str(src),
                line=line_num,
                variable=var_name,
            ) from e
        except TemplateSyntaxError as e:
            raise TemplateRenderError(
                f"Template syntax error: {e.message}",
                file=str(src),
                line=e.lineno,
            ) from e
        except TemplateNotFound as e:
            raise TemplateRenderError(
                f"Included/extended template not found: '{e.name}'. "
                f"Searchpath: {self.searchpath}",
                file=str(src),
            ) from e

        # Restore shell variables
        rendered = _restore_shell_vars(rendered, shell_replacements)

        # For YAML files, parse and re-serialize to ensure valid output
        suffix = dst.suffix.lower()
        if suffix in (".yaml", ".yml"):
            rendered = self._yaml_round_trip(rendered)

        # Ensure destination directory exists
        dst.parent.mkdir(parents=True, exist_ok=True)

        # Write the rendered output
        dst.write_text(rendered, encoding="utf-8")

        # Verify no unresolved Jinja2 tokens remain
        self._verify_no_unresolved(dst)

    def render_tree(self, src_dir: Path, dst_dir: Path) -> list[Path]:
        """Render all .j2 files in a directory tree.

        Walks the source directory recursively, rendering every file with
        a `.j2` extension. The output file has the `.j2` suffix stripped.
        Non-.j2 files are ignored.

        Args:
            src_dir: Root directory containing template files.
            dst_dir: Root directory where rendered files will be written.

        Returns:
            List of Path objects for all rendered output files.

        Raises:
            TemplateRenderError: If strict mode is enabled and an
                undefined variable is encountered in any template.
        """
        src_dir = Path(src_dir)
        dst_dir = Path(dst_dir)
        rendered_files: list[Path] = []

        for src_file in sorted(src_dir.rglob("*.j2")):
            # Compute relative path and strip .j2 suffix for output
            rel_path = src_file.relative_to(src_dir)

            # Strip the .j2 extension to get the output filename
            dst_name = rel_path.name[:-3]  # Remove trailing '.j2'
            dst_file = dst_dir / rel_path.parent / dst_name

            self.render_file(src_file, dst_file)
            rendered_files.append(dst_file)

        return rendered_files

    def render_string(self, template_text: str) -> str:
        """Render a Jinja2 template string against the context.

        Shell variable patterns (${VAR}) are preserved verbatim.

        Args:
            template_text: The Jinja2 template as a string.

        Returns:
            The rendered string.

        Raises:
            TemplateRenderError: If strict mode is enabled and an
                undefined variable is encountered.
        """
        protected_text, shell_replacements = _protect_shell_vars(template_text)
        try:
            template = self._env.from_string(protected_text)
            rendered = template.render(self.context)
        except UndefinedError as e:
            var_name = _extract_var_name(str(e))
            raise TemplateRenderError(
                f"Undefined variable '{var_name}'",
                variable=var_name,
            ) from e
        return _restore_shell_vars(rendered, shell_replacements)

    def _yaml_round_trip(self, rendered_text: str) -> str:
        """Parse rendered YAML and re-serialize to ensure validity.

        Uses sort_keys=False for canonical serialization as specified
        in the design (Req 4.8, AC9).

        Args:
            rendered_text: The rendered YAML content as a string.

        Returns:
            Re-serialized YAML string with consistent formatting.
        """
        # Parse the YAML (may be multi-document)
        docs = list(yaml.safe_load_all(rendered_text))

        if len(docs) == 1:
            return yaml.dump(
                docs[0],
                default_flow_style=False,
                sort_keys=False,
                allow_unicode=True,
            )
        else:
            return yaml.dump_all(
                docs,
                default_flow_style=False,
                sort_keys=False,
                allow_unicode=True,
            )

    def _verify_no_unresolved(self, path: Path) -> None:
        """Verify that a rendered file contains no unresolved Jinja2 tokens.

        Args:
            path: Path to the rendered file to check.

        Raises:
            TemplateRenderError: If unresolved tokens are found.
        """
        content = path.read_text(encoding="utf-8")
        # Check for unresolved Jinja2 delimiters
        for token in ("{{", "{%", "{#"):
            idx = content.find(token)
            if idx != -1:
                line_num = content[:idx].count('\n') + 1
                raise TemplateRenderError(
                    f"Unresolved Jinja2 token '{token}' found after rendering",
                    file=str(path),
                    line=line_num,
                )


def save_as_yaml(cfg: dict, path: Path) -> str:
    """Serialize a configuration dict to canonical YAML and write to a file.

    Uses sort_keys=False to preserve insertion order and default_flow_style=False
    for human-readable block style output.

    Traces to: Requirement 4.8

    Args:
        cfg: Configuration dictionary to serialize.
        path: File path where the YAML output will be written.

    Returns:
        The serialized YAML string.
    """
    path = Path(path)
    serialized = yaml.dump(
        cfg,
        default_flow_style=False,
        sort_keys=False,
        allow_unicode=True,
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(serialized, encoding="utf-8")
    return serialized


def _extract_var_name(error_message: str) -> str:
    """Extract the variable name from a Jinja2 UndefinedError message.

    Jinja2 error messages follow the pattern: "'var_name' is undefined"
    """
    match = re.match(r"'([^']+)'", error_message)
    if match:
        return match.group(1)
    return error_message


def _find_undefined_line(template_text: str, var_name: str) -> int | None:
    """Find the line number where an undefined variable is referenced.

    Searches for {{ var_name }} or similar patterns in the template text.

    Returns:
        Line number (1-indexed) or None if not found.
    """
    # Search for the variable in Jinja2 expression contexts
    patterns = [
        re.compile(rf'\{{\{{\s*{re.escape(var_name)}\b'),  # {{ var_name
        re.compile(rf'\{{% .*\b{re.escape(var_name)}\b'),  # {% ... var_name
    ]
    for pattern in patterns:
        match = pattern.search(template_text)
        if match:
            return template_text[:match.start()].count('\n') + 1
    return None

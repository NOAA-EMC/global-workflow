"""Validator for MOM6 parameter file format.

Format: Lines are either:
- Empty / whitespace-only
- Comment headers starting with `!`
- Parameter assignments: `PARAM = VALUE`

No Fortran namelist syntax (&group / /) is used.

Traces to: Requirements 10.1, 10.5, 10.6
"""

import re


class MOM6ParameterValidator:
    """Validates MOM6 parameter file format.

    MOM6 parameter files use:
    - `! section` comment headers (lines starting with `!`)
    - `PARAM = VALUE` assignments (uppercase parameter names, `=` separator)
    - Empty/blank lines
    - Shell variables like `${TOPOEDITS}` are valid values
    """

    # Pattern for valid parameter assignment lines:
    # Uppercase letters/digits/underscores, optional whitespace, =, optional whitespace, value
    _PARAM_PATTERN = re.compile(
        r'^[A-Z][A-Z0-9_]*\s*=\s*.+$'
    )

    def validate(self, content: str, filepath: str) -> list[str]:
        """Validate MOM6 parameter file content.

        Args:
            content: The rendered MOM6 parameter file content.
            filepath: Path to the file (used in error messages).

        Returns:
            List of error messages. Empty list means valid.
        """
        errors = []
        for lineno, line in enumerate(content.splitlines(), 1):
            stripped = line.strip()

            # Empty/whitespace lines are valid
            if not stripped:
                continue

            # Comment lines starting with ! are valid
            if stripped.startswith('!'):
                continue

            # Parameter assignment lines: PARAM = VALUE
            if self._PARAM_PATTERN.match(stripped):
                continue

            # Anything else is invalid
            errors.append(
                f"MOM6 parameter format error at "
                f"{filepath}:{lineno}: {stripped}"
            )

        return errors

"""Validator for FV3 model_configure format.

Format: One key-value pair per line, where the key and value are separated
by a colon. Values are parseable as string, integer, float, logical
(.true./.false.), or ISO-8601 date types.

Traces to: Requirement 7.1
"""

import re


class ModelConfigureValidator:
    """Validates FV3 model_configure key:value format."""

    VALID_LINE = re.compile(
        r'^(\w+):\s+(.+)$'
    )
    LOGICAL = re.compile(r'^\.(true|false)\.$', re.IGNORECASE)
    INTEGER = re.compile(r'^-?\d+$')
    FLOAT = re.compile(r'^-?\d+\.\d*$')

    def validate(self, content: str, filepath: str) -> list[str]:
        """Validate model_configure content.

        Args:
            content: The rendered model_configure file content.
            filepath: Path to the file (used in error messages).

        Returns:
            List of error messages. Empty list means valid.
        """
        errors = []
        for lineno, line in enumerate(content.splitlines(), 1):
            line = line.strip()
            if not line or line.startswith('#'):
                continue
            match = self.VALID_LINE.match(line)
            if not match:
                errors.append(
                    f"{filepath}:{lineno}: Invalid key:value format: '{line}'"
                )
        return errors

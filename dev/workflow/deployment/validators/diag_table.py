"""Validator for FMS diag_table format.

Format: File entries have 6 columns, field entries have 8 columns.
Lines are comma-separated quoted strings and values.

Traces to: Requirement 7.3
"""


class DiagTableValidator:
    """Validates FMS diag_table format."""

    def validate(self, content: str, filepath: str) -> list[str]:
        """Validate diag_table content.

        Args:
            content: The rendered diag_table file content.
            filepath: Path to the file (used in error messages).

        Returns:
            List of error messages. Empty list means valid.
        """
        errors = []
        lines = content.splitlines()
        for lineno, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                continue
            # File entries: "name", freq, "units", format, "time_units", "long_name"
            # Field entries: "module", "field", "output", "file", "time", avg, "opts", pack
            parts = [p.strip() for p in stripped.split(',')]
            # Heuristic: field entries start with a quoted module name
            if stripped.startswith('"') and len(parts) >= 6:
                if len(parts) not in (6, 7, 8, 9, 10):
                    errors.append(
                        f"{filepath}:{lineno}: Expected 6-10 columns, "
                        f"got {len(parts)}"
                    )
        return errors

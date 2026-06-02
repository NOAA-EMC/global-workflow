"""Validator for FMS field_table format.

Format: Tracer blocks with "TRACER", "module", "name" header,
attribute lines, terminated by /.

Traces to: Requirement 7.5
"""

import re


class FieldTableValidator:
    """Validates FMS field_table format."""

    TRACER_HEADER = re.compile(
        r'^\s*"TRACER"\s*,\s*"(\w+)"\s*,\s*"(\w+)"'
    )

    def validate(self, content: str, filepath: str) -> list[str]:
        """Validate field_table content.

        Args:
            content: The rendered field_table file content.
            filepath: Path to the file (used in error messages).

        Returns:
            List of error messages. Empty list means valid.
        """
        errors = []
        in_tracer = False
        tracer_name = None
        tracer_line = 0
        for lineno, line in enumerate(content.splitlines(), 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                continue
            header = self.TRACER_HEADER.match(stripped)
            if header:
                if in_tracer:
                    errors.append(
                        f"{filepath}:{lineno}: New tracer '{header.group(2)}' "
                        f"before closing '/' for '{tracer_name}' "
                        f"(opened line {tracer_line})"
                    )
                in_tracer = True
                tracer_name = header.group(2)
                tracer_line = lineno
            elif stripped.endswith('/'):
                if not in_tracer:
                    errors.append(
                        f"{filepath}:{lineno}: Tracer terminator '/' "
                        f"without matching TRACER header"
                    )
                in_tracer = False
                tracer_name = None
        if in_tracer:
            errors.append(
                f"{filepath}: Unclosed tracer block '{tracer_name}' "
                f"(opened line {tracer_line})"
            )
        return errors

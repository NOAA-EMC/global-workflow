"""Validator for Fortran namelist syntax.

Format: &group_name opens a group, / closes it. Variables are name = value
with Fortran types. Comments start with !.

Traces to: Requirement 7.2
"""


class NamelistValidator:
    """Validates Fortran namelist syntax."""

    def validate(self, content: str, filepath: str) -> list[str]:
        """Validate Fortran namelist content.

        Args:
            content: The rendered input.nml file content.
            filepath: Path to the file (used in error messages).

        Returns:
            List of error messages. Empty list means valid.
        """
        errors = []
        in_group = False
        group_name = None
        for lineno, line in enumerate(content.splitlines(), 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('!'):
                continue
            if stripped.startswith('&'):
                if in_group:
                    errors.append(
                        f"{filepath}:{lineno}: Nested group '{stripped}' "
                        f"inside unclosed group '&{group_name}'"
                    )
                group_name = stripped[1:]
                in_group = True
            elif stripped == '/':
                if not in_group:
                    errors.append(
                        f"{filepath}:{lineno}: Group terminator '/' "
                        f"without matching '&group'"
                    )
                in_group = False
                group_name = None
            elif in_group:
                # Validate variable assignment
                if '=' not in stripped and not stripped.startswith('!'):
                    errors.append(
                        f"{filepath}:{lineno}: Expected 'var = value' "
                        f"inside &{group_name}"
                    )
        if in_group:
            errors.append(
                f"{filepath}: Unclosed namelist group '&{group_name}'"
            )
        return errors

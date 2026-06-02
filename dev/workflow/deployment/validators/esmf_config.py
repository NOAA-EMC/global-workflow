"""Validator for ESMF/NUOPC configuration syntax.

Format: label: value attributes, label:: opens a block, :: closes it.
runSeq:: contains the coupling sequence.
In MAPL/ESMF config, a standalone :: can also terminate a list started
by "label:" followed by items (e.g., COLLECTIONS: ... ::).

Traces to: Requirement 7.4
"""

import re


class ESMFConfigValidator:
    """Validates ESMF/NUOPC configuration syntax."""

    # Pattern for lines that start a list/block that ends with ::
    # e.g., "COLLECTIONS:" or "GRID_LABELS:" (colon at end, no value after)
    # These are implicitly opened and closed by a standalone ::
    _LIST_OPENER_RE = re.compile(r'^(\w[\w.]*)\s*:\s*$')

    def validate(self, content: str, filepath: str) -> list[str]:
        """Validate ESMF configuration content.

        Args:
            content: The rendered ufs.configure file content.
            filepath: Path to the file (used in error messages).

        Returns:
            List of error messages. Empty list means valid.
        """
        errors = []
        block_stack = []
        for lineno, line in enumerate(content.splitlines(), 1):
            stripped = line.strip()
            if not stripped or stripped.startswith('#'):
                continue
            # Block opener: "label::" (not just "::")
            if stripped.endswith('::') and stripped != '::':
                block_name = stripped[:-2].strip()
                block_stack.append((block_name, lineno))
            elif stripped == '::':
                if not block_stack:
                    # In ESMF/MAPL config, standalone :: can terminate
                    # inline lists (e.g., COLLECTIONS: 'x' \n ::)
                    # This is valid — don't flag as error
                    pass
                else:
                    block_stack.pop()
            # Attribute line: "label: value" (outside blocks is fine)
        for block_name, open_line in block_stack:
            errors.append(
                f"{filepath}:{open_line}: Unclosed block '{block_name}::'"
            )
        return errors

"""Config_Conditioner — evaluates deploy-time conditionals in config files.

Resolves conditionals on deploy-time-known variables (from the registry)
and eliminates dead branches, producing a config specific to the target
workflow.  Uses a regex-based approach for pragmatic reasons: config files
use a limited subset of bash conditional patterns, and the conservative
approach (preserve anything we can't fully evaluate) makes regex safe.

Design Decision: Regex vs Bash AST
    A full bash AST parser was rejected because config files use only
    case/if patterns with simple string comparisons, and the conservative
    approach (preserve anything we can't fully evaluate) makes regex safe
    with no external dependencies.

Traces to: Requirements 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8
"""

from __future__ import annotations

import fnmatch
import re
import subprocess
from dataclasses import dataclass, field
from typing import Optional


# ---------------------------------------------------------------------------
# Regex patterns
# ---------------------------------------------------------------------------

# Extracts variable names from ${VAR} references in conditional expressions.
# Also matches ${VAR:-default}, ${VAR:=default}, ${VAR^^}, etc.
_CONDITIONAL_VAR_PATTERN = re.compile(
    r'\$\{(?P<var>[A-Z_][A-Z0-9_]*)(?:[:#%^,][^}]*)?\}'
)

# Matches the start of an if-block:
#   if [[ ... ]]; then
#   if [[ ... ]] && [[ ... ]]; then
#   if [[ ... ]] || [[ ... ]]; then
_IF_BLOCK_PATTERN = re.compile(
    r'^(?P<indent>\s*)if\s+(?P<expr>.+?)\s*;\s*then\s*$'
)

# Matches the opening line of a case block: case ${VAR} in
# Supports both ${VAR} and $VAR forms
_CASE_BLOCK_PATTERN = re.compile(
    r'^(\s*)case\s+\$\{?(?P<var>[A-Z_][A-Z0-9_]*)\}?\s+in\s*$',
    re.MULTILINE,
)

# Matches elif lines:
#   elif [[ ... ]]; then
_ELIF_PATTERN = re.compile(
    r'^(?P<indent>\s*)elif\s+(?P<expr>.+?)\s*;\s*then\s*$'
)

# Matches else lines
_ELSE_PATTERN = re.compile(
    r'^(?P<indent>\s*)else\s*$'
)

# Matches fi lines
_FI_PATTERN = re.compile(
    r'^(?P<indent>\s*)fi\s*$'
)

# Matches a single [[ ... ]] test expression for evaluation
# Handles: [[ "${VAR}" == "value" ]], [[ ${VAR} == "value" ]],
#           [[ "${VAR}" != "value" ]], [[ "${VAR:-}" == "value" ]]
_SINGLE_TEST_PATTERN = re.compile(
    r'\[\[\s*"?\$\{(?P<var>[A-Z_][A-Z0-9_]*)(?::-[^}]*)?\}"?\s*'
    r'(?P<op>==|!=)\s*"?(?P<value>[^"]*?)"?\s*\]\]'
)


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------

@dataclass
class ConditionerResult:
    """Result of conditioning a single config file.

    Attributes:
        output: Conditioned file content with deploy-time branches resolved.
        eliminated_branches: Count of dead branches removed.
        preserved_conditionals: Count of runtime conditionals kept unchanged.
        is_valid_shell: Whether the output passes bash -n validation.
    """

    output: str
    eliminated_branches: int
    preserved_conditionals: int
    is_valid_shell: bool


@dataclass
class _IfBlock:
    """Internal representation of an if/elif/else/fi block during parsing.

    Tracks the branches of a single if-block for evaluation.
    """

    # The full expression from the 'if' line (for comment generation)
    if_expr: str
    # Whether this block tests only deploy-time variables
    is_deploy_time: bool
    # List of (expression_or_None, body_lines) tuples
    # None expression means 'else' branch
    branches: list[tuple[Optional[str], list[str]]] = field(
        default_factory=list
    )
    # Indentation of the if keyword
    indent: str = ""
    # Nesting depth tracker — how many nested if blocks are inside
    nested_depth: int = 0


# ---------------------------------------------------------------------------
# ConfigConditioner
# ---------------------------------------------------------------------------

class ConfigConditioner:
    """Evaluates deploy-time conditionals in config files.

    Rules:
        1. if/case testing ONLY deploy-time vars -> evaluate, keep matching branch
        2. if/case testing ANY runtime var -> preserve unchanged
        3. Mixed deploy-time + runtime in same expression -> preserve unchanged
        4. Eliminated branches get a comment: # Resolved: VAR=value at deploy time

    Args:
        deploy_time_vars: Dict of variable name -> resolved value.
            Sourced from the Deploy_Time_Variable registry via
            ``get_deploy_time_values(context)``.
    """

    def __init__(self, deploy_time_vars: dict[str, str]) -> None:
        self.deploy_time_vars = deploy_time_vars

    def _is_deploy_time_expression(self, expr: str) -> bool:
        """Check if an expression tests only deploy-time variables.

        Extracts all ``${VAR}`` references from the expression and checks
        whether ALL of them are present in the deploy_time_vars dict.
        If the expression contains no variable references, it is considered
        deploy-time (a constant expression).

        Args:
            expr: A bash conditional expression string, e.g.
                ``"${RUN}" == "gfs"`` or ``"${PDY}" != ""``.

        Returns:
            True if every variable referenced in the expression is a
            deploy-time variable (or if no variables are referenced).
            False if any variable is NOT in the deploy-time registry.
        """
        vars_found = _CONDITIONAL_VAR_PATTERN.findall(expr)
        if not vars_found:
            # No variable references - treat as constant/deploy-time
            return True
        return all(var in self.deploy_time_vars for var in vars_found)

    def _evaluate_condition(self, expr: str) -> bool:
        """Evaluate a simple bash conditional expression with known values.

        Handles:
            - Single test: [[ "${VAR}" == "value" ]]
            - Single test: [[ "${VAR}" != "value" ]]
            - AND compound: [[ ... ]] && [[ ... ]]
            - OR compound: [[ ... ]] || [[ ... ]]

        For compound expressions with mixed && and ||, we evaluate
        left-to-right with && binding tighter (standard bash semantics).

        Args:
            expr: The full conditional expression from the if/elif line.

        Returns:
            True if the condition evaluates to true, False otherwise.
            Returns False if the expression cannot be parsed (conservative).
        """
        # Check for OR compound (lower precedence)
        or_parts = re.split(r'\]\]\s*\|\|\s*\[\[', expr)
        if len(or_parts) > 1:
            tests = re.findall(r'\[\[.+?\]\]', expr)
            return any(self._evaluate_single_test(t) for t in tests)

        # Check for AND compound
        and_parts = re.split(r'\]\]\s*&&\s*\[\[', expr)
        if len(and_parts) > 1:
            tests = re.findall(r'\[\[.+?\]\]', expr)
            return all(self._evaluate_single_test(t) for t in tests)

        # Single test expression
        return self._evaluate_single_test(expr)

    def _evaluate_single_test(self, test_expr: str) -> bool:
        """Evaluate a single [[ ... ]] test expression.

        Args:
            test_expr: A single test like ``[[ "${RUN}" == "gfs" ]]``.

        Returns:
            True if the test evaluates to true, False otherwise.
            Returns False if the test cannot be parsed (conservative).
        """
        match = _SINGLE_TEST_PATTERN.search(test_expr)
        if not match:
            # Cannot parse - conservative: return False
            return False

        var_name = match.group("var")
        operator = match.group("op")
        expected_value = match.group("value")

        actual_value = self.deploy_time_vars.get(var_name, "")

        if operator == "==":
            return actual_value == expected_value
        elif operator == "!=":
            return actual_value != expected_value

        return False

    def validate_shell_syntax(self, content: str) -> bool:
        """Run ``bash -n`` on the content to verify syntactic validity.

        Uses subprocess to invoke bash's syntax-check mode on the
        conditioned output.  Captures stderr for error reporting (available
        via the last_syntax_error attribute after a failed check).

        Args:
            content: Shell script content to validate.

        Returns:
            True if the content is syntactically valid shell, False otherwise.
        """
        try:
            result = subprocess.run(
                ["bash", "-n"],
                input=content,
                capture_output=True,
                text=True,
                timeout=10,
            )
            # bash -n returns non-zero for most syntax errors, but some
            # parse errors (e.g. inside [[ ]]) report to stderr with rc=0.
            # We treat any stderr output as a syntax error.
            if result.returncode != 0 or result.stderr.strip():
                self._last_syntax_error = result.stderr.strip()
                return False
            self._last_syntax_error = ""
            return True
        except (subprocess.TimeoutExpired, FileNotFoundError, OSError) as exc:
            self._last_syntax_error = str(exc)
            return False

    @property
    def last_syntax_error(self) -> str:
        """The stderr output from the last failed ``bash -n`` check.

        Returns an empty string if the last check passed or no check
        has been performed yet.
        """
        return getattr(self, "_last_syntax_error", "")

    def _build_resolution_comment(self, block: _IfBlock) -> str:
        """Build the '# Resolved: ...' comment for an eliminated block.

        Extracts the first deploy-time variable and its value from the
        if expression to produce a human-readable comment.

        Args:
            block: The _IfBlock being resolved.

        Returns:
            A comment string like '# Resolved: RUN=gfs at deploy time'.
        """
        vars_found = _CONDITIONAL_VAR_PATTERN.findall(block.if_expr)
        if vars_found:
            var = vars_found[0]
            value = self.deploy_time_vars.get(var, "")
            return f"{block.indent}# Resolved: {var}={value} at deploy time"
        return f"{block.indent}# Resolved at deploy time"

    def condition_file(self, content: str) -> ConditionerResult:
        """Process a config file, resolving deploy-time conditionals.

        Performs two passes:
            1. Case-block pass: resolve ``case ${VAR} in ... esac`` blocks
               where VAR is a deploy-time variable.
            2. If-block pass: resolve ``if [[ ... ]]; then ... fi`` blocks
               where the expression tests only deploy-time variables.

        Rules:
            1. if/case testing ONLY deploy-time vars -> evaluate, keep
               matching branch
            2. if/case testing ANY runtime var -> preserve unchanged
            3. Mixed deploy-time + runtime in same expression -> preserve
               unchanged
            4. Eliminated branches get a comment:
               ``# Resolved: VAR=value at deploy time``

        Args:
            content: The raw config file content.

        Returns:
            A ConditionerResult with the conditioned output and statistics.
        """
        # --- Pass 1: Case blocks ---
        content, case_eliminated, case_preserved = self._condition_case_blocks(
            content
        )

        # --- Pass 2: If blocks ---
        lines = content.split('\n')
        output_lines: list[str] = []
        eliminated_branches = 0
        preserved_conditionals = 0

        # Stack of _IfBlock objects for nested if handling
        block_stack: list[_IfBlock] = []

        i = 0
        while i < len(lines):
            line = lines[i]

            # Check for 'if' start
            if_match = _IF_BLOCK_PATTERN.match(line)
            if if_match:
                # If we're inside a deploy-time block being collected,
                # track nesting
                if block_stack and block_stack[-1].is_deploy_time:
                    block_stack[-1].nested_depth += 1
                    block_stack[-1].branches[-1][1].append(line)
                    i += 1
                    continue

                expr = if_match.group("expr")
                indent = if_match.group("indent")
                is_deploy = self._is_deploy_time_expression(expr)

                if is_deploy:
                    # Start collecting this block for evaluation
                    block = _IfBlock(
                        if_expr=expr,
                        is_deploy_time=True,
                        indent=indent,
                    )
                    block.branches.append((expr, []))
                    block_stack.append(block)
                else:
                    # Runtime/mixed - preserve unchanged
                    preserved_conditionals += 1
                    output_lines.append(line)
                i += 1
                continue

            # Check for 'elif'
            elif_match = _ELIF_PATTERN.match(line)
            if elif_match:
                if block_stack and block_stack[-1].is_deploy_time:
                    block = block_stack[-1]
                    if block.nested_depth > 0:
                        # Inside a nested block - just collect
                        block.branches[-1][1].append(line)
                    else:
                        expr = elif_match.group("expr")
                        # Check if this elif also tests only deploy-time vars
                        if self._is_deploy_time_expression(expr):
                            block.branches.append((expr, []))
                        else:
                            # Mixed block - abort evaluation, preserve all
                            block.is_deploy_time = False
                            preserved_conditionals += 1
                            # Emit the entire block as-is by reconstructing
                            # and then skipping to fi
                            output_lines.extend(
                                self._reconstruct_block_header(block)
                            )
                            output_lines.append(line)
                            # Continue collecting the rest normally
                            block_stack.pop()
                            i += 1
                            # Now we need to pass through the rest of this
                            # block unchanged until the matching fi
                            depth = 1
                            while i < len(lines) and depth > 0:
                                curr = lines[i]
                                if _IF_BLOCK_PATTERN.match(curr):
                                    depth += 1
                                elif _FI_PATTERN.match(curr):
                                    depth -= 1
                                output_lines.append(curr)
                                i += 1
                            continue
                else:
                    output_lines.append(line)
                i += 1
                continue

            # Check for 'else'
            else_match = _ELSE_PATTERN.match(line)
            if else_match:
                if block_stack and block_stack[-1].is_deploy_time:
                    block = block_stack[-1]
                    if block.nested_depth > 0:
                        block.branches[-1][1].append(line)
                    else:
                        block.branches.append((None, []))
                else:
                    output_lines.append(line)
                i += 1
                continue

            # Check for 'fi'
            fi_match = _FI_PATTERN.match(line)
            if fi_match:
                if block_stack and block_stack[-1].is_deploy_time:
                    block = block_stack[-1]
                    if block.nested_depth > 0:
                        block.nested_depth -= 1
                        block.branches[-1][1].append(line)
                    else:
                        # End of the deploy-time block - evaluate it
                        block_stack.pop()
                        resolved_lines, elim_count = self._resolve_if_block(
                            block
                        )
                        output_lines.extend(resolved_lines)
                        eliminated_branches += elim_count
                else:
                    output_lines.append(line)
                i += 1
                continue

            # Regular line - either collect into block or emit
            if block_stack and block_stack[-1].is_deploy_time:
                block_stack[-1].branches[-1][1].append(line)
            else:
                output_lines.append(line)
            i += 1

        # If we have unclosed blocks (malformed input), emit them as-is
        for block in block_stack:
            output_lines.extend(self._emit_block_as_is(block))

        output = '\n'.join(output_lines)

        # Validate shell syntax (Requirement 5.8)
        is_valid_shell = self.validate_shell_syntax(output)

        return ConditionerResult(
            output=output,
            eliminated_branches=eliminated_branches + case_eliminated,
            preserved_conditionals=preserved_conditionals + case_preserved,
            is_valid_shell=is_valid_shell,
        )

    def _resolve_if_block(
        self, block: _IfBlock
    ) -> tuple[list[str], int]:
        """Evaluate a deploy-time if-block and return the matching branch.

        Evaluates each branch's condition in order. The first branch whose
        condition evaluates to True is kept. If no branch matches and there
        is an else branch, the else branch is kept. If no branch matches
        and there is no else, the block is eliminated entirely.

        Args:
            block: The collected _IfBlock with all branches.

        Returns:
            Tuple of (output_lines, eliminated_branch_count).
        """
        matching_branch: Optional[list[str]] = None
        total_branches = len(block.branches)

        for expr, body in block.branches:
            if expr is None:
                # This is the 'else' branch - matches if nothing else did
                if matching_branch is None:
                    matching_branch = body
            else:
                if matching_branch is None and self._evaluate_condition(expr):
                    matching_branch = body

        result_lines: list[str] = []
        eliminated = 0

        # Add resolution comment
        comment = self._build_resolution_comment(block)
        result_lines.append(comment)

        if matching_branch is not None:
            result_lines.extend(matching_branch)
            # All other branches were eliminated
            eliminated = total_branches - 1
        else:
            # No branch matched - entire block eliminated
            eliminated = total_branches

        return result_lines, eliminated

    def _reconstruct_block_header(self, block: _IfBlock) -> list[str]:
        """Reconstruct the header lines of a partially-collected block.

        Called when we discover mid-parse that a block has mixed
        deploy-time and runtime variables. We emit the 'if' line and
        all collected branch bodies so far.

        Args:
            block: The partially collected _IfBlock.

        Returns:
            List of lines representing the original block header.
        """
        result: list[str] = []
        for idx, (expr, body) in enumerate(block.branches):
            if idx == 0:
                result.append(f"{block.indent}if {expr}; then")
            else:
                if expr is not None:
                    result.append(f"{block.indent}elif {expr}; then")
                else:
                    result.append(f"{block.indent}else")
            result.extend(body)
        return result

    def _emit_block_as_is(self, block: _IfBlock) -> list[str]:
        """Emit a block's collected content as-is (for unclosed blocks).

        Args:
            block: The _IfBlock to emit.

        Returns:
            List of lines representing the block content.
        """
        result: list[str] = []
        for idx, (expr, body) in enumerate(block.branches):
            if idx == 0:
                result.append(f"{block.indent}if {expr}; then")
            else:
                if expr is not None:
                    result.append(f"{block.indent}elif {expr}; then")
                else:
                    result.append(f"{block.indent}else")
            result.extend(body)
        return result

    # ------------------------------------------------------------------
    # Case-block handling (task 4.3)
    # ------------------------------------------------------------------

    def _condition_case_blocks(
        self, content: str
    ) -> tuple[str, int, int]:
        """Resolve deploy-time case blocks in the content.

        Finds all ``case ${VAR} in ... esac`` blocks. For blocks where
        VAR is a deploy-time variable, evaluates the case patterns and
        replaces the block with the matching branch body plus a resolution
        comment. For blocks where VAR is a runtime variable, preserves
        the block unchanged.

        Args:
            content: The file content to process.

        Returns:
            Tuple of (processed_content, eliminated_count, preserved_count).
        """
        eliminated = 0
        preserved = 0

        output = content
        # Process case blocks from end to start to preserve string offsets
        case_blocks = list(_CASE_BLOCK_PATTERN.finditer(output))
        for match in reversed(case_blocks):
            var_name = match.group("var")

            # Rule 2: preserve case blocks testing runtime variables
            if var_name not in self.deploy_time_vars:
                preserved += 1
                continue

            # Find the full case block extent (from 'case' line to 'esac')
            block_start = match.start()
            block_info = self._extract_case_block(output, block_start)
            if block_info is None:
                # Could not parse the block — preserve unchanged (conservative)
                preserved += 1
                continue

            block_end, branches = block_info
            value = self.deploy_time_vars[var_name]

            # Find the matching branch
            matching_branch = self._find_matching_case_branch(branches, value)

            # Build replacement
            indent = match.group(1)
            if matching_branch is not None:
                pattern_text, body_lines = matching_branch
                comment = (
                    f"{indent}# Resolved: case ${{{var_name}}} \u2192 "
                    f"{pattern_text} at deploy time ({var_name}={value})"
                )
                # Build replacement with body lines
                if body_lines:
                    replacement = comment + "\n" + "\n".join(body_lines)
                else:
                    replacement = comment
            else:
                # No branch matched — replace with a comment only
                comment = (
                    f"{indent}# Resolved: case ${{{var_name}}} \u2014 "
                    f"no branch matched at deploy time ({var_name}={value})"
                )
                replacement = comment

            output = output[:block_start] + replacement + output[block_end:]
            eliminated += 1

        return output, eliminated, preserved

    def _extract_case_block(
        self, content: str, start: int
    ) -> Optional[tuple[int, list[tuple[str, list[str]]]]]:
        """Extract the full case block from content starting at *start*.

        Parses from the ``case ... in`` line through to the matching
        ``esac`` line, extracting each branch's pattern and body lines.

        Args:
            content: The full file content.
            start: Character offset of the ``case`` line start.

        Returns:
            A tuple of (end_position, branches) where end_position is the
            index just after the 'esac' line (including its newline), and
            branches is a list of (pattern_string, body_lines) tuples.
            Returns None if the block cannot be parsed (no 'esac' found).
        """
        lines = content[start:].split("\n")
        if not lines:
            return None

        # Find 'esac' to determine block extent
        esac_idx: Optional[int] = None
        for i, line in enumerate(lines):
            if line.strip() == "esac":
                esac_idx = i
                break

        if esac_idx is None:
            return None

        # Calculate absolute end position
        # Sum lengths of lines[0..esac_idx] plus newlines
        block_text_lines = lines[: esac_idx + 1]
        block_len = sum(len(ln) + 1 for ln in block_text_lines)
        end_pos = start + block_len
        if end_pos > len(content):
            end_pos = len(content)

        # Parse branches between 'case ... in' and 'esac'
        branches: list[tuple[str, list[str]]] = []
        body_lines: list[str] = []
        current_pattern: Optional[str] = None

        for i in range(1, esac_idx):
            line = lines[i]
            stripped = line.strip()

            # Skip empty lines and comments between branches
            if not stripped or stripped.startswith("#"):
                if current_pattern is not None:
                    body_lines.append(line)
                continue

            # Check if this line is a case pattern line
            if self._is_case_pattern_line(stripped):
                # Save previous branch if any
                if current_pattern is not None:
                    branches.append((current_pattern, body_lines))
                current_pattern = self._extract_case_pattern(stripped)
                body_lines = []
                # Check for inline body (pattern and body on same line)
                inline_body = self._extract_inline_body(stripped)
                if inline_body is not None:
                    body_lines.append(inline_body)
                    # Inline branches end with ;; on the same line
                    branches.append((current_pattern, body_lines))
                    current_pattern = None
                    body_lines = []
            elif stripped == ";;":
                # End of current branch body
                if current_pattern is not None:
                    branches.append((current_pattern, body_lines))
                    current_pattern = None
                    body_lines = []
            elif current_pattern is not None:
                # Accumulate body line
                body_lines.append(line)

        # Handle last branch if not terminated by ;;
        if current_pattern is not None:
            branches.append((current_pattern, body_lines))

        return (end_pos, branches)

    def _is_case_pattern_line(self, stripped: str) -> bool:
        """Determine if a stripped line is a case pattern (e.g. '*gfs)').

        A case pattern line contains ')' and is not a comment, subshell,
        or assignment. It may also contain inline body ending with ';;'.

        Args:
            stripped: A whitespace-stripped line from inside a case block.

        Returns:
            True if the line looks like a case pattern.
        """
        if not stripped or stripped.startswith("#"):
            return False
        # Find the first ')' that's not inside $(...) or a string
        paren_idx = self._find_case_paren(stripped)
        if paren_idx < 0:
            return False
        # The part before ')' should look like a pattern
        before = stripped[:paren_idx].strip()
        if not before:
            return False
        # Patterns should not contain '=' (assignments) or '[[' (tests)
        if "=" in before and "==" not in before:
            return False
        if "[[" in before:
            return False
        return True

    def _find_case_paren(self, stripped: str) -> int:
        """Find the index of the case-pattern closing ')'.

        Skips ')' that are part of $(...) subshell expansions.

        Args:
            stripped: A stripped line from inside a case block.

        Returns:
            Index of the pattern-closing ')' or -1 if not found.
        """
        i = 0
        while i < len(stripped):
            ch = stripped[i]
            if ch == "$" and i + 1 < len(stripped) and stripped[i + 1] == "(":
                # Skip $(...) subshell — find matching ')'
                depth = 1
                i += 2
                while i < len(stripped) and depth > 0:
                    if stripped[i] == "(":
                        depth += 1
                    elif stripped[i] == ")":
                        depth -= 1
                    i += 1
                continue
            if ch == ")":
                return i
            i += 1
        return -1

    def _extract_case_pattern(self, stripped: str) -> str:
        """Extract the pattern string from a case pattern line.

        E.g. ``'  *gfs)'`` → ``'*gfs'``,
             ``'gdas | gfs)  body ;;'`` → ``'gdas | gfs'``

        Args:
            stripped: A stripped line identified as a case pattern.

        Returns:
            The pattern text (without the closing parenthesis).
        """
        paren_idx = self._find_case_paren(stripped)
        return stripped[:paren_idx].strip()

    def _extract_inline_body(self, stripped: str) -> Optional[str]:
        """Extract inline body from a pattern line if present.

        E.g. ``'gdas | gfs) selective_exclude_string+="..." ;;'``
        → the body part without the trailing ``;;``.

        Args:
            stripped: A stripped case pattern line.

        Returns:
            The inline body text, or None if there's no inline body.
        """
        paren_idx = self._find_case_paren(stripped)
        after = stripped[paren_idx + 1:].strip()
        if not after:
            return None
        # Remove trailing ;; if present
        if after.endswith(";;"):
            after = after[:-2].strip()
        if not after:
            return None
        return after

    def _find_matching_case_branch(
        self,
        branches: list[tuple[str, list[str]]],
        value: str,
    ) -> Optional[tuple[str, list[str]]]:
        """Find the first branch whose pattern matches the given value.

        Supports:
            - Exact match: ``gfs)`` matches only "gfs"
            - Glob prefix: ``*gfs)`` matches "gfs", "anygfs"
            - Glob suffix: ``gfs*)`` matches "gfs", "gfsanything"
            - Wildcard: ``*)`` matches anything (default case)
            - Pipe-separated: ``gdas | gfs)`` matches "gdas" or "gfs"
            - Quoted patterns: ``"gfs")`` matches "gfs"

        Args:
            branches: List of (pattern_text, body_lines) tuples.
            value: The deploy-time variable value to match against.

        Returns:
            The (pattern_text, body_lines) tuple for the first matching
            branch, or None if no branch matches.
        """
        for pattern_text, body_lines in branches:
            if self._case_pattern_matches(pattern_text, value):
                return (pattern_text, body_lines)
        return None

    def _case_pattern_matches(self, pattern_text: str, value: str) -> bool:
        """Check if a case pattern matches a value.

        Handles pipe-separated alternatives and glob patterns.

        Args:
            pattern_text: The pattern string (e.g. ``'gdas | gfs'``).
            value: The value to match against.

        Returns:
            True if any alternative in the pattern matches the value.
        """
        # Split on '|' for alternatives
        alternatives = [alt.strip() for alt in pattern_text.split("|")]
        for alt in alternatives:
            # Strip quotes from the alternative
            alt_clean = alt.strip("\"'")
            if fnmatch.fnmatch(value, alt_clean):
                return True
        return False

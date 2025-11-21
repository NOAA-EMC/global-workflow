#!/usr/bin/env python3
"""
rename_variable_suffix_multi.py

Recursively scan a directory for source/config/template files and rename variable
(or key) names that:
  - start with a capital letter (A-Z)
  - end with a specified old suffix (default: gfs)
Replacing that suffix with a new suffix (default: gcafs).

Supported languages / file types (by extension + optional heuristic):
  - Shell: .sh (or shebang detection)
  - Python: .py
  - YAML: .yml, .yaml
  - Jinja2 templates: .j2, .jinja2, .tmpl, .jinja

Features:
  --from-suffix / --to-suffix customization
  --dry-run with optional unified diffs
  --definitions-only (language-specific behavior)
  --python-safe (tokenize-based: avoids strings/comments)
  --exclude-dirs
  --file-types override (extension list)
  --case-insensitive matching on suffix
  --include-jinja-loops (include for-loop binding variables when definitions-only)
  --verbose for progress logs

Matching pattern (core):
    \b([A-Z][A-Za-z0-9_]*)<OLD_SUFFIX>\b

Language-specific definition logic when --definitions-only:
  Shell : lines with assignments (VAR=...), possibly preceded by export|declare|typeset.
  Python: assignment targets only (e.g., VAR = ..., VAR += 1); loop/control references ignored unless they are targets.
  YAML  : key positions (start of line or indentation) matching 'Key:'.
  Jinja : {% set VAR = ... %} and optionally {% for VAR in ... %} if --include-jinja-loops.

For full-context replacements (no --definitions-only), all occurrences are replaced in each file type:
  - Shell/Python/YAML/Jinja text tokens containing the pattern
  - Python safe mode restricts to identifier names (NAME tokens).

Limitations:
  - YAML multi-line complex keys or anchors may not be fully distinguished.
  - Jinja parsing is regex-based; complex nested structures may need a real parser.
  - If you need to avoid changing inside YAML scalar values only, a true YAML parser would be required.
  - Python safe mode does not detect dynamic attribute strings or f-string expressions embedded as text.

Exit codes:
  0 success
  2 directory not found
  3 unexpected error
"""

from __future__ import annotations
import argparse
import os
import re
import sys
import time
import difflib
from typing import List, Tuple, Pattern, Iterable

# -------------------------
# Pattern construction
# -------------------------
def build_pattern(old_suffix: str, case_insensitive: bool) -> Pattern:
    flags = re.IGNORECASE if case_insensitive else 0
    return re.compile(rf"\b([A-Z][A-Za-z0-9_]*){re.escape(old_suffix)}\b", flags)

# -------------------------
# File type classification
# -------------------------
SHELL_EXTS = {".sh"}
PYTHON_EXTS = {".py"}
YAML_EXTS = {".yml", ".yaml"}
JINJA_EXTS = {".j2", ".jinja2", ".tmpl", ".jinja"}

def detect_type(path: str, file_types: List[str]) -> str | None:
    """
    Returns one of: 'shell', 'python', 'yaml', 'jinja', or None if unsupported.
    Uses explicit extensions list first, then maps to type categories.
    """
    _, ext = os.path.splitext(path)
    ext = ext.lower()
    # If user restricted file_types, only consider those
    if file_types and ext not in file_types:
        return None

    if ext in SHELL_EXTS:
        return "shell"
    if ext in PYTHON_EXTS:
        return "python"
    if ext in YAML_EXTS:
        return "yaml"
    if ext in JINJA_EXTS:
        return "jinja"

    # Shebang shell fallback
    if ext == "" and maybe_shell_by_shebang(path):
        return "shell"
    return None

def maybe_shell_by_shebang(path: str) -> bool:
    try:
        with open(path, "rb") as f:
            first = f.readline(256).decode(errors="ignore")
        return first.startswith("#!") and re.search(r"\b(sh|bash|zsh|ksh)\b", first)
    except Exception:
        return False

def is_binary(path: str) -> bool:
    try:
        with open(path, "rb") as f:
            chunk = f.read(2048)
        return b"\x00" in chunk
    except Exception:
        return True

# -------------------------
# Shell transform
# -------------------------
def shell_def_line(line: str, vars_full: List[str]) -> bool:
    stripped = line.lstrip()
    for v in vars_full:
        # export/declare/typeset optional
        assign_re = re.compile(rf"^(?:export\s+|declare\s+|typeset\s+)?{re.escape(v)}=")
        if assign_re.search(stripped):
            return True
    return False

def transform_shell(content: str,
                    pattern: Pattern,
                    new_suffix: str,
                    definitions_only: bool) -> Tuple[str, int]:
    if not definitions_only:
        return pattern.sub(lambda m: f"{m.group(1)}{new_suffix}", content), len(pattern.findall(content))

    lines = content.splitlines(keepends=False)
    total = 0
    for i, line in enumerate(lines):
        matches = list(pattern.finditer(line))
        if not matches:
            continue
        full_names = [m.group(0) for m in matches]
        if not shell_def_line(line, full_names):
            continue
        new_line, count = pattern.subn(lambda m: f"{m.group(1)}{new_suffix}", line)
        lines[i] = new_line
        total += count
    return "\n".join(lines) + ("\n" if content.endswith("\n") else ""), total

# -------------------------
# Python transform (token-based safe mode)
# -------------------------
def transform_python(content: str,
                     pattern: Pattern,
                     new_suffix: str,
                     definitions_only: bool,
                     safe_mode: bool) -> Tuple[str, int]:
    if not safe_mode and not definitions_only:
        return pattern.sub(lambda m: f"{m.group(1)}{new_suffix}", content), len(pattern.findall(content))

    import tokenize
    from io import StringIO

    # First pass: gather assignment targets if definitions_only
    assignment_targets = set()
    if definitions_only:
        try:
            tokens = list(tokenize.generate_tokens(StringIO(content).readline))
            # Look for NAME tokens followed by '=' (or augmented assign)
            for i, tok in enumerate(tokens):
                if tok.type == tokenize.NAME:
                    name = tok.string
                    # Pattern full match?
                    if not pattern.fullmatch(name):
                        continue
                    # Look ahead for '=' or augmented operators
                    j = i + 1
                    while j < len(tokens) and tokens[j].type == tokenize.NL:
                        j += 1
                    if j < len(tokens) and tokens[j].type == tokenize.OP and tokens[j].string in ("=", "+=", "-=", "*=", "/=", "%=", "//=", "**=", "&=", "|=", "^=", ">>=", "<<="):
                        assignment_targets.add(name)
        except Exception:
            # Fallback: regex line-based
            lines = content.splitlines()
            for line in lines:
                assigns = re.findall(r"^([A-Z][A-Za-z0-9_]*" + re.escape(pattern.pattern.split('(')[-1].split(')')[0].replace('\\b','') + r")\s*=", line)
                for a in assigns:
                    if pattern.fullmatch(a):
                        assignment_targets.add(a)

    # Second pass: rebuild tokens with replacements
    out_parts = []
    replaced = 0
    try:
        tokens = list(tokenize.generate_tokens(StringIO(content).readline))
        for tok in tokens:
            if tok.type == tokenize.NAME:
                name = tok.string
                if pattern.fullmatch(name):
                    if definitions_only:
                        if name in assignment_targets:
                            new_name = name[: len(name) - len_extracted_suffix(name, pattern)] + new_suffix
                            out_parts.append(new_name)
                            replaced += 1
                        else:
                            out_parts.append(name)
                    else:
                        new_name = name[: len(name) - len_extracted_suffix(name, pattern)] + new_suffix
                        out_parts.append(new_name)
                        replaced += 1
                else:
                    out_parts.append(name)
            else:
                if safe_mode and tok.type in (tokenize.STRING, tokenize.COMMENT):
                    # leave unchanged
                    out_parts.append(tok.string)
                else:
                    out_parts.append(tok.string)
        new_content = "".join(out_parts)
        return new_content, replaced
    except Exception:
        # Fallback simple regex logic
        if definitions_only:
            new_lines = []
            lines = content.splitlines(keepends=False)
            for line in lines:
                if pattern.search(line):
                    # Replace only if assignment present
                    targets_here = []
                    for m in pattern.finditer(line):
                        full = m.group(0)
                        if re.search(rf"\b{re.escape(full)}\s*=", line):
                            targets_here.append(full)
                    if targets_here:
                        def repl(m):
                            full = m.group(0)
                            if full in targets_here:
                                return f"{m.group(1)}{new_suffix}"
                            return full
                        nl, c = pattern.subn(repl, line)
                        replaced += c
                        new_lines.append(nl)
                        continue
                new_lines.append(line)
            return "\n".join(new_lines) + ("\n" if content.endswith("\n") else ""), replaced
        else:
            new_content, c = pattern.subn(lambda m: f"{m.group(1)}{new_suffix}", content)
            return new_content, c

def len_extracted_suffix(name: str, pattern: Pattern) -> int:
    # Extract suffix length by matching again
    m = pattern.fullmatch(name)
    if not m:
        return 0
    # suffix length = full match length - prefix group length
    return len(m.group(0)) - len(m.group(1))

# -------------------------
# YAML transform
# -------------------------
def transform_yaml(content: str,
                   pattern: Pattern,
                   new_suffix: str,
                   definitions_only: bool) -> Tuple[str, int]:
    if not definitions_only:
        return pattern.sub(lambda m: f"{m.group(1)}{new_suffix}", content), len(pattern.findall(content))

    lines = content.splitlines(keepends=False)
    total = 0
    key_regex_cache = {}
    for i, line in enumerate(lines):
        # Identify potential key segment before colon
        if ":" not in line:
            continue
        # Extract key token (simplistic): leading (indent)*(key):(space|value)
        match_key = re.match(r"^(\s*)([A-Z][A-Za-z0-9_]*[A-Za-z0-9_]*)", line)
        if not match_key:
            continue
        # Now test full variable pattern at key start
        key = match_key.group(2)
        if pattern.fullmatch(key):
            new_key = key[: len(key) - len_extracted_suffix(key, pattern)] + new_suffix
            # Replace only the first occurrence of key at its position
            prefix_len = len(match_key.group(1))
            lines[i] = match_key.group(1) + new_key + line[prefix_len + len(key):]
            total += 1
    return "\n".join(lines) + ("\n" if content.endswith("\n") else ""), total

# -------------------------
# Jinja transform
# -------------------------
def transform_jinja(content: str,
                    pattern: Pattern,
                    new_suffix: str,
                    definitions_only: bool,
                    include_loops: bool) -> Tuple[str, int]:
    if not definitions_only:
        return pattern.sub(lambda m: f"{m.group(1)}{new_suffix}", content), len(pattern.findall(content))

    lines = content.splitlines(keepends=False)
    total = 0
    set_re = re.compile(r"{%\s*set\s+([A-Z][A-Za-z0-9_]*[A-Za-z0-9_]*)\s*=")
    loop_re = re.compile(r"{%\s*for\s+([A-Z][A-Za-z0-9_]*[A-Za-z0-9_]*)\s+in\b")
    for i, line in enumerate(lines):
        modified_line = line
        # Handle set statements
        for m in set_re.finditer(line):
            var = m.group(1)
            if pattern.fullmatch(var):
                new_var = var[: len(var) - len_extracted_suffix(var, pattern)] + new_suffix
                modified_line = modified_line.replace(f"set {var}", f"set {new_var}")
                total += 1
        if include_loops:
            for m in loop_re.finditer(line):
                var = m.group(1)
                if pattern.fullmatch(var):
                    new_var = var[: len(var) - len_extracted_suffix(var, pattern)] + new_suffix
                    modified_line = re.sub(rf"(for\s+){re.escape(var)}(\s+in\b)", rf"\1{new_var}\2", modified_line)
                    total += 1
        lines[i] = modified_line
    return "\n".join(lines) + ("\n" if content.endswith("\n") else ""), total

# -------------------------
# Dispatcher
# -------------------------
def transform_content(content: str,
                      file_type: str,
                      pattern: Pattern,
                      new_suffix: str,
                      definitions_only: bool,
                      python_safe: bool,
                      include_jinja_loops: bool) -> Tuple[str, int]:
    if file_type == "shell":
        return transform_shell(content, pattern, new_suffix, definitions_only)
    if file_type == "python":
        return transform_python(content, pattern, new_suffix, definitions_only, python_safe)
    if file_type == "yaml":
        return transform_yaml(content, pattern, new_suffix, definitions_only)
    if file_type == "jinja":
        return transform_jinja(content, pattern, new_suffix, definitions_only, include_jinja_loops)
    # Fallback generic text
    return pattern.sub(lambda m: f"{m.group(1)}{new_suffix}", content), len(pattern.findall(content))

# -------------------------
# Utilities
# -------------------------
def backup_path(path: str) -> str:
    return f"{path}.bak_{time.strftime('%Y%m%d_%H%M%S')}"

def collect_files(root: str, exclude_dirs: List[str]) -> List[str]:
    ex_abs = {os.path.abspath(os.path.join(root, d)) for d in exclude_dirs}
    result = []
    for dirpath, dirnames, filenames in os.walk(root):
        # prune excluded
        dirnames[:] = [d for d in dirnames if os.path.abspath(os.path.join(dirpath, d)) not in ex_abs]
        if ".git" in dirpath.split(os.sep):
            continue
        for name in filenames:
            result.append(os.path.join(dirpath, name))
    return result

# -------------------------
# Main
# -------------------------
def main():
    parser = argparse.ArgumentParser(description="Rename capital-start variable/key suffixes across multiple file types.")
    parser.add_argument("directory", help="Root directory to scan.")
    parser.add_argument("--from-suffix", default="gfs", help="Old suffix to replace.")
    parser.add_argument("--to-suffix", default="gcafs", help="New suffix to apply.")
    parser.add_argument("--dry-run", action="store_true", help="Preview changes only.")
    parser.add_argument("--no-diff", action="store_true", help="Skip unified diffs in dry-run.")
    parser.add_argument("--definitions-only", action="store_true", help="Limit changes to definition contexts per language.")
    parser.add_argument("--python-safe", action="store_true", help="Use token-based Python processing (avoid strings/comments).")
    parser.add_argument("--include-jinja-loops", action="store_true", help="In definitions-only, also rename loop vars in {% for VAR in ... %}.")
    parser.add_argument("--exclude-dirs", nargs="*", default=[".venv", "node_modules", "dist", "build"], help="Directories to exclude relative to root.")
    parser.add_argument("--file-types", nargs="*", default=[
        ".sh", ".py", ".yml", ".yaml", ".j2", ".jinja2", ".tmpl", ".jinja"
    ], help="Extensions to consider. Provide a subset to restrict.")
    parser.add_argument("--case-insensitive", action="store_true", help="Case-insensitive suffix match.")
    parser.add_argument("--verbose", action="store_true", help="Verbose logging.")
    args = parser.parse_args()

    root = args.directory
    if not os.path.isdir(root):
        print(f"Directory not found: {root}", file=sys.stderr)
        return 2

    pattern = build_pattern(args.from_suffix, args.case_insensitive)
    all_files = collect_files(root, args.exclude_dirs)
    changed: List[Tuple[str, str, str, int]] = []

    # Normalize extension list to lowercase
    user_exts = [ext.lower() for ext in args.file_types]

    for path in all_files:
        ftype = detect_type(path, user_exts)
        if ftype is None:
            continue
        if is_binary(path):
            if args.verbose:
                print(f"Skipping binary: {path}")
            continue
        try:
            with open(path, "r", encoding="utf-8", errors="replace") as f:
                original = f.read()
            new_content, count = transform_content(
                original,
                ftype,
                pattern,
                args.to_suffix,
                args.definitions_only,
                args.python_safe,
                args.include_jinja_loops
            )
            if count > 0 and new_content != original:
                changed.append((path, original, new_content, count))
                if args.verbose:
                    print(f"Scheduled change: {path} ({count} replacements)")
        except Exception as e:
            print(f"Warning: could not process {path}: {e}", file=sys.stderr)

    if args.dry_run:
        if not changed:
            print("Dry run: No files would be modified.")
            return 0
        for path, old, new, count in changed:
            print(f"Would modify: {path} (replacements: {count})")
            if not args.no_diff:
                diff = difflib.unified_diff(
                    old.splitlines(), new.splitlines(),
                    fromfile=f"{path} (original)",
                    tofile=f"{path} (modified)",
                    lineterm=""
                )
                for line in diff:
                    print(line)
            print()
        print(f"Dry run complete. Files that would change: {len(changed)} | Total replacements: {sum(c for _,_,_,c in changed)}")
        return 0

    for path, old, new, count in changed:
        bkp = backup_path(path)
        with open(bkp, "w", encoding="utf-8") as f:
            f.write(old)
        with open(path, "w", encoding="utf-8") as f:
            f.write(new)
        print(f"Modified: {path} (replacements: {count}, backup: {bkp})")

    print(f"Done. Files modified: {len(changed)} | Total replacements: {sum(c for _,_,_,c in changed)}")
    return 0

if __name__ == "__main__":
    sys.exit(main())

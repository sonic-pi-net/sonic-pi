#!/usr/bin/env python3
"""
unwrap-prose.py — join soft-wrapped prose lines so each paragraph is one line.

Only body-text paragraphs are unwrapped. Everything structural is left
byte-for-byte identical: YAML/TOML front matter, fenced & indented code,
tables (any line containing "|"), lists, blockquotes, ATX & setext headings,
block-level HTML, Liquid "{% %}" tag lines, reference/footnote definitions,
thematic breaks, and any paragraph that uses hard line breaks (a line ending
in two+ spaces or a backslash).

Conservative by design: when a line's role is ambiguous it is treated as a
boundary and left alone rather than risk corrupting structure. The transform
is idempotent — running it twice yields the same result as running it once.

Usage:
  unwrap-prose.py --check [PATHS ...]   # list files that WOULD change; exit 1 if any
  unwrap-prose.py --diff  [PATHS ...]   # print a unified diff; no writes
  unwrap-prose.py --write [PATHS ...]   # rewrite files in place

With no PATHS, operates on the current repo's git-tracked *.md / *.markdown
files (so vendored/ignored/submodule content is excluded automatically).
"""
# Vendored verbatim from the bamr87 hub (tools/unwrap-prose.py) into each repo
# by tools/fanout.sh; it is not the host repo's own source, so it opts out of
# that repo's linters / type-checkers rather than chase every project's config.
# ruff: noqa
# flake8: noqa
# mypy: ignore-errors
from __future__ import annotations

import argparse
import difflib
import re
import subprocess
import sys
from pathlib import Path

# --- line classifiers -------------------------------------------------------
BLANK = re.compile(r"^\s*$")
FRONT_FENCE = re.compile(r"^(---|\+\+\+)\s*$")          # YAML/TOML front matter delimiter
FENCE = re.compile(r"^\s{0,3}(`{3,}|~{3,})")            # ``` or ~~~ code fence
ATX = re.compile(r"^\s{0,3}#{1,6}(\s|$)")               # # Heading
LIST = re.compile(r"^\s{0,3}([-*+]|\d+[.)])\s")         # -, *, +, 1. list item
QUOTE = re.compile(r"^\s{0,3}>")                        # > blockquote
HTML = re.compile(r"^\s{0,3}<")                          # block-level HTML / autolink
LIQUID = re.compile(r"^\s*\{%")                          # {% if %}, {% for %}, {% include %}
REF_DEF = re.compile(r"^\s{0,3}\[[^\]]+\]:\s")          # [id]: https://...
FOOTNOTE = re.compile(r"^\s{0,3}\[\^[^\]]+\]:")         # [^n]: ...
HR = re.compile(r"^\s{0,3}([-*_])(\s*\1){2,}\s*$")      # ---, ***, ___ thematic break
INDENT_CODE = re.compile(r"^(\t| {4,})")                # 4-space / tab indented code
SETEXT = re.compile(r"^\s{0,3}(=+|-+)\s*$")             # === or --- heading underline
HARD_BREAK = re.compile(r"(\s{2,}|\\)$")                # trailing "  " or "\" -> <br>


def starts_atomic(line: str) -> bool:
    """A line that must never be merged into a prose paragraph."""
    return bool(
        ATX.match(line)
        or LIST.match(line)
        or QUOTE.match(line)
        or HTML.match(line)
        or LIQUID.match(line)
        or REF_DEF.match(line)
        or FOOTNOTE.match(line)
        or HR.match(line)
        or INDENT_CODE.match(line)
        or FENCE.match(line)
        or "|" in line  # any table row (leading-pipe or not) — safe to leave alone
    )


def transform(text: str) -> str:
    had_final_nl = text.endswith("\n")
    lines = text.splitlines()
    out: list[str] = []
    i, n = 0, len(lines)

    # Front matter: only when the very first line is exactly --- or +++.
    if n and FRONT_FENCE.match(lines[0]):
        out.append(lines[0])
        i = 1
        while i < n:
            out.append(lines[i])
            closed = FRONT_FENCE.match(lines[i])
            i += 1
            if closed:
                break

    while i < n:
        line = lines[i]

        # Fenced code block: copy verbatim through the matching closing fence.
        m = FENCE.match(line)
        if m:
            ticks = m.group(1)
            fence_char, fence_len = ticks[0], len(ticks)
            out.append(line)
            i += 1
            while i < n:
                out.append(lines[i])
                c = FENCE.match(lines[i])
                i += 1
                if c and c.group(1)[0] == fence_char and len(c.group(1)) >= fence_len:
                    break
            continue

        if BLANK.match(line) or starts_atomic(line):
            out.append(line)
            i += 1
            continue

        # A prose paragraph: gather its wrapped continuation lines.
        group = [line]
        i += 1
        while i < n:
            nxt = lines[i]
            if BLANK.match(nxt) or starts_atomic(nxt) or SETEXT.match(nxt):
                break
            if HARD_BREAK.search(group[-1]):          # previous line forces a break
                break
            if i + 1 < n and SETEXT.match(lines[i + 1]):  # nxt is a setext heading's text
                break
            group.append(nxt)
            i += 1

        # A paragraph that uses hard breaks keeps its author-chosen line layout.
        if len(group) == 1 or any(HARD_BREAK.search(g) for g in group):
            out.extend(group)
        else:
            out.append(" ".join(g.strip() for g in group))

    result = "\n".join(out)
    if had_final_nl:
        result += "\n"
    return result


# --- driver -----------------------------------------------------------------
def git_tracked_md() -> list[Path]:
    try:
        raw = subprocess.check_output(
            ["git", "ls-files", "-z", "*.md", "*.markdown"], text=True
        )
    except (subprocess.CalledProcessError, FileNotFoundError):
        return []
    return [Path(p) for p in raw.split("\0") if p]


def iter_paths(paths: list[str]) -> list[Path]:
    if not paths:
        return git_tracked_md()
    found: list[Path] = []
    for p in paths:
        pp = Path(p)
        if pp.is_dir():
            found += sorted(pp.rglob("*.md")) + sorted(pp.rglob("*.markdown"))
        else:
            found.append(pp)
    return found


def main() -> int:
    ap = argparse.ArgumentParser(description="Unwrap soft-wrapped markdown prose.")
    mode = ap.add_mutually_exclusive_group()
    mode.add_argument("--check", action="store_true", help="list files that would change; exit 1 if any")
    mode.add_argument("--diff", action="store_true", help="print a unified diff; no writes")
    mode.add_argument("--write", action="store_true", help="rewrite files in place")
    ap.add_argument("--exclude", action="append", metavar="REGEX", default=[],
                    help="skip paths matching this regex (repeatable)")
    ap.add_argument("paths", nargs="*", help="files/dirs (default: git-tracked markdown)")
    args = ap.parse_args()

    excludes = [re.compile(p) for p in args.exclude]
    changed: list[Path] = []
    for path in iter_paths(args.paths):
        if any(rx.search(path.as_posix()) for rx in excludes):
            continue
        try:
            original = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError) as e:
            print(f"skip {path}: {e}", file=sys.stderr)
            continue
        updated = transform(original)
        if updated == original:
            continue
        changed.append(path)
        if args.diff:
            sys.stdout.writelines(
                difflib.unified_diff(
                    original.splitlines(keepends=True),
                    updated.splitlines(keepends=True),
                    fromfile=str(path),
                    tofile=str(path),
                )
            )
        elif args.write:
            path.write_text(updated, encoding="utf-8")
            print(f"unwrapped {path}")
        else:  # --check (default)
            print(path)

    if args.check or not (args.diff or args.write):
        if changed:
            print(f"\n{len(changed)} file(s) would change.", file=sys.stderr)
            return 1
        print("All markdown prose already unwrapped.", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

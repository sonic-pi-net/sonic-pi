# Sonic Pi fork of QScintilla

This tree is a Sonic Pi-maintained fork of **QScintilla 2.14.1** (Riverbank
Computing, released 2023-06-07 — the last upstream release). Upstream is
dormant, so Sonic Pi carries its own changes here rather than tracking a moving
target or relying on out-of-tree patches.

Every Sonic Pi-specific deviation from stock 2.14.1 is recorded below. Keep this
list current when you touch the vendored source so the fork stays auditable
(`git diff` against the import commit `40bf3794a` is the ground truth).

## Changes

### 2026-06-29 — Fix screen-reader crash in `QsciAccessibleScintillaBase::textRange`

- **File:** `src/SciAccessibility.cpp`
- **What:** Guard `textRange()` against an empty/reversed range
  (`end_position <= start_position`), returning an empty string instead of
  calling `SCI_GETTEXTRANGE`.
- **Why:** For such a range the `QByteArray` is empty, so `.data()` returns Qt's
  shared **read-only** empty buffer; `SCI_GETTEXTRANGE` writing its NUL
  terminator there is a write to a read-only page → `EXC_BAD_ACCESS` (SIGBUS).
  Reproduced with VoiceOver on macOS querying the current/empty editor line
  while the completion popup was open; affects every platform's screen-reader
  path (NVDA/Narrator/Orca) since this code is cross-platform.
- **Reportable upstream:** yes (genuine QScintilla a11y bug), if Riverbank ever
  resumes releases.

# Sonic Pi fork of QScintilla

This tree is a Sonic Pi-maintained fork of **QScintilla 2.14.1** (Riverbank
Computing, released 2023-06-07 — the last upstream release). Upstream is
dormant, so Sonic Pi carries its own changes here rather than tracking a moving
target or relying on out-of-tree patches.

Every Sonic Pi-specific deviation from stock 2.14.1 is recorded below. Keep this
list current when you touch the vendored source so the fork stays auditable
(`git diff` against the import commit `40bf3794a` is the ground truth).

## Changes

### 2026-07-11 — `INDIC_STRAIGHTBOX` hugs the glyph band, not the whole line rect

- **Files:** `scintilla/src/EditView.cpp`, `scintilla/src/Indicator.cpp`
- **What:** `DrawIndicator` now passes the true glyph band (line top +
  `extraAscent` … baseline + font descent, i.e. excluding `extraAscent` /
  `extraDescent` padding) as `rcCharacter`'s vertical extent, and
  `INDIC_STRAIGHTBOX` uses that instead of `rcLine` for its box. Stock
  behaviour boxes the whole line rect (only 1px off the top), which is
  identical to `INDIC_FULLBOX` in all but name.
- **Why:** Sonic Pi adds `SCI_SETEXTRADESCENT` line padding (room for the
  error squiggle), so a stock straight-box hangs well below the text and reads
  as vertically off-centre. The trigger-flash code wash needs a box that sits
  symmetrically around the glyphs. `rcCharacter`'s vertical extent was
  otherwise unused (`INDIC_POINT`/`POINTCHARACTER` only read its x-range), and
  `INDIC_FULLBOX` retains the full-line behaviour for anyone who wants it.
- **Reportable upstream:** arguably (STRAIGHTBOX == FULLBOX is surely not
  intended), but the fix changes documented behaviour, so it stays ours.

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

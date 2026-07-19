# Sonic Pi fork of QScintilla

This tree is a Sonic Pi-maintained fork of **QScintilla 2.14.1** (Riverbank
Computing, released 2023-06-07 — the last upstream release). Upstream is
dormant, so Sonic Pi carries its own changes here rather than tracking a moving
target or relying on out-of-tree patches.

Every Sonic Pi-specific deviation from stock 2.14.1 is recorded below. Keep this
list current when you touch the vendored source so the fork stays auditable
(`git diff` against the import commit `40bf3794a` is the ground truth).

## Changes

### 2026-07-19 — Blank left margin joins the text painting (chip padding at column 0)

- **Files:** `scintilla/src/EditView.cpp`, `scintilla/src/Editor.cpp`,
  `scintilla/src/ViewStyle.h`
- **What:** `leftTextOverlap` widens from a fixed 1px to the full blank left
  margin (`SCI_SETMARGINLEFT`), via a single `ViewStyle::LeftTextOverlap`
  helper: the paint clip, spacer clear, buffered-copy rect and redraw rect
  all cover the strip. The strip is kept seamless with the line in every
  configuration: `DrawTranslucentLineState` extends caret-line and
  background/underline marker washes across it, and the spacer clear runs in
  all phase modes (including `SC_PHASES_MULTIPLE`, which otherwise never
  re-primes it) filling with the line's effective background
  (`ViewStyle::Background`) so opaque caret-line/marker colours meet the
  margin without a notch.
- **Why:** find-match chips pad a few px around the glyphs; at column 0 the
  padding + border needs room left of the text. Stock behaviour clipped
  there, and a blank `SCI_SETMARGINLEFT` inset previously left an untinted
  seam beside the margin on washed lines (the reason Sonic Pi had zeroed it).
  Sonic Pi sets a 7dx inset in `sonicpiscintilla.cpp`.
- **Known limits:** the overlap collapses to 0 while horizontally scrolled
  (`xOffset > 0`, as stock), so column-0 chip padding is clipped at the text
  edge mid-scroll; validated for `marginInside == true` (Sonic Pi's config)
  only.
- **Reportable upstream:** no — behavioural styling choice.

### 2026-07-19 — `INDIC_ROUNDBOX` draws real rounded chips (find-match highlights)

- **Files:** `src/PlatQt.cpp`, `scintilla/src/Indicator.cpp`
- **What:** `SurfaceImpl::AlphaRectangle` renders the rounded (`cornerSize`)
  case antialiased with a 2px stroke inset fully inside the rect and a small
  absolute corner radius; stock code used an unantialiased 1px cosmetic pen
  sitting exactly on the rect edge (clipped invisible at column 0) and
  `Qt::RelativeSize` 25% radii, which taper into ovals on wide boxes.
  Borderless boxes (outline == fill, e.g. the solid current-match chip) fill
  the full rect with no inset, keeping the caller's exact geometry.
  `INDIC_ROUNDBOX` also now hugs the glyph band like the 2026-07-11
  `INDIC_STRAIGHTBOX` change, padded a few px around the glyphs and clamped
  to the line rect vertically (the box must not leak into adjacent lines'
  rects — per-line invalidation would leave stale/shaved border rows there).
  Column-0 left padding relies on the blank-left-margin change above; the
  host must set `SCI_SETMARGINLEFT` at least padding + stroke wide, and the
  find code coalesces back-to-back matches so padded chips don't overlap.
- **Why:** the editor find bar highlights matches as rounded outlined chips;
  the stock rendering lost the left border on column-0 matches and drew
  lopsided corners.
- **Reportable upstream:** the clipped/unantialiased outline arguably; the
  radius/stroke choices are Sonic Pi styling.

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

# Qt App

This is the Qt Sonic Pi GUI.

## Accessibility testing

Screen-reader accessibility is verified at three layers; changes that touch
widgets, focus, or announcements should be checked against all that apply:

1. **Headless unit tests** — `app/gui-tests` (Catch2, offscreen). Pin the
   accessibility *data*: roles, names, text interfaces, announcement
   signals. Run with `QT_QPA_PLATFORM=offscreen ./gui-tests` from
   `app/build/gui-tests`.

2. **Platform bridge self-test** — `app/mac-selftest-accessibility.sh`
   (and `--selftest-accessibility` on Windows). Drives the real
   NSAccessibility/UIA bridge in-process: popup pruning, announcements,
   checkable-toolbar activation, prose text areas, card reading order.

3. **VoiceOver transcript** — `app/mac-selftest-voiceover.sh`. Drives a
   running Sonic Pi with VoiceOver on and prints VoiceOver's actual spoken
   phrases for the app's main flows (docs caret reading, TOC, examples,
   cards, panes, preferences, FX reference). This is the only layer that
   shows what a screen-reader user really hears — judge it by reading the
   transcript. Requires VoiceOver running, "Allow VoiceOver to be
   controlled with AppleScript" (VoiceOver Utility > General), and
   Automation permission for the invoking terminal.

Conventions worth knowing (learned the hard way):

- VoiceOver orders group children by screen position unless
  `AXChildrenInNavigationOrder` is provided; `installAccessibilityNavigationOrderShim()`
  (platform/macos.mm) supplies it app-wide as tree order, so **widget
  creation order is the reading order** — create content before actions.
- Rich-text QLabels expose raw markup to assistive tech; set a plain-text
  `accessibleName` explicitly.
- Content a user must hear on focus belongs in the accessible *name*; the
  *description* (AXHelp) is only spoken at some verbosity settings.
- Never land focus on a bare container (scroll area, splitter): focus the
  named content widget.
- Pointer-only affordances (drag handles) get `Qt::NoFocus` plus the
  `a11yIgnored` dynamic property so the factories prune them from the tree.
- Speak through `MainWindow::announce()` / `announceRequested` signals —
  never `QAccessible::updateAccessibility` ad hoc (see
  `utils/announcementpolicy.h` for the categories).

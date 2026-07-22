#!/bin/bash
# Captures what VoiceOver ACTUALLY SPEAKS while walking a running Sonic Pi —
# the layer even the NSAccessibility self-test can't see (VoiceOver
# synthesises its speech from the AX tree plus its own settings).
#
# Prints a labelled transcript covering the app's main accessibility flows:
# help TOC, docs caret reading, examples, cards (order/wrap/read-code),
# panes, preferences and the FX reference. Judge it by reading — the point
# is VoiceOver's own words.
#
# Requirements (one-time):
#   1. VoiceOver running (Cmd+F5).
#   2. VoiceOver Utility > General > "Allow VoiceOver to be controlled with
#      AppleScript" ticked.
#   3. The invoking terminal granted Accessibility + Automation permissions
#      (System Events and VoiceOver).
#
# Hands off the keyboard while it runs (~2 minutes).

set -e

osascript <<'APPLESCRIPT'
on voPhrase()
    try
        tell application "VoiceOver" to return content of last phrase
    on error errMsg
        return "(could not read VoiceOver: " & errMsg & ")"
    end try
end voPhrase

on capture(aList, aLabel, aDelay)
    delay aDelay
    set end of aList to aLabel & " :: " & voPhrase()
end capture

on pressKey(aKey, mods)
    tell application "System Events" to keystroke aKey using mods
end pressKey

on pressCode(aCode, mods)
    tell application "System Events" to key code aCode using mods
end pressCode

try
    tell application "VoiceOver" to get content of last phrase
on error
    return "VoiceOver is not running or not scriptable. Turn VoiceOver on (Cmd+F5) and tick VoiceOver Utility > General > 'Allow VoiceOver to be controlled with AppleScript', then re-run."
end try

set t to {}

tell application "Sonic Pi" to activate
delay 1.5

-- ===== tutorial TOC: chapters navigable by plain arrows =====
pressKey("h", {control down, shift down}) of me -- Focus Help Docs Listing
capture(t, "TOC focus", 2.0) of me
pressCode(125, {}) of me
capture(t, "TOC down", 2.0) of me

-- ===== docs caret: character/line reading (reads like a web page) =====
pressKey("d", {control down, shift down}) of me -- Focus Help Docs Details
capture(t, "docs details focus", 2.0) of me
pressCode(124, {}) of me
capture(t, "docs char", 1.2) of me
pressCode(125, {}) of me
capture(t, "docs line down", 1.5) of me

-- ===== examples: browse + line-by-line =====
tell application "System Events" to tell process "Sonic Pi"
    click menu bar item "Examples" of menu bar 1
    delay 0.8
    click menu item "Browse Examples in Help..." of menu 1 of menu bar item "Examples" of menu bar 1
end tell
capture(t, "browse examples", 2.5) of me
pressCode(125, {}) of me
capture(t, "examples down", 2.0) of me
pressKey("d", {control down, shift down}) of me
capture(t, "example details focus", 2.0) of me
pressCode(125, {}) of me
capture(t, "example line down", 1.5) of me

-- ===== cards: content-first order, ring wrap, code on demand =====
pressKey("q", {control down, shift down}) of me -- Focus Help Cards
capture(t, "cards focus", 2.2) of me
pressCode(123, {}) of me -- Left from page 1: wraps to the end
capture(t, "cards wrap left", 2.2) of me
pressCode(124, {}) of me -- Right: wraps back
capture(t, "cards wrap right", 2.2) of me
pressKey("c", {}) of me -- C reads the card's code aloud
capture(t, "cards read code", 2.0) of me
pressCode(125, {control down, option down, shift down}) of me -- interact in
capture(t, "card interact in", 1.5) of me
pressCode(124, {control down, option down}) of me -- first element inside
capture(t, "card step 1", 1.3) of me
pressCode(124, {control down, option down}) of me
capture(t, "card step 2", 1.3) of me
pressCode(126, {control down, option down, shift down}) of me -- interact out
capture(t, "card interact out", 1.5) of me

-- ===== panes: logs, cues, context, help logs/debug =====
pressKey("l", {control down, shift down}) of me
capture(t, "log pane focus", 2.0) of me
pressKey("c", {control down, shift down}) of me
capture(t, "cue pane focus", 2.0) of me
pressKey("t", {control down, shift down}) of me
capture(t, "context pane focus", 2.0) of me
pressKey("f", {control down, shift down}) of me
capture(t, "help logs focus", 2.0) of me
pressKey("g", {control down, shift down}) of me
capture(t, "help debug focus", 2.0) of me

-- ===== preferences =====
pressKey(",", {command down}) of me -- Focus Preferences (Meta+,)
capture(t, "prefs focus", 2.2) of me
pressCode(48, {}) of me -- Tab
capture(t, "prefs tab", 1.8) of me

-- ===== FX reference: dials + snippets (also guards the dial paint hang) =====
tell application "System Events" to tell process "Sonic Pi"
    click menu bar item "Examples" of menu bar 1
    delay 0.8
    click menu item "Browse FX in Help..." of menu 1 of menu bar item "Examples" of menu bar 1
end tell
capture(t, "browse fx", 2.5) of me
pressCode(125, {}) of me
capture(t, "fx down", 2.2) of me
pressKey("d", {control down, shift down}) of me
capture(t, "fx details focus", 2.2) of me
pressCode(48, {}) of me
capture(t, "fx tab", 1.8) of me

-- ===== metro scrubbers, then leave focus in the editor =====
pressKey("b", {control down, shift down}) of me
capture(t, "bpm scrubber focus", 2.0) of me
pressKey("e", {control down, shift down}) of me
capture(t, "editor refocus", 1.5) of me

set out to ""
repeat with line1 in t
    set out to out & (line1 as text) & linefeed
end repeat
return out
APPLESCRIPT

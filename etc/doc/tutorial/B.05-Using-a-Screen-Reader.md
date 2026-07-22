B.5 Using a Screen Reader

# Using a Screen Reader

Sonic Pi is designed to be fully playable with a screen reader such as
VoiceOver, NVDA or JAWS. Everything you need — writing code, running it,
reading the documentation, exploring the cards and changing your
preferences — can be done from the keyboard, with spoken feedback along
the way. This section is a tour of how it all fits together, so you can
head straight to the good bits rather than discovering them by accident.

A quick note on names: in Sonic Pi's shortcut language, *Control* is the
Control key on all platforms, and *Meta* is Alt on Windows/Linux and
Command on Mac. See the section on Using Shortcuts for the full story.

## Jumping Around: the Focus Menu

The fastest way to move around Sonic Pi is not to walk the interface
element by element but to jump straight to where you want to be. The
*Focus* menu (and its shortcuts) teleports your focus:

* `Control-Shift-e` — the code editor
* `Control-Shift-l` — the run log
* `Control-Shift-c` — the cue log
* `Control-Shift-h` — the help docs listing (table of contents)
* `Control-Shift-d` — the help docs details (the page content itself)
* `Control-Shift-q` — the quickstart cards
* `Control-Shift-f` — the help logs
* `Control-Shift-g` — the help debug pane
* `Meta-,` — the preferences
* `Control-Shift-r` — the most recent error

You can also cycle through whichever panes are currently visible with
`F6` (and back again with `Shift-F6`).

## Writing Code

The editor is a standard accessible text area: your screen reader will
echo characters and lines as you move and type, and each buffer
announces itself by name when you switch to it.

The autocomplete popup is designed to stay out of your way: it never
steals focus from the editor, so your typing echo is uninterrupted. When
completions are available, press the down arrow to step through them —
each one is announced with its name, its category (synth, sample, FX and
so on) and its position in the list. Press Return to accept, and the
characters that were inserted are read out so you always know the exact
spelling. If you'd like the full documentation for the selected
completion, press `Control-i` and it will be spoken in full.

When you run or stop code, that's announced too. If you'd rather not
have "Run" and "Stop" spoken over the start of your music, you can turn
just those announcements off with *Speak Run and Stop* in the
Accessibility menu — errors are always spoken.

## Reading the Documentation

Open the help system with the Help toggle (or jump straight in with
`Control-Shift-h`). You land in the table of contents: plain up and down
arrows move through the chapters, and each page is announced as it
loads. `Control-Shift-d` then places you on the page content itself.

Documentation pages read like a web page: the text carries a real cursor,
so the plain arrow keys move through it — left and right by character,
up and down by line, with word-by-word movement and Home/End also
available. You can re-read or spell out anything, and holding Shift
selects as you go for copying. When you reach the end of one paragraph
or code block, the cursor flows on to the next, so a whole chapter reads
as one continuous document. Headings are exposed as real headings, so
your screen reader's heading navigation can skim from section to
section.

If the cursor crosses a link you'll hear "Link" — press Return to
follow it.

Every chapter ends with Previous and Next buttons, and they wrap around:
Next from the final chapter returns to the first, so there's never a
dead end. The topic filter box above the table of contents announces how
many topics match as you type.

## The Cards

The quickstart cards (`Control-Shift-q`, or *Examples → Quickstart
Cards...*) are small playable snippets of music code. Focus lands
directly on the first card, which announces its name, its position in
the deck and what it does.

While you're on a card:

* `Space` plays or stops the card
* `C` reads the card's code aloud
* `I` inserts the card's code into your editor at the cursor
* Left and Right arrows move through the deck, wrapping at the ends
  (you'll hear when you've wrapped)

Stepping inside a card with your screen reader reads its contents in
order: title, code, description, then the Run, Add and Copy buttons.

## The Synth and FX Playgrounds

Each synth and FX documentation page opens with a playable instrument: a
panel of knobs for its options, a piano you can play with the QWERTY keys
(the bottom row `a s d f g h j k l` are the white notes, with the black
notes on the row above), and a code snippet that rewrites itself as you
turn the knobs.

The knob panel is a single Tab stop, however many knobs the instrument
has. Tab into it and you land on the first knob, which announces its
name and value. Then:

* Left and Right arrows move between the knobs
* Up and Down arrows turn the focused knob, with the new value spoken
* Shift with an arrow makes bigger jumps
* Tab moves on past the whole panel to the code snippet

The snippet below the knobs is the code you'd write to get the sound you
just dialled in — read it line by line, run it with the Run button, or
copy it out. And remember `Control-Shift-d` jumps straight to the page
content from anywhere, so you're never more than one shortcut from the
code.

## The Examples

Choosing an example from the *Examples* menu takes you straight to its
code, which reads line by line with the arrow keys just like an editor
buffer. *Browse Examples in Help...* places you in the examples list so
you can arrow through them all, with each one loading as you go.

## Preferences

The Preferences toggle (or `Meta-,`) takes you directly into the
preferences panel: its tabs, checkboxes and controls all announce their
names and states. This is also where you'll find the audio, look and
feel, and accessibility-related options — including visual theme and
contrast choices.

## If You Get Lost

Focus is never intentionally dropped: opening a pane moves you into it,
closing it returns you to the editor, and navigating between
documentation pages keeps you on the content. If you do find yourself
somewhere unexpected, `Control-Shift-e` always brings you home to the
editor, and `F6` walks you through everything currently on screen.

Found something that doesn't speak, or doesn't behave as this section
promises? That's a bug — we'd love to hear about it at
https://in-thread.sonic-pi.net so we can fix it.

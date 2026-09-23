# The GUI's components

One implementation of each thing the interface shows more than once, used everywhere it appears: in the app's
panes, on the site's pages and in the web tutorial, on a desktop, an iPad and a phone. A component is a module and
a stylesheet of the same name; nothing else styles its parts.

| Component | Module | Stylesheet | Where it stands |
| --- | --- | --- | --- |
| The code card | `card.js` (`createCard`) | `card.css` | the quickstart pane, the docs pane and the app tutorial, the site's hero and examples, the web tutorial's blocks |
| The deck | `deck.js` (`createDeck`) | — | wherever cards are: one plays at a time, on a scope slot of its own, its runs one of the runtime's groups (Scheduler#stop_group) — Play again redefines, Stop fades and ends the group and no other, and the runtime says when it is over |
| The event strip | `strip.js` (`createEventStrip`) | (in `card.css`: `.qs-strip`) | a card's footer: its own run's sounds scrolling by, a small piano roll on the Threads pane's model |
| The submenu | `submenu.js` (`createSubmenu`) | `submenu.css` | the docs pane's topics (with its filter), a site page's sections, the examples by level, the web tutorial's contents |

Also theirs: `../fold.js` (the fold a submenu makes on a phone) and `../icons.js` (Tabler's glyphs).

## The contract

- **Colours are the theme's tokens** (`theme.js`: the scheme's colours and the roles derived from them — the
  surface ladder `--surface1/2/3`, `--softForeground`, `--accentTint` …). A component never names a colour.
- **Sizes a host may set are custom properties** with defaults in the stylesheet: the card's `--qs-code-size`
  and `--qs-title-size`. A pane sets them on itself (`style.css`, `site.css`), never by restyling the parts.
- **Form factors are the component's business.** The card shrinks its scope and grows its buttons under 760px;
  the submenu is a column beside the content on a wide screen and, under an 820px container, a strip at the top
  with the list folding open over the page. The host places the component; the component lays itself out.
- **A shadow root gets the stylesheet inlined** (`info.js` imports it as text); the document gets it in `app.css`
  (`scripts/build-app.mjs` appends every `ui/*.css`). Both are the same file.
- **The session talks to decks through one face**: `groups(live)`, `error(r)`, `flash(job, line)`, `record(r)`,
  `release(job)`, `owns(job)`, `stop()`, `playing` — `main.js` treats the docs pane, the quickstart pane and the site's pages alike.
- **A card's block is its editor, still.** The code at rest is drawn in CodeMirror's own tree of elements and token
  classes (`highlight.js` `renderLines`), and `card.css` lays out both by the same rules, so the Edit toggle moves
  nothing and the colours never differ.

## Adding one

Write `thing.js` exporting `createThing(options) → { el, … }` and `thing.css` owning every `.thing-*` rule, add a
row above, and replace each hand-made copy with a call. Test it on a desktop, an iPad and a phone before merging:
`scripts/browser-check.mjs` and the probes cover the first and last.

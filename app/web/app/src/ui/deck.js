// SPDX-License-Identifier: AGPL-3.0-or-later
// A deck: the cards (./card.js) in one place, of which one plays at a time, on a scope slot of its own — the
// quickstart pane's, the docs pane's, a site page's, the web tutorial's. A playing card is a group of the
// runtime's (Scheduler#stop_group): every run Play starts from the card goes into the card's group, so Play
// again runs the code again in it and a live loop the first run started takes the new code, and Stop stops the
// group — its threads now, its sounds faded out and freed — and nothing else. Whether the group is still live
// (a thread, an fx or a sound of it) is the runtime's to say, in the session's status; the deck only reads it.
//
//   const deck = createDeck({ play, stopGroup, group, scopeFrame }, host);
//   deck.add(card);            // and deck.detach() before the cards are replaced: a playing one is adopted back by its key
//   deck.groups(live); deck.error(r); deck.flash(job, line); deck.record(r); deck.release(job); deck.owns(job)   // from the session (main.js)
const SLOTS = 8;   // scope slots the cards share, below the live loops' (10 up), as native's cards have
// A card's program runs wrapped in a with_fx :scope_out line (main.js play, for the card's own slot), so the
// runtime's line numbers stand one above the card's: the wrap's line comes off before a line is shown
const WRAP_LINES = 1;
const FADE = 0.25;   // seconds a card's stop takes to turn its sounds down

/**
 * @param hooks { play(code, {scopeSlot, group}) → Promise<job|null>, stopGroup(group, fade), group() → a fresh group id, scopeFrame(slot, n) → frame|null }
 * @param host  the element whose Escape stops the playing card (the pane, the page)
 */
export function createDeck(hooks, host = null) {
  const cards = [];
  let playing = null;     // { card, key, group, jobs: Set of the runs started (for a record's card), last, slot, starting: runs whose head is going }
  let lastError = null;   // an error that came before its card's play returned
  let nextSlot = 0;

  const readFrame = () => (playing ? hooks.scopeFrame?.(playing.slot, 1024) ?? null : null);
  const owns = (job) => !!playing && job != null && playing.jobs.has(job);

  async function play(card) {
    if (playing && playing.card !== card) stop();   // one card at a time
    card.setError(null);
    card.setBooting(true);
    if (!playing) card.clearOutput?.();   // a run from rest starts a fresh page of output; a run again adds to it
    const g = playing ?? (playing = { card, key: card.key, group: hooks.group(), jobs: new Set(), last: null, slot: 1 + (nextSlot++ % SLOTS), starting: 0 });
    g.starting++;
    let job = null;
    try { job = await hooks.play(card.code(), { scopeSlot: g.slot, group: g.group }); } catch (e) { card.setError(String(e?.message ?? e)); }
    card.setBooting(false);
    g.starting--;
    if (job == null) {
      card.setError(card.el.querySelector(".qs-state").textContent || "The program did not start");
      if (playing === g && !g.jobs.size) { playing = null; card.setPlaying(false); }
      return;
    }
    if (playing !== g) { hooks.stopGroup(g.group, 0); return; }   // stopped while its head ran
    g.jobs.add(job);
    g.last = job;
    card.job = job;
    card.setPlaying(true, readFrame);
    if (lastError && owns(lastError.job)) { const r = lastError; lastError = null; api.error(r); }   // it failed in its head, before play returned
  }

  function stop() {
    if (!playing) return;
    const p = playing;
    hooks.stopGroup(p.group, FADE);
    playing = null;
    p.card.job = null;
    p.card.setPlaying(false);
  }

  const api = {
    cards,
    /** A card joins the deck: its transport is the deck's. A card re-made while playing (a re-render) is adopted by its key. */
    add(card) {
      cards.push(card);
      card.onRun = () => play(card);   // and again while playing: the code runs again, a live loop taking the new code
      card.onStop = () => { if (playing?.card === card) stop(); };
      card.onReset = () => { if (playing?.card === card) play(card); };   // heard as well as seen
      if (playing && playing.key === card.key && playing.card !== card) { playing.card = card; card.job = playing.last; card.setPlaying(true, readFrame); }
      return card;
    },
    /** The cards are about to be replaced: their rings stop; a playing group carries on, for its card to be adopted back. */
    detach() { for (const c of cards) c.detach(); cards.length = 0; },
    /** The card known by this key. */
    find(key) { return cards.find((c) => c.key === key) ?? null; },
    /** The session's live groups (its status): the playing card's group gone from them is a card that has finished. */
    groups(live) {
      if (!playing || playing.starting || live.includes(playing.group)) return;
      const p = playing;
      playing = null; p.card.job = null; p.card.setPlaying(false);
    },
    /** An error in a card's program shows on the card. */
    error(r) {
      if (!playing || (r.job != null && !owns(r.job))) { lastError = r; return; }   // kept for the card whose play has not returned yet
      const line = r.line > WRAP_LINES ? r.line - WRAP_LINES : 0;
      playing.card.setError(`${r.class ? `${r.class}: ` : ""}${String(r.message ?? "").split("\n")[0]}${line ? ` (line ${line})` : ""}`);
    },
    /** A sound of this job's, from this line: the playing card flashes it. */
    flash(job, line) { if (owns(job) && line > WRAP_LINES) playing.card.flash(line - WRAP_LINES); },
    /** A record of one of the playing card's runs (or of a run just starting): its strip draws it. */
    record(r) { if (playing && (owns(r.job) || (playing.starting && r.job != null))) playing.card.record(r); },
    /** Whether this job is one of the playing card's. */
    owns,
    /** Whether a run of the playing card's is starting, its job not yet known: a sound of its head may arrive first. */
    get starting() { return !!playing?.starting; },
    /** The card whose group this job is in lets it go, sounding on: the editor has it now. */
    release(job) { if (owns(job)) { playing.card.job = null; playing.card.setPlaying(false); playing = null; } },
    stop,
    /** The playing card's latest run, or null. */
    get playing() { return playing?.last ?? null; },
  };
  host?.addEventListener("keydown", (e) => { if (e.key === "Escape" && playing) stop(); });
  return api;
}

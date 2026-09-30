// SPDX-License-Identifier: AGPL-3.0-or-later
// A deck: the cards (./card.js) in one place, of which one plays at a time, on a scope slot of its own — the
// quickstart pane's, the docs pane's, a site page's, the web tutorial's. A playing card is its runs: every Play is
// a run of the runtime's, there from its start to its end as any run is, so Play again runs the code again and a
// live loop the first run started takes the new code, moving under the new run as it does (Scheduler#move_loop).
// Stop stops each run the card started, as a subtree (Scheduler#stop_run) — its threads now, its sounds faded out
// and freed — and nothing else. The card has no group of its own: a group made for each play was there for good,
// fourteen plays fourteen groups in the Threads pane. Whether a run is still live (a thread, an fx or a sound
// under it) is the runtime's to say, in the session's status; the deck only reads it.
//
//   const deck = createDeck({ play, stopRun, scopeFrame }, host);
//   deck.add(card);            // and deck.detach() before the cards are replaced: a playing one is adopted back by its key
//   deck.runs(live); deck.error(r); deck.flash(job, line); deck.record(r); deck.release(job); deck.owns(job)   // from the session (main.js)
const SLOTS = 8;   // scope slots the cards share, below the live loops' (10 up), as native's cards have
// A card's program runs wrapped in a with_fx :scope_out line (main.js play, for the card's own slot), so the
// runtime's line numbers stand one above the card's: the wrap's line comes off before a line is shown
const WRAP_LINES = 1;
const FADE = 0.25;   // seconds a card's stop takes to turn its sounds down

/**
 * @param hooks { play(code, {scopeSlot}) → Promise<job|null>, stopRun(job, fade), scopeFrame(slot, n) → frame|null }
 * @param host  the element whose Escape stops the playing card (the pane, the page)
 */
export function createDeck(hooks, host = null) {
  const cards = [];
  let playing = null;     // { card, key, jobs: Set of the runs started (the card's to stop, and a record's card), last, slot, starting: runs whose head is going }
  let lastError = null;   // an error that came before its card's play returned
  let nextSlot = 0;

  const readFrame = () => (playing ? hooks.scopeFrame?.(playing.slot, 1024) ?? null : null);
  const owns = (job) => !!playing && job != null && playing.jobs.has(job);

  async function play(card) {
    if (playing && playing.card !== card) stop();   // one card at a time
    card.setError(null);
    card.setBooting(true);
    if (!playing) card.clearOutput?.();   // a run from rest starts a fresh page of output; a run again adds to it
    const g = playing ?? (playing = { card, key: card.key, jobs: new Set(), last: null, slot: 1 + (nextSlot++ % SLOTS), starting: 0 });
    g.starting++;
    let job = null;
    try { job = await hooks.play(card.code(), { scopeSlot: g.slot }); } catch (e) { card.setError(String(e?.message ?? e)); }
    card.setBooting(false);
    g.starting--;
    if (job == null) {
      card.setError(card.el.querySelector(".qs-state").textContent || "The program did not start");
      if (playing === g && !g.jobs.size) { playing = null; card.setPlaying(false); }
      return;
    }
    if (playing !== g) { hooks.stopRun(job, 0); return; }   // stopped while its head ran
    g.jobs.add(job);
    g.last = job;
    card.job = job;
    card.setPlaying(true, readFrame);
    if (lastError && owns(lastError.job)) { const r = lastError; lastError = null; api.error(r); }   // it failed in its head, before play returned
  }

  function stop() {
    if (!playing) return;
    const p = playing;
    for (const job of p.jobs) hooks.stopRun(job, FADE);   // each run it started: one that is over is nothing to stop
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
    /** The session's live runs (its status): none of the playing card's among them is a card that has finished. */
    runs(live) {
      if (!playing || playing.starting || live.some((job) => playing.jobs.has(job))) return;
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
    /** The card whose run this job is lets it go, sounding on: the editor has it now. */
    release(job) { if (owns(job)) { playing.card.job = null; playing.card.setPlaying(false); playing = null; } },
    stop,
    /** The playing card's latest run, or null. */
    get playing() { return playing?.last ?? null; },
  };
  host?.addEventListener("keydown", (e) => { if (e.key === "Escape" && playing) stop(); });
  return api;
}

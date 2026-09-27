// SPDX-License-Identifier: AGPL-3.0-or-later
// The audio's trail: what happened to the sound, kept on every page, so that when it goes and will not come back
// there is something to find out why from. The page hidden and shown, the audio context's states, the engine's,
// each tap on Resume and what it found, each reload of the engine and how it ended — each a line with its time, the
// last few hundred of them. Cheap: nothing is sampled, a line is written only as something happens.
//
// A trail ending in a card that cannot bring the audio back (Restart Sonic Pi) would go with the reload that button
// does, the one moment it is wanted. So it is kept (keep) in the tab's sessionStorage with the logs' last lines, and
// the page that follows takes it (taken): into its Logs pane, under "before the restart", and its flight report.
//
//   const trail = createAudioTrail();
//   const before = trail.taken();        // the last page's, if it kept one: once, then gone
//   trail.note("context", { state });    // something happened
//   trail.keep({ logs, state });         // for the page after this one
//   trail.lines                          // this page's, for a report

const KEY = "sp-audio-trail";
const LIMIT = 300;

export function createAudioTrail() {
  const lines = [];
  return {
    note(what, detail = {}) {
      lines.push({ at: Math.round(performance.now()), time: new Date().toISOString(), what, ...detail });
      if (lines.length > LIMIT) lines.shift();
    },
    keep(extra = {}) {
      try { sessionStorage.setItem(KEY, JSON.stringify({ kept: new Date().toISOString(), trail: lines, ...extra })); } catch { /* storage refused: nothing kept */ }
    },
    taken() {
      try {
        const kept = sessionStorage.getItem(KEY);
        sessionStorage.removeItem(KEY);
        return kept ? JSON.parse(kept) : null;
      } catch { return null; }
    },
    get lines() { return lines.slice(); },
  };
}

/** A trail's line as the Logs pane shows it: its time, what happened, and the rest as key=value. */
export function trailText({ time, what, at, ...rest }) {
  const detail = Object.entries(rest).map(([k, v]) => `${k}=${typeof v === "string" ? v : JSON.stringify(v)}`).join(" ");
  return `${time.slice(11, 23)} ${what}${detail ? ` ${detail}` : ""}`;
}

// SPDX-License-Identifier: AGPL-3.0-or-later
// Piano roll: the notes a running program plays.
// A keyboard down the side, a row per semitone, time across with the clock.
// Each note is a bar on its pitch's row, as long as its envelope, bright
// while held and fading through its release; a slide bends the bar to its
// new pitch and a kill cuts it short. Samples get drum-map rows beneath.
// Keys light as their notes sound, in the colour of the thread playing them.
import { css } from "./theme.js";
import { animateWhileShown } from "./ui/shown.js";
import { perfAdd } from "./perf.js";

const NAMES = ["C", "C♯", "D", "E♭", "E", "F", "F♯", "G", "A♭", "A", "B♭", "B"];
const BLACK = new Set([1, 3, 6, 8, 10]);
const KEEP_SECS = 40;       // longer than the widest window
const MAX_NOTES = 4000;
const SAMPLE_SECS = 0.12;   // a hit's width: a sample has no pitch or note length to show
const MIDI_OPEN_SECS = 8;   // how far a note_on with no note_off yet may reach
const KEYS_W = 56;
const TOP = 16;
const DRUM_ROW_H = 14;
const MAX_DRUM_ROWS = 8;
// A keyboard in the theme's own ink rather than a grey slab in the middle of it: the long keys are the
// foreground dimmed to a surface, the short ones the window's own dark, and the seam between them the border
// every other panel uses. It still reads as a keyboard — long light, short dark — in all six schemes.
const keyColours = () => ({
  white: css("faintForeground"),
  black: css("Background"),
  seam: css("WindowBorder"),
  label: css("Background"),
});

const pc = (n) => ((Math.round(n) % 12) + 12) % 12;

/** A MIDI note as a musician names it: 60 → C4, 60.25 → C4+25¢. */
export function noteName(n) {
  const r = Math.round(n);
  const cents = Math.round((n - r) * 100);
  return `${NAMES[pc(r)]}${Math.floor(r / 12) - 1}${cents ? `${cents > 0 ? "+" : ""}${cents}¢` : ""}`;
}

export const isBlack = (n) => BLACK.has(pc(n));

/** The range to show, held: grown at once to take a new note in, shrunk only once the room it had has gone unused
 * for `hold` seconds, so a note scrolling off the left edge does not rescale every row. `held` is the last call's
 * result (null the first time); `want` the range the notes in view need now (pitchRange); `t` seconds. */
export function holdRange(held, want, t, hold = 6) {
  if (!held) return { lo: want[0], hi: want[1], loAt: t, hiAt: t };
  const h = { ...held };
  if (want[0] <= h.lo || t - h.loAt > hold) { h.lo = want[0]; h.loAt = t; }
  if (want[1] >= h.hi || t - h.hiAt > hold) { h.hi = want[1]; h.hiAt = t; }
  return h;
}

/** The rows to show for these pitches: padded, at least `min` semitones, within MIDI's range. */
export function pitchRange(pitches, { pad = 2, min = 18 } = {}) {
  if (!pitches.length) return [48, 72];
  let lo = Math.floor(Math.min(...pitches)) - pad;
  let hi = Math.ceil(Math.max(...pitches)) + pad;
  if (hi - lo < min) {
    const grow = min - (hi - lo);
    lo -= Math.floor(grow / 2);
    hi += Math.ceil(grow / 2);
  }
  if (lo < 0) { hi -= lo; lo = 0; }
  if (hi > 127) { lo -= hi - 127; hi = 127; }
  return [Math.max(0, lo), Math.min(127, hi)];
}

/** Lines at each whole beat of a thread's clock between two times; every fourth is a bar. */
export function beatGrid(anchor, start, end, max = 512) {
  if (!anchor || !(anchor.bpm > 0)) return [];
  const step = 60 / anchor.bpm;
  const lines = [];
  for (let b = Math.ceil(anchor.beat + (start - anchor.time) / step - 1e-9); lines.length < max; b++) {
    const time = anchor.time + (b - anchor.beat) * step;
    if (time > end) break;
    lines.push({ time, beat: b, bar: ((b % 4) + 4) % 4 === 0 });
  }
  return lines;
}

const num = (v) => (typeof v === "number" && Number.isFinite(v) ? v : null);

/**
 * The notes and hits of a session, from its live records.
 * @param defaults synth name (sonic-pi-…) → its documented opts' defaults, in beats, or null
 */
export function createRollModel({ defaults = () => null } = {}) {
  let notes = [];
  let hits = [];
  const byNode = new Map();
  const openMidi = new Map();
  const bpmOf = new Map();
  const pending = new Map();   // thread → notes whose tempo its next sleep will tell
  const parentOf = new Map();  // thread → [its parent, its name], while it runs
  const helpers = new Map();   // a thread that only loads a sample → the thread it plays for
  let anchor = null;
  let count = 0;

  /** When a note starts, stops being held, and ends. */
  function span(n, now = Infinity) {
    if (n.midi) {
      const end = n.off ?? Math.min(Math.max(n.start, now), n.start + MIDI_OPEN_SECS);
      return { start: n.start, hold: end, end };
    }
    // the envelope the program gave is in seconds already; the synth's defaults are in beats
    const k = 60 / (n.bpm ?? bpmOf.get(n.thread) ?? 60);
    const part = (name) => n.env[name] ?? (n.beats[name] ?? 0) * k;
    let hold = n.start + part("attack") + part("decay") + part("sustain");
    let end = hold + part("release");
    if (n.killed != null) {
      end = Math.min(end, Math.max(n.start, n.killed));
      hold = Math.min(hold, end);
    }
    return { start: n.start, hold, end };
  }

  /** A note's pitch over time, to its end: [{t, n}], bending where it slides. */
  function path(n, end) {
    let cur = n.segs[0].note;
    const pts = [{ t: n.start, n: cur }];
    for (let i = 1; i < n.segs.length; i++) {
      const s = n.segs[i];
      if (s.at >= end) break;
      pts.push({ t: Math.max(n.start, s.at), n: cur });
      // a note that ends mid-slide ends part way there
      const reached = s.slide > 0 ? Math.min(1, (end - s.at) / s.slide) : 1;
      cur += (s.note - cur) * reached;
      pts.push({ t: s.at + (s.slide || 0) * reached, n: cur });
    }
    if (end > pts[pts.length - 1].t) pts.push({ t: end, n: cur });
    return pts;
  }

  function prune(now) {
    const keep = now - KEEP_SECS;
    notes = notes.filter((n) => span(n).end >= keep || (n.midi && n.off == null && n.start >= keep - MIDI_OPEN_SECS)).slice(-MAX_NOTES);
    hits = hits.filter((h) => h.time >= keep).slice(-MAX_NOTES);
    const live = new Set(notes);
    for (const [k, n] of byNode) if (!live.has(n)) byNode.delete(k);
    for (const [k, n] of openMidi) if (!live.has(n)) openMidi.delete(k);
    for (const [k, list] of pending) {
      const still = list.filter((n) => live.has(n));
      still.length ? pending.set(k, still) : pending.delete(k);
    }
  }

  // Stop, and what was already on its way: the runtime schedules ahead, so a sound due after the Stop can have been
  // sent before it and arrive just after it. A run's jobs are numbered in order, so a record of a job the Stop had
  // seen, due after the Stop, is one that will never sound: it is not drawn.
  let stopAt = null, stopJob = -Infinity, lastJob = -Infinity;
  function record(r) {
    if (r.thread == null) return;
    if (r.job != null) {
      if (stopAt != null && r.job <= stopJob && r.time > stopAt) return;
      lastJob = Math.max(lastJob, r.job);
    }
    if (r.kind === "thread") {
      if (r.event === "start") parentOf.set(r.thread, [r.parent, r.name]);
      else { parentOf.delete(r.thread); helpers.delete(r.thread); }
      return;
    }
    // the first sound of a sample not yet loaded plays from a helper thread, as
    // in Sonic Pi: it is the sound of the thread that asked for it
    if (r.kind === "sample_load") {
      const parent = parentOf.get(r.thread)?.[0];
      if (parent != null && parentOf.has(parent)) helpers.set(r.thread, parent);
      return;
    }
    const a = r.args || {};
    const thread = helpers.get(r.thread) ?? r.thread;
    const who = { thread, name: thread === r.thread ? r.name : parentOf.get(thread)[1], job: r.job, line: r.line };
    switch (r.kind) {
      case "synth": {
        if (r.synth.startsWith("sonic-pi-fx_")) return;
        const synth = r.synth.replace(/^sonic-pi-/, "");
        if (a.buf != null) {
          hits.push({ ...who, sample: String(a.buf).replace(/\.(flac|wav|aiff?|ogg)$/i, ""), time: r.time, amp: num(a.amp) ?? 1 });
          break;
        }
        const d = defaults(r.synth) || {};
        const note = num(a.note) ?? num(d.note);
        if (note == null) {
          hits.push({ ...who, sample: `:${synth}`, time: r.time, amp: num(a.amp) ?? 1 });
          break;
        }
        const env = {}, beats = {};
        for (const k of ["attack", "decay", "sustain", "release"]) {
          if (num(a[k]) != null) env[k] = Math.max(0, a[k]);
          else beats[k] = Math.max(0, num(d[k]) ?? (k === "release" ? 1 : 0));
        }
        const n = { ...who, synth, start: r.time, env, beats, bpm: null, amp: num(a.amp) ?? num(d.amp) ?? 1, slide: num(a.note_slide) ?? 0, segs: [{ at: r.time, note }], killed: null };
        notes.push(n);
        const list = pending.get(r.thread) ?? [];
        if (list.length < 256) list.push(n);
        pending.set(r.thread, list);
        if (r.node != null) byNode.set(r.node, n);
        break;
      }
      case "control": {
        const n = byNode.get(r.node);
        if (!n) break;
        if (num(a.note_slide) != null) n.slide = a.note_slide;
        if (num(a.note) != null) n.segs.push({ at: r.time, note: a.note, slide: n.slide });
        break;
      }
      case "kill": {
        const n = byNode.get(r.node);
        if (n) n.killed = Math.min(n.killed ?? Infinity, r.time);
        break;
      }
      case "midi": {
        if (r.path !== "/note_on" && r.path !== "/note_off") break;
        const [, channel, note, velocity] = r.args || [];
        if (num(note) == null) break;
        const key = `${r.job}:${channel}:${note}`;
        const open = openMidi.get(key);
        if (open) { open.off = r.time; openMidi.delete(key); }
        if (r.path === "/note_on" && (velocity ?? 1) > 0) {
          const n = { ...who, midi: true, synth: `midi ch ${channel === -1 ? "all" : channel}`, start: r.time, amp: (velocity ?? 127) / 127, segs: [{ at: r.time, note }], off: null };
          notes.push(n);
          openMidi.set(key, n);
        }
        break;
      }
      case "sleep": {
        const secs = r.until - r.t;
        if (!(secs > 0 && r.beats > 0)) break;
        const bpm = (r.beats * 60) / secs;
        bpmOf.set(r.thread, bpm);
        // a sleep tells the tempo the notes just before it were played at
        for (const n of pending.get(r.thread) ?? []) n.bpm = bpm;
        pending.delete(r.thread);
        // the grid follows one thread's beats: the first to sleep, until it stops
        if (!anchor || anchor.thread === r.thread || r.time - anchor.seen > 4) {
          anchor = { thread: r.thread, name: r.name, time: r.time, beat: r.beat - r.beats, bpm, seen: r.time };
        }
        break;
      }
      default: return;
    }
    // The sweep every 256 records is the cheap cadence. The cap is held to here by dropping the oldest, not by
    // sweeping: a controller reaches MAX_NOTES in seconds (KEEP_SECS of notes at a knob's rate is several times
    // MAX_NOTES), and a sweep there would run on every record. What the drop leaves behind in byNode and the rest
    // goes on the next sweep, at most 256 later.
    if (++count % 256 === 0) prune(r.time);
    else if (notes.length > MAX_NOTES) { notes = notes.slice(-MAX_NOTES); hits = hits.slice(-MAX_NOTES); }
  }

  /** Stop: what was scheduled after it never sounds, and what was sounding ends. */
  function stop(time) {
    stopAt = time;
    stopJob = lastJob;
    notes = notes.filter((n) => n.start <= time);
    hits = hits.filter((h) => h.time <= time);
    for (const n of notes) {
      if (n.midi) n.off ??= time;
      else if (span(n).end > time) n.killed = Math.min(n.killed ?? Infinity, time);
    }
    byNode.clear();
    openMidi.clear();
    pending.clear();
  }

  return {
    record,
    stop,
    span,
    path,
    notes: () => notes,
    hits: () => hits,
    anchor: () => anchor,
    clear() {
      notes = [];
      hits = [];
      byNode.clear();
      openMidi.clear();
      pending.clear();
      parentOf.clear();
      helpers.clear();
      anchor = null;
    },
  };
}

// ── The view ────────────────────────────────────────────────────────────────

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};
const round = (v, places = 2) => String(Math.round(v * 10 ** places) / 10 ** places);

/** Where along a polyline of [x, y] points a coordinate falls. */
function along(pts, v, from = 0, to = 1) {
  for (let i = 1; i < pts.length; i++) {
    const p = pts[i - 1], q = pts[i];
    if (v <= q[from]) return q[from] === p[from] ? q[to] : p[to] + (q[to] - p[to]) * Math.max(0, Math.min(1, (v - p[from]) / (q[from] - p[from])));
  }
  return pts[pts.length - 1][to];
}

/**
 * @param root element to fill
 * @param hooks { now() → the clock in seconds or null; still() → paused: the view held as it is; windowSecs(); colourOf(thread); label(thread, name);
 *                jump(line, job); defaults(synth) → the synth's opt defaults }
 */
export function createPianoRoll(root, hooks) {
  const model = createRollModel({ defaults: hooks.defaults });
  const hidden = new Set();
  root.textContent = "";
  root.classList.add("insight-roll");
  const head = el("div", "roll-threads");
  const chips = el("div", "roll-chips");
  const info = el("span", "roll-info");
  head.append(chips, info);
  const wrap = el("div", "roll-canvas");
  const canvas = el("canvas");
  canvas.setAttribute("role", "img");
  canvas.setAttribute("aria-label", "Piano roll: the notes each thread plays, by pitch and time");
  const tip = el("div", "roll-tip");
  tip.hidden = true;
  wrap.append(canvas, tip);
  root.append(head, wrap);

  let lo = 48, hi = 72;          // the rows shown, easing toward the notes in view
  let held = null;               // the range the notes have needed lately (holdRange)
  let drums = new Map();         // each sample's drum row, and when it was last in view: held as the range is
  let drumH = 0;                 // the drum rows' height, easing as the pitch rows do
  const HOLD_SECS = 10;   // past the view's own few seconds: a note a loop plays each bar keeps its room
  let frame = { notes: [], hits: [], rows: [], lit: [], range: [lo, hi], bpm: null };
  let mouse = null;
  const tints = new Map();

  /** A theme colour at an alpha, for gradients. */
  function tint(ctx, colour, alpha) {
    let rgb = tints.get(colour);
    if (!rgb) {
      ctx.fillStyle = "#000";
      ctx.fillStyle = colour;
      const c = ctx.fillStyle;
      rgb = c.startsWith("#") ? [1, 3, 5].map((i) => parseInt(c.slice(i, i + 2), 16)).join(", ") : c.replace(/^rgba?\(|\)$/g, "").split(",").slice(0, 3).join(",");
      tints.set(colour, rgb);
    }
    return `rgba(${rgb}, ${Math.max(0, Math.min(1, alpha))})`;
  }

  function hitAt(mx, my) {
    for (let i = frame.hits.length - 1; i >= 0; i--) {
      const h = frame.hits[i];
      if (mx >= h.x0 - 2 && mx <= h.x1 + 2 && Math.abs(my - h.y) <= DRUM_ROW_H / 2) return h;
    }
    for (let i = frame.notes.length - 1; i >= 0; i--) {
      const f = frame.notes[i];
      if (mx < f.x0 - 2 || mx > f.x1 + 2) continue;
      if (Math.abs(my - along(f.pts, mx)) <= Math.max(4, f.half + 1)) return f;
    }
    return null;
  }

  function describe(f) {
    const where = `${f.label}${f.line ? ` · line ${f.line}` : ""}`;
    if (f.sample != null) return `${f.sample.startsWith(":") ? f.sample : `:${f.sample}`} · amp ${round(f.amp)} · ${where}`;
    const pitches = f.notes.filter((n, i) => i === 0 || n !== f.notes[i - 1]).map((n) => `${noteName(n)} (${round(n)})`).join(" → ");
    return `${pitches} · ${f.midi ? f.synth : `:${f.synth}`} · ${f.midi ? "vel" : "amp"} ${f.midi ? Math.round(f.amp * 127) : round(f.amp)} · ${round(f.end - f.start)} s · ${where}`;
  }

  function showTip() {
    const f = mouse && hitAt(mouse.x, mouse.y);
    canvas.style.cursor = f?.line ? "pointer" : "";
    if (!f) { tip.hidden = true; return; }
    tip.textContent = describe(f);
    tip.hidden = false;
    const w = wrap.clientWidth, h = wrap.clientHeight;
    tip.style.left = `${Math.max(4, Math.min(mouse.x + 12, w - tip.offsetWidth - 4))}px`;
    tip.style.top = `${mouse.y + 16 + tip.offsetHeight > h ? mouse.y - tip.offsetHeight - 8 : mouse.y + 16}px`;
  }

  canvas.addEventListener("mousemove", (e) => {
    const b = canvas.getBoundingClientRect();
    mouse = { x: e.clientX - b.left, y: e.clientY - b.top };
    showTip();
  });
  canvas.addEventListener("mouseleave", () => { mouse = null; showTip(); });
  canvas.addEventListener("click", (e) => {
    const b = canvas.getBoundingClientRect();
    const f = hitAt(e.clientX - b.left, e.clientY - b.top);
    if (f?.line) hooks.jump(f.line, f.job);
  });

  let chipsKey = "";
  function renderChips() {
    const now = hooks.now();
    const anchor = model.anchor();
    info.textContent = anchor ? `${round(anchor.bpm, 1)} bpm · bars counted by ${hooks.label(anchor.thread, anchor.name)}` : "";
    const since = now == null ? -Infinity : now - hooks.windowSecs() * 0.8;   // the past the roll shows
    const ids = new Map();
    for (const n of model.notes()) if (model.span(n, now ?? Infinity).end >= since) ids.set(n.thread, n.name);
    for (const h of model.hits()) if (h.time >= since && !ids.has(h.thread)) ids.set(h.thread, h.name);
    const list = [...ids].map(([id, name]) => ({ id, label: hooks.label(id, name), colour: hooks.colourOf(id), off: hidden.has(id) }));
    const key = JSON.stringify(list);
    if (key === chipsKey) return;
    chipsKey = key;
    chips.textContent = "";
    if (!list.length) {
      chips.appendChild(el("span", "roll-hint", "Each thread that plays gets a colour here: click one to hide it."));
      return;
    }
    for (const t of list) {
      const b = el("button", `roll-chip${t.off ? " off" : ""}`);
      b.type = "button";
      b.title = t.off ? `Show ${t.label}` : `Hide ${t.label}`;
      const dot = el("i");
      dot.style.background = t.colour;
      b.append(dot, el("span", "", t.label));
      b.addEventListener("click", () => {
        hidden.has(t.id) ? hidden.delete(t.id) : hidden.add(t.id);
        chipsKey = "";
        renderChips();
      });
      chips.appendChild(b);
    }
  }

  function drawFrame() {
    const dpr = window.devicePixelRatio || 1;
    const w = wrap.clientWidth, h = wrap.clientHeight;
    if (!w || !h) return;
    if (canvas.width !== Math.round(w * dpr) || canvas.height !== Math.round(h * dpr)) {
      canvas.width = Math.round(w * dpr);
      canvas.height = Math.round(h * dpr);
      canvas.style.width = `${w}px`;
      canvas.style.height = `${h}px`;
    }
    const ctx = canvas.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.globalAlpha = 1;
    ctx.fillStyle = css("Background");
    ctx.fillRect(0, 0, w, h);
    ctx.font = "11px ui-monospace, Menlo, monospace";
    const now = hooks.now();
    const next = { notes: [], hits: [], rows: [], lit: [], range: [lo, hi], bpm: null };
    if (now == null) {
      ctx.fillStyle = css("faintForeground");
      ctx.fillText("Press Run: the notes each thread plays will scroll past here.", KEYS_W + 12, 30);
      frame = next;
      return;
    }
    const secs = hooks.windowSecs();
    const start = now - secs * 0.8, end = start + secs;
    const plotW = w - KEYS_W - 6;
    const x = (t) => KEYS_W + ((t - start) / secs) * plotW;

    const shown = [];
    for (const n of model.notes()) {
      if (hidden.has(n.thread) || n.start > end) continue;
      const sp = model.span(n, now);
      if (sp.end < start) continue;
      shown.push({ n, sp, pts: model.path(n, sp.end) });
    }
    const hits = model.hits().filter((hh) => !hidden.has(hh.thread) && hh.time <= end && hh.time + SAMPLE_SECS >= start);
    // a sample's row stays a while after its last hit has scrolled off, and a new one joins at the foot: the rows
    // above never jump for a sample that comes and goes
    // paused (hooks.still): the view held as it is, its rows and its range with it, to be read and pointed at
    const still = !!hooks.still?.();
    const wall = performance.now() / 1000;
    if (!still) {
      for (const hh of hits) if (drums.has(hh.sample) || drums.size < MAX_DRUM_ROWS) drums.set(hh.sample, wall);
      for (const [name, at] of drums) if (wall - at > HOLD_SECS) drums.delete(name);
    }
    const rows = [...drums.keys()];
    const rowOf = new Map(rows.map((r, i) => [r, i]));
    next.rows = rows;
    const wantDrumH = rows.length ? rows.length * DRUM_ROW_H + 6 : 0;
    if (!still) drumH += (wantDrumH - drumH) * 0.15;
    if (!still && Math.abs(wantDrumH - drumH) < 0.5) drumH = wantDrumH;
    const rollBottom = h - drumH;

    const pitches = [];
    for (const s of shown) for (const p of s.pts) pitches.push(p.n);
    if (!still) held = holdRange(held, pitchRange(pitches), wall, HOLD_SECS);
    const tlo = still ? lo : held.lo, thi = still ? hi : held.hi;
    lo += (tlo - lo) * (tlo < lo ? 0.15 : 0.04);   // growing for a new note: at once; shrinking: gently
    hi += (thi - hi) * (thi > hi ? 0.15 : 0.04);
    if (Math.abs(tlo - lo) < 0.01) lo = tlo;
    if (Math.abs(thi - hi) < 0.01) hi = thi;
    next.range = [tlo, thi];
    const rowH = (rollBottom - TOP) / (hi - lo + 1);
    const yOf = (n) => TOP + (hi + 0.5 - n) * rowH;

    // rows: black keys' rows shaded, a line under each C
    ctx.save();
    ctx.beginPath();
    ctx.rect(0, TOP, w, rollBottom - TOP);
    ctx.clip();
    ctx.lineWidth = 1;
    for (let n = Math.floor(lo) - 1; n <= Math.ceil(hi) + 1; n++) {
      const y = yOf(n) - rowH / 2;
      if (isBlack(n)) {
        ctx.fillStyle = css("subtleFill");
        ctx.fillRect(KEYS_W, y, w - KEYS_W, rowH);
      }
      if (pc(n) === 0) {
        ctx.strokeStyle = css("WindowBorder");
        ctx.beginPath();
        ctx.moveTo(0, Math.round(y + rowH) + 0.5);
        ctx.lineTo(w, Math.round(y + rowH) + 0.5);
        ctx.stroke();
      }
    }
    ctx.restore();

    // the future: already scheduled, not yet heard
    ctx.fillStyle = css("subtleFill");
    ctx.fillRect(x(now), TOP, w - x(now), h - TOP);

    // beats and bars of one thread's clock, or seconds when no thread has slept yet
    const anchor = model.anchor();
    const beatPx = anchor ? (60 / anchor.bpm / secs) * plotW : 0;
    ctx.strokeStyle = css("WindowBorder");
    ctx.fillStyle = css("faintForeground");
    if (anchor && beatPx * 4 >= 6) {
      next.bpm = anchor.bpm;
      for (const l of beatGrid(anchor, start, end)) {
        if (!l.bar && beatPx < 6) continue;
        const gx = Math.round(x(l.time)) + 0.5;
        if (gx < KEYS_W) continue;
        ctx.globalAlpha = l.bar ? 1 : 0.45;
        ctx.beginPath();
        ctx.moveTo(gx, TOP - (l.bar ? 4 : 0));
        ctx.lineTo(gx, h);
        ctx.stroke();
        ctx.globalAlpha = 1;
        if (l.bar && beatPx * 4 > 28) ctx.fillText(String(Math.floor(l.beat / 4) + 1), gx + 3, 11);
      }
    } else {
      for (let s = Math.ceil(start); s < end; s++) {
        const gx = Math.round(x(s)) + 0.5;
        if (gx < KEYS_W) continue;
        ctx.beginPath();
        ctx.moveTo(gx, TOP - 4);
        ctx.lineTo(gx, h);
        ctx.stroke();
        ctx.fillText(`${s - Math.round(now) >= 0 ? "+" : ""}${Math.round(s - now)}s`, gx + 3, 11);
      }
    }

    // notes
    const thick = Math.max(2, Math.min(18, rowH * 0.8));
    const lit = new Map();
    ctx.save();
    ctx.beginPath();
    ctx.rect(KEYS_W, TOP, w - KEYS_W, rollBottom - TOP);
    ctx.clip();
    ctx.lineCap = "butt";
    ctx.lineJoin = "round";
    ctx.lineWidth = thick;
    for (const { n, sp, pts } of shown) {
      const colour = hooks.colourOf(n.thread);
      const base = (n.start > now ? 0.55 : 1) * (0.5 + 0.5 * Math.min(1, n.amp));
      const x0 = x(sp.start), x1 = Math.max(x(sp.end), x0 + 2);
      // bright while held, fading through the release
      const g = ctx.createLinearGradient(x0, 0, x1, 0);
      const held = Math.max(0, Math.min(1, (x(sp.hold) - x0) / (x1 - x0)));
      g.addColorStop(0, tint(ctx, colour, base));
      g.addColorStop(held, tint(ctx, colour, base * 0.9));
      g.addColorStop(1, tint(ctx, colour, base * 0.2));
      ctx.strokeStyle = g;
      ctx.beginPath();
      pts.forEach((p, i) => (i ? ctx.lineTo(Math.max(x(p.t), x0 + 2), yOf(p.n)) : ctx.moveTo(x0, yOf(p.n))));
      ctx.stroke();
      // the attack, crisp, so repeated notes stay apart
      const y0 = yOf(pts[0].n);
      ctx.fillStyle = colour;
      ctx.globalAlpha = n.start > now ? 0.55 : 1;
      ctx.fillRect(x0, y0 - thick / 2, 2, thick);
      if (x1 - x0 > 34 && thick >= 11) {
        ctx.fillStyle = css("Foreground");
        ctx.font = "9px ui-monospace, Menlo, monospace";
        ctx.fillText(noteName(pts[0].n), x0 + 5, y0 + 3);
        ctx.font = "11px ui-monospace, Menlo, monospace";
      }
      ctx.globalAlpha = 1;
      if (sp.start <= now && now < sp.end) {
        const pitch = Math.round(along(pts.map((p) => [p.t, p.n]), now));
        lit.set(pitch, colour);
      }
      next.notes.push({
        thread: n.thread, label: hooks.label(n.thread, n.name), job: n.job, line: n.line, synth: n.synth, midi: !!n.midi, amp: n.amp,
        note: n.segs[0].note, notes: pts.map((p) => p.n), start: sp.start, end: sp.end,
        x0, x1, y: y0, half: thick / 2, pts: pts.map((p) => [x(p.t), yOf(p.n)]),
      });
    }
    ctx.restore();
    next.lit = [...lit.keys()];

    // the keyboard, its keys lit as their notes sound
    ctx.save();
    ctx.beginPath();
    ctx.rect(0, TOP, KEYS_W, rollBottom - TOP);
    ctx.clip();
    const KEY = keyColours();
    ctx.fillStyle = KEY.white;
    ctx.fillRect(0, TOP, KEYS_W, rollBottom - TOP);
    ctx.font = "9px ui-monospace, Menlo, monospace";
    ctx.textAlign = "right";
    for (let n = Math.floor(lo) - 1; n <= Math.ceil(hi) + 1; n++) {
      const y = yOf(n) - rowH / 2;
      const black = isBlack(n);
      const on = lit.get(n);
      if (on && !black) {
        ctx.fillStyle = on;
        ctx.fillRect(0, y, KEYS_W, rowH);
      }
      if (black) {
        ctx.fillStyle = on ?? KEY.black;
        ctx.fillRect(0, y, KEYS_W * 0.6, rowH);
      } else if (!isBlack(n + 1)) {
        // E–F and B–C touch with no black key between: a seam
        ctx.strokeStyle = KEY.seam;
        ctx.beginPath();
        ctx.moveTo(0, Math.round(y) + 0.5);
        ctx.lineTo(KEYS_W, Math.round(y) + 0.5);
        ctx.stroke();
      }
      if (pc(n) === 0 && rowH >= 7) {
        ctx.fillStyle = on ? css("Background") : KEY.label;
        ctx.fillText(noteName(n), KEYS_W - 4, yOf(n) + 3);
      }
    }
    ctx.textAlign = "left";
    ctx.restore();
    ctx.strokeStyle = css("WindowBorder");
    ctx.beginPath();
    ctx.moveTo(KEYS_W + 0.5, TOP);
    ctx.lineTo(KEYS_W + 0.5, h);
    ctx.stroke();

    // samples: a drum-map row each
    if (rows.length) {
      ctx.strokeStyle = css("WindowBorder");
      ctx.beginPath();
      ctx.moveTo(0, rollBottom + 0.5);
      ctx.lineTo(w, rollBottom + 0.5);
      ctx.stroke();
      const y0 = rollBottom + 3;
      const width = Math.max(3, Math.min(8, (SAMPLE_SECS / secs) * plotW));
      const flash = new Map();
      ctx.save();
      ctx.beginPath();
      ctx.rect(KEYS_W, y0, w - KEYS_W, h - y0);
      ctx.clip();
      rows.forEach((_, i) => {
        if (i % 2) {
          ctx.fillStyle = css("subtleFill");
          ctx.fillRect(KEYS_W, y0 + i * DRUM_ROW_H, w - KEYS_W, DRUM_ROW_H);
        }
      });
      for (const hh of hits) {
        const i = rowOf.get(hh.sample);
        if (i == null) continue;
        const colour = hooks.colourOf(hh.thread);
        const hx = x(hh.time), ry = y0 + i * DRUM_ROW_H;
        ctx.globalAlpha = (hh.time > now ? 0.55 : 1) * (0.5 + 0.5 * Math.min(1, hh.amp));
        ctx.fillStyle = colour;
        ctx.fillRect(hx, ry + 2, width, DRUM_ROW_H - 4);
        ctx.globalAlpha = 1;
        if (hh.time <= now && now - hh.time < SAMPLE_SECS * 1.5) flash.set(i, colour);
        next.hits.push({ thread: hh.thread, label: hooks.label(hh.thread, hh.name), job: hh.job, line: hh.line, sample: hh.sample, amp: hh.amp, time: hh.time, x0: hx, x1: hx + width, y: ry + DRUM_ROW_H / 2 });
      }
      ctx.restore();
      ctx.font = "9px ui-monospace, Menlo, monospace";
      rows.forEach((name, i) => {
        const ry = y0 + i * DRUM_ROW_H;
        if (flash.has(i)) {
          ctx.fillStyle = flash.get(i);
          ctx.fillRect(0, ry, KEYS_W, DRUM_ROW_H);
        }
        ctx.fillStyle = flash.has(i) ? css("Background") : css("faintForeground");
        ctx.fillText(name.length > 9 ? `${name.slice(0, 8)}…` : name, 4, ry + DRUM_ROW_H / 2 + 3);
      });
      ctx.font = "11px ui-monospace, Menlo, monospace";
    }

    if (!shown.length && !hits.length) {
      ctx.fillStyle = css("faintForeground");
      ctx.fillText("Notes will scroll past here as the threads play them.", KEYS_W + 12, TOP + 24);
    }

    // now
    ctx.strokeStyle = css("HighlightedBackground");
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(x(now), TOP - 4);
    ctx.lineTo(x(now), h);
    ctx.stroke();
    frame = next;
    if (mouse) showTip();
  }

  // drawn each frame while the roll is on screen, and not at all otherwise (ui/shown.js)
  function draw() {
    const t0 = performance.now();
    try { drawFrame(); } finally { perfAdd("pianoRoll", performance.now() - t0); }
  }
  const shown = animateWhileShown(wrap, draw);
  setInterval(() => { if (shown.shown) renderChips(); }, 500);
  renderChips();

  return {
    record: model.record,
    stop: (time) => model.stop(time),
    clear() {
      model.clear();
      chipsKey = "";
      renderChips();
    },
    /** What the last frame drew, for checks. */
    snapshot: () => ({ ...frame, notes: frame.notes.map(({ pts, ...f }) => f) }),
  };
}

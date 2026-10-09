// SPDX-License-Identifier: AGPL-3.0-or-later
// Threads: what a running program is doing, as it happens, for seeing a
// program's concurrency rather than only hearing it. Every thread is a lane
// on a timeline that moves with the clock: the notes it plays (placed by
// pitch), its samples, controls and MIDI, its sleeps, its waits on sync, and
// the cues passing between threads. Beside it, a table of each thread's
// state, the line it is at, its beat, tempo and rate of sounds. And a piano
// roll: the notes themselves.
import { css } from "./theme.js";
import { animateWhileShown } from "./ui/shown.js";
import { createProcessTree } from "./process-tree.js";
import { createPianoRoll } from "./piano-roll.js";
import { perfAdd } from "./perf.js";
import { icon } from "./icons.js";
import { askTwice } from "./ui/ask-twice.js";

const PALETTE = ["HighlightedBackground", "NumberForeground", "KeywordForeground", "DoubleQuotedStringForeground", "Scope_2", "HoverButton", "LogBackground_2", "SymbolForeground"];
const MAX_EVENTS = 600;

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};

/**
 * A thread's name as a musician reads it. A named thread is its name (a live
 * loop's without the live_loop_ the runtime puts on it); a run's main thread
 * is main; any other is whose it is and which: ":song › 256" for the 257th
 * thread the loop :song started, "main › 7", and a thread started inside an
 * unnamed thread shows that thread's number, "256 › 1". A thread's id is its
 * path in the spawn tree (0.0.0.256); the path never shows, however deep.
 * `lookup(id)` gives a known thread ({ name }) or null, for the parent's name.
 */
export function threadLabel(id, name, lookup = () => null) {
  if (name && name.startsWith("live_loop_")) return `:${name.slice(10)}`;
  if (name) return `:${name}`;
  const path = String(id).split(".");
  if (path.length <= 2) return "main";
  const parentId = path.slice(0, -1).join(".");
  const parent = lookup(parentId);
  const parentLabel = parent?.name ? threadLabel(parentId, parent.name, lookup)
    : path.length <= 3 ? "main"
    : path[path.length - 2];
  return `${parentLabel} › ${path[path.length - 1]}`;
}

/**
 * The lane a record belongs in. A thread's lane is the place in the program
 * it was started from — its parent and the line of the in_thread — so the
 * thread the next phrase starts from the same line takes over the same lane,
 * and a part keeps its place on the timeline for as long as the song runs.
 * A thread whose start was not seen is a lane of its own.
 */
export function laneKey(r) {
  if (r.kind === "thread" && r.event === "start" && r.parent != null && r.line) return `${r.parent}@${r.line}`;
  return String(r.thread);
}

/**
 * The thread a record's thread folds into, or null: a thread the runtime
 * started for itself (a sample's loading, a play's block) is shown as part
 * of its parent, since the program never wrote it.
 */
export const foldsInto = (r) => (r.kind === "thread" && r.event === "start" && r.internal && r.parent != null ? String(r.parent) : null);

/** How lit a sound is as it sounds: 1 at its onset, fading over 150 ms; 0 before and after. */
export function flashAt(e, now) {
  if (now == null || e.time == null) return 0;
  const since = now - e.time;
  return since < 0 || since > 0.15 ? 0 : 1 - since / 0.15;
}

/** What a lane's thread is doing, in words: the status where there is one, and after the end whether it still sounds. */
export function threadState(lane, s, now) {
  if (lane.error) return "error";
  if (s?.state === "sleeping") return now != null ? `sleeping ${Math.max(0, s.wake - now).toFixed(2)}s` : "sleeping";
  if (s?.state === "waiting") return `sync ${s.on}`;
  if (lane.ended != null) return soundingAfter(lane, now) ? "ended · sounding" : "ended";
  return "running";
}

/** Whether a lane has sounds still to play, or playing, at `now`: a thread that ends in a time_warp leaves a bar's worth. */
export const soundingAfter = (lane, now) => now != null && lane.events.some((e) => e.time + (e.dur || 0) > now);

/**
 * The beat grid: every beat from an anchor the program gave (a sleep at a
 * beat and a time, at a tempo), across a window from `start` for `secs`;
 * bars every four beats from beat 0, as a 4/4 ear counts them. No anchor, no
 * grid: seconds are still there above.
 */
export function beatTicks(anchor, start, secs) {
  if (!anchor || !(anchor.bpm > 0)) return [];
  const beatLen = 60 / anchor.bpm;
  const first = Math.ceil(anchor.beat + (start - anchor.time) / beatLen);
  const last = Math.floor(anchor.beat + (start + secs - anchor.time) / beatLen);
  const ticks = [];
  for (let b = first; b <= last; b++) ticks.push({ beat: b, time: anchor.time + (b - anchor.beat) * beatLen, bar: ((b % 4) + 4) % 4 === 0 });
  return ticks;
}

/** A cue's name as a musician reads it: a live loop's own, each time round, is the loop's name. */
export const cueLabel = (address) => (String(address).startsWith("/live_loop/") ? `:${String(address).slice(11)}` : String(address));

/**
 * Whether a cue's name goes in the strip: only a cue the program sends. A live
 * loop's own, each time round, is named by its lane; one from no thread of the
 * program (Sonic Pi's own, such as a controller connecting, or MIDI and OSC
 * from outside) is the system's, not the program's. Both are drawn faint.
 */
export const cueIsNamed = (address, fromProgram) => fromProgram && !String(address).startsWith("/live_loop/");

/**
 * Where the names of the cues a program sends go ({ x, text }, one per cue,
 * in time order): whole, at the cue's own line, or left off (text null) where
 * the name before is still in the way, or where it would reach the now line
 * `nowX`: a cue still to come is not named until it has sounded. `measure` is
 * the canvas's text width.
 */
export function placeCueNames(cues, x, nowX, measure, gap = 6) {
  let free = -Infinity;
  return cues.map((c) => {
    const at = Math.round(x(c.time)) + 3;
    const width = measure(c.name);
    if (at < free || x(c.time) >= nowX || at + width > nowX - gap) return { x: at, text: null };
    free = at + width + gap;
    return { x: at, text: c.name };
  });
}

/**
 * Stop: every lane still going ends now, and the sounds it had scheduled
 * past now never play, so they leave the record (the roll does the same).
 */
export function endLanesAt(lanes, time) {
  if (time == null) return;
  for (const t of lanes) {
    if (t.ended == null) t.ended = time;
    t.events = t.events.filter((e) => e.time <= time);
    for (const w of t.waits) if (w.to == null) w.to = time;
    for (const sl of t.sleeps) if (sl.to > time) sl.to = time;
  }
}

/** Whether a lane of an earlier run has nothing more to show once a new run starts: ended, and no sound left to play. */
export const laneRetired = (lane, job, now) => lane.job !== job && lane.ended != null && !soundingAfter(lane, now);

/** One frame of a window change: a fifth of the way to the target, settling exactly once close. */
export function easeWindow(current, target) {
  const next = current + (target - current) * 0.2;
  return Math.abs(target - next) < 0.01 ? target : next;
}

const round = (v, places = 2) => (typeof v === "number" ? String(Math.round(v * 10 ** places) / 10 ** places) : String(v));

/**
 * @param root element to fill
 * @param hooks { now() → the engine clock in seconds, or null; jump(line, job); processes() → process table rows;
 *                synthDefaults(synth) → a synth's opt defaults }
 */
export function createInsight(root, hooks) {
  const threads = new Map();   // thread id → its lane
  const lanes = new Map();     // lane key (laneKey) → lane
  let cues = [];
  let windowSecs = 8;          // what is drawn: eases to windowTarget frame by frame (easeWindow)
  let windowTarget = 8;
  let beatAnchor = null;       // the latest sleep: { time, beat, bpm }, what the beat grid hangs from
  let paused = false;
  let frozenNow = null;
  let lastStatus = null;

  root.textContent = "";
  root.classList.add("insight");
  const head = el("div", "insight-head");
  // Three views of one program: its processes (the thread tree, as it stands),
  // its timeline (what each thread did, as it happened) and its piano roll
  // (the notes it played).
  const views = el("div", "seg insight-views");
  let view = "processes";
  for (const [id, label] of [["processes", "Processes"], ["timeline", "Timeline"], ["roll", "Piano roll"]]) {
    const b = el("button", id === view ? "active" : "", label);
    b.type = "button";
    b.dataset.view = id;
    b.setAttribute("aria-pressed", String(id === view));   // which view is showing, said as well as lit
    b.addEventListener("click", () => {
      view = id;
      [...views.children].forEach((c) => { c.classList.toggle("active", c === b); c.setAttribute("aria-pressed", String(c === b)); });
      root.dataset.view = id;
    });
    views.appendChild(b);
  }
  root.dataset.view = view;
  head.appendChild(views);
  const timelineControls = el("span", "insight-timeline-controls");
  const windows = el("div", "seg");
  for (const secs of [4, 8, 16, 32]) {
    const b = el("button", secs === windowSecs ? "active" : "", `${secs}s`);
    b.type = "button";
    b.setAttribute("aria-label", `${secs} seconds`);
    b.setAttribute("aria-pressed", String(secs === windowSecs));
    b.addEventListener("click", () => { windowTarget = secs; [...windows.children].forEach((c) => { c.classList.toggle("active", c === b); c.setAttribute("aria-pressed", String(c === b)); }); });
    windows.appendChild(b);
  }
  // Pause holds every view where it is — the tree's rows, the timeline's clock, the roll — to explore it. A toggle,
  // the size of the chips beside it: lit while it holds, its name the same either way
  let frozenRows = null;
  const pauseSeg = el("div", "seg");
  const pause = el("button", "insight-pause");
  pause.type = "button";
  pause.innerHTML = `${icon("player-pause")}Pause`;
  pause.setAttribute("aria-pressed", "false");
  pause.title = "Hold every view where it is, to look around it";
  pause.addEventListener("click", () => {
    paused = !paused;
    frozenNow = paused ? hooks.now() : null;
    frozenRows = paused ? hooks.processes() : null;
    pause.classList.toggle("active", paused);
    pause.setAttribute("aria-pressed", String(paused));
  });
  pauseSeg.appendChild(pause);
  // Clear drops the record every view has drawn, which cannot be brought back: at the far end, after the legend,
  // away from the controls that are only ways of looking, quieter than they are, and it asks twice (ui/ask-twice.js)
  const clearSeg = el("div", "seg insight-clear-seg");
  const clear = el("button", "insight-clear");
  clear.type = "button";
  clear.innerHTML = `${icon("trash")}<span class="ask-label">Clear</span>`;
  clear.title = "Clear what every view has drawn so far: it cannot be brought back";
  const clearAll = () => { threads.clear(); lanes.clear(); cues = []; beatAnchor = null; roll.clear(); renderTable(); };
  askTwice(clear, { label: "Clear", armedLabel: "Clear for good", act: clearAll, settle: 3000 });
  clearSeg.appendChild(clear);
  const legend = el("span", "insight-legend");
  legend.innerHTML = '<i class="lg-note"></i>note <i class="lg-sample"></i>sample <i class="lg-sleep"></i>sleep <i class="lg-wait"></i>sync <i class="lg-cue"></i>cue';
  timelineControls.append(windows, legend, clearSeg);   // Clear only where there is a record to clear: not the tree's
  head.append(timelineControls, pauseSeg);

  const body = el("div", "insight-body");
  const canvasWrap = el("div", "insight-canvas");
  const canvas = el("canvas");
  canvas.setAttribute("role", "img");
  canvas.setAttribute("aria-label", "The threads over time, drawn: the table beside it has them as rows");
  canvasWrap.appendChild(canvas);
  const tableWrap = el("div", "insight-table table-wrap");
  const table = el("table");
  tableWrap.appendChild(table);
  body.append(canvasWrap, tableWrap);
  const processes = el("div", "insight-processes");
  root.append(head, processes, body);
  const tree = createProcessTree(processes, { read: () => (paused ? frozenRows : hooks.processes()), now: () => (paused ? frozenNow : hooks.now()), jump: hooks.jump });
  const rollPane = el("div");
  root.insertBefore(rollPane, body);
  const roll = createPianoRoll(rollPane, {
    now: () => (paused ? frozenNow : hooks.now()),
    still: () => paused,
    windowSecs: () => windowSecs,
    colourOf: (id) => (threads.has(id) ? colourOf(threads.get(id)) : css("HighlightedBackground")),
    label: (id, name) => threadLabel(id, nameOf(id) || name, lookup),
    jump: hooks.jump,
    defaults: hooks.synthDefaults,
  });

  const names = new Map();     // thread id → its name, for labels: a parent's name outlives its lane
  const nameOf = (id) => names.get(id) ?? threads.get(id)?.name ?? "";
  const lookup = (id) => (names.has(id) || threads.has(id) ? { id, name: nameOf(id) } : null);
  const labelOf = (t) => threadLabel(t.id, nameOf(t.id), lookup);

  // A lane: the threads started from one place, one after another, drawn as
  // one row. `id` and `name` are the thread in it now; `segments` every
  // thread it has held, with when each took over, for the marks on the lane.
  const lane = (r) => {
    let t = threads.get(r.thread);
    if (!t) {
      // the runtime's own thread: its records go to its parent's lane
      const into = foldsInto(r);
      if (into != null && threads.has(into)) {
        t = threads.get(into);
        threads.set(r.thread, t);
        return t;
      }
      const key = laneKey(r);
      t = lanes.get(key);
      if (!t) {
        t = { key, id: r.thread, name: r.name, job: r.job, index: lanes.size, events: [], sleeps: [], waits: [], segments: [], count: 0, recent: [], last: "", line: null, started: r.time, ended: null, error: null };
        lanes.set(key, t);
      }
      if (t.id !== r.thread || !t.segments.length) {
        t.id = r.thread;
        t.name = r.name;
        t.ended = null;
        t.error = null;
        push(t.segments, { id: r.thread, from: r.time });
      }
      threads.set(r.thread, t);
    }
    if (r.name) { t.name = r.name; names.set(r.thread, r.name); }
    t.job = r.job;
    if (t.ended != null && r.kind !== "thread") t.ended = null;
    const open = t.waits[t.waits.length - 1];
    if (open && open.to == null && r.kind !== "sync") open.to = r.time;
    return t;
  };
  const push = (list, item) => { list.push(item); if (list.length > MAX_EVENTS) list.splice(0, list.length - MAX_EVENTS); };

  /** One record from the running session. */
  function record(r) {
    if (paused || r.thread == null) return;
    roll.record(r);
    switch (r.kind) {
      case "synth": {
        if (r.synth.startsWith("sonic-pi-fx_")) return;
        const t = lane(r);
        const a = r.args || {};
        const sample = a.buf != null;
        const dur = (a.attack || 0) + (a.decay || 0) + (a.sustain || 0) + (a.release ?? (sample ? 0.2 : 1));
        push(t.events, { time: r.time, kind: sample ? "sample" : "note", note: a.note, dur: Math.min(dur, 8), line: r.line });
        t.count++;
        push(t.recent, r.time);
        t.last = sample ? `sample :${String(a.buf).replace(/\.flac$/, "")}` : `${r.synth.replace(/^sonic-pi-/, ":")} note ${round(a.note)}`;
        break;
      }
      case "control": {
        const t = lane(r);
        push(t.events, { time: r.time, kind: "control", line: r.line });
        t.last = `control ${Object.entries(r.args || {}).map(([k, v]) => `${k}: ${round(v)}`).join(", ")}`;
        break;
      }
      case "kill": push(lane(r).events, { time: r.time, kind: "kill", line: r.line }); break;
      case "midi": {
        const t = lane(r);
        push(t.events, { time: r.time, kind: "midi", note: r.path === "/note_on" ? r.args[2] : null, dur: 0.25, line: r.line });
        t.count++;
        push(t.recent, r.time);
        t.last = `midi ${r.path.slice(1)} ${r.args.slice(1).join(" ")}`;
        break;
      }
      case "output": push(lane(r).events, { time: r.time, kind: "output", text: r.text, line: r.line }); lane(r).last = `puts ${r.text}`; break;
      case "sleep": {
        const secs = r.until - r.t;
        push(lane(r).sleeps, { from: r.time, to: r.time + secs, beats: r.beats });
        // the grid's anchor: this beat was at this time, and the tempo is what this sleep says
        if (r.beat != null && r.beats > 0 && secs > 0) beatAnchor = { time: r.time, beat: r.beat, bpm: (r.beats * 60) / secs };
        break;
      }
      case "sync": push(lane(r).waits, { from: r.time, to: null, on: (r.on || [])[0] }); break;
      case "cue": push(cues, { time: r.time, address: r.address, thread: r.thread }); lane(r); break;
      case "thread": {
        if (r.name) names.set(r.thread, r.name);
        // a new run: the lanes of earlier runs that have ended and fallen silent make way for it
        if (r.event === "start" && r.job != null) {
          for (const [k, t] of lanes) {
            if (!laneRetired(t, r.job, r.time)) continue;
            lanes.delete(k);
            for (const [id, l] of threads) if (l === t) threads.delete(id);
          }
        }
        const t = lane(r);
        if (r.event === "end" && t.id === r.thread) t.ended = r.time;
        break;
      }
      case "error": lane(r).error = `${r.class}: ${r.message}`; break;
      default: break;
    }
  }

  /** The session's status: where each thread is now. */
  function status(s) {
    lastStatus = s;
    renderTable();
  }

  const colourOf = (t) => css(PALETTE[t.index % PALETTE.length]);

  function renderTable() {
    const live = new Map((lastStatus?.threads ?? []).map((t) => [t.id, t]));
    const now = hooks.now();
    // a thread that has finished leaves the table once it has left the timeline
    const rows = [...lanes.values()].filter((t) => t.ended == null || now == null || t.ended > now - windowSecs || soundingAfter(t, now)).sort((a, b) => a.index - b.index);
    table.textContent = "";
    const thead = el("thead");
    // fixed columns, each as wide as its values get: a countdown or a new count redrawn every status never moves
    // its neighbours, and the numbers keep their digits still (fixed places, right-aligned, tabular)
    const cols = el("colgroup");
    for (const c of ["thread", "state", "line", "beat", "bpm", "rate", "total", "last"]) cols.appendChild(el("col", `ic-${c}`));
    thead.innerHTML = '<tr><th>thread</th><th>state</th><th class="num">line</th><th class="num">beat</th><th class="num">bpm</th><th class="num">sounds/s</th><th class="num">total</th><th>last</th></tr>';
    table.append(cols, thead);
    const tbody = el("tbody");
    for (const t of rows) {
      const s = live.get(t.id);
      const tr = el("tr");
      const chip = el("i", "insight-chip");
      chip.style.background = colourOf(t);
      const name = el("td");
      name.append(chip, el("span", "", labelOf(t)));
      name.title = `thread ${t.id}`;   // the id as the runtime has it, for anyone who wants it
      const state = threadState(t, s, now);
      const line = s?.line ?? t.events[t.events.length - 1]?.line ?? null;
      t.line = line;
      // sounds a second over the last four seconds, while the thread runs: blank once it has ended
      const recent = now != null && t.ended == null ? t.recent.filter((x) => x > now - 4 && x <= now).length / 4 : null;
      const num = (text) => el("td", "num", text);
      tr.append(name, el("td", `insight-state ${state.split(" ")[0]}`, state), num(line ?? ""), num(s ? s.beat.toFixed(2) : ""), num(s ? round(s.bpm, 1) : ""), num(recent == null ? "" : recent.toFixed(1)), num(String(t.count)), el("td", "insight-last", t.error ?? t.last));
      for (const td of [tr.children[1], tr.lastChild]) td.title = td.textContent;   // a sync or a last event cut short by its column: the whole of it on hover
      if (line) {
        tr.classList.add("jump");
        tr.title = `Go to line ${line}`;
        tr.addEventListener("click", () => hooks.jump(line, t.job));
      }
      tbody.appendChild(tr);
    }
    if (!rows.length) {
      const tr = el("tr");
      const td = el("td", "insight-empty", "Run a program to see its threads here.");
      td.colSpan = 8;
      tr.appendChild(td);
      tbody.appendChild(tr);
    }
    table.appendChild(tbody);
  }

  // drawn each frame while the timeline is on screen, and not at all otherwise (ui/shown.js)
  function draw() {
    const t0 = performance.now();
    try { drawFrame(); } finally { perfAdd("timeline", performance.now() - t0); }
  }

  const LANE_H = 26;   // a lane's height never changes: lanes come and go, nothing else moves

  function drawFrame() {
    const dpr = window.devicePixelRatio || 1;
    const w = canvasWrap.clientWidth, hWrap = canvasWrap.clientHeight;
    if (!w || !hWrap) return;
    const now = paused ? frozenNow : hooks.now();
    if (!paused) windowSecs = easeWindow(windowSecs, windowTarget);
    // the seconds along the top, then a strip of the cues' names, then the lanes: the cue lines run through the
    // lanes only, so a line never crosses a name
    const AXIS = 18, STRIP = 14;
    const top = AXIS + STRIP;
    const visible = now == null ? [] : [...lanes.values()].filter((t) => t.ended == null || t.ended > now - windowSecs * 0.8 || soundingAfter(t, now)).sort((a, b) => a.index - b.index);
    // the canvas is as tall as its lanes need, and the pane scrolls: a lane is never squeezed to fit
    const h = Math.max(hWrap, top + visible.length * LANE_H + 6);
    if (canvas.width !== Math.round(w * dpr) || canvas.height !== Math.round(h * dpr)) {
      canvas.width = Math.round(w * dpr);
      canvas.height = Math.round(h * dpr);
      canvas.style.width = `${w}px`;
      canvas.style.height = `${h}px`;
    }
    const ctx = canvas.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = css("Background");
    ctx.fillRect(0, 0, w, h);
    ctx.font = "11px ui-monospace, Menlo, monospace";
    if (now == null) {
      ctx.fillStyle = css("faintForeground");
      ctx.fillText("Press Run: each thread will get a lane here.", 110, 30);
      return;
    }
    // the label column fits the longest label on show, and no label is ever cut
    const labels = visible.map((t) => labelOf(t));
    const labelW = Math.max(90, Math.min(220, 24 + Math.max(0, ...labels.map((l) => ctx.measureText(l).width))));
    const plotW = w - labelW - 8;
    const start = now - windowSecs * 0.8;
    const x = (time) => labelW + ((time - start) / windowSecs) * plotW;
    const laneH = LANE_H;
    const lanesBottom = top + visible.length * laneH;

    // the future: what is already scheduled, ahead of the sound
    ctx.fillStyle = css("subtleFill");
    ctx.fillRect(x(now), 0, w - x(now), h);
    // the program's beats: a faint line each, a firmer one every bar, numbered as the program counts them
    ctx.lineWidth = 1;
    const ticks = beatTicks(beatAnchor, start, windowSecs);
    const bars = ticks.filter((k) => k.bar).length;
    const everyBeat = ticks.length <= plotW / 14;   // closer than that, bars alone
    for (const tk of ticks) {
      if (!tk.bar && !everyBeat) continue;
      const gx = Math.round(x(tk.time)) + 0.5;
      ctx.strokeStyle = tk.bar ? css("mutedForeground") : css("WindowBorder");
      ctx.globalAlpha = tk.bar ? 0.55 : 0.5;
      ctx.beginPath();
      ctx.moveTo(gx, top);
      ctx.lineTo(gx, h);
      ctx.stroke();
      ctx.globalAlpha = 1;
      if (tk.bar && plotW / (bars || 1) > 36) {
        ctx.fillStyle = css("mutedForeground");
        ctx.fillText(String(tk.beat), gx + 3, Math.min(h - 4, lanesBottom + 14));
      }
    }
    // seconds, along the top
    ctx.strokeStyle = css("WindowBorder");
    ctx.fillStyle = css("faintForeground");
    for (let sec = Math.ceil(start); sec < start + windowSecs; sec++) {
      const gx = Math.round(x(sec)) + 0.5;
      ctx.beginPath();
      ctx.moveTo(gx, AXIS - 4);
      ctx.lineTo(gx, AXIS);
      ctx.stroke();
      ctx.fillText(`${Math.round(sec - now) >= 0 ? "+" : ""}${Math.round(sec - now)}s`, gx + 3, 11);
    }

    visible.forEach((t, i) => {
      const y = top + i * laneH;
      const colour = colourOf(t);
      if (i % 2) {
        ctx.fillStyle = css("subtleFill");
        ctx.fillRect(0, y, labelW, laneH);
      }
      ctx.fillStyle = colour;
      ctx.fillRect(4, y + laneH / 2 - 4, 8, 8);
      // the label fades only once the thread has ended and its sounds are over
      ctx.fillStyle = t.ended != null && !soundingAfter(t, now) ? css("faintForeground") : css("Foreground");
      ctx.fillText(labels[i], 16, y + laneH / 2 + 4);

      ctx.save();
      ctx.beginPath();
      ctx.rect(labelW, y, plotW + 8, laneH);
      ctx.clip();
      // where another thread took the lane over: a mark, with the thread's number
      ctx.fillStyle = css("faintForeground");
      for (const seg of t.segments.slice(1)) {
        if (seg.from < start || seg.from > start + windowSecs) continue;
        const sx = Math.round(x(seg.from));
        ctx.fillRect(sx, y + 1, 1, 6);
        ctx.fillText(String(seg.id).split(".").pop(), sx + 3, y + 8);
      }
      // sleeps: a thin line along the lane's foot
      ctx.strokeStyle = css("mutedForeground");
      ctx.lineWidth = 2;
      for (const s of t.sleeps) {
        if (s.to < start || s.from > start + windowSecs) continue;
        ctx.beginPath();
        ctx.moveTo(x(s.from), y + laneH - 3);
        ctx.lineTo(x(s.to) - 1, y + laneH - 3);
        ctx.stroke();
      }
      // syncs: dashed, with what is awaited
      ctx.setLineDash([4, 3]);
      ctx.strokeStyle = css("HoverButton");
      for (const s of t.waits) {
        const to = s.to ?? now;
        if (to < start) continue;
        ctx.beginPath();
        ctx.moveTo(x(s.from), y + laneH / 2);
        ctx.lineTo(x(to), y + laneH / 2);
        ctx.stroke();
        if (s.to == null) {
          ctx.fillStyle = css("HoverButton");
          ctx.fillText(`sync ${s.on}`, Math.max(labelW + 2, x(s.from) + 3), y + laneH / 2 - 3);
        }
      }
      ctx.setLineDash([]);
      for (const e of t.events) {
        if (e.time + (e.dur || 0) < start || e.time > start + windowSecs) continue;
        const ex = x(e.time);
        const past = e.time <= now;
        ctx.globalAlpha = past ? 1 : 0.55;
        const lit = flashAt(e, now);   // as it sounds: brighter and bigger for a moment, so the eye gets the beat the ear does
        if (e.kind === "note" || (e.kind === "midi" && e.note != null)) {
          const n = Math.min(108, Math.max(24, e.note ?? 60));
          const ny = y + 3 + (1 - (n - 24) / 84) * (laneH - 10);
          ctx.fillStyle = colour;
          const th = (e.kind === "midi" ? 3 : 4) + lit * 3;
          ctx.fillRect(ex, ny - lit * 1.5, Math.max(3, (e.dur / windowSecs) * plotW), th);
          if (lit > 0) { ctx.fillStyle = css("Foreground"); ctx.globalAlpha = lit * 0.8; ctx.fillRect(ex, ny - lit * 1.5, 3 + lit * 3, th); ctx.globalAlpha = 1; }
        } else if (e.kind === "sample") {
          const r = 5 + lit * 4;
          ctx.fillStyle = lit > 0.5 ? css("Foreground") : colour;
          ctx.beginPath();
          ctx.moveTo(ex, y + laneH / 2 - r);
          ctx.lineTo(ex + r, y + laneH / 2);
          ctx.lineTo(ex, y + laneH / 2 + r);
          ctx.lineTo(ex - r, y + laneH / 2);
          ctx.fill();
        } else if (e.kind === "control" || e.kind === "kill") {
          ctx.strokeStyle = colour;
          ctx.lineWidth = 1.5;
          ctx.beginPath();
          ctx.moveTo(ex, y + 2);
          ctx.lineTo(ex, y + laneH - 6);
          ctx.stroke();
          ctx.fillStyle = colour;
          ctx.fillText(e.kind === "kill" ? "✕" : "▲", ex + 2, y + 10);
        } else if (e.kind === "output") {
          ctx.fillStyle = css("LogForeground_1");
          ctx.fillText(`» ${e.text}`.slice(0, 24), ex + 2, y + laneH - 6);
        }
        ctx.globalAlpha = 1;
      }
      ctx.restore();
      ctx.strokeStyle = css("WindowBorder");
      ctx.beginPath();
      ctx.moveTo(0, y + laneH + 0.5);
      ctx.lineTo(w, y + laneH + 0.5);
      ctx.stroke();
    });

    // cues: a line through every lane, in the colour of the thread that sent it. A cue the program sends is named in
    // the strip above the lanes, at its line (placeCueNames): never over a line, another name or the now line. Any
    // other (a live loop's own, each time round; one from no thread of the program) is faint and unnamed (cueIsNamed)
    const shownCues = cues.filter((c) => c.time >= start && c.time <= start + windowSecs).sort((a, b) => a.time - b.time);
    const cueColour = (c) => { const t = threads.get(c.thread); return t ? colourOf(t) : css("CuePathBackground"); };
    const named = (c) => cueIsNamed(c.address, threads.has(c.thread));
    for (const c of shownCues) {
      const cx = Math.round(x(c.time)) + 0.5;
      ctx.strokeStyle = cueColour(c);
      ctx.globalAlpha = (named(c) ? 0.9 : 0.35) * (c.time <= now ? 1 : 0.5);
      ctx.beginPath();
      ctx.moveTo(cx, top);
      ctx.lineTo(cx, lanesBottom);
      ctx.stroke();
    }
    const sent = shownCues.filter(named);
    ctx.globalAlpha = 0.9;
    placeCueNames(sent.map((c) => ({ time: c.time, name: cueLabel(c.address) })), x, Math.round(x(now)), (t) => ctx.measureText(t).width)
      .forEach((p, i) => {
        if (!p.text) return;
        ctx.fillStyle = cueColour(sent[i]);
        ctx.fillText(p.text, p.x, AXIS + 11);
      });
    ctx.globalAlpha = 1;

    // now
    ctx.strokeStyle = css("HighlightedBackground");
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(x(now), 0);
    ctx.lineTo(x(now), h);
    ctx.stroke();
    ctx.fillStyle = css("HighlightedBackground");
    ctx.fillText("now", x(now) + 4, Math.min(h - 4, lanesBottom + 14));
  }
  const shown = animateWhileShown(root, draw);
  setInterval(() => { if (shown.shown && !paused) renderTable(); }, 500);   // the table as it stands, twice a second, while it can be seen
  renderTable();

  return {
    record,
    status,
    colourOf: (id) => (threads.has(id) ? colourOf(threads.get(id)) : css("HighlightedBackground")),
    tree,
    roll,
    /** Everything stopped at this time: sounds scheduled after it never play. */
    stopped: (time) => { if (time != null) { roll.stop(time); endLanesAt(lanes.values(), time); renderTable(); } },
    clear: clearAll,
  };
}

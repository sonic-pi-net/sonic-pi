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

const PALETTE = ["HighlightedBackground", "NumberForeground", "KeywordForeground", "DoubleQuotedStringForeground", "Scope_2", "HoverButton", "LogBackground_2", "SymbolForeground"];
const MAX_EVENTS = 600;

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};

/** A thread's name as a program calls it. */
export function threadLabel(id, name) {
  if (name && name.startsWith("live_loop_")) return `:${name.slice(10)}`;
  if (name) return `:${name}`;
  const depth = String(id).split(".").length;
  return depth <= 2 ? "main" : `thread ${id}`;
}

const round = (v, places = 2) => (typeof v === "number" ? String(Math.round(v * 10 ** places) / 10 ** places) : String(v));

/**
 * @param root element to fill
 * @param hooks { now() → the engine clock in seconds, or null; jump(line, job); processes() → process table rows;
 *                synthDefaults(synth) → a synth's opt defaults }
 */
export function createInsight(root, hooks) {
  const threads = new Map();
  let cues = [];
  let windowSecs = 8;
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
    b.addEventListener("click", () => { windowSecs = secs; [...windows.children].forEach((c) => { c.classList.toggle("active", c === b); c.setAttribute("aria-pressed", String(c === b)); }); });
    windows.appendChild(b);
  }
  // Pause holds every view where it is — the tree's rows, the timeline's clock, the roll — to explore it
  let frozenRows = null;
  const pause = el("button", "sp-mini-btn insight-pause", "Pause");
  pause.addEventListener("click", () => {
    paused = !paused;
    frozenNow = paused ? hooks.now() : null;
    frozenRows = paused ? hooks.processes() : null;
    pause.textContent = paused ? "Resume" : "Pause";
    pause.classList.toggle("on", paused);
  });
  head.appendChild(pause);
  const clear = el("button", "sp-mini-btn", "Clear");
  clear.addEventListener("click", () => { threads.clear(); cues = []; roll.clear(); renderTable(); });
  const legend = el("span", "insight-legend");
  legend.innerHTML = '<i class="lg-note"></i>note <i class="lg-sample"></i>sample <i class="lg-sleep"></i>sleep <i class="lg-wait"></i>sync <i class="lg-cue"></i>cue';
  timelineControls.append(windows, clear, legend);
  head.appendChild(timelineControls);

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
  const tree = createProcessTree(processes, { read: () => (paused ? frozenRows : hooks.processes()), now: () => (paused ? frozenNow : hooks.now()), jump: hooks.jump, stop: hooks.stop });
  const rollPane = el("div");
  root.insertBefore(rollPane, body);
  const roll = createPianoRoll(rollPane, {
    now: () => (paused ? frozenNow : hooks.now()),
    still: () => paused,
    windowSecs: () => windowSecs,
    colourOf: (id) => (threads.has(id) ? colourOf(threads.get(id)) : css("HighlightedBackground")),
    label: (id, name) => threadLabel(id, threads.get(id)?.name || name),
    jump: hooks.jump,
    defaults: hooks.synthDefaults,
  });

  const lane = (r) => {
    let t = threads.get(r.thread);
    if (!t) {
      t = { id: r.thread, name: r.name, job: r.job, index: threads.size, events: [], sleeps: [], waits: [], count: 0, recent: [], last: "", line: null, started: r.time, ended: null, error: null };
      threads.set(r.thread, t);
    }
    if (r.name) t.name = r.name;
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
      case "sleep": push(lane(r).sleeps, { from: r.time, to: r.time + (r.until - r.t), beats: r.beats }); break;
      case "sync": push(lane(r).waits, { from: r.time, to: null, on: (r.on || [])[0] }); break;
      case "cue": push(cues, { time: r.time, address: r.address, thread: r.thread }); lane(r); break;
      case "thread": {
        const t = lane(r);
        if (r.event === "end") t.ended = r.time;
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
    const rows = [...threads.values()].filter((t) => t.ended == null || now == null || t.ended > now - windowSecs).sort((a, b) => a.index - b.index);
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
      name.append(chip, el("span", "", threadLabel(t.id, t.name)));
      let state = t.error ? "error" : t.ended != null ? "finished" : "running";
      if (s?.state === "sleeping") state = now != null ? `sleeping ${Math.max(0, s.wake - now).toFixed(2)}s` : "sleeping";
      if (s?.state === "waiting") state = `sync ${s.on}`;
      const line = s?.line ?? t.events[t.events.length - 1]?.line ?? null;
      t.line = line;
      const recent = now != null ? t.recent.filter((x) => x > now - 4 && x <= now).length / 4 : 0;
      const num = (text) => el("td", "num", text);
      tr.append(name, el("td", `insight-state ${state.split(" ")[0]}`, state), num(line ?? ""), num(s ? s.beat.toFixed(2) : ""), num(s ? round(s.bpm, 1) : ""), num(recent.toFixed(1)), num(String(t.count)), el("td", "insight-last", t.error ?? t.last));
      for (const td of [tr.children[0], tr.children[1], tr.lastChild]) td.title = td.textContent;   // a name, a sync or a last event cut short by its column: the whole of it on hover
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

  function drawFrame() {
    const dpr = window.devicePixelRatio || 1;
    const w = canvasWrap.clientWidth, h = canvasWrap.clientHeight;
    if (!w || !h) return;
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
    const now = paused ? frozenNow : hooks.now();
    const labelW = 110, top = 18;
    const plotW = w - labelW - 8;
    ctx.font = "11px ui-monospace, Menlo, monospace";
    if (now == null) {
      ctx.fillStyle = css("faintForeground");
      ctx.fillText("Press Run: each thread will get a lane here.", labelW, 30);
      return;
    }
    const start = now - windowSecs * 0.8;
    const x = (time) => labelW + ((time - start) / windowSecs) * plotW;
    const visible = [...threads.values()].filter((t) => t.ended == null || t.ended > start).sort((a, b) => a.index - b.index);
    const laneH = Math.max(18, Math.min(56, (h - top - 4) / Math.max(1, visible.length)));

    // the future: what is already scheduled, ahead of the sound
    ctx.fillStyle = css("subtleFill");
    ctx.fillRect(x(now), 0, w - x(now), h);
    // seconds
    ctx.strokeStyle = css("WindowBorder");
    ctx.fillStyle = css("faintForeground");
    ctx.lineWidth = 1;
    for (let s = Math.ceil(start); s < start + windowSecs; s++) {
      const gx = Math.round(x(s)) + 0.5;
      ctx.beginPath();
      ctx.moveTo(gx, top - 4);
      ctx.lineTo(gx, h);
      ctx.stroke();
      ctx.fillText(`${Math.round(s - now) >= 0 ? "+" : ""}${Math.round(s - now)}s`, gx + 3, 11);
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
      ctx.fillStyle = t.ended != null ? css("faintForeground") : css("Foreground");
      ctx.fillText(threadLabel(t.id, t.name).slice(0, 14), 16, y + laneH / 2 + 4);

      ctx.save();
      ctx.beginPath();
      ctx.rect(labelW, y, plotW + 8, laneH);
      ctx.clip();
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
        if (e.kind === "note" || (e.kind === "midi" && e.note != null)) {
          const n = Math.min(108, Math.max(24, e.note ?? 60));
          const ny = y + 3 + (1 - (n - 24) / 84) * (laneH - 10);
          ctx.fillStyle = colour;
          ctx.fillRect(ex, ny, Math.max(3, (e.dur / windowSecs) * plotW), e.kind === "midi" ? 3 : 4);
        } else if (e.kind === "sample") {
          ctx.fillStyle = colour;
          ctx.beginPath();
          ctx.moveTo(ex, y + laneH / 2 - 5);
          ctx.lineTo(ex + 5, y + laneH / 2);
          ctx.lineTo(ex, y + laneH / 2 + 5);
          ctx.lineTo(ex - 5, y + laneH / 2);
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

    // cues: a line through every lane, from the thread that sent it
    ctx.fillStyle = css("CuePathForeground");
    for (const c of cues) {
      if (c.time < start || c.time > start + windowSecs) continue;
      const cx = Math.round(x(c.time)) + 0.5;
      ctx.strokeStyle = css("CuePathBackground");
      ctx.globalAlpha = c.time <= now ? 0.9 : 0.45;
      ctx.beginPath();
      ctx.moveTo(cx, top);
      ctx.lineTo(cx, top + visible.length * laneH);
      ctx.stroke();
      ctx.globalAlpha = 1;
      ctx.fillStyle = css("CuePathBackground");
      ctx.fillText(c.address, cx + 3, top + 10);
    }

    // now
    ctx.strokeStyle = css("HighlightedBackground");
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(x(now), 0);
    ctx.lineTo(x(now), h);
    ctx.stroke();
    ctx.fillStyle = css("HighlightedBackground");
    ctx.fillText("now", x(now) + 4, h - 4);
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
    stopped: (time) => { if (time != null) roll.stop(time); },
    clear: () => { threads.clear(); cues = []; roll.clear(); renderTable(); },
  };
}

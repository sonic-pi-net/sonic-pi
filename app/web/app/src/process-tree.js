// SPDX-License-Identifier: AGPL-3.0-or-later
// The process tree: the running program's threads as native Sonic Pi draws
// scsynth's node tree (app/gui/widgets/nodetreegraph.cpp), only for threads.
// A top-down hierarchy — the session, its runs, each run's main thread and
// the threads it spawned — joined by curved edges, easing to new positions as
// threads start and end. Native's core: leaves take sequential slots and
// parents centre over their children; each node eases with a viscosity by
// kind (root pinned, runs stickiest, threads freest), by wall-clock time;
// a new node grows out of its parent; past 200 nodes positions snap and
// edges go straight. On top, what a thread is doing: filled while sleeping,
// hollow while waiting on a sync, a pulse when it wakes, fading when it ends,
// ringed red when it failed.
//
// Beside the threads, what they make: a with_fx block is a square hanging from
// the thread that opened it (hollow once its block has ended and it waits on
// its threads and sounds), and the threads started inside it and the sounds
// played inside it hang from it, so a live loop run again from another
// with_fx is seen to move there. A sound is a small diamond that fades as it
// ends.
//
// It reads the runtime's process table (web/sonic_pi.js processTable): rows
// of numbers straight out of the runtime's memory, every frame.
import { css } from "./theme.js";
import { animateWhileShown } from "./ui/shown.js";
import { perfAdd } from "./perf.js";

export const KIND = { run: 0, main: 1, liveLoop: 2, named: 3, thread: 4, session: 5, fx: 6, synth: 7, sample: 8, group: 9 };   // group: a container the runs hang from (Scheduler#stop_group)
const SOUND_LINGER = 1.0;
const isSound = (n) => n.kind === KIND.synth || n.kind === KIND.sample;
export const STATE = { running: 0, sleeping: 1, waiting: 2, done: 3, error: 4, stopped: 5 };
const MAX_ANIMATED = 200;
// labels while few enough threads, runs and fx to read them: counted without the sounds (they have no label, and come
// and go on every beat), off past the most, back only below fewer, so a count at the edge does not flash them
const LABELS_OFF = 60, LABELS_ON = 48;
const LINGER = 2.5;

// Native's colours by kind: groups NumberForeground, synths
// FunctionMethodNameForeground, fx KeywordForeground, samples
// DoubleQuotedStringForeground. Runs are the groups; live loops the synths.
const KIND_COLOUR = {
  [KIND.session]: "NumberForeground",
  [KIND.run]: "NumberForeground",
  [KIND.group]: "NumberForeground",
  [KIND.liveLoop]: "FunctionMethodNameForeground",
  [KIND.named]: "KeywordForeground",
  [KIND.main]: "DoubleQuotedStringForeground",
  [KIND.thread]: "DoubleQuotedStringForeground",
  [KIND.fx]: "KeywordForeground",
  [KIND.synth]: "FunctionMethodNameForeground",
  [KIND.sample]: "DoubleQuotedStringForeground",
};
// Viscosity: higher is stickier. Native: root 1.00, groups 0.98, fx 0.96, synths 0.90.
const VISCOSITY = { [KIND.session]: 1.0, [KIND.group]: 0.99, [KIND.run]: 0.98, [KIND.main]: 0.96, [KIND.liveLoop]: 0.96, [KIND.named]: 0.93, [KIND.thread]: 0.9,
  [KIND.fx]: 0.96, [KIND.synth]: 0.9, [KIND.sample]: 0.9 };

/**
 * Native's level-by-depth layout. nodes: [{id, parent}] in sibling order.
 * Returns id → {tx, ty}, both in 0..1: leaves take sequential slots across,
 * parents centre over their children, depth goes down.
 */
export function layoutTargets(nodes) {
  const ids = new Set(nodes.map((n) => n.id));
  const children = new Map();
  const roots = [];
  for (const n of nodes) {
    if (n.parent != null && ids.has(n.parent) && n.parent !== n.id) {
      if (!children.has(n.parent)) children.set(n.parent, []);
      children.get(n.parent).push(n.id);
    } else {
      roots.push(n.id);
    }
  }
  const depth = new Map();
  const xpos = new Map();
  let leaf = 0;
  let maxDepth = 0;
  const dfs = (id, d) => {
    depth.set(id, d);
    maxDepth = Math.max(maxDepth, d);
    const ch = children.get(id) || [];
    if (!ch.length) {
      xpos.set(id, leaf++);
    } else {
      let sum = 0;
      for (const c of ch) { dfs(c, d + 1); sum += xpos.get(c); }
      xpos.set(id, sum / ch.length);
    }
  };
  for (const r of roots) dfs(r, 0);
  const maxX = Math.max(1, leaf - 1);
  const md = Math.max(1, maxDepth);
  const out = new Map();
  for (const n of nodes) {
    out.set(n.id, { tx: leaf <= 1 ? 0.5 : xpos.get(n.id) / maxX, ty: depth.get(n.id) / md });
  }
  return out;
}

/** A thread's name as a program would say it. */
export function processLabel(row) {
  if (row.kind === KIND.session) return "session";
  if (row.kind === KIND.group) return row.label ?? `group ${row.group}`;
  if (row.kind === KIND.run) return `run ${row.job}`;
  if (row.kind === KIND.main) return "main";
  if (row.kind === KIND.fx) return `with_fx :${(row.name ?? "").replace(/^sonic-pi-fx_/, "")}`;
  if (row.kind === KIND.sample) return `sample :${(row.sample || row.name || "").replace(/\.(flac|wav|wave|aiff?|ogg|oga|mp3)$/i, "")}`;
  if (row.kind === KIND.synth) return `synth :${(row.name ?? "").replace(/^sonic-pi-/, "")}`;
  if (row.name?.startsWith("live_loop_")) return `live_loop :${row.name.slice(10)}`;
  if (row.name) return `in_thread :${row.name}`;
  return `in_thread ${row.id}`;
}

const STATE_WORDS = ["running", "sleeping", "waiting on sync", "done", "failed", "stopped"];

/**
 * @param root element to fill
 * @param hooks { read() → process rows, or null with no session; now() → engine clock; jump(line, job); stop(uid) — a node's subtree stops (a run, a loop, an fx block) }
 */
export function createProcessTree(root, hooks) {
  const layout = new Map();   // id → {cx, cy, tx, ty, visc, seeded}
  let nodes = [];
  let lastKey = "";
  let lastStep = null;
  let labels = true;
  let crowded = false;   // too many to label (LABELS_OFF, LABELS_ON)
  let hover = null;
  const screen = new Map();   // id → {x, y, r}
  const reduceMotion = window.matchMedia?.("(prefers-reduced-motion: reduce)");

  root.textContent = "";
  root.classList.add("ptree");
  const stats = document.createElement("div");
  stats.className = "ptree-stats";
  const labelToggle = document.createElement("button");
  labelToggle.className = "sp-mini-btn ptree-labels";
  labelToggle.textContent = "Labels";
  labelToggle.addEventListener("click", () => { labels = !labels; labelToggle.classList.toggle("on", labels); });
  labelToggle.classList.add("on");
  const head = document.createElement("div");
  head.className = "ptree-head";
  head.append(stats, labelToggle);
  const wrap = document.createElement("div");
  wrap.className = "ptree-canvas";
  const canvas = document.createElement("canvas");
  canvas.setAttribute("role", "img");
  canvas.setAttribute("aria-label", "The program's threads, drawn as a tree");
  const tip = document.createElement("div");
  tip.className = "ptree-tip";
  tip.hidden = true;
  wrap.append(canvas, tip);
  root.append(head, wrap);

  function setTree(rows) {
    // The session is the root every run hangs from, as scsynth's group 0 is.
    const session = { uid: 0, parent: -1, job: -1, kind: KIND.session, state: STATE.running, line: -1, wake: -1, beat: 0, bpm: 0, active: -1, events: 0, redefs: 0, ended: -1, id: "", name: "" };
    nodes = [session, ...rows.map((r) => ({ ...r, parent: r.parent < 0 ? 0 : r.parent })).sort((a, b) => a.uid - b.uid)];
    const key = nodes.map((n) => `${n.uid}:${n.parent}`).join(",");
    if (key === lastKey) return;
    lastKey = key;
    const targets = layoutTargets(nodes.map((n) => ({ id: n.uid, parent: n.uid === 0 ? null : n.parent })));
    for (const id of [...layout.keys()]) if (!targets.has(id)) layout.delete(id);
    const snap = nodes.length > MAX_ANIMATED || reduceMotion?.matches;
    for (const n of nodes) {
      const t = targets.get(n.uid);
      let l = layout.get(n.uid);
      if (!l) {
        const p = layout.get(n.parent);
        l = { cx: p ? p.cx : t.tx, cy: p ? p.cy : t.ty, seeded: true };
        layout.set(n.uid, l);
      }
      l.tx = t.tx;
      l.ty = t.ty;
      l.visc = VISCOSITY[n.kind] ?? 0.9;
      if (snap) { l.cx = l.tx; l.cy = l.ty; }
    }
  }

  function ease() {
    const t = performance.now();
    const dt = lastStep == null ? 16 : Math.min(100, Math.max(1, t - lastStep));
    lastStep = t;
    const frames = dt / 16.6667;
    for (const l of layout.values()) {
      const step = 1 - Math.pow(l.visc, frames);
      l.cx += (l.tx - l.cx) * step;
      l.cy += (l.ty - l.cy) * step;
      if (Math.abs(l.tx - l.cx) < 0.0005 && Math.abs(l.ty - l.cy) < 0.0005) { l.cx = l.tx; l.cy = l.ty; }
    }
  }

  // drawn each frame while the tree is on screen, and not at all otherwise (ui/shown.js); its easing starts afresh
  function draw() {
    const t0 = performance.now();
    try { drawFrame(); } finally { perfAdd("processTree", performance.now() - t0); }
  }

  function drawFrame() {
    const rows = hooks.read();
    setTree(rows ?? []);
    ease();
    const now = hooks.now();
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
    ctx.fillStyle = css("PaneBackground");
    ctx.fillRect(0, 0, w, h);
    renderStats(rows);
    if (!rows || !rows.length) {
      ctx.fillStyle = css("faintForeground");
      ctx.font = "12px ui-monospace, Menlo, monospace";
      ctx.textAlign = "center";
      ctx.fillText("(no running threads)", w / 2, h / 2);
      ctx.textAlign = "left";
      screen.clear();
      tip.hidden = true;
      return;
    }
    const dense = nodes.length > MAX_ANIMATED;
    const labelled = nodes.reduce((k, n) => k + (isSound(n) ? 0 : 1), 0);
    crowded = labelled > (crowded ? LABELS_ON : LABELS_OFF);
    const margin = 26;
    const pw = Math.max(1, w - 2 * margin), ph = Math.max(1, h - 2 * margin - (labels ? 12 : 0));
    const px = (nx) => margin + nx * pw, py = (ny) => margin + ny * ph;
    const byId = new Map(nodes.map((n) => [n.uid, n]));
    // A finished thread whose children still run stays in the tree, dimmed:
    // a run's main thread ends as soon as it has started its live loops.
    const parentOfLive = new Set();
    for (const n of nodes) {
      if (n.ended >= 0) continue;
      for (let p = byId.get(n.parent); p && !parentOfLive.has(p.uid); p = byId.get(p.parent)) parentOfLive.add(p.uid);
    }
    const fade = (n) => {
      if (n.ended < 0 || now == null) return 1;
      if (isSound(n)) return Math.min(1, Math.max(0, 1 - (now - n.ended) / SOUND_LINGER));   // a sound's end is known from its start
      if (parentOfLive.has(n.uid)) return 0.5;
      return Math.max(0.15, 1 - Math.max(0, now - n.ended) / LINGER);
    };

    // edges: parent → child, faint S-curves (straight when dense), in one path
    const text = css("Foreground");
    ctx.beginPath();
    for (const n of nodes) {
      const p = byId.get(n.parent);
      if (!p || n.uid === 0) continue;
      const a = layout.get(p.uid), b = layout.get(n.uid);
      const ax = px(a.cx), ay = py(a.cy), bx = px(b.cx), by = py(b.cy);
      ctx.moveTo(ax, ay);
      if (dense) ctx.lineTo(bx, by);
      else {
        const midY = (ay + by) / 2;
        ctx.bezierCurveTo(ax, midY, bx, midY, bx, by);
      }
    }
    ctx.strokeStyle = text;
    ctx.globalAlpha = 90 / 255;
    ctx.lineWidth = 1.2;
    ctx.stroke();
    ctx.globalAlpha = 1;

    // nodes: runs under threads, as native paints groups first
    screen.clear();
    const ordered = [...nodes].sort((a, b) => (a.kind === KIND.run || a.kind === KIND.session ? 0 : 1) - (b.kind === KIND.run || b.kind === KIND.session ? 0 : 1));
    ctx.font = "10px ui-monospace, Menlo, monospace";
    for (const n of ordered) {
      const l = layout.get(n.uid);
      const x = px(l.cx), y = py(l.cy);
      const group = n.kind === KIND.run || n.kind === KIND.session;
      const fx = n.kind === KIND.fx, sound = isSound(n);
      let r = dense ? (group ? 4 : sound ? 2 : 3) : (group ? 6 : fx ? 5 : sound ? 2.5 : 4.5);
      const alpha = fade(n);
      if (alpha <= 0) continue;
      if (n.ended >= 0 && !sound && !parentOfLive.has(n.uid)) r *= 0.6 + 0.4 * alpha;
      const colour = css(KIND_COLOUR[n.kind]);
      screen.set(n.uid, { x, y, r });
      ctx.globalAlpha = alpha;
      // a pulse as a thread wakes: it ran just now
      if (!group && n.active >= 0 && now != null && now >= n.active && now - n.active < 0.35 && n.ended < 0) {
        const k = (now - n.active) / 0.35;
        ctx.beginPath();
        ctx.arc(x, y, r + 2 + k * 10, 0, Math.PI * 2);
        ctx.strokeStyle = colour;
        ctx.lineWidth = 2;
        ctx.globalAlpha = alpha * (1 - k);
        ctx.stroke();
        ctx.globalAlpha = alpha;
      }
      ctx.beginPath();
      if (fx) ctx.rect(x - r, y - r, 2 * r, 2 * r);
      else if (sound) { ctx.moveTo(x, y - r - 1); ctx.lineTo(x + r + 1, y); ctx.lineTo(x, y + r + 1); ctx.lineTo(x - r - 1, y); ctx.closePath(); }
      else ctx.arc(x, y, r, 0, Math.PI * 2);
      if (n.state === STATE.waiting || (fx && n.state === STATE.sleeping)) {
        ctx.fillStyle = css("PaneBackground");
        ctx.fill();
        ctx.strokeStyle = colour;
        ctx.lineWidth = 2;
        ctx.stroke();
      } else {
        ctx.fillStyle = colour;
        ctx.fill();
        ctx.strokeStyle = css("Background");
        ctx.lineWidth = 1;
        ctx.stroke();
      }
      if (n.state === STATE.error) {
        ctx.beginPath();
        ctx.arc(x, y, r + 3, 0, Math.PI * 2);
        ctx.strokeStyle = css("LogForeground_3");
        ctx.lineWidth = 2;
        ctx.stroke();
      }
      if (hover === n.uid) {
        ctx.beginPath();
        ctx.arc(x, y, r + 4, 0, Math.PI * 2);
        ctx.strokeStyle = css("HighlightedBackground");
        ctx.lineWidth = 1.5;
        ctx.stroke();
      }
      if (labels && !dense && !sound && !crowded) {
        ctx.fillStyle = css("mutedForeground");
        ctx.textAlign = "center";
        ctx.fillText(processLabel(n).replace(/^(live_loop|in_thread|with_fx) /, ""), x, y + r + 12);
        ctx.textAlign = "left";
      }
      ctx.globalAlpha = 1;
    }
    if (hover != null) showTip(byId.get(hover), now);
  }

  function renderStats(rows) {
    const count = (f) => (rows || []).filter(f).length;
    // each count in a box of its own width (style.css .ptree-n), so a count going from 9 to 12 moves nothing after it
    const swatch = (key, label, n) => `<span class="ptree-stat"><i style="background:${css(key)}"></i>${label} <span class="ptree-n">${n}</span></span>`;
    const html = swatch("NumberForeground", "Runs", count((r) => r.kind === KIND.run && r.state === STATE.running))
      + swatch("FunctionMethodNameForeground", "Live loops", count((r) => r.kind === KIND.liveLoop && r.ended < 0))
      + swatch("KeywordForeground", "Named", count((r) => r.kind === KIND.named && r.ended < 0))
      + swatch("DoubleQuotedStringForeground", "Threads", count((r) => (r.kind === KIND.thread || r.kind === KIND.main) && r.ended < 0))
      + swatch("KeywordForeground", "FX", count((r) => r.kind === KIND.fx && r.ended < 0))
      + swatch("FunctionMethodNameForeground", "Synths", count((r) => isSound(r) && r.state === STATE.running))
      + `<span class="ptree-stat ptree-waiting">waiting on sync <span class="ptree-n">${count((r) => r.state === STATE.waiting)}</span></span>`;
    if (stats.innerHTML !== html) stats.innerHTML = html;
  }

  function showTip(n, now) {
    const s = n && screen.get(n.uid);
    if (!s) { tip.hidden = true; return; }
    const lines = [processLabel(n)];
    if (n.kind === KIND.fx || isSound(n)) {
      const words = n.kind === KIND.fx ? ["its block is running", "its block has ended: waiting on its threads and sounds", "", "freed"] : ["sounding", "", "", "ended"];
      lines.push((words[n.state] ?? "") + (n.line > 0 ? ` · line ${n.line}` : ""));
      if (n.kind === KIND.fx && n.state === STATE.sleeping && n.events > 0) lines.push(`threads it waits on: ${n.events}`);
    } else if (n.kind !== KIND.session) {
      if (n.kind !== KIND.run) lines[0] += `  ·  ${n.id}`;
      let state = n.kind === KIND.run ? (n.state === STATE.running ? "running" : "finished") : STATE_WORDS[n.state] ?? "";
      if (n.state === STATE.sleeping && now != null && n.wake >= 0) state += ` · wakes in ${Math.max(0, n.wake - now).toFixed(2)}s`;
      if (n.line > 0) state += ` · line ${n.line}`;
      lines.push(state);
      if (n.kind !== KIND.run) lines.push(`beat ${Math.round(n.beat * 100) / 100} · ${Math.round(n.bpm * 10) / 10} bpm · ${n.events} sounds${n.redefs ? ` · redefined ${n.redefs}×` : ""}`);
    }
    if (stoppable(n)) lines.push("shift-click: stop it, and everything under it");
    tip.textContent = lines.join("\n");
    tip.hidden = false;
    const x = Math.min(s.x + 12, wrap.clientWidth - tip.offsetWidth - 4);
    const y = s.y + 12 + tip.offsetHeight > wrap.clientHeight ? s.y - tip.offsetHeight - 10 : s.y + 12;
    tip.style.left = `${Math.max(4, x)}px`;
    tip.style.top = `${Math.max(4, y)}px`;
  }

  const nearest = (e) => {
    const rect = canvas.getBoundingClientRect();
    const mx = e.clientX - rect.left, my = e.clientY - rect.top;
    let best = null, bestD2 = 144;
    for (const [id, s] of screen) {
      const d2 = (s.x - mx) ** 2 + (s.y - my) ** 2;
      if (d2 < bestD2) { bestD2 = d2; best = id; }
    }
    return best;
  };
  canvas.addEventListener("mousemove", (e) => {
    hover = nearest(e);
    canvas.style.cursor = hover != null && nodes.find((n) => n.uid === hover)?.line > 0 ? "pointer" : "default";
    if (hover == null) tip.hidden = true;
  });
  canvas.addEventListener("mouseleave", () => { hover = null; tip.hidden = true; });
  // a node still going — a run, a loop, a thread, an fx block — can be stopped with everything under it (Scheduler#stop_subtree)
  const stoppable = (n) => !!n && hooks.stop && n.uid > 0 && ((n.kind >= KIND.run && n.kind <= KIND.thread && n.state <= STATE.waiting) || (n.kind === KIND.fx && n.state < 3) || (n.kind === KIND.group && n.state === STATE.running));
  canvas.addEventListener("click", (e) => {
    const id = nearest(e);
    const n = nodes.find((x) => x.uid === id);
    if (!n) return;
    if (e.shiftKey && stoppable(n)) { hooks.stop(n.uid); tip.hidden = true; return; }
    if (n.line > 0) hooks.jump(n.line, n.job);
  });

  animateWhileShown(root, draw, { onStop: () => { lastStep = null; } });
  return {
    /** The tree as drawn: [{uid, parent, label, kind, state, x, y}]. */
    snapshot: () => nodes.map((n) => ({ uid: n.uid, parent: n.parent, label: processLabel(n), kind: n.kind, state: n.state, line: n.line, ...(screen.get(n.uid) ?? {}) })),
  };
}

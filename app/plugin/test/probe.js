// SPDX-License-Identifier: MIT
// Copyright (c) 2026 Sam Aaron
//
// Runs before the page. Turns audio off and records what reaches the page, in
// a panel at the bottom right and in the log.
(() => {
  if (window.top !== window) return;

  class NoAudio { constructor() { throw new Error("audio is off in this probe"); } }
  window.AudioContext = NoAudio;
  window.webkitAudioContext = NoAudio;

  const post = (message) => { try { window.ipc.postMessage(JSON.stringify(message)); } catch {} };
  const tally = { app: 0, page: 0, vst3: 0 };
  const last = { app: "-", page: "-", vst3: "-" };
  let panel = null;

  // The editor sits in a shadow root.
  const deepActive = () => {
    let el = document.activeElement;
    while (el?.shadowRoot?.activeElement) el = el.shadowRoot.activeElement;
    return el;
  };
  const editorContent = () => {
    for (const el of document.querySelectorAll("*")) {
      const found = el.shadowRoot?.querySelector(".cm-content");
      if (found) return found;
    }
    return document.querySelector(".cm-content");
  };

  const paint = () => {
    if (!panel) return;
    const el = deepActive();
    const name = el ? el.tagName.toLowerCase() + (el.classList?.[0] ? "." + el.classList[0] : "") : "-";
    panel.firstChild.textContent = [
      `window focus  ${document.hasFocus() ? "yes" : "no"}`,
      `focused on    ${name}`,
      `app saw       ${tally.app}, last ${last.app}`,
      `page saw      ${tally.page}, last ${last.page}`,
      `VST3 saw      ${tally.vst3}, last ${last.vst3}`,
    ].join("\n");
  };

  window.__probe = {
    app(text) { tally.app++; last.app = text; paint(); },
    vst3(text) { tally.vst3++; last.vst3 = text; paint(); },
  };

  const describe = (e) => (e.metaKey ? "⌘" : "") + (e.ctrlKey ? "⌃" : "") + (e.altKey ? "⌥" : "") + e.key;
  // "take": the page takes every key and acts on none, so a sweep of every shortcut changes nothing (test/host.mm)
  let keyMode = "page";
  window.__probe.keyMode = (mode) => { keyMode = mode; return mode; };
  addEventListener("keydown", (e) => {
    tally.page++;
    last.page = describe(e);
    paint();
    // the chord as the page names it, and the commands it is bound to
    const keys = window.sonicPi?.keys;
    const hit = keys?.match?.(e);
    const chord = hit?.chord ?? keys?.chord?.(e) ?? null;
    const ids = hit?.ids ?? null;
    post({ t: "key", chord, ids, key: e.key, code: e.code, meta: e.metaKey, ctrl: e.ctrlKey, alt: e.altKey, shift: e.shiftKey, composing: e.isComposing });
    if (keyMode === "take") { e.preventDefault(); e.stopImmediatePropagation(); }
  }, true);
  // A copy, cut or paste reaches the page and goes no further: the person's clipboard is never the test's.
  for (const type of ["copy", "cut", "paste"]) {
    addEventListener(type, (e) => { e.preventDefault(); e.stopImmediatePropagation(); post({ t: "clipboard", type }); }, true);
  }

  document.addEventListener("input", (e) => {
    post({ t: "doc", kind: e.inputType, data: e.data, text: editorContent()?.textContent.slice(-20000) ?? null });
  }, true);

  for (const type of ["compositionstart", "compositionend"]) {
    document.addEventListener(type, (e) => post({ t: type, data: e.data }), true);
  }
  for (const type of ["focus", "blur"]) addEventListener(type, () => { post({ t: type }); paint(); });
  addEventListener("resize", () => post({ t: "size", w: innerWidth, h: innerHeight }));
  addEventListener("error", (e) => post({ t: "error", message: e.message ?? String(e.target?.src ?? e.target?.href ?? "resource"), source: e.filename ?? null, line: e.lineno ?? null }), true);
  addEventListener("unhandledrejection", (e) => post({ t: "rejection", message: String(e.reason) }));

  addEventListener("DOMContentLoaded", () => {
    panel = document.createElement("div");
    panel.style.cssText = "position:fixed;right:8px;bottom:8px;z-index:2147483647;background:rgba(0,0,0,.85);color:#fff;font:11px/1.4 ui-monospace,monospace;padding:8px 10px;border-radius:6px;";
    const text = document.createElement("pre");
    text.style.cssText = "margin:0 0 6px;font:inherit;color:inherit;background:none;";
    const label = document.createElement("label");
    const box = document.createElement("input");
    box.type = "checkbox";
    box.checked = true;
    box.addEventListener("change", () => post({ t: "forward", on: box.checked }));
    label.append(box, " Pass Cmd keys to the page");
    panel.append(text, label);
    document.documentElement.append(panel);
    paint();
  });

  // The session's status as the page's views read it (a card's Stop is live while one of its runs is in it).
  let lastJobs = null;
  setInterval(() => {
    const status = window.sonicPi?.session?.status?.();
    if (!status) return;
    const jobs = JSON.stringify(status.jobs ?? []);
    if (jobs === lastJobs) return;
    lastJobs = jobs;
    post({ t: "status", jobs: status.jobs ?? [] });
  }, 100);

  // The log pane's newest text when it changes, and the page's clock as sonic_pi.js reads it, against the wall's.
  // What the page draws from the engine's copies, as it changes: the main scope's peak, whether a live loop's scope
  // in the editor has drawn more than a flat line (lit as the browser check counts it), and the engine's blocks
  // counted in its metrics.
  let lastDrawn = "", peak = 0;
  const watchScope = () => {
    const { L, R } = window.sonicPi?.scope ?? {};
    for (let i = 0; L && i < L.length; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
    requestAnimationFrame(watchScope);
  };
  requestAnimationFrame(watchScope);
  setInterval(() => {
    const root = document.querySelector("#editor-mount")?.shadowRoot;
    let loop = false;
    for (const c of root?.querySelectorAll(".sp-loop-scope") ?? []) {
      const d = c.getContext("2d").getImageData(0, 0, c.width, c.height).data;
      let lit = 0;
      for (let i = 3; i < d.length; i += 4) if (d[i] > 8) lit++;
      if (lit > c.width * 1.4) loop = true;
    }
    const processed = window.sonicPi?.engine?.getMetricsArray?.()?.[0] ?? 0;
    const now = JSON.stringify({ scope: peak > 0.01, loop, metrics: processed > 0 });
    peak = 0;
    if (now !== lastDrawn) { lastDrawn = now; post({ t: "drawn", ...JSON.parse(now), processed }); }
  }, 500);

  // What the System log's tabs hold of the instance's lines, and how often a marked line is in the Log pane: as either
  // changes.
  let lastTabs = "";
  setInterval(() => {
    const logs = window.sonicPi?.logs;
    if (!logs) return;
    const has = (tab, text) => logs.recent(tab, 500).some((line) => String(line).includes(text));
    // the desktop's lines (the pane holds the phone's rows of the same lines beside them), in its shadow root
    const box = document.querySelector("#log");
    const pane = [...((box?.shadowRoot ?? box)?.querySelectorAll(".log-line") ?? [])].map((el) => el.textContent).join("\n");
    const now = JSON.stringify({
      engine: has("SuperSonic", "DSP: SuperSonic"),   // the engine's first line, said as it starts
      runtime: has("Runtime", "load synthdef sonic-pi-beep"),
      marked: pane.split("kept in the log").length - 1,
    });
    if (now !== lastTabs) { lastTabs = now; post({ t: "tabs", ...JSON.parse(now) }); }
  }, 500);

  // What the plugin delivered, by kind, and what the log pane holds: every two seconds while either changes.
  const kinds = {};
  const plugin = window.__sonicPiPlugin, receive = plugin.receive.bind(plugin);
  plugin.receive = (batch) => { for (const [k] of batch) kinds[k] = (kinds[k] ?? 0) + 1; return receive(batch); };
  let lastHeard = "", frames = 0;
  const frame = () => { frames++; requestAnimationFrame(frame); };
  requestAnimationFrame(frame);
  setInterval(() => {
    const pane = document.querySelector("#log");
    const r = pane?.getBoundingClientRect();
    const now = JSON.stringify({ kinds, frames, hidden: document.hidden, pane: r ? [Math.round(r.width), Math.round(r.height)] : null });
    if (now !== lastHeard) { lastHeard = now; post({ t: "heard", now }); }
  }, 2000);
  let lastLog = "";
  setInterval(() => {
    const pane = document.querySelector("#log");
    const text = (pane?.shadowRoot ?? pane)?.textContent?.replace(/\s+/g, " ").trim() ?? "";
    if (text === lastLog) return;
    lastLog = text;
    post({ t: "log", tail: text.slice(-300) });
  }, 200);
  setTimeout(() => post({ t: "clock", page: (performance.timeOrigin + performance.now()) / 1000 + 2208988800, wall: Date.now() / 1000 + 2208988800 }), 3000);

  // The full screen button, as any of it changes: whether it shows on, where its middle is in the web view's own
  // pixels (test/host_win.cpp clicks it there, having no way to run a script in the page), the page's size, and
  // whether the page has the keyboard.
  let lastZen = "";
  setInterval(() => {
    const b = document.getElementById("btn-zen");
    if (!b) return;
    const r = b.getBoundingClientRect(), dpr = devicePixelRatio;
    const now = JSON.stringify({ on: b.classList.contains("on"), x: Math.round((r.x + r.width / 2) * dpr), y: Math.round((r.y + r.height / 2) * dpr), vw: Math.round(innerWidth * dpr), vh: Math.round(innerHeight * dpr), focus: document.hasFocus() });
    if (now !== lastZen) { lastZen = now; post({ t: "zen", ...JSON.parse(now) }); }
  }, 100);

  // Ready once the editor has stopped moving: the page lays itself out in stages.
  let lastAt = "", steady = 0;
  const whenEditor = setInterval(() => {
    const content = editorContent();
    if (!content || document.readyState !== "complete") return;
    const r = content.getBoundingClientRect();
    const at = `${r.x | 0},${r.y | 0},${r.width | 0}`;
    steady = at === lastAt ? steady + 1 : 0;
    lastAt = at;
    if (steady < 5) return;
    clearInterval(whenEditor);
    post({ t: "ready", title: document.title, isolated: crossOriginIsolated, secure: isSecureContext, x: r.x | 0, y: r.y | 0, w: r.width | 0, h: r.height | 0, vw: innerWidth, vh: innerHeight, code: editorContent()?.textContent.slice(0, 400) ?? null });
  }, 100);
})();

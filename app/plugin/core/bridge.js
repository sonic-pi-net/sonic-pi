// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//
// Runs before the page: the page's way to its plugin instance. What the page
// asks (run, stop) goes over the web view's channel; what the instance says
// arrives in batches (receive) and goes to whoever listens. The page knows
// it is in a plugin by this being there (web/sonic_pi.js inPlugin).
(() => {
  if (window.top !== window) return;
  const listeners = new Set();
  const backlog = [];   // what arrived before anyone listened: a log line from before the page asked
  const bytes = (b64) => Uint8Array.from(atob(b64), (c) => c.charCodeAt(0));
  const hand = (fn, kind, b) => {
    try { fn(kind, b); } catch (e) { console.error("plugin message", kind, e); }
  };
  window.__sonicPiPlugin = {
    /** The plugin's version from Cargo.toml (lib.rs sets it). DAWs show the same number. */
    version: window.__sonicPiPluginVersion,
    /** The name of the host's MIDI port, in and out (lib.rs sets it from instance.cpp's). */
    midiPort: window.__sonicPiPluginMidiPort,
    /** One verb for the instance: {t: "run", code, group} and the rest (sonic_pi.js PluginSession). */
    post(message) { window.ipc.postMessage(JSON.stringify(message)); },
    /**
     * fn(kind, bytes) for each thing the instance says; what came before anyone listened is replayed to the first,
     * once this has returned, so fn may stop listening with what this returns.
     */
    listen(fn) {
      listeners.add(fn);
      const missed = backlog.splice(0);
      if (missed.length) queueMicrotask(() => { for (const [kind, b] of missed) hand(fn, kind, b); });
      return () => listeners.delete(fn);
    },
    /** From the plugin: [[kind, base64], ...]. */
    receive(batch) {
      for (const [kind, b64] of batch) {
        const b = bytes(b64);
        if (!listeners.size) backlog.push([kind, b]);
        for (const fn of [...listeners]) hand(fn, kind, b);
      }
    },
  };
  // The page's storage is the instance's, kept in the host's project with the plugin's state (shell.cpp), not the
  // host's WebKit storage, which every Sonic Pi in the host would share: its buffers, its sets, its preferences. Seeded
  // as the editor opens (lib.rs, __sonicPiPluginSeed), each write sent to the plugin. A reload keeps what was written
  // since (this window's sessionStorage, its own); a state the host gives the plugin with the editor open comes in by
  // replaceStore, and the page starts again on it.
  const KEPT = "sp-plugin-store";
  const seeded = (() => {
    try { const kept = sessionStorage.getItem(KEPT); if (kept) return JSON.parse(kept); } catch { /* none kept */ }
    return window.__sonicPiPluginSeed ?? {};
  })();
  const items = new Map(Object.entries(seeded));
  const keep = () => { try { sessionStorage.setItem(KEPT, JSON.stringify(Object.fromEntries(items))); } catch { /* the plugin has it */ } };
  const said = (message) => { try { window.__sonicPiPlugin.post(message); } catch { /* no plugin to tell */ } };
  const storage = {
    get length() { return items.size; },
    key: (i) => [...items.keys()][i] ?? null,
    getItem: (k) => items.get(String(k)) ?? null,
    setItem(k, v) {
      k = String(k); v = String(v);
      if (items.get(k) === v) return;
      items.set(k, v);
      keep();
      said({ t: "store", key: k, value: v });
    },
    removeItem(k) {
      if (!items.delete(String(k))) return;
      keep();
      said({ t: "store", key: String(k), value: null });
    },
    clear() {
      if (!items.size) return;
      items.clear();
      keep();
      said({ t: "store", clear: true });
    },
  };
  Object.defineProperty(window, "localStorage", { value: storage, configurable: true });
  window.__sonicPiPlugin.replaceStore = (entries) => {
    try { sessionStorage.setItem(KEPT, JSON.stringify(entries)); } catch { /* the seed is the old one */ }
    location.reload();
  };

  // what the page reports as an error, in the plugin's log too: there is no console to read in a host
  const consoleError = console.error.bind(console);
  console.error = (...args) => {
    consoleError(...args);
    try { window.__sonicPiPlugin.post({ t: "page-error", text: args.map((a) => a?.stack ?? String(a)).join(" ") }); } catch {}
  };
  // what the page was refused because it is not in the bundle: the plugin loads nothing from the network
  document.addEventListener("securitypolicyviolation", (e) => {
    try { window.__sonicPiPlugin.post({ t: "page-error", text: `refused, not in the bundle: ${e.blockedURI} (${e.violatedDirective})` }); } catch {}
  });
  window.addEventListener("error", (e) => window.__sonicPiPlugin.post({ t: "page-error", text: `${e.message} (${e.filename}:${e.lineno})` }));
  window.addEventListener("unhandledrejection", (e) => window.__sonicPiPlugin.post({ t: "page-error", text: String(e.reason?.stack ?? e.reason) }));
})();

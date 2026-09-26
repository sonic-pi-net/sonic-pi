// SPDX-License-Identifier: AGPL-3.0-or-later
// Logs, as native's Logs tab (LogPanel): what each part of Sonic Pi says as it
// runs, side by side, for finding out what happened when something did not
// sound right. Native tails its log files (GUI, Spider, Daemon, SuperSonic);
// the web has none, so each source here is fed as things happen, from the
// moment the page loads: the page itself (GUI), the language runtime, the
// host that boots and feeds the engine, and SuperSonic. Each is a live tail
// with the time of every line, the last 5000 lines kept, following the end
// unless scrolled back.
const MAX_LINES = 5000;

const el = (tag, cls, text) => {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text != null) e.textContent = text;
  return e;
};

const pad = (n, w = 2) => String(n).padStart(w, "0");
const stamp = (d) => `[${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}.${pad(d.getMilliseconds(), 3)}]`;

/**
 * @param root the pane the sources sit in, left to right
 * @param names the sources, as their titles
 * @returns {{add(name, text), shown()}} add: a line (or lines) to a source;
 *   shown: the pane has just been shown, so the tails that follow the end go there
 */
export function createLogs(root, names) {
  const sources = new Map();
  let frame = 0;

  names.forEach((name, i) => {
    if (i > 0) root.appendChild(divider());
    const pane = el("section", "logs-source");
    pane.appendChild(el("div", "logs-title", name.toUpperCase()));
    const body = el("div", "logs-body");
    body.tabIndex = 0;
    // native binds Copy and Select All to each tail: select all is this pane's lines, not the page
    body.addEventListener("keydown", (e) => {
      if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === "a") {
        e.preventDefault();
        // selectAllChildren, not a range added: the pane is in a shadow root (shadow.js), and WebKit adds no range
        // from inside one to the page's selection (it takes this, and a drag, as Chromium does)
        getSelection().selectAllChildren(body);
      }
    });
    const source = { pane, body, pending: [], lines: 0, following: true };
    body.addEventListener("scroll", () => {
      if (body.clientHeight) source.following = body.scrollTop + body.clientHeight >= body.scrollHeight - 4;
    });
    pane.appendChild(body);
    root.appendChild(pane);
    sources.set(name, source);
  });

  // Dividers invisible at rest that show a grab line on hover, as native's
  // splitter: dragging one moves the room between the sources either side.
  function divider() {
    const d = el("div", "logs-divider");
    d.addEventListener("pointerdown", (e) => {
      const left = d.previousElementSibling, right = d.nextElementSibling;
      if (!left || !right) return;
      d.setPointerCapture(e.pointerId);
      d.classList.add("dragging");
      // from here the sources share the row by their widths now, so a resized window keeps the proportions
      for (const s of sources.values()) {
        s.pane.style.flexGrow = String(s.pane.getBoundingClientRect().width);
        s.pane.style.flexBasis = "0";
      }
      const x0 = e.clientX, lw = left.getBoundingClientRect().width, rw = right.getBoundingClientRect().width;
      const move = (m) => {
        const dx = Math.max(80 - lw, Math.min(rw - 80, m.clientX - x0));
        left.style.flexGrow = String(lw + dx);
        right.style.flexGrow = String(rw - dx);
      };
      const up = () => {
        d.classList.remove("dragging");
        d.removeEventListener("pointermove", move);
        d.removeEventListener("pointerup", up);
        d.removeEventListener("pointercancel", up);
      };
      d.addEventListener("pointermove", move);
      d.addEventListener("pointerup", up);
      d.addEventListener("pointercancel", up);
    });
    return d;
  }

  function flush() {
    frame = 0;
    for (const s of sources.values()) {
      if (!s.pending.length) continue;
      const lines = document.createDocumentFragment();
      for (const [at, text] of s.pending) {
        text.split("\n").forEach((part, i) => {
          const line = el("div", "logs-line");
          if (i === 0) line.appendChild(el("span", "logs-time", `${stamp(at)} `));
          else line.classList.add("logs-more");
          line.appendChild(document.createTextNode(part));
          lines.appendChild(line);
          s.lines++;
        });
      }
      s.pending = [];
      s.body.appendChild(lines);
      for (; s.lines > MAX_LINES; s.lines--) s.body.firstChild.remove();
      if (s.following) s.body.scrollTop = s.body.scrollHeight;
    }
  }

  return {
    add(name, text) {
      const s = sources.get(name);
      if (!s) return;
      s.pending.push([new Date(), String(text)]);
      frame ||= requestAnimationFrame(flush);
    },
    shown() {
      for (const s of sources.values()) if (s.following) s.body.scrollTop = s.body.scrollHeight;
    },
  };
}

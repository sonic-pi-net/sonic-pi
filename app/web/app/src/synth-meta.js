// SPDX-License-Identifier: AGPL-3.0-or-later
// A user's synth, as the GUI shows a built-in one: its metadata (the .json beside its .scsyndef, in the format the
// built-ins are written out in, web/data/synth-meta.json; runtime/lib/sonic_pi/synth_meta.rb installs it in the
// runtime) made into a reference page like theirs (web/data/reference/synths.json), for the docs, completion and the
// live synth. With no metadata, a page made from what its .scsyndef says of its controls: their names and defaults,
// and ranges guessed from them, for dials to turn.

const esc = (t) => String(t).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
const title = (key) => key.split(/[_-]/).map((w) => w.charAt(0).toUpperCase() + w.slice(1)).join(" ");
// its words: paragraphs at blank lines, `code` as code; a metadata file's doc is plain text with that much markdown
const docHTML = (doc) => String(doc ?? "").trim().split(/\n\s*\n/).filter(Boolean).map((p) => `<p>${esc(p).replace(/`([^`]+)`/g, "<code>$1</code>")}</p>`).join("");

/** A synth's metadata → its reference page (the shape web/data/reference/synths.json has). */
export function pageFromMeta(meta, url) {
  const opts = Object.entries(meta.opts ?? {}).map(([name, o]) => {
    const page = { name, default: typeof o.default === "string" ? `:${o.default}` : o.default, doc: o.doc ?? "", slidable: !!o.slidable };
    if (o.type) page.type = o.type;
    if (Array.isArray(o.range) && typeof o.range[0] === "number") {
      page.min = o.range[0];
      // no upper end (a time, say): a dial still needs one, so one that leaves room around the default
      page.max = typeof o.range[1] === "number" ? o.range[1] : Math.max(page.min + 1, 4, (typeof o.default === "number" ? o.default : 1) * 4);
    }
    if (Array.isArray(o.options)) page.options = o.options;
    else if (o.type === "switch") page.options = [0, 1];   // off or on: a switch in the GUI (docs.js choice)
    if (o.group) page.group = String(o.group).toUpperCase();
    return page;
  });
  const description = meta.description ?? meta.doc;   // "doc": the format's first name for it
  return {
    key: meta.name, title: meta.title ?? title(meta.name), summary: meta.summary ?? "",
    doc_html: docHTML(description) || `<p>Loaded from <code>${esc(url)}</code>.</p>`, opts, gui: meta.gui ?? {}, user: true, url,
  };
}

// the controls a synth has but nobody turns: where it plays to, its buffer, its trigger
const PLUMBING = /^(out_?bus|out|in_?bus|bus|buf(num)?|gate|t_?trig|trig|fx_?bus)$/i;
// a range for a control from its name, as Sonic Pi's own opts have them, else from its default
function guess(name, d) {
  if (/^note$|^pitch$/.test(name)) return [0, 127];
  if (/cutoff|lpf|hpf/.test(name)) return [0, 130];
  if (/^pan$/.test(name)) return [-1, 1];
  if (/^amp$|_amp$|level/.test(name)) return [0, Math.max(1, d * 2)];
  if (/^res$|mix|_mix$|coef/.test(name)) return [0, 1];
  if (/freq/.test(name)) return [20, Math.max(2000, d * 4)];
  if (d < 0) return [d * 2, -d * 2];
  return [0, d > 0 ? Math.max(1, d * 4) : 1];
}

/** A synth with no metadata: a page made from its .scsyndef's controls (runtime.js parseSynthdefControls). */
export function pageFromSynthdef(name, derived, url) {
  const opts = derived.controls.filter((c) => !PLUMBING.test(c.name) && !/_slide(_shape|_curve)?$/.test(c.name)).map((c) => {   // a slide's controls glide a change, and are not a sound to dial
    const [min, max] = guess(c.name, c.default);
    return { name: c.name, default: c.default, doc: "", min, max, slidable: false };
  });
  return {
    key: name, title: title(name), opts, gui: {}, user: true, url, derived: true,
    doc_html: `<p>Loaded from <code>${esc(url)}</code>. Its controls are the synthdef's own, their ranges guessed: a <code>${esc(name)}.json</code> beside it says what they are (the format each built-in synth is written in, <code>data/synth-meta.json</code>).</p>`,
  };
}

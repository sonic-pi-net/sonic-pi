// SPDX-License-Identifier: AGPL-3.0-or-later
// The plugin's licence screen (app/src/licence.js): what it says, and when it sends what was typed.
import test from "node:test";
import assert from "node:assert/strict";
import { licenceWords, createLicenceGate, SAYS_LICENCE } from "../src/licence.js";

test("a licensed plugin says whose it is", () => {
  assert.equal(licenceWords({ licensed: true, name: "Ada Lovelace", id: "SP-AAAAA-11111" }), "Licensed to Ada Lovelace");
});

test("with no licence entered there is nothing to put right", () => {
  assert.equal(licenceWords({ licensed: false, why: "none" }), "");
});

test("each refusal is said in words a person can act on", () => {
  const said = Object.fromEntries(["not-a-licence", "damaged", "unsupported", "not-genuine", "wrong-product", "revoked"].map((why) => [why, licenceWords({ licensed: false, why })]));
  for (const [why, words] of Object.entries(said)) assert.ok(words.length > 20 && /[.]$/.test(words), `${why}: ${words}`);
  assert.equal(new Set(Object.values(said)).size, 6, "each its own words");
  assert.match(licenceWords({ licensed: false, why: "version", from: "6.0", until: "7.0" }), /Sonic Pi 6\.0/);
  assert.ok(licenceWords({ licensed: false, why: "a word from a newer plugin" }).length > 20, "a reason this page does not know still says something");
});

// The page the gate lives in, as much of it as the gate uses.
function fakeDocument() {
  const el = (tag) => ({
    tag, children: [], attrs: {}, listeners: {}, style: {}, classes: new Set(), textContent: "", value: "", hidden: false,
    append(...kids) { this.children.push(...kids); }, appendChild(k) { this.children.push(k); return k; },
    setAttribute(k, v) { this.attrs[k] = v; }, addEventListener(type, fn) { (this.listeners[type] ??= []).push(fn); },
    get classList() { const c = this.classes; return { add: (x) => c.add(x), remove: (x) => c.delete(x), toggle: (x, on) => (on ? c.add(x) : c.delete(x)), contains: (x) => c.has(x) }; },
    remove() { this.removed = true; }, focus() { this.focused = true; },
    fire(type, e = {}) { for (const fn of this.listeners[type] ?? []) fn({ preventDefault() {}, ...e }); },
  });
  return { createElement: el, body: el("body") };
}
function fakePlugin() {
  const posted = [];
  let heard = null;
  return { posted, post: (m) => posted.push(m), listen: (fn) => { heard = fn; return () => {}; }, says: (standing) => heard(SAYS_LICENCE, new TextEncoder().encode(JSON.stringify(standing))) };
}
const find = (node, cls) => (node.classes?.has(cls) ? node : node.children?.map((c) => find(c, cls)).find(Boolean) ?? null);

test("the gate asks how the plugin stands as it opens, and stays out of the way of a licensed plugin", () => {
  const document = fakeDocument(), plugin = fakePlugin(), licensed = [];
  createLicenceGate({ plugin, document, onStanding: (s) => licensed.push(s) });
  assert.deepEqual(plugin.posted, [{ t: "licence" }]);
  assert.equal(document.body.children.length, 0, "nothing is shown before the plugin answers");
  plugin.says({ licensed: true, name: "Ada Lovelace", id: "SP-AAAAA-11111" });
  assert.equal(document.body.children.length, 0, "licensed: no screen at all");
  assert.deepEqual(licensed, [{ licensed: true, name: "Ada Lovelace", id: "SP-AAAAA-11111" }]);
});

test("unlicensed, the screen is shown; a paste is sent at once, and a refusal is said and leaves the text to be put right", () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: false, why: "none" });
  const gate = document.body.children[0];
  assert.ok(gate && !gate.removed, "the screen is there");
  const box = find(gate, "licence-box"), say = find(gate, "licence-say");
  assert.equal(say.textContent, "", "and says nothing is wrong");

  box.value = "Hi Ada, here is your licence: -----BEGIN…";
  box.fire("paste");
  assert.deepEqual(plugin.posted.at(-1), { t: "licenceEnter", text: "Hi Ada, here is your licence: -----BEGIN…" }, "sent as it was pasted, with no button to press");

  plugin.says({ licensed: false, why: "damaged" });
  assert.match(say.textContent, /missing/);
  assert.equal(box.value, "Hi Ada, here is your licence: -----BEGIN…", "what was pasted is still there");
  assert.ok(!gate.removed);
});

test("typing sends nothing until the button is pressed, and an empty box sends nothing at all", () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: false, why: "none" });
  const gate = document.body.children[0], box = find(gate, "licence-box"), button = find(gate, "licence-unlock");
  const before = plugin.posted.length;
  button.fire("click");
  assert.equal(plugin.posted.length, before, "nothing to send");
  box.value = "SPL1.abc";
  box.fire("input");
  assert.equal(plugin.posted.length, before, "typed, not sent");
  button.fire("click");
  assert.deepEqual(plugin.posted.at(-1), { t: "licenceEnter", text: "SPL1.abc" });
});

// A licence taken, up to the thanks: the gate, the timers set, and how the plugin stood each time it said.
function thanked() {
  const document = fakeDocument(), plugin = fakePlugin(), stood = [], waits = [];
  createLicenceGate({ plugin, document, logo: "<svg>the logo</svg>", later: (fn, ms) => waits.push({ fn, ms }), onStanding: (s) => stood.push(s.licensed) });
  plugin.says({ licensed: false, why: "none" });
  const gate = document.body.children[0];
  assert.equal(find(gate, "licence-mark").innerHTML, "<svg>the logo</svg>", "it opens with Sonic Pi's logo");
  find(gate, "licence-box").value = "a licence";
  find(gate, "licence-unlock").fire("click");
  plugin.says({ licensed: true, name: "Ada Lovelace", id: "SP-AAAAA-11111" });
  return { gate, waits, stood };
}
const runAll = (waits) => { while (waits.length) waits.shift().fn(); };

test("a licence taken is thanked by name, with the heart where the logo was, for a good while, and then the screen goes", () => {
  const { gate, waits, stood } = thanked();
  assert.match(find(gate, "licence-say").textContent, /Ada Lovelace/, "thanked by name");
  const mark = find(gate, "licence-mark");
  assert.ok(mark.classes.has("licence-heart") && /<path/.test(mark.innerHTML) && !/the logo/.test(mark.innerHTML), "the heart is for the thanks");
  assert.ok(gate.classes.has("licensed") && !gate.removed, "the thanks are on show");
  assert.ok(waits.length === 1 && waits[0].ms >= 5000, `long enough to be read and enjoyed: ${waits.map((w) => w.ms)} ms`);
  runAll(waits);
  assert.ok(gate.classes.has("going") && gate.removed, "then they fade and the screen goes");
  assert.deepEqual(stood, [false, true]);
});

test("Begin ends the thanks sooner, once", () => {
  const { gate, waits } = thanked();
  const begin = find(gate, "licence-begin");
  assert.equal(begin.textContent, "Begin");
  assert.ok(begin.focused, "the one thing to press has the keyboard");
  begin.fire("click");
  assert.ok(gate.classes.has("going"), "it fades at the press");
  runAll(waits);
  assert.ok(gate.removed);
  begin.fire("click");   // pressed again as it fades: nothing more to do
  assert.equal(waits.length, 0);
});

test("a plugin whose licence stops standing while it is open asks again", () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: true, name: "Ada Lovelace", id: "SP-AAAAA-11111" });
  assert.equal(document.body.children.filter((c) => !c.removed).length, 0);
  plugin.says({ licensed: false, why: "revoked" });   // a run was refused (lib.rs)
  const gate = document.body.children.find((c) => !c.removed);
  assert.match(find(gate, "licence-say").textContent, /revoked/);
});

test("a licence file can be chosen as well as dropped: the button opens the file picker, and the file chosen is sent", async () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: false, why: "none" });
  const gate = document.body.children[0], choose = find(gate, "licence-choose"), picker = find(gate, "licence-file");
  assert.equal(picker.attrs.type, "file");
  let opened = 0;
  picker.click = () => opened++;
  choose.fire("click");
  assert.equal(opened, 1, "the button opens the picker");
  picker.files = [{ text: async () => "the licence in the file" }];
  picker.fire("change");
  await new Promise((r) => setTimeout(r, 0));
  assert.deepEqual(plugin.posted.at(-1), { t: "licenceEnter", text: "the licence in the file" });
});

test("a licence the plugin found copied is offered by its owner's name, and taking it is one press", () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: false, why: "none", copied: "Ada Lovelace" });
  const gate = document.body.children[0], offer = find(gate, "licence-copied");
  assert.ok(!offer.hidden, "the offer is there");
  assert.match(offer.textContent, /Ada Lovelace/);
  offer.fire("click");
  assert.deepEqual(plugin.posted.at(-1), { t: "licenceUseCopied" }, "the plugin reads it again itself: the page never has the clipboard");
  plugin.says({ licensed: true, name: "Ada Lovelace", id: "SP-AAAAA-11111" });
  assert.match(find(gate, "licence-say").textContent, /Thank you, Ada Lovelace/);
});

test("with nothing copied there is no offer", () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: false, why: "none" });
  assert.ok(find(document.body.children[0], "licence-copied").hidden);
});

test("a licence kept from before that no longer stands is said to be the one on this computer", () => {
  // nothing was entered: the screen opens on a refusal of what the plugin had kept (a licence since revoked)
  assert.equal(licenceWords({ licensed: false, why: "revoked" }, { kept: true }), "The licence on this computer has been revoked. Paste a new one, or write to us if you think that is a mistake.");
  assert.match(licenceWords({ licensed: false, why: "damaged" }, { kept: true }), /^The licence on this computer /);
  assert.match(licenceWords({ licensed: false, why: "version", from: "6.0", until: "7.0" }, { kept: true }), /^The licence on this computer is for Sonic Pi 6\.0/);
  assert.equal(licenceWords({ licensed: false, why: "none" }, { kept: true }), "");
  assert.equal(licenceWords({ licensed: false, why: "revoked" }), "That licence has been revoked. Write to us if you think that is a mistake.", "one a person entered is still 'that licence'");
});

test("the screen opens saying which licence was refused, and one entered afterwards is 'that licence'", () => {
  const document = fakeDocument(), plugin = fakePlugin();
  createLicenceGate({ plugin, document, later: (fn) => fn() });
  plugin.says({ licensed: false, why: "revoked" });   // the licence the plugin had kept
  const gate = document.body.children.find((c) => !c.removed);
  assert.match(find(gate, "licence-say").textContent, /^The licence on this computer has been revoked/);
  assert.ok(!gate.classList.contains("refused"), "a refusal is told in words: nothing is marked in red");
  const box = find(gate, "licence-box");
  box.value = "not a licence";
  find(gate, "licence-unlock").fire("click");
  plugin.says({ licensed: false, why: "not-a-licence" });
  assert.match(find(gate, "licence-say").textContent, /^That has no licence in it/);
});

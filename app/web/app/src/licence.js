// SPDX-License-Identifier: AGPL-3.0-or-later
// The plugin's licence, as its page shows it. The plugin itself says how it stands (app/plugin/core/licence.rs: it
// reads the licence its user keeps, checks it with no network, and runs no code without one); this is the screen
// that asks for a licence when there is none, and the words for why one was refused.
//
// Entering one is a paste: the email the licence came in, whole, or the licence alone. A paste is sent at once, with
// no button to find; a refusal says what to do about it and leaves the text there to be put right; a licence taken
// is thanked by name: the thanks fade in, stay a few seconds and fade out, or go when Begin is pressed.
//
//   page → plugin   { t: "licence" }                  how do I stand?
//                   { t: "licenceEnter", text }       a licence a person entered
//                   { t: "licenceUseCopied" }         take the licence the plugin found copied
//   plugin → page   SAYS_LICENCE, JSON                { licensed: true, name, id } or { licensed: false, why, from?, until?, copied? }
//                                                     copied: whose licence the plugin found on the clipboard, if it found one

/** The plugin's message kind for how it stands (app/plugin/core/instance.rs SAYS_LICENCE). */
export const SAYS_LICENCE = 15;

const THANKS_MS = 5400;   // how long the thanks are on show before they fade
const FADE_MS = 900;      // the fade, in and out (style.css)

const REFUSED = {
  "not-a-licence": "That has no licence in it. Paste the email your licence came in, or the licence itself.",
  damaged: "Part of that licence is missing. Paste the whole of it, down to its last line.",
  unsupported: "That licence is newer than this plugin. Update the plugin and enter it again.",
  "not-genuine": "That licence could not be verified. Paste it exactly as it was sent to you.",
  "wrong-product": "That licence is for something else, not the Sonic Pi plugin.",
  revoked: "That licence has been revoked. Write to us if you think that is a mistake.",
};

// Sonic Pi's heart, as the support card has it: filled, in the accent, with a glow (style.css). For the thanks.
const HEART = '<svg viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><path d="M6.979 3.074a6 6 0 0 1 4.988 1.425l.037 .033l.034 -.03a6 6 0 0 1 4.733 -1.44l.246 .036a6 6 0 0 1 3.364 10.008l-.18 .185l-.048 .041l-7.45 7.379a1 1 0 0 1 -1.313 .082l-.094 -.082l-7.493 -7.422a6 6 0 0 1 3.176 -10.215z"/></svg>';

/** What a person is told about how the plugin stands: whose it is, or why what they entered was not taken ("" with nothing entered). */
export function licenceWords(standing) {
  if (standing.licensed) return `Licensed to ${standing.name}`;
  if (standing.why === "none") return "";
  if (standing.why === "version") return `That licence is for Sonic Pi ${standing.from} up to ${standing.until}, which this plugin is not.`;
  return REFUSED[standing.why] ?? "That licence was not taken. Paste it exactly as it was sent to you.";
}

/**
 * The licence screen, shown over the page while the plugin is unlicensed.
 * @param plugin      window.__sonicPiPlugin: post(message), listen(fn(kind, bytes))
 * @param document    where the screen goes (its body)
 * @param onStanding  told each time the plugin says how it stands
 * @param logo        markup for Sonic Pi's mark: what the screen opens with (the heart is for the thanks)
 * @param later       setTimeout, for the moment the thanks are on show
 */
export function createLicenceGate({ plugin, document, onStanding = () => {}, logo = "", later = (fn, ms) => setTimeout(fn, ms) }) {
  const el = (tag, cls, text) => { const e = document.createElement(tag); if (cls) e.classList.add(cls); if (text != null) e.textContent = text; return e; };
  let gate = null, card = null, box = null, say = null, copied = null, mark = null, entered = false;

  function show() {
    if (gate) return;
    gate = el("div", "licence-gate");
    gate.setAttribute("role", "dialog");
    gate.setAttribute("aria-modal", "true");
    gate.setAttribute("aria-label", "Enter your licence");
    card = el("div", "licence-card");
    mark = el("div", "licence-mark");
    // the logo with ids of its own: the page may hold the same drawing elsewhere, out of sight, and a mask found by
    // id there (the first of the two) cuts nothing here
    mark.innerHTML = logo.replace(/\bid="([^"]+)"/g, 'id="$1-licence"').replace(/(url\(#|href="#)([^")]+)/g, "$1$2-licence");
    const lead = el("p", "licence-lead");
    lead.innerHTML = 'Paste your licence to <b>begin</b>.';
    box = el("textarea", "licence-box");
    box.setAttribute("aria-label", "Your licence");
    box.setAttribute("placeholder", "Paste your licence here");
    box.setAttribute("spellcheck", "false");
    box.setAttribute("rows", "5");
    say = el("p", "licence-say");
    say.setAttribute("role", "status");
    say.setAttribute("aria-live", "polite");
    const unlock = el("button", "licence-unlock", "Unlock");
    unlock.setAttribute("type", "button");
    const enter = () => { const text = box.value; if (!text.trim()) return; entered = true; plugin.post({ t: "licenceEnter", text }); };
    // a paste is the whole of it: sent once the box has it (the event comes before the text is in)
    box.addEventListener("paste", () => later(enter, 0));
    box.addEventListener("input", () => { say.textContent = ""; gate.classList.remove("refused"); });
    unlock.addEventListener("click", enter);
    // a licence saved as a file: dropped on the screen, or chosen
    const take = (file) => { if (file?.text) file.text().then((text) => { box.value = text; enter(); }).catch(() => {}); };
    gate.addEventListener("dragover", (e) => e.preventDefault());
    gate.addEventListener("drop", (e) => { e.preventDefault(); take(e.dataTransfer?.files?.[0]); });
    const picker = el("input", "licence-file");
    picker.setAttribute("type", "file");
    picker.setAttribute("accept", ".txt,.eml,text/plain,message/rfc822");
    picker.hidden = true;
    picker.addEventListener("change", () => take(picker.files?.[0]));
    const choose = el("button", "licence-choose", "or choose a licence file");
    choose.setAttribute("type", "button");
    choose.addEventListener("click", () => picker.click());
    // a licence the plugin found copied (the plugin reads the clipboard, where the system lets it without asking:
    // the page never does): offered by its owner's name, and taken by the plugin itself at a press
    copied = el("button", "licence-copied");
    copied.setAttribute("type", "button");
    copied.hidden = true;
    copied.addEventListener("click", () => { entered = true; plugin.post({ t: "licenceUseCopied" }); });
    card.append(mark, el("h1", "licence-title", "Welcome to the Sonic Pi Plugin"), lead, copied, box, say, unlock, choose, picker,
      el("p", "licence-buy", "No licence yet? Get one at sonic-pi.net/plugin"));
    gate.appendChild(card);
    document.body.appendChild(gate);
    box.focus();
  }

  function heard(standing) {
    onStanding(standing);
    if (!standing.licensed) {
      show();
      copied.hidden = !standing.copied;
      copied.textContent = standing.copied ? `Use the licence you copied, for ${standing.copied}` : "";
      say.textContent = licenceWords(standing);
      gate.classList.toggle("refused", standing.why !== "none");
      return;
    }
    if (!gate) return;
    const going = gate;
    gate = null;
    if (!entered) return going.remove();   // licensed from elsewhere (another instance): nothing to thank
    entered = false;
    mark.innerHTML = HEART;   // the logo asked; the heart thanks
    mark.classList.add("licence-heart");
    say.textContent = `Thank you, ${standing.name}.`;
    going.classList.remove("refused");
    going.classList.add("licensed");
    // the thanks fade in, stay a good while and fade out; Begin, which has the keyboard, ends them sooner
    let leaving = false;
    const leave = () => { if (leaving) return; leaving = true; going.classList.add("going"); later(() => going.remove(), FADE_MS); };
    const begin = el("button", "licence-begin", "Begin");
    begin.setAttribute("type", "button");
    begin.addEventListener("click", leave);
    card.appendChild(begin);
    begin.focus();
    later(leave, THANKS_MS);
  }

  plugin.listen((kind, bytes) => {
    if (kind !== SAYS_LICENCE) return;
    try { heard(JSON.parse(new TextDecoder().decode(bytes))); } catch { /* not ours to read */ }
  });
  plugin.post({ t: "licence" });
}

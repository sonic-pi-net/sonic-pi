// SPDX-License-Identifier: AGPL-3.0-or-later
// The plugin page's form for being told when the plugin is on sale: an email address, sent to the site's own
// server, where nginx writes it as a line of a file: there is no program behind it (how is the server's own
// business, kept with it and not here). The form is
// under the page's Buy button while the store is not open (site/pages/plugin.html: form.plugin-notify).
//
//   GET notify?email=…&website=…   → 200: taken  |  429: too many from this sender
//
// `website` is a field no person sees: a program that fills forms in fills it, and nothing of its form is kept. The
// server cannot tell an address from anything else with an @ in it, so whether it looks like one is asked here.

/** What the form says for each outcome. */
export const SAID = {
  sending: "Sending…",
  ok: "Thanks for your interest, we'll email you just as soon as the plugin is on sale",
  invalid: "That doesn't look like an email address.",
  slow: "Too many tries. Wait a minute and try again.",
  fail: "That didn't work. Please try again in a moment.",
};

const ADDRESS = /^[^@\s]+@[^@\s]+\.[^@\s]+$/;

/**
 * Sends an address, and says how it went: a key of SAID.
 * @param get  sends the fields to the server: (fields) => { status }
 */
export async function ask({ email = "", website = "" }, get) {
  email = email.trim();
  if (email.length > 254 || !ADDRESS.test(email)) return "invalid";
  try {
    const { status } = await get({ email, website });
    return status === 200 ? "ok" : status === 429 ? "slow" : "fail";
  } catch { return "fail"; }
}

const send = (fields) => fetch(`notify?${new URLSearchParams(fields)}`, { cache: "no-store" });

/** The form at work: sent without leaving the page, its outcome said under it; once an address is kept the thanks are all that is left. */
export function mountNotify(form, get = send) {
  const said = form.querySelector(".notify-said"), field = form.elements.email, button = form.querySelector("button");
  form.addEventListener("submit", async (e) => {
    e.preventDefault();
    if (button.disabled) return;
    button.disabled = true;
    said.textContent = SAID.sending;
    const how = await ask({ email: field.value, website: form.elements.website?.value ?? "" }, get);
    said.textContent = SAID[how];
    form.classList.toggle("refused", how !== "ok");
    if (how === "ok") form.classList.add("told");   // the field and the button go: there is nothing more to do here
    else { button.disabled = false; field.focus(); }
  });
}

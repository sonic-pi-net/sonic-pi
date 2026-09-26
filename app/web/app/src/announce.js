// SPDX-License-Identifier: AGPL-3.0-or-later
// What a screen reader hears, as native Sonic Pi's MainWindow::announce
// (app/gui/utils/announcementpolicy.h): a short message, spoken politely or
// assertively, in a category the player can silence — native's "Speak Run and
// Stop" quiets the transport, errors always speak. Two live regions off screen
// carry it; a sighted player never sees them.

/** Native's Announcement categories. */
export const Announcement = { General: "general", Transport: "transport", Navigation: "navigation", Error: "error" };

const policy = { speakTransport: true };
export const setSpeakTransport = (on) => { policy.speakTransport = !!on; };
export const speaksTransport = () => policy.speakTransport;

const regions = {};
function region(assertive) {
  const key = assertive ? "assertive" : "polite";
  if (!regions[key]) {
    const r = document.createElement("div");
    r.id = `announce-${key}`;
    r.className = "sr-only sr-news";
    r.setAttribute("aria-live", key);
    r.setAttribute("aria-relevant", "additions");
    document.body.appendChild(r);
    regions[key] = r;
  }
  return regions[key];
}

const QUIET_AFTER = 10000;   // ms: as shared.css .sr-news says it
const said = new WeakMap();   // a message → when it was said

/**
 * Speak `message`. `assertive` interrupts what the reader is saying, as
 * native's errors and navigation do; a category the player has silenced is
 * dropped. Each message is a node of its own, so two in one frame are both
 * heard, and the same message twice is spoken twice; a node goes quiet after a
 * while, so a reader arriving at the region does not find stale news. Hidden
 * then by its own animation (shared.css .sr-news), which changes nothing in the
 * page, and taken out as the next is said: the region changes only as
 * something is said, never on a timer as a program plays, where a page-wide
 * watcher (an ad blocker's) would hear it (shadow.js).
 */
export function announce(message, assertive = false, category = Announcement.General) {
  const text = String(message ?? "").trim();
  if (!text) return;
  if (category === Announcement.Transport && !policy.speakTransport) return;
  const r = region(assertive), now = performance.now();
  for (const old of [...r.children]) if (now - (said.get(old) ?? 0) >= QUIET_AFTER) old.remove();
  const line = document.createElement("div");
  line.textContent = text;
  said.set(line, now);
  r.appendChild(line);
}

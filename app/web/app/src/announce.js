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
    r.className = "sr-only";
    r.setAttribute("aria-live", key);
    r.setAttribute("aria-relevant", "additions");
    document.body.appendChild(r);
    regions[key] = r;
  }
  return regions[key];
}

/**
 * Speak `message`. `assertive` interrupts what the reader is saying, as
 * native's errors and navigation do; a category the player has silenced is
 * dropped. Each message is a node of its own, so two in one frame are both
 * heard, and the same message twice is spoken twice; a node goes after a while,
 * so a reader arriving at the region does not find stale news.
 */
export function announce(message, assertive = false, category = Announcement.General) {
  const text = String(message ?? "").trim();
  if (!text) return;
  if (category === Announcement.Transport && !policy.speakTransport) return;
  const r = region(assertive);
  const line = document.createElement("div");
  line.textContent = text;
  r.appendChild(line);
  setTimeout(() => line.remove(), 10000);
}

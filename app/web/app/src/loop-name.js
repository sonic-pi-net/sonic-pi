// SPDX-License-Identifier: AGPL-3.0-or-later
// A live loop's name as written, from its thread's: the runtime names a live
// loop's thread live_loop_<name> (lang.rb live_loop).

/** The loop's name as written, or null for a thread that is no live loop's. */
export function loopName(threadName) {
  const m = /^live_loop_(.*)$/s.exec(threadName ?? "");
  return m ? m[1] : null;
}

// SPDX-License-Identifier: AGPL-3.0-or-later
// The web's colour schemes are native's (scripts/gen-themes.mjs reads them out of app/gui/model/sonicpitheme.cpp into
// web/theme/themes.json). A change to native's themes the web has not taken fails here, rather than the web quietly
// keeping the old colours: 6 November's log colours for Signal, Mild Dark and Phosphor sat out of the web for days.
import { test } from "node:test";
import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import path from "node:path";
import { fileURLToPath } from "node:url";

const script = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../../scripts/gen-themes.mjs");

test("the web's colour schemes are native's as they are now", () => {
  try {
    execFileSync(process.execPath, [script, "--check"], { encoding: "utf8", stdio: "pipe" });
  } catch (e) {
    assert.fail(`${e.stdout ?? ""}${e.stderr ?? ""}`.trim() || e.message);
  }
});

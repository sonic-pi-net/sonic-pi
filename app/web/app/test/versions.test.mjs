// SPDX-License-Identifier: AGPL-3.0-or-later
// Tests for app/src/versions.js, which formats the versions the page shows.
import { test } from "node:test";
import assert from "node:assert/strict";
import { releaseVersion, versionLine, targetVersion } from "../src/versions.js";

test("formats VERSION like native's Version#to_s, with major.minor for the status line", () => {
  assert.deepEqual(releaseVersion("5.1.0-beta1"), { full: "v5.1.0-beta1", short: "v5.1" });
  assert.deepEqual(releaseVersion("5.1.0"), { full: "v5.1", short: "v5.1" });
  assert.deepEqual(releaseVersion("5.1.2"), { full: "v5.1.2", short: "v5.1" });
  assert.deepEqual(releaseVersion("v6.0.0-RC3"), { full: "v6.0.0-RC3", short: "v6.0" });
  assert.deepEqual(releaseVersion("6\n"), { full: "v6.0", short: "v6.0" });   // VERSION ends in a newline
});

test("throws on a VERSION that native would reject", () => {
  assert.throws(() => releaseVersion("5.1-beta1"), /Malformed version/);
  assert.throws(() => releaseVersion(""), /Malformed version/);
});

test("status line: Sonic Pi version plus 'on Web' or 'VST'", () => {
  assert.equal(versionLine("v5.1", false), "Sonic Pi v5.1 on Web");
  assert.equal(versionLine("v5.1", true), "Sonic Pi v5.1 VST");
});

test("About's App row: 'Web' or 'VST' plus that target's own version", () => {
  assert.equal(targetVersion(false, { web: "1.0.0", plugin: "0.1.0" }), "Web 1.0.0");
  assert.equal(targetVersion(true, { web: "1.0.0", plugin: "0.1.0" }), "VST 0.1.0");
});

// SPDX-License-Identifier: AGPL-3.0-or-later
// Tests for app/src/versions.js, which formats the versions the page shows.
import { test } from "node:test";
import assert from "node:assert/strict";
import { releaseVersion, versionLine, targetVersion } from "../src/versions.js";

test("formats VERSION like native's Version#to_s, with major.minor for the status line", () => {
  assert.deepEqual(releaseVersion("5.1.0-beta1"), { full: "v5.1.0-beta1", stable: "v5.1", short: "v5.1", pre: "beta1" });
  assert.deepEqual(releaseVersion("5.1.0"), { full: "v5.1", stable: "v5.1", short: "v5.1", pre: "" });
  assert.deepEqual(releaseVersion("5.1.2"), { full: "v5.1.2", stable: "v5.1.2", short: "v5.1", pre: "" });
  assert.deepEqual(releaseVersion("v6.0.0-RC3"), { full: "v6.0.0-RC3", stable: "v6.0", short: "v6.0", pre: "RC3" });
  assert.deepEqual(releaseVersion("6\n"), { full: "v6.0", stable: "v6.0", short: "v6.0", pre: "" });   // VERSION ends in a newline
});

test("throws on a VERSION that native would reject", () => {
  assert.throws(() => releaseVersion("5.1-beta1"), /Malformed version/);
  assert.throws(() => releaseVersion(""), /Malformed version/);
});

// Sonic Pi's version names the language, the same on every target. Whether a build is a beta is the build's own
// business: the desktop app is at 5.1.0-beta1, and the web and the plugin, with versions of their own, are not.
test("status line: Sonic Pi's version, then 'Web' or the plugin's format, and the target's own pre-release word if it has one", () => {
  assert.equal(versionLine(releaseVersion("5.1.0"), "", "1.0.0"), "Sonic Pi v5.1 Web");
  assert.equal(versionLine(releaseVersion("5.1.0"), "VST", "1.0.0"), "Sonic Pi v5.1 VST");
  assert.equal(versionLine(releaseVersion("5.1.0"), "AU", "1.0.0"), "Sonic Pi v5.1 AU");
  assert.equal(versionLine(releaseVersion("5.1.0-beta1"), "VST", "1.0.0"), "Sonic Pi v5.1 VST", "the desktop's beta is not the plugin's");
  assert.equal(versionLine(releaseVersion("5.1.0-beta1"), "", "1.0.0"), "Sonic Pi v5.1 Web", "nor the web's");
  assert.equal(versionLine(releaseVersion("5.1.0-beta1"), "VST", "1.1.0-beta2"), "Sonic Pi v5.1 VST beta2", "a plugin that is itself a beta says so");
  assert.equal(versionLine(releaseVersion("5.1.0"), "", "2.0.0-rc1"), "Sonic Pi v5.1 Web rc1");
  assert.equal(versionLine(releaseVersion("5.1.0-beta1"), "VST"), "Sonic Pi v5.1 VST", "with no version of its own known, no word");
});

test("the language's version is Sonic Pi's without any pre-release word", () => {
  assert.equal(releaseVersion("5.1.0-beta1").stable, "v5.1");
  assert.equal(releaseVersion("5.1.2-rc1").stable, "v5.1.2");
  assert.equal(releaseVersion("5.1.0").stable, "v5.1");
});

test("About's App row: 'Web' or the plugin's format, plus that target's own version", () => {
  assert.equal(targetVersion("", { web: "1.0.0", plugin: "0.1.0" }), "Web 1.0.0");
  assert.equal(targetVersion("VST", { web: "1.0.0", plugin: "0.1.0" }), "VST 0.1.0");
  assert.equal(targetVersion("AU", { web: "1.0.0", plugin: "0.1.0" }), "AU 0.1.0");
});

// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
/**
 * Formats the versions the page shows. The Sonic Pi version comes from the repository's VERSION file and is
 * formatted the same way as native's Version#to_s (server/ruby/lib/sonicpi/version.rb): 5.1.0-beta1 becomes
 * v5.1.0-beta1 and 5.1.0 becomes v5.1. The status line uses major.minor only, and a page shows the language's
 * version without the pre-release word, which is the desktop build's. The web and the VST also have their
 * own versions, from package.json and Cargo.toml. scripts/lib/versions.mjs reads the files; this file only formats.
 */

/** Parses VERSION's text into { full, stable, short, pre }: pre is a pre-release's word ("beta1" for 5.1.0-beta1), or
 * "", and stable is the version without it (v5.1): the language's, which is the same on every target.
 * Accepts the same forms as native's Version.init_from_string and throws the same error for anything else. */
export function releaseVersion(text) {
  const s = String(text).trim();
  const m = /^[vV]?(\d+)(?:\.(\d+)(?:\.(\d+)(?:-(.+))?)?)?$/.exec(s);
  if (!m) throw new Error(`Malformed version: ${s}. Expecting something of the form v2.1.0-RC3`);
  const [major, minor, patch] = [m[1], m[2] ?? 0, m[3] ?? 0].map(Number), dev = m[4];
  const stable = patch === 0 ? `v${major}.${minor}` : `v${major}.${minor}.${patch}`;
  const full = dev ? `v${major}.${minor}.${patch}-${dev}` : stable;
  return { full, stable, short: `v${major}.${minor}`, pre: dev ?? "" };
}

/** The status line: "Sonic Pi v5.1 VST" (or AU, or Web). Sonic Pi's version names the language, the same on every
 * target; whether a build is a pre-release is the build's own business. VERSION's "beta1" is the desktop app's
 * (native's line is "Sonic Pi v5.1.0-beta1 on Mac"), so it is not shown here: the word shown is the pre-release
 * part of this target's `own` version (package.json's for the web, Cargo.toml's for the plugin), if it has one.
 * `plugin` is the plugin's format ("VST", "AU"), or "" on the web. */
export const versionLine = (release, plugin, own = "") => {
  const pre = /^\d+(?:\.\d+)*-(.+)$/.exec(String(own).trim())?.[1] ?? "";
  return `Sonic Pi ${release.short} ${plugin || "Web"}${pre ? ` ${pre}` : ""}`;
};

/** The text for About's App row: "Web 1.0.0", "VST 0.1.0" or "AU 0.1.0". */
export const targetVersion = (plugin, own) => (plugin ? `${plugin} ${own.plugin}` : `Web ${own.web}`);

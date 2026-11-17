// SPDX-License-Identifier: AGPL-3.0-or-later
// Reads the versions the build passes to the page: the Sonic Pi version from the repository's VERSION file (native
// reads the same file) and the web version from package.json. app/src/versions.js formats them.
import fs from "node:fs";
import path from "node:path";
import { releaseVersion } from "../../app/src/versions.js";

/** Returns { release, web } for the app under ROOT (app/web). Throws on a malformed VERSION, as native's server does. */
export function pageVersions(ROOT) {
  const release = fs.readFileSync(path.join(ROOT, "../../VERSION"), "utf8").trim();
  releaseVersion(release);
  const web = JSON.parse(fs.readFileSync(path.join(ROOT, "package.json"), "utf8")).version;
  if (!web) throw new Error(`no "version" field in ${path.join(ROOT, "package.json")}`);
  return { release, web };
}

#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The site as a zip to drop into a web server: a folder, sonic-pi-web/, that works wherever it is served from (every
// path in it is relative), and beside it the lines nginx needs for all of it to work.
//
// It works as plain static files: SuperSonic, its synthdefs and its samples come from the CDN (build-web.mjs), and
// nothing needs a server of ours. Two response headers make the page cross-origin isolated, which is what lets
// SuperSonic share memory with the page: without them it runs in its postMessage mode, and the recording and the
// scopes that read that memory are not there. A folder cannot send headers, so the zip says how (nginx.conf).
//
//   node scripts/package-web.mjs [build-web.mjs's options: --supersonic cdn[@version] …]
//   → build/sonic-pi-web-<version>.zip
import fs from "node:fs";
import path from "node:path";
import { execFileSync } from "node:child_process";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const WEB = path.join(ROOT, "build/web");
const STAGE = path.join(ROOT, "build/package");
const NAME = "sonic-pi-web";

execFileSync(process.execPath, [path.join(ROOT, "scripts/build-web.mjs"), ...process.argv.slice(2)], { stdio: "inherit" });

let version = "dev";
try { version = execFileSync("git", ["describe", "--tags", "--always"], { cwd: ROOT, encoding: "utf8" }).trim(); } catch {}
const supersonic = JSON.parse(fs.readFileSync(path.join(WEB, "supersonic/version.json"), "utf8"));

fs.rmSync(STAGE, { recursive: true, force: true });
fs.mkdirSync(STAGE, { recursive: true });
fs.cpSync(WEB, path.join(STAGE, NAME), { recursive: true });
fs.writeFileSync(path.join(STAGE, "nginx.conf"), `# Sonic Pi for the web (${version}, SuperSonic ${supersonic.version ?? "?"} from ${supersonic.source ?? "?"})
#
# What is in ${NAME}/ goes where nginx serves the site from (the site's root, or a folder of it: every path is
# relative). For all of it to work (recording, and the scopes that read the engine's memory) the pages and the
# runtime's worker have to be cross-origin isolated: these, in the server block, as sonic-pi.net's /supersonic/
# already has them.
#
# add_header in a location replaces the add_header lines it would otherwise inherit from the server block, so repeat
# any of those you rely on inside it.

location / {
    add_header Cross-Origin-Opener-Policy same-origin always;
    add_header Cross-Origin-Embedder-Policy require-corp always;
    add_header Cross-Origin-Resource-Policy cross-origin always;
}

# nginx's mime.types has no .mjs, and a browser will not run the runtime (runtime/sp_runtime.mjs) served as
# application/octet-stream: without this nothing plays. types in a location replaces the http block's, so this one
# names only its own two.
location ~ \\.(mjs|webmanifest)$ {
    types {
        text/javascript mjs;
        application/manifest+json webmanifest;
    }
    add_header Cross-Origin-Opener-Policy same-origin always;
    add_header Cross-Origin-Embedder-Policy require-corp always;
    add_header Cross-Origin-Resource-Policy cross-origin always;
}

# nginx 1.21 and later know .wasm already (mime.types); for an older one, add to its http block:
#   types { application/wasm wasm; }
`);

const zip = path.join(ROOT, "build", `${NAME}-${version}.zip`);
fs.rmSync(zip, { force: true });
try {
  execFileSync("zip", ["-qr", "-X", zip, NAME, "nginx.conf"], { cwd: STAGE, stdio: "inherit" });
} catch (e) {
  console.error(`could not zip (is the zip command installed?): ${e.message}`);
  process.exit(1);
}
fs.rmSync(STAGE, { recursive: true, force: true });
console.log(`packaged ${path.relative(process.cwd(), zip)} (${(fs.statSync(zip).size / 1048576).toFixed(1)} MB): ${NAME}/ and nginx.conf`);

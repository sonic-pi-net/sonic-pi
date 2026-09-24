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
import zlib from "node:zlib";

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
# relative). These lines go in that site's server block. They say three things:
#
#   - cross-origin isolation (the three Cross-Origin headers): without it there is no recording, and no scopes that
#     read the engine's memory. add_header in a location replaces the ones it would inherit, so every location
#     here repeats them, and repeats any others of the server block's you rely on.
#   - caching: the pages, the code and the runtime are checked with the server on every load (no-cache: a tiny
#     request, answered "not modified" when nothing changed), so a new deploy never runs beside an old file;
#     sounds, fonts and images keep for a day; chunks/, named for their contents, for good.
#   - compression: every file worth compressing sits beside its .br and .gz, made once at build time at the
#     strongest settings. brotli_static serves the .br to a browser that takes brotli (all of them), gzip_static
#     the .gz to one that does not. brotli_static needs nginx's brotli module:
#       apt install libnginx-mod-http-brotli-static
#     without it, leave that one line out: gzip_static is part of nginx.

brotli_static on;
gzip_static on;
gzip_vary on;

location / {
    add_header Cache-Control "no-cache" always;
    add_header Cross-Origin-Opener-Policy same-origin always;
    add_header Cross-Origin-Embedder-Policy require-corp always;
    add_header Cross-Origin-Resource-Policy cross-origin always;
}

# nginx's mime.types has no .mjs, and a browser will not run the runtime (runtime/sp_runtime.mjs) served as
# application/octet-stream: without this nothing plays. types in a location replaces the http block's, so each of
# these names every type it serves.
location ~ \\.(mjs|webmanifest)$ {
    types {
        text/javascript mjs;
        application/manifest+json webmanifest;
    }
    add_header Cache-Control "no-cache" always;
    add_header Cross-Origin-Opener-Policy same-origin always;
    add_header Cross-Origin-Embedder-Policy require-corp always;
    add_header Cross-Origin-Resource-Policy cross-origin always;
}

location ~ \\.(scsyndef|flac|wav|dat|woff2|png|jpg|svg)$ {
    types {
        application/octet-stream scsyndef dat;
        audio/flac flac;
        audio/wav wav;
        font/woff2 woff2;
        image/png png;
        image/jpeg jpg;
        image/svg+xml svg;
    }
    add_header Cache-Control "public, max-age=86400" always;
    add_header Cross-Origin-Opener-Policy same-origin always;
    add_header Cross-Origin-Embedder-Policy require-corp always;
    add_header Cross-Origin-Resource-Policy cross-origin always;
}

# chunks/: what app.js loads when first used, each named for its contents (a new build, a new name), so kept for good
location ~ /chunks/[^/]+\\.js$ {
    add_header Cache-Control "public, max-age=31536000, immutable" always;
    add_header Cross-Origin-Opener-Policy same-origin always;
    add_header Cross-Origin-Embedder-Policy require-corp always;
    add_header Cross-Origin-Resource-Policy cross-origin always;
}

# nginx 1.21 and later know .wasm already (mime.types); for an older one, add to its http block:
#   types { application/wasm wasm; }
`);

// Every file worth compressing beside its .br and .gz (the nginx lines above serve them): brotli at its strongest,
// gzip at its, once, here, so the server spends nothing on it. A file that does not shrink by a tenth is left alone:
// the synthdefs shrink to a fifth, the sounds by a sixth; images and fonts are compressed already.
const COMPRESS = /\.(js|mjs|css|html|json|wasm|svg|webmanifest|txt|md|map|scsyndef|wav|dat)$/;
let raw = 0, br = 0, compressed = 0;
const walk = (dir) => {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const f = path.join(dir, e.name);
    if (e.isDirectory()) { walk(f); continue; }
    if (!COMPRESS.test(e.name)) continue;
    const bytes = fs.readFileSync(f);
    const b = zlib.brotliCompressSync(bytes, { params: { [zlib.constants.BROTLI_PARAM_QUALITY]: 11, [zlib.constants.BROTLI_PARAM_SIZE_HINT]: bytes.length } });
    if (b.length > bytes.length * 0.9) continue;
    fs.writeFileSync(f + ".br", b);
    fs.writeFileSync(f + ".gz", zlib.gzipSync(bytes, { level: 9 }));
    raw += bytes.length; br += b.length; compressed++;
  }
};
walk(path.join(STAGE, NAME));
console.log(`compressed ${compressed} files: ${(raw / 1048576).toFixed(1)} MB → ${(br / 1048576).toFixed(1)} MB as brotli`);

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

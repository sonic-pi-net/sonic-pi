#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// The spec browser's dev server: the repository as static files, specs.json
// generated fresh from the tree, and the endpoints the page needs to run a
// spec or a typed program through the oracle now (or through another
// runtime, with ADAPTER set). The static build (scripts/build-web.mjs) needs
// none of this to show and play the specs.
//
//   node scripts/serve.mjs [--port 8460] [--host 127.0.0.1]
//   node scripts/serve.mjs --host 0.0.0.0 --https        # on the LAN
//   ADAPTER="$PWD/build/runtime/sp-trace" node scripts/serve.mjs
//
// Open http://127.0.0.1:8460/web/ . SuperSonic is served as /web/supersonic/:
// the local build when there is one (the sibling checkout's dist/, or
// SUPERSONIC_DIST), else its released packages on the CDN at package.json's
// supersonicVersion; --supersonic
// local|cdn[@version] says which, --assets cdn[@version] takes the synthdefs
// and samples from the CDN with a local engine (lib/runtime-assets.mjs). The
// two cross-origin isolation headers are sent so SuperSonic can use its
// SharedArrayBuffer transport. Over a LAN
// address the page must be https for an AudioWorklet to boot at all: --https
// serves build/dev-cert/{cert,key}.pem (a self-signed certificate; the
// browser asks once), or --cert and --key name others.
//
// /api/run and /api/eval run programs through the adapter, so they answer
// only on a loopback host unless --allow-eval says otherwise.
import http from "node:http";
import https from "node:https";
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { execFile } from "node:child_process";
import { specsJSON } from "./specs-json.mjs";
import { samplesJSON, TABLES, resolveSupersonic, describeSupersonic, supersonicVersionJSON, supersonicShims, flagValue, trimTable } from "./lib/runtime-assets.mjs";

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), "..");
const args = process.argv.slice(2);
const opt = (name, fallback) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : fallback; };
const PORT = Number(opt("--port", 8460));
const HOST = String(opt("--host", "127.0.0.1"));
const NATIVE_ETC = path.resolve(ROOT, "../../etc");   // Sonic Pi's samples and random streams, above the web app
const trimmed = {};                                   // a random table, with its repeats taken off, kept once
const ADAPTER = process.env.ADAPTER || `ruby ${path.join(ROOT, "oracle/harness/oracle.rb")}`;
const TYPES = {
  ".html": "text/html; charset=utf-8", ".js": "text/javascript", ".mjs": "text/javascript", ".css": "text/css",
  ".json": "application/json", ".rb": "text/plain; charset=utf-8", ".md": "text/plain; charset=utf-8",
  ".flac": "audio/flac", ".wasm": "application/wasm", ".mjs": "text/javascript", ".wav": "audio/wav", ".scsyndef": "application/octet-stream", ".png": "image/png",
  ".svg": "image/svg+xml", ".jpg": "image/jpeg", ".jpeg": "image/jpeg", ".gif": "image/gif", ".webp": "image/webp", ".ico": "image/x-icon", ".webmanifest": "application/manifest+json", ".woff2": "font/woff2", ".woff": "font/woff", ".txt": "text/plain; charset=utf-8", ".pdf": "application/pdf",
};

// Runs a program the page typed: written to a temporary file, traced like a spec.
function evalCode(req, res) {
  let body = "";
  req.on("data", (c) => { body += c; if (body.length > 1 << 20) req.destroy(); });
  req.on("end", () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "sonic-pi-web-"));
    const file = path.join(dir, "playground.rb");
    fs.writeFileSync(file, body);
    runFile(file, "playground", res, () => fs.rmSync(dir, { recursive: true, force: true }));
  });
}

// Runs one spec through the adapter; the spec must be a file under specs/.
function run(spec, res) {
  const rel = path.normalize(spec).replace(/^(\.\.[/\\])+/, "");
  const abs = path.join(ROOT, rel);
  if (!rel.startsWith("specs" + path.sep) || !rel.endsWith(".rb") || !fs.existsSync(abs)) {
    res.writeHead(400, { "content-type": "application/json" });
    return res.end(JSON.stringify({ error: `not a spec: ${spec}` }));
  }
  runFile(abs, rel, res);
}

function runFile(abs, label, res, done = () => {}) {
  const [cmd, ...cmdArgs] = ADAPTER.split(/\s+/);
  const t0 = Date.now();
  execFile(cmd, [...cmdArgs, abs], { maxBuffer: 64 << 20, timeout: 120_000 }, (err, stdout, stderr) => {
    done();
    res.writeHead(200, { "content-type": "application/json" });
    let trace = null;
    try { trace = JSON.parse(stdout); } catch {}
    res.end(JSON.stringify({ spec: label, ok: !err && !!trace, trace, stderr: String(stderr).slice(0, 4000),
                             error: err ? String(err.message).slice(0, 500) : null, ms: Date.now() - t0 }));
  });
}

// The sound's data — synthdefs, samples, the random streams — changes only when SuperSonic or the oracle is rebuilt,
// and a new session would otherwise fetch it all again (the rest is no-store: the app is edited live). The browser
// keeps these a week; past that, or on a hard reload, it asks again with the ETag and a 304 costs next to nothing.
const CACHEABLE = /^\/web\/((supersonic\/)?(synthdefs|samples)\/|buffers\/[^/]+\.wav$)/;
/** Cache headers for a sound-data file; true when the browser's copy is current (a 304 has been sent). */
function cached(req, res, abs) {
  const st = fs.statSync(abs);
  const etag = `"${st.size.toString(16)}-${Math.floor(st.mtimeMs).toString(16)}"`;
  res.setHeader("Cache-Control", "public, max-age=604800");
  res.setHeader("ETag", etag);
  res.setHeader("Last-Modified", st.mtime.toUTCString());
  if (req.headers["if-none-match"] === etag) { res.writeHead(304); res.end(); return true; }
  return false;
}

// Where SuperSonic is, resolved once at startup (below): the local build, or the CDN's packages
let SUPERSONIC = null, supersonicError = null;
function serveSupersonic(rel, res, req = null) {
  if (!SUPERSONIC) { res.writeHead(404, { "content-type": "text/plain" }); return res.end(supersonicError ?? "SuperSonic is not resolved yet"); }
  if (rel === "version.json") { res.writeHead(200, { "content-type": "application/json" }); return res.end(supersonicVersionJSON(SUPERSONIC)); }
  const shim = supersonicShims(SUPERSONIC)[rel];
  if (shim) { res.writeHead(200, { "content-type": "text/javascript" }); return res.end(shim); }
  const dist = SUPERSONIC.dist;
  const abs = dist && path.join(dist, path.normalize(rel));
  if (!abs || !abs.startsWith(dist + path.sep) || !fs.existsSync(abs) || fs.statSync(abs).isDirectory()) { res.writeHead(404); return res.end("not found"); }
  if (req && CACHEABLE.test(`/web/supersonic/${rel}`) && cached(req, res, abs)) return;
  res.writeHead(200, { "content-type": TYPES[path.extname(abs)] || "application/octet-stream" });
  fs.createReadStream(abs).pipe(res);
}

const HTTPS = args.includes("--https");
const LOOPBACK = HOST === "localhost" || HOST === "::1" || HOST.startsWith("127.");
const EVAL = LOOPBACK || args.includes("--allow-eval");
const CERT_DIR = path.join(ROOT, "build/dev-cert");

const handler = (req, res) => {
  const url = new URL(req.url, `http://${req.headers.host}`);
  res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
  res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");
  res.setHeader("Cache-Control", "no-store");
  if (url.pathname === "/api/ping") { res.writeHead(200, { "content-type": "application/json" }); return res.end(JSON.stringify({ adapter: ADAPTER })); }
  if (url.pathname === "/web/specs.json") {            // always fresh from the tree, never the built copy
    res.writeHead(200, { "content-type": "application/json" });
    return res.end(specsJSON(ROOT));
  }
  if (url.pathname === "/web/samples.json") {
    res.writeHead(200, { "content-type": "application/json" });
    return res.end(samplesJSON(ROOT));
  }
  // The page's relative runtime paths, served from where the build puts them
  let p0 = decodeURIComponent(url.pathname);
  if (p0.startsWith("/web/supersonic/")) return serveSupersonic(p0.slice("/web/supersonic/".length), res, req);
  const soundData = CACHEABLE.test(p0);   // the random streams, before their path is mapped to the oracle's
  if (p0.startsWith("/web/runtime/")) p0 = "/build/runtime/" + p0.slice("/web/runtime/".length);
  // the synths are Sonic Pi's own too, in etc/synthdefs/compiled: what the desktop app plays, so the web plays the
  // same build of each one. SuperSonic bundles a set of its own, which drifts from this repo's (its autotuner predates
  // the fix for the pitch tracker's first reading, and is silent)
  if (p0.startsWith("/web/synthdefs/")) {
    const dir = path.join(NATIVE_ETC, "synthdefs/compiled");
    const abs = path.join(dir, path.normalize(p0.slice("/web/synthdefs/".length)));
    if (!abs.startsWith(dir + path.sep) || !fs.existsSync(abs) || fs.statSync(abs).isDirectory()) { res.writeHead(404); return res.end("not found"); }
    if (cached(req, res, abs)) return;
    res.writeHead(200, { "content-type": TYPES[path.extname(abs)] || "application/octet-stream" });
    return fs.createReadStream(abs).pipe(res);
  }
  // the random streams are Sonic Pi's own, in etc/ above this directory: served from there rather than mapped into
  // a path under the web root, which nothing outside it is allowed to reach
  if (p0.startsWith("/web/buffers/")) {
    const abs = path.join(NATIVE_ETC, "buffers", path.normalize(p0.slice("/web/buffers/".length)));
    if (!abs.startsWith(path.join(NATIVE_ETC, "buffers")) || !fs.existsSync(abs)) { res.writeHead(404); return res.end("not found"); }
    // trimmed as a build serves them, so what is developed against is what is deployed (runtime-assets.mjs)
    const body = (trimmed[abs] ??= trimTable(fs.readFileSync(abs)));
    res.writeHead(200, { "content-type": TYPES[path.extname(abs)] || "application/octet-stream", "content-length": body.length });
    return res.end(body);
  }
  url.pathname = p0;
  if (url.pathname === "/api/run" || url.pathname === "/api/eval") {
    if (!EVAL) { res.writeHead(403, { "content-type": "application/json" }); return res.end(JSON.stringify({ error: "running programs is off on a LAN host (--allow-eval)" })); }
    if (url.pathname === "/api/run") return run(url.searchParams.get("spec") || "", res);
    if (req.method === "POST") return evalCode(req, res);
  }
  let p = url.pathname;
  // a directory without its slash: the app is /web/, and a phone typing /web (or scanning a code for it) should land there
  if (p === "/") { res.writeHead(302, { location: "/web/" }); return res.end(); }
  if (!p.endsWith("/") && fs.existsSync(path.join(ROOT, p, "index.html"))) { res.writeHead(301, { location: `${p}/${url.search}` }); return res.end(); }
  if (p.endsWith("/")) p += "index.html";
  const abs = path.join(ROOT, path.normalize(p));
  // never the server's own key
  if (!abs.startsWith(ROOT) || abs.startsWith(CERT_DIR) || !fs.existsSync(abs) || fs.statSync(abs).isDirectory()) {
    res.writeHead(404); return res.end("not found");
  }
  if (soundData && cached(req, res, abs)) return;
  res.writeHead(200, { "content-type": TYPES[path.extname(abs)] || "application/octet-stream" });
  fs.createReadStream(abs).pipe(res);
};

const server = HTTPS
  ? https.createServer({ cert: fs.readFileSync(opt("--cert", path.join(CERT_DIR, "cert.pem"))), key: fs.readFileSync(opt("--key", path.join(CERT_DIR, "key.pem"))) }, handler)
  : http.createServer(handler);
try { SUPERSONIC = await resolveSupersonic(ROOT, process.env.SUPERSONIC || flagValue(args, "--supersonic") || "auto", flagValue(args, "--assets")); } catch (e) { supersonicError = e.message; }
server.listen(PORT, HOST, () => {
  const shown = HOST === "0.0.0.0" || HOST === "::" ? "<this machine's address>" : HOST;
  console.log(`spec browser: ${HTTPS ? "https" : "http"}://${shown}:${PORT}/web/   (adapter: ${ADAPTER}${EVAL ? "" : "; running programs off"})`);
  console.log(SUPERSONIC ? describeSupersonic(SUPERSONIC) : `SuperSonic: none — ${supersonicError}`);
});

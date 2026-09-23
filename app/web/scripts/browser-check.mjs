#!/usr/bin/env node
// SPDX-License-Identifier: AGPL-3.0-or-later
// Drives both pages in headless Chromium and headless WebKit against a
// running dev server and says what worked. Every check runs in both, each
// line saying which engine said it.
// Audio is not heard (headless), but everything up to the bundles reaching
// SuperSonic is: boot, run, live loops, redefinition, errors, stop,
// highlighting, autocomplete, docs, quickstart cards, themes, the share link,
// and the spec browser's verdicts.
//
//   node scripts/serve.mjs &
//   node scripts/browser-check.mjs [http://127.0.0.1:8460/web/]
//
// Needs Playwright (npm install); PLAYWRIGHT=/path/to/playwright/index.mjs
// points at another copy.
import { quiet } from "./lib/quiet.mjs";
const BASE = process.argv[2] ?? "http://127.0.0.1:8460/web/";
const { chromium, webkit, devices } = await import(process.env.PLAYWRIGHT ?? "playwright");

const results = [];
let engine = "";
const say = (mark, name, detail) => console.log(`${engine.padEnd(8)} ${mark} ${name}${detail ? ` — ${detail}` : ""}`);
const check = (name, ok, detail = "") => { results.push({ engine, name, ok }); say(ok ? "ok  " : "FAIL", name, detail); };
// what only one engine can be asked: said with its reason, and counted apart from what passed
const skip = (name, why) => { results.push({ engine, name, ok: true, skipped: true }); say("skip", name, why); };
// the dev server speaks https with a certificate it signed itself (scripts/serve.mjs): the browser is told to go on
const HTTPS = { ignoreHTTPSErrors: BASE.startsWith("https:") };

for (const [name, engineType] of [["chromium", chromium], ["webkit", webkit]]) {
  engine = name;
  // Chromium wants telling that a page may make a sound unasked; WebKit has no such switch
  const browser = quiet(await engineType.launch(name === "chromium" ? { args: ["--autoplay-policy=no-user-gesture-required", "--mute-audio"] } : {}));
  try {
    // ── the app ──
    const page = await browser.newPage({ ...HTTPS, viewport: { width: 1400, height: 900 } });
    const pageErrors = [];
    page.on("pageerror", (e) => pageErrors.push(e.message));
    // where each synthdef is fetched from: the engine loads them in a worker, so this watches the whole context
    // rather than the page's own resource timings, which Chromium leaves those out of
    const defURLs = [];
    page.context().on("request", (r) => { if (r.url().includes(".scsyndef")) defURLs.push(new URL(r.url()).pathname); });
    // a clean slate before app.js runs, rather than a load-clear-reload: the reload cancels the first load's own
    // requests, and WebKit calls the runtime's module worker going with them an uncaught error
    // (and the first visit's pointer to Help already seen: it floats over the panel the checks click in; the phone
    // page below checks it)
    await page.addInitScript(() => { try { localStorage.clear(); localStorage.setItem("sp-help-seen", "true"); } catch {} });
    await page.goto(BASE + "#app");   // the site lands over the editor; the checks are the editor's
    await page.waitForFunction(() => /ready|Error/.test(document.getElementById("status-engine").textContent), null, { timeout: 60000 });
    // the runtime says its version as it loads, in the Logs pane (the status line only says it is ready)
    const runtimeSaid = await page.evaluate(() => [...document.querySelectorAll("#logs-list, #pane-logs")].map((n) => n.textContent).join(" "));
    const versions = await page.locator("#info-versions").textContent();   // the corner says which Sonic Pi this is
    check("the runtime loads in the page", /mruby runtime \S+ loaded/.test(runtimeSaid) || /Sonic Pi v\d/.test(versions), versions);
    const setCode = (code) => page.evaluate((c) => window.sonicPi.editor.setCode(c), code);
    // as native: Help opens the help pane, and its rail picks cards, docs or threads
    const openPane = async (name) => {
      if (!(await page.evaluate(() => document.body.dataset.drawer))) await page.click("#btn-help");
      if ((await page.evaluate(() => document.body.dataset.drawer)) !== name) await page.click(`#drawer-rail button[data-drawer='${name}']`);   // a pane already open stays so (its rail button would shut it)
    };
    const caretToEnd = () => page.evaluate(() => { const v = window.sonicPi.editor.view; v.focus(); v.dispatch({ selection: { anchor: v.state.doc.length } }); });

    // highlighting: native's lexer keys. A cleared buffer has none of them in it, so the code is put there first
    await setCode("live_loop :beat do    # a comment\n  sample :bd_haus, amp: 2\n  sleep 1\nend");
    await page.waitForSelector(".cm-content .sp-keyword", { timeout: 5000 });
    const colours = await page.evaluate(() => {
      const get = (sel) => getComputedStyle(document.querySelector(sel)).color;
      return { keyword: get(".cm-content .sp-keyword"), symbol: get(".cm-content .sp-symbol"), number: get(".cm-content .sp-number"), comment: get(".cm-content .sp-comment") };
    });
    check("the editor highlights with the theme's lexer colours", new Set(Object.values(colours)).size === 4, JSON.stringify(colours));

    // autocomplete, as native offers it
    await setCode("");
    await caretToEnd();
    await page.keyboard.type("sample ");
    await page.waitForSelector(".cm-tooltip-autocomplete li", { timeout: 5000 }).catch(() => {});
    const sampleRows = await page.locator(".cm-tooltip-autocomplete li").allTextContents();
    check("after sample the popup offers samples", sampleRows.some((t) => t.startsWith(":bd_haus")), `${sampleRows.length} rows`);
    await page.keyboard.type("amb_dr");
    await page.waitForTimeout(200);
    const fuzzy = await page.locator(".cm-tooltip-autocomplete li").first().textContent();
    check("the popup ranks fuzzily, as native does", fuzzy.startsWith(":ambi_drone"), fuzzy);
    await page.keyboard.press("Tab");
    await page.keyboard.type(", ");
    await page.waitForTimeout(200);
    const optRows = await page.locator(".cm-tooltip-autocomplete li").allTextContents();
    check("after a sample and a comma the popup offers sample opts", optRows.some((t) => t.startsWith("rate:")), `${optRows.length} rows`);
    await page.keyboard.type("amp");
    await page.waitForFunction(() => /^amp:/.test(document.querySelector(".cm-tooltip-autocomplete li")?.textContent ?? ""), null, { timeout: 3000 }).catch(() => {});
    await page.keyboard.press("Tab");
    await page.waitForSelector(".cm-tooltip-autocomplete .sp-opt-range", { timeout: 3000 }).catch(() => {});
    await page.waitForTimeout(200);
    const range = await page.locator(".cm-tooltip-autocomplete li").first().textContent().catch(() => "");
    const rangeCode = await page.evaluate(() => window.sonicPi.editor.getCode());
    check("a ranged opt offers its default", /^\d/.test(range), `${range} · buffer: ${JSON.stringify(rangeCode)}`);
    await page.keyboard.press("Tab");
    await page.waitForSelector(".sp-slider", { timeout: 3000 }).catch(() => {});
    const slider = page.locator(".sp-slider input");
    const hasSlider = await slider.count();
    let slid = "";
    if (hasSlider) {
      await slider.evaluate((i) => { i.value = i.max; i.dispatchEvent(new Event("input", { bubbles: true })); });
      slid = await page.evaluate(() => window.sonicPi.editor.getCode());
    }
    check("the slider over a ranged value edits the buffer", hasSlider && /amp: 4\b/.test(slid), slid);
    // Escape and Ctrl-G put it away, and it stays away while the caret stays on that value
    await page.evaluate(() => window.sonicPi.editor.view.focus());
    await page.keyboard.press("Escape");
    await page.waitForTimeout(150);
    const gone = await page.evaluate(() => !document.querySelector(".sp-slider"));
    await page.keyboard.press("ArrowLeft");
    await page.waitForTimeout(150);
    const stays = await page.evaluate(() => !document.querySelector(".sp-slider"));
    check("Escape puts the value's slider away, and it stays away on that value", hasSlider && gone && stays, `${gone} ${stays}`);
    await setCode("use_synth :prophet\nplay 60, ");
    await caretToEnd();
    await page.keyboard.type("res");
    await page.waitForFunction(() => [...document.querySelectorAll(".cm-tooltip-autocomplete li")].some((li) => li.textContent.startsWith("res:")), null, { timeout: 3000 }).catch(() => {});
    const synthOpts = await page.locator(".cm-tooltip-autocomplete li").allTextContents();
    check("play offers the current synth's opts", synthOpts.some((t) => t.startsWith("res:")), `${synthOpts.length} rows`);
    // a screen reader, as native: the list is not in the accessibility tree (the code keeps the reader), and each
    // move says the row, its kind, summary and place, and nothing at the list's ends
    await page.evaluate(() => { window.__said = []; new MutationObserver((ms) => ms.forEach((m) => m.addedNodes.forEach((n) => { if (n.parentElement?.closest?.("[aria-live=assertive]")) window.__said.push(n.textContent); }))).observe(document.body, { childList: true, subtree: true }); });
    await page.keyboard.press("ArrowUp");
    await page.keyboard.press("ArrowDown");
    await page.waitForTimeout(150);
    const speech = await page.evaluate(() => ({ said: window.__said, hidden: document.querySelector(".cm-tooltip-autocomplete")?.getAttribute("aria-hidden"), combobox: document.querySelector("#editor-mount .cm-content").hasAttribute("aria-activedescendant") }));
    check("the completion list speaks each move (name, kind, summary, place), silent at its ends, out of the accessibility tree",
      speech.said.length === 1 && / 2 of \d+$/.test(speech.said[0]) && speech.said[0].split(", ").length >= 3 && speech.hidden === "true" && !speech.combobox, JSON.stringify(speech));
    await page.keyboard.press("Escape");
    await setCode("");
    await caretToEnd();
    await page.keyboard.type("play ");
    // a list that is all notes gets native's NotePiano: one keyboard under the rows, in the popup itself, not in a row's detail pane
    await page.waitForSelector(".cm-tooltip-autocomplete .sp-cpiano .sp-piano", { timeout: 3000 }).catch(() => {});
    check("play offers notes with a piano", (await page.locator(".cm-tooltip-autocomplete .sp-cpiano .sp-piano").count()) === 1);
    await page.keyboard.press("Escape");

    // Run, live loops, redefinition, errors, Stop
    await setCode("live_loop :kick do\n  sample :bd_haus\n  sleep 0.5\nend\nlive_loop :hat do\n  sample :drum_cymbal_closed\n  sleep 0.25\nend");
    await page.click("#btn-run");
    await page.waitForFunction(() => /:kick/.test(document.getElementById("status-jobs").title), null, { timeout: 90000 }).catch(() => {});
    await page.waitForTimeout(1200);
    const jobs = await page.locator("#status-jobs").getAttribute("title");
    check("Run plays both live loops", /:kick/.test(jobs) && /:hat/.test(jobs), jobs);
    const runs = await page.locator("#status-jobs").textContent();
    check("the status bar says the runs going as their ids alone", /^\[\d+(, \d+)*\]$/.test(runs), runs);
    check("the log shows what played", (await page.locator("#log .log-line").count()) > 3);
    const audio = await page.evaluate(() => ({ sent: window.sonicPi.session.bridge.sent, failures: window.sonicPi.session.failures }));
    check("sounds reach the engine as the runtime's own OSC, and scsynth refuses none", audio.sent > 3 && audio.failures === 0, JSON.stringify(audio));
    await setCode("live_loop :kick do\n  sample :bd_haus\n  puts :redefined\n  sleep 0.5\nend");
    await page.click("#btn-run");
    await page.waitForFunction(() => /redefined/.test(document.getElementById("log").textContent), null, { timeout: 10000 }).catch(() => {});
    const jobs2 = await page.locator("#status-jobs").getAttribute("title");
    check("a second Run redefines a live loop in place", (await page.locator("#log").textContent()).includes("redefined") && /:hat/.test(jobs2), jobs2);
    // The synths are this repo's build of each one (web/sonic_pi.js synthdefBaseURL), not the set SuperSonic bundles:
    // those drift, and a stale one plays differently or not at all. The autotuner is what this listens to — the
    // engine's copy predates the fix for the pitch tracker's first reading, and makes no sound at all — so a sour saw
    // through it must come out audible, and in tune.
    await setCode("live_loop :sour do\n  with_fx :autotuner do\n    synth :saw, note: 60.4, sustain: 2\n  end\n  sleep 2\nend");
    await page.click("#btn-run");
    await page.evaluate(() => {   // the tap first, so it has a window of sound to report on
      const e = window.sonicPi.engine;
      window.__peakTap = Object.assign(e.audioContext.createAnalyser(), { fftSize: 16384, smoothingTimeConstant: 0 });
      e.node.connect(window.__peakTap);
    });
    const tuned = await page.waitForFunction(() => {
      const buf = new Float32Array(16384);
      window.__peakTap.getFloatTimeDomainData(buf);
      let peak = 0;
      for (const v of buf) peak = Math.max(peak, Math.abs(v));
      return peak > 0.02 ? peak : false;
    }, null, { timeout: 15000 }).then((h) => h.jsonValue()).catch(() => 0)
      .then((peak) => ({ peak: +Number(peak).toFixed(3), defs: defURLs.length, from: defURLs[0] ?? "" }));
    tuned.own = defURLs.length > 0 && defURLs.every((u) => u.startsWith(new URL("synthdefs/", BASE).pathname));
    check("an FX plays this repo's build of its synthdef, not the engine's bundled copy", tuned.own && tuned.peak > 0.02, JSON.stringify(tuned));
    await page.click("#btn-stop");
    await setCode("live_loop :broken do\n  play [60, 64].tock\n  sleep 0.25\nend");
    await page.click("#btn-run");
    await page.waitForSelector("#error-pane:not([hidden])", { timeout: 10000 }).catch(() => {});
    const errors = await page.locator("#error-pane").textContent();
    check("an error in a thread shows in the error pane with its line", /NoMethodError/.test(errors) && /line 2/.test(errors), errors.trim());
    check("the error's line is marked in the editor", (await page.locator(".cm-sp-error-line").count()) === 1);
    await page.click("#btn-stop");
    await page.waitForTimeout(800);
    check("Stop stops everything", (await page.locator("#status-jobs").textContent()) === "");

    // an alias plays another synth's synthdef (:sine is sonic-pi-beep): what is loaded ahead of a run is what the
    // runtime says each name plays, not a name made from the symbol, which would be a 404 and an error card
    defURLs.length = 0;
    await setCode("use_synth :sine\nplay 60, release: 0.1\nuse_synth :mod_beep\nplay 62, release: 0.1");
    await page.click("#btn-run");
    await page.waitForTimeout(1500);
    await page.click("#btn-stop");
    const aliasDefs = defURLs.map((u) => u.split("/").pop());
    check("a synth alias loads the synthdef it plays (:sine → sonic-pi-beep, :mod_beep → sonic-pi-mod_sine), and no made-up one",
      aliasDefs.includes("sonic-pi-mod_sine.scsyndef") && !aliasDefs.some((f) => /sonic-pi-(sine|mod_beep)\.scsyndef/.test(f)), aliasDefs.join(" "));
    // the browser pausing the sound (a call, another app, the tab away): a card asks for the one tap that may start it
    // again, and that tap is the recovery — no other tap or key on the page doubles as a resume
    await page.evaluate(() => window.sonicPi.engine.audioContext.suspend());
    const asked = await page.waitForSelector("#resume-overlay:not([hidden])", { timeout: 4000 }).then(() => true).catch(() => false);
    await page.keyboard.press("Shift");   // a key on the page is only a key
    const stillAsked = await page.evaluate(() => !document.getElementById("resume-overlay").hidden);
    if (asked) await page.click("#resume-go");
    const resumed = await page.waitForFunction(() => document.getElementById("resume-overlay").hidden && window.sonicPi.engine.audioContext.state === "running", null, { timeout: 15000 }).then(() => true).catch(() => false);
    check("sound paused by the browser: a card asks for the tap, a stray key does not resume, and its button brings the sound back", asked && stillAsked && resumed, JSON.stringify({ asked, stillAsked, resumed }));
    // live lines, the threads view, and more of the runtime's verbs
    await setCode("live_loop :kick do\n  sample :bd_haus\n  sleep 0.5\nend\nlive_loop :bass do\n  use_synth :tb303; use_sched_ahead_time 1\n  play rrand_i(40, 52), release: 0.2\n  sleep 0.25\nend");   // the bass a second ahead: notes on the roll still to sound when Stop is pressed (the default lead is too short to be sure of one)
    await page.click("#btn-run");
    const flashed = await page.waitForSelector(".cm-line.sp-flash-a, .cm-line.sp-flash-b", { timeout: 10000 }).then(() => true).catch(() => false);
    check("a sounding line flashes, as native's code flash", flashed);
    // Debug, as native's: the engine's metrics, and under them what went to SuperSonic (the runtime's bundles too) and
    // what came back
    await openPane("debug");
    // a synth not heard yet: its synthdef loads, and the engine's answer to that comes back
    await setCode("use_synth :zawa\nplay 60, release: 0.1");
    await page.click("#btn-run");
    await page.waitForFunction(() => { const [to, from] = document.querySelectorAll("#debug-pane .logs-source"); return to?.textContent.includes("/s_new") && from?.querySelector(".logs-line"); }, null, { timeout: 8000 }).catch(() => {});
    const osc = await page.evaluate(() => [...document.querySelectorAll("#debug-pane .logs-source")].map((s) => ({ title: s.querySelector(".logs-title").textContent, lines: s.querySelectorAll(".logs-line").length, s_new: /\+\d+\.\d+s \/s_new "sonic-pi-/.test(s.textContent) })));
    check("the Debug pane logs the OSC to SuperSonic (the runtime's bundles, when each is due) and from it", osc.length === 2 && osc[0].s_new && osc[1].lines > 0, JSON.stringify(osc));
    await openPane("insight");
    await page.waitForTimeout(1500);
    const tree = await page.evaluate(() => window.sonicPi.processTree());
    const labelOf = (uid) => tree.find((n) => n.uid === uid)?.label;
    // threads stopped by the last Stop linger a moment, fading: look at the live ones
    const loops = tree.filter((n) => /^live_loop :(kick|bass)$/.test(n.label) && n.state <= 2);
    check("the process tree hangs each live loop from its run's main thread", loops.length === 2 && loops.every((n) => labelOf(n.parent) === "main" && /^run \d+$/.test(labelOf(tree.find((m) => m.uid === n.parent)?.parent))), tree.map((n) => `${n.label}<${labelOf(n.parent) ?? "-"}`).join(" "));
    check("the process tree shows sleeping loops", loops.every((n) => n.state === 1 || n.state === 0), loops.map((n) => n.state).join(","));
    // a node the program has just made is in the tree before the next layout has placed it, and a snapshot taken
    // in between catches it without coordinates: the wait is for the drawing to catch up, not for the check to pass
    const drawn = await page.waitForFunction(() => {
      const t = window.sonicPi.processTree();
      return t.length >= 4 && t.every((n) => n.x != null) ? t.length : false;
    }, null, { timeout: 5000 }).then((h) => h.jsonValue()).catch(() => null);
    check("the process tree draws its nodes", drawn != null, drawn != null ? `${drawn} nodes` : "a node was left unplaced");
    // a loop held on a sync says so in its scope
    await setCode("live_loop :kick do\n  sample :bd_haus\n  sleep 0.5\nend\nlive_loop :held do\n  sync :never_cued\n  play 60\nend");
    await page.click("#btn-run");
    const held = await page.waitForSelector(".sp-wait", { timeout: 10000 }).then((h) => h.evaluate((el) => `${el.title} @ line ${[...el.closest(".cm-content").querySelectorAll(".cm-line")].indexOf(el.closest(".cm-line")) + 1}`)).catch(() => null);
    check("a thread held on a sync is marked at its sync line", held != null && /waiting on sync :never_cued @ line 6$/.test(held), String(held));
    await page.click(".insight-views button[data-view='timeline']");
    await page.waitForTimeout(800);
    const threadRows = await page.locator(".insight-table tbody tr").allTextContents();
    check("the Threads view has a row per thread with its state", threadRows.some((t) => t.includes(":kick") && /sleeping/.test(t)) && threadRows.some((t) => t.includes(":bass")), threadRows.slice(0, 3).join(" | "));
    const painted = await page.evaluate(() => {
      const c = document.querySelector(".insight-canvas canvas");
      const d = c.getContext("2d").getImageData(0, 0, c.width, c.height).data;
      const seen = new Set();
      for (let i = 0; i < d.length; i += 97 * 4) seen.add(`${d[i]},${d[i + 1]},${d[i + 2]}`);
      return seen.size;
    });
    check("the Threads timeline draws", painted > 3, `${painted} colours`);
    // the piano roll: paused, so what it drew holds still to be read and pointed at
    await page.click(".insight-views button[data-view='roll']");
    await page.waitForTimeout(2000);
    // Pause is the head's own button, beside the view picker, and holds every view — the tree, the timeline and the roll
    const pause = page.locator(".insight-pause");
    await pause.click();
    await page.waitForTimeout(150);
    const roll = await page.evaluate(() => window.sonicPi.pianoRoll());
    const bass = roll.notes.filter((n) => n.label === ":bass" && n.start <= n.end);
    const sorted = [...bass].sort((a, b) => a.note - b.note);
    check("the piano roll draws the bass line's notes on their rows, higher notes higher", bass.length >= 3 && bass.every((n) => n.note >= 40 && n.note <= 52 && n.x1 > n.x0) && roll.range[0] <= 40 && roll.range[1] >= 52 && sorted.every((n, i) => i === 0 || n.note === sorted[i - 1].note || n.y < sorted[i - 1].y), `${bass.length} notes, rows ${roll.range}`);
    check("a note lasts the release it was given", bass.every((n) => Math.abs(n.end - n.start - 0.2) < 1e-6), bass.map((n) => (n.end - n.start).toFixed(3)).slice(0, 4).join(","));
    check("the piano roll gives samples a drum row", roll.rows.includes("bd_haus") && roll.hits.some((h) => h.sample === "bd_haus" && h.label === ":kick"), roll.rows.join(","));
    check("the piano roll's grid follows a thread's beats", roll.bpm === 60, String(roll.bpm));
    const target = bass.find((n) => n.x1 - n.x0 >= 4 && n.x0 > 60);
    const box = await page.locator(".roll-canvas canvas").boundingBox();
    if (target) await page.mouse.move(box.x + (target.x0 + target.x1) / 2, box.y + target.y);
    await page.waitForTimeout(100);
    const tipText = (await page.locator(".roll-tip:not([hidden])").textContent().catch(() => "")) ?? "";
    check("pointing at a note names it, its synth and its line", /^[A-G][♯♭]?\d \(\d+\) · :tb303 · amp 1 · 0\.2 s · :bass · line 7$/.test(tipText), tipText);
    await page.mouse.move(0, 0);
    await page.locator(".insight-pause", { hasText: "Resume" }).click();
    await page.waitForTimeout(300);
    const ahead = await page.evaluate(() => { const now = window.sonicPi.session.clockNow(); return window.sonicPi.pianoRoll().notes.filter((n) => n.start > now).length; });
    const stoppedAt = await page.evaluate(() => { document.getElementById("btn-stop").click(); return window.sonicPi.session.clockNow(); });
    await page.waitForTimeout(300);
    const rollStopped = await page.evaluate(() => window.sonicPi.pianoRoll());
    check("Stop takes the notes that will now never sound off the piano roll", ahead > 0 && rollStopped.notes.every((n) => n.start <= stoppedAt && n.end <= stoppedAt + 1e-6) && rollStopped.hits.every((h) => h.time <= stoppedAt), `${ahead} ahead before, ${rollStopped.notes.filter((n) => n.start > stoppedAt).length} after the stop`);
    await setCode("n = play 60, release: 2\nsleep 0.25\ncontrol n, note: 72\nset :answer, 42\nputs get(:answer)\nputs spread(3, 8)\nmidi_note_on 60\nputs chord_invert(chord(:c4, :major), 1)");
    await page.click("#btn-run");
    await page.waitForFunction(() => /\(ring 64, 67, 72\)/.test(document.getElementById("log").textContent), null, { timeout: 10000 }).catch(() => {});
    const newVerbs = await page.locator("#log").textContent();
    check("control, set and get, spread, MIDI and chord_invert run", /control node/.test(newVerbs) && /42/.test(newVerbs) && /\(ring true, false, false, true/.test(newVerbs) && /\(ring 64, 67, 72\)/.test(newVerbs), newVerbs.slice(-200));
    check("controls and samples all session long: scsynth has refused nothing", (await page.evaluate(() => window.sonicPi.session.failures)) === 0);
    const bent = (await page.evaluate(() => window.sonicPi.pianoRoll())).notes.filter((n) => n.notes[0] === 60);
    check("the piano roll bends a controlled note to its new pitch", bent.some((n) => n.notes.includes(72)), JSON.stringify(bent.map((n) => n.notes)));
    // and a refusal is seen: control a synth that has already ended
    const refusedBefore = await page.evaluate(() => window.sonicPi.session.failures);
    await setCode("n = play 60, release: 0.05\nsleep 0.5\ncontrol n, note: 72");
    await page.click("#btn-run");
    await page.waitForFunction((b) => window.sonicPi.session.failures > b, refusedBefore, { timeout: 5000 }).catch(() => {});
    check("a message scsynth refuses is counted", (await page.evaluate(() => window.sonicPi.session.failures)) > refusedBefore);
    await setCode("load_example :haunted");
    await page.click("#btn-run");
    await page.waitForFunction(() => window.sonicPi.editor.getCode().startsWith("# Haunted"), null, { timeout: 5000 }).catch(() => {});
    check("load_example puts the example in the buffer", (await page.evaluate(() => window.sonicPi.editor.getCode())).startsWith("# Haunted"));
    await page.click("#btn-stop");

    // a user's synth with its metadata (app/test/fixtures/whoosh.json beside its .scsyndef): a standard synth from its
    // first note — played with external synths off, its opts checked, listed in the docs, its page's live synth its own
    // the fixture is the tree's, so only the dev server has it: a deployed site is checked without it
    const whoosh = new URL("../app/test/fixtures/whoosh.scsyndef", BASE).href;
    if (!(await page.evaluate((u) => fetch(u, { method: "HEAD" }).then((r) => r.ok, () => false), whoosh))) skip("a user's synth with its metadata is a standard synth", "no test fixtures at " + whoosh);
    else {
      await setCode(`load_synthdef "${whoosh}"\nsynth :whoosh, note: 64, release: 0.2`);
      await page.click("#btn-run");
      await page.waitForFunction(() => /synth :whoosh/.test(document.getElementById("log").textContent), null, { timeout: 15000 }).catch(() => {});
      const played = await page.evaluate(() => ({ log: /synth :whoosh, \{note: 64\.0, release: 0\.2\}/.test(document.getElementById("log").textContent), err: !document.getElementById("error-pane").hidden }));
      await setCode("synth :whoosh, release: -1");
      await page.click("#btn-run");
      await page.waitForSelector("#error-pane:not([hidden])", { timeout: 5000 }).catch(() => {});
      const refused = /release/.test(await page.locator("#error-pane").textContent());
      await page.click("#btn-stop");
      await openPane("docs");
      await page.click(".docs-tabs button:has-text('Synths')");
      await page.waitForSelector("#docs-pane .docs-item[data-key='whoosh']", { timeout: 5000 }).catch(() => {});
      const listed = await page.evaluate(() => ({ group: document.querySelector("#docs-pane .docs-list > .docs-group")?.textContent, whoosh: !!document.querySelector("#docs-pane .docs-item[data-key='whoosh']") }));
      if (listed.whoosh) await page.click("#docs-pane .docs-item[data-key='whoosh']");
      const spare = await page.evaluate(() => [...document.querySelectorAll("#docs-pane .dial.pg-spare")].map((d) => d.getAttribute("aria-label")).join(","));
      check("a user's synth with its metadata is a standard synth: played, checked, in the docs, its Basic knobs its own",
        played.log && !played.err && refused && listed.group === "Your synths" && listed.whoosh && spare === "attack,sustain_level", JSON.stringify({ played, refused, listed, spare }));
    }

    // docs: an instrument page plays its program
    await openPane("docs");
    await page.click(".docs-tabs button:has-text('Synths')");
    await page.click(".docs-item[data-key='prophet']");
    const dials = await page.locator("#docs-pane .pg-face .dial").count();
    check("a synth page has dials for its opts", dials >= 8, `${dials} dials`);
    const cutoff = page.locator("#docs-pane .dial", { hasText: "cutoff" });
    await cutoff.focus();
    await page.keyboard.press("ArrowUp");
    const program = await page.locator("#docs-pane .pg-card .qs-card-body").innerText();   // the card's block: a line an element, so its text by line
    check("turning a dial writes the program", /use_synth :prophet\nplay \d+, cutoff: \d+/.test(program), program);
    // the page's demo runs on the quickstart card's transport (ui/card.js): Play is its first button, Stop the second
    await page.click("#docs-pane .pg-card .qs-card-foot .qs-run:not(.qs-stop)");
    await page.waitForFunction(() => /synth :prophet, \{note: 52\.0, cutoff: 115/.test(document.getElementById("log").textContent), null, { timeout: 5000 }).catch(() => {});
    check("the instrument page plays its program", /synth :prophet, \{note: 52\.0, cutoff: 115/.test(await page.locator("#log").textContent()));
    // an FX's live demo keeps its FX node in Time State for the knobs to steer (set :docs_fx, fx): a node is thread safe,
    // as native has it, so the demo runs with no error, and one that fails says so on its own card, not over the code
    await page.click(".docs-tabs button:has-text('FX')");
    await page.click("#docs-pane .docs-item[data-key='autotuner']");
    await page.locator("#docs-pane .pg-card .qs-card-foot .qs-transport button").first().click();
    await page.waitForTimeout(3000);
    const fxDemo = await page.evaluate(() => ({ pane: /thread safe|docs_fx/.test(document.getElementById("error-pane").hidden ? "" : document.getElementById("error-pane").textContent), card: document.querySelector("#docs-pane .pg-card.errored .qs-state")?.textContent ?? null }));   // the pane may hold an earlier check's error: only this demo's counts
    // and its loop's scope is fed while it plays: the canvas is placed from the records, so one drawn but starved
    // (the card not given scopeFrame) still looks like a scope — it is a flat line under a synth plainly playing
    const loopLit = await page.evaluate(() => {
      const c = document.querySelector("#docs-pane .pg-card .sp-loop-scope");
      if (!c) return null;
      const d = c.getContext("2d").getImageData(0, 0, c.width, c.height).data;
      let lit = 0;
      for (let i = 3; i < d.length; i += 4) if (d[i] > 8) lit++;
      return { w: c.width, lit };
    });
    await page.locator("#docs-pane .pg-card .qs-card-foot .qs-transport button").nth(1).click();
    check("an FX page's live demo runs, its FX node kept in Time State for its knobs", !fxDemo.pane && !fxDemo.card, JSON.stringify(fxDemo));
    check("the demo's live loop draws its scope, not a flat line", loopLit != null && loopLit.lit > loopLit.w * 1.4, JSON.stringify(loopLit));
    // the tutorial is the site's, a page a chapter (scripts/build-site.mjs): an old link into the docs pane's tutorial
    // lands on its chapter's page at that part, its programs runnable cards, the bar's Tutorial lit, the next chapter a click on.
    // The site's pages are built from site/ (scripts/build-site.mjs), not kept in the repository: where they have not
    // been built the site's checks have nothing to look at, and saying so is better than failing
    const siteBuilt = await page.evaluate(async () => (await fetch("learn.html").catch(() => ({ ok: false }))).ok);
    await page.evaluate(() => { location.hash = "#docs/tutorial/02.1-Your-First-Beeps"; });
    await page.waitForFunction(() => location.pathname.endsWith("tutorial-02.html") && document.querySelector(".site-body:not([hidden]) .tut-page .qs-card .qs-transport"), null, { timeout: 30000 }).catch(() => {});
    const tut = await page.evaluate(() => { const b = document.querySelector(".site-body:not([hidden]) .tut-page"); return { at: location.pathname.split("/").pop() + location.hash, cards: b?.querySelectorAll(".qs-card .qs-transport").length ?? 0, title: document.getElementById("tut-02-1-your-first-beeps")?.querySelector("h1")?.textContent, book: document.querySelector('#site-nav .ic-tab[data-tab="tutorial"]')?.classList.contains("active"), inHelp: [...document.querySelectorAll("#docs-pane .docs-tabs button")].some((t) => /tutorial/i.test(t.textContent)), next: b?.querySelector(".tut-step.next")?.textContent }; });
    check("the tutorial is the site's, a page a chapter: an old link lands at its part, runnable cards, the Tutorial tab lit, the next chapter on, and none in the help pane", tut.at === "tutorial-02.html#tut-02-1-your-first-beeps" && tut.cards > 0 && tut.title === "2.1 Your First Beeps" && tut.book && !tut.inHelp && /3 Samples/.test(tut.next ?? ""), JSON.stringify(tut));
    await page.evaluate(() => { location.hash = "#app"; });
    await page.waitForFunction(() => document.getElementById("info-card").hidden, null, { timeout: 5000 }).catch(() => {});

    // quickstart cards
    await openPane("quickstart");
    await page.waitForSelector("#quickstart-pane .qs-card", { timeout: 5000 });
    // a deck opens on its intro card, its words and Start (quickstart.js introCard): Start deals the first card
    const intro = await page.evaluate(() => ({ words: document.querySelector("#quickstart-pane .qs-intro .qs-desc-lead")?.textContent ?? "", start: !!document.querySelector("#quickstart-pane .qs-start") }));
    // from the keyboard, as a screen reader user would: focus goes to the card dealt, not back to the page
    // (the pane settles first: opening it can hand focus back to the editor a moment later)
    await page.waitForTimeout(400);
    if (intro.start) await page.locator("#quickstart-pane .qs-start").press("Enter");
    await page.waitForSelector("#quickstart-pane .qs-card .qs-run", { timeout: 5000 }).catch(() => {});
    check("a deck opens on its intro card, its words and Start, which deals the first card", intro.words.length > 0 && intro.start && (await page.locator("#quickstart-pane .qs-card .qs-run").count()) > 0, JSON.stringify(intro));
    const dealtFocus = await page.evaluate(() => { const a = document.activeElement, id = a?.getAttribute("aria-labelledby"); return { group: a?.getAttribute("role"), name: id ? document.getElementById(id)?.textContent : null, first: document.querySelector("#quickstart-pane .qs-card:not(.qs-intro) .qs-card-title")?.textContent }; });
    check("Start keeps focus in the cards: on the card it dealt, a group named by its title", dealtFocus.group === "group" && dealtFocus.name && dealtFocus.name === dealtFocus.first, JSON.stringify(dealtFocus));
    await page.locator("#quickstart-pane .qs-card .qs-run:not(.qs-stop)").first().click();
    // through the boot if this is the first sound, then the card's own run, which is short. Play and Stop are a pair of
    // icon buttons (ui/card.js createTransport): the transport wears "playing" and Stop comes live while it sounds
    const playing = () => page.evaluate(() => {
      const t = document.querySelector("#quickstart-pane .qs-card .qs-transport");
      return `${t.classList.contains("playing")}/${!t.querySelector(".qs-stop").disabled}`;
    });
    const isPlaying = (want) => page.waitForFunction((w) => document.querySelector("#quickstart-pane .qs-card .qs-transport")?.classList.contains("playing") === w, want, { timeout: want ? 30000 : 10000 }).catch(() => {});
    const cardStates = [];
    await isPlaying(true);
    cardStates.push(await playing());
    await isPlaying(false);
    cardStates.push(await playing());
    check("a quickstart card plays and returns to Play when done", cardStates[0] === "true/true" && cardStates[1] === "false/false", cardStates.join(" → "));
    await page.locator("#quickstart-pane .qs-card .qs-card-insert").first().click();
    check("a card inserts at the cursor", (await page.evaluate(() => window.sonicPi.editor.getCode())).includes("play 60"));
    // a card's live loops have their scopes, as the editor's do (ui/card.js cardLoopScopes): the Live Loops deck's first
    // card, played, draws its loop's sound on its live_loop line. A page of its own, the deck reached (its tab shows
    // only then), as a learner coming back to it has it
    await page.evaluate(() => { localStorage.setItem("sp-quickstart-deck", "3"); localStorage.setItem("sp-quickstart-reached", "3"); localStorage.setItem("sp-quickstart-shown", JSON.stringify({ 3: 1 })); });
    const qp = await page.context().newPage();
    await qp.goto(BASE + "#app");
    await qp.waitForFunction(() => /ready|Error/.test(document.getElementById("status-engine")?.textContent ?? ""), null, { timeout: 60000 });
    if (!(await qp.evaluate(() => document.body.dataset.drawer))) await qp.click("#btn-help");
    if ((await qp.evaluate(() => document.body.dataset.drawer)) !== "quickstart") await qp.click("#drawer-rail button[data-drawer='quickstart']");
    await qp.locator("#quickstart-pane .qs-start").click({ timeout: 5000 }).catch(() => {});
    const loopCard = qp.locator("#quickstart-pane .qs-card:not(.qs-intro)", { hasText: "live_loop" }).first();
    await loopCard.locator(".qs-transport button").first().click({ timeout: 5000 }).catch(() => {});
    const loopScope = await qp.waitForFunction(() => document.querySelector("#quickstart-pane .qs-card:not(.qs-intro) .sp-loop-scope[data-painted]")?.closest(".cm-line")?.textContent.trim() ?? false, null, { timeout: 15000 }).then((h) => h.jsonValue()).catch(() => null);
    await loopCard.locator(".qs-transport button").nth(1).click({ timeout: 2000 }).catch(() => {});
    await qp.close();
    check("a quickstart card's live loop has its scope on its line while it plays", /^live_loop :\w+ do/.test(loopScope ?? ""), String(loopScope));

    // themes: the bar's palette, not the preferences
    await page.click("#btn-prefs");
    await page.waitForSelector("#prefs-pane .pref-group", { timeout: 5000 });
    check("the preferences leave the colour theme to the bar's palette", await page.evaluate(() => ![...document.querySelectorAll("#prefs-pane h3")].some((h) => /theme/i.test(h.textContent)) && !document.querySelector("#prefs-pane .scheme-btn")));
    await page.click("#site-nav .sn-theme");
    await page.waitForSelector("#theme-menu:not([hidden]) .scheme-btn", { timeout: 5000 });
    // the theme follows the OS until a scheme is picked (theme.js), and headless is a light OS: the scheme to click is
    // any one painted in a Background that is not the one already on the page
    const before = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
    const other = await page.evaluate((bg) => [...document.querySelectorAll(".scheme-btn")].findIndex((b) => getComputedStyle(b).backgroundColor !== bg), before);
    if (other >= 0) await page.locator(".scheme-btn").nth(other).click();
    const after = await page.evaluate(() => getComputedStyle(document.body).backgroundColor);
    check("choosing a native scheme in the bar's palette repaints the page", other >= 0 && before !== after, `${before} → ${after}`);
    await page.keyboard.press("Escape");
    check("Escape puts the palette's menu away, the focus back on its button", await page.evaluate(() => document.getElementById("theme-menu").hidden && document.activeElement?.classList.contains("sn-theme")));

    // share
    await setCode("live_loop :shared do\n  play 72\n  sleep 1\nend");
    await page.context().grantPermissions(["clipboard-read", "clipboard-write"]).catch(() => {});
    await page.click("#btn-share");
    await page.waitForFunction(() => location.hash.startsWith("#code="), null, { timeout: 5000 }).catch(() => {});
    const url = await page.evaluate(() => location.href);
    const page2 = await browser.newPage(HTTPS);
    await page2.goto(url);
    await page2.waitForFunction(() => window.sonicPi?.editor.getCode().includes(":shared"), null, { timeout: 60000 }).catch(() => {});
    check("the share link restores the program", (await page2.evaluate(() => window.sonicPi?.editor.getCode() ?? "")).includes("live_loop :shared"));
    await page2.close();
    // the flight recorder: samples every clock, marks moments, reports
    const flight = await page.evaluate(() => {
      const f = window.sonicPi.flight;
      f.mark("heard", "check");
      const r = f.report();
      return { samples: r.samples.length, withEngine: r.samples.filter((s) => s.engine && !s.engine.error).length, withRuntime: r.samples.filter((s) => s.runtime && s.runtime.ticks > 0).length,
        heard: r.marks.some((m) => m.kind === "heard"), errors: r.marks.filter((m) => m.kind === "recorder-error").length, programs: r.programs.length, records: r.records.length,
        sounds: f.summary(0).sounds };
    });
    check("the flight recorder samples the engine, the runtime and the page", flight.samples > 50 && flight.withEngine > 10 && flight.withRuntime > 10 && flight.errors === 0 && flight.sounds > 0, JSON.stringify(flight));
    check("a flight report carries the marks, the programs and the records", flight.heard && flight.programs > 0 && flight.records > 0);
    // keyboard shortcuts: native's keymaps, pressed, and its shortcut editor
    const keymap = await page.evaluate(() => ({ platform: window.sonicPi.keys.platform, mode: window.sonicPi.keys.mode }));
    const meta = keymap.platform === "mac" ? "Meta" : "Alt";     // native's Meta: Cmd on a Mac, Alt elsewhere
    check("the keymap is native's own for the platform", keymap.mode === (keymap.platform === "mac" ? "mac" : "win"), JSON.stringify(keymap));
    await setCode("play 60");
    await caretToEnd();
    await page.keyboard.press(`${meta}+/`);
    check("native's Comment key comments the line", (await page.evaluate(() => window.sonicPi.editor.getCode())) === "# play 60");
    await page.evaluate(() => window.sonicPi.keys.setMode("emacs"));
    await setCode("hello world");
    await page.evaluate(() => { const v = window.sonicPi.editor.view; v.focus(); v.dispatch({ selection: { anchor: 3 } }); });
    await page.keyboard.press("Control+e");
    check("Emacs Live's Ctrl+E goes to the end of the line", (await page.evaluate(() => window.sonicPi.editor.view.state.selection.main.head)) === 11);
    await page.evaluate(() => window.sonicPi.shortcuts.open());
    await page.click(".sc-row[data-id='Run'] .sc-key");
    await page.click("[data-role='customise']");
    await page.keyboard.press(`${meta}+k`);
    await page.click(".sc-row[data-id='Stop'] .sc-key");
    await page.keyboard.press(`${meta}+k`);
    await page.waitForSelector(".sc-clash", { timeout: 3000 }).catch(() => {});
    const clashChoices = await page.locator(".sc-clash button").allTextContents();
    await page.click("[data-role='reassign']").catch(() => {});
    const custom = await page.evaluate(() => ({ mode: window.sonicPi.keys.mode, ...window.sonicPi.keys.custom }));
    check("the shortcut editor records a key, and a key in use offers Reassign, Keep both or Cancel",
      clashChoices.length === 3 && custom.mode === "custom" && custom.base === "emacs" && custom.keys.Stop === "Meta+K" && custom.keys.Run === "" && Object.keys(custom.keys).length === 2, JSON.stringify(custom));
    const [download] = await Promise.all([page.waitForEvent("download"), page.click(".sc-foot button:has-text('Export')")]);
    const ini = await (await import("node:fs/promises")).readFile(await download.path(), "utf8");
    check("the shortcut editor exports native's .ini", ini === "[General]\nbase=emacs\nRun=\nStop=Meta+K\n", JSON.stringify(ini));
    await page.setInputFiles(".sc-foot input[type=file]", { name: "v5-keyboard-shortcuts.ini", mimeType: "text/plain", buffer: Buffer.from("[General]\nbase=win\nScope=CtrlShift+O\n") });
    await page.waitForFunction(() => window.sonicPi.keys.custom.base === "win", null, { timeout: 3000 }).catch(() => {});
    check("the shortcut editor imports native's .ini", (await page.evaluate(() => window.sonicPi.keys.custom.keys.Scope)) === "CtrlShift+O");
    await page.keyboard.press("Escape");
    check("Escape closes the shortcut editor", !(await page.evaluate(() => window.sonicPi.shortcuts.isOpen)));
    await page.evaluate(() => { window.sonicPi.keys.setCustom({}); window.sonicPi.keys.setMode(window.sonicPi.keys.platform === "mac" ? "mac" : "win"); });

    // WebKit hands the page its own "ResizeObserver loop completed with undelivered notifications" as an uncaught
    // error, where Chromium keeps it to the console: it is the layout taking a second pass to settle (the site card
    // opening, main.js placeInfo), not a throw of ours. It is not counted, but it is always said, so it cannot hide.
    const settling = pageErrors.filter((m) => /ResizeObserver loop/.test(m)), thrown = pageErrors.filter((m) => !/ResizeObserver loop/.test(m));
    check("no uncaught errors on the app page", thrown.length === 0, [...thrown.slice(0, 3), ...(settling.length ? [`${settling.length} ResizeObserver loop notices, not counted`] : [])].join(" | "));
    await page.close();

    // ── the spec browser ──
    const specs = await browser.newPage(HTTPS);
    const specErrors = [];
    specs.on("pageerror", (e) => specErrors.push(e.message));
    await specs.goto(new URL("specs.html", BASE).href);
    await specs.waitForFunction(() => /mruby|no in-browser/.test(document.getElementById("engineBadge").textContent), null, { timeout: 60000 });
    await specs.click("#evalAll");
    await specs.waitForFunction(() => /^All /.test(document.getElementById("status").textContent), null, { timeout: 300000 });
    const all = await specs.locator("#status").textContent();
    check("the spec browser evaluates every spec on mruby", /pass/.test(all), all);
    check("no uncaught errors on the spec page", specErrors.length === 0, specErrors.slice(0, 3).join(" | "));

    // ── the site: every page a whole document that reads without a script; with one, a page to page in the one
    // document, the address following (scripts/build-site.mjs, app/src/info.js) ──
    if (siteBuilt) {
      const PAGES = ["index.html", "examples.html", "learn.html", "support.html"];
      const bare = await browser.newContext({ ...HTTPS, javaScriptEnabled: false, viewport: { width: 1280, height: 900 } });
      const nb = await bare.newPage();
      const unread = [];
      for (const f of PAGES) {
        await nb.goto(BASE + f);
        const seen = await nb.evaluate(() => ({ title: document.title, sections: document.querySelectorAll(".ic-main article[id]").length, list: document.querySelectorAll(".ic-side a[href^='#']").length, text: document.querySelector("#info-body").innerText.length }));
        if (!seen.sections || !seen.list || seen.text < 500) unread.push(`${f} ${JSON.stringify(seen)}`);
      }
      check("every page of the site reads without a script: its sections, its list, its words", !unread.length, unread.join(" | ") || `${PAGES.length} pages`);
      await nb.goto(BASE + "learn.html#talks");
      await nb.waitForTimeout(1500);   // the page scrolls there smoothly (site.css .ic-main): the anchor where it settles
      const nativeAnchor = await nb.evaluate(() => { const t = document.getElementById("talks").getBoundingClientRect().top; return t >= -2 && t < innerHeight / 2; });
      check("without a script an anchor is the browser's own: learn.html#talks opens at Talks", nativeAnchor);
      await bare.close();

      // the home page's live synth, under Code. Music. Live.: the docs pane's instrument, its keys fitted to their room
      const home = await browser.newPage({ ...HTTPS, viewport: { width: 1280, height: 900 } });
      await home.addInitScript(() => { try { localStorage.setItem("sp-help-seen", "true"); } catch {} });
      await home.goto(BASE + "index.html#synths-to-code");
      await home.waitForSelector(".home-synth.live .sp-key", { timeout: 30000 }).catch(() => {});
      const synth = await home.evaluate(() => { const h = document.querySelector(".home-synth"), box = h?.querySelector(".pg-piano"); return { live: !!h?.classList.contains("live"), synth: /use_synth (:\w+)/.exec(h?.querySelector(".qs-card-body")?.innerText ?? "")?.[1], dials: h?.querySelectorAll(".dial").length ?? 0, fits: !!box && box.scrollWidth <= box.clientWidth + 1 }; });
      check("the home page has a live synth in its own section, Synths as Code: keys that fit, and its dials", synth.live && synth.synth === ":pluck" && synth.dials > 4 && synth.fits, JSON.stringify(synth));
      await home.close();

      const site = await browser.newPage({ ...HTTPS, viewport: { width: 1280, height: 900 } });
      const siteErrors = [];
      site.on("pageerror", (e) => siteErrors.push(e.message));
      let loads = 0;
      site.on("load", () => loads++);
      const where = () => site.evaluate(() => ({ at: location.pathname.split("/").pop() + location.hash, lit: document.querySelector(".site-body:not([hidden]) .ic-side .docs-item.active")?.dataset.key }));
      await site.goto(BASE + "learn.html#talks");
      await site.waitForFunction(() => document.querySelector(".site-body:not([hidden]) .ic-side .docs-item.active"), null, { timeout: 30000 });
      await site.waitForTimeout(1500);
      let w = await where();
      check("a page opened at an anchor stays there, its address kept and its list lit", w.at === "learn.html#talks" && w.lit === "talks", JSON.stringify(w));
      const loadsBefore = loads;
      await site.click('#site-nav .ic-tab[data-tab="examples"]');
      await site.waitForFunction(() => location.pathname.endsWith("examples.html") && document.querySelector(".site-body:not([hidden]) #sp-live-cards .qs-card"), null, { timeout: 30000 }).catch(() => {});
      w = await where();
      check("a tab goes to its page in the same document: the address the page's, nothing reloaded", w.at === "examples.html" && loads === loadsBefore, `${JSON.stringify(w)}, ${loads - loadsBefore} loads`);
      await site.goBack();
      await site.waitForTimeout(1500);
      w = await where();
      check("Back returns to the page before, at the section it was at", w.at === "learn.html#talks" && w.lit === "talks", JSON.stringify(w));
      // Code: the editor at its own address, in the same document, and Back the page again
      const loadsCode = loads;
      await site.click('#site-nav [data-tab="code"]');   // the code icon beside the palette
      await site.waitForFunction(() => location.pathname.endsWith("code.html"), null, { timeout: 5000 }).catch(() => {});
      const ed = await site.evaluate(() => ({ at: location.pathname.split("/").pop() + location.hash, title: document.title, lit: document.querySelector('#site-nav [aria-current="page"]')?.dataset.tab, card: !document.getElementById("info-card").hidden, inert: document.getElementById("main").inert }));
      check("Code is the editor at its own address (code.html), nothing reloaded, the Code tab lit", ed.at === "code.html" && ed.title === "Code · Sonic Pi" && ed.lit === "code" && !ed.card && !ed.inert && loads === loadsCode, `${JSON.stringify(ed)}, ${loads - loadsCode} loads`);
      await site.goBack();
      await site.waitForTimeout(1500);
      w = await where();
      check("Back from the editor is the page again, where it was", w.at === "learn.html#talks" && w.lit === "talks" && (await site.evaluate(() => !document.getElementById("info-card").hidden)), JSON.stringify(w));
      // the bar's words in order, Support among them before Code, and the palette last
      await site.click('#site-nav .ic-tab[data-tab="support"]');
      await site.waitForFunction(() => location.pathname.endsWith("support.html"), null, { timeout: 10000 }).catch(() => {});
      const sup = await site.evaluate(() => ({ at: location.pathname.split("/").pop(), lit: document.querySelector('#site-nav [aria-current="page"]')?.dataset.tab, order: [...document.querySelectorAll("#site-nav .sn-brand, #site-nav .ic-tab, #site-nav .sn-code, #site-nav .sn-theme")].filter((e) => e.offsetParent).map((e) => e.dataset.tab ?? (e.classList.contains("sn-brand") ? "brand" : "palette")).join(" ") }));
      check("the bar reads the wordmark (Home), Examples, Learn, Tutorial, Support, then the code icon and the palette; Support lit on its page", sup.at === "support.html" && sup.lit === "support" && sup.order === "brand examples learn tutorial support code palette", JSON.stringify(sup));
      await site.goBack();
      await site.waitForTimeout(1500);
      // a page gone back to by its tab opens at its top, wherever it was left: WebKit keeps a hidden column's scroll
      // (a smooth scroll to the top is dropped), and Safari paints nothing there
      await site.evaluate(() => document.getElementById("teachers").scrollIntoView({ behavior: "instant" }));
      await site.waitForTimeout(800);
      await site.click('#site-nav .ic-tab[data-tab="support"]');
      await site.waitForFunction(() => location.pathname.endsWith("support.html"), null, { timeout: 10000 }).catch(() => {});
      await site.click('#site-nav .ic-tab[data-tab="learn"]');
      await site.waitForFunction(() => location.pathname.endsWith("learn.html"), null, { timeout: 10000 }).catch(() => {});
      await site.waitForTimeout(1200);
      const top = await site.evaluate(() => ({ at: location.pathname.split("/").pop() + location.hash, top: document.querySelector(".site-body:not([hidden]) .ic-main").scrollTop }));
      check("a page gone back to by its tab opens at its top", top.at === "learn.html" && top.top === 0, JSON.stringify(top));
      await site.evaluate(() => document.getElementById("community").scrollIntoView());
      await site.waitForFunction(() => location.hash === "#community", null, { timeout: 5000 }).catch(() => {});
      w = await where();
      check("the address follows the section being read", w.at === "learn.html#community", JSON.stringify(w));
      const old = [];
      for (const [from, to] of [["#mac", "index.html#mac"], ["#schools", "learn.html#teachers"], ["#learn", "learn.html"], ["#tutorial", "tutorial.html"], ["#patreon", "support.html"], ["#app", "code.html"]]) {
        // each on a page of its own, left once it has loaded: WebKit calls a load cut short by the next one an
        // uncaught error of the page's (a module import cancelled), which is the check's doing, not the site's
        const pg = await browser.newPage({ ...HTTPS, viewport: { width: 1280, height: 900 } });
        pg.on("pageerror", (e) => siteErrors.push(e.message));
        await pg.goto(BASE + from);
        await pg.waitForFunction((to) => location.href.endsWith(to), to, { timeout: 15000 }).catch(() => {});
        await pg.waitForLoadState("networkidle").catch(() => {});
        const at = await pg.evaluate(() => location.pathname.split("/").pop() + location.hash);
        if (at !== to) old.push(`${from} → ${at}, not ${to}`);
        await pg.close();
      }
      check("the old one-page links (#mac, #schools, #learn, #tutorial, #patreon, #app) land where they live now", !old.length, old.join(" | "));
      // a screen reader's view of every page: axe-core's rules, all of them but the two that are the palette's and the
      // app's viewport (colour-contrast, meta-viewport: open questions of the design, not of the markup)
      const axe = (await import("node:fs")).readFileSync(new URL("../node_modules/axe-core/axe.min.js", import.meta.url), "utf8");
      const found = [];
      for (const f of PAGES) {
        await site.goto(BASE + f);
        await site.waitForFunction(() => document.querySelector(".site-body:not([hidden]) .qs-card") || !document.querySelector("pre.sp-card"), null, { timeout: 30000 }).catch(() => {});
        await site.waitForTimeout(800);
        await site.addScriptTag({ content: axe });
        const v = await site.evaluate(async () => (await window.axe.run(document, { rules: { "color-contrast": { enabled: false }, "meta-viewport": { enabled: false } } })).violations.map((x) => `${x.id} ×${x.nodes.length} (${x.nodes[0].target.join(" ").slice(0, 60)})`));
        if (v.length) found.push(`${f}: ${v.join(", ")}`);
      }
      check("every page of the site passes axe-core's accessibility rules", !found.length, found.join(" | ") || `${PAGES.length} pages`);
      check("no uncaught errors on the site's pages", siteErrors.length === 0, siteErrors.slice(0, 3).join(" | "));
      await site.close();
    }

    // ── a phone: the completion popup over the code keyboard (completion/cm.js positionInfo, keyboard.js) ──
    // ── a first visit's pointer to Help (main.js showHelpHint): shown with the editor, the Δ swelling out of itself, gone for good once
    // Help is opened ──
    {
      const first = await browser.newPage({ ...HTTPS, viewport: { width: 1280, height: 800 } });
      await first.goto(BASE + "#app");
      await first.waitForSelector("#help-hint:not([hidden]) .hh-note", { timeout: 20000 }).catch(() => {});
      const hint = await first.evaluate(() => {
        const el = document.getElementById("help-hint");
        if (!el || el.hidden) return null;
        const r = document.querySelector("#btn-help .tb-glyph").getBoundingClientRect(), e = getComputedStyle(document.getElementById("btn-help"), "::before"), c = document.getElementById("btn-help").getBoundingClientRect(), g = e.content === "none" ? { left: NaN, top: NaN, width: 0, height: 0 } : { left: c.left + c.width / 2 - parseFloat(e.width) / 2, top: c.top + c.height / 2 - parseFloat(e.height) / 2, width: parseFloat(e.width), height: parseFloat(e.height) };
        return { off: Math.hypot((g.left + g.width / 2) - (r.left + r.width / 2), (g.top + g.height / 2) - (r.top + r.height / 2)), says: el.querySelector(".hh-note").textContent };
      });
      check("a first visit points at Help: the Δ's echo centred on it and a note beneath it", hint && hint.off < 1 && /Toggle the documentation/.test(hint.says), JSON.stringify(hint));
      await first.click("#btn-help");
      await first.reload();
      await first.waitForFunction(() => window.sonicPi?.editor, null, { timeout: 20000 }).catch(() => {});
      check("once Help has been opened, the pointer to it is gone for good", await first.evaluate(() => !document.getElementById("help-hint")));
      await first.close();
    }

    // a game controller, as native's: its buttons and sticks as cues, a press and a release as their own edges. The
    // browser's Gamepad API stood in for by a pad the check presses (the engine's front polls getGamepads)
    {
      const ctx = await browser.newContext({ ...HTTPS, viewport: { width: 1280, height: 900 } });
      await ctx.addInitScript(() => {
        try { localStorage.setItem("sp-help-seen", "true"); } catch {}
        const pad = { id: "Test Pad (STANDARD GAMEPAD)", index: 0, connected: true, mapping: "standard", timestamp: 0, axes: [0, 0, 0, 0],
          buttons: Array.from({ length: 17 }, () => ({ pressed: false, touched: false, value: 0 })) };
        window.__pad = pad;
        Object.defineProperty(Navigator.prototype, "getGamepads", { configurable: true, value: () => [pad, null, null, null] });
      });
      const gp = await ctx.newPage();
      await gp.goto(BASE + "code.html");
      await gp.waitForFunction(() => window.sonicPi?.editor, null, { timeout: 30000 }).catch(() => {});
      await gp.evaluate(() => window.sonicPi.editor.setCode('live_loop :pad do\n  sync "/gamepad:*/button/south/down"\n  puts :pressed\nend'));
      await gp.click("#btn-run");
      await gp.waitForFunction(() => window.sonicPi.engine?.gamepad, null, { timeout: 60000 }).catch(() => {});
      await gp.waitForTimeout(1000);
      await gp.evaluate(() => { Object.assign(__pad.buttons[0], { pressed: true, value: 1 }); __pad.timestamp++; });
      await gp.waitForTimeout(300);
      await gp.evaluate(() => { Object.assign(__pad.buttons[0], { pressed: false, value: 0 }); __pad.axes[0] = 0.8; __pad.timestamp++; });
      await gp.waitForFunction(() => /pressed/.test(document.getElementById("log").textContent) && /\/axis\/left_x/.test(document.getElementById("cues").textContent) && /\/south\/up/.test(document.getElementById("cues").textContent), null, { timeout: 5000 }).catch(() => {});
      const pad = await gp.evaluate(() => ({ cues: [...document.querySelectorAll("#cues > *")].map((e) => e.textContent.trim()).filter((t) => t.startsWith("/gamepad:")).map((t) => t.replace(/\[.*$/, "")), synced: /pressed/.test(document.getElementById("log").textContent) }));
      const want = ["button/south", "button/south/down", "button/south/up", "axis/left_x"].every((w) => pad.cues.some((c) => c.endsWith(`/${w}`)));
      check("a game controller's buttons and sticks are cues, as native's, and a sync on a press wakes", want && pad.synced, JSON.stringify(pad));
      await ctx.close();
    }

    const phone = await browser.newContext({ ...devices["iPhone 15"], ...HTTPS });
    await phone.addInitScript(() => { try { localStorage.setItem("sp-help-seen", "true"); } catch {} });
    const ph = await phone.newPage();
    const phoneErrors = [];
    ph.on("pageerror", (e) => phoneErrors.push(e.message));
    await ph.goto(BASE + "#app");
    await ph.waitForFunction(() => window.sonicPi?.keys, null, { timeout: 60000 });
    const apart = (a, b) => !(a && b && a.top < b.bottom && b.top < a.bottom && a.left < b.right && b.left < a.right);
    const complete = async () => {
      await ph.click('#input-dock .hg-button[data-skbtn="{tab}"]');
      await ph.waitForSelector(".cm-tooltip-autocomplete li[aria-selected]", { timeout: 10000 });
    };
    // the detail pane under or over the list, never on the caret line, the list or the keyboard, wherever the caret is
    for (const [where, before, after] of [["near the top", "", "\n".repeat(14)], ["low down", "\n".repeat(12), "\n"], ["in the middle", "\n".repeat(6), "\n".repeat(8)]]) {
      await ph.evaluate(([code, anchor]) => { const v = window.sonicPi.editor.view; window.sonicPi.editor.setCode(code); v.dispatch({ selection: { anchor }, scrollIntoView: true }); v.focus(); window.sonicPi.keyboard.open(); }, [`${before}lin${after}`, before.length + 3]);
      await ph.waitForSelector("#input-dock:not([hidden])");
      await complete();
      await ph.waitForSelector(".cm-completionInfo", { timeout: 10000 });
      await ph.waitForTimeout(300);
      const r = await ph.evaluate(() => {
        const rect = (el) => { const b = el?.getBoundingClientRect(); return b ? { top: b.top, bottom: b.bottom, left: b.left, right: b.right } : null; };
        const v = window.sonicPi.editor.view, c = v.coordsAtPos(v.state.selection.main.head);
        return { list: rect(document.querySelector(".cm-tooltip-autocomplete > ul")), info: rect(document.querySelector(".cm-completionInfo")), dock: rect(document.querySelector("#input-dock")),
                 caret: c && { top: c.top, bottom: c.bottom, left: 0, right: innerWidth }, vw: innerWidth };
      });
      check(`on a phone the completion pane keeps off the caret line, the list and the keyboard (caret ${where})`,
            !!r.info && apart(r.info, r.caret) && apart(r.info, r.list) && r.info.bottom <= r.dock.top && r.info.left >= 0 && r.info.right <= r.vw, JSON.stringify(r));
      await ph.keyboard.press("Escape");
    }
    // the code keyboard's arrows walk the list (once the list has settled: its source answers again after the first rows show)
    await complete();
    await ph.waitForSelector(".cm-completionInfo", { timeout: 10000 });
    await ph.waitForTimeout(300);
    const selected = () => ph.evaluate(() => document.querySelector(".cm-tooltip-autocomplete li[aria-selected]")?.textContent);
    const s0 = await selected();
    await ph.click('#input-dock .hg-button[data-skbtn="{down}"]'); await ph.waitForTimeout(120);
    const s1 = await selected();
    await ph.click('#input-dock .hg-button[data-skbtn="{up}"]'); await ph.waitForTimeout(120);
    const s2 = await selected();
    check("on a phone the code keyboard's ↓ and ↑ walk the completion list", !!s0 && s1 !== s0 && s2 === s0, `${s0} → ${s1} → ${s2}`);
    // Δ with the code keyboard up: the keyboard makes way, and the help pane shows
    await ph.keyboard.press("Escape");
    if (await ph.evaluate(() => !!document.body.dataset.drawer)) { await ph.click("#btn-help"); }
    await ph.evaluate(() => window.sonicPi.keyboard.open());
    await ph.waitForSelector("#input-dock:not([hidden])");
    await ph.click("#btn-help");
    const helped = await ph.waitForFunction(() => !document.body.classList.contains("kbd-open") && !!document.body.dataset.drawer, null, { timeout: 3000 }).then(() => true).catch(() => false);
    check("on a phone Δ puts the code keyboard away and shows the help pane", helped,
      await ph.evaluate(() => `kbd-open ${document.body.classList.contains("kbd-open")}, pane ${document.body.dataset.drawer || "none"}`));
    await ph.click("#btn-help");   // put away again: what follows starts from the code
    // the buffers are the foot's own pads on a phone, as many across the width as the set has (workspace.js), in place
    // of the caret readout (style.css), the set's chip over them: a tap on a pad switches, as a tab does on a desktop
    await ph.keyboard.press("Escape");
    await ph.evaluate(() => window.sonicPi.keyboard.close());   // the code keyboard hides the foot, and covers the drawer, while it is up
    await ph.waitForFunction(() => getComputedStyle(document.getElementById("editor-foot")).display !== "none");
    const caretShown = await ph.evaluate(() => getComputedStyle(document.getElementById("caret-pos")).display !== "none");
    const pads = await ph.locator("#buffer-tabs .buffer-tab").count(), size = await ph.evaluate(() => window.sonicPi.workspace.size);
    await ph.click("#buffer-tabs .buffer-tab[data-idx='2']");
    await ph.waitForTimeout(100);
    const activeBuffer = await ph.evaluate(() => window.sonicPi.editor.active);
    const lit = await ph.locator("#buffer-tabs .buffer-tab.active").getAttribute("data-idx");
    check("on a phone the buffers are pads across the foot, and a tap switches",
          !caretShown && pads === size && activeBuffer === 2 && lit === "2", `caret ${caretShown}, ${pads} pads of ${size}, active ${activeBuffer}, lit ${lit}`);
    // the rail's zoom pair (the drawer head, with native's ZoomBar, is hidden on a phone) zooms the pane and leaves it open
    await ph.evaluate(() => document.querySelector("#drawer-rail [data-drawer=docs]").click());   // Help would toggle whatever pane is open
    await ph.waitForFunction(() => document.body.dataset.drawer === "docs");
    // the help panel's tabs zoom as one (main.js zoomPane): --help-zoom, not a variable per pane
    const railZoom = () => ph.evaluate(() => Number(getComputedStyle(document.documentElement).getPropertyValue("--help-zoom")));
    const rz0 = await railZoom();
    await ph.click("#rail-smaller");
    await ph.waitForTimeout(100);
    const rz1 = await railZoom(), railDrawer = await ph.evaluate(() => document.body.dataset.drawer);
    check("on a phone the rail's zoom buttons zoom the docs and keep the drawer open", railDrawer === "docs" && rz1 < rz0, `drawer ${railDrawer}, zoom ${rz0} → ${rz1}`);
    check("no uncaught errors on the phone page", phoneErrors.length === 0, phoneErrors.slice(0, 3).join(" | "));
    await phone.close();
  } finally {
    await browser.close();
  }
}

console.log("");
for (const e of [...new Set(results.map((r) => r.engine))]) {
  const rs = results.filter((r) => r.engine === e);
  const bad = rs.filter((r) => !r.ok), skipped = rs.filter((r) => r.skipped);
  console.log(`${e.padEnd(8)} ${rs.length - bad.length - skipped.length} ok · ${skipped.length} skipped · ${bad.length} FAIL${bad.length ? `: ${bad.map((r) => r.name).join("; ")}` : ""}`);
}
process.exit(results.some((r) => !r.ok) ? 1 : 0);

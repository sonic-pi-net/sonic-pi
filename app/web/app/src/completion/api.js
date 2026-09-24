// SPDX-License-Identifier: AGPL-3.0-or-later
// What to offer at the caret: a port of native Sonic Pi's
// app/gui/utils/scintilla_api.cpp (v5 GUI), minus what the web build does not
// have (tracks, plugins, Link audio peers). Fed by web/data/completion.json
// and the reference pages, generated from the language oracle.
import { lineToContext, resolveArgKind, resolveFnOpts, atOptKeySlot } from "./context.js";

// Native keeps these lists by hand; so does this.
const CHORDS = ["'1'", "'5'", "'+5'", "'m+5'", ":sus2", ":sus4", "'6'", ":m6", "'7sus2'", "'7sus4'", "'7-5'", ":halfdiminished", "'7+5'", "'m7+5'", "'9'", ":m9", "'m7+9'", ":maj9", "'9sus4'", "'9-5'", "'6*9'", "'m6*9'", "'7-9'", "'m7-9'", "'7-10'", "'7+9'", "'7-11'", "'7-13'", "'9+5'", "'m9+5'", "'7+5-9'", "'m7+5-9'", "'11'", ":m11", ":maj11", "'11+'", "'m11+'", "'13'", ":m13", ":maj13", ":add2", ":add4", ":add9", ":add11", ":add13", ":madd2", ":madd4", ":madd9", ":madd11", ":madd13", ":major", ":maj", ":M", ":minor", ":min", ":m", ":major7", ":maj7", ":dom7", "'7'", ":M7", ":minor7", ":min7", ":m7", ":minor_major7", "'mM7'", "'mmaj7'", ":augmented", ":a", ":diminished", ":dim", ":i", ":diminished7", ":dim7", ":i7", ":halfdim", "'m7b5'", "'m7-5'"];
const SCALES = [":diatonic", ":ionian", ":major", ":dorian", ":phrygian", ":lydian", ":mixolydian", ":aeolian", ":minor", ":locrian", ":hex_major6", ":hex_dorian", ":hex_phrygian", ":hex_major7", ":hex_sus", ":hex_aeolian", ":minor_pentatonic", ":yu", ":major_pentatonic", ":gong", ":egyptian", ":shang", ":jiao", ":zhi", ":ritusen", ":whole_tone", ":whole", ":chromatic", ":harmonic_minor", ":melodic_minor_asc", ":hungarian_minor", ":octatonic", ":messiaen1", ":messiaen2", ":messiaen3", ":messiaen4", ":messiaen5", ":messiaen6", ":messiaen7", ":super_locrian", ":hirajoshi", ":kumoi", ":neapolitan_major", ":bartok", ":bhairav", ":locrian_major", ":ahirbhairav", ":enigmatic", ":neapolitan_minor", ":pelog", ":augmented2", ":scriabin", ":harmonic_major", ":melodic_minor_desc", ":romanian_minor", ":hindu", ":iwato", ":melodic_minor", ":diminished2", ":marva", ":melodic_major", ":indian", ":spanish", ":prometheus", ":diminished", ":todi", ":leading_whole", ":augmented", ":purvi", ":chinese", ":lydian_minor", ":blues_major", ":blues_minor", ":lydian_dominant", ":acoustic", ":altered", ":phrygian_dominant", ":double_harmonic", ":byzantine", ":cargah", ":buselik", ":buselik_2", ":kurdi", ":rast", ":acemli_rast", ":ussak", ":bayati", ":bayati_2", ":isfahan", ":isfahan_2", ":hicaz_humayun", ":hicaz_humayun_2", ":hicaz", ":hicaz_2", ":uzzal", ":uzzal_2", ":zirguleli_hicaz", ":zirguleli_hicaz_2", ":huseyni", ":huseyni_2", ":muhayyer", ":gulizar", ":neva", ":neva_2", ":tahir", ":tahir_2", ":karcigar", ":suznak", ":suznak_2", ":mahur", ":acem_asiran", ":nihavend", ":nihavend_2", ":sultani_yegah", ":sultani_yegah_2", ":kurdili_hicazkar", ":kurdili_hicazkar_2", ":kurdili_hicazkar_3", ":kurdili_hicazkar_4", ":kurdili_hicazkar_5", ":zirguleli_suznak", ":zirguleli_suznak_2", ":zirguleli_suznak_3", ":hicazkar", ":hicazkar_2", ":evcara", ":evcara_2", ":evcara_3", ":evcara_4", ":suzidil", ":suzidil_2", ":sedaraban", ":sedaraban_2", ":segah", ":segah_2", ":huzzam", ":huzzam_2", ":bayati_araban", ":acem_kurdi", ":sehnaz", ":sehnaz_2", ":sehnaz_3", ":sehnaz_4", ":saba", ":dugah", ":dugah_2", ":evic", ":evic_2", ":bestenigar", ":ferahnak", ":sevkefza", ":sevkefza_2", ":sevkefza_3", ":ferahfeza", ":ferahfeza_2", ":yegah", ":yegah_2"];
const EXAMPLES = [":haunted", ":ambient_experiment", ":chord_inversions", ":filtered_dnb", ":fm_noise", ":jungle", ":ocean", ":reich_phase", ":acid", ":ambient", ":compus_beats", ":echo_drama", ":idm_breakbeat", ":tron_bike", ":wob_rhyth", ":bach", ":driving_pulse", ":monday_blues", ":rerezzed", ":square_skit", ":blimp_zones", ":blip_rhythm", ":shufflit", ":tilburg_2", ":time_machine", ":sonic_dreams", ":blockgame", ":cloud_beat", ":lorezzed"];
const TUNINGS = [":just", ":pythagorean", ":meantone", ":equal"];
const MIDI_PARAMS = ["sustain:", "velocity:", "vel:", "velocity_f:", "vel_f:", "port:", "channel:"];
const RANDOM_SOURCES = [":white", ":light_pink", ":pink", ":dark_pink", ":perlin"];

/** An opt's doc (<p>Default: <code>3</code></p><p>Time for reverberation to complete in seconds</p>…) as a line:
 * "Time for reverberation to complete in seconds, default 3". What a screen reader hears after the opt's name. */
export function optSummary(html) {
  if (!html) return "";
  const text = (h) => h.replace(/<[^>]*>/g, "").replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&amp;/g, "&").replace(/\s+/g, " ").trim();
  const paras = [...html.matchAll(/<p>([\s\S]*?)<\/p>/g)].map((m) => m[1]);
  const def = paras.map((p) => /^\s*Default:\s*([\s\S]*)$/.exec(p)).find(Boolean);
  const say = paras.map(text).find((t) => t && !/^Default:/.test(t)) ?? "";
  const first = (/^(.+?[.!?])(\s|$)/.exec(say)?.[1] ?? say).replace(/\.$/, "");
  // the default's value alone (the line goes on: "· slidable"), and not twice where the sentence has said it
  const value = def ? text(/<code>([\s\S]*?)<\/code>/.exec(def[1])?.[1] ?? def[1].split("·")[0]) : "";
  return [first, value && !/\bdefault\b/i.test(first) ? `default ${value}` : ""].filter(Boolean).join(", ");
}

function lastWordBeforePartial(context) {
  for (let i = context.length - 2; i >= 0; --i) if (context[i]) return context[i];
  return "";
}

// MIDI numbers first, each with its note name; then named spellings. C4 = 60.
let notesCache = null;
export function noteCompletions() {
  if (notesCache) return notesCache;
  const canon = ["c", "cs", "d", "eb", "e", "f", "fs", "g", "ab", "a", "bb", "b"];
  const out = [];
  for (let midi = 36; midi <= 96; ++midi) {
    out.push({ text: String(midi), kind: "note", summary: `:${canon[midi % 12]}${Math.floor(midi / 12) - 1}`, note: midi });
  }
  const spellings = [["c", 0], ["cs", 1], ["db", 1], ["d", 2], ["ds", 3], ["eb", 3], ["e", 4], ["f", 5], ["fs", 6], ["gb", 6], ["g", 7], ["gs", 8], ["ab", 8], ["a", 9], ["as", 10], ["bb", 10], ["b", 11], ["cb", -1], ["es", 5], ["fb", 4], ["bs", 12]];
  for (let oct = 2; oct <= 7; ++oct) {
    const base = (oct + 1) * 12;
    for (const [name, offset] of spellings) {
      const midi = base + offset;
      if (midi < 36 || midi > 96) continue;
      out.push({ text: `:${name}${oct}`, kind: "note", summary: String(midi), note: midi });
    }
  }
  return (notesCache = out);
}

function bareName(s) {
  let t = s.trim();
  if (t.startsWith(":")) t = t.slice(1);
  if (t.startsWith("'") && t.endsWith("'") && t.length >= 2) t = t.slice(1, -1);
  return t;
}

function nextArgToken(after) {
  const m = /^[\s,(]*([:'][^\s,)\]]*)/.exec(after);
  return m ? m[1] : "";
}

function tonicToMidi(tok) {
  const t = tok.trim().toLowerCase();
  for (const it of noteCompletions()) if (it.text.toLowerCase() === t) return it.note;
  const n = Number.parseInt(t, 10);
  return String(n) === t ? n : -1;
}

function enumLabelFor(doc, value) {
  if (!doc || !value) return "";
  const esc = value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const m = new RegExp(`\\b${esc}\\b\\s*=?\\s*([A-Za-z][A-Za-z ]*?)(?=[,.;)<]|\\s+\\d|\\s+and\\b|$)`).exec(doc);
  return m ? m[1].trim() : "";
}

function makeRange(lo, hi, def, loExcl, hiExcl) {
  const grid = Math.pow(10, Math.floor(Math.log10(hi - lo)) - 2);
  if (loExcl) lo += grid;
  if (hiExcl) hi -= grid;
  return { lo, hi, def: Math.min(Math.max(def, lo), hi) };
}

function kindForContext(ctx) {
  switch (ctx) {
    case "FX": return "fx";
    case "Synth": return "synth";
    case "Sample": return "sample";
    case "Chord": return "chord";
    case "Scale": return "scale";
    case "Tuning": return "tuning";
    case "Examples": return "example";
    case "MidiOuts": return "port";
    case "CuePath": return "cue";
    case "PlayParam": case "SampleParam": case "MidiParam": case "RandomSource": return "opt";
    default: return "fn";
  }
}

// what an API offers before its data has arrived: nothing, in the shapes the data has
const EMPTY_DATA = { synths: {}, fx: {}, entries: {}, argKinds: {}, fnOpts: {}, chordIntervals: {}, scaleIntervals: {}, optOptions: {},
                     optRanges: {}, optOwners: { docs: [], options: [], ranges: [] }, playArgs: [], sampleArgs: [] };
const EMPTY_REF = { lang: [], synths: [], fx: [], samples: [] };

export class CompletionAPI {
  /**
   * @param data completion.json, or nothing yet: an API that offers nothing until load() is given it (the page need
   *             not wait for the editor's data to start)
   * @param ref { lang, synths, fx, samples } reference pages, for the name lists
   */
  constructor(data = null, ref = null) {
    this.synthResolver = () => "";
    this.lastKind = "";
    this.ownSynths = new Map();   // a program's own synths (addSynth), kept to be offered again after a load
    this.load(data ?? EMPTY_DATA, ref ?? EMPTY_REF);
  }

  /** The data arrived: completion.json and the reference pages. What was added meanwhile (a program's own synths, the
   *  cue paths seen, the MIDI outputs) is kept. */
  load(data, ref) {
    const cues = this.keywords?.CuePath ?? [], outs = this.keywords?.MidiOuts ?? [];
    this.synthArgs = data.synths;
    this.fxArgs = data.fx;
    this.entries = data.entries;
    this.argKinds = data.argKinds;
    this.fnOpts = data.fnOpts;
    this.chordIntervals = data.chordIntervals;
    this.scaleIntervals = data.scaleIntervals;
    this.optOptions = data.optOptions;
    this.optRanges = {};
    for (const [k, r] of Object.entries(data.optRanges)) this.optRanges[k] = makeRange(...r);
    const key = (owner, opt) => `${owner} ${opt}`;
    this.ownerDocs = new Map(data.optOwners.docs.map((o) => [key(o.owner, o.opt), o.doc]));
    this.ownerOptions = new Map(data.optOwners.options.map((o) => [key(o.owner, o.opt), o.options]));
    this.ownerRanges = new Map(data.optOwners.ranges.map((o) => [key(o.owner, o.opt), makeRange(...o.range)]));
    this.key = key;
    this.keywords = {
      Func: ref.lang.map((p) => p.key),
      Synth: ref.synths.map((p) => ":" + p.key),
      FX: ref.fx.map((p) => ":" + p.key),
      Sample: ref.samples.flatMap((g) => g.samples.map((s) => ":" + s)),
      Chord: CHORDS, Scale: SCALES, Examples: EXAMPLES, Tuning: TUNINGS,
      MidiParam: MIDI_PARAMS, RandomSource: RANDOM_SOURCES,
      PlayParam: data.playArgs, SampleParam: data.sampleArgs,
      CuePath: cues, MidiOuts: outs,
    };
    for (const page of this.ownSynths.values()) this.addSynth(page);
  }

  setSynthResolver(fn) { this.synthResolver = fn; }

  /** A synth of the program's own (load_synthdef, its metadata: synth-meta.js), offered as a built-in one is: its
   *  name after use_synth and synth, its opts after its name, its doc in the detail pane. */
  addSynth(page) {
    this.ownSynths.set(page.key, page);
    const name = ":" + page.key;
    if (!this.keywords.Synth.includes(name)) this.keywords.Synth.push(name);
    this.synthArgs[name] = page.opts.map((o) => `${o.name}:`);
    this.entries[name] = { summary: page.title, doc: page.doc_html };
    for (const o of page.opts) {
      const at = this.key(name, `${o.name}:`);
      if (o.doc) this.ownerDocs.set(at, `<p>${o.doc}</p>`);
      if (typeof o.min === "number" && typeof o.max === "number") this.ownerRanges.set(at, makeRange(o.min, o.max, typeof o.default === "number" ? o.default : o.min));
      if (Array.isArray(o.options)) this.ownerOptions.set(at, o.options);
    }
  }
  addCuePath(path, val) {
    if (!this.keywords.CuePath.includes(path)) this.keywords.CuePath.push(path);
    if (val !== undefined) (this.cueValues ??= new Map()).set(path, val);   // what it last carried, for the panel
  }
  cueValue(path) { return this.cueValues?.get(path); }
  updateMidiOuts(names) { this.keywords.MidiOuts = names.map((n) => `"${n}"`); }

  entry(name) { return this.entries[name] || {}; }
  // with no owner, the fallback: `"" ?? fallback` would be "", and no row would have its doc
  ownerDoc(owner, opt, fallback) { return (owner ? this.ownerDocs.get(this.key(owner, opt)) : undefined) ?? fallback; }
  ownerOpts(owner, opt, fallback) { return (owner ? this.ownerOptions.get(this.key(owner, opt)) : undefined) ?? fallback; }
  ownerRange(owner, opt) { return owner ? this.ownerRanges.get(this.key(owner, opt)) : undefined; }

  isNoteContext(context) {
    const lw = lastWordBeforePartial(context);
    if (lw === "play" || lw === "scale" || lw === "chord" || lw === "note:") return true;
    return resolveArgKind(context, this.argKinds) === "Note";
  }

  ownerForContext(context) {
    if (!context.length) return "";
    const first = context[0], second = context.length > 1 ? context[1] : "";
    if (first === "with_fx" && this.fxArgs[second]) return second;
    if (first === "synth" && this.synthArgs[second]) return second;
    if (first === "play" || first === "control") {
      const synth = this.synthResolver();
      if (synth) return synth.startsWith(":") ? synth : ":" + synth;
    }
    return "";
  }

  /** The candidates at a caret: `line` is the caret's line, `col` its column. */
  completionsAt(line, col) {
    const context = lineToContext(line, col);
    return { context, items: this.completionsFor(context, line.slice(col)) };
  }

  completionsFor(context, afterCursor = "") {
    if (this.isNoteContext(context)) {
      const lw = lastWordBeforePartial(context);
      if (lw === "chord" || lw === "scale") {
        const iv = (lw === "scale" ? this.scaleIntervals : this.chordIntervals)[bareName(nextArgToken(afterCursor))];
        if (iv && iv.length) return noteCompletions().map((it) => ({ ...it, intervals: iv }));
      }
      return noteCompletions();
    }
    const optBefore = lastWordBeforePartial(context);
    const owner = this.ownerForContext(context);
    if (this.optOptions[optBefore]) {
      const optDoc = this.ownerDoc(owner, optBefore, this.entry(optBefore).doc);
      let illo = "";
      if (optBefore === "wave:" || optBefore === "mod_wave:") illo = "wave";
      else if (optBefore.endsWith("env_curve:")) illo = "curve";
      return this.ownerOpts(owner, optBefore, this.optOptions[optBefore]).map((v) => ({
        kind: "optval", text: v, summary: enumLabelFor(optDoc, v), doc: optDoc, illo,
      }));
    }
    const ownRange = this.ownerRange(owner, optBefore);
    if (ownRange || this.optRanges[optBefore]) {
      const r = ownRange || this.optRanges[optBefore];
      const typed = context.length ? Number(context[context.length - 1]) : NaN;
      const partial = context.length ? context[context.length - 1] : "";
      return [{
        kind: "range", slider: true, rmin: r.lo, rmax: r.hi,
        rdefault: partial !== "" && Number.isFinite(typed) ? typed : r.def,
        text: optBefore, doc: this.ownerDoc(owner, optBefore, this.entry(optBefore).doc),
      }];
    }
    const names = this.updateAutoCompletionList(context);
    const isChord = this.lastKind === "chord", isScale = this.lastKind === "scale";
    let tonic = 60;
    if (isChord || isScale) {
      const fnIdx = context.lastIndexOf(isChord ? "chord" : "scale");
      for (let i = fnIdx + 1; fnIdx >= 0 && i < context.length - 1; ++i) {
        if (!context[i]) continue;
        const m = tonicToMidi(context[i]);
        if (m >= 0) tonic = m;
        break;
      }
    }
    return names.map((n) => {
      const e = this.entry(n);
      const item = { text: n, kind: this.lastKind, summary: e.summary || "", usage: e.usage || "", doc: this.ownerDoc(owner, n, e.doc || "") };
      // an opt's summary is its own name in the data: its doc's first sentence says what it is, and its default
      if (item.kind === "opt" && (!item.summary || item.summary === n)) item.summary = optSummary(item.doc);
      if (isChord || isScale) {
        const table = (isChord ? this.chordIntervals : this.scaleIntervals)[bareName(n)];
        if (table && table.length) { item.intervals = table; item.note = tonic; }
      }
      return item;
    });
  }

  updateAutoCompletionList(context) {
    this.lastKind = "";
    if (!context.length) return [];
    let ctx = "Func";
    const partial = context[context.length - 1];
    const words = context.slice(0, -1).filter((w) => w !== "");
    const last = words.length ? words[words.length - 1] : "";
    const first = words.length ? words[0] : "";
    const second = words.length < 2 ? "" : words[1];
    const docOpts = resolveFnOpts(context, this.fnOpts);
    switch (resolveArgKind(context, this.argKinds)) {
      case "Sample": ctx = "Sample"; break;
      case "CuePath": ctx = "CuePath"; break;
      case "Fx": ctx = "FX"; break;
      case "Synth": ctx = "Synth"; break;
      case "Scale": ctx = "Scale"; break;
      case "Chord": ctx = "Chord"; break;
      case "LinkAudioPeer": case "LinkAudioChannel": case "Track": return [];   // native only
      default: break;
    }
    if (ctx !== "Func") {
      // resolved from the arg-kinds table
    } else if (last === "sync:") {
      ctx = "CuePath";
    } else if ((first === "midi" || first.startsWith("midi_") || first === "use_midi_defaults" || first === "with_midi_defaults") && last === "port:") {
      ctx = "MidiOuts";
    } else if (last === "load_example") {
      ctx = "Examples";
    } else if (last === "use_random_source" || last === "with_random_source") {
      ctx = "RandomSource";
    } else if (last === "use_tuning" || last === "with_tuning") {
      ctx = "Tuning";
    } else if (words.length >= 2 && first === "with_fx") {
      if (last.endsWith(":")) return [];
      if (this.fxArgs[second]) { this.lastKind = "opt"; return this.fxArgs[second]; }
    } else if (words.length >= 2 && first === "synth") {
      if (last.endsWith(":")) return [];
      if (this.synthArgs[second]) { this.lastKind = "opt"; return this.synthArgs[second]; }
    } else if (words.length >= 2 && (first === "play" || first === "control")) {
      if (last.endsWith(":")) return [];
      const synth = this.synthResolver();
      if (synth) {
        const k = synth.startsWith(":") ? synth : ":" + synth;
        if (this.synthArgs[k]) { this.lastKind = "opt"; return this.synthArgs[k]; }
      }
      ctx = "PlayParam";
    } else if (words.length >= 2 && first === "sample") {
      if (last.endsWith(":")) return [];
      ctx = "SampleParam";
    } else if (first === "use_sample_defaults" || first === "with_sample_defaults") {
      if (last.endsWith(":")) return [];
      ctx = "SampleParam";
    } else if (words.length >= 2 && first === "midi") {
      if (last.endsWith(":")) return [];
      ctx = "MidiParam";
    } else if (docOpts.length) {
      this.lastKind = "opt";
      return docOpts;
    } else if (context.length > 1) {
      if (atOptKeySlot(context)) return [];
      if (partial.length <= 2) return [];
    }
    this.lastKind = kindForContext(ctx);
    return [...this.keywords[ctx]];
  }
}

/** The synth in effect at `pos` in `doc`: the last use_synth or with_synth above it. */
export function synthAt(doc, pos) {
  const before = doc.slice(0, pos);
  let found = "";
  for (const m of before.matchAll(/\b(?:use_synth|with_synth)\s*\(?\s*:(\w+)/g)) found = m[1];
  return found;
}

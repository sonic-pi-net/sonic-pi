// SPDX-License-Identifier: AGPL-3.0-or-later
// Errors as Sonic Pi explains them: what went wrong, in the words of someone making music, where, and how to fix it.
// The parser's and the interpreter's own words stay available (the card's Details), never the headline.
//
// explainError({ syntax, cls, message, line, col, code, thread }, known) answers
//   { headline, hint?, line, from, to, fix?, example?, reason }
// headline and hint mark code with backticks (the card sets them as code); line is 1-based, from/to 0-based columns in
// it (from === to: a point, drawn as a caret); a fix is { line, from, to, insert, label } in the same terms, only when
// the change is certain enough to offer as one tap. known (makeKnown) is what the docs say exists.

export function makeKnown(completion) {
  const entries = completion.entries ?? {};
  const fns = [], blockFns = new Set(), usage = new Map();
  for (const [k, e] of Object.entries(entries)) {
    if (k.startsWith(":")) continue;
    fns.push(k);
    if (e.usage) usage.set(k, e.usage);
    if (/\bdo\b/.test(e.usage ?? "")) blockFns.add(k);
  }
  ["loop", "define", "in_thread", "live_loop", "with_fx", "uncomment", "comment"].forEach((f) => blockFns.add(f));
  const optDefault = new Map(Object.entries(completion.optRanges ?? {}).map(([k, r]) => [k.replace(/:$/, ""), r[2]]));
  const synthNames = new Set(Object.keys(completion.synths ?? {})), fxNames = new Set(Object.keys(completion.fx ?? {}));
  const bare = (list) => (list ?? []).map((o) => o.replace(/:$/, ""));
  return {
    fns, blockFns, usage, optDefault,
    // each synth's, FX's and sample's opts, for "did you mean" on an unknown one
    opts: {
      synth: (name) => bare(completion.synths?.[`:${name}`]),
      FX: (name) => bare(completion.fx?.[`:${name}`]),
      sample: () => bare(completion.sampleArgs),
    },
    allOpts: [...new Set([...Object.values(completion.synths ?? {}), ...Object.values(completion.fx ?? {}), completion.sampleArgs ?? []].flat().map((o) => o.replace(/:$/, "")))],
    samples: Object.keys(entries).filter((k) => k.startsWith(":") && !synthNames.has(k) && !fxNames.has(k)).map((k) => k.slice(1)),
    synths: Object.keys(completion.synths ?? {}).map((s) => s.slice(1)),
    fx: Object.keys(completion.fx ?? {}).map((s) => s.slice(1)),
    chords: Object.keys(completion.chordIntervals ?? {}),
    scales: Object.keys(completion.scaleIntervals ?? {}),
  };
}

// ── Reading the code ────────────────────────────────────────────────────────

// a line with its strings and comment blanked (same length), so brackets and words in them are not counted
function bare(line) {
  let out = "", quote = null;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (quote) {
      if (ch === "\\") { out += "  "; i++; continue; }
      if (ch === quote) { quote = null; out += ch; continue; }
      out += " ";
    } else if (ch === "#") {
      return out + " ".repeat(line.length - i);
    } else {
      if (ch === '"' || ch === "'") quote = ch;
      out += ch;
    }
  }
  return out;
}

const indentOf = (s) => /^\s*/.exec(s)[0];
const OPENERS = /^(if|unless|while|until|case|begin|def|class|module)\b/;
const PAIRS = { "(": ")", "[": "]", "{": "}" };

// the brackets and blocks still open at the end of the code: [{ kind: "(" | "do" | "if" …, line, col, text }]
function openings(lines) {
  const stack = [];
  lines.forEach((raw, i) => {
    const s = bare(raw);
    const start = s.search(/\S/);
    // if, def and the like open a block only where they start a line (play 60 if x does not)
    const m = start >= 0 ? OPENERS.exec(s.slice(start)) : null;
    if (m) stack.push({ kind: m[0], line: i + 1, col: start, text: raw.trim() });
    const re = /[()[\]{}]|\b(do|end)\b/g;
    for (let m; (m = re.exec(s)); ) {
      const t = m[0];
      if (t === "do") stack.push({ kind: "do", line: i + 1, col: m.index, text: raw.trim() });
      else if (t === "end") {
        const k = stack.findLastIndex((o) => !PAIRS[o.kind]);
        if (k >= 0) stack.splice(k, 1);
      } else if (PAIRS[t]) stack.push({ kind: t, line: i + 1, col: m.index, text: raw.trim() });
      else {
        const k = stack.findLastIndex((o) => PAIRS[o.kind] === t);
        if (k >= 0) stack.splice(k, 1);
      }
    }
  });
  return stack;
}

// Damerau–Levenshtein distance, for "did you mean"
function distance(a, b) {
  const d = Array.from({ length: a.length + 1 }, (_, i) => [i, ...new Array(b.length).fill(0)]);
  for (let j = 1; j <= b.length; j++) d[0][j] = j;
  for (let i = 1; i <= a.length; i++) {
    for (let j = 1; j <= b.length; j++) {
      const cost = a[i - 1] === b[j - 1] ? 0 : 1;
      d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);
      if (i > 1 && j > 1 && a[i - 1] === b[j - 2] && a[i - 2] === b[j - 1]) d[i][j] = Math.min(d[i][j], d[i - 2][j - 2] + 1);
    }
  }
  return d[a.length][b.length];
}
function closest(word, candidates) {
  const limit = word.length <= 4 ? 1 : word.length <= 8 ? 2 : 3;
  let best = null, bestD = Infinity;
  for (const c of candidates) {
    if (Math.abs(c.length - word.length) > limit) continue;
    const d = distance(word, c);
    // on a tie, the one sharing the longer start with the word, then the shorter (minorr: minor, not minor7)
    const prefix = (x) => { let i = 0; while (i < x.length && x[i] === word[i]) i++; return i; };
    if (d < bestD || (d === bestD && (prefix(c) > prefix(best) || (prefix(c) === prefix(best) && c.length < best.length)))) { best = c; bestD = d; }
  }
  return bestD <= limit ? best : null;
}

// where a word sits in a line, whole-word: [from, to] or null
function wordAt(line, word) {
  const esc = word.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const m = new RegExp(`(^|[^\\w:])(${esc})(?![\\w])`).exec(line);
  return m ? [m.index + m[1].length, m.index + m[1].length + m[2].length] : null;
}

const plural = (n, word) => `${n} ${word}${n === 1 ? "" : "s"}`;
const example = (known, fn) => (known.usage.get(fn) ? { code: known.usage.get(fn), fn } : undefined);

// ── Syntax errors: the code could not be read ─────────────────────────────

function explainSyntax(raw, code, line, col, known) {
  const lines = code.split("\n");
  const at = (n) => lines[n - 1] ?? "";
  const L = at(line), c0 = Math.max(0, Math.min((col || 1) - 1, L.length));
  const msg = raw.replace(/^syntax error,?\s*/, "");
  const tok = (/^(\d[\w.]*|[A-Za-z_]\w*[?!]?:?|:\w+[?!]?|\S)/.exec(L.slice(c0)) ?? [""])[0];
  const before = L.slice(0, c0);
  const firstFn = (s) => (/^\s*([a-z_]\w*)/.exec(s) ?? [])[1];
  const here = (headline, extra = {}) => ({ headline, line, from: c0, to: c0 + tok.length, ...extra });
  const opens = openings(lines);

  // a word at the start of a line with a stray , ' or " in it (p,ay, use_s,ynth, sam'ple): the parser reads a list of
  // names to assign to, or a string, and says something beside the point. The word, rejoined, is a function's name.
  {
    const w = /^(\s*)([a-z_][\w,'"]*[\w'"])(?=\s|$|\()/.exec(L) ?? /^(\s*)([a-z_]\w*['"]\w+)/.exec(L);
    if (w && /[,'"]/.test(w[2])) {
      const guess = known.fns.includes(w[2].replace(/[,'"]/g, "")) ? w[2].replace(/[,'"]/g, "") : closest(w[2], known.fns);
      if (guess) {
        const stray = /[,'"]/.exec(w[2])[0];
        const from = w[1].length, to = from + w[2].length;
        return {
          headline: `\`${w[2]}\` has a stray \`${stray}\` in it. Did you mean \`${guess}\`?`,
          line, from, to,
          fix: { line, from, to, insert: guess, label: `Change it to \`${guess}\`` },
          example: example(known, guess),
        };
      }
    }
  }

  // a number written without its leading 0 (.4): the parser trips on the point, at columns that vary (the point, the
  // digits, the = or comma before), so the line itself is searched, nearest the column first
  if (/unexpected '\.'|unexpected integer|expected an expression|expected an argument|receiver for unary/.test(msg)) {
    const bareLine = bare(L);
    const dots = [...bareLine.matchAll(/(^|[^\w.)\]}"'])(\.\d+)/g)].map((m) => ({ at: m.index + m[1].length, text: m[2] }));
    if (dots.length) {
      const d = dots.sort((a, b) => Math.abs(a.at - c0) - Math.abs(b.at - c0))[0];
      return {
        headline: `\`${d.text}\` needs a \`0\` in front of it: a number starts with a digit, so write \`0${d.text}\`.`,
        line, from: d.at, to: d.at + d.text.length,
        fix: { line, from: d.at, to: d.at, insert: "0", label: `Use \`0${d.text}\`` },
      };
    }
  }

  // a string that never ends
  if (/unterminated string/.test(msg)) {
    const q = L[c0] === "'" ? "'" : '"';
    return {
      headline: `This text in quotes never ends: it starts with \`${q}\` but has no closing \`${q}\`.`,
      line, from: c0, to: c0 + 1,
      fix: { line, from: L.length, to: L.length, insert: q, label: `Close it with \`${q}\` at the end of the line` },
    };
  }

  // a bracket opened and never closed
  const bracket = opens.findLast((o) => PAIRS[o.kind]);
  if (bracket && /end-of-input|expected a `[\]),]`|separator|expected a matching/.test(msg)) {
    const close = PAIRS[bracket.kind], B = at(bracket.line);
    const endAt = bare(B).trimEnd().length;
    return {
      headline: `This \`${bracket.kind}\` is never closed: it needs a \`${close}\` to match it.`,
      line: bracket.line, from: bracket.col, to: bracket.col + 1,
      fix: { line: bracket.line, from: endAt, to: endAt, insert: close, label: `Add \`${close}\` at the end of line ${bracket.line}` },
    };
  }

  // a block with no end: by the indentation, the block whose lines stop without one (so a missing end inside another
  // block is found there, not at the outer block the ends were counted against); else the last left open
  const byIndent = () => {
    let found = null;
    lines.forEach((raw, i) => {
      const s = bare(raw);
      if (!/\bdo(\s*\|[^|]*\|)?\s*$/.test(s.trimEnd()) && !OPENERS.test(s.trim())) return;
      const ind = indentOf(raw).length;
      for (let n = i + 2; n <= lines.length; n++) {
        const t = lines[n - 1];
        if (!t.trim()) continue;
        if (indentOf(t).length > ind) continue;
        if (!/^(end|else|elsif|when|rescue|ensure)\b/.test(t.trim())) found = { kind: "do", line: i + 1, col: Math.max(0, s.search(/\bdo\b/)), text: raw.trim() };
        return;
      }
      found = { kind: "do", line: i + 1, col: Math.max(0, s.search(/\bdo\b/)), text: raw.trim() };
    });
    return found;
  };
  const block = /end-of-input/.test(msg) ? (byIndent() ?? opens.findLast((o) => !PAIRS[o.kind])) : null;
  if (block) {
    const B = at(block.line), ind = indentOf(B);
    let last = block.line;
    for (let n = block.line + 1; n <= lines.length; n++) {
      const s = at(n);
      if (!s.trim()) continue;
      if (indentOf(s).length > ind.length) last = n; else break;
    }
    const fn = firstFn(B);
    const what = block.kind === "do" ? (fn ? `\`${B.trim().replace(/\s+do\b.*$/, "")}\`` : "this block") : `this \`${block.kind}\``;
    return {
      headline: `${what[0].toUpperCase()}${what.slice(1)} on line ${block.line} has no \`end\` to finish it.`,
      hint: "Every `do` needs an `end` to show where its block stops.",
      line: block.line, from: block.col, to: block.col + block.kind.length,
      fix: { line: last, from: at(last).length, to: at(last).length, insert: `\n${ind}end`, label: `Add \`end\` after line ${last}` },
    };
  }

  // an end with nothing to close: most often the do was left off the line that starts the block
  if (/unexpected 'end'/.test(msg)) {
    for (let n = line - 1; n >= Math.max(1, line - 40); n--) {
      const s = bare(at(n)).trimEnd(), fn = firstFn(s);
      if (!s.trim()) continue;
      if (/\bdo(\s*\|[^|]*\|)?$/.test(s) || /^\s*end\b/.test(s)) break;
      if ((fn && known.blockFns.has(fn)) || /\.(times|each\w*|map)\s*$/.test(s)) {
        const call = at(n).trim();
        return {
          headline: `\`${call}\` needs a \`do\` at the end of its line to start its block.`,
          hint: `That's why the \`end\` on line ${line} has nothing to finish.`,
          line: n, from: s.length, to: s.length,
          fix: { line: n, from: s.length, to: s.length, insert: " do", label: `Add \`do\` to the end of line ${n}` },
          example: example(known, fn),
        };
      }
    }
    return here("This `end` has nothing to finish.", {
      hint: "Remove it, or check that the block above it starts with a `do`.",
      ...(L.trim() === "end" ? { fix: { line, from: 0, to: L.length, insert: "", label: "Remove this `end`" } } : {}),
    });
  }

  // a closing bracket with nothing to close
  if (/unexpected '[)\]}]'/.test(msg) && /[)\]}]/.test(tok)) {
    const gap = /\s*$/.exec(before)[0].length;
    return here(`This \`${tok}\` has nothing to close.`, { fix: { line, from: c0 - gap, to: c0 + 1, insert: "", label: `Remove the \`${tok}\`` } });
  }

  // the line before ends mid-thought (a comma, an opt with no value), so this line was read as more of it
  const prevN = (() => { for (let n = line - 1; n >= 1; n--) if (bare(at(n)).trim()) return n; return 0; })();
  const prev = prevN ? bare(at(prevN)).trimEnd() : "";
  if (prevN && /unexpected/.test(msg)) {
    const label = /(?<![:\w])([a-z_]\w*):$/.exec(prev);
    if (label) {
      const v = known.optDefault.get(label[1]);
      return {
        headline: `\`${label[1]}:\` on line ${prevN} is missing its value.`,
        hint: `Give it one, like \`${label[1]}: ${v ?? 1}\`.`,
        line: prevN, from: label.index, to: prev.length,
        fix: { line: prevN, from: prev.length, to: prev.length, insert: ` ${v ?? 1}`, label: `Give \`${label[1]}:\` the value \`${v ?? 1}\`` },
      };
    }
    if (prev.endsWith(",")) {
      return {
        headline: `Line ${prevN} ends with a comma, so Sonic Pi read this line as more of it.`,
        line: prevN, from: prev.length - 1, to: prev.length,
        fix: { line: prevN, from: prev.length - 1, to: prev.length, insert: "", label: `Remove the comma at the end of line ${prevN}` },
      };
    }
  }

  // an opt with no comma before it
  if (/^[a-z_]\w*:$/.test(tok) && L[c0 + tok.length] !== ":" && /[\w)\]"':]\s*$/.test(before)) {
    const gap = /\s*$/.exec(before)[0].length;
    const fn = firstFn(L);
    return here(`Opts are separated by commas: \`${tok}\` needs a \`,\` before it.`, {
      fix: { line, from: c0 - gap, to: c0, insert: ", ", label: `Add a comma before \`${tok}\`` },
      example: example(known, fn),
    });
  }

  // letters straight after a number: 4s
  if (/^[A-Za-z_]/.test(tok) && /\d$/.test(before)) {
    const num = /[\d.]+$/.exec(before)[0];
    return {
      headline: `\`${num}${tok}\` isn't a number: a number can't have letters straight after it.`,
      line, from: c0, to: c0 + tok.length,   // the letters marked, as native marks them: they are what was not expected
      fix: { line, from: c0, to: c0 + tok.length, insert: "", label: `Use \`${num}\`` },
    };
  }

  // two commas in a row
  if (/unexpected ','/.test(msg) && /,\s*$/.test(before)) {
    return here("There are two commas in a row here.", { fix: { line, from: c0, to: c0 + 1, insert: "", label: "Remove the extra comma" } });
  }

  // two values with no comma between them: play :c4 :e4
  if (/unexpected (integer|float|local variable or method|symbol|string|':'|constant)/.test(msg) && /[\w)\]"']\s+$/.test(before) && !/\b(do|end)\s*$/.test(before)) {
    const gap = /\s*$/.exec(before)[0].length;
    return here(`Two values in a row need a comma between them: add a \`,\` before \`${tok === ":" ? L.slice(c0).split(/[\s,)]/)[0] : tok}\`.`, {
      fix: { line, from: c0 - gap, to: c0 - gap, insert: ",", label: "Add the comma" },
    });
  }

  if (/fraction part after numeric literal/.test(msg)) {
    const head = /[\d.]+$/.exec(before)?.[0] ?? "", tail = /^[\d.]+/.exec(L.slice(c0))?.[0] ?? "";
    return { headline: `\`${head}${tail}\` has more than one decimal point: a number can only have one.`, line, from: c0 - head.length, to: c0 + tail.length };
  }
  if (/block parameters to end with `\|`/.test(msg)) {
    const m = /\|\s*([\w, ]*)$/.exec(bare(at(line - 1) || "").trimEnd()) ?? /\|\s*([\w, ]*)$/.exec(bare(L).trimEnd());
    const n = m && /\|\s*[\w, ]*$/.test(bare(at(line - 1)).trimEnd()) ? line - 1 : line;
    const s = bare(at(n)).trimEnd();
    return {
      headline: "The block's `|` needs a matching `|` after its names, like `do |i|`.",
      line: n, from: s.lastIndexOf("|"), to: s.length,
      fix: { line: n, from: s.length, to: s.length, insert: "|", label: "Add the closing `|`" },
    };
  }
  if (/expected an expression after the operator/.test(msg)) return here(`The \`${before.trim().slice(-1)}\` needs something after it.`);
  if (/receiver for unary `-`/.test(msg)) return here("The `-` needs a number after it, like `-1`.");

  return here(tok ? `Sonic Pi didn't expect \`${tok}\` here.` : "Sonic Pi couldn't make sense of this line.", {
    hint: "Look for a missing comma, bracket, `do` or `end` just before it.",
  });
}

// ── Runtime errors: the code was read, then something went wrong running it ─

// A link's code that could not be read (share.js decodeCode, main.js loadFromHash): what happened to it on its way,
// and what to do. No line: it is the link's fault, not the code's.
function explainLink(raw) {
  const said = (headline, hint) => ({ headline, hint, line: 0, from: 0, to: 0 });
  if (/cut short/.test(raw)) return said("This link is missing part of its code: it was cut short on its way here.", "A copy that missed the end, or a message that clipped a long link. Ask for the link again, or for the program as a file.");
  if (/newer Sonic Pi/.test(raw)) return said("This link was made by a newer Sonic Pi than this page.", "Reload the page for the latest Sonic Pi, then open the link again.");
  return said("This link's code could not be read: part of it is missing or changed.", "Ask for the link again, or for the program as a file. From another Sonic Pi? Reload this page first: the link may be newer than it.");
}

function explainRuntime(cls, raw, code, line, thread, known, fault) {
  const lines = code.split("\n");
  const L = lines[line - 1] ?? "";
  const span = (word) => wordAt(L, word) ?? [0, 0];
  const firstFn = (/^\s*([a-z_]\w*)/.exec(L) ?? [])[1];
  // the word the error is about, else the whole of the line's code
  const whole = () => [indentOf(L).length, L.trimEnd().length];
  const out = (headline, word, extra = {}) => { const [from, to] = (word && wordAt(L, word)) || whole(); return { headline, line, from, to, ...extra }; };
  const replace = (word, by, label) => { const s = wordAt(L, word); return s ? { line, from: s[0], to: s[1], insert: by, label } : undefined; };

  let m;
  if ((m = /undefined (?:local variable or method|method) '([^']+)'/.exec(raw))) {
    const name = m[1];
    if (!/^[A-Za-z_]/.test(name)) {
      const op = L.indexOf(` ${name} `);
      if (op >= 0) return { headline: `Something on this line has no value (it's \`nil\`), so \`${name}\` can't work with it.`, hint: "A variable that was never given a value, or a function that gives nothing back, is `nil`.", line, from: op + 1, to: op + 1 + name.length };
      return out(`Something on this line has no value (it's \`nil\`), so \`${name}\` can't work with it.`, null, {
        hint: "A variable that was never given a value, or a function that gives nothing back, is `nil`.",
      });
    }
    const defined = [...code.matchAll(/\bdefine\s+:(\w+)/g)].map((d) => d[1]);
    const assigned = [...code.matchAll(/^\s*([a-z_]\w*)\s*=[^=]/gm)].map((d) => d[1]);
    const setAt = lines.findIndex((s) => new RegExp(`^\\s*${name}\\s*=[^=]`).test(s)) + 1;
    if (setAt > line) {
      return out(`\`${name}\` is used here, on line ${line}, before it's given a value on line ${setAt}.`, name, {
        hint: `Move \`${lines[setAt - 1].trim()}\` above line ${line}.`,
      });
    }
    // half a word split by a stray . ; , ' or space (sl.eep names sl, pla y names y): the word rejoined is a function's
    const esc = name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
    for (const re of [new RegExp(`(?<![\\w.])(${esc})[.,;' ](\\w+)`), new RegExp(`(?<![\\w.])(\\w+)[.,;' ](${esc})(?!\\w)`)]) {
      const sm = re.exec(L);
      if (!sm) continue;
      const joined = sm[1] + sm[2];
      const g = known.fns.includes(joined) ? joined : closest(joined, known.fns);
      if (!g) continue;
      const stray = sm[0].slice(sm[1].length, sm[1].length + 1);
      return {
        headline: `\`${sm[0]}\` has a stray ${stray === " " ? "space" : `\`${stray}\``} in it. Did you mean \`${g}\`?`,
        line, from: sm.index, to: sm.index + sm[0].length,
        fix: { line, from: sm.index, to: sm.index + sm[0].length, insert: g, label: `Change it to \`${g}\`` },
        example: example(known, g),
      };
    }
    // a name with a .method after it (notes.tick) is a value, a variable: only the code's own are candidates
    const asValue = new RegExp(`(?<![\\w.])${esc}\\s*\\.\\w`).test(L);
    if (asValue && !closest(name, assigned)) {
      return out(`\`${name}\` hasn't been given a value.`, name, { hint: `Give it one before it's used, like \`${name} = (ring 60, 64, 67)\`.` });
    }
    const guess = closest(name, asValue ? assigned : [...known.fns, ...defined, ...assigned]);
    if (guess) {
      return out(`Sonic Pi doesn't know \`${name}\`. Did you mean \`${guess}\`?`, name, {
        fix: replace(name, guess, `Change \`${name}\` to \`${guess}\``),
        example: assigned.includes(guess) || defined.includes(guess) ? undefined : example(known, guess),   // the code's own name has no docs
      });
    }
    return out(`Sonic Pi doesn't know \`${name}\`.`, name, {
      hint: `If it's a variable, give it a value before it's used (\`${name} = 1\`). If it's a function of your own, make it first with \`define :${name} do … end\`.`,
    });
  }

  const unknownName = (kind, list, word, helpTab) => {
    const guess = closest(word, list);
    return out(`There's no ${kind} called \`:${word}\`.${guess ? ` Did you mean \`:${guess}\`?` : ""}`, `:${word}`, {
      ...(guess ? { fix: replace(`:${word}`, `:${guess}`, `Change it to \`:${guess}\``) } : { hint: `Help's ${helpTab} lists them all.` }),
    });
  };
  // the unknown-opt warning (Preferences: Warn about unknown opts): an opt nothing takes, ignored as Sonic Pi ignores
  // it, the code playing on
  if ((m = /Unknown opt (\w+): for (synth|FX|sample) :?(\w+)/.exec(raw))) {
    const [, opt, kind, name] = m;
    const own = known.opts[kind](name);
    const guess = closest(opt, own) ?? closest(opt, known.allOpts);
    const what = kind === "sample" ? "`sample`" : `\`:${name}\``;
    if (!guess) return out(`\`${opt}:\` isn't an opt ${what} takes, so Sonic Pi ignores it.`, `${opt}:`, { hint: `The docs for ${what} list the opts it takes.` });
    return out(`\`${opt}:\` isn't an opt, so it does nothing here. Did you mean \`${guess}:\`?`, `${opt}:`, {
      // a real opt, but not one this synth has: said, so the fix is not taken for a cure
      ...(own.includes(guess) ? {} : { hint: `${what} doesn't use \`${guess}:\` either: other synths do.` }),
      fix: replace(`${opt}:`, `${guess}:`, `Change it to \`${guess}:\``),
    });
  }
  if ((m = /Unknown sample :(\w+)/.exec(raw))) return unknownName("sample", known.samples, m[1], "Samples tab");
  // Safe mode: an opt given a value its synth doesn't allow. The runtime sends the rule itself (SonicPi::OptError's
  // fault, from validation.rb, the same rule native checks), so this says the rule in Sonic Pi's friendliest words
  // and works out a value that would do — neither is read back out of the sentence the runtime raised.
  if (fault && fault.rule) {
    const { rule } = fault;
    // the opt as the code writes it: a sample's cutoff: reaches the synth as lpf:, and an error about a word that is
    // not on the line is no help
    const named = new RegExp(`\\b(${fault.opt}|${fault.as ?? fault.opt}):`);
    // which line: the run may not know (a sample heard for the first time loads in a thread of its own, and the
    // error comes from there), and then the one line that names the opt is the line meant
    let at = line, text = L;
    if (!named.test(text)) {
      const hits = lines.map((t, i) => [t, i + 1]).filter(([t]) => named.test(t));
      if (hits.length === 1) [text, at] = hits[0];
    }
    const opt = fault.as && new RegExp(`\\b${fault.as}:`).test(text) ? fault.as : fault.opt;
    const v = typeof fault.value === "number" ? fault.value : Number(fault.value);
    let got = fault.value == null ? "nothing" : String(fault.value);
    const num = Number.isFinite(v);
    // the nearest end of the range, when the value has overshot one
    const near = (lo, hi) => (num ? (lo != null && v < lo ? lo : hi != null && v > hi ? hi : null) : null);
    // just inside a bound it may not touch: a whole step below a whole bound over 2 (cutoff under 131 → 130),
    // otherwise a hundredth (res under 1 → 0.99)
    const inside = (n, below) => {
      const step = Number.isInteger(n) && Math.abs(n) > 2 ? 1 : 0.01;
      return Number(((below ? n - step : n + step)).toFixed(2));
    };
    let said = "isn't a value Sonic Pi will take here", to = null;
    switch (rule.kind) {
      case "positive":
        if (rule.incl) { said = "can't be less than 0"; to = near(0, null); }
        else { said = "must be more than 0"; to = num && v <= 0 ? inside(0, false) : null; }
        break;
      case "min":
        if (rule.incl) { said = `can't be less than ${rule.min}`; to = near(rule.min, null); }
        else { said = `must be over ${rule.min}`; to = num && v <= rule.min ? inside(rule.min, false) : null; }
        break;
      case "max":
        if (rule.incl) { said = `can't be more than ${rule.max}`; to = near(null, rule.max); }
        else { said = `must be under ${rule.max}`; to = num && v >= rule.max ? inside(rule.max, true) : null; }
        break;
      case "between":
        said = rule.incl ? `must be between ${rule.min} and ${rule.max}` : `must be between ${rule.min} and ${rule.max}, and neither of them`;
        to = rule.incl ? near(rule.min, rule.max)
                       : num && v <= rule.min ? inside(rule.min, false) : num && v >= rule.max ? inside(rule.max, true) : null;
        break;
      case "one_of": {
        const list = rule.options ?? [];
        said = `must be one of ${list.join(", ")}`;
        if (num && list.length) to = list.reduce((a, b) => (Math.abs(b - v) < Math.abs(a - v) ? b : a));
        break;
      }
      case "not":
        said = `can't be ${rule.value}`;
        break;
      case "positive_or":
        said = `must be more than 0, or ${rule.other} to turn it off`;
        to = num && v < 0 && v !== rule.other ? rule.other : null;
        break;
      case "sum_max":
        said = `and \`${rule.other}:\` can't add up to more than ${rule.max}`;
        break;
      case "buffer_like":
        said = "needs a sample's name, like `:loop_amen`";
        break;
    }
    // the value as written, when it is a plain number: then it can be set right in one tap, and it is quoted back
    // as it was typed (the tempo-scaled opts reach the runtime as floats: -1 should not come back as -1.0)
    const lit = new RegExp(`\\b${opt}:\\s*(-?[\\d.]+)(?![\\w.])`).exec(text);
    if (lit && num && Number(lit[1]) === v) got = lit[1];
    return {
      headline: `\`${opt}:\` ${said}, but here it's \`${got}\`.`,
      line: at,
      ...(lit ? { from: lit.index, to: lit.index + lit[0].length }
              : (() => { const [from, t] = wordAt(text, `${opt}:`) ?? [indentOf(text).length, text.trimEnd().length]; return { from, to: t }; })()),
      ...(lit && to != null ? { fix: { line: at, from: lit.index + lit[0].length - lit[1].length, to: lit.index + lit[0].length, insert: String(to), label: `Use \`${opt}: ${to}\`` } } : {}),
    };
  }

  if ((m = /Unknown synth :(\w+)/.exec(raw))) return unknownName("synth", known.synths, m[1], "Synths tab");
  if ((m = /Unknown FX :(\w+)/i.exec(raw))) return unknownName("FX", known.fx, m[1], "FX tab");
  if ((m = /Unknown chord name: :?(\w+)/.exec(raw))) return unknownName("chord", known.chords, m[1], "`chord` page");
  if ((m = /Unknown scale name: :?(\w+)/.exec(raw))) return unknownName("scale", known.scales, m[1], "`scale` page");
  if ((m = /Invalid note: :?(\S+)/.exec(raw))) {
    return out(`\`:${m[1].replace(/^:/, "")}\` isn't a note Sonic Pi knows.`, `:${m[1].replace(/^:/, "")}`, {
      hint: "A note is a letter from `a` to `g`, then `s` for sharp or `b` for flat if it needs one, then its octave: `:c4`, `:fs3`, `:eb5`.",
    });
  }
  if ((m = /wrong number of arguments \(given (\d+), expected (\d+)(\+|\.\.\d+)?\)/.exec(raw))) {
    const [given, want] = [Number(m[1]), Number(m[2])];
    // a function of the code's own: the error names its define; the call is the line that gave it the wrong count
    const own = /^\s*define\s+:(\w+)/.exec(L)?.[1];
    if (own) {
      const call = lines.findIndex((s, i) => i !== line - 1 && new RegExp(`^\\s*${own}\\b`).test(s)) + 1;
      const C = lines[call - 1] ?? L;
      const span = wordAt(C, own) ?? [0, 0];
      return {
        headline: `\`${own}\` takes ${want === 0 ? "no values" : plural(want, "value")}, but it was given ${given}.`,
        hint: want === 0 ? `To give it values, name them after its \`do\`: \`define :${own} do |n|\`.` : `Its \`|…|\` after \`define :${own} do\` names each value it takes.`,
        line: call || line, from: span[0], to: span[1],
      };
    }
    const fn = firstFn;
    const ex = example(known, fn);
    const needs = m[3] === "+" ? `at least ${plural(want, "value")}` : plural(want, "value");
    return out(fn ? `\`${fn}\` needs ${needs}, but it was given ${given}.` : `This needs ${needs}, but it was given ${given}.`, fn, {
      ...(ex ? { example: ex } : {}),
    });
  }
  if ((m = /(\w+) does not work with a do\/end block\. Perhaps you meant (\w+)/.exec(raw))) {
    return out(`\`${m[1]}\` doesn't take a \`do\`/\`end\` block. For a block, use \`${m[2]}\`.`, m[1], {
      fix: replace(m[1], m[2], `Change \`${m[1]}\` to \`${m[2]}\``),
      example: example(known, m[2]),
    });
  }
  if (/String cannot be converted to|no implicit conversion of String/.test(raw)) {
    return out("Text in quotes was used where a number is needed.", null, { hint: "Numbers go without quotes: `0.5`, not `\"0.5\"`." });
  }
  if (/nil can't be coerced|nil cannot be converted|no implicit conversion (of|from) nil/.test(raw)) {
    return out("Something here has no value (it's `nil`) where a number is needed.", null, {
      hint: "A variable that was never given a value, or a function that gives nothing back, is `nil`.",
    });
  }
  if (/ZeroTimeLoopError|did not sleep or sync/.test(cls + raw)) {
    const name = /^live_loop_(\w+)$/.exec(thread ?? "")?.[1] ?? /:(\w+)/.exec(thread ?? "")?.[1];
    const start = name ? lines.findIndex((s) => new RegExp(`\\blive_loop\\s+:${name}\\b`).test(s)) + 1 : 0;
    let fix;
    if (start) {
      const ind = indentOf(lines[start - 1]);
      for (let n = start + 1; n <= lines.length; n++) {
        if (indentOf(lines[n - 1]) === ind && lines[n - 1].trim() === "end") { fix = { line: n, from: 0, to: 0, insert: `${ind}  sleep 1\n`, label: `Add \`sleep 1\` before line ${n}'s \`end\`` }; break; }
      }
    }
    return {
      headline: "All live loops must call `sleep` or `sync` otherwise they spin wildly out of control.",
      hint: `The live loop${name ? ` \`:${name}\`` : ""} never sleeps or syncs.`,
      line: start || line, from: 0, to: 0, fix,
      example: example(known, "live_loop"),
    };
  }
  const said = raw.replace(/^[A-Z]\w*(::\w+)*:\s*/, "");
  return out(said.charAt(0).toUpperCase() + said.slice(1), null);
}

export function explainError({ syntax, cls, message, line, col, code, thread, fault }, known) {
  const reason = syntax ? String(message) : `${cls ? `${cls}: ` : ""}${message}`;
  if (cls === "LinkError") return { ...explainLink(String(message)), reason };
  try {
    const e = syntax ? explainSyntax(String(message), code ?? "", line, col, known) : explainRuntime(cls ?? "", String(message), code ?? "", line, thread, known, fault);
    return { ...e, reason };
  } catch {
    return { headline: String(message), line, from: 0, to: 0, reason };
  }
}

// SPDX-License-Identifier: AGPL-3.0-or-later
// Where the caret is, in words: a port of native Sonic Pi's
// app/gui/utils/completion_context.cpp (v5 GUI), function for function, so
// the web editor reads a line the way the native one does. Pinned by
// app/test/completion-context.test.mjs, ported from native's gui-tests.

const isLetter = (c) => /\p{L}/u.test(c);
const isLetterOrNumber = (c) => /[\p{L}\p{N}]/u.test(c);

/** What the text before the caret is inside: a string, a comment, brackets. */
export function scanLineToCaret(line, caretCol) {
  const s = { inComment: false, inString: false, quote: null, openQuoteCol: -1, bracketDepth: 0 };
  let end = caretCol < 0 ? 0 : caretCol;
  if (end > line.length) end = line.length;
  for (let i = 0; i < end; ++i) {
    const c = line[i];
    if (s.inString) {
      if (c === "\\") { ++i; continue; }
      if (c === s.quote) { s.inString = false; s.quote = null; s.openQuoteCol = -1; }
      continue;
    }
    if (c === "#") { s.inComment = true; break; }
    if (c === '"' || c === "'") { s.inString = true; s.quote = c; s.openQuoteCol = i; continue; }
    if (c === "(" || c === "[" || c === "{") { ++s.bracketDepth; continue; }
    if (c === ")" || c === "]" || c === "}") { if (s.bracketDepth > 0) --s.bracketDepth; }
  }
  return s;
}

function isTokenSeparator(c) {
  return c === " " || c === "\t" || c === "\n" || c === "\r" || c === "," ||
         c === "(" || c === ")" || c === "{" || c === "}" ||
         c === "[" || c === "]" || c === '"' || c === "'" || c === "#";
}

/** The end of the token the caret is in: `lpf: 7|0` completes 70, not 7. */
export function tokenEndAtCaret(line, caretCol) {
  let end = caretCol < 0 ? 0 : caretCol;
  if (end > line.length) end = line.length;
  while (end < line.length && !isTokenSeparator(line[end])) ++end;
  return end;
}

const MODIFIERS = new Set(["if", "unless", "while", "until", "and", "or", "then", "do"]);

/**
 * The call the caret is in, as tokens: the function and its arguments so far,
 * the last token being the partial under the caret.
 */
export function lineToContext(fullLine, caretCol) {
  let line = fullLine.slice(0, tokenEndAtCaret(fullLine, caretCol));
  // A trailing statement modifier or operator ends the call's argument list:
  // text after it is a fresh expression.
  {
    let depth = 0, cut = -1, quote = null;
    for (let i = 0; i < line.length; ++i) {
      const c = line[i];
      if (quote !== null) { if (c === quote) quote = null; continue; }
      if (c === '"' || c === "'") { quote = c; continue; }
      if (c === "(" || c === "[" || c === "{") { ++depth; continue; }
      if (c === ")" || c === "]" || c === "}") { if (depth > 0) --depth; continue; }
      if (depth !== 0) continue;
      if (c === ";") { cut = i + 1; continue; }
      if ((c === "&" && line[i + 1] === "&") || (c === "|" && line[i + 1] === "|")) { cut = i + 2; ++i; continue; }
      const startsWord = (isLetter(c) || c === "_") && (i === 0 || !(isLetterOrNumber(line[i - 1]) || line[i - 1] === "_"));
      if (!startsWord) continue;
      let j = i;
      while (j < line.length && (isLetterOrNumber(line[j]) || line[j] === "_")) ++j;
      if (MODIFIERS.has(line.slice(i, j))) cut = j;
      i = j - 1;
    }
    if (cut >= 0) {
      while (cut < line.length && line[cut] === " ") ++cut;
      line = line.slice(cut);
    }
  }
  // Nested calls resolve to the innermost: `play (scale ` completes scale's args.
  const open = [];
  for (let i = 0; i < line.length; ++i) {
    const c = line[i];
    if (c === "(" || c === "[" || c === "{") open.push(i);
    else if ((c === ")" || c === "]" || c === "}") && open.length) open.pop();
  }
  if (open.length) {
    const innermost = open[open.length - 1];
    let s = innermost;
    while (s > 0 && (isLetterOrNumber(line[s - 1]) || line[s - 1] === "_")) --s;
    const fn = line.slice(s, innermost);
    line = line.slice(innermost + 1);
    if (fn) line = fn + " " + line;
  }
  // Split on spaces, commas and brackets, except inside a string, which stays
  // one token. A run of separators yields one split.
  const out = [];
  let cur = "", quote = null, inSeparators = false;
  for (let i = 0; i < line.length; ++i) {
    const c = line[i];
    if (quote !== null) {
      cur += c;
      if (c === "\\" && i + 1 < line.length) { cur += line[++i]; continue; }
      if (c === quote) quote = null;
      continue;
    }
    if (c === " " || c === "," || c === "(" || c === ")" || c === "{" || c === "}") {
      if (!inSeparators) { out.push(cur); cur = ""; inSeparators = true; }
      continue;
    }
    inSeparators = false;
    if (c === '"' || c === "'") quote = c;
    cur += c;
  }
  out.push(cur);
  return out;
}

const wordsBeforePartial = (context) => context.slice(0, -1).filter((w) => w !== "");

/** What kind of value the caret's argument slot takes, from the arg-kinds table. */
export function resolveArgKind(context, table) {
  if (!context.length) return "None";
  const words = wordsBeforePartial(context);
  let argIndex = 0;
  for (let i = words.length - 1; i >= 0; --i) {
    if (words[i].endsWith(":")) return "None";
    const kinds = table[words[i]];
    if (kinds) return argIndex < kinds.length ? kinds[argIndex] : "None";
    ++argIndex;
  }
  return "None";
}

const isOptKey = (w) => w.length > 1 && w.endsWith(":") && !w.startsWith(":");

/** The documented opts of the function the caret is in, once past its first argument. */
export function resolveFnOpts(context, table) {
  if (!context.length) return [];
  const words = wordsBeforePartial(context);
  if (!words.length) return [];
  if (isOptKey(words[words.length - 1])) return [];
  for (let i = words.length - 1; i >= 0; --i) {
    const opts = table[words[i]];
    if (opts) return i < words.length - 1 ? opts : [];
  }
  return [];
}

/** True where another opt key goes: an opt has been given and its value too. */
export function atOptKeySlot(context) {
  let seen = false, last = "";
  for (let i = 0; i < context.length - 1; ++i) {
    if (!context[i]) continue;
    if (isOptKey(context[i])) seen = true;
    last = context[i];
  }
  return seen && !isOptKey(last);
}

/** The caret sits right after a closing bracket or quote: no completion. */
export function caretAfterClosedValue(line, caretCol) {
  let i = caretCol < 0 ? 0 : caretCol;
  if (i > line.length) i = line.length;
  if (i === 0) return false;
  const prev = line[i - 1];
  return prev === ")" || prev === "]" || prev === "}" || prev === '"' || prev === "'";
}

/**
 * Fuzzy subsequence match and rank: every char of `pat` in `text`, in order,
 * case-insensitive. Tiers: exact > prefix > substring (word boundary > mid
 * word) > scattered, shorter winning ties. Returns the score, or null.
 */
export function fuzzyMatch(pat, text) {
  if (!pat) return 0;
  const lower = (c) => c.toLowerCase();
  const isBoundary = (i) => {
    if (i === 0) return true;
    const c = text[i - 1];
    return c === ":" || c === "_" || c === " " || c === "-" || c === "/" || c === "." || c === "!" || c === "?";
  };
  let ti = 0, pi = 0, run = 0, base = 0, gaps = 0;
  while (ti < text.length && pi < pat.length) {
    if (lower(text[ti]) === lower(pat[pi])) {
      if (isBoundary(ti) && ti > 0) base += 8;
      ++run;
      base += 1 + run * 3;
      ++pi;
    } else {
      if (pi > 0) ++gaps;
      run = 0;
    }
    ++ti;
  }
  if (pi !== pat.length) return null;
  base -= gaps * 2;
  const t = text.toLowerCase(), p = pat.toLowerCase();
  let tier = 0;
  if (t === p) tier = 1000;
  else if (t.startsWith(p)) tier = 600;
  else {
    const idx = t.indexOf(p);
    if (idx >= 0) tier = isBoundary(idx) ? 400 : 250;
  }
  return tier + base - text.length;
}

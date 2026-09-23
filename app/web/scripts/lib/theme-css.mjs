// SPDX-License-Identifier: AGPL-3.0-or-later
// The site's colours as the app's theme tokens: every literal colour in a
// stylesheet becomes the theme variable that plays its part — the pink is
// the accent, the greys the foreground at its depths, the whites the pane
// — so a page keeps its design and takes whichever scheme the app is in.
// White is the pane's colour, except as text on an accent (a button, a
// pink bar), where it is the accent's contrasting text; on the site's dark
// panels it is the pane's again, the panel itself being the foreground.
const TOKENS = {
  deeppink: "HighlightedBackground", "#ff1493": "HighlightedBackground", "#e60f7f": "HighlightedBackground", "#99004a": "HighlightedBackground",
  dodgerblue: "Link", "#1e90ff": "Link", "#43b3e0": "Link", "#43bff0": "Link", "#6a9fb5": "Link", "#fbde2d": "Link", "#003cc7": "Link", "#5d99f3": "NumberForeground",
  darkorange: "KeywordForeground", "#ff8c00": "KeywordForeground", "#f4bf75": "KeywordForeground", "#d28445": "KeywordForeground", "#4f4303": "KeywordForeground",
  "#90a959": "DoubleQuotedStringForeground", "#61ce3c": "DoubleQuotedStringForeground", "#006400": "DoubleQuotedStringForeground",
  "#ac4142": "ErrorBackground", "#75b5aa": "Foreground", "#8f5536": "Foreground",
  "#fff": "PaneBackground", "#ffffff": "PaneBackground", white: "PaneBackground", "#000": "PaneBackground",
  "#5e5e5e": "Foreground", "#303030": "Foreground", "#e6e6e6": "Foreground", "#d4d4d4": "Foreground",
  "#282828": "WindowForeground", "#151515": "WindowForeground", black: "WindowForeground", "#2b2b2b": "WindowForeground", "#3e3e3e": "WindowForeground", "#dcdcdc": "WindowForeground",
  "#535353": "mutedForeground", "#555": "mutedForeground", "#383838": "mutedForeground", "#484848": "mutedForeground",
  "#6e6e6e": "softForeground", "#888": "softForeground", "#999": "ghostForeground", "#aaa": "faintForeground", "#ccc": "faintForeground", "#d0d0d0": "faintForeground",
  "#505050": "CommentForeground", "#6a737d": "CommentForeground", "#808080": "CommentForeground", "#595959": "CommentForeground", gray: "CommentForeground",
  "#fafafa": "raisedSurface", "#f8f8f8": "raisedSurface", "#f4f4f4": "subtleFill", "#f0f0f0": "subtleFill", "#ededed": "subtleFill", "#e8e8e8": "subtleFill", "#1a1a1a": "subtleFill", "#e4e4e4": "WindowBorder",
  "#fff1f9": "accentTint", "#ffdef0": "accentTintStrong",
  "#181818": null,   // a text shadow on the dark panel: none
};
// The site's dark panels (its style4 wrappers and dark boxes: near-black ground, mid-grey boxes, white text) are not
// inverted with the theme — a light panel of mid-grey boxes with black text is what that gives in the dark — but
// become tinted sections: a step off the pane for the panel, a step further for its boxes, the foreground for text.
const PANEL = { "#303030": "subtleFill", "#535353": "raisedSurface", "#fff": "Foreground", "#ffffff": "Foreground", white: "Foreground", "#999": "mutedForeground", "#181818": null };
const ACCENT = /\.button|\.qs-card-header|\.qs-copy|\.qs-glyph|\.qs-run|\.qs-tab|\.qs-disc|\.yt-play|\.course-card-action|(^|[\s,>+~])h1\b|\.copy-button|error_description|\.ic-try/;
const DARK_PANEL = /\.wrapper\.style4|\.dark-box|#nav\b/;

export function themeCSS(css) {
  return css.replace(/([^{}]+)\{([^{}]*)\}/g, (m, sel, body) => {
    if (!/[:]/.test(body)) return m;
    const onPanel = DARK_PANEL.test(sel), onAccent = ACCENT.test(sel) && !onPanel, heading = /(^|[\s,>+~])h[1-6]\b/.test(sel);
    const themed = body
      .replace(/rgba\(\s*255\s*,\s*20\s*,\s*147\s*,\s*([\d.]+)\s*\)/g, (_, a) => `color-mix(in srgb, var(--HighlightedBackground) ${Math.round(a * 100)}%, transparent)`)
      .replace(/rgba\(\s*255\s*,\s*255\s*,\s*255\s*,\s*([\d.]+)\s*\)/g, (_, a) => `color-mix(in srgb, var(--PaneBackground) ${Math.round(a * 100)}%, transparent)`)
      .replace(/rgba\(\s*27\s*,\s*31\s*,\s*35\s*,\s*[\d.]+\s*\)/g, "var(--subtleFill)")
      .replace(/#[0-9a-fA-F]{3,6}\b|\b(?:white|black|deeppink|dodgerblue|darkorange|gray)\b(?!-)/g, (c) => {   // not white-space
        const key = c.toLowerCase();
        if (onPanel && key in PANEL) { const t = PANEL[key]; return t === null ? "transparent" : `var(--${t === "Foreground" && heading ? "WindowForeground" : t})`; }
        if (!(key in TOKENS)) return c;
        const token = TOKENS[key];
        if (token === null) return "transparent";
        if (token === "PaneBackground" && onAccent) return "var(--accentContrastText)";
        return `var(--${token})`;
      });
    return `${sel}{${themed}}`;
  });
}

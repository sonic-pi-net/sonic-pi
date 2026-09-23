// SPDX-License-Identifier: AGPL-3.0-or-later
// The theme a first visit paints with, as CSS: app/src/theme.js's own colours for its Dark and Light schemes, worked
// out here at build time, so a page opens in them rather than blank until the script has fetched themes.json. The
// OS's light or dark mode picks between them, as theme.js does until a scheme is chosen; a chosen theme is put back
// before the first paint by the head of the page itself (app/src/index.html), from what theme.js last applied.
import fs from "node:fs";
import path from "node:path";
import { pathToFileURL } from "node:url";

export async function themeDefaultsCSS(root) {
  const vars = new Map();
  const style = { setProperty: (k, v) => vars.set(k, v), colorScheme: "" };
  const stub = {
    document: { documentElement: { style, dataset: {} }, head: { appendChild() {} }, createElement: () => ({ isConnected: true }) },
    localStorage: { getItem: () => null, setItem() {} },
    location: { href: "http://build/" },
    fetch: async (url) => {
      const rel = String(url).replace(/^http:\/\/build\//, "");
      const f = path.join(root, "web", rel);
      return { json: async () => JSON.parse(fs.readFileSync(f, "utf8")), text: async () => (fs.existsSync(f) ? fs.readFileSync(f, "utf8") : "") };
    },
  };
  const saved = Object.fromEntries(Object.keys(stub).map((k) => [k, Object.getOwnPropertyDescriptor(globalThis, k)]));
  for (const [k, v] of Object.entries(stub)) Object.defineProperty(globalThis, k, { value: v, configurable: true, writable: true });
  try {
    const theme = await import(`${pathToFileURL(path.join(root, "app/src/theme.js")).href}?defaults`);
    await theme.loadThemes("./");
    const snapshot = (scheme) => {
      vars.clear();
      theme.set({ scheme, hue: 0, spread: 0, monochrome: false, invert: false });
      return `color-scheme: ${style.colorScheme}; ${[...vars].map(([k, v]) => `${k}: ${v};`).join(" ")}`;
    };
    const dark = snapshot("dark"), light = snapshot("light");
    await new Promise((r) => setTimeout(r, 20));   // theme.js's doc styles (its own fetch) settle before the stand-ins go
    return `/* The theme a first visit paints with: theme.js's Dark and Light, worked out at build time (scripts/lib/theme-defaults.mjs) */
:root { ${dark} }
@media (prefers-color-scheme: light) { :root { ${light} } }
`;
  } finally {
    for (const [k, d] of Object.entries(saved)) { if (d) Object.defineProperty(globalThis, k, d); else delete globalThis[k]; }
  }
}

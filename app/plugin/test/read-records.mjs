// The page's own reader over the records the plugin sent (target/records.hex): what it makes of each.
import fs from "node:fs";
const web = process.argv[2];
const { decode } = await import(`${web}/web/osc.js`);
const { createRecordReader } = await import(`${web}/web/gui-stream.js`);
const reader = createRecordReader();
for (const hex of fs.readFileSync(process.argv[3], "utf8").split("\n").filter(Boolean)) {
  const bytes = Uint8Array.from(hex.match(/../g).map((h) => parseInt(h, 16)));
  let shown;
  try {
    const m = decode(bytes);
    const r = reader.read(m);
    shown = `${m[0]}  ->  ${r ? JSON.stringify({ kind: r.kind, text: r.text ?? r.message, job: r.job, time: r.time }) : "nothing"}`;
  } catch (e) {
    shown = `could not read: ${e.message}`;
  }
  console.log(shown.slice(0, 200));
}

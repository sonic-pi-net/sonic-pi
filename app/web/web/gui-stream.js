// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
// The runtime's GUI stream (runtime/host/sp_host.c, "The GUI stream"; sent by
// runtime/lib/sonic_pi/scheduler.rb): every record the page shows, as an OSC
// message /sonic-pi/<kind> with the thread's uid, the record's time on the
// engine's clock, its job and its program line (null for none), then the
// kind's own fields. Read back here into the records the views and the checks
// use, with their keys in the order a trace has them. A thread's id and name
// come once, when it starts; every record after carries only its uid.

// An OSC array of keys and values, as the object it was; deep for records
// whose values are objects themselves (a studio setting's opts).
const object = (pairs, deep = false) => {
  const o = {};
  for (let i = 0; i + 1 < (pairs?.length ?? 0); i += 2) {
    o[pairs[i]] = deep && Array.isArray(pairs[i + 1]) ? object(pairs[i + 1], true) : pairs[i + 1];
  }
  return o;
};

const FORGET_AFTER = 30;     // seconds a finished thread's name is kept, for views that linger on it

export function createRecordReader() {
  const threads = new Map();   // uid → { id, name, parent, on, ended }
  const nodes = new Map();     // node → { synth, buf }: what the threads view calls an fx or a sound
  let finished = 0;

  function forget(now) {
    for (const [uid, t] of threads) if (t.ended != null && now - t.ended > FORGET_AFTER) threads.delete(uid);
    finished = 0;
  }

  /** One decoded message, as its record. */
  function read([address, uid, time, job, line, ...f]) {
    const kind = address.slice(10);    // after "/sonic-pi/"
    if (kind === "thread_start") threads.set(uid, { id: f[2], name: f[3] ?? "", parent: f[4], on: null, ended: null });
    const t = threads.get(uid);
    const thread = t?.id ?? "", name = t?.name ?? "";
    const tail = (r) => {
      r.time = time;
      r.job = job;
      if (line != null) r.line = line;
      return r;
    };
    switch (kind) {
      case "synth": {
        const r = { kind, t: f[0], beat: f[1], thread, name, synth: f[2], args: object(f[5]), now: f[3], time, job };
        if (f[4] != null) r.node = f[4];
        if (line != null) r.line = line;
        if (f[6] != null) r.fx = { t: f[6], thread: f[7], synth: f[8] };    // the fx it plays into
        if (f[9]) r.immediate = true;   // a real-time thread's: sent immediately (scheduler.rb audio_time)
        if (f[4] != null) {
          nodes.set(f[4], { synth: f[2], buf: r.args.buf ?? null });
          if (nodes.size > 4000) nodes.delete(nodes.keys().next().value);
        }
        return r;
      }
      case "fx_free": return { kind, t: f[0], thread, name, synth: f[3], of: { t: f[1], thread: f[2], synth: f[3] }, time, job, node: f[4] };
      case "loop_move": {
        // a live loop moved into another with_fx (fx absent: out of any), under a new parent thread
        const r = { kind, t: f[0], beat: f[1], thread, name, loop: f[2] };
        if (f[3] != null) r.fx = { t: f[3], thread: f[4], synth: f[5] };
        r.loopUid = f[6];
        r.parentUid = uid;
        return tail(r);
      }
      case "control": {
        const r = { kind, t: f[0], beat: f[1], thread, name, synth: f[2], of: { t: f[3], thread: f[4] }, args: object(f[6]), time, job, node: f[5] };
        if (f[7]) r.immediate = true;   // a real-time thread's: sent immediately (scheduler.rb audio_time)
        if (line != null) r.line = line;
        return r;
      }
      case "kill": {
        const r = { kind, t: f[0], beat: f[1], thread, name, synth: f[2], of: { t: f[3], thread: f[4] }, time, job, node: f[5] };
        if (f[6]) r.immediate = true;   // a real-time thread's: sent immediately (scheduler.rb audio_time)
        if (line != null) r.line = line;
        return r;
      }
      case "midi": return tail({ kind, t: f[0], beat: f[1], thread, name, path: f[2], args: f[3] });
      case "sample_load": return tail({ kind, t: f[0], beat: f[1], thread, name, path: f[2] });
      case "output": case "log": return tail({ kind, t: f[0], thread, name, text: f[1] });
      case "error": {
        const r = { class: f[0], message: f[1], line, thread, name, kind, time, job };
        if (f[2] != null) {
          // an opt that broke a rule, with the rule as data. One level deep on purpose: the rule is an object, but
          // what it holds (a list of allowed values) is a list and must stay one
          const fault = object(f[2]);
          if (Array.isArray(fault.rule)) fault.rule = object(fault.rule);
          r.fault = fault;
        }
        return r;
      }
      case "thread_start": return tail({ kind: "thread", t: f[0], beat: f[1], thread, name, event: "start", parent: f[4] });
      case "thread_end":
        if (t) t.ended = time;
        if (++finished > 500) forget(time);
        return { kind: "thread", t: f[0], beat: f[1], thread, name, event: "end", line, time, job };
      case "sleep": return tail({ kind, t: f[0], beat: f[1], thread, name, beats: f[2], until: f[3] });
      case "sync":
        if (t) t.on = f[2]?.[0] ?? null;
        return tail({ kind, t: f[0], beat: f[1], thread, name, on: f[2] });
      case "cue": return tail({ kind, t: f[0], beat: f[1], thread, name, address: f[2], val: f[3] });
      case "record": {
        const h = object(f[2], true);
        return tail({ kind: h.kind, t: f[0], beat: f[1], thread, name, ...h });
      }
      default: return tail({ kind, t: f[0], beat: f[1], thread, name });
    }
  }

  return {
    read,
    /** A thread's id, name and what it last synced on, by uid; null once forgotten. */
    thread: (uid) => threads.get(uid) ?? null,
    /** A node's synth and sample buffer, from the record that started it; null once forgotten. */
    node: (node) => nodes.get(node) ?? null,
  };
}

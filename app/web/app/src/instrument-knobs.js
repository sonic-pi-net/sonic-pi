// SPDX-License-Identifier: AGPL-3.0-or-later
// The core of every synth's Basic line (docs.js createInstrument), in the order a synth panel reads: what it plays, how
// long, how loud. A synth's own two beside them are its metadata's (gui.basic): native's SynthInfo GUI_BASIC for the
// built-ins, the .json beside its .scsyndef for a user's.
export const CORE = ["note", "release", "amp"];

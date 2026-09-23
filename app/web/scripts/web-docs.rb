# SPDX-License-Identifier: AGPL-3.0-or-later
# The web's own docstrings, where a function works differently in the browser from native Sonic Pi's (no files, a
# preference named differently): each replaces native's in the docs pane and the completion popup alike
# (gen-editor-data.rb merges them over native's before either is written). Only what differs is given; the rest of
# a doc (introduced, opts) stays native's.

EXTERNAL_SYNTH_NOTES = <<~MD
  ## Important notes

  The file must be a compiled SuperCollider synth design with the extension `.scsyndef`, somewhere the web can
  reach: the browser has no files to load from. A raw GitHub link works well. A site that does not allow its files
  to be loaded from other sites (cross-origin requests) cannot be used; the error says so.

  The synthdef is named after its file, as SuperCollider's `writeDefFile` names it: `whoosh.scsyndef` holds the
  synth `:whoosh`.

  You may not trigger external synthdefs unless you enable the following preference:

  ```
  Preferences -> Synths and FX -> Enable external synths
  ```

  or say `use_external_synths true` in the code.

  If you wish your synth to work with Sonic Pi's automatic stereo sound infrastructure *you need to ensure your
  synth outputs a stereo signal* to an audio bus with an index specified by a synth arg named `out_bus`. Also,
  Sonic Pi makes no automatic attempt to free a synth once triggered, so to behave like the built-in synths, your
  synth needs to automatically free itself. For example, the following synth would work nicely:

      (
      SynthDef(\\piTest,
               {|freq = 200, amp = 1, out_bus = 0 |
                 Out.ar(out_bus,
                        SinOsc.ar([freq,freq],0,0.5)* Line.kr(1, 0, 5, amp, doneAction: 2))}
      ).writeDefFile("/Users/sam/Desktop/")
      )

  An external synth is played by its own name with its opts exactly as given: it needs no `note:` (the example
  above takes `freq:`), and none of the built-in synths' defaults or checks apply.
MD

WEB_DOCS = {
  live_audio: {
    doc_note: "On the web the sound card is the browser's audio input: the first time a program uses `live_audio` the " \
              "browser asks to use the microphone (or line in), and remembers the answer for this site. The browser's " \
              "voice processing (echo cancellation, noise suppression, auto gain) is off, so the input is as it arrives. " \
              "Use headphones: a microphone near the speakers will feed back.",
  },
  load_synthdef: {
    summary: "Load a single external synthdef from a URL",
    usage_example: "load_synthdef \"https://example.com/whoosh.scsyndef\"",
    args: [[:url, :string]],
    doc: "Load a pre-compiled synth design from the given URL. This is useful if you wish to use your own " \
         "SuperCollider synthesiser designs within Sonic Pi.\n\n#{EXTERNAL_SYNTH_NOTES}",
    examples: [
      "load_synthdef \"https://example.com/whoosh.scsyndef\" # Load the whoosh synthdef design.\nuse_external_synths true\nsynth :whoosh, freq: 300              # and play it",
    ],
  },
  load_synthdefs: {
    summary: "Load external synthdefs from URLs",
    usage_example: "load_synthdefs [\"https://example.com/whoosh.scsyndef\",\n                \"https://example.com/pad.scsyndef\"]",
    args: [[:urls, :list]],
    doc: "Load several pre-compiled synth designs, one from each URL in the list. This is useful if you wish to use " \
         "your own SuperCollider synthesiser designs within Sonic Pi. A web address cannot be listed like a folder, " \
         "so each synthdef is named.\n\n#{EXTERNAL_SYNTH_NOTES}",
    examples: [
      "load_synthdefs [\"https://example.com/whoosh.scsyndef\",\n                \"https://example.com/pad.scsyndef\"]\nuse_external_synths true\nsynth :pad, freq: 200",
    ],
  },
  use_external_synths: {
    name: :use_external_synths,
    introduced: SonicPi::Version.new(3, 0, 0),
    summary: "Allow external synths",
    usage_example: "use_external_synths true",
    args: [[:allow, :boolean]],
    opts: nil,
    accepts_block: false,
    doc: "Allows synths the built-in set does not have to be played: synthdefs of your own, loaded with " \
         "`load_synthdef`. The Preferences' *Synths and FX -> Enable external synths* turns this on for every run.",
    examples: ["load_synthdef \"https://example.com/whoosh.scsyndef\"\nuse_external_synths true\nsynth :whoosh, freq: 300"],
  },
  with_external_synths: {
    name: :with_external_synths,
    introduced: SonicPi::Version.new(3, 0, 0),
    summary: "Block-level allowing of external synths",
    usage_example: "with_external_synths true do\n  synth :whoosh\nend",
    args: [[:allow, :boolean]],
    opts: nil,
    accepts_block: true,
    doc: "As `use_external_synths`, only for the code within the do/end block.",
    examples: ["with_external_synths true do\n  synth :whoosh, freq: 300\nend"],
  },
}

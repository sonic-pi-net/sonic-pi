# License

## Main Source Code

(contents of [app/](https://github.com/sonic-pi-net/sonic-pi/tree/stable/app) directory)

The MIT License (MIT)

Copyright (c) 2012 - 2026 Samuel Aaron and contributors (sam@sonic-pi.net)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

## GPL Compliance

As Sonic Pi links with and contains GPLv3-licensed software, distributors of
Sonic Pi GUI binaries must comply with the terms of the GPL.

## Samples

(contents of [etc/samples/](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/samples))

All the bundled samples are individually licensed under a
[CC0 1.0 Universal Public Domain Dedication](http://creativecommons.org/publicdomain/zero/1.0/). They
have been obtained from [freesound.org](http://freesound.org) and
links to their sources can be found in the file
[etc/samples/README.md](https://github.com/sonic-pi-net/sonic-pi/blob/main/etc/samples/README.md)

Two exceptions: the samples prefixed with `arovane_` have been kindly donated
by Uwe Zahn ([Arovane](https://arovane.bandcamp.com)), and those prefixed
with `tbd_` by [The Black Dog](https://theblackdogma.com) — both also under a
CC0 license. Thanks Uwe and TBD!

[http://creativecommons.org/publicdomain/zero/1.0/](http://creativecommons.org/publicdomain/zero/1.0/)

## Wavetables

(contents of [etc/wavetables/](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/wavetables)

All the bundled wavetable samples are individually licensed under a
[CC0 1.0 Universal Public Domain Dedication](http://creativecommons.org/publicdomain/zero/1.0/). They
have been obtained from the
[Adventure Kid](http://www.adventurekid.se/akrt/waveforms/adventure-kid-waveforms/)
site.

## Font

The font used in the GUI is [Hack](http://sourcefoundry.org/hack/) released under the [Hack Open Font License v2.0](https://github.com/chrissimpkins/Hack/blob/master/LICENSE.md)

## Icons

Some GUI icons are from [Tabler Icons](https://tabler.io/icons) released under the [MIT License](http://opensource.org/licenses/MIT) (Copyright (c) 2020-2026 Paweł Kuna). See [app/gui/widgets/Tabler-Icons-License.md](app/gui/widgets/Tabler-Icons-License.md).

## Docs, Tutorial and Examples

(contents of [etc/doc/](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/doc) and [etc/examples](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/examples) directories)

All the examples (in
[etc/examples](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/examples))
and contents of the doc directory including the articles and the
tutorial are copyright by Sam Aaron unless a specific author is stated
with the comment `# coded by ...` in which case the copyright is
associated with that author (2015) and the content is released under the
CC BY-SA 4.0 license:

[http://creativecommons.org/licenses/by-sa/4.0/](http://creativecommons.org/licenses/by-sa/4.0/)

## Synth Designs

(contents of [etc/synthdefs/](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/synthdefs) directory)

The bundled synth designs (synthdefs) are licensed under the [MIT License](http://opensource.org/licenses/MIT) with the following exceptions, which are licensed under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.html):

- [etc/synthdefs/designs/supercollider/bass_foundation.scd](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/synthdefs/designs/supercollider/bass_foundation.scd)
- [etc/synthdefs/designs/supercollider/bass_highend.scd](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/synthdefs/designs/supercollider/bass_highend.scd)
- [etc/synthdefs/designs/supercollider/winwood_lead.scd](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/synthdefs/designs/supercollider/winwood_lead.scd)
- [etc/synthdefs/designs/supercollider/organ_tonewheel.scd](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/synthdefs/designs/supercollider/organ_tonewheel.scd)
- [etc/synthdefs/designs/supercollider/autotuner.sc](https://github.com/sonic-pi-net/sonic-pi/tree/stable/etc/synthdefs/designs/supercollider/autotuner.sc) (derived from [PitchShiftPA](https://github.com/dyfer/PitchShiftPA), GPL-3.0)

The sc808 drum synths are adapted from [Yoshinosuke Horiuchi's SC-808](https://www.patreon.com/4H/posts),
released free of charge with the author's published permission for free use;
the original carries no formal licence text. The adaptations retain
attribution in their source headers.

The Overtone designs under `designs/overtone/` are licensed under the
Eclipse Public License 2.0 (see the LICENSE file in that directory).

See the source files for links to the original designs.

## Bundled Software

The following is a list of the software included in Sonic Pi with their
relevant licenses:

### Standalone bundled programs (separate processes, not linked to the GUI)

- [Ruby](http://ruby-lang.org) 4.0 - dual [Ruby License](https://www.ruby-lang.org/en/about/license.txt) / [BSD 2-Clause](https://opensource.org/licenses/BSD-2-Clause)
- [SuperSonic](https://github.com/sonic-pi-net/supersonic) - SuperSonic's own code is [MIT](http://opensource.org/licenses/MIT) OR [GPL-3.0-or-later](https://www.gnu.org/licenses/gpl-3.0.html); it contains the [SuperCollider](https://supercollider.github.io)-derived scsynth engine core ([GPL-2.0-or-later](https://www.gnu.org/licenses/gpl-2.0.html), Copyright (c) James McCartney), so the combined binary is distributed under GPL terms. Its full component list is below.
- aubio_onset (built from [aubio](http://aubio.org) 0.4.9) - [GPL-3.0-or-later](https://www.gnu.org/licenses/gpl-3.0.html); statically includes [libsndfile](https://libsndfile.github.io/libsndfile/) ([LGPL-2.1](https://www.gnu.org/licenses/lgpl-2.1.html)) and [FLAC](https://github.com/xiph/flac), [ogg](https://github.com/xiph/ogg), [vorbis](https://github.com/xiph/vorbis), [opus](https://github.com/xiph/opus) (all [BSD 3-Clause](http://opensource.org/licenses/BSD-3-Clause))

### Compiled into or shipped with the GUI

- [Qt](http://qt-project.org) 6 - [GNU Lesser General Public License v3](https://www.gnu.org/licenses/lgpl-3.0.html) (dynamically linked frameworks)
- [QScintilla](https://www.riverbankcomputing.com/software/qscintilla/intro) 2.14.1 (vendored Sonic Pi fork, statically linked) - [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.html)
- [Scintilla](https://www.scintilla.org) (within QScintilla) - [Scintilla License](https://www.scintilla.org/License.txt) (permissive)
- [OpenSSL](https://www.openssl.org) 3 - [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (dynamic libraries, used by the bundled Ruby)
- [KISS FFT](https://github.com/mborgerding/kissfft) - [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause) (dynamic library)
- [ghc_filesystem](https://github.com/gulrak/filesystem) - [MIT License](http://opensource.org/licenses/MIT)
- [kissnet](https://github.com/Ybalrid/kissnet) - [MIT License](http://opensource.org/licenses/MIT)
- [oscpkt](http://gruntthepeon.free.fr/oscpkt/) - [zlib License](https://opensource.org/licenses/Zlib)
- [PlatformFolders](https://github.com/sago007/PlatformFolders) - [MIT License](http://opensource.org/licenses/MIT) (Windows only)
- [reproc](https://github.com/DaanDeMeyer/reproc) - [MIT License](http://opensource.org/licenses/MIT)
- [TLSF](http://www.gii.upv.es/tlsf/) 2.4.6 - dual [GPL-2.0](https://www.gnu.org/licenses/gpl-2.0.html) / [LGPL-2.1](https://www.gnu.org/licenses/lgpl-2.1.html) with an explicit linking exception
- [Spout2](https://github.com/leadedge/Spout2) - [BSD 2-Clause License](https://opensource.org/licenses/BSD-2-Clause) (Windows only)
- [Syphon](https://github.com/Syphon/Syphon-Framework) - [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause) (macOS only)

### Compiled into SuperSonic

- SuperSmoothy (SuperSonic's audio-device layer, a vendored fork of the permissively-licensed modules of [JUCE](https://juce.com) 7.0.12) - [ISC License](https://opensource.org/licenses/ISC); includes [zlib](https://zlib.net) ([zlib License](https://opensource.org/licenses/Zlib))
- [SuperCollider](https://supercollider.github.io) scsynth-derived engine core (incl. nova-simd) - [GPL-2.0-or-later](https://www.gnu.org/licenses/gpl-2.0.html)
- MdaPiano synth + piano wavetable data (Paul Kellett / Dan Stowell, from the [mda plug-ins](https://sourceforge.net/projects/mda-vst/)) - dual [MIT](http://opensource.org/licenses/MIT) / [GPL-2.0-or-later](https://www.gnu.org/licenses/gpl-2.0.html)
- [libsndfile](https://libsndfile.github.io/libsndfile/) - [GNU Lesser General Public License v2.1](https://www.gnu.org/licenses/lgpl-2.1.html)
- [FLAC](https://github.com/xiph/flac), [ogg](https://github.com/xiph/ogg), [vorbis](https://github.com/xiph/vorbis), [opus](https://github.com/xiph/opus) - [BSD 3-Clause License](http://opensource.org/licenses/BSD-3-Clause)
- [Ableton Link](https://github.com/Ableton/link) (with local patches, conveyed in the SuperSonic repo) - [GPL-2.0-or-later](https://www.gnu.org/licenses/gpl-2.0.html)
- [asio (standalone)](https://think-async.com/Asio/) (vendored within Link) - [Boost Software License 1.0](https://opensource.org/licenses/BSL-1.0)
- [boost (subset)](https://www.boost.org) 1.86 - [Boost Software License 1.0 (BSL-1.0)](https://opensource.org/licenses/BSL-1.0)
- [TLSF](http://www.gii.upv.es/tlsf/) 2.4.6 - dual [GPL-2.0](https://www.gnu.org/licenses/gpl-2.0.html) / [LGPL-2.1](https://www.gnu.org/licenses/lgpl-2.1.html)
- [oscpack](http://www.rossbencina.com/code/oscpack) - permissive (MIT-style) licence
- Rust subsystems (MIDI / gamepad / OSC networking): [midir](https://github.com/Boddlnagg/midir) fork ([MIT](http://opensource.org/licenses/MIT)) plus permissively-licensed crates (MIT / Apache-2.0 / BSD / ISC / Zlib), enforced by a cargo-deny licence allowlist
- [Steinberg ASIO SDK](https://www.steinberg.net/developers/) 2.3.4 - used under its [GPL-3.0](https://www.gnu.org/licenses/gpl-3.0.html) option (Windows only)

### Included Ruby Libraries for Spider Language Server

(contents of [app/server/ruby/vendor/](https://github.com/sonic-pi-net/sonic-pi/tree/stable/app/server/ruby/vendor))

- [Blankslate](https://github.com/masover/blankslate) - [MIT License](http://opensource.org/licenses/MIT)
- [Concurrent Ruby](https://github.com/ruby-concurrency/concurrent-ruby) - [MIT License](http://opensource.org/licenses/MIT)
- [gettext](https://github.com/ruby-gettext/gettext) - dual [Ruby License](https://www.ruby-lang.org/en/about/license.txt) / [LGPL-3.0-or-later](https://www.gnu.org/licenses/lgpl-3.0.html)
- [i18n](https://github.com/svenfuchs/i18n) - [MIT License](http://opensource.org/licenses/MIT)
- [Kramdown](http://kramdown.gettalong.org) - [MIT License](http://opensource.org/licenses/MIT)
- [Locale](https://github.com/ruby-gettext/locale) - dual [Ruby License](https://www.ruby-lang.org/en/about/license.txt) / [GPL-2.0](https://www.gnu.org/licenses/gpl-2.0.html)
- [Memoist](https://github.com/matthewrudy/memoist) - [MIT License](http://opensource.org/licenses/MIT)
- [Metaclass](https://github.com/floehopper/metaclass) - [MIT License](http://opensource.org/licenses/MIT)
- [MiniTest](https://github.com/minitest/minitest) - [MIT License](http://opensource.org/licenses/MIT)
- [Mocha](http://gofreerange.com/mocha/docs/) - dual [Ruby License](https://www.ruby-lang.org/en/about/license.txt) / [MIT License](http://opensource.org/licenses/MIT)
- [Multi JSON](https://github.com/intridea/multi_json) - [MIT License](http://opensource.org/licenses/MIT)
- [Rouge](https://github.com/jneen/rouge) - [MIT License](http://opensource.org/licenses/MIT)
- [Ruby Beautify](https://github.com/erniebrodeur/ruby-beautify) - [MIT License](http://opensource.org/licenses/MIT)
- [Rugged](https://github.com/libgit2/rugged) - [MIT License](http://opensource.org/licenses/MIT); statically bundles [libgit2](https://libgit2.org) ([GPL-2.0 with linking exception](https://github.com/libgit2/libgit2/blob/main/COPYING))
- [Text](https://github.com/threedaymonk/text) - [MIT License](http://opensource.org/licenses/MIT)
- [Titleize](https://github.com/greatseth/titleize) - [MIT License](http://opensource.org/licenses/MIT)
- [Tomlrb](https://github.com/fbernier/tomlrb) - [MIT License](http://opensource.org/licenses/MIT)
- [TZ Info](https://github.com/tzinfo/tzinfo) - [MIT License](http://opensource.org/licenses/MIT)

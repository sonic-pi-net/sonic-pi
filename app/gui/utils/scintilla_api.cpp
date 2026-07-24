//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++


#include <QDir>
#include <iostream>
#include <cmath>
#include <QRegularExpression>
#include "scintilla_api.h"
#include "completion_context.h"
#include "completion_argkinds.gen.h"

using namespace std;

namespace {
// The word immediately before the partial being typed (skips empty tokens).
QString lastWordBeforePartial(const QStringList& context) {
    for (int i = context.size() - 2; i >= 0; --i)
        if (!context[i].isEmpty()) return context[i];
    return QString();
}

// True when the cursor is in a note slot: the first argument of play / scale /
// chord (their tonic), or a note: opt value (e.g. `synth :saw, note: `).
bool isNoteContext(const QStringList& context) {
    const QString lw = lastWordBeforePartial(context);
    return lw == "play" || lw == "scale" || lw == "chord" || lw == "note:";
}

// Note completions. MIDI NUMBERS come first — no music theory needed to pick a
// pitch — each annotated with its note name and frequency. Then named spellings
// (naturals, sharps, flats: :c4, :cs4, :db4, :eb1, :cb3) for those who think in
// notes. Sonic Pi uses C4 = 60, A4 = 69 = 440 Hz.
// The list is a compile-time constant, so build it once and reuse it (this runs
// on the per-keystroke completion path).
const QList<CompletionItem>& noteCompletions() {
    static const QList<CompletionItem> items = [] {
        // Idiomatic spellings: sharps for C#/F#, flats for Eb/Ab/Bb (the common
        // mix — e.g. "Bb" reads more naturally than "As").
        static const char* kCanon[12] =
            {"c", "cs", "d", "eb", "e", "f", "fs", "g", "ab", "a", "bb", "b"};
        QList<CompletionItem> out;

        // 1) Numbers (priority); the dimmed summary shows the note name.
        for (int midi = 36; midi <= 96; ++midi) {
            CompletionItem it;
            it.text = QString::number(midi);
            it.kind = "note";
            it.summary = QString(":%1%2").arg(kCanon[midi % 12]).arg(midi / 12 - 1);
            it.note = midi;
            out.append(it);
        }

        // 2) Named spellings.
        struct Spelling { const char* name; int offset; };
        static const Spelling kSpellings[] = {
            {"c", 0},  {"cs", 1},  {"db", 1},
            {"d", 2},  {"ds", 3},  {"eb", 3},
            {"e", 4},
            {"f", 5},  {"fs", 6},  {"gb", 6},
            {"g", 7},  {"gs", 8},  {"ab", 8},
            {"a", 9},  {"as", 10}, {"bb", 10},
            {"b", 11},
            {"cb", -1}, {"es", 5}, {"fb", 4}, {"bs", 12}, // rarer enharmonics
        };
        for (int oct = 2; oct <= 7; ++oct) {
            const int base = (oct + 1) * 12;
            for (const Spelling& sp : kSpellings) {
                const int midi = base + sp.offset;
                if (midi < 36 || midi > 96) continue;   // stay within the keyboard range
                CompletionItem it;
                it.text = QString(":%1%2").arg(sp.name).arg(oct);
                it.kind = "note";
                it.summary = QString::number(midi);
                it.note = midi;
                out.append(it);
            }
        }
        return out;
    }();
    return items;
}
} // namespace

// The ctor.
ScintillaAPI::ScintillaAPI(QsciLexer *lexer)
    : QsciAbstractAPIs(lexer)
{
  // manually managed for now
  keywords[Chord] << "'1'" << "'5'" << "'+5'" << "'m+5'" << ":sus2" << ":sus4" << "'6'" << ":m6" << "'7sus2'" << "'7sus4'" << "'7-5'" << ":halfdiminished" << "'7+5'" << "'m7+5'" << "'9'" << ":m9" << "'m7+9'" << ":maj9" << "'9sus4'" << "'9-5'" << "'6*9'" << "'m6*9'" << "'7-9'" << "'m7-9'" << "'7-10'" << "'7+9'" << "'7-11'" << "'7-13'" << "'9+5'" << "'m9+5'" << "'7+5-9'" << "'m7+5-9'" << "'11'" << ":m11" << ":maj11" << "'11+'" << "'m11+'" << "'13'" << ":m13" << ":maj13" << ":add2" << ":add4" << ":add9" << ":add11" << ":add13" << ":madd2" << ":madd4" << ":madd9" << ":madd11" << ":madd13" << ":major" << ":maj" << ":M" << ":minor" << ":min" << ":m" << ":major7" << ":maj7" << ":dom7" << "'7'" << ":M7" << ":minor7" << ":min7" << ":m7" << ":minor_major7" << "'mM7'" << "'mmaj7'" << ":augmented" << ":a" << ":diminished" << ":dim" << ":i" << ":diminished7" << ":dim7" << ":i7" << ":halfdim" << "'m7b5'" << "'m7-5'";


  keywords[Scale] << ":diatonic" << ":ionian" << ":major" << ":dorian" << ":phrygian" << ":lydian" << ":mixolydian" << ":aeolian" << ":minor" << ":locrian" << ":hex_major6" << ":hex_dorian" << ":hex_phrygian" << ":hex_major7" << ":hex_sus" << ":hex_aeolian" << ":minor_pentatonic" << ":yu" << ":major_pentatonic" << ":gong" << ":egyptian" << ":shang" << ":jiao" << ":zhi" << ":ritusen" << ":whole_tone" << ":whole" << ":chromatic" << ":harmonic_minor" << ":melodic_minor_asc" << ":hungarian_minor" << ":octatonic" << ":messiaen1" << ":messiaen2" << ":messiaen3" << ":messiaen4" << ":messiaen5" << ":messiaen6" << ":messiaen7" << ":super_locrian" << ":hirajoshi" << ":kumoi" << ":neapolitan_major" << ":bartok" << ":bhairav" << ":locrian_major" << ":ahirbhairav" << ":enigmatic" << ":neapolitan_minor" << ":pelog" << ":augmented2" << ":scriabin" << ":harmonic_major" << ":melodic_minor_desc" << ":romanian_minor" << ":hindu" << ":iwato" << ":melodic_minor" << ":diminished2" << ":marva" << ":melodic_major" << ":indian" << ":spanish" << ":prometheus" << ":diminished" << ":todi" << ":leading_whole" << ":augmented" << ":purvi" << ":chinese" << ":lydian_minor" << ":blues_major" << ":blues_minor" << ":lydian_dominant" << ":acoustic" << ":altered" << ":phrygian_dominant" << ":double_harmonic" << ":byzantine" << ":cargah" << ":buselik" << ":buselik_2" << ":kurdi" << ":rast" << ":acemli_rast" << ":ussak" << ":bayati" << ":bayati_2" << ":isfahan" << ":isfahan_2" << ":hicaz_humayun" << ":hicaz_humayun_2" << ":hicaz" << ":hicaz_2" << ":uzzal" << ":uzzal_2" << ":zirguleli_hicaz" << ":zirguleli_hicaz_2" << ":huseyni" << ":huseyni_2" << ":muhayyer" << ":gulizar" << ":neva" << ":neva_2" << ":tahir" << ":tahir_2" << ":karcigar" << ":suznak" << ":suznak_2" << ":mahur" << ":acem_asiran" << ":nihavend" << ":nihavend_2" << ":sultani_yegah" << ":sultani_yegah_2" << ":kurdili_hicazkar" << ":kurdili_hicazkar_2" << ":kurdili_hicazkar_3" << ":kurdili_hicazkar_4" << ":kurdili_hicazkar_5" << ":zirguleli_suznak" << ":zirguleli_suznak_2" << ":zirguleli_suznak_3" << ":hicazkar" << ":hicazkar_2" << ":evcara" << ":evcara_2" << ":evcara_3" << ":evcara_4" << ":suzidil" << ":suzidil_2" << ":sedaraban" << ":sedaraban_2" << ":segah" << ":segah_2" << ":huzzam" << ":huzzam_2" << ":bayati_araban" << ":acem_kurdi" << ":sehnaz" << ":sehnaz_2" << ":sehnaz_3" << ":sehnaz_4" << ":saba" << ":dugah" << ":dugah_2" << ":evic" << ":evic_2" << ":bestenigar" << ":ferahnak" << ":sevkefza" << ":sevkefza_2" << ":sevkefza_3" << ":ferahfeza" << ":ferahfeza_2" << ":yegah" << ":yegah_2";


  // PlayParam and SampleParam are filled from synthinfo.rb at runtime via
  // setPlayArgs()/setSampleArgs() (generated into initDocsWindow), so they
  // stay in sync with the actual synthdefs rather than drifting here.

  keywords[Examples] << ":haunted" << ":ambient_experiment" << ":chord_inversions" << ":filtered_dnb" << ":fm_noise" << ":jungle" << ":ocean" << ":reich_phase" << ":acid" << ":ambient" << ":compus_beats" << ":echo_drama" << ":idm_breakbeat" << ":tron_bike" << ":wob_rhyth" << ":bach" << ":driving_pulse" << ":monday_blues" << ":rerezzed" << ":square_skit" << ":blimp_zones" << ":blip_rhythm" << ":shufflit" << ":tilburg_2" << ":time_machine" << ":sonic_dreams" << ":blockgame" << ":cloud_beat" << ":lorezzed";

  keywords[Tuning] << ":just" << ":pythagorean" << ":meantone" << ":equal";

  keywords[MidiParam] << "sustain:" << "velocity:" << "vel:" << "velocity_f:" << "vel_f:" << "port:" << "channel:";

  keywords[RandomSource] << ":white" << ":light_pink" << ":pink" << ":dark_pink" << ":perlin";
}



void ScintillaAPI::loadSamples(QString sample_path) {
  QDir dir(sample_path);
  QStringList filetypes;
  filetypes << "*.wav" << "*.wave" << "*.aif" << "*.aiff" << "*.flac";
  dir.setNameFilters(filetypes);

  QFileInfoList files = dir.entryInfoList(QDir::Files | QDir::NoDotAndDotDot);
  foreach (QFileInfo file, files) {
    addSymbol(Sample, file.baseName());
  }
}

void ScintillaAPI::addSymbol(int context, QString sym) {
  addKeyword(context, QString(":" + sym));
}

void ScintillaAPI::addKeyword(int context, QString keyword) {
  keywords[context] << keyword;
}

void ScintillaAPI::addFXArgs(QString fx, QStringList args) {
  fxArgs.insert(fx, args);
}

void ScintillaAPI::addSynthArgs(QString fx, QStringList args) {
  synthArgs.insert(fx, args);
}

void ScintillaAPI::addCuePath(QString path) {
  // Cues arrive repeatedly during a session; only keep one entry per path so
  // the sync/cue/get/set completion list doesn't fill with duplicates.
  if (!keywords[CuePath].contains(path))
    keywords[CuePath] << path;
}

void ScintillaAPI::updateMidiOuts(QString port_info) {
  // port info is a \n separated list of MIDI port names. Need to first split it up
  keywords[MidiOuts].clear();

  for ( const auto& i : port_info.split(QRegularExpression("[\r\n]")) )
    {
      keywords[MidiOuts] << QString("\"%1\"").arg(i);
    }
}

void ScintillaAPI::updateLinkAudioStreams(const QStringList& peers, const QStringList& channels) {
  // Quoted peer / channel names announced on the network, for link_audio.
  keywords[LinkAudioPeer] = peers;
  keywords[LinkAudioChannel] = channels;
}

void ScintillaAPI::setPlayArgs(const QStringList& args) {
  keywords[PlayParam] = args;
}

void ScintillaAPI::setSampleArgs(const QStringList& args) {
  keywords[SampleParam] = args;
}

void ScintillaAPI::setSynthResolver(std::function<QString()> resolver) {
  synthResolver = resolver;
}

void ScintillaAPI::setSummary(const QString& name, const QString& summary) {
  summaries.insert(name, summary);
}

void ScintillaAPI::setDoc(const QString& name, const QString& doc) {
  docs.insert(name, doc);
}

void ScintillaAPI::setUsage(const QString& name, const QString& usage) {
  usages.insert(name, usage);
}

void ScintillaAPI::setOptRange(const QString& name, double lo, double hi, double def,
                               bool loExcl, bool hiExcl) {
  // The finest value the popup slider produces (drag rounding grid, see
  // RangeSlider): two decades below the range's magnitude. Exclusive bounds
  // retreat one grid step so the slider's edge is always a valid value.
  const double grid = std::pow(10.0, std::floor(std::log10(hi - lo)) - 2.0);
  if (loExcl)
    lo += grid;
  if (hiExcl)
    hi -= grid;
  optRanges.insert(name, {lo, hi, qBound(lo, def, hi)});
}

void ScintillaAPI::setOptOptions(const QString& name, const QStringList& opts) {
  optOptions.insert(name, opts);
}

void ScintillaAPI::setChordIntervals(const QString& name, const QList<int>& semis) {
  chordIntervals.insert(name, semis);
}

void ScintillaAPI::setScaleIntervals(const QString& name, const QList<int>& semis) {
  scaleIntervals.insert(name, semis);
}

namespace {
// A chord/scale completion entry like ":minor7", "'m7b5'" or ":major" → the bare
// name ("minor7", "m7b5", "major") used to key the interval tables.
QString bareName(const QString& s) {
  QString t = s.trimmed();
  if (t.startsWith(':')) t = t.mid(1);
  if (t.startsWith('\'') && t.endsWith('\'') && t.size() >= 2) t = t.mid(1, t.size() - 2);
  return t;
}

// The next argument token in `after` (line text past the caret): ", :minor7)"
// → ":minor7", ", '7')" → "'7'". Empty when there's no following argument.
QString nextArgToken(const QString& after) {
  static const QRegularExpression re(QStringLiteral("^[\\s,(]*([:'][^\\s,)\\]]*)"));
  const QRegularExpressionMatch m = re.match(after);
  return m.hasMatch() ? m.captured(1) : QString();
}

// Resolve a tonic token (":e3", "Eb4", "60") to a MIDI note, or -1. Reuses the
// note-completion table for names; falls back to a bare integer.
int tonicToMidi(const QString& tok) {
  static const QHash<QString, int> byName = [] {
    QHash<QString, int> m;
    for (const CompletionItem& it : noteCompletions())
      if (it.note >= 0) m.insert(it.text.toLower(), it.note);
    return m;
  }();
  const QString t = tok.trimmed().toLower();
  if (byName.contains(t)) return byName.value(t);
  bool ok = false;
  const int n = t.toInt(&ok);
  return ok ? n : -1;
}

// Pull an enum value's meaning out of the opt's doc prose, e.g. "0 saw, 1 pulse"
// or "0=saw wave, 1=pulse" → "saw" / "saw wave". Empty if it isn't described.
QString enumLabelFor(const QString& doc, const QString& value) {
  if (doc.isEmpty() || value.isEmpty()) return QString();
  const QRegularExpression re(
      QStringLiteral("\\b") + QRegularExpression::escape(value) +
      QStringLiteral("\\b\\s*=?\\s*([A-Za-z][A-Za-z ]*?)(?=[,.;)<]|\\s+\\d|\\s+and\\b|$)"));
  const QRegularExpressionMatch m = re.match(doc);
  return m.hasMatch() ? m.captured(1).trimmed() : QString();
}
} // namespace

QString ScintillaAPI::kindForContext(int ctx) {
  switch (ctx) {
    case FX:              return "fx";
    case Synth:           return "synth";
    case Sample:          return "sample";
    case Chord:           return "chord";
    case Scale:           return "scale";
    case Tuning:          return "tuning";
    case Examples:        return "example";
    case MidiOuts:        return "port";
    case CuePath:         return "cue";
    case LinkAudioPeer:   return "peer";
    case LinkAudioChannel:return "channel";
    case PlayParam:
    case SampleParam:
    case MidiParam:
    case RandomSource:    return "opt";
    default:              return "fn";
  }
}

QList<CompletionItem> ScintillaAPI::completionsFor(const QStringList& context,
                                                   const QString& afterCursor) {
  if (isNoteContext(context)) {
    // Completing the root of a chord/scale and the name argument already follows
    // the caret? Attach its intervals so the keyboard previews the whole chord
    // at each candidate root (the mirror of name-completion knowing the root).
    const QString lw = lastWordBeforePartial(context);
    if (lw == "chord" || lw == "scale") {
      const QList<int>& iv = (lw == "scale" ? scaleIntervals : chordIntervals)
                                 .value(bareName(nextArgToken(afterCursor)));
      if (!iv.isEmpty()) {
        QList<CompletionItem> out = noteCompletions();
        for (CompletionItem& it : out) it.intervals = iv;
        return out;
      }
    }
    return noteCompletions();
  }
  // Opt value slot. An enum opt (e.g. `wave: `) → a choice list of its values; a
  // bounded opt (e.g. `pan: `) → a single slider item.
  const QString optBefore = lastWordBeforePartial(context);
  if (optOptions.contains(optBefore)) {
    // Each value carries its meaning (parsed from the opt docs) inline, and the
    // full opt doc in the detail pane — so `wave: 0` reads "0 saw", not a bare 0.
    const QString optDoc = docs.value(optBefore);
    QString illo;
    if (optBefore == "wave:" || optBefore == "mod_wave:") illo = "wave";
    else if (optBefore.endsWith("env_curve:")) illo = "curve";
    QList<CompletionItem> out;
    for (const QString& v : optOptions.value(optBefore)) {
      CompletionItem it;
      it.kind = "optval";
      it.text = v;
      it.summary = enumLabelFor(optDoc, v);
      it.doc = optDoc;
      it.illo = illo;
      out.append(it);
    }
    return out;
  }
  if (optRanges.contains(optBefore)) {
    const OptRange r = optRanges.value(optBefore);
    CompletionItem it;
    it.kind = "range";
    it.slider = true;
    it.rmin = r.lo;
    it.rmax = r.hi;
    // Start at the value already typed (if numeric), else the opt's default.
    bool ok = false;
    const double typed = context.isEmpty() ? 0.0 : context.last().toDouble(&ok);
    it.rdefault = ok ? typed : r.def;
    it.text = optBefore;     // e.g. "pan:" — for context/labelling
    return { it };
  }
  QStringList names;
  updateAutoCompletionList(context, names);

  // For chord/scale name completion, resolve the tonic (the arg before the name)
  // so the popup can plot the actual notes on the keyboard.
  const bool isChord = (lastKind == "chord");
  const bool isScale = (lastKind == "scale");
  int tonic = 60;   // default to middle C when the tonic can't be resolved
  if (isChord || isScale) {
    const int fnIdx = context.lastIndexOf(isChord ? QStringLiteral("chord")
                                                   : QStringLiteral("scale"));
    for (int i = fnIdx + 1; fnIdx >= 0 && i < context.size() - 1; ++i) {
      if (context[i].isEmpty()) continue;
      const int m = tonicToMidi(context[i]);
      if (m >= 0) { tonic = m; }
      break;   // the first arg after the function name is the tonic
    }
  }

  QList<CompletionItem> items;
  items.reserve(names.size());
  for (const QString& n : names) {
    CompletionItem item;
    item.text = n;
    item.kind = lastKind;
    item.summary = summaries.value(n);
    item.usage = usages.value(n);
    item.doc = docs.value(n);
    if (isChord || isScale) {
      const QList<int>& table = (isChord ? chordIntervals : scaleIntervals).value(bareName(n));
      if (!table.isEmpty()) { item.intervals = table; item.note = tonic; }
    }
    items.append(item);
  }
  return items;
}

void ScintillaAPI::updateAutoCompletionList(const QStringList &context,
					   QStringList &list) {
  if (context.isEmpty()) return;

  lastKind.clear();

  // default
  int ctx = Func;

  QString partial = context.last();
  QStringList words;
  for (int i=0; i<context.length()-1; i++) {
    if (context[i] != "")
      words.append(context[i]);
  }

  QString last = words.isEmpty() ? "" : words.last();
  QString first = words.isEmpty() ? "" : words.first();
  QString second = words.length() < 2 ? "" : words[1];

  /* // debug
  for (int i=0; i<context.length(); i++)
    cout << "context[" << i << "] = " << context[i].toStdString() << endl;
  for (int i=0; i<words.length(); i++)
    cout << "words[" << i << "] = " << words[i].toStdString() << endl;
  cout << "first = " << first.toStdString()
       << ", second = " << second.toStdString()
       << ", last = " << last.toStdString()
       << ", partial = " << partial.toStdString() << endl;
  */

  // Name/value-slot contexts come from the generated arg-kinds table (the language
  // metadata's `arg_kinds:` tags) via resolveArgKind — the single source, so
  // detection can't drift from the language. Opt-VALUE slots (MIDI `port:`, `sync:`)
  // aren't positional args so stay inline, as do the untagged examples/random/
  // tuning. Note slots resolve to Func here and fall through to the note handling.
  static const SonicPi::ArgKindTable s_argKinds = SonicPi::generatedArgKinds();
  switch (SonicPi::resolveArgKind(context, s_argKinds)) {
    case SonicPi::ArgKind::Sample:           ctx = Sample; break;
    case SonicPi::ArgKind::CuePath:          ctx = CuePath; break;
    case SonicPi::ArgKind::Fx:               ctx = FX; break;
    case SonicPi::ArgKind::Synth:            ctx = Synth; break;
    case SonicPi::ArgKind::Scale:            ctx = Scale; break;
    case SonicPi::ArgKind::Chord:            ctx = Chord; break;
    case SonicPi::ArgKind::LinkAudioPeer:    ctx = LinkAudioPeer; break;
    case SonicPi::ArgKind::LinkAudioChannel: ctx = LinkAudioChannel; break;
    default: break;
  }

  if (ctx != Func) {
    // resolved from the metadata table above
  } else if (last == "sync:") {
    ctx = CuePath;
  } else if ((first == "midi" || first.startsWith("midi_") || first == "use_midi_defaults" || first == "with_midi_defaults") && last == "port:") {
    ctx = MidiOuts;
  } else if (last == "load_example") {
    ctx = Examples;
  } else if (last == "use_random_source" || last == "with_random_source") {
    ctx = RandomSource;
  } else if (last == "use_tuning" || last == "with_tuning") {
    ctx = Tuning;

  // FX params
  } else if (words.length() >= 2 &&
             (first == "with_fx")) {
    if (last.endsWith(':')) return; // don't try to complete parameters
    if (fxArgs.contains(second)) {
      lastKind = "opt";
      list = fxArgs[second];
      return;
    }

  // Synth params
  } else if (words.length() >= 2 && first == "synth") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    if (synthArgs.contains(second)) {
      lastKind = "opt";
      list = synthArgs[second];
      return;
    }

  // Play/control params — the opts for the synth currently in effect (set by
  // use_synth, resolved live; defaults to :beep). control modulates a node whose
  // synth may differ from the current one, but opts overlap heavily across synths
  // (amp/note/cutoff/pan/release + slides), so the current synth is a good-enough
  // suggestion source. Falls back to the generic PlayParam list when unknown.
  } else if (words.length() >= 2 && (first == "play" || first == "control")) {
    if (last.endsWith(':')) return; // don't try to complete parameters
    QString synth = synthResolver ? synthResolver() : QString();
    if (!synth.isEmpty()) {
      QString key = synth.startsWith(':') ? synth : (":" + synth);
      if (synthArgs.contains(key)) {
        lastKind = "opt";
        list = synthArgs[key];
        return;
      }
    }
    ctx = PlayParam;

  // Sample params
  } else if (words.length() >= 2 && first == "sample") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = SampleParam;
  } else if (first == "use_sample_defaults" || first == "with_sample_defaults") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = SampleParam;
  } else if (words.length() >= 2 && first == "midi") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = MidiParam;
  } else if (context.length() > 1) {
    if (partial.length() <= 2) {
      // don't attempt to autocomplete other words on the same line
      // unless we have a plausible match
      return;
    }
  }

  lastKind = kindForContext(ctx);

  // Return the FULL candidate list — the caller (completionsFor → the editor's
  // fuzzy matcher) does the filtering/ranking, so typing "b" can still match
  // ":beep". (Prefix-filtering here would defeat fuzzy matching.)
  list << keywords[ctx];
}

QStringList ScintillaAPI::callTips(const QStringList &context, int commas, QsciScintilla::CallTipsStyle style, QList<int> &shifts) {
  Q_UNUSED( commas );
  Q_UNUSED( style );
  Q_UNUSED( shifts );
  QStringList ctx = context;
  // some day...
  QStringList none;
  return none;
}

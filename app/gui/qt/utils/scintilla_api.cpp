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
#include <QRegularExpression>
#include "scintilla_api.h"

using namespace std;

// The ctor.
ScintillaAPI::ScintillaAPI(QsciLexer *lexer)
    : QsciAbstractAPIs(lexer)
{
  // manually managed for now
  keywords[Chord] << "'1'" << "'5'" << "'+5'" << "'m+5'" << ":sus2" << ":sus4" << "'6'" << ":m6" << "'7sus2'" << "'7sus4'" << "'7-5'" << ":halfdiminished" << "'7+5'" << "'m7+5'" << "'9'" << ":m9" << "'m7+9'" << ":maj9" << "'9sus4'" << "'6*9'" << "'m6*9'" << "'7-9'" << "'m7-9'" << "'7-10'" << "'7-11'" << "'7-13'" << "'9+5'" << "'m9+5'" << "'7+5-9'" << "'m7+5-9'" << "'11'" << ":m11" << ":maj11" << "'11+'" << "'m11+'" << "'13'" << ":m13" << ":add2" << ":add4" << ":add9" << ":add11" << ":add13" << ":madd2" << ":madd4" << ":madd9" << ":madd11" << ":madd13" << ":major" << ":maj" << ":M" << ":minor" << ":min" << ":m" << ":major7" << ":dom7" << "'7'" << ":M7" << ":minor7" << ":m7" << ":augmented" << ":a" << ":diminished" << ":dim" << ":i" << ":diminished7" << ":dim7" << ":i7" << ":halfdim" << "'m7b5'" << "'m7-5'";


  keywords[Scale] << ":diatonic" << ":ionian" << ":major" << ":dorian" << ":phrygian" << ":lydian" << ":mixolydian" << ":aeolian" << ":minor" << ":locrian" << ":hex_major6" << ":hex_dorian" << ":hex_phrygian" << ":hex_major7" << ":hex_sus" << ":hex_aeolian" << ":minor_pentatonic" << ":yu" << ":major_pentatonic" << ":gong" << ":egyptian" << ":shang" << ":jiao" << ":zhi" << ":ritusen" << ":whole_tone" << ":whole" << ":chromatic" << ":harmonic_minor" << ":melodic_minor_asc" << ":hungarian_minor" << ":octatonic" << ":messiaen1" << ":messiaen2" << ":messiaen3" << ":messiaen4" << ":messiaen5" << ":messiaen6" << ":messiaen7" << ":super_locrian" << ":hirajoshi" << ":kumoi" << ":neapolitan_major" << ":bartok" << ":bhairav" << ":locrian_major" << ":ahirbhairav" << ":enigmatic" << ":neapolitan_minor" << ":pelog" << ":augmented2" << ":scriabin" << ":harmonic_major" << ":melodic_minor_desc" << ":romanian_minor" << ":hindu" << ":iwato" << ":melodic_minor" << ":diminished2" << ":marva" << ":melodic_major" << ":indian" << ":spanish" << ":prometheus" << ":diminished" << ":todi" << ":leading_whole" << ":augmented" << ":purvi" << ":chinese" << ":lydian_minor" << ":blues_major" << ":blues_minor" << ":cargah" << ":buselik" << ":buselik_2" << ":kurdi" << ":rast" << ":acemli_rast" << ":ussak" << ":bayati" << ":bayati_2" << ":isfahan" << ":isfahan_2" << ":hicaz_humayun" << ":hicaz_humayun_2" << ":hicaz" << ":hicaz_2" << ":uzzal" << ":uzzal_2" << ":zirguleli_hicaz" << ":zirguleli_hicaz_2" << ":huseyni" << ":huseyni_2" << ":muhayyer" << ":gulizar" << ":neva" << ":neva_2" << ":tahir" << ":tahir_2" << ":karcigar" << ":suznak" << ":suznak_2" << ":mahur" << ":acem_asiran" << ":nihavend" << ":nihavend_2" << ":sultani_yegah" << ":sultani_yegah_2" << ":kurdili_hicazkar" << ":kurdili_hicazkar_2" << ":kurdili_hicazkar_3" << ":kurdili_hicazkar_4" << ":kurdili_hicazkar_5" << ":zirguleli_suznak" << ":zirguleli_suznak_2" << ":zirguleli_suznak_3" << ":hicazkar" << ":hicazkar_2" << ":evcara" << ":evcara_2" << ":evcara_3" << ":evcara_4" << ":suzidil" << ":suzidil_2" << ":sedaraban" << ":sedaraban_2" << ":segah" << ":segah_2" << ":huzzam" << ":huzzam_2" << ":bayati_araban" << ":acem_kurdi" << ":sehnaz" << ":sehnaz_2" << ":sehnaz_3" << ":sehnaz_4" << ":saba" << ":dugah" << ":dugah_2" << ":evic" << ":evic_2" << ":bestenigar" << ":ferahnak" << ":sevkefza" << ":sevkefza_2" << ":sevkefza_3" << ":ferahfeza" << ":ferahfeza_2" << ":yegah" << ":yegah_2";


  keywords[PlayParam] << "amp:" << "attack:" << "release:" << "sustain:" << "decay:" << "env_curve:" << "sustain_level:" << "pan:" << "attack_level:" << "decay_level:" << "on:" << "slide:" << "pitch:";

  keywords[SampleParam] << "amp:" << "pan:" << "attack:" << "decay:" << "sustain:" << "release:" << "attack_level:" << "decay_level:" << "sustain_level:" << "env_curve:" << "rate:" << "beat_stretch:" << "start:" << "finish:" << "slice:" << "num_slices:" << "onset:" << "on:" << "res:" << "lpf:" << "lpf_min:" << "lpf_attack:" << "lpf_decay:" << "lpf_sustain:" << "lpf_release:" << "lpf_init_level:" << "lpf_attack_level:" << "lpf_decay_level:" << "lpf_sustain_level:" << "lpf_release_level" << "lpf_env_curve:" << "hpf:" << "hpf_max:" <<"hpf_attack:" << "hpf_decay:" << "hpf_sustain:" << "hpf_release:" << "hpf_init_level:" << "hpf_attack_level:" << "hpf_decay_level:" << "hpf_sustain_level:" <<  "hpf_release_level:" << "hpf_env_curve:" << "norm:" << "rpitch:" << "pitch:" << "pitch_stretch:" << "window_size:" << "pitch_dis:" << "time_dis:" << "compress:" << "threshold:" << "clamp_time:" << "slope_above:" << "slope_below:" << "relax_time:" << "pre_amp:" << "cutoff:" << "cutoff_slide:" << "cutoff_slide_curve:" << "cutoff_slide_shape:";

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

void ScintillaAPI::updateAutoCompletionList(const QStringList &context,
					   QStringList &list) {
  if (context.isEmpty()) return;

  // default
  int ctx = Func;

  QString partial = context.last();
  QStringList words;
  for (int i=0; i<context.length()-1; i++) {
    if (context[i] != "")
      words.append(context[i]);
  }

  QString last = words.isEmpty() ? "" : words.last();
  QString lastButOne = words.length() < 2 ? "" : words[words.length()-2];
  QString first = words.isEmpty() ? "" : words.first();
  QString second = words.length() < 2 ? "" : words[1];

  /* // debug
  for (int i=0; i<context.length(); i++)
    cout << "context[" << i << "] = " << context[i].toStdString() << endl;
  for (int i=0; i<words.length(); i++)
    cout << "words[" << i << "] = " << words[i].toStdString() << endl;
  cout << "first = " << first.toStdString()
       << ", second = " << second.toStdString()
       << ", lastButOne = " << lastButOne.toStdString()
       << ", last = " << last.toStdString()
       << ", partial = " << partial.toStdString() << endl;
  */

  if (last == "sample" || last == "sample_info" || last == "sample_duration" || last == "use_sample_bpm" || last == "sample_buffer" || last == "sample_loaded?" || last == "load_sample" || last == "load_samples") {
    ctx = Sample;
  } else if (last == "sync" || last == "sync:" || last == "cue" || last == "get" || last == "set" || last == "get[" ) {
    ctx = CuePath;
  } else if (last == "with_fx") {
    ctx = FX;
  } else if (last == "with_synth" || last == "use_synth" || last == "synth") {
    ctx = Synth;
  } else if (last == "load_example") {
    ctx = Examples;
  } else if (last == "use_random_source" || last == "with_random_source") {
    ctx = RandomSource;
  // autocomplete the second arg of scale/chord
  } else if (lastButOne == "scale") {
    ctx = Scale;
  } else if (lastButOne == "chord") {
    ctx = Chord;
  } else if (last == "use_tuning" || last == "with_tuning") {
    ctx = Tuning;

  // FX params
  } else if (words.length() >= 2 &&
             (first == "with_fx")) {
    if (last.endsWith(':')) return; // don't try to complete parameters
    if (fxArgs.contains(second)) {
      list = fxArgs[second];
      return;
    }

  // Synth params
  } else if (words.length() >= 2 && first == "synth") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    if (synthArgs.contains(second)) {
      list = synthArgs[second];
      return;
    }

  // Play params
  } else if (words.length() >= 2 && first == "play") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = PlayParam;

  // Sample params
  } else if (words.length() >= 2 && first == "sample") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = SampleParam;
  } else if (first == "use_sample_defaults" || first == "with_sample_defaults") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = SampleParam;
  } else if ((first == "midi" || (first.startsWith("midi_")) || (first == "use_midi_defaults") || (first == "with_midi_defaults")) && last == "port:") {
    ctx = MidiOuts;
  }  else if (words.length() >= 2 && first == "midi") {
    if (last.endsWith(':')) return; // don't try to complete parameters
    ctx = MidiParam;
  } else if (context.length() > 1) {
    if (partial.length() <= 2) {
      // don't attempt to autocomplete other words on the same line
      // unless we have a plausible match
      return;
    }
  }

  if (partial == "") {
    list << keywords[ctx];
  } else {
    foreach (const QString &str, keywords[ctx]) {
      if (str.startsWith(partial)) {
	list << str;
      }
    }
  }
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

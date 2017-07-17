//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
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

#include "sonicpiapis.h"

using namespace std;

// The ctor.
SonicPiAPIs::SonicPiAPIs(QsciLexer *lexer)
    : QsciAbstractAPIs(lexer)
{
  // manually managed for now
  keywords[Chord] << "'1'" << "'5'" << "'+5'" << "'m+5'" << ":sus2" << ":sus4" << "'6'" << ":m6" << "'7sus2'" << "'7sus4'" << "'7-5'" << ":halfdiminished" << "'7+5'" << "'m7+5'" << "'9'" << ":m9" << "'m7+9'" << ":maj9" << "'9sus4'" << "'6*9'" << "'m6*9'" << "'7-9'" << "'m7-9'" << "'7-10'" << "'7-11'" << "'7-13'" << "'9+5'" << "'m9+5'" << "'7+5-9'" << "'m7+5-9'" << "'11'" << ":m11" << ":maj11" << "'11+'" << "'m11+'" << "'13'" << ":m13" << ":add2" << ":add4" << ":add9" << ":add11" << ":add13" << ":madd2" << ":madd4" << ":madd9" << ":madd11" << ":madd13" << ":major" << ":maj" << ":M" << ":minor" << ":min" << ":m" << ":major7" << ":dom7" << "'7'" << ":M7" << ":minor7" << ":m7" << ":augmented" << ":a" << ":diminished" << ":dim" << ":i" << ":diminished7" << ":dim7" << ":i7" << ":halfdim" << "'m7b5'" << "'m7-5'";


  keywords[Scale] << ":diatonic" << ":ionian" << ":major" << ":dorian" << ":phrygian" << ":lydian" << ":mixolydian" << ":aeolian" << ":minor" << ":locrian" << ":hex_major6" << ":hex_dorian" << ":hex_phrygian" << ":hex_major7" << ":hex_sus" << ":hex_aeolian" << ":minor_pentatonic" << ":yu" << ":major_pentatonic" << ":gong" << ":egyptian" << ":shang" << ":jiao" << ":zhi" << ":ritusen" << ":whole_tone" << ":whole" << ":chromatic" << ":harmonic_minor" << ":melodic_minor_asc" << ":hungarian_minor" << ":octatonic" << ":messiaen1" << ":messiaen2" << ":messiaen3" << ":messiaen4" << ":messiaen5" << ":messiaen6" << ":messiaen7" << ":super_locrian" << ":hirajoshi" << ":kumoi" << ":neapolitan_major" << ":bartok" << ":bhairav" << ":locrian_major" << ":ahirbhairav" << ":enigmatic" << ":neapolitan_minor" << ":pelog" << ":augmented2" << ":scriabin" << ":harmonic_major" << ":melodic_minor_desc" << ":romanian_minor" << ":hindu" << ":iwato" << ":melodic_minor" << ":diminished2" << ":marva" << ":melodic_major" << ":indian" << ":spanish" << ":prometheus" << ":diminished" << ":todi" << ":leading_whole" << ":augmented" << ":purvi" << ":chinese" << ":lydian_minor" << ":blues_major" << ":blues_minor";

  keywords[MCBlock] << ":air" << ":stone" << ":grass" << ":dirt" << ":cobblestone" << ":wood_plank" << ":sapling" << ":bedrock" << ":water_flowing" << ":water" << ":water_stationary" << ":lava_flowing" << ":lava" << ":lava_stationary" << ":sand" << ":gravel" << ":gold_ore" << ":iron_ore" << ":coal_ore" << ":wood" << ":leaves" << ":glass" << ":lapis" << ":lapis_lazuli_block" << ":sandstone" << ":bed" << ":cobweb" << ":grass_tall" << ":flower_yellow" << ":flower_cyan" << ":mushroom_brown" << ":mushroom_red" << ":gold_block" << ":gold" << ":iron_block" << ":iron" << ":stone_slab_double" << ":stone_slab" << ":brick" << ":brick_block" << ":tnt" << ":bookshelf" << ":moss_stone" << ":obsidian" << ":torch" << ":fire" << ":stairs_wood" << ":chest" << ":diamond_ore" << ":diamond_block" << ":diamond" << ":crafting_table" << ":farmland" << ":furnace_inactive" << ":furnace_active" << ":door_wood" << ":ladder" << ":stairs_cobblestone" << ":door_iron" << ":redstone_ore" << ":snow" << ":ice" << ":snow_block" << ":cactus" << ":clay" << ":sugar_cane" << ":fence" << ":glowstone_block" << ":bedrock_invisible" << ":stone_brick" << ":glass_pane" << ":melon" << ":fence_gate" << ":glowing_obsidian" << ":nether_reactor_core";

  keywords[PlayParam] << "amp:" << "attack:" << "release:" << "sustain:" << "decay:" << "env_curve:" << "sustain_level:" << "pan:" << "attack_level:" << "decay_level:" << "on:" << "slide:" << "pitch:";

  keywords[SampleParam] << "amp:" << "pan:" << "attack:" << "decay:" << "sustain:" << "release:" << "attack_level:" << "decay_level:" << "sustain_level:" << "env_curve:" << "rate:" << "beat_stretch:" << "start:" << "finish:" << "slice:" << "num_slices:" << "onset:" << "res:" << "lpf:" << "lpf_min:" << "lpf_attack:" << "lpf_decay:" << "lpf_sustain:" << "lpf_release:" << "lpf_init_level:" << "lpf_attack_level:" << "lpf_decay_level:" << "lpf_sustain_level:" << "lpf_release_level" << "lpf_env_curve:" << "hpf:" << "hpf_max:" <<"hpf_attack:" << "hpf_decay:" << "hpf_sustain:" << "hpf_release:" << "hpf_init_level:" << "hpf_attack_level:" << "hpf_decay_level:" << "hpf_sustain_level:" <<  "hpf_release_level:" << "hpf_env_curve:" << "norm:" << "rpitch:" << "pitch:" << "pitch_stretch:" << "window_size:" << "pitch_dis:" << "time_dis:" << "compress:" << "threshold:" << "clamp_time:" << "slope_above:" << "slope_below:" << "relax_time:" << "pre_amp:";

  keywords[Examples] << ":haunted" << ":ambient_experiment" << ":chord_inversions" << ":filtered_dnb" << ":fm_noise" << ":jungle" << ":ocean" << ":reich_phase" << ":acid" << ":ambient" << ":compus_beats" << ":echo_drama" << ":idm_breakbeat" << ":tron_bike" << ":wob_rhyth" << ":bach" << ":driving_pulse" << ":monday_blues" << ":rerezzed" << ":square_skit" << ":blimp_zones" << ":blip_rhythm" << ":shufflit" << ":tilburg_2" << ":time_machine" << ":sonic_dreams";

  keywords[Tuning] << ":just" << ":pythagorean" << ":meantone" << ":equal";

  keywords[MidiParam] << "sustain:" << "velocity:" << "vel:" << "velocity_f:" << "vel_f:" << "port:" << "channel:";
}



void SonicPiAPIs::loadSamples(QString sample_path) {
  QDir dir(sample_path);
  QStringList filetypes;
  filetypes << "*.wav" << "*.wave" << "*.aif" << "*.aiff" << "*.flac";
  dir.setNameFilters(filetypes);

  QFileInfoList files = dir.entryInfoList(QDir::Files | QDir::NoDotAndDotDot);
  foreach (QFileInfo file, files) {
    addSymbol(Sample, file.baseName());
  }
}

void SonicPiAPIs::addSymbol(int context, QString sym) {
  addKeyword(context, QString(":" + sym));
}

void SonicPiAPIs::addKeyword(int context, QString keyword) {
  keywords[context] << keyword;
}

void SonicPiAPIs::addFXArgs(QString fx, QStringList args) {
  fxArgs.insert(fx, args);
}

void SonicPiAPIs::addSynthArgs(QString fx, QStringList args) {
  synthArgs.insert(fx, args);
}

void SonicPiAPIs::addCuePath(QString path) {
  keywords[CuePath] << path;
}

void SonicPiAPIs::updateAutoCompletionList(const QStringList &context,
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
  } else if (last == "sync" || last == "cue" || last == "get" || last == "set" || last == "get[" ) {
    ctx = CuePath;
  } else if (last == "with_fx" || last == "use_fx") {
    ctx = FX;
  } else if (last == "with_synth" || last == "use_synth" || last == "synth") {
    ctx = Synth;
  } else if (last == "load_example") {
    ctx = Examples;

  // autocomplete the second arg of scale/chord
  } else if (lastButOne == "scale") {
    ctx = Scale;
  } else if (lastButOne == "chord") {
    ctx = Chord;
  } else if (last == "mc_set_block" ||
             last == "mc_block_id" ||
             last == "mc_set_area") {
    ctx = MCBlock;
  } else if (last == "use_tuning" || last == "with_tuning") {
    ctx = Tuning;

  // FX params
  } else if (words.length() >= 2 &&
             (first == "with_fx" || first == "use_fx")) {
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

QStringList SonicPiAPIs::callTips(const QStringList &context, int commas, QsciScintilla::CallTipsStyle style, QList<int> &shifts) {
  Q_UNUSED( commas );
  Q_UNUSED( style );
  Q_UNUSED( shifts );
  QStringList ctx = context;
  // some day...
  QStringList none;
  return none;
}

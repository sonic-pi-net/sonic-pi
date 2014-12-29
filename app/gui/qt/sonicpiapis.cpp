//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, distribution,
// and distribution of modified versions of this work as long as this
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
  keywords[Chord] << "'1'" << "'5'" << "'+5'" << "'m+5'" << ":sus2" << ":sus4" << "'6'" << ":m6" << "'7sus2'" << "'7sus4'" << "'7-5'" << "'m7-5'" << "'7+5'" << "'m7+5'" << "'9'" << ":m9" << "'m7+9'" << ":maj9" << "'9sus4'" << "'6*9'" << "'m6*9'" << "'7-9'" << "'m7-9'" << "'7-10'" << "'9+5'" << "'m9+5'" << "'7+5-9'" << "'m7+5-9'" << "'11'" << ":m11" << ":maj11" << "'11+'" << "'m11+'" << "'13'" << ":m13" << ":major" << ":M" << ":minor" << ":m" << ":major7" << ":dom7" << "'7'" << ":M7" << ":minor7" << ":m7" << ":augmented" << ":a" << ":diminished" << ":dim" << ":i" << ":diminished7" << ":dim7" << ":i7";

  keywords[Scale] << ":diatonic" << ":ionian" << ":major" << ":dorian" << ":phrygian" << ":lydian" << ":mixolydian" << ":aeolian" << ":minor" << ":locrian" << ":hex_major6" << ":hex_dorian" << ":hex_phrygian" << ":hex_major7" << ":hex_sus" << ":hex_aeolian" << ":minor_pentatonic" << ":yu" << ":major_pentatonic" << ":gong" << ":egyptian" << ":shang" << ":jiao" << ":zhi" << ":ritusen" << ":whole_tone" << ":whole" << ":chromatic" << ":harmonic_minor" << ":melodic_minor_asc" << ":hungarian_minor" << ":octatonic" << ":messiaen1" << ":messiaen2" << ":messiaen3" << ":messiaen4" << ":messiaen5" << ":messiaen6" << ":messiaen7" << ":super_locrian" << ":hirajoshi" << ":kumoi" << ":neapolitan_major" << ":bartok" << ":bhairav" << ":locrian_major" << ":ahirbhairav" << ":enigmatic" << ":neapolitan_minor" << ":pelog" << ":augmented2" << ":scriabin" << ":harmonic_major" << ":melodic_minor_desc" << ":romanian_minor" << ":hindu" << ":iwato" << ":melodic_minor" << ":diminished2" << ":marva" << ":melodic_major" << ":indian" << ":spanish" << ":prometheus" << ":diminished" << ":todi" << ":leading_whole" << ":augmented" << ":purvi" << ":chinese" << ":lydian_minor";
}

void SonicPiAPIs::loadSamples(QString sample_path) {
  QDir dir(sample_path);
  QStringList filetypes;
  filetypes << "*.wav";
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

void SonicPiAPIs::updateAutoCompletionList(const QStringList &context,
					   QStringList &list) {
  //  for (int i=0; i<context.length(); i++)
  //    cout << "context[" << i << "] = " << context[i].toStdString() << endl;

  // default
  int ctx = Func;
  int last = context.length()-1;
  if (context.length() > 1 && 
      (context[last] == "" || context[last][0] == ':'))
    last--;
  int lastButOne = last - 1;

  if (context[last] == "sample") {
    ctx = Sample;
  } else if (context[last] == "with_fx" || context[last] == "use_fx") {
    ctx = FX;
  } else if (context[last] == "with_synth" || context[last] == "use_synth") {
    ctx = Synth;

  // autocomplete the second arg of scale/chord
  } else if (lastButOne >= 0 && context[lastButOne] == "scale") {
    ctx = Scale;
  } else if (lastButOne >= 0 && context[lastButOne] == "chord") {
    ctx = Chord;

  // FX params
  } else if (context.length() > 2 &&
	     (context[0] == "with_fx" ||
	      context[0] == "use_fx")) {
    if (context[last].endsWith(':')) return; // don't try to complete parameters
    if (fxArgs.contains(context[1])) {
      list = fxArgs[context[1]];
      return;
    }

  } else if (context.length() > 1) {
    if (context[context.length()-1].length() <= 2) {
      // don't attempt to autocomplete other words on the same line
      // unless we have a plausible match
      return;
    }
  }

  if (context[context.length()-1].length() == 0) {
    list << keywords[ctx];
  } else {
    foreach (const QString &str, keywords[ctx]) {
      if (str.startsWith(context[context.length()-1])) {
	list << str;
      }
    }
  }
}

QStringList SonicPiAPIs::callTips(const QStringList &context, int commas,
				  QsciScintilla::CallTipsStyle style,
				  QList<int> &shifts) {
  QStringList ctx = context; commas = commas; style = style; shifts = shifts;
  // some day...
  QStringList none;
  return none;
}

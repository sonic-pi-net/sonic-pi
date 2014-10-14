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

#include <iostream>
#include <sstream>
#include <fstream>
#include <QDir>

#include "sonicpiapis.h"


void SonicPiAPIs::addSymbol(int context, QString sym) {
  addKeyword(context, QString(":" + sym));
}

void SonicPiAPIs::addKeyword(int context, QString keyword) {
  keywords[context] << keyword;
}

// The ctor.
SonicPiAPIs::SonicPiAPIs(QsciLexer *lexer, QString sample_path)
    : QsciAbstractAPIs(lexer)
{
  std::cout << "got sample_path: " << sample_path.toStdString() << std::endl;
  QDir dir(sample_path);
  QStringList filetypes;
  filetypes << "*.wav";
  dir.setNameFilters(filetypes);

  QFileInfoList files = dir.entryInfoList(QDir::Files | QDir::NoDotAndDotDot);
  foreach (QFileInfo file, files) {
    addSymbol(Sample, file.baseName());
  }
}


// The dtor.
SonicPiAPIs::~SonicPiAPIs()
{

}

void SonicPiAPIs::updateAutoCompletionList(const QStringList &context,
					   QStringList &list) {
  // QSci's idea of context is somewhat different from mine
  // apparently only gets the previous word at most
  if (context.length() == 0) {
    // does this ever happen?
    std::cout << "updateAutoCompletionList context = []" << std::endl;
    return;
  }

  /*
  for (int i=0; i<context.length(); i++) {
    std::cout << "context[" << i << "] = " << context[i].toStdString() << std::endl;
  }
  */

  // default
  int ctx = Func;

  if (context[0] == "sample") {
    ctx = Sample;
  } else if (context[0] == "with_fx" || context[0] == "use_fx") {
    ctx = FX;
  } else if (context[0] == "with_synth" || context[0] == "use_synth") {
    ctx = Synth;
  } else {
    // no context
    if (context[context.length()-1].length() < 2) {
      // too short, don't show full list of matches
      return;
    }
  }

  list << keywords[ctx];
}

void SonicPiAPIs::autoCompletionSelected(const QString &sel) {

}

QStringList SonicPiAPIs::callTips(const QStringList &context, int commas,
				  QsciScintilla::CallTipsStyle style,
				  QList<int> &shifts) {
  QStringList tips;
  return tips;
}
  
bool SonicPiAPIs::event(QEvent *e) {
  return QObject::event(e); // huh?
}

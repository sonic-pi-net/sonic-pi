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

#include "sonicpiscintilla.h"

// The ctor.
SonicPiScintilla::SonicPiScintilla()
  : QsciScintilla()
{
  // set defaults here
}

QStringList SonicPiScintilla::apiContext(int pos, int &context_start,
					 int &last_word_start)
{
  QStringList context;
  // sampl|
  // sample |
  // chord :E3,|

  int linenum, cursor;
  getCursorPosition(&linenum, &cursor);
  QString line = text(linenum);
  line.truncate(cursor);
  context = line.split(QRegExp("[ ,(){}]+"));

  context_start = 0;
  last_word_start = pos;

  return context;
}

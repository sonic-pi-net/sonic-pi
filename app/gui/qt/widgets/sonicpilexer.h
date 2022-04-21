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

#include  <Qsci/qscilexerruby.h>

#include "model/sonicpitheme.h"

class SonicPiLexer : public QsciLexerRuby
{

  Q_OBJECT

public:
  SonicPiLexer(SonicPiTheme *customTheme);
  QColor defaultColor(int style) const;
  QColor defaultPaper(int style) const;
  QFont defaultFont(int style) const;
  QStringList autoCompletionWordSeparators() const;

private:
  SonicPiTheme *theme;

public slots:
  void highlightAll();
  void unhighlightAll();
};

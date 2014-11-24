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


#include <Qsci/qsciscintilla.h>

class SonicPiLexer;
class QSettings;

class SonicPiScintilla : public QsciScintilla
{
 public:
  SonicPiScintilla(SonicPiLexer *lexer);
  
  virtual QStringList apiContext(int pos, int &context_start,
				 int &last_word_start);

 private:
    void addKeyBinding(QSettings &qs, int cmd, int key);
    void addOtherKeyBinding(QSettings &qs, int cmd, int key);
};

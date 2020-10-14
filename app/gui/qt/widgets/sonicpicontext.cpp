//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2020 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++


#include "sonicpicontext.h"

// Standard stuff
#include <QScrollBar>

SonicPiContext::SonicPiContext(QWidget *parent) : QPlainTextEdit(parent)
{


}

void SonicPiContext::setContent(QString text) {
  setPlainText(text);
}

void SonicPiContext::setFontFamily(QString font_name)
{
#ifdef __APPLE__
  setFont(QFont(font_name, 14, -1, false));
#elif __linux__
  setFont(QFont(font_name, 12, -1, false));
#else
  setFont(QFont(font_name, 8, -1, false));
#endif
}

void SonicPiContext::setTextColor(QColor c)
{
  QTextCharFormat tf;
  tf.setForeground(c);
  setCurrentCharFormat(tf);
}

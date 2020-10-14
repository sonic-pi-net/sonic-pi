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

#ifndef SONICPICONTEXT_H
#define SONICPICONTEXT_H

#include <QPlainTextEdit>

class SonicPiContext : public QPlainTextEdit
{
    Q_OBJECT
public:
    explicit SonicPiContext(QWidget *parent = 0);

signals:

public slots:
  void setContent(QString text);
  void setFontFamily(QString font_name);
  void setTextColor(QColor c);

protected:
};

#endif // SONICPICONTEXT_H

//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2022 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPIEDITOR_H
#define SONICPIEDITOR_H

#include <QWidget>
#include "sonicpicontext.h"
#include "widgets/sonicpiscintilla.h"
#include "model/sonicpitheme.h"

class SonicPiEditor : public QWidget
{
    Q_OBJECT
public:
  SonicPiEditor(SonicPiScintilla *workspace, SonicPiTheme *theme, QWidget *parent = nullptr);

  SonicPiScintilla* getWorkspace();
  SonicPiContext* getContext();
  void setContextContent(QString s);
  void hideContext();
  void showContext();
  void updateColourTheme(QString appStyling,  SonicPiTheme::Style themeStyle);

signals:

public slots:

protected:

private:
  SonicPiScintilla* m_workspace;
  SonicPiContext* m_context;
  SonicPiTheme* m_theme;


};

#endif // SONICPIEDITOR_H

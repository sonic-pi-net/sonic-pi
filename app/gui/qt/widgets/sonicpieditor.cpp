//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2022 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++
#include <iostream>
#include "sonicpieditor.h"
#include "sonicpicontext.h"
#include "widgets/sonicpiscintilla.h"
#include <QVBoxLayout>
#include "dpi.h"

SonicPiEditor::SonicPiEditor(SonicPiScintilla *workspace, QWidget* parent)
  : QWidget(parent),
    m_workspace(workspace)
{
  QVBoxLayout* workspace_layout = new QVBoxLayout;
  QWidget* workspace_widget = new QWidget;
  setLayout(workspace_layout);
  m_context = new SonicPiContext(this);
  m_context->setContent("");
  m_context->setMaximumHeight(ScaleHeightForDPI(30));
  m_context->setReadOnly(true);
  m_context->setLineWrapMode(QPlainTextEdit::NoWrap);
  m_context->setFontFamily("Hack");

  workspace_layout->addWidget(m_workspace);
  workspace_layout->addWidget(m_context);
}

SonicPiScintilla* SonicPiEditor::getWorkspace()
{
  return m_workspace;
}

SonicPiContext* SonicPiEditor::getContext()
{
  return m_context;
}

void SonicPiEditor::setContextContent(QString s)
{
  m_context->setContent(s);
}

void SonicPiEditor::hideContext()
{
  m_context->hide();
}

void SonicPiEditor::showContext()
{
  m_context->show();
}

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
#include <QFontMetrics>
#include <QTextDocument>
#include "dpi.h"

SonicPiEditor::SonicPiEditor(SonicPiScintilla *workspace, SonicPiTheme *theme, QWidget* parent)
  : QWidget(parent),
    m_workspace(workspace),
    m_theme(theme)
{
  QVBoxLayout* workspace_layout = new QVBoxLayout;
  QWidget* workspace_widget = new QWidget;
  setLayout(workspace_layout);
  // Fill the pane: the style's default layout margins would inset the whole
  // editor (and so its scrollbars) ~10px from the pane edge, leaving the code
  // scrollbar floating while the log/info panes sit flush. Zero margins +
  // spacing so every pane's scrollbar shares the same edge offset.
  workspace_layout->setContentsMargins(0, 0, 0, 0);
  workspace_layout->setSpacing(0);
  m_context = new SonicPiContext(this);
  m_context->setContent("");
  m_context->setReadOnly(true);
  m_context->setLineWrapMode(QPlainTextEdit::NoWrap);
  m_context->setFontFamily("Hack");
  m_context->setTextColor(QColor(m_theme->color("LogForeground")));
  // The context pane is a single-line status readout (current line / cursor
  // position), so pin it to exactly one line's height and never scroll it.
  // Without a fixed height it overflows and Qt shows stray scrollbars, whose
  // corner reads as a little grey square.
  m_context->setFrameShape(QFrame::NoFrame);
  m_context->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  m_context->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  m_context->document()->setDocumentMargin(ScaleHeightForDPI(4));
  QFontMetrics contextFm(m_context->font());
  m_context->setFixedHeight(contextFm.height() + ScaleHeightForDPI(12));
  workspace_layout->addWidget(m_workspace);
  workspace_layout->addWidget(m_context);

  // The editor's scroll-area corner (where its horizontal and vertical
  // scrollbars meet) is painted with the window grey by the style. Now that the
  // scrollbar tracks are transparent that corner stands out as a stray grey
  // square, so cover it with a widget coloured like the editor background. It's
  // only shown by Qt when both scrollbars are visible — exactly when the corner
  // would otherwise appear.
  QWidget* scrollCorner = new QWidget(m_workspace);
  scrollCorner->setStyleSheet(QString("background: %1;").arg(m_theme->color("Background").name()));
  m_workspace->setCornerWidget(scrollCorner);
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
  m_context->setContent(QString("    ") + s);
}

void SonicPiEditor::hideContext()
{
  m_context->hide();
}

void SonicPiEditor::showContext()
{
  m_context->show();
}

void SonicPiEditor::updateColourTheme(QString appStyling,  SonicPiTheme::Style themeStyle)
{

  m_workspace->setFrameShape(QFrame::NoFrame);
  m_workspace->setStyleSheet("");
  m_workspace->setStyleSheet(appStyling);
  if (QWidget* corner = m_workspace->cornerWidget())
    corner->setStyleSheet(QString("background: %1;").arg(m_theme->color("Background").name()));
  m_context->setTextColor(QColor(m_theme->color("LogForeground")));
  if (themeStyle == SonicPiTheme::HighContrastMode)
    {
      m_workspace->setCaretWidth(8);
    }
  else
    {
      m_workspace->setCaretWidth(5);
    }
  m_workspace->redraw();

}

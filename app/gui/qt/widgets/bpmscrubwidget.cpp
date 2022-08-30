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

#include "bpmscrubwidget.h"
#include "qt_api_client.h"
#include <QDoubleValidator>
#include <QApplication>
#include <QShortcut>
#include "dpi.h"

BPMScrubWidget::BPMScrubWidget(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, bool setPosAvailable, QWidget* parent)
  : QLineEdit(parent)
  , m_spClient(spClient)
  , m_spAPI(spAPI)
  , theme(theme)
  , m_setPosAvailable(setPosAvailable)
{
  m_isDragging = false;
  m_bpmValue = 60.0;
  m_linkEnabled = false;
  displayBPM();
  connect(this, &QLineEdit::editingFinished, this, &BPMScrubWidget::readSetDisplayAndSyncBPM);

  QShortcut* escape = new QShortcut(QKeySequence("Escape"), this);
  connect(escape, &QShortcut::activated, [=]() {
    editingCancelled();
  });

};


void BPMScrubWidget::setBPM(double bpm)
{
  if(bpm < 20.0) {
    m_bpmValue = 20.0;
  } else if(bpm > 999.0) {
    m_bpmValue = 999.0;
  } else {
    m_bpmValue = bpm;
  }
}

double BPMScrubWidget::getBPM()
{
  return m_bpmValue;
}

QString BPMScrubWidget::formatBPM()
{
  return QString("%1 bpm").arg(m_bpmValue, 6, 'f', 2);
}

void BPMScrubWidget::displayBPM()
{
  setText(formatBPM());
}

void BPMScrubWidget::syncBPM()
{
  m_spAPI.get()->SetLinkBPM(m_bpmValue);
}

void BPMScrubWidget::editingCancelled()
{
    displayBPM();
    m_isEditing = false;
}


void BPMScrubWidget::readAndSetBPM()
{
  bool ok(false);
  double d = text().split(QRegularExpression("[^.0-9]"))[0].trimmed().toDouble(&ok);
  if (ok) {
    setBPM(d);
  }
}

void BPMScrubWidget::readSetDisplayAndSyncBPM()
{
  readAndSetBPM();
  displayBPM();
  syncBPM();
}

void BPMScrubWidget::setAndDisplayBPM(double bpm)
{
  setBPM(bpm);
  displayBPM();
}

void BPMScrubWidget::setDisplayAndSyncBPM(double bpm)
{
  setBPM(bpm);
  displayBPM();
  syncBPM();
}

void BPMScrubWidget::mousePressEvent(QMouseEvent* event)
{
  m_lastMouseClickGlobalPos = event->globalPos();
  m_lastMouseDragGlobalPos = m_lastMouseClickGlobalPos;
  m_isDragging = true;
  m_dragDelta = 0;
  m_preDragBpmValue = m_bpmValue;
  QGuiApplication::setOverrideCursor(QCursor(Qt::BlankCursor));
}

void BPMScrubWidget::mouseReleaseEvent(QMouseEvent* event)
{
  if(m_isDragging)
  {
    QCursor::setPos(m_lastMouseClickGlobalPos);
  }

  m_isDragging = false;
  QGuiApplication::restoreOverrideCursor();
}

void BPMScrubWidget::mouseMoveEvent(QMouseEvent* event)
{
  if(m_isDragging)
  {
    int diff = m_lastMouseDragGlobalPos.y() - event->globalPos().y();

    if(diff > 0) {
      m_dragDelta = m_dragDelta + 0.5;
    } else if(diff < 0) {
      m_dragDelta = m_dragDelta - 0.5;
    } else {
      // diff is 0 - do nothing
    }

    if(m_dragDelta > 1) {
      setDisplayAndSyncBPM(std::round(m_bpmValue + 1));
      m_dragDelta = 0;
      m_isEditing = false;
      QCursor::setPos(m_lastMouseClickGlobalPos);
      if(m_setPosAvailable) {
        m_lastMouseDragGlobalPos = m_lastMouseClickGlobalPos;
      } else {
        m_lastMouseDragGlobalPos = QCursor::pos();
      }
    } else if(m_dragDelta < -1) {
      setDisplayAndSyncBPM(std::round(m_bpmValue - 1));
      m_dragDelta = 0;
      m_isEditing = false;
      QCursor::setPos(m_lastMouseClickGlobalPos);
      if(m_setPosAvailable) {
        m_lastMouseDragGlobalPos = m_lastMouseClickGlobalPos;
      } else {
        m_lastMouseDragGlobalPos = QCursor::pos();
      }

    }
  }
}

void BPMScrubWidget::wheelEvent(QWheelEvent* event)
{

  QPoint numPixels = event->pixelDelta() / 4;
  QPoint numDegrees = event->angleDelta() / 8;

  if (!numPixels.isNull()) {
    setDisplayAndSyncBPM(m_bpmValue - numPixels.y());
  } else if (!numDegrees.isNull()) {
    QPoint numSteps = numDegrees / 15;
    setDisplayAndSyncBPM(m_bpmValue + numSteps.y());
  }

  event->accept();
}

void BPMScrubWidget::mouseDoubleClickEvent(QMouseEvent* event)
{
  m_isDragging = false;
  QGuiApplication::restoreOverrideCursor();

  setDisplayAndSyncBPM(60);
}

void BPMScrubWidget::keyReleaseEvent(QKeyEvent* event)
{
    switch( event->key() ) {

    case Qt::Key_Escape:
      editingCancelled();
      break;
    default:
      break;
    }

    QLineEdit::keyReleaseEvent(event);
}



void BPMScrubWidget::keyPressEvent(QKeyEvent* event)
{
  QString str = text();
  bool wasEditing = m_isEditing;

  switch( event->key() ) {

  case Qt::Key_Return:
    m_isEditing = false;
    readSetDisplayAndSyncBPM();
    break;
  case Qt::Key_Escape:
    m_isEditing = false;
    editingCancelled();
    break;
  case Qt::Key_Up:
    if(!wasEditing) {
      m_isEditing = false;
      setDisplayAndSyncBPM(m_bpmValue + 1);
    }
    break;
  case Qt::Key_Down:
    if(!wasEditing) {
      m_isEditing = false;
      setDisplayAndSyncBPM(m_bpmValue - 1);
    }
    break;
  case Qt::Key_Left:
    m_isEditing = true;
    cursorBackward(false);
    break;
  case Qt::Key_Right:
    m_isEditing = true;
    cursorForward(false);
    break;
  case Qt::Key_0:
  case Qt::Key_1:
  case Qt::Key_2:
  case Qt::Key_3:
  case Qt::Key_4:
  case Qt::Key_5:
  case Qt::Key_6:
  case Qt::Key_7:
  case Qt::Key_8:
  case Qt::Key_9:
    if(!wasEditing) {
      m_isEditing = true;
      str.clear();
      str.append(event->text());
      setText(str);
    } else {
      m_isEditing = true;
      insert(event->text());
    }
    break;
  case Qt::Key_Period:
    if(!str.contains(".")) {
      m_isEditing = true;
      insert(event->text());
    }
    break;
  case Qt::Key_Backspace:
    if(!wasEditing) {
      m_isEditing = true;
      str.clear();
      setText(str);
    } else {
      m_isEditing = true;
      backspace();
    }
    break;

  default:
    break;
  }
}

QString BPMScrubWidget::generateStylesheet(QString text, QString border, QString background, QString pressedBackground)
{
  return QString("\nQLineEdit#bpmScrubber {\ncolor: %1;\nborder-color: %2;\nbackground-color: %3;}\n\nQLineEdit#bpmScrubber::hover:!pressed {\nbackground-color: %4;}\n").arg(theme->color(text).name()).arg(theme->color(border).name()).arg(theme->color(background).name()).arg(theme->color(pressedBackground).name());
}

void BPMScrubWidget::displayBPMChangeVisualCue()
{
  QString qss;

  if(m_linkEnabled){
    qss = generateStylesheet("ButtonText",
                             "PressedButton",
                             "PressedButton",
                             "PressedButton");
  } else {
    qss = generateStylesheet("ButtonText",
                             "PressedButton",
                             "PressedButton",
                             "PressedButton");
  }

  setStyleSheet(theme->getAppStylesheet() + qss);
  QTimer::singleShot(250, this, &BPMScrubWidget::displayNoVisualCue);
}


void BPMScrubWidget::displayNoVisualCue()
{
  QString qss;
  if(m_linkEnabled){
    qss = generateStylesheet("WindowForeground",
                             "PressedButton",
                             "PaneBackground",
                             "PressedButton");

  } else {
    qss = generateStylesheet("WindowForeground",
                             "HoverButton",
                             "PaneBackground",
                             "HoverButton");

  }
  setStyleSheet(theme->getAppStylesheet() + qss);
}

void BPMScrubWidget::displayResetVisualCue()
{
  QString qss;
  if(m_linkEnabled){
    qss = generateStylesheet("PaneBackground",
                             "ButtonText",
                             "WindowForeground",
                             "PressedButton");

  } else {
    qss = generateStylesheet("PaneBackground",
                             "ButtonText",
                             "WindowForeground",
                             "HoverButton");
  }

  setStyleSheet(theme->getAppStylesheet() + qss);
  QTimer::singleShot(250, this, &BPMScrubWidget::displayNoVisualCue);

}

void BPMScrubWidget::setLinkEnabled()
{
  m_linkEnabled = true;
  displayNoVisualCue();
}

void BPMScrubWidget::setLinkDisabled()
{
  m_linkEnabled = false;
  displayNoVisualCue();
}

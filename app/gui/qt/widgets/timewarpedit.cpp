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

#include "timewarpedit.h"
#include "qt_api_client.h"
#include <QDoubleValidator>
#include <QApplication>
#include <QShortcut>
#include "dpi.h"

TimeWarpEdit::TimeWarpEdit(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, bool setPosAvailable, QWidget* parent)
  : QLineEdit(parent)
  , m_spClient(spClient)
  , m_spAPI(spAPI)
  , theme(theme)
  , m_setPosAvailable(setPosAvailable)
{
  m_isDragging = false;
  m_timeWarpValue = 0;
  m_linkEnabled = false;
  displayTimeWarpValue();

  connect(this, &QLineEdit::editingFinished, [=]() {
    warpToTime();
  });

  QShortcut* escape = new QShortcut(QKeySequence("Escape"), this);
  connect(escape, &QShortcut::activated, [=]() {
    editingCancelled();
  });

  m_timer = new QTimer(this);
  m_timer->setSingleShot(true);

  connect(m_timer, &QTimer::timeout, this, [=](){
    setStyleSheet(theme->getAppStylesheet());
  });

  m_valueMatcher = new QRegularExpression("(-?[0-9]+)");
};

void TimeWarpEdit::warpToTime()
{
  //insert new API call here..
  m_spAPI.get()->SetGlobalTimeWarp(m_timeWarpValue);
  flash();
}

void TimeWarpEdit::editingCancelled()
{
    displayTimeWarpValue();
    m_isEditing = false;
}

void TimeWarpEdit::mousePressEvent(QMouseEvent* event)
{
  m_lastMouseClickGlobalPos = event->globalPos();
  m_lastMouseDragGlobalPos = m_lastMouseClickGlobalPos;
  m_isDragging = true;
  m_preDragTimeWarpValue = m_timeWarpValue;
  QGuiApplication::setOverrideCursor(QCursor(Qt::BlankCursor));
}

void TimeWarpEdit::mouseReleaseEvent(QMouseEvent* event)
{
  if(m_isDragging)
  {
    QCursor::setPos(m_lastMouseClickGlobalPos);
  }

  m_isDragging = false;
  QGuiApplication::restoreOverrideCursor();
  readSetDisplayAndWarpToTime();
}

void TimeWarpEdit::mouseMoveEvent(QMouseEvent* event)
{
  if(m_isDragging)
  {
    int diff = m_lastMouseDragGlobalPos.y() - event->globalPos().y();
    int delta = 0;

    if(diff > 0) {
      delta = 1;
    } else if(diff < 0) {
      delta = -1;
    } else {
      // delta is 0 - do nothing to the time warp value
    }

    if(delta != 0) {
      setDisplayAndWarpToTime(std::round(m_timeWarpValue + delta));
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

void TimeWarpEdit::wheelEvent(QWheelEvent* event)
{

  QPoint numPixels = event->pixelDelta() / 4;
  QPoint numDegrees = event->angleDelta() / 8;

  if (!numPixels.isNull()) {
    setDisplayAndWarpToTime(m_timeWarpValue - numPixels.y());
  } else if (!numDegrees.isNull()) {
    QPoint numSteps = numDegrees / 15;
    setDisplayAndWarpToTime(m_timeWarpValue + numSteps.y());
  }

  event->accept();
}

void TimeWarpEdit::mouseDoubleClickEvent(QMouseEvent* event)
{
  m_isDragging = false;
  QGuiApplication::restoreOverrideCursor();

  setDisplayAndWarpToTime(0);
}

void TimeWarpEdit::keyReleaseEvent(QKeyEvent* event)
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



void TimeWarpEdit::keyPressEvent(QKeyEvent* event)
{
  QString str = text();
  bool wasEditing = m_isEditing;

  switch( event->key() ) {

  case Qt::Key_Return:
    m_isEditing = false;
    readSetDisplayAndWarpToTime();

    break;
  case Qt::Key_Escape:
    m_isEditing = false;
    editingCancelled();
    break;
  case Qt::Key_Up:
    if(!wasEditing) {
      m_isEditing = false;
      setDisplayAndWarpToTime(m_timeWarpValue + 1);
    }
    break;
  case Qt::Key_Down:
    if(!wasEditing) {
      m_isEditing = false;
      setDisplayAndWarpToTime(m_timeWarpValue - 1);
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
  case Qt::Key_Minus:
    if((!wasEditing) || (text().trimmed() == ""))  {
      m_isEditing = true;
      str.clear();
      str.append(event->text());
      setText(str);
    } else {
      // do nothing
    }
    break;
  case Qt::Key_Plus:
    if((!wasEditing) || (text().trimmed() == ""))  {
      m_isEditing = true;
      str.clear();
      str.append(event->text());
      setText(str);
    } else {
      // do nothing
    }
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

void TimeWarpEdit::setDisplayAndWarpToTime(int val)
{
  setTimeWarpValue(val);
  displayTimeWarpValue();
  warpToTime();
}

void TimeWarpEdit::readAndSetTimeWarpValue()
{

  int val = textToInt(text());
  setTimeWarpValue(val);
}

int TimeWarpEdit::textToInt(QString text)
{
  bool ok(false);
  int val = m_valueMatcher->match(text).captured(1).trimmed().toInt(&ok);
  if (ok) {
    return val;
  } else {
    return 0;
  }
}

void TimeWarpEdit::readSetDisplayAndWarpToTime()
{
  readAndSetTimeWarpValue();
  displayTimeWarpValue();
  warpToTime();

}

int TimeWarpEdit::getTimeWarpValue()
{
  return m_timeWarpValue;
}

void TimeWarpEdit::setTimeWarpValue(int val)
{
  if (val < -250) {
      val = -250;
    } else if (val > 999) {
      val = 999;
    }
  m_timeWarpValue = val;
}

void TimeWarpEdit::displayTimeWarpValue()
{
  if (m_timeWarpValue > 0) {
    setText(QString("+%1 ms").arg(m_timeWarpValue));
  } else {
    setText(QString("%1 ms").arg(m_timeWarpValue));
  }
}

void TimeWarpEdit::flash()
{
  QString qss = QString("\nQLineEdit#timeWarpEdit {\ncolor: %1;\nborder-color: %2;\nbackground-color: %3;}\n\nQLineEdit#timeWarpEdit::hover:!pressed {\nbackground-color: %4;}\n").arg(theme->color("ButtonText").name()).arg(theme->color("PressedButton").name()).arg(theme->color("PressedButton").name()).arg(theme->color("PressedButton").name());

  setStyleSheet(theme->getAppStylesheet() + qss);
  m_timer->stop();
  m_timer->start(250);


}

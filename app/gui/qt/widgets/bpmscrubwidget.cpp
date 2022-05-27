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

BPMScrubWidget::BPMScrubWidget(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, QWidget* parent)
  : QLineEdit(parent)
  , m_spClient(spClient)
  , m_spAPI(spAPI)
{
  m_isDragging = false;
  m_bpmValue = 60.0;
  setText(QString("%1").arg(m_bpmValue));
  connect(this, &QLineEdit::returnPressed, this, &BPMScrubWidget::readAndSetBPM);
  setValidator( new QDoubleValidator(20, 1000, 2, this) );
};

void BPMScrubWidget::readAndSetBPM()
{
  bool ok(false);
  double d = text().toDouble(&ok);
  if (ok) {
    setBPMLabel(d);
  }
  m_spAPI.get()->SetLinkBPM(m_bpmValue);
}

void BPMScrubWidget::mousePressEvent(QMouseEvent* event)
{
  m_lastMouseClickPos = event->pos();
  m_lastMouseClickGlobalPos = event->globalPos();
  m_isDragging = true;
  m_preDragBpmValue = m_bpmValue;
  QGuiApplication::setOverrideCursor(QCursor(Qt::BlankCursor));
}

void BPMScrubWidget::setBPMLabel(double bpm)
{
  if(bpm < 20.0) {
    m_bpmValue = 20.0;
  } else if(bpm > 999.0) {
    m_bpmValue = 999.0;
  } else {
    m_bpmValue = bpm;
  }
  setText(QString("%1").arg(m_bpmValue, 6, 'f', 2));
}

void BPMScrubWidget::setBPM(double bpm)
{
  setBPMLabel(bpm);
  readAndSetBPM();
}

void BPMScrubWidget::mouseReleaseEvent(QMouseEvent* event)
{
  m_isDragging = false;
  QGuiApplication::restoreOverrideCursor();
}

void BPMScrubWidget::mouseMoveEvent(QMouseEvent* event)
{
  if(m_isDragging)
  {
    int diff = m_lastMouseClickPos.y() - event->pos().y();
    setBPM(m_bpmValue + diff);
    QCursor::setPos(m_lastMouseClickGlobalPos);
  }
}

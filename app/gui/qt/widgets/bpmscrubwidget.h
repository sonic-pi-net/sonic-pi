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
#ifndef BPMSCRUBWIDGET_H
#define BPMSCRUBWIDGET_H

#include <QWidget>
#include <QLineEdit>
#include <QMouseEvent>
#include <QPoint>
#include "model/sonicpitheme.h"
#include "qt_api_client.h"
#include "api/sonicpi_api.h"


class BPMScrubWidget : public QLineEdit
{
  Q_OBJECT

public:
  BPMScrubWidget(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, bool setPosAvailable, QWidget *parent = nullptr);

  void setAndDisplayBPM(double bpm);
  void setDisplayAndSyncBPM(double bpm);
  void setLinkEnabled();
  void setLinkDisabled();
  void displayResetVisualCue();
  void displayNoVisualCue();
  void displayBPMChangeVisualCue();
  double getBPM();
  SonicPiTheme *theme;

signals:

public slots:

protected:
  void mousePressEvent(QMouseEvent * event);
  void mouseReleaseEvent(QMouseEvent * event);
  void mouseMoveEvent(QMouseEvent * event);
  void mouseDoubleClickEvent(QMouseEvent * event);
  void keyPressEvent(QKeyEvent* event);
  void keyReleaseEvent(QKeyEvent* event);
  void wheelEvent(QWheelEvent * event);

private:
  std::shared_ptr<SonicPi::QtAPIClient> m_spClient;
  std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
  QPoint m_lastMouseClickGlobalPos, m_lastMouseDragGlobalPos;
  bool m_isDragging, m_isEditing, m_linkEnabled, m_setPosAvailable;
  double m_dragDelta;
  double m_bpmValue, m_preDragBpmValue;
  void readAndSetBPM();

  void readSetDisplayAndSyncBPM();
  void syncBPM();
  void displayBPM();
  QString formatBPM();
  void editingCancelled();
  void setBPM(double bpm);
  QString generateStylesheet(QString text, QString border, QString background, QString pressedBackground );




};

#endif // BPMSCRUBWIDGET_H

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
#ifndef TIMEWARPEDIT_H
#define TIMEWARPEDIT_H

#include <QWidget>
#include <QLineEdit>
#include <QMouseEvent>
#include <QPoint>
#include <QTimer>
#include "model/sonicpitheme.h"
#include "qt_api_client.h"
#include "api/sonicpi_api.h"


class TimeWarpEdit : public QLineEdit
{
  Q_OBJECT
public:
  TimeWarpEdit(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, bool setPosAvailable, QWidget *parent = nullptr);

  void setDisplayAndWarpToTime(int val);
  void warpToTime();
  int getTimeWarpValue();
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
  int m_timeWarpValue, m_preDragTimeWarpValue;
  QTimer *m_timer;
  QRegularExpression *m_valueMatcher;

  void editingCancelled();
  void setTimeWarpValue(int val);
  void displayTimeWarpValue();
  void readSetDisplayAndWarpToTime();
  void readAndSetTimeWarpValue();
  int textToInt(QString text);
  void flash();

};

#endif // TIMEWARPEDIT_H

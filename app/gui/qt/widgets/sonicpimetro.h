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

#ifndef SONICPIMETRO_H
#define SONICPIMETRO_H

#include <QWidget>
#include <QFrame>
#include <QPushButton>
#include <QMutex>
#include <QPaintEvent>
#include <QSlider>
#include <QLineEdit>
#include "model/sonicpitheme.h"
#include "qt_api_client.h"
#include "api/sonicpi_api.h"
#include "bpmscrubwidget.h"
#include "timewarpedit.h"

class SonicPiMetro : public QWidget
{
    Q_OBJECT
public:
  SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, QWidget *parent = nullptr);

  SonicPiTheme *theme;

  void updateActiveLinkCount(int count);
  void setBPM(double bpm);
  void updateColourTheme();
  void linkEnable();
  void linkDisable();

  void setFocusBPMScrubber();
  void setFocusTimeWarpScrubber();

signals:
  void linkEnabled();
  void linkDisabled();

public slots:
  void tapTempo(int flashDelay=250);

protected:
  void paintEvent(QPaintEvent *event);

private:
  QPushButton *enableLinkButton;
  QPushButton *tapButton;
  QSlider *timeWarpSlider;
  TimeWarpEdit *timeWarpLineEdit;
  bool m_linkEnabled = false;
  int numActiveLinks = 0;
  QMutex *mutex;
  BPMScrubWidget *bpmScrubWidget;

  int numTaps = 0;
  qint64 firstTap = 0;
  qint64 lastTap = 0;

  void toggleLink();
  bool isSetPosAvailable();
  void updateActiveLinkText();
  void updateLinkButtonDisplay();

  std::shared_ptr<SonicPi::QtAPIClient> m_spClient;
  std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
};

#endif // SONICPIMETRO_H

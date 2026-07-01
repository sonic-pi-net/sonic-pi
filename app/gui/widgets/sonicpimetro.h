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

class LinkAudioStreamsWidget;

class SonicPiMetro : public QWidget
{
    Q_OBJECT
public:
  SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, QWidget *parent = nullptr);

  SonicPiTheme *theme;

  void updateActiveLinkCount(int count);
  void setBPM(double bpm);
  void updateColourTheme();
  void linkEnable();   // Link button on.
  void linkDisable();  // Link button off.

  void setFocusBPMScrubber();
  void setFocusTimeWarpScrubber();
  void toggleLink();   // Flips the Link button.

  // Expand or collapse the inline Link Audio Streams panel below the
  // main metro row.
  void toggleLinkAudioStreams();

signals:
  void linkEnabled();
  void linkDisabled();
  // Emitted when the inline Link Audio Streams panel is shown/hidden.
  // MainWindow listens to resize the surrounding dock — the metro
  // dock is hard-capped to a small height when collapsed.
  void linkAudioStreamsExpandedChanged(bool expanded);

public slots:
  void tapTempo(int flashDelay=250);
  // Notified by the prefs IO tab when the SuperSonic-wide visibility
  // (Off/Local/Net) changes. Re-evaluates effective visibility and the
  // Link button enabled/greyed state.
  void onSupersonicNetworkVisibilityChanged(int mode);

protected:
  void paintEvent(QPaintEvent *event);

private:
  QPushButton *enableLinkButton = nullptr;
  QPushButton *linkStreamsButton;
  QPushButton *tapButton;
  QSlider *timeWarpSlider;
  TimeWarpEdit *timeWarpLineEdit;
  QPushButton *m_rowVisibility = nullptr;  // Local/Network button on the metro row (a GlyphButton)
  // Embedded inline below the main metro row; toggled by linkStreamsButton.
  LinkAudioStreamsWidget *linkStreamsWidget = nullptr;
  // Independent state pair. Effective Link visibility sent over OSC =
  // m_linkEnabled ? m_networkMode : Off. Visibility scope (Local/Net)
  // is set in Preferences ▶ IO; default is Local.
  bool                                m_linkEnabled = false;
  SonicPi::SonicPiAPI::LinkVisibility m_networkMode =
      SonicPi::SonicPiAPI::LinkVisibility::LoopbackOnly;
  int numActiveLinks = 0;
  QMutex *mutex;
  BPMScrubWidget *bpmScrubWidget;

  int numTaps = 0;
  qint64 firstTap = 0;
  qint64 lastTap = 0;


  bool isSetPosAvailable();
  void updateRowVisibility();      // Sync the button's glyph / public state / tip.
  void updateActiveLinkText();
  void updateLinkButtonDisplay();
  void pushLinkConfigToServer();  // Sends peer-name then effective visibility.

  std::shared_ptr<SonicPi::QtAPIClient> m_spClient;
  std::shared_ptr<SonicPi::SonicPiAPI> m_spAPI;
};

#endif // SONICPIMETRO_H

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

#include "sonicpimetro.h"
#include <QStyle>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QSettings>
#include "qt_api_client.h"
#include "linkaudiostreamswidget.h"
#include <QStyleOption>
#include <QPainter>
#include <QSpacerItem>
#include <QThread>
#include "dpi.h"

SonicPiMetro::SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, QWidget* parent)
  : QWidget(parent)
  , m_spClient(spClient)
  , m_spAPI(spAPI)
{
  this->theme = theme;
  bool setPosAvailable = isSetPosAvailable();
  mutex = new QMutex;

  // Gated by network visibility (Off/Local/Net) in Preferences > IO:
  // greyed out when Off.
  enableLinkButton = new QPushButton(tr("Link"));
  enableLinkButton->setAutoFillBackground(true);
  enableLinkButton->setObjectName("enableLinkButton");
  enableLinkButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  enableLinkButton->setFlat(true);
  #ifdef Q_OS_MAC
  QString link_shortcut = QKeySequence("Ctrl+t").toString(QKeySequence::NativeText);
  #else
  QString link_shortcut = QKeySequence("alt+t").toString(QKeySequence::NativeText);
  #endif
  enableLinkButton->setToolTip(tr(
      "Enable / disable Ableton Link tempo sync.\n\n"
      "Scope (Off / Local / Net) is set under Preferences ▶ IO ▶ "
      "SuperSonic network. Greyed out when the master scope is Off.")
      + "\n(" + link_shortcut + ")");

  tapButton = new QPushButton(tr("Tap"));
  tapButton->setAutoFillBackground(true);
  tapButton->setObjectName("tapButton");
  tapButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  tapButton->setFlat(true);

  tapButton->setToolTip(tr("Tap tempo.\n\nClick repeatedly to the beat to set the BPM manually.\nAccuracy increases with every additional click.") + "\n(" + QKeySequence("Shift+Return").toString(QKeySequence::NativeText) + ")");


  timeWarpSlider = new QSlider(Qt::Horizontal, this);
  timeWarpSlider->setAutoFillBackground(true);
  timeWarpSlider->setObjectName("timeWarpSlider");
  timeWarpSlider->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  timeWarpSlider->setTickPosition(QSlider::TicksBelow);
  timeWarpSlider->setToolTip(tr("Global Time Warp.\n\nSlide to shift the phase of all triggered synths / FX and sent MIDI/OSC events.\nNegative values trigger everything earlier, positive values trigger things later.\nThe unit is milliseconds."));
  timeWarpSlider->setMinimum(-250);
  timeWarpSlider->setMaximum(999);
  timeWarpSlider->setValue(0);


  timeWarpLineEdit = new TimeWarpEdit(m_spClient, m_spAPI, theme, setPosAvailable);
  timeWarpLineEdit->setAutoFillBackground(true);
  timeWarpLineEdit->setObjectName("timeWarpEdit");
  timeWarpLineEdit->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

  timeWarpLineEdit->setToolTip(tr("Global Time Warp.\n\nAdjust to shift the phase of all triggered synths / FX and sent MIDI/OSC events.\nNegative values trigger everything earlier, positive values trigger things later.\nEdit, drag or scroll to modify. Double click to reset to 0. The unit is milliseconds."));

  connect(timeWarpSlider, &QSlider::valueChanged, [this](int value) {
    QSignalBlocker blocker(timeWarpLineEdit);
    timeWarpLineEdit->setDisplayAndWarpToTime(value);
  });

  connect(timeWarpLineEdit, &QLineEdit::textChanged, [this](QString text) {
    QSignalBlocker blocker(timeWarpSlider);
    timeWarpSlider->setValue(timeWarpLineEdit->getTimeWarpValue());
  });

  bpmScrubWidget = new BPMScrubWidget(m_spClient, m_spAPI, theme, setPosAvailable);
  bpmScrubWidget->setObjectName("bpmScrubber");
  bpmScrubWidget->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  bpmScrubWidget->setToolTip(tr("Current Link Tempo in BPM (Beats Per Minute).\n\nEdit, drag or scroll to modify. Double click to reset to 60."));

  // Expand/collapse the inline Link Audio Streams panel. Up-arrow when
  // collapsed, down-arrow when expanded.
  linkStreamsButton = new QPushButton(QString::fromUtf8("\xe2\x86\x91"));  // ↑
  linkStreamsButton->setObjectName("linkStreamsButton");
  linkStreamsButton->setCheckable(true);
  linkStreamsButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  linkStreamsButton->setFlat(true);
  linkStreamsButton->setToolTip(tr("Show / hide the Link Audio streams panel."));

  // The three groups (Link / Tap+BPM / TimeWarp) spread across the row's width
  // via expanding inner spacers; the row itself is capped to the same natural
  // width as the streams panel below (see metroRowWidget), so they line up.
  QHBoxLayout* metro_row = new QHBoxLayout;
  metro_row->setContentsMargins(0, 0, 0, 0);
  metro_row->addWidget(enableLinkButton);
  metro_row->addWidget(linkStreamsButton);
  metro_row->addSpacerItem(new QSpacerItem(ScaleWidthForDPI(30), 0, QSizePolicy::Expanding, QSizePolicy::Fixed));
  metro_row->addWidget(tapButton);
  metro_row->addWidget(bpmScrubWidget);
  metro_row->addSpacerItem(new QSpacerItem(ScaleWidthForDPI(30), 0, QSizePolicy::Expanding, QSizePolicy::Fixed));
  metro_row->addWidget(timeWarpSlider);
  metro_row->addWidget(timeWarpLineEdit);

  // Same capped natural width as the streams panel, left-aligned, so the bottom
  // controls and the panel above share one column.
  QWidget* metroRowWidget = new QWidget(this);
  metroRowWidget->setLayout(metro_row);
  metroRowWidget->setMaximumWidth(640);

  // Hidden by default; toggled by linkStreamsButton.
  linkStreamsWidget = new LinkAudioStreamsWidget(m_spAPI, this);
  linkStreamsWidget->setVisible(false);

  // Streams panel sits just above the anchored metro row.
  // Stack the streams panel and the metro row in one fixed-width, left-aligned
  // column so they're always the same width — and crucially, so the metro row
  // doesn't change width when the streams panel is shown or hidden (a content-
  // sized column would shrink to the metro row's natural width when collapsed).
  QWidget* linkColumn = new QWidget(this);
  QVBoxLayout* colLayout = new QVBoxLayout(linkColumn);
  colLayout->setContentsMargins(0, 0, 0, 0);
  colLayout->setSpacing(0);
  colLayout->addWidget(linkStreamsWidget);
  colLayout->addWidget(metroRowWidget);
  // Size the column to the identity controls' natural width (Link Name /
  // Latency / Stream Audio / Visibility). The peer table and the metro row both
  // match this — the metro row's expanding spacers shrink so it fits — and the
  // top controls fill it exactly (no trailing slack). Fixed so it doesn't move
  // when the streams panel is shown or hidden.
  linkColumn->setFixedWidth(linkStreamsWidget->controlsNaturalWidth());

  QVBoxLayout* metro_layout = new QVBoxLayout;
  metro_layout->addStretch(1);
  metro_layout->addWidget(linkColumn, 0, Qt::AlignLeft);
  setLayout(metro_layout);

  // Restore visibility scope from QSettings (Local default). Link enable
  // is not persisted: joining a mesh is a per-session opt-in.
  {
    QSettings s;
    const int savedNet = s.value("supersonic/networkVisibility", 1).toInt();
    if (savedNet == 1 || savedNet == 2) {
      m_networkMode = static_cast<SonicPi::SonicPiAPI::LinkVisibility>(savedNet);
    }
    m_linkEnabled = false;
    pushLinkConfigToServer();
    updateLinkButtonDisplay();
  }

  connect(enableLinkButton, &QPushButton::clicked, [this]() {
    this->toggleLink();
  });

  connect(linkStreamsButton, &QPushButton::clicked, [this]() {
    this->toggleLinkAudioStreams();
  });

  connect(tapButton, &QPushButton::clicked, [this]() {
    this->tapTempo(100);
  });

  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateNumActiveLinks, this, &SonicPiMetro::updateActiveLinkCount);
  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateBPM, this, &SonicPiMetro::setBPM);

  updateColourTheme();
}

bool SonicPiMetro::isSetPosAvailable()
{
  QPoint pos, new_pos;
  pos = QCursor::pos();
  QGuiApplication::setOverrideCursor(QCursor(Qt::BlankCursor));
  QCursor::setPos(QPoint(0, 0));
  QThread::msleep(250);
  new_pos = QCursor::pos();
  bool available = pos != new_pos;
  QGuiApplication::restoreOverrideCursor();
  QCursor::setPos(pos);
  return available;
}

void SonicPiMetro::onSupersonicNetworkVisibilityChanged(int mode)
{
  if (mode != 1 && mode != 2) return;
  mutex->lock();
  m_networkMode = static_cast<SonicPi::SonicPiAPI::LinkVisibility>(mode);
  pushLinkConfigToServer();
  updateLinkButtonDisplay();
  mutex->unlock();
  // Keep the streams widget's header/empty-state/slider in sync.
  if (linkStreamsWidget) linkStreamsWidget->applyMasterVisibility(mode);
}

void SonicPiMetro::linkEnable()
{
  mutex->lock();
  if (!m_linkEnabled) {
    m_linkEnabled = true;
    pushLinkConfigToServer();
    emit linkEnabled();
  }
  updateLinkButtonDisplay();
  if (linkStreamsWidget) linkStreamsWidget->applyLinkEnabled(true);
  mutex->unlock();
}

void SonicPiMetro::linkDisable()
{
  mutex->lock();
  if (m_linkEnabled) {
    m_linkEnabled = false;
    pushLinkConfigToServer();
    emit linkDisabled();
  }
  updateLinkButtonDisplay();
  if (linkStreamsWidget) linkStreamsWidget->applyLinkEnabled(false);
  mutex->unlock();
}

void SonicPiMetro::toggleLink()
{
  if (m_linkEnabled) linkDisable(); else linkEnable();
}

void SonicPiMetro::pushLinkConfigToServer()
{
  if (!m_spAPI) return;
  // Peer-name first so peers see it. Idempotent on SuperSonic.
  const QString name = QSettings().value("link/peerName", QStringLiteral("Sonic Pi")).toString();
  m_spAPI->SetLinkPeerName(name.toStdString());
  // Link button gates Link; master scope forces Off when Link is disabled.
  const auto effective = m_linkEnabled
      ? m_networkMode
      : SonicPi::SonicPiAPI::LinkVisibility::Off;
  m_spAPI->SetLinkVisibility(effective);
}

void SonicPiMetro::updateActiveLinkCount(int count)
{
  numActiveLinks = count;
  updateLinkButtonDisplay();
}

void SonicPiMetro::updateActiveLinkText()
{
  if (!m_linkEnabled) {
    enableLinkButton->setText(tr("Link"));
  } else if (numActiveLinks == 1) {
    enableLinkButton->setText(tr("1 Link"));
  } else {
    enableLinkButton->setText(tr("%1 Links").arg(numActiveLinks));
  }
}

void SonicPiMetro::updateLinkButtonDisplay()
{
  updateActiveLinkText();
  // Active look driven by the linkOn dynamic property (see app.qss).
  enableLinkButton->setProperty("linkOn", m_linkEnabled);
  enableLinkButton->style()->unpolish(enableLinkButton);
  enableLinkButton->style()->polish(enableLinkButton);
  enableLinkButton->update();

  if (m_linkEnabled) bpmScrubWidget->setLinkEnabled();
  else bpmScrubWidget->setLinkDisabled();
}

void SonicPiMetro::setBPM(double bpm)
{
  bpmScrubWidget->setAndDisplayBPM(bpm);
}

void SonicPiMetro::updateColourTheme()
{
  // Qt re-applies the app-wide stylesheet itself; just re-evaluate
  // state-dependent styling.
  updateLinkButtonDisplay();
}

 void SonicPiMetro::paintEvent(QPaintEvent *)
 {
     QStyleOption opt;
     opt.initFrom(this);
     QPainter p(this);
     style()->drawPrimitive(QStyle::PE_Widget, &opt, &p, this);
 }

void SonicPiMetro::tapTempo(int flashDelay)
{
  qint64 timeStamp = QDateTime::currentMSecsSinceEpoch();

  // Flash driven by the flashing dynamic property (see app.qss).
  auto setFlashing = [this](bool on) {
    tapButton->setProperty("flashing", on);
    tapButton->style()->unpolish(tapButton);
    tapButton->style()->polish(tapButton);
    tapButton->update();
  };
  setFlashing(true);
  QTimer::singleShot(flashDelay, this, [=]() { setFlashing(false); });

  numTaps = numTaps + 1;

  if(numTaps == 1) {
    firstTap = timeStamp;
  } else {

    double timeSinceLastTap = (double)(timeStamp - lastTap);
    double totalTapDistance = (double)(timeStamp - firstTap);
    double avgDistance = totalTapDistance / (numTaps - 1);

    //make sure the first three taps are similarly spaced
    if (((numTaps < 3) &&
         ((timeSinceLastTap > (avgDistance + 30)) ||
          (timeSinceLastTap < (avgDistance - 30)))) ||

        //drop the accuracy requirement for later taps as the error
        //introduced by input timing jitter of the last tap reduces as
        //the tap count increases.
        ((timeSinceLastTap > (avgDistance + 50)) ||
         (timeSinceLastTap < (avgDistance - 50))))
      {
        bpmScrubWidget->displayResetVisualCue();
        numTaps = 1;
        firstTap = timeStamp;

      } else if (numTaps > 2) {
      double newBpm = round(60.0 / (avgDistance / 1000.0));
      if(newBpm != bpmScrubWidget->getBPM()) {
        bpmScrubWidget->setDisplayAndSyncBPM(newBpm);
        bpmScrubWidget->displayBPMChangeVisualCue();
      }
    }
  }
  lastTap = timeStamp;
}

void SonicPiMetro::setFocusBPMScrubber()
{
  bpmScrubWidget->setAccessibleName(tr("BPM Scrubber"));
  bpmScrubWidget->setFocusPolicy(Qt::StrongFocus);
  bpmScrubWidget->setFocus();
  bpmScrubWidget->raise();
  bpmScrubWidget->setVisible(true);

}

void SonicPiMetro::setFocusTimeWarpScrubber()
{
  timeWarpLineEdit->setAccessibleName(tr("Time Warp Scrubber"));
  timeWarpLineEdit->setFocusPolicy(Qt::StrongFocus);
  timeWarpLineEdit->setFocus();
  timeWarpLineEdit->raise();
  timeWarpLineEdit->setVisible(true);
}

void SonicPiMetro::toggleLinkAudioStreams()
{
  if (!linkStreamsWidget) return;
  const bool nowVisible = !linkStreamsWidget->isVisible();
  linkStreamsWidget->setVisible(nowVisible);
  linkStreamsButton->setChecked(nowVisible);
  // ↓ when expanded, ↑ when collapsed.
  linkStreamsButton->setText(nowVisible
      ? QString::fromUtf8("\xe2\x86\x93")
      : QString::fromUtf8("\xe2\x86\x91"));
  emit linkAudioStreamsExpandedChanged(nowVisible);
}

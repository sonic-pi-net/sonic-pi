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
#include <QVBoxLayout>
#include "qt_api_client.h"
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
  enableLinkButton->setToolTip(tr("Enable/Disable network sync.\n\nThis controls whether the Link metronome will synchronise with other Link metronomes on the local WiFi/ethernet network.\nWhen enabled, BPM changes to this metronome will also change all other Link metronomes on the network\nand changes to any other Link metronome will affect this metronome.") + "\n(" + link_shortcut + ")");

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

  connect(timeWarpSlider, &QSlider::valueChanged, [=](int value) {
    QSignalBlocker blocker(timeWarpLineEdit);
    timeWarpLineEdit->setDisplayAndWarpToTime(value);
  });

  connect(timeWarpLineEdit, &QLineEdit::textChanged, [=](QString text) {
    QSignalBlocker blocker(timeWarpSlider);
    timeWarpSlider->setValue(timeWarpLineEdit->getTimeWarpValue());
  });

  bpmScrubWidget = new BPMScrubWidget(m_spClient, m_spAPI, theme, setPosAvailable);
  bpmScrubWidget->setObjectName("bpmScrubber");
  bpmScrubWidget->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  bpmScrubWidget->setToolTip(tr("Current Link Tempo in BPM (Beats Per Minute).\n\nEdit, drag or scroll to modify. Double click to reset to 60."));

  QHBoxLayout* metro_layout  = new QHBoxLayout;
  metro_layout->addWidget(enableLinkButton);
  metro_layout->addSpacerItem(new QSpacerItem(ScaleWidthForDPI(30), 0, QSizePolicy::Maximum, QSizePolicy::Fixed));
  metro_layout->addWidget(tapButton);
  metro_layout->addWidget(bpmScrubWidget);
  metro_layout->addSpacerItem(new QSpacerItem(ScaleWidthForDPI(30), 0, QSizePolicy::Maximum, QSizePolicy::Fixed));
  metro_layout->addWidget(timeWarpSlider);
  metro_layout->addWidget(timeWarpLineEdit);
  metro_layout->addSpacerItem(new QSpacerItem(ScaleWidthForDPI(30), 0, QSizePolicy::MinimumExpanding, QSizePolicy::Fixed));

  setLayout(metro_layout);

  connect(enableLinkButton, &QPushButton::clicked, [=]() {
    this->toggleLink();
  });

  connect(tapButton, &QPushButton::clicked, [=]() {
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

void SonicPiMetro::linkEnable()
{
  mutex->lock();

  if(!m_linkEnabled) {
    m_spAPI.get()->LinkEnable();
    m_linkEnabled = true;
  }

  emit linkEnabled();
  updateLinkButtonDisplay();
  mutex->unlock();
}

void SonicPiMetro::linkDisable()
{

  mutex->lock();

  if(m_linkEnabled) {
    m_spAPI.get()->LinkDisable();
    m_linkEnabled = false;
  }

  emit linkDisabled();
  updateLinkButtonDisplay();
  mutex->unlock();
}

void SonicPiMetro::toggleLink()
{
  mutex->lock();
  m_linkEnabled = !m_linkEnabled;

  if(m_linkEnabled) {
    m_spAPI.get()->LinkEnable();
    emit linkEnabled();
  }
  else {
    m_spAPI.get()->LinkDisable();
    emit linkDisabled();
  }

  updateLinkButtonDisplay();
  mutex->unlock();
}

void SonicPiMetro::updateActiveLinkCount(int count)
{
  numActiveLinks = count;
  updateLinkButtonDisplay();
}

void SonicPiMetro::updateActiveLinkText()
{
  if(numActiveLinks == 1) {
    enableLinkButton->setText("1 Link");
  } else {
    enableLinkButton->setText(QString("%1 Links").arg(numActiveLinks));
  }
}

void SonicPiMetro::updateLinkButtonDisplay()
{
  QString qss;
  if(m_linkEnabled) {
    updateActiveLinkText();
    qss = QString("\nQPushButton {\nbackground-color: %1;}\nQPushButton::hover:!pressed {\nbackground-color: %2}\n").arg(theme->color("PressedButton").name()).arg(theme->color("PressedButton").name());
    enableLinkButton->setStyleSheet(theme->getAppStylesheet() + qss);
    tapButton->setStyleSheet(theme->getAppStylesheet());
    bpmScrubWidget->setLinkEnabled();

  } else {
    enableLinkButton->setText("Link");
    enableLinkButton->setStyleSheet(theme->getAppStylesheet());
    tapButton->setStyleSheet(theme->getAppStylesheet());
    bpmScrubWidget->setLinkDisabled();
  }
}

void SonicPiMetro::setBPM(double bpm)
{
  bpmScrubWidget->setAndDisplayBPM(bpm);
}

void SonicPiMetro::updateColourTheme()
{
  updateLinkButtonDisplay();
  timeWarpSlider->setStyleSheet(theme->getAppStylesheet());
  timeWarpLineEdit->setStyleSheet(theme->getAppStylesheet());
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

  QString qss = QString("\nQPushButton#tapButton\n {\nborder-color: %1;\nbackground-color: %2;\ncolor: %3;\n}\n").arg(theme->color("Pane").name()).arg(theme->color("PressedButton").name()).arg(theme->color("ButtonText").name());
  tapButton->setStyleSheet(theme->getAppStylesheet() + qss);

  QTimer::singleShot(flashDelay, this, [=]() {
    tapButton->setStyleSheet(theme->getAppStylesheet());
  });

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
  bpmScrubWidget->setFocusPolicy(Qt::StrongFocus);
  bpmScrubWidget->setFocus();
  bpmScrubWidget->raise();
  bpmScrubWidget->setVisible(true);

}

void SonicPiMetro::setFocusTimeWarpScrubber()
{
  timeWarpLineEdit->setFocusPolicy(Qt::StrongFocus);
  timeWarpLineEdit->setFocus();
  timeWarpLineEdit->raise();
  timeWarpLineEdit->setVisible(true);
}

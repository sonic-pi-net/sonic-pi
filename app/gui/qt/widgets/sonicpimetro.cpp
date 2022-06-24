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

SonicPiMetro::SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, std::shared_ptr<SonicPi::SonicPiAPI> spAPI, SonicPiTheme *theme, QWidget* parent)
  : QWidget(parent)
  , m_spClient(spClient)
  , m_spAPI(spAPI)
{
  this->theme = theme;
  mutex = new QMutex;
  enableLinkButton = new QPushButton(tr("Link"));
  enableLinkButton->setAutoFillBackground(true);
  enableLinkButton->setObjectName("enableLinkButton");
  enableLinkButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  enableLinkButton->setFlat(true);
  enableLinkButton->setToolTip(tr("Enable/Disable network sync.\nThis controls whether the Link metronome will synchronise with other Link metronomes on the local network."));

  tapButton = new QPushButton(tr("Tap"));
  tapButton->setAutoFillBackground(true);
  tapButton->setObjectName("tapButton");
  tapButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  tapButton->setFlat(true);
  tapButton->setToolTip(tr("Tap tempo.\nClick repeatedly to the beat to set the BPM manually.\nAccuracy increases with every additional click."));


  QHBoxLayout* metro_layout  = new QHBoxLayout;
  QWidget* spacer = new QWidget();
  spacer->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
  setLayout(metro_layout);

  bpmScrubWidget = new BPMScrubWidget(m_spClient, m_spAPI, theme);
  bpmScrubWidget->setObjectName("bpmScrubber");
  bpmScrubWidget->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
  bpmScrubWidget->setToolTip(tr("Current Link BPM. Edit or drag to modify."));
  metro_layout->addWidget(enableLinkButton);
  metro_layout->addWidget(tapButton);
  metro_layout->addWidget(bpmScrubWidget);
  metro_layout->addWidget(spacer);

  connect(enableLinkButton, &QPushButton::clicked, [=]() {
    this->toggleLink();
  });

  connect(tapButton, &QPushButton::clicked, [=]() {
    this->tapTempo();
  });

  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateNumActiveLinks, this, &SonicPiMetro::updateActiveLinkCount);

  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateBPM, this, &SonicPiMetro::setBPM);



  updateLinkButtonDisplay();
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
  QPalette palette = enableLinkButton->palette();
  QString qss;
  if(m_linkEnabled) {
    updateActiveLinkText();
    qss = QString("\nQPushButton {\nbackground-color: %1;}\nQPushButton::hover:!pressed {\nbackground-color: %2}\n").arg(theme->color("PressedButton").name()).arg(theme->color("PressedButton").name());
    enableLinkButton->setStyleSheet(theme->getAppStylesheet() + qss);

    qss = QString("\nQLineEdit#bpmScrubber {\nborder-color: %1;}\n \nQLineEdit#bpmScrubber::hover:!pressed {\nbackground-color: %2;}\n").arg(theme->color("PressedButton").name()).arg(theme->color("PressedButton").name());
    bpmScrubWidget->setStyleSheet(theme->getAppStylesheet() + qss);

  } else {
    enableLinkButton->setText("Link");
    qss = QString("\nQPushButton {\nbackground-color: %1;}\nQPushButton::hover:!pressed {\nbackground-color: %2}\n").arg(theme->color("Button").name()).arg(theme->color("HoverButton").name());
    enableLinkButton->setStyleSheet(theme->getAppStylesheet() + qss);

    qss = QString("\nQLineEdit#bpmScrubber {\nborder-color: %1;}\n \nQLineEdit#bpmScrubber::hover:!pressed {\nbackground-color: %2;}\n").arg(theme->color("HoverButton").name()).arg(theme->color("HoverButton").name());

    bpmScrubWidget->setStyleSheet(theme->getAppStylesheet() + qss);
  }
}

void SonicPiMetro::setBPM(double bpm)
{
  bpmScrubWidget->setAndDisplayBPM(bpm);
}

void SonicPiMetro::updateColourTheme()
{

  updateLinkButtonDisplay();
}

 void SonicPiMetro::paintEvent(QPaintEvent *)
 {
     QStyleOption opt;
     opt.initFrom(this);
     QPainter p(this);
     style()->drawPrimitive(QStyle::PE_Widget, &opt, &p, this);
 }

void SonicPiMetro::tapTempo()
{
  qint64 timeStamp = QDateTime::currentMSecsSinceEpoch();
  numTaps = numTaps + 1;
  qint64 timeSinceLastTap = timeStamp - lastTap;
  lastTap = timeStamp;

  if(numTaps == 1) {
    firstTap = timeStamp;
  } else {
    // if we're not the first tap and it's been a sensible amount of
    // time since the last tap...
    if((timeSinceLastTap < 2500) && (timeSinceLastTap > 0)) {
      qint64 totalTapDistance = timeStamp - firstTap;
      qint64 avgDistance = totalTapDistance / (numTaps - 1);

      if((timeSinceLastTap < (1.1 * avgDistance)) &&
         (timeSinceLastTap > (0.9 * avgDistance))) {
        double newBpm = 60.0 / (((double) avgDistance) / 1000.0);
        bpmScrubWidget->setDisplayAndSyncBPM(newBpm);
      } else {
        //reset if we're not within 10% of average tap distance
        numTaps = 0;
        firstTap = 0;
        lastTap = 0;
      }
    } else {
      //reset as too much time has elapsed since the last tap
      numTaps = 0;
      firstTap = 0;
      lastTap = 0;
    }
  }
}

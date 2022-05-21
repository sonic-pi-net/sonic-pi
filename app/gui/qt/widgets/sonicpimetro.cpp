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

SonicPiMetro::SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, QWidget* parent)
  : QWidget(parent)
  , m_spClient(spClient)
{
  mutex = new QMutex;
  enableLinkButton = new QPushButton(tr("Enable Link"));;
  QHBoxLayout* metro_layout  = new QHBoxLayout;
  metro_layout->addWidget(enableLinkButton);
  setLayout(metro_layout);

  connect(enableLinkButton, &QPushButton::clicked, [=]() {
    this->toggleLink();
  });

  connect(m_spClient.get(), &SonicPi::QtAPIClient::UpdateNumActiveLinks, this, &SonicPiMetro::updateActiveLinkCount);
}

void SonicPiMetro::toggleLink()
{
  mutex->lock();
  if(linkEnabled) {
    enableLinkButton->setText("Enable Link");
    linkEnabled = false;
  } else {
    lockedUpdateActiveLinkText();
    linkEnabled = true;
  }
  mutex->unlock();
}

void SonicPiMetro::updateActiveLinkText()
{
  mutex->lock();
  lockedUpdateActiveLinkText();
  mutex->unlock();
}

void SonicPiMetro::updateActiveLinkCount(int count)
{
  mutex->lock();
  numActiveLinks = count;
  lockedUpdateActiveLinkText();
  mutex->unlock();
}

void SonicPiMetro::lockedUpdateActiveLinkText()
{
  // assumes mutex is locked
  if(numActiveLinks == 1) {
    enableLinkButton->setText("1 Link");
  } else {
    enableLinkButton->setText(QString("%1 Links").arg(numActiveLinks));
  }
}

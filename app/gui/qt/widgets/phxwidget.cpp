//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2021 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include <iostream>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QDesktopServices>

#include "phxwidget.h"
#include "phxwebview.h"
#include "dpi.h"

PhxWidget::PhxWidget(QWidget *parent)
  : QWidget(parent)
{
  phxAlive = false;
  phxView = new PhxWebView(this);
  QSizePolicy sp_retain = phxView->sizePolicy();
  sp_retain.setRetainSizeWhenHidden(true);
  phxView->setSizePolicy(sp_retain);
  phxView->hide();
  mainLayout = new QHBoxLayout(this);
  topRowSubLayout = new QVBoxLayout();
  sizeDownButton = new QPushButton("-");
  sizeUpButton = new QPushButton("+");
  openExternalBrowserButton = new QPushButton(" E ");
  resetBrowserButton = new QPushButton(" R ");

  sizeDownButton->setFixedSize(ScaleForDPI(38, 38));
  sizeUpButton->setFixedSize(ScaleForDPI(38, 38));
  openExternalBrowserButton->setFixedHeight(ScaleHeightForDPI(38));

  topRowSubLayout->addStretch(1);
  topRowSubLayout->addWidget(resetBrowserButton, 0, Qt::AlignRight);
  topRowSubLayout->addWidget(openExternalBrowserButton, 0, Qt::AlignRight);
  topRowSubLayout->addWidget(sizeDownButton, 0, Qt::AlignRight);
  topRowSubLayout->addWidget(sizeUpButton, 0, Qt::AlignRight);

  mainLayout->addWidget(phxView, 1);
  mainLayout->addLayout(topRowSubLayout);

  connect(sizeDownButton, &QPushButton::released, this, &PhxWidget::handleSizeDown);
  connect(sizeUpButton, &QPushButton::released, this, &PhxWidget::handleSizeUp);
  connect(openExternalBrowserButton, &QPushButton::released, this, &PhxWidget::handleOpenExternalBrowser);
  connect(resetBrowserButton, &QPushButton::released, this, &PhxWidget::handleResetBrowser);
  connect(phxView, &PhxWebView::loadFinished, this, &PhxWidget::handleLoadFinished);
}

void PhxWidget::handleSizeDown()
{
  // zoom out of webview
  // min zoom is 0.25
  qreal size = phxView->zoomFactor();
  size = size - 0.2;
  if (size < 0.25) {
    size = 0.25;
  }

  phxView->setZoomFactor(size);
  // resize button
}

void PhxWidget::handleSizeUp()
{
  // zoom into webview
  // max zoom is 5.0
  qreal size = phxView->zoomFactor();
  size = size + 0.2;
  if (size > 5.0) {
    size = 5.0;
  }

  phxView->setZoomFactor(size);
}

void PhxWidget::handleOpenExternalBrowser()
{
  QDesktopServices::openUrl(phxView->url());
}

void PhxWidget::connectToTauPhx(QUrl url)
{
  defaultUrl = url;
  std::cout << "[PHX] - connecting to: " << url.toString().toStdString() << std::endl;
  phxView->load(url);
}

void PhxWidget::handleLoadFinished(bool ok)
{
  if(ok) {
    if(!phxAlive) {
      std::cout << "[PHX] - initial load finished" << std::endl;
      phxAlive = true;
      phxView->show();
    }
  } else {
    std::cout << "[PHX] - load error" << std::endl;
    phxView->load(defaultUrl);
  }
}

void PhxWidget::handleResetBrowser()
{
  phxView->load(defaultUrl);
}

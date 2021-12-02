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
  phxView = new PhxWebView(this);
  mainLayout = new QVBoxLayout(this);
  topRowSubLayout = new QHBoxLayout(this);
  sizeDownButton = new QPushButton("-");
  sizeUpButton = new QPushButton("+");
  openExternalBrowserButton = new QPushButton("External Viewer");

  sizeDownButton->setFixedSize(ScaleForDPI(38, 38));
  sizeUpButton->setFixedSize(ScaleForDPI(38, 38));
  openExternalBrowserButton->setFixedHeight(ScaleHeightForDPI(38));

  topRowSubLayout->addStretch(1);
  topRowSubLayout->addWidget(openExternalBrowserButton, 0, Qt::AlignRight);
  topRowSubLayout->addWidget(sizeDownButton, 0, Qt::AlignRight);
  topRowSubLayout->addWidget(sizeUpButton, 0, Qt::AlignRight);

  mainLayout->addLayout(topRowSubLayout);
  mainLayout->addWidget(phxView);

  connect(sizeDownButton, &QPushButton::released, this, &PhxWidget::handleSizeDown);
  connect(sizeUpButton, &QPushButton::released, this, &PhxWidget::handleSizeUp);
  connect(openExternalBrowserButton, &QPushButton::released, this, &PhxWidget::handleOpenExternalBrowser);
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

void PhxWidget::load(QUrl url)
{
  phxView->load(url);
}

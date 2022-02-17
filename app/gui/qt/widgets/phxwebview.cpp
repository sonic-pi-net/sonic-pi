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

#include <QWebEngineView>
#include "phxwebview.h"

PhxWebView::PhxWebView(QWidget *parent)
    : QWebEngineView(parent)
{
    phxProfile = new QWebEngineProfile();
    phxPage = new QWebEnginePage(phxProfile);
    phxPage->setParent(this);
    phxProfile->setParent(this);
    setPage(phxPage);
    setContextMenuPolicy(Qt::NoContextMenu);
    setZoomFactor(2.0);
}

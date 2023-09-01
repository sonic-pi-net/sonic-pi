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

#ifndef PHXWEBVIEW_H
#define PHXWEBVIEW_H

#include <QWebEngineView>
#include <QWebEngineProfile>

class QWebEngineProfile;
class QWebEnginePage;

class PhxWebView : public QWebEngineView
{
  Q_OBJECT

 public:
    PhxWebView(QWidget *parent = nullptr);
    void setScrollbarColours(QColor foreground, QColor background, QColor hover);

 private:
    void insertStyleSheet(const QString& name, const QString& source);

    QWebEngineProfile *phxProfile;
    QWebEnginePage *phxPage;


};

#endif // PHXWEBVIEW_H

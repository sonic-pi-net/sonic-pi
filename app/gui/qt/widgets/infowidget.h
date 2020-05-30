//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2017 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef INFOWIDGET_H
#define INFOWIDGET_H

#include <QWidget>
#include <QCloseEvent>

class InfoWidget : public QWidget
{
    Q_OBJECT

signals:
    void closed();

public:
    explicit InfoWidget(QWidget *parent = 0);

 private:
    void closeEvent(QCloseEvent *event);
};

#endif

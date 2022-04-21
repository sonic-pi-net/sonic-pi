//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2017 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

# include <QCloseEvent>
# include "infowidget.h"

InfoWidget::InfoWidget(QWidget *parent) : QWidget(parent)
{
}

void InfoWidget::closeEvent(QCloseEvent *event){
  emit closed();
  event->accept();
}

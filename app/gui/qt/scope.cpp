//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md 
//
// Copyright (C) 2016 by Adrian Cheater
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#include "scope.h"

#include <QPaintEvent>
#include <QResizeEvent>
#include <QVBoxLayout>
#include <QIcon>
#include <QTimer>
#include <QPainter>
#include <QDebug>

Scope::Scope( QWidget* parent ) : QWidget(parent), plot(QwtText("Left")), plot2(QwtText("Right"))
{
  resize(640,480);
  setWindowTitle( "Sonic Pi - Scope" );
  setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);
  setWindowIcon(QIcon(":images/icon-smaller.png"));

  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(refreshScope()));
  scopeTimer->start(1);

  for( unsigned int i = 0; i < 4096; ++i )
  {
    sample_x[i] = i;
    sample_y[i] = 0.0f;
    sample_y2[i] = 0.0f;
  }
  plot_curve.setRawSamples( sample_x, sample_y, 4096 );
  plot_curve2.setRawSamples( sample_x, sample_y2, 4096 );

  plot_curve.attach(&plot);
  plot_curve2.attach(&plot2);

  plot.setAxisScale(QwtPlot::Axis::yLeft,-1,1);
  plot.setAxisScale(QwtPlot::Axis::xBottom,0,4096);

  plot2.setAxisScale(QwtPlot::Axis::yLeft,-1,1);
  plot2.setAxisScale(QwtPlot::Axis::xBottom,0,4096);

  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(&plot);
  layout->addWidget(&plot2);
  setLayout(layout);
}

Scope::~Scope()
{
}

void Scope::refreshScope() {
  if( !isVisible() ) 
  {
    return;
  }

  if( shm_reader.valid() )
  {
    unsigned int frames;
    if( shm_reader.pull( frames ) )
    {
      float* data = shm_reader.data();
      for( unsigned int i = 0; i < frames; ++i )
      {
        sample_y[i] = data[i];
      }
      for( unsigned int i = 0; i < frames; ++i )
      {
        sample_y2[i] = data[i+shm_reader.max_frames()];
      }
      plot.replot();
      plot2.replot();
    }
  } else
  {
    shm_client.reset(new server_shared_memory_client(4556));
    shm_reader = shm_client->get_scope_buffer_reader(0);
  }
}

void Scope::resizeEvent( QResizeEvent* p_evt )
{ 
  QWidget::resizeEvent(p_evt);
//  plot.resize(p_evt->size());
//  plot2.resize(p_evt->size());
}

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
#include <cmath>

ScopePanel::ScopePanel( const std::string& name, QWidget* parent ) : QWidget(parent), plot(QwtText(name.c_str()),this), max_y(0), counter(0), channel(0)
{
  for( unsigned int i = 0; i < 4096; ++i )
  {
    sample_x[i] = i;
    sample_y[i] = 0.0f;
  }
  plot_curve.setRawSamples( sample_x, sample_y, 4096 );
  plot_curve.attach(&plot);

  plot.setAxisScale(QwtPlot::Axis::yLeft,-1,1);
  plot.setAxisScale(QwtPlot::Axis::xBottom,0,4096);
  
  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(&plot);
  setLayout(layout);
}

ScopePanel::~ScopePanel()
{
}

void ScopePanel::setChannel( unsigned int i )
{
  channel = i;
}

void ScopePanel::setReader( scope_buffer_reader* shmReader )
{
  reader = shmReader;
}

void ScopePanel::refresh()
{
  if( reader == nullptr ) return;
  qDebug() << "Have reader";
  if( !reader->valid() ) return;
  qDebug() << "Reader is valid";

  unsigned int frames;
  if( reader->pull( frames ) )
  {
    qDebug() << "Reader got " << frames << " frames of audio";
    ++counter;
    float* data = reader->data();
    unsigned int offset = reader->max_frames() * channel;
    for( unsigned int i = 0; i < frames; ++i )
    {
      sample_y[i] = data[i+offset];
      if( fabs(data[i]) > max_y ) max_y = fabs(data[i]);
    }
    if( counter > 50 )
    {
      counter = 0;
      if( max_y == 0 ) max_y = 1;
//      plot.setAxisScale(QwtPlot::Axis::yLeft,-max_y,max_y);
      max_y = 0;
    }
    plot.replot();
  }
}

Scope::Scope( QWidget* parent ) : QWidget(parent), left("Left",this), right("Right",this)
{
  right.setChannel(1);
  resize(640,480);
  setWindowTitle( "Sonic Pi - Scope" );
  setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint );
  setWindowIcon(QIcon(":images/icon-smaller.png"));

  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(refreshScope()));
  scopeTimer->start(1);

  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(&left);
  layout->addWidget(&right);
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

  if( !shmReader.valid() )
  {
    shmClient.reset(new server_shared_memory_client(4556));
    shmReader = shmClient->get_scope_buffer_reader(0);
    left.setReader(&shmReader);
    right.setReader(&shmReader);
  }
  left.refresh();
  right.refresh();
  repaint();
}

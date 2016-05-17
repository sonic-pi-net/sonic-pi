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
#include <qwt_text_label.h>
#include <cmath>

ScopePanel::ScopePanel( const std::string& name, QWidget* parent ) : QWidget(parent), name(name), plot(QwtText(name.c_str()),this), max_y(0), counter(0), channel(0)
{
  for( unsigned int i = 0; i < 4096; ++i )
  {
    sample_x[i] = i;
    sample_y[i] = 0.0f;
  }
  plot_curve.setRawSamples( sample_x, sample_y, 4096 );
  plot_curve.setItemAttribute( QwtPlotItem::AutoScale );
  plot_curve.attach(&plot);
  plot_curve.setPen(QPen(QColor("deeppink"), 2));
#if QWT_VERSION >= 0x60100
  plot_curve.setPaintAttribute( QwtPlotCurve::PaintAttribute::FilterPoints );
#endif

  plot.setAxisScale(QwtPlot::Axis::yLeft,-1,1);
  plot.setAxisScale(QwtPlot::Axis::xBottom,0,4096);
  plot.enableAxis(QwtPlot::Axis::xBottom, false);

  QSizePolicy sp(QSizePolicy::MinimumExpanding,QSizePolicy::Expanding);
  plot.setSizePolicy(sp);

  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(&plot);
  layout->setContentsMargins(0,0,0,0);
  layout->setSpacing(0);
  setLayout(layout);
}

ScopePanel::~ScopePanel()
{
}

bool ScopePanel::setAxes(bool b)
{
  plot.enableAxis(QwtPlot::Axis::yLeft,b);
  if( b )
  {
    plot.setTitle(QwtText(name.c_str()));
  } else
  {
    plot.setTitle(QwtText(""));
  }
  return b;
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
  if( !reader->valid() ) return;

  unsigned int frames;
  if( reader->pull( frames ) )
  {
//    ++counter;
    float* data = reader->data();
    unsigned int offset = reader->max_frames() * channel;
    for( unsigned int i = 0; i < frames; ++i )
    {
      sample_y[i] = data[i+offset];
    }
/*
    if( counter > 100 )
    {
      counter = 0;
      if( max_y == 0 ) max_y = 1;
      if( max_y > 1 ) max_y = 1;
      if( max_y > plot.axisInterval(QwtPlot::Axis::yLeft).maxValue() )
      {
        plot.setAxisScale(QwtPlot::Axis::yLeft,-max_y,max_y);
      }
      max_y = 0;
    }
*/
    plot.replot();
  }
}

Scope::Scope( QWidget* parent ) : QWidget(parent), left("Left",this), right("Right",this)
{
  right.setChannel(1);
  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(refreshScope()));
  scopeTimer->start(4096*1000/44100); // sample size (4096)*1000 ms/s / Sample Rate (Hz)

  QVBoxLayout* layout = new QVBoxLayout();
  layout->setSpacing(0);
  layout->setContentsMargins(0,0,0,0);
  layout->addWidget(&left);
  layout->addWidget(&right);
  setLayout(layout);
}

Scope::~Scope()
{
}

bool Scope::setLeftScope(bool b)
{
  left.setVisible(b);
  return b;
}

bool Scope::setRightScope(bool b)
{
  right.setVisible(b);
  return b;
}

bool Scope::setScopeAxes(bool on)
{
  left.setAxes(on);
  right.setAxes(on);
  return on;
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

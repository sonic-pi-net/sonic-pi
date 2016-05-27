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

ScopePanel::ScopePanel( const std::string& name, double* sample_x, double* sample_y, int num_samples, QWidget* parent ) : QWidget(parent), name(name), plot(QwtText(name.c_str()),this) 
{
#if QWT_VERSION >= 0x60100
  plot_curve.setPaintAttribute( QwtPlotCurve::PaintAttribute::FilterPoints );
#endif

  plot_curve.setRawSamples( sample_x, sample_y, num_samples );
  setXRange( 0, num_samples, false );
  setYRange( -1, 1, true );
  setPen(QPen(QColor("deeppink"), 2));

/*
  if( name == "Lissajous" )
  {
    plot_curve.setRawSamples( sample_x + (4096-1024), sample_y + (4096-1024), 1024 );
    plot_curve.setItemAttribute( QwtPlotItem::AutoScale );
    plot.setAxisScale(QwtPlot::Axis::xBottom,-1,1);
    plot.enableAxis(QwtPlot::Axis::xBottom, true);
    plot_curve.setPen(QPen(QColor("deeppink"), 1));
  }
  else
  {
    plot_curve.setPen(QPen(QColor("deeppink"), 2));
    plot_curve.setItemAttribute( QwtPlotItem::AutoScale );
    plot.setAxisScale(QwtPlot::Axis::xBottom,0,4096);
    plot.enableAxis(QwtPlot::Axis::xBottom, false);
  }
*/
  plot_curve.attach(&plot);

  QSizePolicy sp(QSizePolicy::MinimumExpanding,QSizePolicy::Expanding);
  plot.setSizePolicy(sp);

  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(&plot);
  layout->setContentsMargins(0,0,0,0);
  layout->setSpacing(0);
  setLayout(layout);
}

void ScopePanel::setYRange( float min, float max, bool showLabel )
{
  plot.setAxisScale( QwtPlot::Axis::yLeft, min, max );
  plot.enableAxis( QwtPlot::Axis::yLeft, showLabel );
  defaultShowY = showLabel;
}

void ScopePanel::setXRange( float min, float max, bool showLabel )
{
  plot.setAxisScale( QwtPlot::Axis::xBottom, min, max );
  plot.enableAxis( QwtPlot::Axis::xBottom, showLabel );
  defaultShowX = showLabel;
}

void ScopePanel::setPen( QPen pen )
{
  plot_curve.setPen( pen );
}


ScopePanel::~ScopePanel()
{
}

bool ScopePanel::setAxesVisible(bool b)
{
  plot.enableAxis(QwtPlot::Axis::yLeft,b && defaultShowY );
  plot.enableAxis(QwtPlot::Axis::xBottom,b && defaultShowX );
  if( b )
  {
    plot.setTitle(QwtText(name.c_str()));
  } else
  {
    plot.setTitle(QwtText(""));
  }
  return b;
}

void ScopePanel::refresh( )
{
  if( !plot.isVisible() ) return;
  plot.replot();
}

Scope::Scope( QWidget* parent ) : QWidget(parent), paused( false )
{
  //lissajous("Lissajous", sample[0]+(4096-1024), sample[1]+(4096-1024), 1024, this ), left("Left",sample_x,sample[0],4096,this), right("Right",sample_x,sample[1],4096, this)
  panels.push_back( new ScopePanel("Lissajous", sample[0]+(4096-1024), sample[1]+(4096-1024), 1024, this ) );
  panels.push_back( new ScopePanel("Left",sample_x,sample[0],4096,this) );
  panels.push_back( new ScopePanel("Right",sample_x,sample[1],4096, this) );
  panels[0]->setPen(QPen(QColor("deeppink"), 1));
  panels[0]->setXRange( -1, 1, true );

  for( unsigned int i = 0; i < 4096; ++i ) sample_x[i] = i;
  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(refreshScope()));
  scopeTimer->start(20);

  QVBoxLayout* layout = new QVBoxLayout();
  layout->setSpacing(0);
  layout->setContentsMargins(0,0,0,0);
  layout->addWidget(panels[0]);
  layout->addWidget(panels[1]);
  layout->addWidget(panels[2]);
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
  left.setAxesVisible(on);
  right.setAxesVisible(on);
  return on;
}

void Scope::togglePause() {
  paused = !paused;
}

void Scope::refreshScope() {
  if( paused ) return;
  if( !isVisible() ) return;

  if( !shmReader.valid() )
  {
    shmClient.reset(new server_shared_memory_client(4556));
    shmReader = shmClient->get_scope_buffer_reader(0);
  }

  unsigned int frames;
  if( shmReader.pull( frames ) )
  {
    float* data = shmReader.data();
    for( unsigned int j = 0; j < 2; ++j )
    {
      unsigned int offset = shmReader.max_frames() * j;
      for( unsigned int i = 0; i < 4096 - frames; ++i )
      {
        sample[j][i] = sample[j][i+frames];
      }

      for( unsigned int i = 0; i < frames; ++i )
      {
        sample[j][4096-frames+i] = data[i+offset];
      }
    }
    left.refresh();
    right.refresh();
    lissajous.refresh();
  }
}

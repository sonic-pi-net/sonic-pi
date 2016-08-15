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

ScopeBase::ScopeBase( const QString& name, QWidget* parent ) : QWidget(parent), name(name), defaultShowX(true), defaultShowY(true), plot(QwtText(name),this) 
{
  QSizePolicy sp(QSizePolicy::MinimumExpanding,QSizePolicy::Expanding);
  plot.setSizePolicy(sp);
  
  QVBoxLayout* layout = new QVBoxLayout();
  layout->addWidget(&plot);
  layout->setContentsMargins(0,0,0,0);
  layout->setSpacing(0);
  setLayout(layout);
}

ScopeBase::~ScopeBase()
{
}

const QString& ScopeBase::getName() { return name; }

void ScopeBase::setYRange( float min, float max, bool showLabel )
{
  plot.setAxisScale( QwtPlot::Axis::yLeft, min, max );
  plot.enableAxis( QwtPlot::Axis::yLeft, showLabel );
  defaultShowY = showLabel;
}

void ScopeBase::setXRange( float min, float max, bool showLabel )
{
  plot.setAxisScale( QwtPlot::Axis::xBottom, min, max );
  plot.enableAxis( QwtPlot::Axis::xBottom, showLabel );
  defaultShowX = showLabel;
}

bool ScopeBase::setAxesVisible(bool b)
{
  plot.enableAxis(QwtPlot::Axis::yLeft,b && defaultShowY );
  plot.enableAxis(QwtPlot::Axis::xBottom,b && defaultShowX );
  if( b )
  {
    plot.setTitle(QwtText(name));
  } else
  {
    plot.setTitle(QwtText(""));
  }
  return b;
}

void ScopeBase::refresh( )
{
  if( !plot.isVisible() ) return;
  plot.replot();
}

ScopePanel::ScopePanel( const QString& name, double* sample_x, double* sample_y, int num_samples, QWidget* parent ) : ScopeBase(name,parent)
{
#if QWT_VERSION >= 0x60100
  plot_curve.setPaintAttribute( QwtPlotCurve::PaintAttribute::FilterPoints );
#endif

  plot_curve.setRawSamples( sample_x, sample_y, num_samples );
  setXRange( 0, num_samples, false );
  setYRange( -1, 1, true );
  setPen(QPen(QColor("deeppink"), 2));

  plot_curve.attach(&plot);
}

void ScopePanel::setPen( QPen pen )
{
  plot_curve.setPen( pen );
}

MultiScopePanel::MultiScopePanel( const QString& name, double* sample_x, double samples_y[][4096], unsigned int num_lines, unsigned int num_samples, QWidget* parent ) : ScopeBase(name,parent)
{
  for( unsigned int i = 0; i < num_lines; ++i )
  {
    auto curve = new QwtPlotCurve();
    curve->setPaintAttribute( QwtPlotCurve::PaintAttribute::FilterPoints );
    curve->setRawSamples( sample_x, samples_y[i], 4096 );
    curve->attach(&plot);
    curves.push_back( std::shared_ptr<QwtPlotCurve>(curve) );
  }

  setXRange( 0, num_samples, false );
  setYRange( -1, 1, true );
  setPen(QPen(QColor("deeppink"), 2));
}

void MultiScopePanel::setPen( QPen pen )
{
  for( auto c : curves )
  {
    c.get()->setPen(pen);
  } 
}

Scope::Scope( QWidget* parent ) : QWidget(parent), paused( false ), emptyFrames(0)
{
  std::fill_n(sample[0],4096,0);
  std::fill_n(sample[1],4096,0);
  std::fill_n(sample_mono,4096,0);
  panels.push_back( std::shared_ptr<ScopePanel>(new ScopePanel("Lissajous", sample[0]+(4096-1024), sample[1]+(4096-1024), 1024, this ) ) );
//  panels.push_back( std::shared_ptr<ScopePanel>(new ScopePanel("Left",sample_x,sample[0],4096,this) ) );
//  panels.push_back( std::shared_ptr<ScopePanel>(new ScopePanel("Right",sample_x,sample[1],4096, this) ) );
  panels.push_back( std::shared_ptr<MultiScopePanel>(new MultiScopePanel("Stereo",sample_x,sample,2,4096,this) ) );
  panels.push_back( std::shared_ptr<ScopePanel>(new ScopePanel("Mono",sample_x,sample_mono,4096, this) ) );
  panels[0]->setPen(QPen(QColor("deeppink"), 1));
  panels[0]->setXRange( -1, 1, true );

  for( unsigned int i = 0; i < 4096; ++i ) sample_x[i] = i;
  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(refreshScope()));
  scopeTimer->start(20);

  QVBoxLayout* layout = new QVBoxLayout();
  layout->setSpacing(0);
  layout->setContentsMargins(0,0,0,0);
  for( auto p : panels )
  {
    layout->addWidget(p.get());
  }
  setLayout(layout);
}

Scope::~Scope()
{
}

std::vector<QString> Scope::getScopeNames() const
{
  std::vector<QString> names;
  for( auto scope : panels )
  {
    names.push_back(scope->getName());
  }
  return names;
}

bool Scope::enableScope( const QString& name, bool on )
{
  for( auto scope : panels )
  {
    if( scope->getName() == name )
    {
      scope->setVisible(on);
      return on;
    }
  }
  return true;
}

bool Scope::setScopeAxes(bool on)
{
  for( auto scope : panels )
  {
    scope->setAxesVisible(on);
  }
  return on;
}

void Scope::togglePause() {
  paused = !paused;
}

void Scope::pause() {
  paused = true;
}

void Scope::resume() {
  paused = false;
}

void Scope::resetScope()
{
  shmClient.reset(new server_shared_memory_client(4556));
  shmReader = shmClient->get_scope_buffer_reader(0);
}

void Scope::refreshScope() {
  if( paused ) return;
  if( !isVisible() ) return;

  if( !shmReader.valid() )
  {
    resetScope();
  }

  unsigned int frames;
  if( shmReader.pull( frames ) )
  {
    emptyFrames = 0;
    float* data = shmReader.data();
    for( unsigned int j = 0; j < 2; ++j )
    {
      unsigned int offset = shmReader.max_frames() * j;
      for( unsigned int i = 0; i < 4096 - frames; ++i )
      {
        sample[j][i] = sample[j][i+frames];
        if( j == 0 )
        {
          sample_mono[i] = sample_mono[i+frames]; 
        }
      }

      for( unsigned int i = 0; i < frames; ++i )
      {
        sample[j][4096-frames+i] = data[i+offset];
        auto d = data[i+offset] + 1.0;
        if(j == 0)
        {
          sample_mono[4096-frames+i] = d*d;
        } else
        {
          sample_mono[4096-frames+i] += d*d;
          sample_mono[4096-frames+i] /= 2.0f;
          sample_mono[4096-frames+i] = sqrt(sample_mono[4096-frames+i]) - 1.0;
        }
      }
    }
    
    for( auto scope : panels )
    {
      scope->refresh();
    }
  } else
  {
    ++emptyFrames;
    if( emptyFrames > 10 )
    {
      resetScope();
      emptyFrames = 0;
    }
  }
}

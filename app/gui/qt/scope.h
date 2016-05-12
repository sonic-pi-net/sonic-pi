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

#ifndef SCOPE_H
#define SCOPE_H

#include <QWidget>
#include <qwt_plot.h>
#include <qwt_plot_curve.h>

#include <server_shm.hpp>
#include <memory>
#include <string>

class QPaintEvent;
class QResizeEvent;

class ScopePanel : public QWidget
{
  Q_OBJECT

public:
  ScopePanel( const std::string& name, QWidget* parent = 0 );
  virtual ~ScopePanel();

  void setChannel( unsigned int i );
  void setReader( scope_buffer_reader* shmReader );
  void refresh();

private:
  scope_buffer_reader* reader;
  QwtPlot plot;
  QwtPlotCurve plot_curve;
  double sample_x[4096];
  double sample_y[4096];
  double max_y;
  int counter;
  unsigned int channel;
};

class Scope : public QWidget 
{
  Q_OBJECT

public:
  Scope( QWidget* parent = 0 );
  virtual ~Scope();

private slots:
  void refreshScope();
 
private: 
  std::unique_ptr<server_shared_memory_client> shmClient;
  scope_buffer_reader shmReader;
  ScopePanel left,right;
};

#endif

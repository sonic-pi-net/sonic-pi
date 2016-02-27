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

class QPaintEvent;
class QResizeEvent;

class Scope : public QWidget
{
  Q_OBJECT

public:
  Scope( QWidget* parent = 0 );
  virtual ~Scope();

protected:
  void resizeEvent( QResizeEvent* p_evt );

private slots:
  void refreshScope();
 
private: 
  std::unique_ptr<server_shared_memory_client> shm_client;
  scope_buffer_reader shm_reader;
  QwtPlot plot;
  QwtPlot plot2;
  QwtPlotCurve plot_curve;
  QwtPlotCurve plot_curve2;
  double sample_x[4096];
  double sample_y[4096];
  double sample_y2[4096];
};

#endif

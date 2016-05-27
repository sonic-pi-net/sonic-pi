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
  ScopePanel( const std::string& name, double* sample_x, double* sample_y, int num_samples, QWidget* parent = 0 );
  virtual ~ScopePanel();

  void refresh();
  void setPen( QPen pen );
  void setXRange( float min, float max, bool showLabel = true );
  void setYRange( float min, float max, bool showLabel = true );
  bool setAxesVisible( bool on );

private:
  std::string name;
  QwtPlot plot;
  QwtPlotCurve plot_curve;
  bool defaultShowX, defaultShowY;
};

class Scope : public QWidget 
{
  Q_OBJECT

public:
  Scope( QWidget* parent = 0 );
  virtual ~Scope();

  //void enableScope( const std::string& name, bool on );

  bool setLeftScope(bool on);
  bool setRightScope(bool on);
  bool setScopeAxes(bool on);
  void togglePause();

private slots:
  void refreshScope();
 
private:
  std::unique_ptr<server_shared_memory_client> shmClient;
  double sample_x[4096];
  double sample[2][4096];
  scope_buffer_reader shmReader;
  std::list<std::unique_ptr<ScopePanel>> panels;
  //ScopePanel lissajous, left,right;
  bool paused;
};

#endif

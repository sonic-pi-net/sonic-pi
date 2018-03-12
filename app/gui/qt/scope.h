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

class ScopeBase : public QWidget
{
  Q_OBJECT
public:
  ScopeBase( const QString& name, const QString& title, int scsynthPort, QWidget* parent = 0 );
  virtual ~ScopeBase();

  const QString& getName();
  virtual void setPen( QPen pen ) = 0;

  void refresh();
  void setXRange( float min, float max, bool showLabel = true );
  void setYRange( float min, float max, bool showLabel = true );
  bool setAxesVisible( bool on );

private:
  QString name,title;
  int scsynthPort;
  bool defaultShowX, defaultShowY;

protected:
  QwtPlot plot;
};


class ScopePanel : public ScopeBase
{
public:
  ScopePanel( const QString& name, const QString& title, int scsynthPort, double* sample_x, double* sample_y, int num_samples, QWidget* parent = 0 );

  void setPen( QPen pen );

private:
  QwtPlotCurve plot_curve;
};


class MultiScopePanel : public ScopeBase
{
public:
  MultiScopePanel( const QString& name, const QString& title, int scsynthPort, double* sample_x, double samples_y[][4096], unsigned int num_lines, unsigned int num_samples, QWidget* parent );

  void setPen( QPen pen );

private:
  std::vector<std::shared_ptr<QwtPlotCurve>> curves;
};


class Scope : public QWidget
{
  Q_OBJECT

public:
  Scope( int scsynthPort, QWidget* parent = 0 );
  virtual ~Scope();

  std::vector<QString> getScopeNames() const;
  bool enableScope( const QString& name, bool on );
  bool setScopeAxes(bool on);
  void togglePause();
  void pause();
  void resume();
  void resetScope();
  void refresh();

private slots:
  void drawLoop();

private:
  std::unique_ptr<server_shared_memory_client> shmClient;
  double sample_x[4096];
  double sample[2][4096];
  double sample_mono[4096];
  scope_buffer_reader shmReader;
  std::vector<std::shared_ptr<ScopeBase>> panels;
  bool paused;
  unsigned int emptyFrames;
  int scsynthPort;
};

#endif

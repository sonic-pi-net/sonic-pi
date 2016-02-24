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
#include <QIcon>
#include <QTimer>
#include <QPainter>
#include <QDebug>

Scope::Scope( QWidget* parent ) : QWidget(parent)
{
  setWindowTitle( "Sonic Pi - Scope" );
  setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::CustomizeWindowHint | Qt::WindowStaysOnTopHint);
  setWindowIcon(QIcon(":images/icon-smaller.png"));

  QTimer *scopeTimer = new QTimer(this);
  connect(scopeTimer, SIGNAL(timeout()), this, SLOT(refreshScope()));
  scopeTimer->start(50);
}

Scope::~Scope()
{
}

unsigned int fNum = 0;
void Scope::refreshScope() {
  if( !isVisible() ) 
  {
    fNum = 0;
    return;
  }
  QPainter p(&scope_pixmap);
  p.fillRect(scope_pixmap.rect(), Qt::black);

  float mid = scope_pixmap.height()/2.0;
  p.translate(0, mid);
  
  QPainterPath path;
  path.moveTo(0,0);
  path.lineTo(scope_pixmap.width(),0);
  p.setPen(Qt::white);
  p.drawPath(path);

  path = QPainterPath();
  path.moveTo(0,-mid);
  path.lineTo(scope_pixmap.width(),-mid);
  p.setPen(Qt::blue);
  p.drawPath(path);

  path = QPainterPath();
  path.moveTo(0,mid-1);
  path.lineTo(scope_pixmap.width(),mid-1);
  p.setPen(Qt::red);
  p.drawPath(path);

  qDebug() << "Refresh scope " << fNum; 
  ++fNum;
  if( shm_reader.valid() )
  {
    qDebug() << "Reader is valid";
    unsigned int frames;
    if( shm_reader.pull( frames ) )
    {
      qDebug() << "Got " << frames << " frames of data";
      float* data = shm_reader.data();
      std::ostringstream vals;
      path = QPainterPath();
      path.moveTo(0,data[0]*mid);
  //    vals << data[0]*mid;  
      std::random_device rd;
      std::mt19937 gen(rd());
      std::uniform_real_distribution<> dis(-1, 1);

      for( unsigned int i = 1; i < frames; ++i )
      {
  //      data[i] = dis(gen);
  //      vals << data[i]*mid;
        path.lineTo(i,data[i]*mid);    
      }
  //    qDebug() << vals.str().c_str();
      p.setPen(Qt::yellow);
      p.drawPath(path);
    }
  }
  {
    shm_client.reset(new server_shared_memory_client(4556));
    shm_reader = shm_client->get_scope_buffer_reader(0);
    qDebug() << "Retrying";
  }
  update();
}

void Scope::paintEvent( QPaintEvent* p_evt )
{
  QPainter p(this);
  p.fillRect(p_evt->rect(),Qt::black);
  p.drawPixmap(0,0,scope_pixmap); 
}

void Scope::resizeEvent( QResizeEvent* p_evt )
{ 
  scope_pixmap = QPixmap( p_evt->size() );
}

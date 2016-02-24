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

/*
// This code needs to be executed in scide or sclang in order to instruct
// scsynth to export the audio data for the scope.
// TODO: Have sonic pi's server issue these commands instead
Server.default = Server.new("localhost",NetAddr("127.0.0.1",4556));
Routine( {
    var buffer;
	"Starting thread".postln;
	s.startAliveThread;
	s.initTree;
	s.sync;
	SynthDef( "my_scope_test", { ScopeOut2.ar(In.ar(),0,1024) } ).send(s);
    s.sync;
	"creating synth".postln;
	Synth.tail(RootNode(s), "my_scope_test");
	"scope running".postln;
}).next;
*/

#ifndef SCOPE_H
#define SCOPE_H

#include <QWidget>
#include <QPixmap>

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
//  Scope( int portNumber );

protected:
  void paintEvent( QPaintEvent* p_evt );
  void resizeEvent( QResizeEvent* p_evt );

private slots:
  void refreshScope();
 
private: 
  std::unique_ptr<server_shared_memory_client> shm_client;
  scope_buffer_reader shm_reader;
  QPixmap scope_pixmap;
};

#endif

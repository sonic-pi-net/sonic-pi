//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/sonic-pi-net/sonic-pi
// License: https://github.com/sonic-pi-net/sonic-pi/blob/main/LICENSE.md
//
// Copyright 2022 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPIMETRO_H
#define SONICPIMETRO_H

#include <QWidget>
#include <QPushButton>
#include <QMutex>
#include "qt_api_client.h"

class SonicPiMetro : public QWidget
{
    Q_OBJECT
public:
  SonicPiMetro(std::shared_ptr<SonicPi::QtAPIClient> spClient, QWidget *parent = nullptr);
  void updateActiveLinkCount(int count);

signals:
  void enableLink();
  void disableLink();

public slots:

protected:

private:
  QPushButton *enableLinkButton;
  bool linkEnabled = false;
  int numActiveLinks = 0;
  QMutex *mutex;

  void toggleLink();
  void updateActiveLinkText();
  void lockedUpdateActiveLinkText();

  std::shared_ptr<SonicPi::QtAPIClient> m_spClient;

};

#endif // SONICPIMETRO_H

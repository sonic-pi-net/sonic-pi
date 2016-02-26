//--
// This file is part of Sonic Pi: http://sonic-pi.net
// Full project source: https://github.com/samaaron/sonic-pi
// License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
//
// Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
// All rights reserved.
//
// Permission is granted for use, copying, modification, and
// distribution of modified versions of this work as long as this
// notice is included.
//++

#ifndef SONICPITHEME_H
#define SONICPITHEME_H

#include <QtCore>
#include <QObject>
#include <QColor>

class SonicPiTheme : public QObject
{
    Q_OBJECT
public:
    explicit SonicPiTheme(QObject *parent = 0, QSettings *settings = 0, bool dark = false);
    ~SonicPiTheme();
    QColor color(QString);
    QString font(QString);
    void darkMode();
    void lightMode();

private:
    QMap<QString, QString> withCustomSettings(QMap<QString, QString> settings);
    QMap<QString, QString> lightTheme();
    QMap<QString, QString> darkTheme();
    QMap<QString, QString> theme;
    QMap<QString, QString> customSettings;

signals:

public slots:
};

#endif // SONICPITHEME_H

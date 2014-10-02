#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

#-------------------------------------------------
#
# Project created by QtCreator 2014-02-28T14:51:06
#
#-------------------------------------------------

LIBS += -L /Users/josephwilk/Workspace/c++/QScintilla-gpl-2.8.4-snapshot-e4e3562b54cb/Qt4Qt5 -lqscintilla2
INCLUDEPATH += /Users/josephwilk/Workspace/c++/QScintilla-gpl-2.8.4-snapshot-e4e3562b54cb/Qt4Qt5/
DEPENDPATH += /Users/josephwilk/Workspace/c++/QScintilla-gpl-2.8.4-snapshot-e4e3562b54cb/Qt4Qt5/

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = 'Sonic-Pi'
TEMPLATE = app

SOURCES += main.cpp \
           mainwindow.cpp \
           sonicpilexer.cpp \
    openglwindow.cpp

HEADERS  += mainwindow.h \
            oscpkt.hh \
            udp.hh \
            sonicpilexer.h \
    openglwindow.h

OTHER_FILES += \
    images/copy.png \
    images/cut.png \
    images/new.png \
    images/save.png \
    images/rec.png \
    images/recording_a.png \
    images/recording_b.png \

RESOURCES += \
    SonicPi.qrc \
    help_files.qrc  \
    info_files.qrc

ICON = images/app.icns
LIBS         += -lqscintilla2

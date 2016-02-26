#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/master/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
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

# -- Change to match the location of QScintilla on your system
#
 LIBS += -L/Users/sam/Downloads/tmp/QScintilla-gpl-2.9/Qt4Qt5
 INCLUDEPATH += /Users/sam/Downloads/tmp/QScintilla-gpl-2.9/Qt4Qt5
 DEPENDPATH += /Users/sam/Downloads/tmp/QScintilla-gpl-2.9/Qt4Qt5
# --

TARGET = 'sonic-pi'

QT += core gui concurrent network
greaterThan(QT_MAJOR_VERSION, 4) {
  QT += widgets
}

# Linux only
unix:!macx {
  lessThan(QT_MAJOR_VERSION, 5) {
    LIBS += -lqscintilla2
  } else {
    LIBS += -lqt5scintilla2
  }
  QMAKE_CXXFLAGS += -Wall -Werror -Wextra
}

# Mac OS X only
macx {
  TARGET = 'Sonic Pi'
  LIBS += -lqscintilla2
  QT += macextras
  DEFINES += DONT_USE_OSX_KEYS
  QMAKE_CXXFLAGS += -Wall -Werror -Wextra
}

# Windows only
win32 {
  LIBS += -lqscintilla2
  QMAKE_CXXFLAGS += /WX
  DEFINES += _CRT_SECURE_NO_WARNINGS _WINSOCK_DEPRECATED_NO_WARNINGS
}

CODECFORSRC = UTF-8
CODECFORTR = UTF-8

TEMPLATE = app

SOURCES += main.cpp \
           mainwindow.cpp \
           sonicpilexer.cpp \
           sonicpiapis.cpp \
           sonicpiscintilla.cpp \
           oschandler.cpp \
           sonicpilog.cpp \
           sonic_pi_osc_server.cpp \
           sonic_pi_udp_osc_server.cpp \
           sonic_pi_tcp_osc_server.cpp \
           sonicpitheme.cpp
win32 {
# have to link these explicitly for some reason
  SOURCES += platform/win/moc_qsciscintilla.cpp \
             platform/win/moc_qsciscintillabase.cpp
}

HEADERS  += mainwindow.h \
            oscpkt.hh \
            udp.hh \
            sonicpilexer.h \
            sonicpilog.h \
            sonicpiapis.h \
            sonicpiscintilla.h \
            oschandler.h \
            sonic_pi_osc_server.h \
            sonic_pi_udp_osc_server.h \
            sonic_pi_tcp_osc_server.h \
            ruby_help.h \
            sonicpitheme.h

TRANSLATIONS = lang/sonic-pi_de.ts \
               lang/sonic-pi_is.ts \
               lang/sonic-pi_ja.ts \
               lang/sonic-pi_nb.ts \
               lang/sonic-pi_pl.ts \
               lang/sonic-pi_fr.ts \
               lang/sonic-pi_es.ts \
               lang/sonic-pi_hu.ts

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
    help_files.qrc \
    info_files.qrc

RC_FILE = SonicPi.rc

ICON = images/app.icns

win32 {
	install_qsci.files = $$[QT_INSTALL_LIBS]\qscintilla2.dll
	install_qsci.path = release

	install_bat.files = sonic-pi.bat
	install_bat.path = ..\..\..

	INSTALLS += install_qsci install_bat
	# allow to run on XP
	QMAKE_SUBSYSTEM_SUFFIX = ,5.01
}

# not unicode ready
win32 {
  DEFINES -= UNICODE
  DEFINES += _MBCS
  DEFINES += NOMINMAX
}

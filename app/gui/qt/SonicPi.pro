#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013, 2014, 2015, 2016 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

# The canonical way of building Sonic Pi is now via cmake
# This qmake method for building Sonic Pi is now only left for macOS
# whilst it is being transitioned to cmake. The cmake build now
# supports Windows and Linux

requires(macos)
lessThan(QT_MAJOR_VERSION, 5): error("requires Qt 5")


TARGET = 'sonic-pi'
CONFIG += qscintilla2 c++11 resources_big

QT += core gui concurrent network opengl widgets

# Set these to point at your system
LIBS += -L/Users/sam/Development/Supercollider/git-src/external_libraries/boost/libs
INCLUDEPATH += /Users/sam/Development/Supercollider/git-src/external_libraries/boost/
DEPENDPATH += /Users/sam/Development/Supercollider/git-src/external_libraries/boost/

LIBS += -L/Users/sam/Development/tmp/QScintilla-2.11.4/Qt4Qt5
INCLUDEPATH += /Users/sam/Development/tmp/QScintilla-2.11.4/Qt4Qt5
DEPENDPATH += /Users/sam/Development/tmp/QScintilla-2.11.4/Qt4Qt5
DEPENDPATH += /Users/sam/Development/tmp/


QT += macextras
QMAKE_CXXFLAGS += -I/usr/local/include
QMAKE_CXXFLAGS += -Wall -Werror -Wextra -Wno-unused-variable -Wno-unused-parameter
CONFIG += warn_off
TARGET = 'Sonic Pi'
LIBS += -lqscintilla2_qt5
debug {
QMAKE_LFLAGS += -g -O0
QMAKE_CXXFLAGS += -g -O0
}


CODECFORSRC = UTF-8
CODECFORTR = UTF-8

TEMPLATE = app

SOURCES += main.cpp \
           mainwindow.cpp \
           utils/sonicpiapis.cpp \
           osc/oschandler.cpp \
           osc/oscsender.cpp \
           osc/sonic_pi_osc_server.cpp \
           osc/sonic_pi_udp_osc_server.cpp \
           osc/sonic_pi_tcp_osc_server.cpp \
           widgets/sonicpilog.cpp \
           widgets/infowidget.cpp \
           widgets/sonicpiscintilla.cpp \
           widgets/sonicpilexer.cpp \
           widgets/settingswidget.cpp \
           model/sonicpitheme.cpp \
           visualizer/scope.cpp \
           external/kiss_fft/kiss_fft.c

HEADERS  += mainwindow.h \
            widgets/sonicpilog.h \
            widgets/infowidget.h \
            widgets/sonicpilexer.h \
            widgets/sonicpiscintilla.h \
            widgets/settingswidget.h \
            utils/sonicpiapis.h \
            utils/ruby_help.h \
            osc/oscpkt.hh \
            osc/udp.hh \
            osc/oschandler.h \
            osc/oscsender.h \
            osc/sonic_pi_osc_server.h \
            osc/sonic_pi_udp_osc_server.h \
            osc/sonic_pi_tcp_osc_server.h \
            model/sonicpitheme.h \
            model/settings.h \
            visualizer/scope.h \
            external/kiss_fft/kiss_fft.h

TRANSLATIONS = lang/sonic-pi_bg.ts \
    lang/sonic-pi_bs.ts \
               lang/sonic-pi_ca.ts \
               lang/sonic-pi_cs.ts \
               lang/sonic-pi_da.ts \
               lang/sonic-pi_de.ts \
               lang/sonic-pi_el.ts \
               lang/sonic-pi_en_US.ts \
               lang/sonic-pi_es.ts \
               lang/sonic-pi_et.ts \
    lang/sonic-pi_fa.ts \
               lang/sonic-pi_fi.ts \
               lang/sonic-pi_fr.ts \
    lang/sonic-pi_gl.ts \
    lang/sonic-pi_he.ts \
               lang/sonic-pi_hi.ts \
               lang/sonic-pi_hu.ts \
               lang/sonic-pi_id.ts \
               lang/sonic-pi_is.ts \
               lang/sonic-pi_it.ts \
               lang/sonic-pi_ja.ts \
    lang/sonic-pi_ka.ts \
               lang/sonic-pi_ko.ts \
               lang/sonic-pi_nb.ts \
               lang/sonic-pi_nl.ts \
               lang/sonic-pi_pl.ts \
               lang/sonic-pi_pt.ts \
               lang/sonic-pi_pt_BR.ts \
               lang/sonic-pi_ro.ts \
               lang/sonic-pi_ru.ts \
    lang/sonic-pi_sk.ts \
    lang/sonic-pi_sl.ts \
               lang/sonic-pi_sv.ts \
               lang/sonic-pi_tr.ts \
    lang/sonic-pi_ug.ts \
               lang/sonic-pi_uk.ts \
    lang/sonic-pi_vi.ts \
               lang/sonic-pi_zh-Hans.ts \
               lang/sonic-pi_zh.ts \
               lang/sonic-pi_zh_HK.ts \
               lang/sonic-pi_zh_TW.ts \


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

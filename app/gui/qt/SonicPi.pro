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

lessThan(QT_MAJOR_VERSION, 5): error("requires Qt 5")

TARGET = 'sonic-pi'
CONFIG += qscintilla2 qwt c++11 resources_big

QT += core gui concurrent network opengl widgets

# Linux only
unix:!macx {
  LIBS += -lrt -lqscintilla2_qt5
  QMAKE_CXXFLAGS += -std=gnu++11
  QMAKE_CXXFLAGS += -Wall -Werror -Wextra -Wno-unused-variable -Wno-unused-parameter
  debug {
    QMAKE_CXXFLAGS += -ggdb
  }
}

# Mac OS X only
macx {
  QMAKE_CXXFLAGS += -I/usr/local/include
  QMAKE_CXXFLAGS += -Wall -Werror -Wextra -Wno-unused-variable -Wno-unused-parameter
  CONFIG += warn_off
  TARGET = 'Sonic Pi'
  LIBS += -lqscintilla2_qt5
  debug {
    QMAKE_LFLAGS += -g -O0
    QMAKE_CXXFLAGS += -g -O0
  }
}

# Windows only
win32 {
  include ( c:/qwt-6.1.3/features/qwt.prf )
  LIBS += -lqscintilla2
  QMAKE_CXXFLAGS += -Ic:/boost_1_61_0
#  QMAKE_CXXFLAGS += /WX
  QMAKE_LFLAGS += /LIBPATH:C:\boost_1_61_0\bin.v2\libs\date_time\build\msvc-12.0\release\link-static\threading-multi
  DEFINES += _CRT_SECURE_NO_WARNINGS _WINSOCK_DEPRECATED_NO_WARNINGS
}

CODECFORSRC = UTF-8
CODECFORTR = UTF-8

TEMPLATE = app

SOURCES += main.cpp \
           mainwindow.cpp \
           utils/sonicpiapis.cpp \
           utils/sonicpitheme.cpp \
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
           visualizer/scope.cpp

HEADERS  += mainwindow.h \
            widgets/sonicpilog.h \
            widgets/infowidget.h \
            widgets/sonicpilexer.h \
            widgets/sonicpiscintilla.h \
            widgets/settingswidget.h \
            utils/sonicpiapis.h \
            utils/ruby_help.h \
            utils/sonicpitheme.h \
            osc/oscpkt.hh \
            osc/udp.hh \
            osc/oschandler.h \
            osc/oscsender.h \
            osc/sonic_pi_osc_server.h \
            osc/sonic_pi_udp_osc_server.h \
            osc/sonic_pi_tcp_osc_server.h \
            model/settings.h \
            visualizer/scope.h

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

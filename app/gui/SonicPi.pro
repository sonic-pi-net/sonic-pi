#--
# This file is part of Sonic Pi: http://sonic-pi.net
# Full project source: https://github.com/samaaron/sonic-pi
# License: https://github.com/samaaron/sonic-pi/blob/main/LICENSE.md
#
# Copyright 2013 by Sam Aaron (http://sam.aaron.name).
# All rights reserved.
#
# Permission is granted for use, copying, modification, distribution,
# and distribution of modified versions of this work as long as this
# notice is included.
#++

# The canonical way of building Sonic Pi is via cmake
# This .pro file is provided for use by lupdate to generate translation files.


SOURCES += main.cpp \
           mainwindow.cpp \
           qt_api_client.cpp \
           model/sonicpitheme.cpp \
           utils/completion_context.cpp \
           utils/reducedmotion.cpp \
           utils/scintilla_api.cpp \
           utils/setbundle.cpp \
           utils/sonicpi_i18n.cpp \
           utils/tutorialdocs.cpp \
           visualizer/scope_window.cpp \
           widgets/bpmscrubwidget.cpp \
           widgets/completionpopup.cpp \
           widgets/devicelistwidget.cpp \
           widgets/infowidget.cpp \
           widgets/linkaudiostreamswidget.cpp \
           widgets/linkvisibilitytoggle.cpp \
           widgets/logpanel.cpp \
           widgets/metricspanel.cpp \
           widgets/nodetreegraph.cpp \
           widgets/quickstartpane.cpp \
           widgets/settingswidget.cpp \
           widgets/sonicpicontext.cpp \
           widgets/sonicpieditor.cpp \
           widgets/sonicpierrorcard.cpp \
           widgets/sonicpilexer.cpp \
           widgets/sonicpilog.cpp \
           widgets/sonicpimetro.cpp \
           widgets/sonicpiscintilla.cpp \
           widgets/sonicpitooltip.cpp \
           widgets/splashwidget.cpp \
           widgets/timewarpedit.cpp \
           widgets/tutorialpane.cpp \
           widgets/tutorialwidgets.cpp \
           widgets/visualizer.cpp \
           widgets/welcomewidget.cpp \
           widgets/zoombar.cpp \

HEADERS += utils/ruby_help.h \
           widgets/tutorialwidgets.h \

TRANSLATIONS = lang/sonic-pi_ar.ts \
               lang/sonic-pi_bg.ts \
               lang/sonic-pi_bn.ts \
               lang/sonic-pi_bs.ts \
               lang/sonic-pi_ca.ts \
               lang/sonic-pi_ca@valencia.ts \
               lang/sonic-pi_cs.ts \
               lang/sonic-pi_da.ts \
               lang/sonic-pi_de.ts \
               lang/sonic-pi_el.ts \
               lang/sonic-pi_en_AU.ts \
               lang/sonic-pi_en_US.ts \
               lang/sonic-pi_eo.ts \
               lang/sonic-pi_es.ts \
               lang/sonic-pi_et.ts \
               lang/sonic-pi_eu.ts \
               lang/sonic-pi_fa.ts \
               lang/sonic-pi_fi.ts \
               lang/sonic-pi_fr.ts \
               lang/sonic-pi_ga.ts \
               lang/sonic-pi_gd.ts \
               lang/sonic-pi_gl.ts \
               lang/sonic-pi_he.ts \
               lang/sonic-pi_hi.ts \
               lang/sonic-pi_hr.ts \
               lang/sonic-pi_hu.ts \
               lang/sonic-pi_hy.ts \
               lang/sonic-pi_id.ts \
               lang/sonic-pi_is.ts \
               lang/sonic-pi_it.ts \
               lang/sonic-pi_ja.ts \
               lang/sonic-pi_ka.ts \
               lang/sonic-pi_ko.ts \
               lang/sonic-pi_lv.ts \
               lang/sonic-pi_nb.ts \
               lang/sonic-pi_nl.ts \
               lang/sonic-pi_pl.ts \
               lang/sonic-pi_pt.ts \
               lang/sonic-pi_pt_BR.ts \
               lang/sonic-pi_ro.ts \
               lang/sonic-pi_ru.ts \
               lang/sonic-pi_si.ts \
               lang/sonic-pi_sk.ts \
               lang/sonic-pi_sl.ts \
               lang/sonic-pi_sv.ts \
               lang/sonic-pi_sw.ts \
               lang/sonic-pi_ta.ts \
               lang/sonic-pi_th.ts \
               lang/sonic-pi_tr.ts \
               lang/sonic-pi_ug.ts \
               lang/sonic-pi_uk.ts \
               lang/sonic-pi_vi.ts \
               lang/sonic-pi_zh-Hans.ts \
               lang/sonic-pi_zh.ts \
               lang/sonic-pi_zh_HK.ts \
               lang/sonic-pi_zh_TW.ts \

RESOURCES += SonicPi.qrc \
             help_files.qrc \
             info_files.qrc

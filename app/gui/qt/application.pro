CONFIG       += release

greaterThan(QT_MAJOR_VERSION, 4) {
    QT += widgets concurrent
}

HEADERS       = mainwindow.h \
                sonicpilexer.h \
                ruby_help.h \
                api_list.h

SOURCES       = main.cpp \
                mainwindow.cpp \
                sonicpilexer.cpp

RESOURCES     = application.qrc \
                help_files.qrc

LIBS         += -lqscintilla2

ICON = images/app.icns

CONFIG       += release

greaterThan(QT_MAJOR_VERSION, 4) {
    QT += widgets
}

HEADERS       = mainwindow.h \
                sonicpilexer.h
SOURCES       = main.cpp \
                mainwindow.cpp \
                sonicpilexer.cpp

RESOURCES     = application.qrc
LIBS         += -lqscintilla2

OTHER_FILES += \
    images/copy.png \
    images/cut.png \
    images/foo.png \
    images/new.png \
    images/open.png \
    images/paste.png \
    images/save.png \

ICON = images/app.icns

#-------------------------------------------------
#
# Project created by QtCreator 2014-02-28T14:51:06
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = SonicPi
TEMPLATE = app


SOURCES += main.cpp\
           mainwindow.cpp \
           sonicpilexer.cpp

HEADERS  += mainwindow.h \
    sonicpilexer.h

LIBS += -lqscintilla2



OTHER_FILES += \
    images/copy.png \
    images/cut.png \
    images/foo.png \
    images/new.png \
    images/open.png \
    images/paste.png \
    images/save.png \

RESOURCES += \
    application.qrc

 ICON = images/app.icns

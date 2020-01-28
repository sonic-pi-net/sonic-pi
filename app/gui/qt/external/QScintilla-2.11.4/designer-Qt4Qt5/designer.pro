# This is the qmake file for the QScintilla plugin for Qt Designer.


TEMPLATE = lib
TARGET = qscintillaplugin

CONFIG += plugin qscintilla2

greaterThan(QT_MAJOR_VERSION, 4) {
    QT += designer

    # Work around QTBUG-39300.
    CONFIG -= android_install
} else {
    CONFIG += designer
}

HEADERS = qscintillaplugin.h
SOURCES = qscintillaplugin.cpp

target.path = $$[QT_INSTALL_PLUGINS]/designer
INSTALLS += target

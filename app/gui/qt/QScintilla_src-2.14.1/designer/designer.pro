# This is the qmake file for the QScintilla plugin for Qt Designer.


TEMPLATE = lib
TARGET = qscintillaplugin

# Comment this in to build a dynamic library supporting multiple architectures
# on macOS.
#QMAKE_APPLE_DEVICE_ARCHS = x86_64 arm64

CONFIG += plugin qscintilla2

QT += designer

# Work around QTBUG-39300.
CONFIG -= android_install

HEADERS = qscintillaplugin.h
SOURCES = qscintillaplugin.cpp

target.path = $$[QT_INSTALL_PLUGINS]/designer
INSTALLS += target

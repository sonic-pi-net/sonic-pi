################################################################
# Qwt Widget Library
# Copyright (C) 1997   Josef Wilgen
# Copyright (C) 2002   Uwe Rathmann
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the Qwt License, Version 1.0
################################################################

# qmake project file for building the qwt libraries

QWT_ROOT = $${PWD}/..
include( $${QWT_ROOT}/qwtconfig.pri )
include( $${QWT_ROOT}/qwtbuild.pri )
include( $${QWT_ROOT}/qwtfunctions.pri )

QWT_OUT_ROOT = $${OUT_PWD}/..

TEMPLATE          = lib
TARGET            = $$qwtLibraryTarget(qwt)

DESTDIR           = $${QWT_OUT_ROOT}/lib

contains(QWT_CONFIG, QwtDll) {

    CONFIG += dll
    win32|symbian: DEFINES += QT_DLL QWT_DLL QWT_MAKEDLL
}
else {
    CONFIG += staticlib
} 

contains(QWT_CONFIG, QwtFramework) {

    CONFIG += lib_bundle
}

include ( $${PWD}/src.pri )

# Install directives

target.path    = $${QWT_INSTALL_LIBS}
INSTALLS       = target 

CONFIG(lib_bundle) {

    FRAMEWORK_HEADERS.version = Versions
    FRAMEWORK_HEADERS.files = $${HEADERS}
    FRAMEWORK_HEADERS.path = Headers
    QMAKE_BUNDLE_DATA += FRAMEWORK_HEADERS
}
else {

    headers.files  = $${HEADERS}
    headers.path   = $${QWT_INSTALL_HEADERS}
    INSTALLS += headers
}

contains(QWT_CONFIG, QwtPkgConfig) {

    CONFIG     += create_pc create_prl no_install_prl

    QMAKE_PKGCONFIG_NAME = Qwt$${QWT_VER_MAJ}
    QMAKE_PKGCONFIG_DESCRIPTION = Qt Widgets for Technical Applications

    QMAKE_PKGCONFIG_LIBDIR = $${QWT_INSTALL_LIBS}
    QMAKE_PKGCONFIG_INCDIR = $${QWT_INSTALL_HEADERS}

    QMAKE_PKGCONFIG_DESTDIR = pkgconfig

    greaterThan(QT_MAJOR_VERSION, 4) {

        QMAKE_PKGCONFIG_FILE = Qt$${QT_MAJOR_VERSION}$${QMAKE_PKGCONFIG_NAME}
        QMAKE_PKGCONFIG_REQUIRES = Qt5Widgets Qt5Concurrent Qt5PrintSupport

        contains(QWT_CONFIG, QwtSvg) {
            QMAKE_PKGCONFIG_REQUIRES += Qt5Svg
        }

        contains(QWT_CONFIG, QwtOpenGL) {
            QMAKE_PKGCONFIG_REQUIRES += Qt5OpenGL
        }

        QMAKE_DISTCLEAN += $${DESTDIR}/$${QMAKE_PKGCONFIG_DESTDIR}/$${QMAKE_PKGCONFIG_FILE}.pc
    }
    else {

        # there is no QMAKE_PKGCONFIG_FILE fo Qt4
        QMAKE_PKGCONFIG_REQUIRES = QtGui 

        contains(QWT_CONFIG, QwtSvg) {
            QMAKE_PKGCONFIG_REQUIRES += QtSvg
        }

        contains(QWT_CONFIG, QwtOpenGL) {
            QMAKE_PKGCONFIG_REQUIRES += QtOpenGL
        }

        QMAKE_DISTCLEAN += $${DESTDIR}/$${QMAKE_PKGCONFIG_DESTDIR}/$${TARGET}.pc
    }

    QMAKE_DISTCLEAN += $${DESTDIR}/libqwt.prl
}

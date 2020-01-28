################################################################
# Qwt Widget Library
# Copyright (C) 1997   Josef Wilgen
# Copyright (C) 2002   Uwe Rathmann
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the Qwt License, Version 1.0
################################################################

# Copied and modified from qt_functions.prf

defineReplace(qwtLibraryTarget) {

    unset(LIBRARY_NAME)
    LIBRARY_NAME = $$1

    mac:contains(QWT_CONFIG, QwtFramework) {

        QMAKE_FRAMEWORK_BUNDLE_NAME = $$LIBRARY_NAME
        export(QMAKE_FRAMEWORK_BUNDLE_NAME)
    }

    contains(TEMPLATE, .*lib):CONFIG(debug, debug|release) {

        !debug_and_release|build_pass {

            mac:RET = $$member(LIBRARY_NAME, 0)_debug
            win32:RET = $$member(LIBRARY_NAME, 0)d
        }
    }

    isEmpty(RET):RET = $$LIBRARY_NAME
    return($$RET)
}

defineTest(qwtAddLibrary) {

    LIB_PATH = $$1
    LIB_NAME = $$2

    mac:contains(QWT_CONFIG, QwtFramework) {

        LIBS      *= -F$${LIB_PATH}
    }
    else {

        unix:lessThan(QT_MAJOR_VERSION, 5) {

            # Many Linux distributions install Qwt in the same directory
            # as the Qt libs and thus we need to prepend the path for the local build
            # to avoid conflicting with the installed version.
            # Qt5 qmake appends ( instead of prepending ) the path to the Qt libs
            # to LIBS, but for Qt4 we need to use the QMAKE_LIBDIR_FLAGS.

            QMAKE_LIBDIR_FLAGS *= -L$${LIB_PATH}
        }
        else {
            LIBS *= -L$${LIB_PATH}
        }
    }

    unset(LINKAGE)

    mac:contains(QWT_CONFIG, QwtFramework) {

        LINKAGE = -framework $${LIB_NAME}
    }

    isEmpty(LINKAGE) {

        if(!debug_and_release|build_pass):CONFIG(debug, debug|release) {

            mac:LINKAGE = -l$${LIB_NAME}_debug
            win32:LINKAGE = -l$${LIB_NAME}d
        }
    }

    isEmpty(LINKAGE) {

        LINKAGE = -l$${LIB_NAME}
    }

    !isEmpty(QMAKE_LSB) {

        QMAKE_LFLAGS *= --lsb-shared-libs=$${LIB_NAME}
    }

    LIBS += $$LINKAGE
    export(LIBS)
    export(QMAKE_LFLAGS)
    export(QMAKE_LIBDIR_FLAGS)

    return(true)
}

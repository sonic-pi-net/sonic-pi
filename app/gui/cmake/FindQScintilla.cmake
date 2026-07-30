if(QSCINTILLA_INCLUDE_DIR)
  set(QSCINTILLA_FIND_QUIETLY TRUE)
endif()

find_path(QSCINTILLA_INCLUDE_DIR Qsci/qsciscintilla.h
  HINTS ${QSCINTILLA_ROOT} ${QSCINTILLA_ROOT}/include)

find_library(QSCINTILLA_LIBRARY
  NAMES
    qscintilla2
    qscintilla2_qt6
    qscintilla2_qt5
    libqscintilla2
    libqscintilla2_qt6
    libqscintilla2_qt5
  HINTS ${QSCINTILLA_ROOT} ${QSCINTILLA_ROOT}/lib)

if(QSCINTILLA_INCLUDE_DIR AND EXISTS "${QSCINTILLA_INCLUDE_DIR}/Qsci/qsciglobal.h")
  file(STRINGS "${QSCINTILLA_INCLUDE_DIR}/Qsci/qsciglobal.h" qscintilla_version_str REGEX "^#define[\t ]+QSCINTILLA_VERSION_STR[\t ]+\".*\"")
  string(REGEX REPLACE "^.*QSCINTILLA_VERSION_STR[\t ]+\"([^\"]*)\".*$" "\\1" QSCINTILLA_VERSION "${qscintilla_version_str}")
  unset(qscintilla_version_str)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(QScintilla
  REQUIRED_VARS
    QSCINTILLA_LIBRARY
    QSCINTILLA_INCLUDE_DIR
  VERSION_VAR
    QSCINTILLA_VERSION)

if(QSCINTILLA_FOUND)
  set(QSCINTILLA_LIBRARIES ${QSCINTILLA_LIBRARY})
  set(QSCINTILLA_INCLUDE_DIRS ${QSCINTILLA_INCLUDE_DIR})

  if(NOT TARGET QScintilla)
    # GLOBAL: this module is found from gui/, but gui-tests/ is a sibling
    # directory and links QScintilla too. A non-global imported target is
    # visible only at or below where it was created, so gui-tests would fall
    # back to a plain -lQScintilla and fail to link (the system library is
    # libqscintilla2_qt6). The vendored fallback defines a normal target,
    # which is global already, which is why only system-QScintilla builds
    # (Debian) hit this.
    add_library(QScintilla UNKNOWN IMPORTED GLOBAL)
    set_target_properties(QScintilla PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES "${QSCINTILLA_INCLUDE_DIRS}"
      IMPORTED_LOCATION "${QSCINTILLA_LIBRARIES}")
  endif()
endif()

mark_as_advanced(QSCINTILLA_INCLUDE_DIR QSCINTILLA_LIBRARY)

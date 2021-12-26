# LIBSSH2_FOUND - system has the libssh2 library
# LIBSSH2_INCLUDE_DIR - the libssh2 include directory
# LIBSSH2_LIBRARY - the libssh2 library name

FIND_PATH(LIBSSH2_INCLUDE_DIR libssh2.h)

FIND_LIBRARY(LIBSSH2_LIBRARY NAMES ssh2 libssh2)

INCLUDE(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LibSSH2
    REQUIRED_VARS LIBSSH2_LIBRARY LIBSSH2_INCLUDE_DIR)

MARK_AS_ADVANCED(LIBSSH2_INCLUDE_DIR LIBSSH2_LIBRARY)

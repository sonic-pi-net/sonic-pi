if (LIBSSH2_LIBRARIES AND LIBSSH2_INCLUDE_DIRS)
  set(LIBSSH2_FOUND TRUE)
else (LIBSSH2_LIBRARIES AND LIBSSH2_INCLUDE_DIRS)
  find_path(LIBSSH2_INCLUDE_DIR
    NAMES
      libssh2.h
    PATHS
      /usr/include
      /usr/local/include
      /opt/local/include
      /sw/include
      ${CMAKE_INCLUDE_PATH}
      ${CMAKE_INSTALL_PREFIX}/include
  )
  
  find_library(LIBSSH2_LIBRARY
    NAMES
      ssh2
      libssh2
    PATHS
      /usr/lib
      /usr/local/lib
      /opt/local/lib
      /sw/lib
      ${CMAKE_LIBRARY_PATH}
      ${CMAKE_INSTALL_PREFIX}/lib
  )

  if (LIBSSH2_INCLUDE_DIR AND LIBSSH2_LIBRARY)
    set(LIBSSH2_FOUND TRUE)
  endif (LIBSSH2_INCLUDE_DIR AND LIBSSH2_LIBRARY)

  if (LIBSSH2_FOUND)
    set(LIBSSH2_INCLUDE_DIRS
      ${LIBSSH2_INCLUDE_DIR}
    )

    set(LIBSSH2_LIBRARIES
      ${LIBSSH2_LIBRARIES}
      ${LIBSSH2_LIBRARY}
    )
  endif (LIBSSH2_FOUND)
endif (LIBSSH2_LIBRARIES AND LIBSSH2_INCLUDE_DIRS)


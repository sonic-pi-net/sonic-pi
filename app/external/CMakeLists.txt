# This CMakeLists builds all the external libraries required for a Sonic Pi
cmake_minimum_required(VERSION 3.13)

message(STATUS " Aubio Builder")

if(WIN32)
  set(ERLANG_INCLUDE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/windows/headers/erlang CACHE STRING "Path to the Erlang header files")
elseif(${CMAKE_SYSTEM_NAME} MATCHES Darwin) # macOS
  set(ERLANG_INCLUDE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/macos/headers/erlang CACHE STRING "Path to the Erlang header files")
else()
  execute_process(
    COMMAND erl -noshell -eval "io:format(\"~s~n\", [filename:join([lists:concat([code:root_dir(), \"/erts-\", erlang:system_info(version)]), \"include\"])]), init:stop(0)."
    OUTPUT_VARIABLE FOUND_ERLANG_INCLUDE_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(ERLANG_INCLUDE_PATH ${FOUND_ERLANG_INCLUDE_PATH} CACHE STRING "Path to the Erlang header files")
endif()
option(USE_SYSTEM_LIBS "Use system libraries instead of bundled libraries when building" OFF)

message(STATUS "ERLANG_INCLUDE_PATH: ${ERLANG_INCLUDE_PATH}")
message(STATUS "USE_SYSTEM_LIBS: ${USE_SYSTEM_LIBS}")

project(AubioBuilder
    LANGUAGES CXX C
    VERSION 1.0.0
    )

  include(GNUInstallDirs)
  include(ExternalProject)

  set(CMAKE_OSX_DEPLOYMENT_TARGET '10.14')

  # sp_midi
ExternalProject_Add(sp_midi
    PREFIX sp_midi-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/sp_midi
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
        -DERLANG_INCLUDE_PATH=${ERLANG_INCLUDE_PATH}
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
        -DUSE_SYSTEM_RTMIDI=${USE_SYSTEM_LIBS}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )

ExternalProject_Add(sp_link
    PREFIX sp_link-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/sp_link
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/../server/beam/tau/priv
        -DERLANG_INCLUDE_PATH=${ERLANG_INCLUDE_PATH}
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )

ExternalProject_Add(ogg
    PREFIX ogg-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/ogg-1.3.4

    INSTALL_DIR ${CMAKE_BINARY_DIR}/ogg-package
    CMAKE_ARGS
        -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
    )

ExternalProject_Add(flac
    PREFIX flac-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/flac_1_3_3
    DEPENDS ogg

    CMAKE_ARGS -DOGG_ROOT=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_CXXLIBS=OFF
        -DBUILD_DOCS=OFF
        -DBUILD_EXAMPLES=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        -DIntl_IS_BUILTIN=TRUE
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}

    INSTALL_DIR ${CMAKE_BINARY_DIR}/flac-package
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/flac-package
    )

ExternalProject_Add(opus
    PREFIX opus-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/opus-1.2
    DEPENDS ogg

    CMAKE_ARGS -DOGG_ROOT=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}

    INSTALL_DIR ${CMAKE_BINARY_DIR}/opus-package
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/opus-package
    )

ExternalProject_Add(vorbis
    PREFIX vorbis-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/vorbis-1.3.4
    DEPENDS ogg

    CMAKE_ARGS -DOGG_ROOT=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}

    INSTALL_DIR ${CMAKE_BINARY_DIR}/vorbis-package
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/vorbis-package
    )

ExternalProject_Add(libsndfile
    PREFIX libsndfile-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/libsndfile
    DEPENDS flac vorbis opus

    CMAKE_ARGS
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTING=OFF
        -DBUILD_REGTEST=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        -DFLAC_ROOT=${CMAKE_BINARY_DIR}/flac-package
        -DPC_OGG_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/ogg-package/include
        -DPC_OGG_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/ogg-package/${CMAKE_INSTALL_LIBDIR}
        -DPC_FLAC_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/flac-package/include
        -DPC_FLAC_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/flac-package/${CMAKE_INSTALL_LIBDIR}
        -DPC_VORBIS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
        -DPC_VORBIS_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/${CMAKE_INSTALL_LIBDIR}
        -DPC_VORBISENC_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
        -DPC_VORBISENC_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/${CMAKE_INSTALL_LIBDIR}
        -DPC_OPUS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/opus-package/include
        -DPC_OPUS_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/opus-package/${CMAKE_INSTALL_LIBDIR}
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}

        INSTALL_DIR ${CMAKE_BINARY_DIR}/libsndfile-package
        CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/libsndfile-package
    )

ExternalProject_Add(aubio
    PREFIX aubio-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/aubio
    DEPENDS libsndfile

    CMAKE_ARGS
        -DLIBSNDFILE_INCLUDE_DIR=${CMAKE_BINARY_DIR}/libsndfile-package/include
        -DLIBSNDFILE_LIBRARY_DIR=${CMAKE_BINARY_DIR}/libsndfile-package/${CMAKE_INSTALL_LIBDIR}
        -DLIBOGG_LIBRARY_DIR=${CMAKE_BINARY_DIR}/ogg-package/${CMAKE_INSTALL_LIBDIR}
        -DLIBVORBIS_LIBRARY_DIR=${CMAKE_BINARY_DIR}/vorbis-package/${CMAKE_INSTALL_LIBDIR}
        -DLIBOPUS_LIBRARY_DIR=${CMAKE_BINARY_DIR}/opus-package/${CMAKE_INSTALL_LIBDIR}
        -DLIBFLAC_LIBRARY_DIR=${CMAKE_BINARY_DIR}/flac-package/${CMAKE_INSTALL_LIBDIR}

        -DPC_OGG_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/ogg-package/include
        -DPC_VORBIS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
        -DPC_VORBISENC_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
        -DPC_OPUS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/opus-package/include
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
    INSTALL_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../server/native
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_SOURCE_DIR}/../server/native
    )

# Kick off a generation by making a dummy/empty project
add_library(AubioBuilder STATIC externals.cpp)

# Dependency ensures the externals are built
add_dependencies(AubioBuilder aubio)

# Copy prebuilt files
if(WIN32)
  if(CMAKE_SIZEOF_VOID_P EQUAL 8) # 64-bit
      add_custom_command(
        TARGET AubioBuilder PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/windows/x64/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
      )
  elseif(CMAKE_SIZEOF_VOID_P EQUAL 4) # 32-bit
      add_custom_command(
        TARGET AubioBuilder PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/windows/x86/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
      )
  endif()
elseif(${CMAKE_SYSTEM_NAME} MATCHES Darwin) # macOS
  add_custom_command(
    TARGET AubioBuilder PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../../prebuilt/macos/x64/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/
    COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/scsynth ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/scsynth
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/extra-plugins/ ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/plugins/
    COMMAND ${CMAKE_COMMAND} -E rm -rf ${CMAKE_CURRENT_SOURCE_DIR}/../server/native/supercollider/extra-plugins
  )
endif()

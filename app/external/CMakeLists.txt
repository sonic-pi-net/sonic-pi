# This CMakeLists builds all the external libraries required for a Sonic Pi
cmake_minimum_required(VERSION 3.13)

message(STATUS " Aubio Builder")

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
    INSTALL_COMMAND ""
    CMAKE_ARGS
        -DERLANG_INCLUDE_PATH=${ERLANG_INCLUDE_PATH}
        -DCMAKE_OSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}
        -DUSE_SYSTEM_RTMIDI=${USE_SYSTEM_LIBS}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )

ExternalProject_Add(sp_link
    PREFIX sp_link-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/sp_link
    INSTALL_COMMAND ""
    CMAKE_ARGS
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
    INSTALL_COMMAND ""
    )

# Kick off a generation by making a dummy/empty project
add_library(AubioBuilder STATIC externals.cpp)

# Dependency ensures the externals are built
add_dependencies(AubioBuilder aubio)

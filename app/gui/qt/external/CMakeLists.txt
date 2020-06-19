# This CMakeLists builds all the external libraries required for a Sonic Pi
cmake_minimum_required(VERSION 3.12)

message(STATUS " Aubio Builder")

project(AubioBuilder
    LANGUAGES CXX C
    VERSION 1.0.0
    )

include(ExternalProject)

# sp_midi
ExternalProject_Add(sp_midi
    PREFIX sp_midi-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/sp_midi
    INSTALL_COMMAND ""
    CMAKE_ARGS -DERLANG_INCLUDE_PATH=${ERLANG_INCLUDE_PATH}
    BUILD_COMMAND ${CMAKE_COMMAND} --build . --config Release
    )

ExternalProject_Add(ogg
    PREFIX ogg-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/ogg-1.3.4

    INSTALL_DIR ${CMAKE_BINARY_DIR}/ogg-package
    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
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
        -DPC_OGG_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/ogg-package/lib
        -DPC_FLAC_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/flac-package/include
        -DPC_FLAC_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/flac-package/lib
        -DPC_VORBIS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
        -DPC_VORBIS_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/lib
        -DPC_VORBISENC_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
        -DPC_VORBISENC_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/lib
        -DPC_OPUS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/opus-package/include
        -DPC_OPUS_LIBRARY_DIRS=${CMAKE_BINARY_DIR}/opus-package/lib

        INSTALL_DIR ${CMAKE_BINARY_DIR}/libsndfile-package
        CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${CMAKE_BINARY_DIR}/libsndfile-package
    )

ExternalProject_Add(aubio
    PREFIX aubio-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/aubio
    DEPENDS libsndfile

    CMAKE_ARGS -DLIBSNDFILE_INCLUDE_DIR=${CMAKE_BINARY_DIR}/libsndfile-package/include
            -DLIBSNDFILE_LIBRARY_DIR=${CMAKE_BINARY_DIR}/libsndfile-package/lib
            -DLIBOGG_LIBRARY_DIR=${CMAKE_BINARY_DIR}/ogg-package/lib
            -DLIBVORBIS_LIBRARY_DIR=${CMAKE_BINARY_DIR}/vorbis-package/lib
            -DLIBOPUS_LIBRARY_DIR=${CMAKE_BINARY_DIR}/opus-package/lib
            -DLIBFLAC_LIBRARY_DIR=${CMAKE_BINARY_DIR}/flac-package/lib

            -DPC_OGG_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/ogg-package/include
            -DPC_VORBIS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
            -DPC_VORBISENC_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/vorbis-package/include
            -DPC_OPUS_INCLUDE_DIRS=${CMAKE_BINARY_DIR}/opus-package/include
    INSTALL_COMMAND ""
    )

# Kick off a generation by making a dummy/empty project
add_library(AubioBuilder STATIC externals.cpp)

# Dependency ensures the externals are built
add_dependencies(AubioBuilder aubio)

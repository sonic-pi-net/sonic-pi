# This CMakeLists builds all the external libraries required for a Sonic Pi
cmake_minimum_required(VERSION 3.12)
project(SonicPiServerExternal
    LANGUAGES CXX C
    VERSION 1.0.0
)

# Osmid
include(ExternalProject)
ExternalProject_Add(osmid
    PREFIX osmid-prefix
    SOURCE_DIR "${CMAKE_CURRENT_LIST_DIR}/osmid"
    INSTALL_DIR "${CMAKE_CURRENT_LIST_DIR}/../native/osmid"

    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
)

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Aubio and dependencies
message(STATUS " SonicPiServerExternal - Aubio Builder")

ExternalProject_Add(ogg
    PREFIX ogg-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/ogg-1.3.4
    INSTALL_DIR ${CMAKE_BINARY_DIR}/ogg-package

    CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
    )

ExternalProject_Add(flac
    PREFIX flac-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/flac_1_3_3
    INSTALL_DIR ${CMAKE_BINARY_DIR}/flac-package
    DEPENDS ogg

    CMAKE_ARGS -DOGG_ROOT=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_CXXLIBS=OFF
        -DBUILD_DOCS=OFF
        -DBUILD_EXAMPLES=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON

        -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
    )

ExternalProject_Add(opus
    PREFIX opus-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/opus-1.2
    INSTALL_DIR ${CMAKE_BINARY_DIR}/opus-package
    DEPENDS ogg

    CMAKE_ARGS -DOGG_ROOT=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON

        -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
    )

ExternalProject_Add(vorbis
    PREFIX vorbis-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/vorbis-1.3.4
    INSTALL_DIR ${CMAKE_BINARY_DIR}/vorbis-package
    DEPENDS ogg

    CMAKE_ARGS -DOGG_ROOT=${CMAKE_BINARY_DIR}/ogg-package
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTING=OFF
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON
        # Vorbis fails to install if the install_dir is in double quotes
        -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
    )

ExternalProject_Add(libsndfile
    PREFIX libsndfile-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/libsndfile
    INSTALL_DIR ${CMAKE_BINARY_DIR}/libsndfile-package
    DEPENDS flac vorbis opus

    CMAKE_ARGS
        -DBUILD_PROGRAMS=OFF
        -DBUILD_EXAMPLES=OFF
        -DBUILD_TESTING=OFF
        -DBUILD_REGTEST=OFF
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
        -DCMAKE_POSITION_INDEPENDENT_CODE=ON

        -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
    )

ExternalProject_Add(aubio
    PREFIX aubio-prefix
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/aubio
    INSTALL_DIR ${CMAKE_CURRENT_LIST_DIR}/../native/aubio
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

            -DCMAKE_POSITION_INDEPENDENT_CODE=ON
            -DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>
    )

# Kick off a generation by making a dummy/empty project
add_library(SonicPiServerExternal STATIC externals.cpp)

# Dependency ensures the externals are built
add_dependencies(SonicPiServerExternal aubio)

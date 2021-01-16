cmake_minimum_required(VERSION 3.12)

project(aubio-5)
set(SOURCE_ROOT ${CMAKE_CURRENT_LIST_DIR}/src)

set(SOURCES
    ${SOURCE_ROOT}/exports.def
    ${SOURCE_ROOT}/aubio.h
    ${SOURCE_ROOT}/aubio_priv.h
    ${SOURCE_ROOT}/cvec.c
    ${SOURCE_ROOT}/cvec.h
    ${SOURCE_ROOT}/fmat.c
    ${SOURCE_ROOT}/fmat.h
    ${SOURCE_ROOT}/fvec.c
    ${SOURCE_ROOT}/fvec.h
    ${SOURCE_ROOT}/lvec.c
    ${SOURCE_ROOT}/lvec.h
    ${SOURCE_ROOT}/mathutils.c
    ${SOURCE_ROOT}/mathutils.h
    ${SOURCE_ROOT}/musicutils.c
    ${SOURCE_ROOT}/musicutils.h
    ${SOURCE_ROOT}/types.h
    ${SOURCE_ROOT}/vecutils.c
    ${SOURCE_ROOT}/vecutils.h
    ${SOURCE_ROOT}/wscript_build
    ${SOURCE_ROOT}/io/audio_unit.c
    ${SOURCE_ROOT}/io/audio_unit.h
    ${SOURCE_ROOT}/io/ioutils.c
    ${SOURCE_ROOT}/io/ioutils.h
    ${SOURCE_ROOT}/io/sink.c
    ${SOURCE_ROOT}/io/sink.h
    ${SOURCE_ROOT}/io/sink_apple_audio.c
    ${SOURCE_ROOT}/io/sink_apple_audio.h
    ${SOURCE_ROOT}/io/sink_sndfile.c
    ${SOURCE_ROOT}/io/sink_sndfile.h
    ${SOURCE_ROOT}/io/sink_wavwrite.c
    ${SOURCE_ROOT}/io/sink_wavwrite.h
    ${SOURCE_ROOT}/io/source.c
    ${SOURCE_ROOT}/io/source.h
    ${SOURCE_ROOT}/io/source_apple_audio.c
    ${SOURCE_ROOT}/io/source_apple_audio.h
    ${SOURCE_ROOT}/io/source_avcodec.c
    ${SOURCE_ROOT}/io/source_avcodec.h
    ${SOURCE_ROOT}/io/source_sndfile.c
    ${SOURCE_ROOT}/io/source_sndfile.h
    ${SOURCE_ROOT}/io/source_wavread.c
    ${SOURCE_ROOT}/io/source_wavread.h
    ${SOURCE_ROOT}/io/utils_apple_audio.c
    ${SOURCE_ROOT}/notes/notes.c
    ${SOURCE_ROOT}/notes/notes.h
    ${SOURCE_ROOT}/onset/onset.c
    ${SOURCE_ROOT}/onset/onset.h
    ${SOURCE_ROOT}/onset/peakpicker.c
    ${SOURCE_ROOT}/onset/peakpicker.h
    ${SOURCE_ROOT}/pitch/pitch.c
    ${SOURCE_ROOT}/pitch/pitch.h
    ${SOURCE_ROOT}/pitch/pitchfcomb.c
    ${SOURCE_ROOT}/pitch/pitchfcomb.h
    ${SOURCE_ROOT}/pitch/pitchmcomb.c
    ${SOURCE_ROOT}/pitch/pitchmcomb.h
    ${SOURCE_ROOT}/pitch/pitchschmitt.c
    ${SOURCE_ROOT}/pitch/pitchschmitt.h
    ${SOURCE_ROOT}/pitch/pitchspecacf.c
    ${SOURCE_ROOT}/pitch/pitchspecacf.h
    ${SOURCE_ROOT}/pitch/pitchyin.c
    ${SOURCE_ROOT}/pitch/pitchyin.h
    ${SOURCE_ROOT}/pitch/pitchyinfast.c
    ${SOURCE_ROOT}/pitch/pitchyinfast.h
    ${SOURCE_ROOT}/pitch/pitchyinfft.c
    ${SOURCE_ROOT}/pitch/pitchyinfft.h
    ${SOURCE_ROOT}/spectral/awhitening.c
    ${SOURCE_ROOT}/spectral/awhitening.h
    ${SOURCE_ROOT}/spectral/dct.c
    ${SOURCE_ROOT}/spectral/dct.h
    ${SOURCE_ROOT}/spectral/dct_accelerate.c
    ${SOURCE_ROOT}/spectral/dct_fftw.c
    ${SOURCE_ROOT}/spectral/dct_ipp.c
    ${SOURCE_ROOT}/spectral/dct_ooura.c
    ${SOURCE_ROOT}/spectral/dct_plain.c
    ${SOURCE_ROOT}/spectral/fft.c
    ${SOURCE_ROOT}/spectral/fft.h
    ${SOURCE_ROOT}/spectral/filterbank.c
    ${SOURCE_ROOT}/spectral/filterbank.h
    ${SOURCE_ROOT}/spectral/filterbank_mel.c
    ${SOURCE_ROOT}/spectral/filterbank_mel.h
    ${SOURCE_ROOT}/spectral/mfcc.c
    ${SOURCE_ROOT}/spectral/mfcc.h
    ${SOURCE_ROOT}/spectral/ooura_fft8g.c
    ${SOURCE_ROOT}/spectral/phasevoc.c
    ${SOURCE_ROOT}/spectral/phasevoc.h
    ${SOURCE_ROOT}/spectral/specdesc.c
    ${SOURCE_ROOT}/spectral/specdesc.h
    ${SOURCE_ROOT}/spectral/statistics.c
    ${SOURCE_ROOT}/spectral/tss.c
    ${SOURCE_ROOT}/spectral/tss.h
    ${SOURCE_ROOT}/synth/sampler.c
    ${SOURCE_ROOT}/synth/sampler.h
    ${SOURCE_ROOT}/synth/wavetable.c
    ${SOURCE_ROOT}/synth/wavetable.h
    ${SOURCE_ROOT}/tempo/beattracking.c
    ${SOURCE_ROOT}/tempo/beattracking.h
    ${SOURCE_ROOT}/tempo/tempo.c
    ${SOURCE_ROOT}/tempo/tempo.h
    ${SOURCE_ROOT}/temporal/a_weighting.c
    ${SOURCE_ROOT}/temporal/a_weighting.h
    ${SOURCE_ROOT}/temporal/biquad.c
    ${SOURCE_ROOT}/temporal/biquad.h
    ${SOURCE_ROOT}/temporal/c_weighting.c
    ${SOURCE_ROOT}/temporal/c_weighting.h
    ${SOURCE_ROOT}/temporal/filter.c
    ${SOURCE_ROOT}/temporal/filter.h
    ${SOURCE_ROOT}/temporal/resampler.c
    ${SOURCE_ROOT}/temporal/resampler.h
    ${SOURCE_ROOT}/utils/hist.c
    ${SOURCE_ROOT}/utils/hist.h
    ${SOURCE_ROOT}/utils/log.c
    ${SOURCE_ROOT}/utils/log.h
    ${SOURCE_ROOT}/utils/parameter.c
    ${SOURCE_ROOT}/utils/parameter.h
    ${SOURCE_ROOT}/utils/scale.c
    ${SOURCE_ROOT}/utils/scale.h
    ${SOURCE_ROOT}/utils/windll.c
)

add_library(${PROJECT_NAME} SHARED ${SOURCES} ${RESOURCES}) # Win32 ignored on non-windows
target_include_directories(${PROJECT_NAME}
    PRIVATE
    src
    ${LIBSNDFILE_INCLUDE_DIR}
)

# 'lib' is appended to the library name automatically on most non-Windows platforms
if (WIN32)
    # Add extra 'lib'
    set_property(TARGET ${PROJECT_NAME} PROPERTY PREFIX "lib")
endif()

message(STATUS "CXX compiler: ${CMAKE_CXX_COMPILER_ID}")
if(CMAKE_CXX_COMPILER_ID MATCHES "MSVC") # Visual Studio
    message(STATUS "Configuring for Visual Studio...")
    #set_property(TARGET ${PROJECT_NAME} APPEND_STRING PROPERTY LINK_FLAGS " /WHOLEARCHIVE") # Not required on Windows?
    target_link_libraries(${PROJECT_NAME}
        PRIVATE
        ${LIBOGG_LIBRARY_DIR}/ogg${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/vorbis${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/vorbisenc${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/vorbisfile${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBOPUS_LIBRARY_DIR}/opus${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBFLAC_LIBRARY_DIR}/FLAC${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBSNDFILE_LIBRARY_DIR}/sndfile${CMAKE_STATIC_LIBRARY_SUFFIX}
    )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "AppleClang") # Clang
    message(STATUS "Configuring for Clang...")
    target_link_libraries(${PROJECT_NAME}
        PRIVATE
        "-Wl,-all_load" # Make sure all required symbols etc. are imported
        ${LIBOGG_LIBRARY_DIR}/libogg${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/libvorbis${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/libvorbisenc${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/libvorbisfile${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBOPUS_LIBRARY_DIR}/libopus${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBFLAC_LIBRARY_DIR}/libFLAC${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBSNDFILE_LIBRARY_DIR}/libsndfile${CMAKE_STATIC_LIBRARY_SUFFIX}
        m # C math library
    )
else() # Assume GCC
    if (NOT CMAKE_CXX_COMPILER_ID MATCHES "GNU")
        message(WARNING "Unknown compiler, defaulting to same link libraries/options as GCC...")
    endif()
    message(STATUS "Configuring for GCC...")
    target_link_libraries(${PROJECT_NAME}
        PRIVATE
        "-Wl,--whole-archive" # Make sure all required symbols etc. are imported
        ${LIBOGG_LIBRARY_DIR}/libogg${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/libvorbis${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/libvorbisenc${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBVORBIS_LIBRARY_DIR}/libvorbisfile${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBOPUS_LIBRARY_DIR}/libopus${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBFLAC_LIBRARY_DIR}/libFLAC${CMAKE_STATIC_LIBRARY_SUFFIX}
        ${LIBSNDFILE_LIBRARY_DIR}/libsndfile${CMAKE_STATIC_LIBRARY_SUFFIX}
        m # C math library
        "-Wl,--no-whole-archive"
        "-Wl,-z,defs" # Report unresolved symbols in object files
    )
endif()

# Note; these definitions need investigating, this is just what seems to work for Aubio
target_compile_definitions(${PROJECT_NAME}
    PRIVATE
    -DHAVE_C99_VARARGS_MACROS
    -DHAVE_STDLIB_H
    -DHAVE_STDIO_H
    -DHAVE_COMPLEX_H
    -DHAVE_MATH_H
    -DHAVE_STRING_H
    -DHAVE_ERRNO_H
    -DHAVE_LIMITS_H
    -DHAVE_STDARG_H
    -DHAVE_BLAS_H # What is this?
    -DHAVE_ATLAS_CBLAS_H # ?
    -DHAVE_OPENBLAS_CBLAS_H # ?
    -DHAVE_CBLAS_H # ?
    -DHAVE_WAVREAD
    -DHAVE_WAVWRITE
    -DHAVE_AVRESAMPLE
    -DHAVE_SWRESAMPLE
    -DHAVE_MEMCPY_HACKS
    -DHAVE_SNDFILE
    #-DHAVE_ACCELERATE
    #-DHAVE_INTEL_IPP (needs intel lib)
    #-DHAVE_SAMPLERATE (needs extra lib in path)
    #-DHAVE_AUBIO_DOUBLE (Not used if Sample rate is used)
    #-DHAVE_WAVREAD (nneds extra lib in path)
    #-DHAVE_FFTW3 (needs extra lib in path)
    #-DHAVE_SOURCE_APPLE_AUDIO ?
    #-DHAVE_SINK_APPLE_AUDIO ?
    )

if (WIN32)
    target_compile_definitions(${PROJECT_NAME}
    PRIVATE
    -DHAVE_WIN_HACKS)
endif()

# Disable warnings
if(WIN32)
    #target_compile_options(${PROJECT_NAME} PRIVATE )
elseif(${CMAKE_SYSTEM_NAME} MATCHES Linux)
    #target_compile_options(${PROJECT_NAME}
    #PRIVATE
    #)
elseif(${CMAKE_SYSTEM_NAME} MATCHES Darwin) # macOS
    #target_compile_options(${PROJECT_NAME}
    #PRIVATE
    #)
else()
    message(FATAL_ERROR "Aubio is not supported on this platform.")
endif()

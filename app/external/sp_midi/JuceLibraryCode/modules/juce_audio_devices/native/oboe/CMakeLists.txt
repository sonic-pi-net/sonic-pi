cmake_minimum_required(VERSION 3.4.1)

# Set the name of the project and store it in PROJECT_NAME. Also set the following variables:
# PROJECT_SOURCE_DIR (usually the root directory where Oboe has been cloned e.g.)
# PROJECT_BINARY_DIR (usually the containing project's binary directory,
# e.g. ${OBOE_HOME}/samples/RhythmGame/.externalNativeBuild/cmake/ndkExtractorDebug/x86/oboe-bin)
project(oboe)

set (oboe_sources
    src/aaudio/AAudioLoader.cpp
    src/aaudio/AudioStreamAAudio.cpp
    src/common/AudioSourceCaller.cpp
    src/common/AudioStream.cpp
    src/common/AudioStreamBuilder.cpp
    src/common/DataConversionFlowGraph.cpp
    src/common/FilterAudioStream.cpp
    src/common/FixedBlockAdapter.cpp
    src/common/FixedBlockReader.cpp
    src/common/FixedBlockWriter.cpp
    src/common/LatencyTuner.cpp
    src/common/SourceFloatCaller.cpp
    src/common/SourceI16Caller.cpp
    src/common/Utilities.cpp
    src/common/QuirksManager.cpp
    src/fifo/FifoBuffer.cpp
    src/fifo/FifoController.cpp
    src/fifo/FifoControllerBase.cpp
    src/fifo/FifoControllerIndirect.cpp
        src/flowgraph/FlowGraphNode.cpp
    src/flowgraph/ClipToRange.cpp
    src/flowgraph/ManyToMultiConverter.cpp
    src/flowgraph/MonoToMultiConverter.cpp
    src/flowgraph/RampLinear.cpp
    src/flowgraph/SampleRateConverter.cpp
    src/flowgraph/SinkFloat.cpp
    src/flowgraph/SinkI16.cpp
    src/flowgraph/SinkI24.cpp
    src/flowgraph/SourceFloat.cpp
    src/flowgraph/SourceI16.cpp
    src/flowgraph/SourceI24.cpp
    src/flowgraph/resampler/IntegerRatio.cpp
    src/flowgraph/resampler/LinearResampler.cpp
    src/flowgraph/resampler/MultiChannelResampler.cpp
    src/flowgraph/resampler/PolyphaseResampler.cpp
    src/flowgraph/resampler/PolyphaseResamplerMono.cpp
    src/flowgraph/resampler/PolyphaseResamplerStereo.cpp
    src/flowgraph/resampler/SincResampler.cpp
    src/flowgraph/resampler/SincResamplerStereo.cpp
    src/opensles/AudioInputStreamOpenSLES.cpp
    src/opensles/AudioOutputStreamOpenSLES.cpp
    src/opensles/AudioStreamBuffered.cpp
    src/opensles/AudioStreamOpenSLES.cpp
    src/opensles/EngineOpenSLES.cpp
    src/opensles/OpenSLESUtilities.cpp
    src/opensles/OutputMixerOpenSLES.cpp
    src/common/StabilizedCallback.cpp
    src/common/Trace.cpp
    src/common/Version.cpp
    )

add_library(oboe ${oboe_sources})

# Specify directories which the compiler should look for headers
target_include_directories(oboe
        PRIVATE src
        PUBLIC include)

# JUCE CHANGE STARTS HERE

# This comment provided for Apache License compliance. We've removed the extra warnings flags and
# the `-Werror` option, to avoid cases where compilers produce unexpected errors and fail the build.
# We've also removed the explicit `-std=c++14` compile option, and replaced it with a more
# cmake-friendly way of specifying the language standard.

target_compile_options(oboe PRIVATE -Ofast)
set_target_properties(oboe PROPERTIES CXX_STANDARD 14 CXX_STANDARD_REQUIRED TRUE CXX_EXTENSIONS FALSE)

# JUCE CHANGE ENDS HERE

# Enable logging for debug builds
target_compile_definitions(oboe PUBLIC $<$<CONFIG:DEBUG>:OBOE_ENABLE_LOGGING=1>)

target_link_libraries(oboe PRIVATE log OpenSLES)

# When installing oboe put the libraries in the lib/<ABI> folder e.g. lib/arm64-v8a
install(TARGETS oboe
        LIBRARY DESTINATION lib/${ANDROID_ABI}
        ARCHIVE DESTINATION lib/${ANDROID_ABI})

# Also install the headers
install(DIRECTORY include/oboe DESTINATION include)

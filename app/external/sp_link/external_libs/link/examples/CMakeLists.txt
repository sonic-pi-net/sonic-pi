cmake_minimum_required(VERSION 3.0)
project(LinkExamples)

#     _    ____ ___ ___
#    / \  / ___|_ _/ _ \
#   / _ \ \___ \| | | | |
#  / ___ \ ___) | | |_| |
# /_/   \_\____/___\___/
#

if(WIN32)
  function(configure_asio asio_sdk_path_OUT)
    # ASIO-related path/file variables
    set(asio_download_root "https://www.steinberg.net/sdk_downloads")
    set(asio_file_name "asiosdk2.3.zip")
    set(asio_dir_name "ASIOSDK2.3")
    set(asio_working_dir "${CMAKE_BINARY_DIR}/modules")
    set(asio_output_path "${asio_working_dir}/${asio_file_name}")

    message(STATUS "Downloading ASIO SDK")
    file(DOWNLOAD "${asio_download_root}/${asio_file_name}" ${asio_output_path})
    file(SHA1 ${asio_output_path} asio_zip_hash)
    message("   ASIO SDK SHA1: ${asio_zip_hash}")

    message("   Extracting ASIO SDK")
    execute_process(COMMAND ${CMAKE_COMMAND} -E tar "xf" ${asio_output_path} --format=zip
      WORKING_DIRECTORY ${asio_working_dir}
      INPUT_FILE ${asio_output_path}
    )

    # Set the ASIO SDK path for the caller
    set(${asio_sdk_path_OUT} "${asio_working_dir}/${asio_dir_name}" PARENT_SCOPE)
  endfunction()
endif()

#     _             _ _
#    / \  _   _  __| (_) ___
#   / _ \| | | |/ _` | |/ _ \
#  / ___ \ |_| | (_| | | (_) |
# /_/   \_\__,_|\__,_|_|\___/
#

set(linkhut_audio_SOURCES)

if(APPLE)
  set(linkhut_audio_SOURCES
    linkaudio/AudioPlatform_CoreAudio.hpp
    linkaudio/AudioPlatform_CoreAudio.cpp
  )
elseif(WIN32)
  if(LINK_BUILD_ASIO)
    configure_asio(asio_sdk_path)

    include_directories(${asio_sdk_path}/common)
    include_directories(${asio_sdk_path}/host)
    include_directories(${asio_sdk_path}/host/pc)

    set(linkhut_audio_SOURCES
      ${asio_sdk_path}/common/asio.cpp
      ${asio_sdk_path}/host/asiodrivers.cpp
      ${asio_sdk_path}/host/pc/asiolist.cpp
      linkaudio/AudioPlatform_Asio.hpp
      linkaudio/AudioPlatform_Asio.cpp
    )
  else()
    message(WARNING "LinkHut has been configured to be built with the WASAPI audio "
      "driver. This driver is considered experimental and has problems with low-latency "
      "playback. Please consider using the ASIO driver instead.")
    set(linkhut_audio_SOURCES
      linkaudio/AudioPlatform_Wasapi.hpp
      linkaudio/AudioPlatform_Wasapi.cpp
    )
  endif()
elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
  if(LINK_BUILD_JACK)
    set(linkhut_audio_SOURCES
      linkaudio/AudioPlatform_Jack.hpp
      linkaudio/AudioPlatform_Jack.cpp
    )
  else()
    set(linkhut_audio_SOURCES
      linkaudio/AudioPlatform_Portaudio.hpp
      linkaudio/AudioPlatform_Portaudio.cpp
    )
  endif()
endif()

include_directories(linkaudio)
source_group("Audio Sources" FILES ${linkhut_audio_SOURCES})

#   ____
#  / ___|___  _ __ ___  _ __ ___   ___  _ __
# | |   / _ \| '_ ` _ \| '_ ` _ \ / _ \| '_ \
# | |__| (_) | | | | | | | | | | | (_) | | | |
#  \____\___/|_| |_| |_|_| |_| |_|\___/|_| |_|
#

function(configure_linkhut_executable target)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    target_link_libraries(${target} atomic pthread)
  endif()

  target_link_libraries(${target} Ableton::Link)
endfunction()

function(configure_linkhut_audio_sources target)
  if(APPLE)
    target_link_libraries(${target} "-framework AudioUnit")
    target_compile_definitions(${target} PRIVATE
      -DLINKHUT_AUDIO_PLATFORM_COREAUDIO=1
    )
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    if(LINK_BUILD_JACK)
      target_link_libraries(${target} jack)
      target_compile_definitions(${target} PRIVATE
        -DLINKHUT_AUDIO_PLATFORM_JACK=1
      )
    else()
      target_link_libraries(${target} asound portaudio)
      target_compile_definitions(${target} PRIVATE
        -DLINKHUT_AUDIO_PLATFORM_PORTAUDIO=1
      )
    endif()
  elseif(WIN32)
    if(LINK_BUILD_ASIO)
      # ASIO uses lots of old-school string APIs from the C stdlib
      add_definitions("/D_CRT_SECURE_NO_WARNINGS")
      target_compile_definitions(${target} PRIVATE
        -DLINKHUT_AUDIO_PLATFORM_ASIO=1
      )
    else()
      target_compile_definitions(${target} PRIVATE
        -DLINKHUT_AUDIO_PLATFORM_WASAPI=1
      )
      endif()

    target_link_libraries(${target} winmm)
  endif()

endfunction()

if(WIN32)
  # When building LinkHut, additional warnings are generated from third-party frameworks
  set(extra_ignored_warnings_LIST
    "/wd4127" # conditional expression is constant
    "/wd4242" # 'identifier' : conversion from 'type1' to 'type2', possible loss of data
    "/wd4619" # #pragma warning : there is no warning number 'number'
    "/wd4668" # 'symbol' is not defined as a preprocessor macro, replacing with '0' for 'directives'
    "/wd4702" # unreachable code
    "/wd4946" # reinterpret_cast used between related classes: 'class1' and 'class2'
  )
  if(LINK_BUILD_ASIO)
    set(extra_ignored_warnings_LIST
      ${extra_ignored_warnings_LIST}
      "/wd4267" # 'argument': conversion from '?' to '?', possible loss of data
      "/wd4477" # 'printf': format string '%?' requires an argument of type '?'
    )
  else()
    set(extra_ignored_warnings_LIST
      ${extra_ignored_warnings_LIST}
      "/wd4191" # 'operator/operation' : unsafe conversion from 'type of expression' to 'type required'
    )
  endif()
  string(REPLACE ";" " " extra_ignored_warnings "${extra_ignored_warnings_LIST}")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${extra_ignored_warnings}")
endif()

#  _     _       _    _   _       _
# | |   (_)_ __ | | _| | | |_   _| |_
# | |   | | '_ \| |/ / |_| | | | | __|
# | |___| | | | |   <|  _  | |_| | |_
# |_____|_|_| |_|_|\_\_| |_|\__,_|\__|
#

set(linkhut_HEADERS
  linkaudio/AudioEngine.hpp
  linkaudio/AudioPlatform.hpp
  ${link_HEADERS}
)

set(linkhut_SOURCES
  linkaudio/AudioEngine.cpp
  linkhut/main.cpp
)

add_executable(LinkHut
  ${linkhut_HEADERS}
  ${linkhut_SOURCES}
  ${linkhut_audio_SOURCES}
)
configure_linkhut_audio_sources(LinkHut)
configure_linkhut_executable(LinkHut)
source_group("LinkHut" FILES ${linkhut_HEADERS} ${linkhut_SOURCES})

#  _     _       _    _   _       _   ____  _ _            _
# | |   (_)_ __ | | _| | | |_   _| |_/ ___|(_) | ___ _ __ | |_
# | |   | | '_ \| |/ / |_| | | | | __\___ \| | |/ _ \ '_ \| __|
# | |___| | | | |   <|  _  | |_| | |_ ___) | | |  __/ | | | |_
# |_____|_|_| |_|_|\_\_| |_|\__,_|\__|____/|_|_|\___|_| |_|\__|
#

set(linkhutsilent_HEADERS
  linkaudio/AudioEngine.hpp
  linkaudio/AudioPlatform_Dummy.hpp
  ${link_HEADERS}
)

set(linkhutsilent_SOURCES
  linkaudio/AudioEngine.cpp
  linkhut/main.cpp
)

add_executable(LinkHutSilent
  ${linkhutsilent_HEADERS}
  ${linkhutsilent_SOURCES}
)

target_compile_definitions(LinkHutSilent PRIVATE
  -DLINKHUT_AUDIO_PLATFORM_DUMMY=1
)

configure_linkhut_executable(LinkHutSilent)
source_group("LinkHutSilent" FILES ${linkhutsilent_HEADERS} ${linkhutsilent_SOURCES})

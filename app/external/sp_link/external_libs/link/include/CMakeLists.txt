cmake_minimum_required(VERSION 3.0)
project(LinkCore)

#   ____
#  / ___|___  _ __ ___
# | |   / _ \| '__/ _ \
# | |__| (_) | | |  __/
#  \____\___/|_|  \___|
#

set(link_core_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ableton/link)
set(link_core_HEADERS
  ${link_core_DIR}/Beats.hpp
  ${link_core_DIR}/CircularFifo.hpp
  ${link_core_DIR}/ClientSessionTimelines.hpp
  ${link_core_DIR}/Controller.hpp
  ${link_core_DIR}/Gateway.hpp
  ${link_core_DIR}/GhostXForm.hpp
  ${link_core_DIR}/HostTimeFilter.hpp
  ${link_core_DIR}/Kalman.hpp
  ${link_core_DIR}/LinearRegression.hpp
  ${link_core_DIR}/Measurement.hpp
  ${link_core_DIR}/MeasurementEndpointV4.hpp
  ${link_core_DIR}/MeasurementService.hpp
  ${link_core_DIR}/NodeId.hpp
  ${link_core_DIR}/NodeState.hpp
  ${link_core_DIR}/PayloadEntries.hpp
  ${link_core_DIR}/Optional.hpp
  ${link_core_DIR}/Peers.hpp
  ${link_core_DIR}/PeerState.hpp
  ${link_core_DIR}/Phase.hpp
  ${link_core_DIR}/PingResponder.hpp
  ${link_core_DIR}/SessionId.hpp
  ${link_core_DIR}/SessionState.hpp
  ${link_core_DIR}/Sessions.hpp
  ${link_core_DIR}/StartStopState.hpp
  ${link_core_DIR}/Tempo.hpp
  ${link_core_DIR}/Timeline.hpp
  ${link_core_DIR}/v1/Messages.hpp
  PARENT_SCOPE
)

#  ____  _
# |  _ \(_)___  ___ _____   _____ _ __ _   _
# | | | | / __|/ __/ _ \ \ / / _ \ '__| | | |
# | |_| | \__ \ (_| (_) \ V /  __/ |  | |_| |
# |____/|_|___/\___\___/ \_/ \___|_|   \__, |
#                                      |___/

set(link_discovery_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ableton/discovery)
set(link_discovery_HEADERS
  ${link_discovery_DIR}/InterfaceScanner.hpp
  ${link_discovery_DIR}/IpV4Interface.hpp
  ${link_discovery_DIR}/MessageTypes.hpp
  ${link_discovery_DIR}/NetworkByteStreamSerializable.hpp
  ${link_discovery_DIR}/Payload.hpp
  ${link_discovery_DIR}/PeerGateway.hpp
  ${link_discovery_DIR}/PeerGateways.hpp
  ${link_discovery_DIR}/Service.hpp
  ${link_discovery_DIR}/UdpMessenger.hpp
  ${link_discovery_DIR}/v1/Messages.hpp
  PARENT_SCOPE
)

#  ____  _       _    __
# |  _ \| | __ _| |_ / _| ___  _ __ _ __ ___
# | |_) | |/ _` | __| |_ / _ \| '__| '_ ` _ \
# |  __/| | (_| | |_|  _| (_) | |  | | | | | |
# |_|   |_|\__,_|\__|_|  \___/|_|  |_| |_| |_|
#

set(link_platform_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ableton/platforms)
set(link_platform_HEADERS
  ${link_platform_DIR}/Config.hpp
  ${link_platform_DIR}/asio/AsioTimer.hpp
  ${link_platform_DIR}/asio/AsioWrapper.hpp
  ${link_platform_DIR}/asio/Context.hpp
  ${link_platform_DIR}/asio/LockFreeCallbackDispatcher.hpp
  ${link_platform_DIR}/asio/Socket.hpp
  ${link_platform_DIR}/asio/Util.hpp
)

if(ESP_PLATFORM)
  set(link_platform_HEADERS
    ${link_platform_HEADERS}
    ${link_platform_DIR}/esp32/Clock.hpp
    ${link_platform_DIR}/esp32/Context.hpp
    ${link_platform_DIR}/esp32/Esp32.hpp
    ${link_platform_DIR}/esp32/Random.hpp
    ${link_platform_DIR}/esp32/ScanIpIfAddrs.hpp
  )
elseif(UNIX)
  set(link_platform_HEADERS
    ${link_platform_HEADERS}
    ${link_platform_DIR}/posix/ScanIpIfAddrs.hpp
  )

  if(APPLE)
    set(link_platform_HEADERS
      ${link_platform_HEADERS}
      ${link_platform_DIR}/darwin/Clock.hpp
      ${link_platform_DIR}/darwin/Darwin.hpp
      ${link_platform_DIR}/stl/Random.hpp
    )
  elseif(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    set(link_platform_HEADERS
      ${link_platform_HEADERS}
      ${link_platform_DIR}/linux/Clock.hpp
      ${link_platform_DIR}/linux/Linux.hpp
      ${link_platform_DIR}/stl/Clock.hpp
      ${link_platform_DIR}/stl/Random.hpp
    )
  endif()
elseif(WIN32)
  set(link_platform_HEADERS
    ${link_platform_HEADERS}
    ${link_platform_DIR}/stl/Random.hpp
    ${link_platform_DIR}/windows/Clock.hpp
    ${link_platform_DIR}/windows/ScanIpIfAddrs.hpp
    ${link_platform_DIR}/windows/Windows.hpp
  )
endif()
set(link_platform_HEADERS
  ${link_platform_HEADERS}
  PARENT_SCOPE
)

#  _   _ _   _ _
# | | | | |_(_) |
# | | | | __| | |
# | |_| | |_| | |
#  \___/ \__|_|_|
#

set(link_util_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ableton/util)
set(link_util_HEADERS
  ${link_util_DIR}/Injected.hpp
  ${link_util_DIR}/Log.hpp
  ${link_util_DIR}/SafeAsyncHandler.hpp
  ${link_util_DIR}/SampleTiming.hpp
  PARENT_SCOPE
)

#  _____                       _           _
# | ____|_  ___ __   ___  _ __| |_ ___  __| |
# |  _| \ \/ / '_ \ / _ \| '__| __/ _ \/ _` |
# | |___ >  <| |_) | (_) | |  | ||  __/ (_| |
# |_____/_/\_\ .__/ \___/|_|   \__\___|\__,_|
#            |_|

# This list contains all of the headers needed by most Link projects.
# Usually, just adding this variable to your linker targets will place
# all relevant Link headers in your project.
set(link_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ableton)
set(link_HEADERS
  ${link_core_HEADERS}
  ${link_discovery_HEADERS}
  ${link_platform_HEADERS}
  ${link_util_HEADERS}
  ${link_DIR}/Link.hpp
  ${link_DIR}/Link.ipp
  PARENT_SCOPE
)

set(link_test_DIR ${CMAKE_CURRENT_SOURCE_DIR}/ableton/test)
set(link_test_HEADERS
  ${link_discovery_DIR}/test/Interface.hpp
  ${link_discovery_DIR}/test/PayloadEntries.hpp
  ${link_discovery_DIR}/test/Socket.hpp
  ${link_util_DIR}/test/IoService.hpp
  ${link_util_DIR}/test/Timer.hpp
  ${link_test_DIR}/CatchWrapper.hpp
  ${link_test_DIR}/serial_io/Context.hpp
  ${link_test_DIR}/serial_io/Fixture.hpp
  ${link_test_DIR}/serial_io/SchedulerTree.hpp
  ${link_test_DIR}/serial_io/Timer.hpp
  PARENT_SCOPE
)

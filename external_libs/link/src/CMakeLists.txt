cmake_minimum_required(VERSION 3.0)
project(LinkTest)

#  ____
# / ___|  ___  _   _ _ __ ___ ___  ___
# \___ \ / _ \| | | | '__/ __/ _ \/ __|
#  ___) | (_) | |_| | | | (_|  __/\__ \
# |____/ \___/ \__,_|_|  \___\___||___/
#

set(link_discovery_test_SOURCES
  ableton/discovery/tst_InterfaceScanner.cpp
  ableton/discovery/tst_Payload.cpp
  ableton/discovery/tst_PeerGateway.cpp
  ableton/discovery/tst_PeerGateways.cpp
  ableton/discovery/tst_UdpMessenger.cpp
  ableton/discovery/v1/tst_Messages.cpp
)

set(link_core_test_SOURCES
  ableton/link/tst_Beats.cpp
  ableton/link/tst_CircularFifo.cpp
  ableton/link/tst_ClientSessionTimelines.cpp
  ableton/link/tst_Controller.cpp
  ableton/link/tst_HostTimeFilter.cpp
  ableton/link/tst_Kalman.cpp
  ableton/link/tst_LinearRegression.cpp
  ableton/link/tst_Measurement.cpp
  ableton/link/tst_Peers.cpp
  ableton/link/tst_Phase.cpp
  ableton/link/tst_PingResponder.cpp
  ableton/link/tst_StartStopState.cpp
  ableton/link/tst_Tempo.cpp
  ableton/link/tst_Timeline.cpp
)

set(link_test_SOURCES
  ableton/test/catch/CatchMain.cpp
  ableton/test/serial_io/SchedulerTree.cpp
)

#  ____                              ____
# / ___|  ___  _   _ _ __ ___ ___   / ___|_ __ ___  _   _ _ __  ___
# \___ \ / _ \| | | | '__/ __/ _ \ | |  _| '__/ _ \| | | | '_ \/ __|
#  ___) | (_) | |_| | | | (_|  __/ | |_| | | | (_) | |_| | |_) \__ \
# |____/ \___/ \__,_|_|  \___\___|  \____|_|  \___/ \__,_| .__/|___/
#                                                        |_|

source_group("Link" FILES
  ${link_core_HEADERS}
)

source_group("Discovery" FILES
  ${link_discovery_HEADERS}
)

source_group("Platform" FILES
  ${link_platform_HEADERS}
)

source_group("Util" FILES
  ${link_util_HEADERS}
)

source_group("Test Utils" FILES
  ${link_test_HEADERS}
  ${link_test_SOURCES}
)

#  _____                    _
# |_   _|_ _ _ __ __ _  ___| |_  ___
#   | |/ _` | '__/ _` |/ _ \ __|/ __|
#   | | (_| | | | (_| |  __/ |_ \__ \
#   |_|\__,_|_|  \__, |\___|\__||___/
#                |___/

function(configure_link_test_executable target)
  if(${CMAKE_SYSTEM_NAME} MATCHES "Linux")
    target_link_libraries(${target} atomic pthread)
  endif()
  target_link_libraries(${target} Catch::Catch Ableton::Link)
endfunction()

# For the LinkCore test suite, we add header dependencies individually so that
# the source groups above are shown correctly to the project. However, most
# other projects integrating Link won't need this functionality, and they should
# should just depend on ${link_HEADERS}.
add_executable(LinkCoreTest
  ${link_core_HEADERS}
  ${link_discovery_HEADERS}
  ${link_platform_HEADERS}
  ${link_util_HEADERS}
  ${link_test_HEADERS}

  ${link_core_test_SOURCES}
  ${link_test_SOURCES}
)
configure_link_test_executable(LinkCoreTest)

# For the LinkDiscovery test suite, we only add dependencies on the headers
# necessary to compile these tests, since the Discovery feature should not have
# dependencies on the Link core code.  Normal targets should always use
# ${link_HEADERS} for their dependencies.
add_executable(LinkDiscoveryTest
  ${link_discovery_HEADERS}
  ${link_platform_HEADERS}
  ${link_util_HEADERS}
  ${link_test_HEADERS}

  ${link_discovery_test_SOURCES}
  ${link_test_SOURCES}
)
configure_link_test_executable(LinkDiscoveryTest)

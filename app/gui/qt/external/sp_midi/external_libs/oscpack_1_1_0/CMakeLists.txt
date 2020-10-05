cmake_minimum_required(VERSION 2.6)
PROJECT(TestOscpack)

INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR})

# separate versions of NetworkingUtils.cpp and UdpSocket.cpp are provided for Win32 and POSIX
# the IpSystemTypePath selects the correct ones based on the current platform

IF(WIN32)
 set(IpSystemTypePath ip/win32)
 set(LIBS ${LIBS} Ws2_32 winmm)
ELSE(WIN32)
 set(IpSystemTypePath ip/posix)
ENDIF(WIN32)

ADD_LIBRARY(oscpack 

ip/IpEndpointName.h
ip/IpEndpointName.cpp

ip/NetworkingUtils.h
${IpSystemTypePath}/NetworkingUtils.cpp

ip/UdpSocket.h
${IpSystemTypePath}/UdpSocket.cpp

ip/PacketListener.h
ip/TimerListener.h

osc/OscTypes.h
osc/OscTypes.cpp 
osc/OscHostEndianness.h
osc/OscException.h
osc/OscPacketListener.h
osc/MessageMappingOscPacketListener.h
osc/OscReceivedElements.h
osc/OscReceivedElements.cpp
osc/OscPrintReceivedElements.h
osc/OscPrintReceivedElements.cpp
osc/OscOutboundPacketStream.h
osc/OscOutboundPacketStream.cpp

)


#ADD_EXECUTABLE(OscUnitTests tests/OscUnitTests.cpp)
#TARGET_LINK_LIBRARIES(OscUnitTests oscpack ${LIBS})

#ADD_EXECUTABLE(OscSendTests tests/OscSendTests.cpp)
#TARGET_LINK_LIBRARIES(OscSendTests oscpack ${LIBS})

#ADD_EXECUTABLE(OscReceiveTest tests/OscReceiveTest.cpp)
#TARGET_LINK_LIBRARIES(OscReceiveTest oscpack ${LIBS})


#ADD_EXECUTABLE(OscDump examples/OscDump.cpp)
#TARGET_LINK_LIBRARIES(OscDump oscpack ${LIBS})

#ADD_EXECUTABLE(SimpleReceive examples/SimpleReceive.cpp)
#TARGET_LINK_LIBRARIES(SimpleReceive oscpack ${LIBS})

#ADD_EXECUTABLE(SimpleSend examples/SimpleSend.cpp)
#TARGET_LINK_LIBRARIES(SimpleSend oscpack ${LIBS})


if(MSVC)
  add_definitions("-D_CRT_SECURE_NO_WARNINGS")
  # Force to always compile with W4
  if(CMAKE_CXX_FLAGS MATCHES "/W[0-4]")
    string(REGEX REPLACE "/W[0-4]" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
  endif()
elseif(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  # Update if necessary
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-long-long -pedantic")
endif()

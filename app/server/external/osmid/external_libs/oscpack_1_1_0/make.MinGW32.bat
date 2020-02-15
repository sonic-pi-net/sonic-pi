del bin\OscUnitTests.exe
del bin\OscDump.exe
del bin\OscSendTests.exe
del bin\OscReceiveTest.exe
mkdir bin

g++ tests\OscUnitTests.cpp osc\OscTypes.cpp osc\OscReceivedElements.cpp osc\OscPrintReceivedElements.cpp osc\OscOutboundPacketStream.cpp -Wall -Wextra -I. -lws2_32 -o bin\OscUnitTests.exe

g++ examples\OscDump.cpp osc\OscTypes.cpp osc\OscReceivedElements.cpp osc\OscPrintReceivedElements.cpp ip\win32\NetworkingUtils.cpp ip\win32\UdpSocket.cpp -Wall -Wextra -I. -lws2_32 -lwinmm -o bin\OscDump.exe

g++ examples\SimpleSend.cpp osc\OscTypes.cpp osc\OscOutboundPacketStream.cpp ip\win32\NetworkingUtils.cpp ip\win32\UdpSocket.cpp ip\IpEndpointName.cpp -Wall -Wextra -I. -lws2_32 -lwinmm -o bin\SimpleSend.exe

g++ examples\SimpleReceive.cpp osc\OscTypes.cpp osc\OscReceivedElements.cpp ip\win32\NetworkingUtils.cpp ip\win32\UdpSocket.cpp -Wall -Wextra -I. -lws2_32 -lwinmm -o bin\SimpleReceive.exe

g++ tests\OscSendTests.cpp osc\OscTypes.cpp osc\OscOutboundPacketStream.cpp ip\win32\NetworkingUtils.cpp ip\win32\UdpSocket.cpp ip\IpEndpointName.cpp -Wall -Wextra -I. -lws2_32 -lwinmm -o bin\OscSendTests.exe

g++ tests\OscReceiveTest.cpp osc\OscTypes.cpp osc\OscReceivedElements.cpp ip\win32\NetworkingUtils.cpp ip\win32\UdpSocket.cpp -Wall -Wextra -I. -lws2_32 -lwinmm -o bin\OscReceiveTest.exe

.\bin\OscUnitTests.exe
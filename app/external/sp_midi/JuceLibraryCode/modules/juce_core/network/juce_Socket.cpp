/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2020 - Raw Material Software Limited

   JUCE is an open source library subject to commercial or open-source
   licensing.

   The code included in this file is provided under the terms of the ISC license
   http://www.isc.org/downloads/software-support-policy/isc-license. Permission
   To use, copy, modify, and/or distribute this software for any purpose with or
   without fee is hereby granted provided that the above copyright notice and
   this permission notice appear in all copies.

   JUCE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY, AND ALL WARRANTIES, WHETHER
   EXPRESSED OR IMPLIED, INCLUDING MERCHANTABILITY AND FITNESS FOR PURPOSE, ARE
   DISCLAIMED.

  ==============================================================================
*/

namespace juce
{

JUCE_BEGIN_IGNORE_WARNINGS_MSVC (4127 4389 4018)

#ifndef AI_NUMERICSERV  // (missing in older Mac SDKs)
 #define AI_NUMERICSERV 0x1000
#endif

#if JUCE_WINDOWS
 using juce_socklen_t       = int;
 using juce_recvsend_size_t = int;
 using SocketHandle         = SOCKET;
 static const SocketHandle invalidSocket = INVALID_SOCKET;
#elif JUCE_ANDROID
 using juce_socklen_t       = socklen_t;
 using juce_recvsend_size_t = size_t;
 using SocketHandle         = int;
 static const SocketHandle invalidSocket = -1;
#else
 using juce_socklen_t       = socklen_t;
 using juce_recvsend_size_t = socklen_t;
 using SocketHandle         = int;
 static const SocketHandle invalidSocket = -1;
#endif

//==============================================================================
namespace SocketHelpers
{
    static void initSockets()
    {
       #if JUCE_WINDOWS
        static bool socketsStarted = false;

        if (! socketsStarted)
        {
            socketsStarted = true;

            WSADATA wsaData;
            const WORD wVersionRequested = MAKEWORD (1, 1);
            WSAStartup (wVersionRequested, &wsaData);
        }
       #endif
    }

    inline bool isValidPortNumber (int port) noexcept
    {
        return isPositiveAndBelow (port, 65536);
    }

    template <typename Type>
    static bool setOption (SocketHandle handle, int mode, int property, Type value) noexcept
    {
        return setsockopt (handle, mode, property, reinterpret_cast<const char*> (&value), sizeof (value)) == 0;
    }

    template <typename Type>
    static bool setOption (SocketHandle handle, int property, Type value) noexcept
    {
        return setOption (handle, SOL_SOCKET, property, value);
    }

    static bool resetSocketOptions (SocketHandle handle, bool isDatagram, bool allowBroadcast) noexcept
    {
        return handle != invalidSocket
                && setOption (handle, SO_RCVBUF, (int) 65536)
                && setOption (handle, SO_SNDBUF, (int) 65536)
                && (isDatagram ? ((! allowBroadcast) || setOption (handle, SO_BROADCAST, (int) 1))
                               : setOption (handle, IPPROTO_TCP, TCP_NODELAY, (int) 1));
    }

    static void closeSocket (std::atomic<int>& handle, CriticalSection& readLock,
                             bool isListener, int portNumber, std::atomic<bool>& connected) noexcept
    {
        const auto h = (SocketHandle) handle.load();
        handle = -1;

       #if JUCE_WINDOWS
        ignoreUnused (portNumber, isListener, readLock);

        if (h != invalidSocket || connected)
            closesocket (h);

        // make sure any read process finishes before we delete the socket
        CriticalSection::ScopedLockType lock (readLock);
        connected = false;
       #else
        if (connected)
        {
            connected = false;

            if (isListener)
            {
                // need to do this to interrupt the accept() function..
                StreamingSocket temp;
                temp.connect (IPAddress::local().toString(), portNumber, 1000);
            }
        }

        if (h >= 0)
        {
            // unblock any pending read requests
            ::shutdown (h, SHUT_RDWR);

            {
                // see man-page of recv on linux about a race condition where the
                // shutdown command is lost if the receiving thread does not have
                // a chance to process before close is called. On Mac OS X shutdown
                // does not unblock a select call, so using a lock here will dead-lock
                // both threads.
               #if JUCE_LINUX || JUCE_ANDROID
                CriticalSection::ScopedLockType lock (readLock);
                ::close (h);
               #else
                ::close (h);
                CriticalSection::ScopedLockType lock (readLock);
              #endif
            }
        }
       #endif
    }

    static bool bindSocket (SocketHandle handle, int port, const String& address) noexcept
    {
        if (handle == invalidSocket || ! isValidPortNumber (port))
            return false;

        struct sockaddr_in addr;
        zerostruct (addr); // (can't use "= { 0 }" on this object because it's typedef'ed as a C struct)

        addr.sin_family = PF_INET;
        addr.sin_port = htons ((uint16) port);
        addr.sin_addr.s_addr = address.isNotEmpty() ? ::inet_addr (address.toRawUTF8())
                                                    : htonl (INADDR_ANY);

        return ::bind (handle, (struct sockaddr*) &addr, sizeof (addr)) >= 0;
    }

    static int getBoundPort (SocketHandle handle) noexcept
    {
        if (handle != invalidSocket)
        {
            struct sockaddr_in addr;
            socklen_t len = sizeof (addr);

            if (getsockname (handle, (struct sockaddr*) &addr, &len) == 0)
                return ntohs (addr.sin_port);
        }

        return -1;
    }

    static String getConnectedAddress (SocketHandle handle) noexcept
    {
        struct sockaddr_in addr;
        socklen_t len = sizeof (addr);

        if (getpeername (handle, (struct sockaddr*) &addr, &len) >= 0)
            return inet_ntoa (addr.sin_addr);

        return "0.0.0.0";
    }

    static bool setSocketBlockingState (SocketHandle handle, bool shouldBlock) noexcept
    {
       #if JUCE_WINDOWS
        u_long nonBlocking = shouldBlock ? 0 : (u_long) 1;
        return ioctlsocket (handle, (long) FIONBIO, &nonBlocking) == 0;
       #else
        int socketFlags = fcntl (handle, F_GETFL, 0);

        if (socketFlags == -1)
            return false;

        if (shouldBlock)
            socketFlags &= ~O_NONBLOCK;
        else
            socketFlags |= O_NONBLOCK;

        return fcntl (handle, F_SETFL, socketFlags) == 0;
       #endif
    }

   #if ! JUCE_WINDOWS
    static bool getSocketBlockingState (SocketHandle handle)
    {
        return (fcntl (handle, F_GETFL, 0) & O_NONBLOCK) == 0;
    }
   #endif

    static int readSocket (SocketHandle handle,
                           void* destBuffer, int maxBytesToRead,
                           std::atomic<bool>& connected,
                           bool blockUntilSpecifiedAmountHasArrived,
                           CriticalSection& readLock,
                           String* senderIP = nullptr,
                           int* senderPort = nullptr) noexcept
    {
       #if ! JUCE_WINDOWS
        if (blockUntilSpecifiedAmountHasArrived != getSocketBlockingState (handle))
       #endif
            setSocketBlockingState (handle, blockUntilSpecifiedAmountHasArrived);

        int bytesRead = 0;

        while (bytesRead < maxBytesToRead)
        {
            long bytesThisTime = -1;
            auto buffer = static_cast<char*> (destBuffer) + bytesRead;
            auto numToRead = (juce_recvsend_size_t) (maxBytesToRead - bytesRead);

            {
                // avoid race-condition
                CriticalSection::ScopedTryLockType lock (readLock);

                if (lock.isLocked())
                {
                    if (senderIP == nullptr || senderPort == nullptr)
                    {
                        bytesThisTime = ::recv (handle, buffer, numToRead, 0);
                    }
                    else
                    {
                        sockaddr_in client;
                        socklen_t clientLen = sizeof (sockaddr);

                        bytesThisTime = ::recvfrom (handle, buffer, numToRead, 0, (sockaddr*) &client, &clientLen);

                        *senderIP = String::fromUTF8 (inet_ntoa (client.sin_addr), 16);
                        *senderPort = ntohs (client.sin_port);
                    }
                }
            }

            if (bytesThisTime <= 0 || ! connected)
            {
                if (bytesRead == 0 && blockUntilSpecifiedAmountHasArrived)
                    bytesRead = -1;

                break;
            }

            bytesRead = static_cast<int> (bytesRead + bytesThisTime);

            if (! blockUntilSpecifiedAmountHasArrived)
                break;
        }

        return (int) bytesRead;
    }

    static int waitForReadiness (std::atomic<int>& handle, CriticalSection& readLock,
                                 bool forReading, int timeoutMsecs) noexcept
    {
        // avoid race-condition
        CriticalSection::ScopedTryLockType lock (readLock);

        if (! lock.isLocked())
            return -1;

        auto hasErrorOccurred = [&handle]() -> bool
        {
            auto h = (SocketHandle) handle.load();

            if (h == invalidSocket)
                return true;

            int opt;
            juce_socklen_t len = sizeof (opt);

            if (getsockopt (h, SOL_SOCKET, SO_ERROR, (char*) &opt, &len) < 0  || opt != 0)
                return true;

            return false;
        };

        auto h = handle.load();

       #if JUCE_WINDOWS || JUCE_MINGW
        struct timeval timeout;
        struct timeval* timeoutp;

        if (timeoutMsecs >= 0)
        {
            timeout.tv_sec = timeoutMsecs / 1000;
            timeout.tv_usec = (timeoutMsecs % 1000) * 1000;
            timeoutp = &timeout;
        }
        else
        {
            timeoutp = nullptr;
        }

        fd_set rset, wset;
        FD_ZERO (&rset);
        FD_SET ((SOCKET) h, &rset);
        FD_ZERO (&wset);
        FD_SET ((SOCKET) h, &wset);

        fd_set* prset = forReading ? &rset : nullptr;
        fd_set* pwset = forReading ? nullptr : &wset;

        // NB - need to use select() here as WSAPoll is broken on Windows
        if (select ((int) h + 1, prset, pwset, nullptr, timeoutp) < 0 || hasErrorOccurred())
            return -1;

        return FD_ISSET (h, forReading ? &rset : &wset) ? 1 : 0;
      #else
        short eventsFlag = (forReading ? POLLIN : POLLOUT);
        pollfd pfd { (SocketHandle) h, eventsFlag, 0 };

        int result = 0;

        for (;;)
        {
            result = poll (&pfd, 1, timeoutMsecs);

            if (result >= 0 || errno != EINTR)
                break;
        }

        if (result < 0 || hasErrorOccurred())
            return -1;

        return (pfd.revents & eventsFlag) != 0;
      #endif
    }

    static addrinfo* getAddressInfo (bool isDatagram, const String& hostName, int portNumber)
    {
        struct addrinfo hints;
        zerostruct (hints);

        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = isDatagram ? SOCK_DGRAM : SOCK_STREAM;
        hints.ai_flags = AI_NUMERICSERV;

        struct addrinfo* info = nullptr;

        if (getaddrinfo (hostName.toRawUTF8(), String (portNumber).toRawUTF8(), &hints, &info) == 0)
            return info;

        return nullptr;
    }

    static bool connectSocket (std::atomic<int>& handle,
                               CriticalSection& readLock,
                               const String& hostName,
                               int portNumber,
                               int timeOutMillisecs) noexcept
    {
        bool success = false;

        if (auto* info = getAddressInfo (false, hostName, portNumber))
        {
            for (auto* i = info; i != nullptr; i = i->ai_next)
            {
                auto newHandle = socket (i->ai_family, i->ai_socktype, 0);

                if (newHandle != invalidSocket)
                {
                    setSocketBlockingState (newHandle, false);
                    auto result = ::connect (newHandle, i->ai_addr, (socklen_t) i->ai_addrlen);
                    success = (result >= 0);

                    if (! success)
                    {
                       #if JUCE_WINDOWS
                        if (result == SOCKET_ERROR && WSAGetLastError() == WSAEWOULDBLOCK)
                       #else
                        if (errno == EINPROGRESS)
                       #endif
                        {
                            std::atomic<int> cvHandle { (int) newHandle };

                            if (waitForReadiness (cvHandle, readLock, false, timeOutMillisecs) == 1)
                                success = true;
                        }
                    }

                    if (success)
                    {
                        handle = (int) newHandle;
                        break;
                    }

                   #if JUCE_WINDOWS
                    closesocket (newHandle);
                   #else
                    ::close (newHandle);
                   #endif
                }
            }

            freeaddrinfo (info);

            if (success)
            {
                auto h = (SocketHandle) handle.load();
                setSocketBlockingState (h, true);
                resetSocketOptions (h, false, false);
            }
        }

        return success;
    }

    static void makeReusable (int handle) noexcept
    {
        setOption ((SocketHandle) handle, SO_REUSEADDR, (int) 1);
    }

    static bool multicast (int handle, const String& multicastIPAddress,
                           const String& interfaceIPAddress, bool join) noexcept
    {
        struct ip_mreq mreq;

        zerostruct (mreq);
        mreq.imr_multiaddr.s_addr = inet_addr (multicastIPAddress.toRawUTF8());
        mreq.imr_interface.s_addr = INADDR_ANY;

        if (interfaceIPAddress.isNotEmpty())
            mreq.imr_interface.s_addr = inet_addr (interfaceIPAddress.toRawUTF8());

        return setsockopt ((SocketHandle) handle, IPPROTO_IP,
                           join ? IP_ADD_MEMBERSHIP
                                : IP_DROP_MEMBERSHIP,
                           (const char*) &mreq, sizeof (mreq)) == 0;
    }
}

//==============================================================================
StreamingSocket::StreamingSocket()
{
    SocketHelpers::initSockets();
}

StreamingSocket::StreamingSocket (const String& host, int portNum, int h)
    : hostName (host),
      portNumber (portNum),
      handle (h),
      connected (true)
{
    jassert (SocketHelpers::isValidPortNumber (portNum));

    SocketHelpers::initSockets();
    SocketHelpers::resetSocketOptions ((SocketHandle) h, false, false);
}

StreamingSocket::~StreamingSocket()
{
    close();
}

//==============================================================================
int StreamingSocket::read (void* destBuffer, int maxBytesToRead, bool shouldBlock)
{
    return (connected && ! isListener) ? SocketHelpers::readSocket ((SocketHandle) handle.load(), destBuffer,maxBytesToRead,
                                                                    connected, shouldBlock, readLock)
                                       : -1;
}

int StreamingSocket::write (const void* sourceBuffer, int numBytesToWrite)
{
    if (isListener || ! connected)
        return -1;

    return (int) ::send ((SocketHandle) handle.load(), (const char*) sourceBuffer, (juce_recvsend_size_t) numBytesToWrite, 0);
}

//==============================================================================
int StreamingSocket::waitUntilReady (bool readyForReading, int timeoutMsecs)
{
    return connected ? SocketHelpers::waitForReadiness (handle, readLock, readyForReading, timeoutMsecs)
                     : -1;
}

//==============================================================================
bool StreamingSocket::bindToPort (int port)
{
    return bindToPort (port, String());
}

bool StreamingSocket::bindToPort (int port, const String& addr)
{
    jassert (SocketHelpers::isValidPortNumber (port));

    return SocketHelpers::bindSocket ((SocketHandle) handle.load(), port, addr);
}

int StreamingSocket::getBoundPort() const noexcept
{
    return SocketHelpers::getBoundPort ((SocketHandle) handle.load());
}

bool StreamingSocket::connect (const String& remoteHostName, int remotePortNumber, int timeOutMillisecs)
{
    jassert (SocketHelpers::isValidPortNumber (remotePortNumber));

    if (isListener)
    {
        // a listener socket can't connect to another one!
        jassertfalse;
        return false;
    }

    if (connected)
        close();

    hostName = remoteHostName;
    portNumber = remotePortNumber;
    isListener = false;

    connected = SocketHelpers::connectSocket (handle, readLock, remoteHostName,
                                              remotePortNumber, timeOutMillisecs);

    if (! connected)
        return false;

    if (! SocketHelpers::resetSocketOptions ((SocketHandle) handle.load(), false, false))
    {
        close();
        return false;
    }

    return true;
}

void StreamingSocket::close()
{
    if (handle >= 0)
        SocketHelpers::closeSocket (handle, readLock, isListener, portNumber, connected);

    hostName.clear();
    portNumber = 0;
    handle = -1;
    isListener = false;
}

//==============================================================================
bool StreamingSocket::createListener (int newPortNumber, const String& localHostName)
{
    jassert (SocketHelpers::isValidPortNumber (newPortNumber));

    if (connected)
        close();

    hostName = "listener";
    portNumber = newPortNumber;
    isListener = true;

    handle = (int) socket (AF_INET, SOCK_STREAM, 0);

    if (handle < 0)
        return false;

   #if ! JUCE_WINDOWS // on windows, adding this option produces behaviour different to posix
    SocketHelpers::makeReusable (handle);
   #endif

    if (SocketHelpers::bindSocket ((SocketHandle) handle.load(), portNumber, localHostName)
         && listen ((SocketHandle) handle.load(), SOMAXCONN) >= 0)
    {
        connected = true;
        return true;
    }

    close();
    return false;
}

StreamingSocket* StreamingSocket::waitForNextConnection() const
{
    // To call this method, you first have to use createListener() to
    // prepare this socket as a listener.
    jassert (isListener || ! connected);

    if (connected && isListener)
    {
        struct sockaddr_storage address;
        juce_socklen_t len = sizeof (address);
        auto newSocket = (int) accept ((SocketHandle) handle.load(), (struct sockaddr*) &address, &len);

        if (newSocket >= 0 && connected)
            return new StreamingSocket (inet_ntoa (((struct sockaddr_in*) &address)->sin_addr),
                                        portNumber, newSocket);
    }

    return nullptr;
}

bool StreamingSocket::isLocal() const noexcept
{
    if (! isConnected())
        return false;

    IPAddress currentIP (SocketHelpers::getConnectedAddress ((SocketHandle) handle.load()));

    for (auto& a : IPAddress::getAllAddresses())
        if (a == currentIP)
            return true;

    return hostName == "127.0.0.1";
}


//==============================================================================
//==============================================================================
DatagramSocket::DatagramSocket (bool canBroadcast)
{
    SocketHelpers::initSockets();

    handle = (int) socket (AF_INET, SOCK_DGRAM, 0);

    if (handle >= 0)
    {
        SocketHelpers::resetSocketOptions ((SocketHandle) handle.load(), true, canBroadcast);
        SocketHelpers::makeReusable (handle);
    }
}

DatagramSocket::~DatagramSocket()
{
    if (lastServerAddress != nullptr)
        freeaddrinfo (static_cast<struct addrinfo*> (lastServerAddress));

    shutdown();
}

void DatagramSocket::shutdown()
{
    if (handle < 0)
        return;

    std::atomic<int> handleCopy { handle.load() };
    handle = -1;

    std::atomic<bool> connected { false };
    SocketHelpers::closeSocket (handleCopy, readLock, false, 0, connected);

    isBound = false;
}

bool DatagramSocket::bindToPort (int port)
{
    return bindToPort (port, String());
}

bool DatagramSocket::bindToPort (int port, const String& addr)
{
    jassert (SocketHelpers::isValidPortNumber (port));

    if (handle < 0)
        return false;

    if (SocketHelpers::bindSocket ((SocketHandle) handle.load(), port, addr))
    {
        isBound = true;
        lastBindAddress = addr;
        return true;
    }

    return false;
}

int DatagramSocket::getBoundPort() const noexcept
{
    return (handle >= 0 && isBound) ? SocketHelpers::getBoundPort ((SocketHandle) handle.load()) : -1;
}

//==============================================================================
int DatagramSocket::waitUntilReady (bool readyForReading, int timeoutMsecs)
{
    if (handle < 0)
        return -1;

    return SocketHelpers::waitForReadiness (handle, readLock, readyForReading, timeoutMsecs);
}

int DatagramSocket::read (void* destBuffer, int maxBytesToRead, bool shouldBlock)
{
    if (handle < 0 || ! isBound)
        return -1;

    std::atomic<bool> connected { true };
    return SocketHelpers::readSocket ((SocketHandle) handle.load(), destBuffer, maxBytesToRead,
                                      connected, shouldBlock, readLock);
}

int DatagramSocket::read (void* destBuffer, int maxBytesToRead, bool shouldBlock, String& senderIPAddress, int& senderPort)
{
    if (handle < 0 || ! isBound)
        return -1;

    std::atomic<bool> connected { true };
    return SocketHelpers::readSocket ((SocketHandle) handle.load(), destBuffer, maxBytesToRead, connected,
                                      shouldBlock, readLock, &senderIPAddress, &senderPort);
}

int DatagramSocket::write (const String& remoteHostname, int remotePortNumber,
                           const void* sourceBuffer, int numBytesToWrite)
{
    jassert (SocketHelpers::isValidPortNumber (remotePortNumber));

    if (handle < 0)
        return -1;

    struct addrinfo*& info = reinterpret_cast<struct addrinfo*&> (lastServerAddress);

    // getaddrinfo can be quite slow so cache the result of the address lookup
    if (info == nullptr || remoteHostname != lastServerHost || remotePortNumber != lastServerPort)
    {
        if (info != nullptr)
            freeaddrinfo (info);

        if ((info = SocketHelpers::getAddressInfo (true, remoteHostname, remotePortNumber)) == nullptr)
            return -1;

        lastServerHost = remoteHostname;
        lastServerPort = remotePortNumber;
    }

    return (int) ::sendto ((SocketHandle) handle.load(), (const char*) sourceBuffer,
                           (juce_recvsend_size_t) numBytesToWrite, 0,
                           info->ai_addr, (socklen_t) info->ai_addrlen);
}

bool DatagramSocket::joinMulticast (const String& multicastIPAddress)
{
    if (handle < 0 || ! isBound)
        return false;

    return SocketHelpers::multicast (handle, multicastIPAddress, lastBindAddress, true);
}

bool DatagramSocket::leaveMulticast (const String& multicastIPAddress)
{
    if (handle < 0 || ! isBound)
        return false;

    return SocketHelpers::multicast (handle, multicastIPAddress, lastBindAddress, false);
}

bool DatagramSocket::setMulticastLoopbackEnabled (bool enable)
{
    if (handle < 0 || ! isBound)
        return false;

    return SocketHelpers::setOption<bool> ((SocketHandle) handle.load(), IPPROTO_IP, IP_MULTICAST_LOOP, enable);
}

bool DatagramSocket::setEnablePortReuse (bool enabled)
{
   #if JUCE_ANDROID
    ignoreUnused (enabled);
   #else
    if (handle >= 0)
        return SocketHelpers::setOption ((SocketHandle) handle.load(),
                                        #if JUCE_WINDOWS || JUCE_LINUX
                                         SO_REUSEADDR,  // port re-use is implied by addr re-use on these platforms
                                        #else
                                         SO_REUSEPORT,
                                        #endif
                                         (int) (enabled ? 1 : 0));
   #endif

    return false;
}

JUCE_END_IGNORE_WARNINGS_MSVC

//==============================================================================
//==============================================================================
#if JUCE_UNIT_TESTS

struct SocketTests : public UnitTest
{
    SocketTests()
        : UnitTest ("Sockets", UnitTestCategories::networking)
    {
    }

    void runTest() override
    {
        auto localHost = IPAddress::local();
        int portNum = 12345;

        beginTest ("StreamingSocket");
        {
            StreamingSocket socketServer;

            expect (socketServer.isConnected() == false);
            expect (socketServer.getHostName().isEmpty());
            expect (socketServer.getBoundPort() == -1);
            expect (static_cast<SocketHandle> (socketServer.getRawSocketHandle()) == invalidSocket);

            expect (socketServer.createListener (portNum, localHost.toString()));

            StreamingSocket socket;

            expect (socket.connect (localHost.toString(), portNum));

            expect (socket.isConnected() == true);
            expect (socket.getHostName() == localHost.toString());
            expect (socket.getBoundPort() != -1);
            expect (static_cast<SocketHandle> (socket.getRawSocketHandle()) != invalidSocket);

            socket.close();

            expect (socket.isConnected() == false);
            expect (socket.getHostName().isEmpty());
            expect (socket.getBoundPort() == -1);
            expect (static_cast<SocketHandle> (socket.getRawSocketHandle()) == invalidSocket);
        }

        beginTest ("DatagramSocket");
        {
            DatagramSocket socket;

            expect (socket.getBoundPort() == -1);
            expect (static_cast<SocketHandle> (socket.getRawSocketHandle()) != invalidSocket);

            expect (socket.bindToPort (portNum, localHost.toString()));

            expect (socket.getBoundPort() == portNum);
            expect (static_cast<SocketHandle> (socket.getRawSocketHandle()) != invalidSocket);

            socket.shutdown();

            expect (socket.getBoundPort() == -1);
            expect (static_cast<SocketHandle> (socket.getRawSocketHandle()) == invalidSocket);
        }
    }
};

static SocketTests socketTests;

#endif

} // namespace juce

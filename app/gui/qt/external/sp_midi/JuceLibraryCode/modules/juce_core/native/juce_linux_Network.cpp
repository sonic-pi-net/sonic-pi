/*
  ==============================================================================

   This file is part of the JUCE library.
   Copyright (c) 2017 - ROLI Ltd.

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

void MACAddress::findAllAddresses (Array<MACAddress>& result)
{
    auto s = socket (AF_INET, SOCK_DGRAM, 0);

    if (s != -1)
    {
        struct ifaddrs* addrs = nullptr;

        if (getifaddrs (&addrs) != -1)
        {
            for (auto* i = addrs; i != nullptr; i = i->ifa_next)
            {
                struct ifreq ifr;
                strcpy (ifr.ifr_name, i->ifa_name);
                ifr.ifr_addr.sa_family = AF_INET;

                if (ioctl (s, SIOCGIFHWADDR, &ifr) == 0)
                {
                    MACAddress ma ((const uint8*) ifr.ifr_hwaddr.sa_data);

                    if (! ma.isNull())
                        result.addIfNotAlreadyThere (ma);
                }
            }

            freeifaddrs (addrs);
        }

        ::close (s);
    }
}


bool JUCE_CALLTYPE Process::openEmailWithAttachments (const String& /* targetEmailAddress */,
                                                      const String& /* emailSubject */,
                                                      const String& /* bodyText */,
                                                      const StringArray& /* filesToAttach */)
{
    jassertfalse;    // xxx todo
    return false;
}

//==============================================================================
#if ! JUCE_USE_CURL
class WebInputStream::Pimpl
{
public:
    Pimpl (WebInputStream& pimplOwner, const URL& urlToCopy, bool shouldUsePost)
        : owner (pimplOwner), url (urlToCopy),
          isPost (shouldUsePost), httpRequestCmd (shouldUsePost ? "POST" : "GET")
    {}

    ~Pimpl()
    {
        closeSocket();
    }

    //==============================================================================
    // WebInputStream methods
    void withExtraHeaders (const String& extraHeaders)
    {
        if (! headers.endsWithChar ('\n') && headers.isNotEmpty())
            headers << "\r\n";

        headers << extraHeaders;

        if (! headers.endsWithChar ('\n') && headers.isNotEmpty())
            headers << "\r\n";
    }

    void withCustomRequestCommand (const String& customRequestCommand)    { httpRequestCmd = customRequestCommand; }
    void withConnectionTimeout (int timeoutInMs)                          { timeOutMs = timeoutInMs; }
    void withNumRedirectsToFollow (int maxRedirectsToFollow)              { numRedirectsToFollow = maxRedirectsToFollow; }
    int getStatusCode() const                                             { return statusCode; }
    StringPairArray getRequestHeaders() const                             { return WebInputStream::parseHttpHeaders (headers); }

    StringPairArray getResponseHeaders() const
    {
        StringPairArray responseHeaders;

        if (! isError())
        {
            for (int i = 0; i < headerLines.size(); ++i)
            {
                auto& headersEntry = headerLines[i];
                auto key   = headersEntry.upToFirstOccurrenceOf (": ", false, false);
                auto value = headersEntry.fromFirstOccurrenceOf (": ", false, false);
                auto previousValue = responseHeaders[key];
                responseHeaders.set (key, previousValue.isEmpty() ? value : (previousValue + "," + value));
            }
        }

        return responseHeaders;
    }

    bool connect (WebInputStream::Listener* listener)
    {
        {
            const ScopedLock lock (createSocketLock);

            if (hasBeenCancelled)
                return false;
        }

        address = url.toString (! isPost);
        statusCode = createConnection (listener, numRedirectsToFollow);

        return statusCode != 0;
    }

    void cancel()
    {
        const ScopedLock lock (createSocketLock);

        hasBeenCancelled = true;
        statusCode = -1;
        finished = true;

        closeSocket();
    }

    //==============================================================================
    bool isError() const                 { return socketHandle < 0; }
    bool isExhausted()                   { return finished; }
    int64 getPosition()                  { return position; }
    int64 getTotalLength()               { return contentLength; }

    int read (void* buffer, int bytesToRead)
    {
        if (finished || isError())
            return 0;

        if (isChunked && ! readingChunk)
        {
            if (position >= chunkEnd)
            {
                const ScopedValueSetter<bool> setter (readingChunk, true, false);
                MemoryOutputStream chunkLengthBuffer;
                char c = 0;

                if (chunkEnd > 0)
                {
                    if (read (&c, 1) != 1 || c != '\r'
                         || read (&c, 1) != 1 || c != '\n')
                    {
                        finished = true;
                        return 0;
                    }
                }

                while (chunkLengthBuffer.getDataSize() < 512 && ! (finished || isError()))
                {
                    if (read (&c, 1) != 1)
                    {
                        finished = true;
                        return 0;
                    }

                    if (c == '\r')
                        continue;

                    if (c == '\n')
                        break;

                    chunkLengthBuffer.writeByte (c);
                }

                auto chunkSize = chunkLengthBuffer.toString().trimStart().getHexValue64();

                if (chunkSize == 0)
                {
                    finished = true;
                    return 0;
                }

                chunkEnd += chunkSize;
            }

            if (bytesToRead > chunkEnd - position)
                bytesToRead = static_cast<int> (chunkEnd - position);
        }

        pollfd pfd { socketHandle, POLLIN, 0 };

        if (poll (&pfd, 1, timeOutMs) <= 0)
            return 0; // (timeout)

        auto bytesRead = jmax (0, (int) recv (socketHandle, buffer, (size_t) bytesToRead, MSG_WAITALL));

        if (bytesRead == 0)
            finished = true;

        if (! readingChunk)
            position += bytesRead;

        return bytesRead;
    }

    bool setPosition (int64 wantedPos)
    {
        if (isError())
            return false;

        if (wantedPos != position)
        {
            finished = false;

            if (wantedPos < position)
                return false;

            auto numBytesToSkip = wantedPos - position;
            auto skipBufferSize = (int) jmin (numBytesToSkip, (int64) 16384);
            HeapBlock<char> temp (skipBufferSize);

            while (numBytesToSkip > 0 && ! isExhausted())
                numBytesToSkip -= read (temp, (int) jmin (numBytesToSkip, (int64) skipBufferSize));
        }

        return true;
    }

    //==============================================================================
    int statusCode = 0;

private:
    WebInputStream& owner;
    URL url;
    int socketHandle = -1, levelsOfRedirection = 0;
    StringArray headerLines;
    String address, headers;
    MemoryBlock postData;
    int64 contentLength = -1, position = 0;
    bool finished = false;
    const bool isPost;
    int timeOutMs = 0;
    int numRedirectsToFollow = 5;
    String httpRequestCmd;
    int64 chunkEnd = 0;
    bool isChunked = false, readingChunk = false;
    CriticalSection closeSocketLock, createSocketLock;
    bool hasBeenCancelled = false;

    void closeSocket (bool resetLevelsOfRedirection = true)
    {
        const ScopedLock lock (closeSocketLock);

        if (socketHandle >= 0)
        {
            ::shutdown (socketHandle, SHUT_RDWR);
            ::close (socketHandle);
        }

        socketHandle = -1;

        if (resetLevelsOfRedirection)
            levelsOfRedirection = 0;
    }

    int createConnection (WebInputStream::Listener* listener, int numRedirects)
    {
        closeSocket (false);

        if (isPost)
            WebInputStream::createHeadersAndPostData (url, headers, postData);

        auto timeOutTime = Time::getMillisecondCounter();

        if (timeOutMs == 0)
            timeOutMs = 30000;

        if (timeOutMs < 0)
            timeOutTime = 0xffffffff;
        else
            timeOutTime += (uint32) timeOutMs;

        String hostName, hostPath;
        int hostPort;

        if (! decomposeURL (address, hostName, hostPath, hostPort))
            return 0;

        String serverName, proxyName, proxyPath;
        int proxyPort = 0;
        int port = 0;

        auto proxyURL = String::fromUTF8 (getenv ("http_proxy"));

        if (proxyURL.startsWithIgnoreCase ("http://"))
        {
            if (! decomposeURL (proxyURL, proxyName, proxyPath, proxyPort))
                return 0;

            serverName = proxyName;
            port = proxyPort;
        }
        else
        {
            serverName = hostName;
            port = hostPort;
        }

        struct addrinfo hints;
        zerostruct (hints);

        hints.ai_family = AF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags = AI_NUMERICSERV;

        struct addrinfo* result = nullptr;

        if (getaddrinfo (serverName.toUTF8(), String (port).toUTF8(), &hints, &result) != 0 || result == 0)
            return 0;

        {
            const ScopedLock lock (createSocketLock);

            socketHandle = hasBeenCancelled ? -1
                                            : socket (result->ai_family, result->ai_socktype, 0);
        }

        if (socketHandle == -1)
        {
            freeaddrinfo (result);
            return 0;
        }

        int receiveBufferSize = 16384;
        setsockopt (socketHandle, SOL_SOCKET, SO_RCVBUF, (char*) &receiveBufferSize, sizeof (receiveBufferSize));
        setsockopt (socketHandle, SOL_SOCKET, SO_KEEPALIVE, 0, 0);

      #if JUCE_MAC
        setsockopt (socketHandle, SOL_SOCKET, SO_NOSIGPIPE, 0, 0);
      #endif

        if (::connect (socketHandle, result->ai_addr, result->ai_addrlen) == -1)
        {
            closeSocket();
            freeaddrinfo (result);
            return 0;
        }

        freeaddrinfo (result);

        {
            const MemoryBlock requestHeader (createRequestHeader (hostName, hostPort, proxyName, proxyPort, hostPath,
                                                                  address, headers, postData, isPost, httpRequestCmd));

            if (! sendHeader (socketHandle, requestHeader, timeOutTime, owner, listener))
            {
                closeSocket();
                return 0;
            }
        }

        auto responseHeader = readResponse (timeOutTime);
        position = 0;

        if (responseHeader.isNotEmpty())
        {
            headerLines = StringArray::fromLines (responseHeader);

            auto status = responseHeader.fromFirstOccurrenceOf (" ", false, false)
                                        .substring (0, 3).getIntValue();

            auto location = findHeaderItem (headerLines, "Location:");

            if (++levelsOfRedirection <= numRedirects
                 && status >= 300 && status < 400
                 && location.isNotEmpty() && location != address)
            {
                if (! (location.startsWithIgnoreCase ("http://")
                        || location.startsWithIgnoreCase ("https://")
                        || location.startsWithIgnoreCase ("ftp://")))
                {
                    // The following is a bit dodgy. Ideally, we should do a proper transform of the relative URI to a target URI
                    if (location.startsWithChar ('/'))
                        location = URL (address).withNewSubPath (location).toString (true);
                    else
                        location = address + "/" + location;
                }

                address = location;
                return createConnection (listener, numRedirects);
            }

            auto contentLengthString = findHeaderItem (headerLines, "Content-Length:");

            if (contentLengthString.isNotEmpty())
                contentLength = contentLengthString.getLargeIntValue();

            isChunked = (findHeaderItem (headerLines, "Transfer-Encoding:") == "chunked");

            return status;
        }

        closeSocket();
        return 0;
    }

    //==============================================================================
    String readResponse (uint32 timeOutTime)
    {
        int numConsecutiveLFs  = 0;
        MemoryOutputStream buffer;

        while (numConsecutiveLFs < 2
                && buffer.getDataSize() < 32768
                && Time::getMillisecondCounter() <= timeOutTime
                && ! (finished || isError()))
        {
            char c = 0;

            if (read (&c, 1) != 1)
                return {};

            buffer.writeByte (c);

            if (c == '\n')
                ++numConsecutiveLFs;
            else if (c != '\r')
                numConsecutiveLFs = 0;
        }

        auto header = buffer.toString().trimEnd();

        if (header.startsWithIgnoreCase ("HTTP/"))
            return header;

        return {};
    }

    static void writeValueIfNotPresent (MemoryOutputStream& dest, const String& headers, const String& key, const String& value)
    {
        if (! headers.containsIgnoreCase (key))
            dest << "\r\n" << key << ' ' << value;
    }

    static void writeHost (MemoryOutputStream& dest, const String& httpRequestCmd,
                           const String& path, const String& host, int port)
    {
        dest << httpRequestCmd << ' ' << path << " HTTP/1.1\r\nHost: " << host;

        /* HTTP spec 14.23 says that the port number must be included in the header if it is not 80 */
        if (port != 80)
            dest << ':' << port;
    }

    static MemoryBlock createRequestHeader (const String& hostName, int hostPort,
                                            const String& proxyName, int proxyPort,
                                            const String& hostPath, const String& originalURL,
                                            const String& userHeaders, const MemoryBlock& postData,
                                            bool isPost, const String& httpRequestCmd)
    {
        MemoryOutputStream header;

        if (proxyName.isEmpty())
            writeHost (header, httpRequestCmd, hostPath, hostName, hostPort);
        else
            writeHost (header, httpRequestCmd, originalURL, proxyName, proxyPort);

        writeValueIfNotPresent (header, userHeaders, "User-Agent:", "JUCE/" JUCE_STRINGIFY(JUCE_MAJOR_VERSION)
                                                                        "." JUCE_STRINGIFY(JUCE_MINOR_VERSION)
                                                                        "." JUCE_STRINGIFY(JUCE_BUILDNUMBER));
        writeValueIfNotPresent (header, userHeaders, "Connection:", "close");

        if (isPost)
            writeValueIfNotPresent (header, userHeaders, "Content-Length:", String ((int) postData.getSize()));

        if (userHeaders.isNotEmpty())
            header << "\r\n" << userHeaders;

        header << "\r\n\r\n";

        if (isPost)
            header << postData;

        return header.getMemoryBlock();
    }

    static bool sendHeader (int socketHandle, const MemoryBlock& requestHeader, uint32 timeOutTime,
                            WebInputStream& pimplOwner, WebInputStream::Listener* listener)
    {
        size_t totalHeaderSent = 0;

        while (totalHeaderSent < requestHeader.getSize())
        {
            if (Time::getMillisecondCounter() > timeOutTime)
                return false;

            auto numToSend = jmin (1024, (int) (requestHeader.getSize() - totalHeaderSent));

            if (send (socketHandle, static_cast<const char*> (requestHeader.getData()) + totalHeaderSent, (size_t) numToSend, 0) != numToSend)
                return false;

            totalHeaderSent += (size_t) numToSend;

            if (listener != nullptr && ! listener->postDataSendProgress (pimplOwner, (int) totalHeaderSent, (int) requestHeader.getSize()))
                return false;
        }

        return true;
    }

    static bool decomposeURL (const String& url, String& host, String& path, int& port)
    {
        if (! url.startsWithIgnoreCase ("http://"))
            return false;

        auto nextSlash = url.indexOfChar (7, '/');
        auto nextColon = url.indexOfChar (7, ':');

        if (nextColon > nextSlash && nextSlash > 0)
            nextColon = -1;

        if (nextColon >= 0)
        {
            host = url.substring (7, nextColon);

            if (nextSlash >= 0)
                port = url.substring (nextColon + 1, nextSlash).getIntValue();
            else
                port = url.substring (nextColon + 1).getIntValue();
        }
        else
        {
            port = 80;

            if (nextSlash >= 0)
                host = url.substring (7, nextSlash);
            else
                host = url.substring (7);
        }

        if (nextSlash >= 0)
            path = url.substring (nextSlash);
        else
            path = "/";

        return true;
    }

    static String findHeaderItem (const StringArray& lines, const String& itemName)
    {
        for (int i = 0; i < lines.size(); ++i)
            if (lines[i].startsWithIgnoreCase (itemName))
                return lines[i].substring (itemName.length()).trim();

        return {};
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Pimpl)
};

URL::DownloadTask* URL::downloadToFile (const File& targetLocation, String extraHeaders, DownloadTask::Listener* listener, bool shouldUsePost)
{
    return URL::DownloadTask::createFallbackDownloader (*this, targetLocation, extraHeaders, listener, shouldUsePost);
}
#endif

} // namespace juce

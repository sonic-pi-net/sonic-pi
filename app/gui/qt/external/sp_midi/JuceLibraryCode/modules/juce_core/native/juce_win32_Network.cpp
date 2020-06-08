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

#ifndef INTERNET_FLAG_NEED_FILE
 #define INTERNET_FLAG_NEED_FILE 0x00000010
#endif

#ifndef INTERNET_OPTION_DISABLE_AUTODIAL
 #define INTERNET_OPTION_DISABLE_AUTODIAL 70
#endif

//==============================================================================
class WebInputStream::Pimpl
{
public:
    Pimpl (WebInputStream& pimplOwner, const URL& urlToCopy, bool shouldBePost)
        : statusCode (0), owner (pimplOwner), url (urlToCopy), isPost (shouldBePost),
          httpRequestCmd (isPost ? "POST" : "GET")
    {}

    ~Pimpl()
    {
        closeConnection();
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
    StringPairArray getRequestHeaders() const                             { return WebInputStream::parseHttpHeaders (headers); }
    StringPairArray getResponseHeaders() const                            { return responseHeaders; }
    int getStatusCode() const                                             { return statusCode; }

    //==============================================================================
    bool connect (WebInputStream::Listener* listener)
    {
        {
            const ScopedLock lock (createConnectionLock);

            if (hasBeenCancelled)
                return false;
        }

        String address = url.toString (! isPost);

        while (numRedirectsToFollow-- >= 0)
        {
            createConnection (address, listener);

            if (! isError())
            {
                DWORD bufferSizeBytes = 4096;
                StringPairArray dataHeaders;

                for (;;)
                {
                    HeapBlock<char> buffer (bufferSizeBytes);

                    if (HttpQueryInfo (request, HTTP_QUERY_RAW_HEADERS_CRLF, buffer.getData(), &bufferSizeBytes, 0))
                    {
                        StringArray headersArray;
                        headersArray.addLines (String (reinterpret_cast<const WCHAR*> (buffer.getData())));

                        for (int i = 0; i < headersArray.size(); ++i)
                        {
                            const String& header = headersArray[i];
                            const String key   (header.upToFirstOccurrenceOf (": ", false, false));
                            const String value (header.fromFirstOccurrenceOf (": ", false, false));
                            const String previousValue (dataHeaders[key]);
                            dataHeaders.set (key, previousValue.isEmpty() ? value : (previousValue + "," + value));
                        }

                        break;
                    }

                    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
                        return false;

                    bufferSizeBytes += 4096;
                }

                DWORD status = 0;
                DWORD statusSize = sizeof (status);

                if (HttpQueryInfo (request, HTTP_QUERY_STATUS_CODE | HTTP_QUERY_FLAG_NUMBER, &status, &statusSize, 0))
                {
                    statusCode = (int) status;

                    if (numRedirectsToFollow >= 0
                         && (statusCode == 301 || statusCode == 302 || statusCode == 303 || statusCode == 307))
                    {
                        String newLocation (dataHeaders["Location"]);

                        // Check whether location is a relative URI - this is an incomplete test for relative path,
                        // but we'll use it for now (valid protocols for this implementation are http, https & ftp)
                        if (! (newLocation.startsWithIgnoreCase ("http://")
                                || newLocation.startsWithIgnoreCase ("https://")
                                || newLocation.startsWithIgnoreCase ("ftp://")))
                        {
                            if (newLocation.startsWithChar ('/'))
                                newLocation = URL (address).withNewSubPath (newLocation).toString (true);
                            else
                                newLocation = address + "/" + newLocation;
                        }

                        if (newLocation.isNotEmpty() && newLocation != address)
                        {
                            address = newLocation;
                            continue;
                        }
                    }
                }

                responseHeaders.addArray (dataHeaders);
            }

            break;
        }

        return (request != 0);
    }

    bool isError() const        { return request == 0; }
    bool isExhausted()          { return finished; }
    int64 getPosition()         { return position; }

    int64 getTotalLength()
    {
        if (! isError())
        {
            DWORD index = 0, result = 0, size = sizeof (result);

            if (HttpQueryInfo (request, HTTP_QUERY_CONTENT_LENGTH | HTTP_QUERY_FLAG_NUMBER, &result, &size, &index))
                return (int64) result;
        }

        return -1;
    }

    int read (void* buffer, int bytesToRead)
    {
        jassert (buffer != nullptr && bytesToRead >= 0);
        DWORD bytesRead = 0;

        if (! (finished || isError()))
        {
            InternetReadFile (request, buffer, (DWORD) bytesToRead, &bytesRead);
            position += bytesRead;

            if (bytesRead == 0)
                finished = true;
        }

        return (int) bytesRead;
    }

    void cancel()
    {
        {
            const ScopedLock lock (createConnectionLock);

            hasBeenCancelled = true;

            closeConnection();
        }
    }

    bool setPosition (int64 wantedPos)
    {
        if (isError())
            return false;

        if (wantedPos != position)
        {
            finished = false;
            position = (int64) InternetSetFilePointer (request, (LONG) wantedPos, 0, FILE_BEGIN, 0);

            if (position == wantedPos)
                return true;

            if (wantedPos < position)
                return false;

            int64 numBytesToSkip = wantedPos - position;
            auto skipBufferSize = (int) jmin (numBytesToSkip, (int64) 16384);
            HeapBlock<char> temp (skipBufferSize);

            while (numBytesToSkip > 0 && ! isExhausted())
                numBytesToSkip -= read (temp, (int) jmin (numBytesToSkip, (int64) skipBufferSize));
        }

        return true;
    }

    int statusCode;

private:
    //==============================================================================
    WebInputStream& owner;
    const URL url;
    HINTERNET connection = 0, request = 0;
    String headers;
    MemoryBlock postData;
    int64 position = 0;
    bool finished = false;
    const bool isPost;
    int timeOutMs = 0;
    String httpRequestCmd;
    int numRedirectsToFollow = 5;
    StringPairArray responseHeaders;
    CriticalSection createConnectionLock;
    bool hasBeenCancelled = false;

    void closeConnection()
    {
        HINTERNET requestCopy = request;

        request = 0;

        if (requestCopy != 0)
            InternetCloseHandle (requestCopy);

        if (connection != 0)
        {
            InternetCloseHandle (connection);
            connection = 0;
        }
    }

    void createConnection (const String& address, WebInputStream::Listener* listener)
    {
        static HINTERNET sessionHandle = InternetOpen (_T("juce"), INTERNET_OPEN_TYPE_PRECONFIG, 0, 0, 0);

        closeConnection();

        if (sessionHandle != 0)
        {
            // break up the url..
            const int fileNumChars = 65536;
            const int serverNumChars = 2048;
            const int usernameNumChars = 1024;
            const int passwordNumChars = 1024;
            HeapBlock<TCHAR> file (fileNumChars), server (serverNumChars),
                             username (usernameNumChars), password (passwordNumChars);

            URL_COMPONENTS uc = { 0 };
            uc.dwStructSize = sizeof (uc);
            uc.lpszUrlPath = file;
            uc.dwUrlPathLength = fileNumChars;
            uc.lpszHostName = server;
            uc.dwHostNameLength = serverNumChars;
            uc.lpszUserName = username;
            uc.dwUserNameLength = usernameNumChars;
            uc.lpszPassword = password;
            uc.dwPasswordLength = passwordNumChars;

            if (isPost)
                WebInputStream::createHeadersAndPostData (url, headers, postData);

            if (InternetCrackUrl (address.toWideCharPointer(), 0, 0, &uc))
                openConnection (uc, sessionHandle, address, listener);
        }
    }

    void openConnection (URL_COMPONENTS& uc, HINTERNET sessionHandle,
                         const String& address,
                         WebInputStream::Listener* listener)
    {
        int disable = 1;
        InternetSetOption (sessionHandle, INTERNET_OPTION_DISABLE_AUTODIAL, &disable, sizeof (disable));

        if (timeOutMs == 0)
            timeOutMs = 30000;
        else if (timeOutMs < 0)
            timeOutMs = -1;

        applyTimeout (sessionHandle, INTERNET_OPTION_CONNECT_TIMEOUT);
        applyTimeout (sessionHandle, INTERNET_OPTION_RECEIVE_TIMEOUT);
        applyTimeout (sessionHandle, INTERNET_OPTION_SEND_TIMEOUT);
        applyTimeout (sessionHandle, INTERNET_OPTION_DATA_RECEIVE_TIMEOUT);
        applyTimeout (sessionHandle, INTERNET_OPTION_DATA_SEND_TIMEOUT);

        const bool isFtp = address.startsWithIgnoreCase ("ftp:");

        {
            const ScopedLock lock (createConnectionLock);

            connection = hasBeenCancelled ? 0
                                          : InternetConnect (sessionHandle,
                                                             uc.lpszHostName, uc.nPort,
                                                             uc.lpszUserName, uc.lpszPassword,
                                                             isFtp ? (DWORD) INTERNET_SERVICE_FTP
                                                                   : (DWORD) INTERNET_SERVICE_HTTP,
                                                             0, 0);
        }

        if (connection != 0)
        {
            if (isFtp)
                request = FtpOpenFile (connection, uc.lpszUrlPath, GENERIC_READ,
                                       FTP_TRANSFER_TYPE_BINARY | INTERNET_FLAG_NEED_FILE, 0);
            else
                openHTTPConnection (uc, address, listener);
        }
    }

    void applyTimeout (HINTERNET sessionHandle, const DWORD option)
    {
        InternetSetOption (sessionHandle, option, &timeOutMs, sizeof (timeOutMs));
    }

    void sendHTTPRequest (INTERNET_BUFFERS& buffers, WebInputStream::Listener* listener)
    {
        if (! HttpSendRequestEx (request, &buffers, 0, HSR_INITIATE, 0))
            return;

        int totalBytesSent = 0;

        while (totalBytesSent < (int) postData.getSize())
        {
            auto bytesToSend = jmin (1024, (int) postData.getSize() - totalBytesSent);
            DWORD bytesSent = 0;

            if (bytesToSend == 0
                || ! InternetWriteFile (request, static_cast<const char*> (postData.getData()) + totalBytesSent,
                                        (DWORD) bytesToSend, &bytesSent))
            {
                return;
            }

            totalBytesSent += bytesSent;

            if (listener != nullptr
                && ! listener->postDataSendProgress (owner, totalBytesSent, (int) postData.getSize()))
            {
                return;
            }
        }
    }

    void openHTTPConnection (URL_COMPONENTS& uc, const String& address, WebInputStream::Listener* listener)
    {
        const TCHAR* mimeTypes[] = { _T("*/*"), nullptr };

        DWORD flags = INTERNET_FLAG_RELOAD | INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_NO_COOKIES
                        | INTERNET_FLAG_NO_AUTO_REDIRECT;

        if (address.startsWithIgnoreCase ("https:"))
            flags |= INTERNET_FLAG_SECURE;  // (this flag only seems necessary if the OS is running IE6 -
                                            //  IE7 seems to automatically work out when it's https)

        {
            const ScopedLock lock (createConnectionLock);

            request = hasBeenCancelled ? 0
                                       : HttpOpenRequest (connection, httpRequestCmd.toWideCharPointer(),
                                                          uc.lpszUrlPath, 0, 0, mimeTypes, flags, 0);
        }

        if (request != 0)
        {
            INTERNET_BUFFERS buffers = { 0 };
            buffers.dwStructSize    = sizeof (INTERNET_BUFFERS);
            buffers.lpcszHeader     = headers.toWideCharPointer();
            buffers.dwHeadersLength = (DWORD) headers.length();
            buffers.dwBufferTotal   = (DWORD) postData.getSize();

            auto sendRequestAndTryEnd = [this, &buffers, &listener]() -> bool
            {
                sendHTTPRequest (buffers, listener);

                if (HttpEndRequest (request, 0, 0, 0))
                    return true;

                return false;
            };

            auto closed = sendRequestAndTryEnd();

            // N.B. this is needed for some authenticated HTTP connections
            if (! closed && GetLastError() == ERROR_INTERNET_FORCE_RETRY)
                closed = sendRequestAndTryEnd();

            if (closed)
                return;
        }

        closeConnection();
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Pimpl)
};


//==============================================================================
struct GetAdaptersAddressesHelper
{
    bool callGetAdaptersAddresses()
    {
        DynamicLibrary dll ("iphlpapi.dll");
        JUCE_LOAD_WINAPI_FUNCTION (dll, GetAdaptersAddresses, getAdaptersAddresses, DWORD, (ULONG, ULONG, PVOID, PIP_ADAPTER_ADDRESSES, PULONG))

        if (getAdaptersAddresses == nullptr)
            return false;

        adaptersAddresses.malloc (1);
        ULONG len = sizeof (IP_ADAPTER_ADDRESSES);

        if (getAdaptersAddresses (AF_UNSPEC, GAA_FLAG_INCLUDE_PREFIX, NULL, adaptersAddresses, &len) == ERROR_BUFFER_OVERFLOW)
            adaptersAddresses.malloc (len, 1);

        return getAdaptersAddresses (AF_UNSPEC, GAA_FLAG_INCLUDE_PREFIX, NULL, adaptersAddresses, &len) == NO_ERROR;
    }

    HeapBlock<IP_ADAPTER_ADDRESSES> adaptersAddresses;
};

namespace MACAddressHelpers
{
    static void addAddress (Array<MACAddress>& result, const MACAddress& ma)
    {
        if (! ma.isNull())
            result.addIfNotAlreadyThere (ma);
    }

    static void getViaGetAdaptersAddresses (Array<MACAddress>& result)
    {
        GetAdaptersAddressesHelper addressesHelper;

        if (addressesHelper.callGetAdaptersAddresses())
        {
            for (PIP_ADAPTER_ADDRESSES adapter = addressesHelper.adaptersAddresses; adapter != nullptr; adapter = adapter->Next)
            {
                if (adapter->PhysicalAddressLength >= 6)
                    addAddress (result, MACAddress (adapter->PhysicalAddress));
            }
        }
    }

    static void getViaNetBios (Array<MACAddress>& result)
    {
        DynamicLibrary dll ("netapi32.dll");
        JUCE_LOAD_WINAPI_FUNCTION (dll, Netbios, NetbiosCall, UCHAR, (PNCB))

        if (NetbiosCall != 0)
        {
            LANA_ENUM enums = { 0 };

            {
                NCB ncb = { 0 };
                ncb.ncb_command = NCBENUM;
                ncb.ncb_buffer = (unsigned char*) &enums;
                ncb.ncb_length = sizeof (LANA_ENUM);
                NetbiosCall (&ncb);
            }

            for (int i = 0; i < enums.length; ++i)
            {
                NCB ncb2 = { 0 };
                ncb2.ncb_command = NCBRESET;
                ncb2.ncb_lana_num = enums.lana[i];

                if (NetbiosCall (&ncb2) == 0)
                {
                    NCB ncb = { 0 };
                    memcpy (ncb.ncb_callname, "*                   ", NCBNAMSZ);
                    ncb.ncb_command = NCBASTAT;
                    ncb.ncb_lana_num = enums.lana[i];

                    struct ASTAT
                    {
                        ADAPTER_STATUS adapt;
                        NAME_BUFFER    NameBuff [30];
                    };

                    ASTAT astat;
                    zerostruct (astat);
                    ncb.ncb_buffer = (unsigned char*) &astat;
                    ncb.ncb_length = sizeof (ASTAT);

                    if (NetbiosCall (&ncb) == 0 && astat.adapt.adapter_type == 0xfe)
                        addAddress (result, MACAddress (astat.adapt.adapter_address));
                }
            }
        }
    }

    static void split (const sockaddr_in6* sa_in6, int off, uint8* split)
    {
       #if JUCE_MINGW
        split[0] = sa_in6->sin6_addr._S6_un._S6_u8[off + 1];
        split[1] = sa_in6->sin6_addr._S6_un._S6_u8[off];
       #else
        split[0] = sa_in6->sin6_addr.u.Byte[off + 1];
        split[1] = sa_in6->sin6_addr.u.Byte[off];
       #endif
    }

    static IPAddress createAddress (const sockaddr_in6* sa_in6)
    {
        IPAddressByteUnion temp;
        uint16 arr[8];

        for (int i = 0; i < 8; ++i)
        {
            split (sa_in6, i * 2, temp.split);
            arr[i] = temp.combined;
        }

        return IPAddress (arr);
    }

    static IPAddress createAddress (const sockaddr_in* sa_in)
    {
        return IPAddress ((uint8*) &sa_in->sin_addr.s_addr, false);
    }

    template <typename Type>
    static void findAddresses (Array<IPAddress>& result, bool includeIPv6, Type start)
    {
        for (auto addr = start; addr != nullptr; addr = addr->Next)
        {
            if (addr->Address.lpSockaddr->sa_family == AF_INET)
                result.addIfNotAlreadyThere (createAddress ((sockaddr_in*) addr->Address.lpSockaddr));
            else if (addr->Address.lpSockaddr->sa_family == AF_INET6 && includeIPv6)
                result.addIfNotAlreadyThere (createAddress ((sockaddr_in6*) addr->Address.lpSockaddr));
        }
    }
}

void MACAddress::findAllAddresses (Array<MACAddress>& result)
{
    MACAddressHelpers::getViaGetAdaptersAddresses (result);
    MACAddressHelpers::getViaNetBios (result);
}

void IPAddress::findAllAddresses (Array<IPAddress>& result, bool includeIPv6)
{
    result.addIfNotAlreadyThere (IPAddress::local());

    if (includeIPv6)
        result.addIfNotAlreadyThere (IPAddress::local (true));

    GetAdaptersAddressesHelper addressesHelper;

    if (addressesHelper.callGetAdaptersAddresses())
    {
        for (PIP_ADAPTER_ADDRESSES adapter = addressesHelper.adaptersAddresses; adapter != nullptr; adapter = adapter->Next)
        {
            MACAddressHelpers::findAddresses (result, includeIPv6, adapter->FirstUnicastAddress);
            MACAddressHelpers::findAddresses (result, includeIPv6, adapter->FirstAnycastAddress);
            MACAddressHelpers::findAddresses (result, includeIPv6, adapter->FirstMulticastAddress);
        }
    }
}

IPAddress IPAddress::getInterfaceBroadcastAddress (const IPAddress&)
{
    // TODO
    return {};
}


//==============================================================================
bool JUCE_CALLTYPE Process::openEmailWithAttachments (const String& targetEmailAddress,
                                                      const String& emailSubject,
                                                      const String& bodyText,
                                                      const StringArray& filesToAttach)
{
    DynamicLibrary dll ("MAPI32.dll");
    JUCE_LOAD_WINAPI_FUNCTION (dll, MAPISendMail, mapiSendMail,
                               ULONG, (LHANDLE, ULONG, lpMapiMessage, ::FLAGS, ULONG))

    if (mapiSendMail == nullptr)
        return false;

    MapiMessage message = { 0 };
    message.lpszSubject = (LPSTR) emailSubject.toRawUTF8();
    message.lpszNoteText = (LPSTR) bodyText.toRawUTF8();

    MapiRecipDesc recip = { 0 };
    recip.ulRecipClass = MAPI_TO;
    String targetEmailAddress_ (targetEmailAddress);
    if (targetEmailAddress_.isEmpty())
        targetEmailAddress_ = " "; // (Windows Mail can't deal with a blank address)
    recip.lpszName = (LPSTR) targetEmailAddress_.toRawUTF8();
    message.nRecipCount = 1;
    message.lpRecips = &recip;

    HeapBlock<MapiFileDesc> files;
    files.calloc (filesToAttach.size());

    message.nFileCount = (ULONG) filesToAttach.size();
    message.lpFiles = files;

    for (int i = 0; i < filesToAttach.size(); ++i)
    {
        files[i].nPosition = (ULONG) -1;
        files[i].lpszPathName = (LPSTR) filesToAttach[i].toRawUTF8();
    }

    return mapiSendMail (0, 0, &message, MAPI_DIALOG | MAPI_LOGON_UI, 0) == SUCCESS_SUCCESS;
}

URL::DownloadTask* URL::downloadToFile (const File& targetLocation, String extraHeaders, DownloadTask::Listener* listener, bool shouldUsePost)
{
    return URL::DownloadTask::createFallbackDownloader (*this, targetLocation, extraHeaders, listener, shouldUsePost);
}

} // namespace juce

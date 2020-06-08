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
    ifaddrs* addrs = nullptr;

    if (getifaddrs (&addrs) == 0)
    {
        for (const ifaddrs* cursor = addrs; cursor != nullptr; cursor = cursor->ifa_next)
        {
            // Required to avoid misaligned pointer access
            sockaddr sto;
            std::memcpy (&sto, cursor->ifa_addr, sizeof (sockaddr));

            if (sto.sa_family == AF_LINK)
            {
                auto sadd = reinterpret_cast<const sockaddr_dl*> (cursor->ifa_addr);

               #ifndef IFT_ETHER
                enum { IFT_ETHER = 6 };
               #endif

                if (sadd->sdl_type == IFT_ETHER)
                {
                    MACAddress ma (((const uint8*) sadd->sdl_data) + sadd->sdl_nlen);

                    if (! ma.isNull())
                        result.addIfNotAlreadyThere (ma);
                }
            }
        }

        freeifaddrs (addrs);
    }
}

//==============================================================================
bool JUCE_CALLTYPE Process::openEmailWithAttachments (const String& targetEmailAddress,
                                                      const String& emailSubject,
                                                      const String& bodyText,
                                                      const StringArray& filesToAttach)
{
  #if JUCE_IOS
    ignoreUnused (targetEmailAddress, emailSubject, bodyText, filesToAttach);

    //xxx probably need to use MFMailComposeViewController
    jassertfalse;
    return false;
  #else
    JUCE_AUTORELEASEPOOL
    {
        String script;
        script << "tell application \"Mail\"\r\n"
                  "set newMessage to make new outgoing message with properties {subject:\""
               << emailSubject.replace ("\"", "\\\"")
               << "\", content:\""
               << bodyText.replace ("\"", "\\\"")
               << "\" & return & return}\r\n"
                  "tell newMessage\r\n"
                  "set visible to true\r\n"
                  "set sender to \"sdfsdfsdfewf\"\r\n"
                  "make new to recipient at end of to recipients with properties {address:\""
               << targetEmailAddress
               << "\"}\r\n";

        for (int i = 0; i < filesToAttach.size(); ++i)
        {
            script << "tell content\r\n"
                      "make new attachment with properties {file name:\""
                   << filesToAttach[i].replace ("\"", "\\\"")
                   << "\"} at after the last paragraph\r\n"
                      "end tell\r\n";
        }

        script << "end tell\r\n"
                  "end tell\r\n";

        NSAppleScript* s = [[NSAppleScript alloc] initWithSource: juceStringToNS (script)];
        NSDictionary* error = nil;
        const bool ok = [s executeAndReturnError: &error] != nil;
        [s release];

        return ok;
    }
  #endif
}

//==============================================================================
// Unfortunately, we need to have this ugly ifdef here as long as some older OS X versions do not support NSURLSession
#if JUCE_IOS || (defined (MAC_OS_X_VERSION_10_10) && MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_10)

//==============================================================================
class URLConnectionState   : private Thread
{
public:
    URLConnectionState (NSURLRequest* req, const int maxRedirects)
        : Thread ("http connection"),
          request ([req retain]),
          data ([[NSMutableData data] retain]),
          numRedirectsToFollow (maxRedirects)
    {
        static DelegateClass cls;
        delegate = [cls.createInstance() init];
        DelegateClass::setState (delegate, this);
    }

    ~URLConnectionState() override
    {
        signalThreadShouldExit();

        {
            const ScopedLock sl (dataLock);
            isBeingDeleted = true;
            [task cancel];
            DelegateClass::setState (delegate, nullptr);
        }

        stopThread (10000);
        [task release];
        [request release];
        [headers release];

        [session finishTasksAndInvalidate];
        [session release];

        const ScopedLock sl (dataLock);
        [delegate release];
        [data release];
    }

    void cancel()
    {
        {
            const ScopedLock lock (createTaskLock);

            hasBeenCancelled = true;
        }

        signalThreadShouldExit();
        stopThread (10000);
    }

    bool start (WebInputStream& inputStream, WebInputStream::Listener* listener)
    {
        {
            const ScopedLock lock (createTaskLock);

            if (hasBeenCancelled)
                return false;
        }

        startThread();

        while (isThreadRunning() && ! initialised)
        {
            if (listener != nullptr)
                if (! listener->postDataSendProgress (inputStream, (int) latestTotalBytes, (int) [[request HTTPBody] length]))
                    return false;

            Thread::sleep (1);
        }

        return true;
    }

    int read (char* dest, int numBytes)
    {
        int numDone = 0;

        while (numBytes > 0)
        {
            const int available = jmin (numBytes, (int) [data length]);

            if (available > 0)
            {
                const ScopedLock sl (dataLock);
                [data getBytes: dest length: (NSUInteger) available];
                [data replaceBytesInRange: NSMakeRange (0, (NSUInteger) available) withBytes: nil length: 0];

                numDone += available;
                numBytes -= available;
                dest += available;
            }
            else
            {
                if (hasFailed || hasFinished)
                    break;

                Thread::sleep (1);
            }
        }

        return numDone;
    }

    void didReceiveResponse (NSURLResponse* response, id completionHandler)
    {
        {
            const ScopedLock sl (dataLock);
            if (isBeingDeleted)
                return;

            [data setLength: 0];
        }

        contentLength = [response expectedContentLength];

        [headers release];
        headers = nil;

        if ([response isKindOfClass: [NSHTTPURLResponse class]])
        {
            auto httpResponse = (NSHTTPURLResponse*) response;
            headers = [[httpResponse allHeaderFields] retain];
            statusCode = (int) [httpResponse statusCode];
        }

        initialised = true;

        if (completionHandler != nil)
        {
            // Need to wrangle this parameter back into an obj-C block,
            // and call it to allow the transfer to continue..
            void (^callbackBlock)(NSURLSessionResponseDisposition) = completionHandler;
            callbackBlock (NSURLSessionResponseAllow);
        }
    }

    void didComplete (NSError* error)
    {
        const ScopedLock sl (dataLock);

        if (isBeingDeleted)
            return;

       #if JUCE_DEBUG
        if (error != nullptr)
            DBG (nsStringToJuce ([error description]));
       #endif

        hasFailed = (error != nullptr);
        initialised = true;
        signalThreadShouldExit();
    }

    void didReceiveData (NSData* newData)
    {
        const ScopedLock sl (dataLock);

        if (isBeingDeleted)
            return;

        [data appendData: newData];
        initialised = true;
    }

    void didSendBodyData (int64_t totalBytesWritten)
    {
        latestTotalBytes = static_cast<int> (totalBytesWritten);
    }

    void willPerformHTTPRedirection (NSURLRequest* urlRequest, void (^completionHandler)(NSURLRequest *))
    {
        {
            const ScopedLock sl (dataLock);

            if (isBeingDeleted)
                return;
        }

        completionHandler (numRedirects++ < numRedirectsToFollow ? urlRequest : nil);
    }

    void run() override
    {
        jassert (task == nil && session == nil);

        session = [[NSURLSession sessionWithConfiguration: [NSURLSessionConfiguration defaultSessionConfiguration]
                                                 delegate: delegate
                                            delegateQueue: [NSOperationQueue currentQueue]] retain];

        {
            const ScopedLock lock (createTaskLock);

            if (! hasBeenCancelled)
                task = [session dataTaskWithRequest: request];
        }

        if (task == nil)
            return;

        [task retain];
        [task resume];

        while (! threadShouldExit())
            wait (5);

        hasFinished = true;
        initialised = true;
    }

    int64 contentLength = -1;
    CriticalSection dataLock;
    id delegate = nil;
    NSURLRequest* request = nil;
    NSURLSession* session = nil;
    NSURLSessionTask* task = nil;
    NSMutableData* data = nil;
    NSDictionary* headers = nil;
    int statusCode = 0;
    std::atomic<bool> initialised { false }, hasFailed { false }, hasFinished { false };
    bool isBeingDeleted = false;
    const int numRedirectsToFollow;
    int numRedirects = 0;
    int64 latestTotalBytes = 0;
    CriticalSection createTaskLock;
    bool hasBeenCancelled = false;

private:
    //==============================================================================
    struct DelegateClass  : public ObjCClass<NSObject>
    {
        DelegateClass()  : ObjCClass<NSObject> ("JUCE_URLDelegate_")
        {
            addIvar<URLConnectionState*> ("state");

            addMethod (@selector (URLSession:dataTask:didReceiveResponse:completionHandler:),
                                                                            didReceiveResponse,         "v@:@@@@");
            addMethod (@selector (URLSession:didBecomeInvalidWithError:),   didBecomeInvalidWithError,  "v@:@@");
            addMethod (@selector (URLSession:dataTask:didReceiveData:),     didReceiveData,             "v@:@@@");
            addMethod (@selector (URLSession:task:didSendBodyData:totalBytesSent:totalBytesExpectedToSend:),
                                                                            didSendBodyData,            "v@:@@qqq");
            addMethod (@selector (URLSession:task:willPerformHTTPRedirection:newRequest:completionHandler:),
                                                                            willPerformHTTPRedirection, "v@:@@@@@");
            addMethod (@selector (URLSession:task:didCompleteWithError:),   didCompleteWithError,       "v@:@@@");

            registerClass();
        }

        static void setState (id self, URLConnectionState* state)  { object_setInstanceVariable (self, "state", state); }
        static URLConnectionState* getState (id self)              { return getIvar<URLConnectionState*> (self, "state"); }

    private:
        static void didReceiveResponse (id self, SEL, NSURLSession*, NSURLSessionDataTask*, NSURLResponse* response, id completionHandler)
        {
            if (auto state = getState (self))
                state->didReceiveResponse (response, completionHandler);
        }

        static void didBecomeInvalidWithError (id self, SEL, NSURLSession*, NSError* error)
        {
            if (auto state = getState (self))
                state->didComplete (error);
        }

        static void didReceiveData (id self, SEL, NSURLSession*, NSURLSessionDataTask*, NSData* newData)
        {
            if (auto state = getState (self))
                state->didReceiveData (newData);
        }

        static void didSendBodyData (id self, SEL, NSURLSession*, NSURLSessionTask*, int64_t, int64_t totalBytesWritten, int64_t)
        {
            if (auto state = getState (self))
                state->didSendBodyData (totalBytesWritten);
        }

        static void willPerformHTTPRedirection (id self, SEL, NSURLSession*, NSURLSessionTask*, NSHTTPURLResponse*,
                                                NSURLRequest* request, void (^completionHandler)(NSURLRequest *))
        {
            if (auto state = getState (self))
                state->willPerformHTTPRedirection (request, completionHandler);
        }

        static void didCompleteWithError (id self, SEL, NSURLConnection*, NSURLSessionTask*, NSError* error)
        {
            if (auto state = getState (self))
                state->didComplete (error);
        }
    };

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (URLConnectionState)
};

//==============================================================================
#if JUCE_IOS
struct BackgroundDownloadTask  : public URL::DownloadTask
{
    BackgroundDownloadTask (const URL& urlToUse,
                            const File& targetLocationToUse,
                            String extraHeadersToUse,
                            URL::DownloadTask::Listener* listenerToUse,
                            bool shouldUsePostRequest)
         : listener (listenerToUse),
           uniqueIdentifier (String (urlToUse.toString (true).hashCode64()) + String (Random().nextInt64()))
    {
        targetLocation = targetLocationToUse;
        downloaded = -1;

        static DelegateClass cls;
        delegate = [cls.createInstance() init];
        DelegateClass::setState (delegate, this);

        activeSessions.set (uniqueIdentifier, this);
        auto nsUrl = [NSURL URLWithString: juceStringToNS (urlToUse.toString (true))];
        NSMutableURLRequest* request = [[NSMutableURLRequest alloc] initWithURL: nsUrl];

        if (shouldUsePostRequest)
            [request setHTTPMethod: @"POST"];

        StringArray headerLines;
        headerLines.addLines (extraHeadersToUse);
        headerLines.removeEmptyStrings (true);

        for (int i = 0; i < headerLines.size(); ++i)
        {
            String key   = headerLines[i].upToFirstOccurrenceOf (":", false, false).trim();
            String value = headerLines[i].fromFirstOccurrenceOf (":", false, false).trim();

            if (key.isNotEmpty() && value.isNotEmpty())
                [request addValue: juceStringToNS (value) forHTTPHeaderField: juceStringToNS (key)];
        }

        session =
            [NSURLSession sessionWithConfiguration: [NSURLSessionConfiguration backgroundSessionConfigurationWithIdentifier: juceStringToNS (uniqueIdentifier)]
                                          delegate: delegate
                                     delegateQueue: nullptr];

        if (session != nullptr)
            downloadTask = [session downloadTaskWithRequest:request];

        // Workaround for an Apple bug. See https://github.com/AFNetworking/AFNetworking/issues/2334
        [request HTTPBody];

        [request release];
    }

    ~BackgroundDownloadTask()
    {
        activeSessions.remove (uniqueIdentifier);

        if (httpCode != -1)
            httpCode = 500;

        finished = true;
        connectionEvent.signal();

        [session invalidateAndCancel];
        while (! hasBeenDestroyed)
            destroyEvent.wait();

        [delegate release];
    }

    bool initOK()
    {
        return (downloadTask != nullptr);
    }

    bool connect()
    {
        [downloadTask resume];
        while (downloaded == -1 && finished == false)
            connectionEvent.wait();

        connectFinished = true;
        return ! error;
    }

    //==============================================================================
    URL::DownloadTask::Listener* listener;
    NSObject<NSURLSessionDelegate>* delegate = nil;
    NSURLSession* session = nil;
    NSURLSessionDownloadTask* downloadTask = nil;
    bool connectFinished = false, hasBeenDestroyed = false;
    Atomic<int> calledComplete;
    WaitableEvent connectionEvent, destroyEvent;
    String uniqueIdentifier;

    static HashMap<String, BackgroundDownloadTask*, DefaultHashFunctions, CriticalSection> activeSessions;

    void didWriteData (int64 totalBytesWritten, int64 totalBytesExpectedToWrite)
    {
        downloaded = totalBytesWritten;

        if (contentLength == -1)
            contentLength = totalBytesExpectedToWrite;

        if (connectFinished && error == false && finished == false && listener != nullptr)
            listener->progress (this, totalBytesWritten, contentLength);

        connectionEvent.signal();
    }

    void didFinishDownloadingToURL (NSURL* location)
    {
        NSFileManager* fileManager = [[NSFileManager alloc] init];
        error = ([fileManager moveItemAtURL: location
                                      toURL: createNSURLFromFile (targetLocation)
                                      error: nil] == NO);
        httpCode = 200;
        finished = true;

        connectionEvent.signal();

        if (listener != nullptr && calledComplete.exchange (1) == 0)
        {
            if (contentLength > 0 && downloaded < contentLength)
            {
                downloaded = contentLength;
                listener->progress (this, downloaded, contentLength);
            }

            listener->finished (this, !error);
        }
    }

    static int getHTTPErrorCode (NSError* nsError)
    {
        // see https://developer.apple.com/reference/foundation/nsurlsessiondownloadtask?language=objc
        switch ([nsError code])
        {
            case NSURLErrorUserAuthenticationRequired:  return 401;
            case NSURLErrorNoPermissionsToReadFile:     return 403;
            case NSURLErrorFileDoesNotExist:            return 404;
            default:                                    return 500;
        }
    }

    void didCompleteWithError (NSError* nsError)
    {
        if (calledComplete.exchange (1) == 0)
        {
            httpCode = nsError != nil ? getHTTPErrorCode (nsError) : -1;
            error = true;
            finished = true;

            if (listener != nullptr)
                listener->finished (this, ! error);
        }

        connectionEvent.signal();
    }

    void didBecomeInvalidWithError()
    {
        hasBeenDestroyed = true;
        destroyEvent.signal();
    }

    //==============================================================================
    void notify()
    {
        if (downloadTask == nullptr) return;

        if (NSError* error = [downloadTask error])
        {
            didCompleteWithError (error);
        }
        else
        {
            const int64 contentLength = [downloadTask countOfBytesExpectedToReceive];

            if ([downloadTask state] == NSURLSessionTaskStateCompleted)
                didWriteData (contentLength, contentLength);
            else
                didWriteData ([downloadTask countOfBytesReceived], contentLength);
        }
    }

    static void invokeNotify (const String& identifier)
    {
        ScopedLock lock (activeSessions.getLock());

        if (auto* task = activeSessions[identifier])
            task->notify();
    }

    //==============================================================================
    struct DelegateClass  : public ObjCClass<NSObject<NSURLSessionDelegate>>
    {
        DelegateClass()  : ObjCClass<NSObject<NSURLSessionDelegate>> ("JUCE_URLDelegate_")
        {
            addIvar<BackgroundDownloadTask*> ("state");

            addMethod (@selector (URLSession:downloadTask:didWriteData:totalBytesWritten:totalBytesExpectedToWrite:),
                                                                                        didWriteData,               "v@:@@qqq");
            addMethod (@selector (URLSession:downloadTask:didFinishDownloadingToURL:),  didFinishDownloadingToURL,  "v@:@@@");
            addMethod (@selector (URLSession:task:didCompleteWithError:),               didCompleteWithError,       "v@:@@@");
            addMethod (@selector (URLSession:didBecomeInvalidWithError:),               didBecomeInvalidWithError,  "v@:@@@");

            registerClass();
        }

        static void setState (id self, BackgroundDownloadTask* state)  { object_setInstanceVariable (self, "state", state); }
        static BackgroundDownloadTask* getState (id self)              { return getIvar<BackgroundDownloadTask*> (self, "state"); }

    private:
        static void didWriteData (id self, SEL, NSURLSession*, NSURLSessionDownloadTask*, int64_t, int64_t totalBytesWritten, int64_t totalBytesExpectedToWrite)
        {
            if (auto state = getState (self))
                state->didWriteData (totalBytesWritten, totalBytesExpectedToWrite);
        }

        static void didFinishDownloadingToURL (id self, SEL, NSURLSession*, NSURLSessionDownloadTask*, NSURL* location)
        {
            if (auto state = getState (self))
                state->didFinishDownloadingToURL (location);
        }

        static void didCompleteWithError (id self, SEL, NSURLSession*, NSURLSessionTask*, NSError* nsError)
        {
            if (auto state = getState (self))
                state->didCompleteWithError (nsError);
        }

        static void didBecomeInvalidWithError (id self, SEL, NSURLSession*, NSURLSessionTask*, NSError*)
        {
            if (auto state = getState (self))
                state->didBecomeInvalidWithError();
        }
    };
};

HashMap<String, BackgroundDownloadTask*, DefaultHashFunctions, CriticalSection> BackgroundDownloadTask::activeSessions;

URL::DownloadTask* URL::downloadToFile (const File& targetLocation, String extraHeaders, DownloadTask::Listener* listener, bool usePostRequest)
{
    std::unique_ptr<BackgroundDownloadTask> downloadTask (new BackgroundDownloadTask (*this, targetLocation, extraHeaders, listener, usePostRequest));

    if (downloadTask->initOK() && downloadTask->connect())
        return downloadTask.release();

    return nullptr;
}

void URL::DownloadTask::juce_iosURLSessionNotify (const String& identifier)
{
    BackgroundDownloadTask::invokeNotify (identifier);
}
#else
URL::DownloadTask* URL::downloadToFile (const File& targetLocation, String extraHeaders, DownloadTask::Listener* listener, bool usePost)
{
    return URL::DownloadTask::createFallbackDownloader (*this, targetLocation, extraHeaders, listener, usePost);
}
#endif

//==============================================================================
#else

// This version is only used for backwards-compatibility with older OSX targets,
// so we'll turn off deprecation warnings. This code will be removed at some point
// in the future.

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated"

//==============================================================================
class URLConnectionState   : public Thread
{
public:
    URLConnectionState (NSURLRequest* req, const int maxRedirects)
        : Thread ("http connection"),
          request ([req retain]),
          data ([[NSMutableData data] retain]),
          numRedirectsToFollow (maxRedirects)
    {
        static DelegateClass cls;
        delegate = [cls.createInstance() init];
        DelegateClass::setState (delegate, this);
    }

    ~URLConnectionState() override
    {
        stop();

        [connection release];
        [request release];
        [headers release];
        [delegate release];
        [data release];
    }

    bool start (WebInputStream& inputStream, WebInputStream::Listener* listener)
    {
        startThread();

        while (isThreadRunning() && ! initialised)
        {
            if (listener != nullptr)
                if (! listener->postDataSendProgress (inputStream, latestTotalBytes, (int) [[request HTTPBody] length]))
                    return false;

            Thread::sleep (1);
        }

        return connection != nil && ! hasFailed;
    }

    void stop()
    {
        {
            const ScopedLock dLock (dataLock);
            const ScopedLock connectionLock (createConnectionLock);

            hasBeenCancelled = true;

            if (connection != nil)
                [connection cancel];
        }

        stopThread (10000);
    }

    void cancel()
    {
        hasFinished = hasFailed = true;
        stop();
    }

    int read (char* dest, int numBytes)
    {
        int numDone = 0;

        while (numBytes > 0)
        {
            const int available = jmin (numBytes, (int) [data length]);

            if (available > 0)
            {
                const ScopedLock sl (dataLock);
                [data getBytes: dest length: (NSUInteger) available];
                [data replaceBytesInRange: NSMakeRange (0, (NSUInteger) available) withBytes: nil length: 0];

                numDone += available;
                numBytes -= available;
                dest += available;
            }
            else
            {
                if (hasFailed || hasFinished)
                    break;

                Thread::sleep (1);
            }
        }

        return numDone;
    }

    void didReceiveResponse (NSURLResponse* response)
    {
        {
            const ScopedLock sl (dataLock);
            [data setLength: 0];
        }

        contentLength = [response expectedContentLength];

        [headers release];
        headers = nil;

        if ([response isKindOfClass: [NSHTTPURLResponse class]])
        {
            NSHTTPURLResponse* httpResponse = (NSHTTPURLResponse*) response;
            headers = [[httpResponse allHeaderFields] retain];
            statusCode = (int) [httpResponse statusCode];
        }

        initialised = true;
    }

    NSURLRequest* willSendRequest (NSURLRequest* newRequest, NSURLResponse* redirectResponse)
    {
        if (redirectResponse != nullptr)
        {
            if (numRedirects >= numRedirectsToFollow)
                return nil;  // Cancel redirect and allow connection to continue

            ++numRedirects;
        }

        return newRequest;
    }

    void didFailWithError (NSError* error)
    {
        DBG (nsStringToJuce ([error description])); ignoreUnused (error);
        nsUrlErrorCode = [error code];
        hasFailed = true;
        initialised = true;
        signalThreadShouldExit();
    }

    void didReceiveData (NSData* newData)
    {
        const ScopedLock sl (dataLock);
        [data appendData: newData];
        initialised = true;
    }

    void didSendBodyData (NSInteger totalBytesWritten, NSInteger /*totalBytesExpected*/)
    {
        latestTotalBytes = static_cast<int> (totalBytesWritten);
    }

    void finishedLoading()
    {
        hasFinished = true;
        initialised = true;
        signalThreadShouldExit();
    }

    void run() override
    {
        {
            const ScopedLock lock (createConnectionLock);

            if (hasBeenCancelled)
                return;

            connection = [[NSURLConnection alloc] initWithRequest: request
                                                         delegate: delegate];
        }

        while (! threadShouldExit())
        {
            JUCE_AUTORELEASEPOOL
            {
                [[NSRunLoop currentRunLoop] runUntilDate: [NSDate dateWithTimeIntervalSinceNow: 0.01]];
            }
        }
    }

    int64 contentLength = -1;
    CriticalSection dataLock;
    NSObject* delegate = nil;
    NSURLRequest* request = nil;
    NSURLConnection* connection = nil;
    NSMutableData* data = nil;
    NSDictionary* headers = nil;
    NSInteger nsUrlErrorCode = 0;
    int statusCode = 0;
    std::atomic<bool> initialised { false }, hasFailed { false }, hasFinished { false };
    const int numRedirectsToFollow;
    int numRedirects = 0;
    int latestTotalBytes = 0;
    CriticalSection createConnectionLock;
    bool hasBeenCancelled = false;

private:
    //==============================================================================
    struct DelegateClass  : public ObjCClass<NSObject>
    {
        DelegateClass()  : ObjCClass<NSObject> ("JUCENetworkDelegate_")
        {
            addIvar<URLConnectionState*> ("state");

            addMethod (@selector (connection:didReceiveResponse:), didReceiveResponse,            "v@:@@");
            addMethod (@selector (connection:didFailWithError:),   didFailWithError,              "v@:@@");
            addMethod (@selector (connection:didReceiveData:),     didReceiveData,                "v@:@@");
            addMethod (@selector (connection:didSendBodyData:totalBytesWritten:totalBytesExpectedToWrite:),
                                                                   connectionDidSendBodyData,     "v@:@iii");
            addMethod (@selector (connectionDidFinishLoading:),    connectionDidFinishLoading,    "v@:@");
            addMethod (@selector (connection:willSendRequest:redirectResponse:), willSendRequest, "@@:@@@");

            registerClass();
        }

        static void setState (id self, URLConnectionState* state)  { object_setInstanceVariable (self, "state", state); }
        static URLConnectionState* getState (id self)              { return getIvar<URLConnectionState*> (self, "state"); }

    private:
        static void didReceiveResponse (id self, SEL, NSURLConnection*, NSURLResponse* response)
        {
            getState (self)->didReceiveResponse (response);
        }

        static void didFailWithError (id self, SEL, NSURLConnection*, NSError* error)
        {
            getState (self)->didFailWithError (error);
        }

        static void didReceiveData (id self, SEL, NSURLConnection*, NSData* newData)
        {
            getState (self)->didReceiveData (newData);
        }

        static NSURLRequest* willSendRequest (id self, SEL, NSURLConnection*, NSURLRequest* request, NSURLResponse* response)
        {
            return getState (self)->willSendRequest (request, response);
        }

        static void connectionDidSendBodyData (id self, SEL, NSURLConnection*, NSInteger, NSInteger totalBytesWritten, NSInteger totalBytesExpected)
        {
            getState (self)->didSendBodyData (totalBytesWritten, totalBytesExpected);
        }

        static void connectionDidFinishLoading (id self, SEL, NSURLConnection*)
        {
            getState (self)->finishedLoading();
        }
    };

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (URLConnectionState)
};

URL::DownloadTask* URL::downloadToFile (const File& targetLocation, String extraHeaders, DownloadTask::Listener* listener, bool shouldUsePost)
{
    return URL::DownloadTask::createFallbackDownloader (*this, targetLocation, extraHeaders, listener, shouldUsePost);
}

#pragma clang diagnostic pop

#endif


//==============================================================================
class WebInputStream::Pimpl
{
public:
    Pimpl (WebInputStream& pimplOwner, const URL& urlToUse, bool shouldBePost)
      : owner (pimplOwner), url (urlToUse), isPost (shouldBePost),
        httpRequestCmd (shouldBePost ? "POST" : "GET")
    {
    }

    ~Pimpl()
    {
        connection.reset();
    }

    bool connect (WebInputStream::Listener* webInputListener, int numRetries = 0)
    {
        ignoreUnused (numRetries);

        {
            const ScopedLock lock (createConnectionLock);

            if (hasBeenCancelled)
                return false;

            createConnection();
        }

        if (connection == nullptr)
            return false;

        if (! connection->start (owner, webInputListener))
        {
            // Workaround for deployment targets below 10.10 where HTTPS POST requests with keep-alive fail with the NSURLErrorNetworkConnectionLost error code.
           #if ! (JUCE_IOS || (defined (MAC_OS_X_VERSION_10_10) && MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_10))
            if (numRetries == 0 && connection->nsUrlErrorCode == NSURLErrorNetworkConnectionLost)
            {
                connection.reset();
                return connect (webInputListener, ++numRetries);
            }
           #endif

            connection.reset();
            return false;
        }

        if (connection->headers != nil)
        {
            statusCode = connection->statusCode;

            NSEnumerator* enumerator = [connection->headers keyEnumerator];

            while (NSString* key = [enumerator nextObject])
                responseHeaders.set (nsStringToJuce (key),
                                     nsStringToJuce ((NSString*) [connection->headers objectForKey: key]));

            return true;
        }

        return false;
    }

    void cancel()
    {
        {
            const ScopedLock lock (createConnectionLock);

            if (connection != nullptr)
                connection->cancel();

            hasBeenCancelled = true;
        }
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
    bool isError() const                { return (connection == nullptr || connection->headers == nullptr); }
    int64 getTotalLength()              { return connection == nullptr ? -1 : connection->contentLength; }
    bool isExhausted()                  { return finished; }
    int64 getPosition()                 { return position; }

    int read (void* buffer, int bytesToRead)
    {
        jassert (buffer != nullptr && bytesToRead >= 0);

        if (finished || isError())
            return 0;

        JUCE_AUTORELEASEPOOL
        {
            const int bytesRead = connection->read (static_cast<char*> (buffer), bytesToRead);
            position += bytesRead;

            if (bytesRead == 0)
                finished = true;

            return bytesRead;
        }
    }

    bool setPosition (int64 wantedPos)
    {
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

    int statusCode = 0;

private:
    WebInputStream& owner;
    URL url;
    std::unique_ptr<URLConnectionState> connection;
    String headers;
    MemoryBlock postData;
    int64 position = 0;
    bool finished = false;
    const bool isPost;
    int timeOutMs = 0;
    int numRedirectsToFollow = 5;
    String httpRequestCmd;
    StringPairArray responseHeaders;
    CriticalSection createConnectionLock;
    bool hasBeenCancelled = false;

    void createConnection()
    {
        jassert (connection == nullptr);

        if (NSURL* nsURL = [NSURL URLWithString: juceStringToNS (url.toString (! isPost))])
        {
            if (NSMutableURLRequest* req = [NSMutableURLRequest requestWithURL: nsURL
                                                                   cachePolicy: NSURLRequestReloadIgnoringLocalCacheData
                                                               timeoutInterval: timeOutMs <= 0 ? 60.0 : (timeOutMs / 1000.0)])
            {
                if (NSString* httpMethod = [NSString stringWithUTF8String: httpRequestCmd.toRawUTF8()])
                {
                    [req setHTTPMethod: httpMethod];

                    if (isPost)
                    {
                        WebInputStream::createHeadersAndPostData (url, headers, postData);

                        if (postData.getSize() > 0)
                            [req setHTTPBody: [NSData dataWithBytes: postData.getData()
                                                             length: postData.getSize()]];
                    }

                    StringArray headerLines;
                    headerLines.addLines (headers);
                    headerLines.removeEmptyStrings (true);

                    for (int i = 0; i < headerLines.size(); ++i)
                    {
                        auto key   = headerLines[i].upToFirstOccurrenceOf (":", false, false).trim();
                        auto value = headerLines[i].fromFirstOccurrenceOf (":", false, false).trim();

                        if (key.isNotEmpty() && value.isNotEmpty())
                            [req addValue: juceStringToNS (value) forHTTPHeaderField: juceStringToNS (key)];
                    }

                    // Workaround for an Apple bug. See https://github.com/AFNetworking/AFNetworking/issues/2334
                    [req HTTPBody];

                    connection.reset (new URLConnectionState (req, numRedirectsToFollow));
                }
            }
        }
    }

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Pimpl)
};

} // namespace juce

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

//==============================================================================
/**
    An InputStream which can be used to read from a given url.

    @tags{Core}
*/
class JUCE_API WebInputStream : public InputStream
{
 public:
    /** Used to receive callbacks for data send progress */
    class JUCE_API Listener
    {
    public:
        virtual ~Listener() = default;

        virtual bool postDataSendProgress (WebInputStream& /*request*/, int /*bytesSent*/, int /*totalBytes*/)    { return true; }
    };

    /** Creates a new WebInputstream which can be used to read from a url.

        @param url      The url that should be retrieved. This parameter may also contain
                        post data and/or parameters.
        @param usePost  Specifies whether a GET or a POST command should be used. This
                        parameter will also influence the way parameters are encoded.
    */
    WebInputStream (const URL& url, const bool usePost);

    ~WebInputStream() override;


    /** Add extra headers to http request

        Returns a reference to itself so that several methods can be chained.

        @param extraHeaders   this string is appended onto the headers that are used for
                              the request. It must therefore be a valid set of HTML
                              header directives, separated by newlines.
    */
    WebInputStream& withExtraHeaders (const String& extraHeaders);

    /** Override the http command that is sent

        Returns a reference to itself so that several methods can be chained.

        Note that this command will not change the way parameters are sent. This
        must be specified in the constructor.

        @param customRequestCommand this string is the custom http request command such
                                    as POST or GET.
    */
    WebInputStream& withCustomRequestCommand (const String& customRequestCommand);

    /** Specify the connection time-out

        Returns a reference to itself so that several methods can be chained.

        @param timeoutInMs    the number of milliseconds to wait until the connection
                              request is aborted.
    */
    WebInputStream& withConnectionTimeout (int timeoutInMs);

    /** Specify the number of redirects to be followed

        Returns a reference to itself so that several methods can be chained.

        @param numRedirects specifies the number of redirects that will be followed
                            before returning a response (ignored for Android which
                            follows up to 5 redirects)
    */
    WebInputStream& withNumRedirectsToFollow (int numRedirects);

    /** Returns a string array pair of the request headers */
    StringPairArray getRequestHeaders() const;

    /** Returns a string array pair of response headers

        If getResponseHeaders is called without an established connection, then
        getResponseHeaders will call connect internally and block until connect
        returns - either due to a successful connection or a connection
        error.

        @see connect
    */
    StringPairArray getResponseHeaders();

    /** Returns the status code returned by the http server

        If getStatusCode is called without an established connection, then
        getStatusCode will call connect internally and block until connect
        returns - either due to a successful connection or a connection
        error.

        @see connect
    */
    int getStatusCode();

    /** Wait until the first byte is ready for reading

        This method will attempt to connect to the url given in the constructor
        and block until the status code and all response headers have been received or
        an error has occurred.

        Note that most methods will call connect internally if they are called without
        an established connection. Therefore, it is not necessary to explicitly
        call connect unless you would like to use a custom listener.

        After a successful call to connect, getResponseHeaders, getTotalLength and
        getStatusCode will all be non-blocking.

        @param listener    A listener to receive progress callbacks on the status
                           of a POST data upload.

        @see getResponseHeaders, getTotalLength, getStatusCode
    */
    bool connect (Listener* listener);

    /** Returns true if there was an error during the connection attempt. */
    bool isError() const;

    /** Will cancel a blocking read and prevent any subsequent connection attempts. */
    void cancel();

    //==============================================================================
    /** Returns the total number of bytes available for reading in this stream.

        Note that this is the number of bytes available from the start of the
        stream, not from the current position.

        If getTotalLength is called without an established connection, then
        getTotalLength will call connect internally and block until connect
        returns - either due to a successful connection or a connection
        error.

        If the size of the stream isn't actually known, this will return -1.
    */
    int64 getTotalLength() override;

    /** Reads some data from the stream into a memory buffer.

        This method will block until the bytesToRead bytes are available.

        This method calls connect internally if the connection hasn't already
        been established.

        @param destBuffer       the destination buffer for the data. This must not be null.
        @param maxBytesToRead   the maximum number of bytes to read - make sure the
                                memory block passed in is big enough to contain this
                                many bytes. This value must not be negative.

        @returns    the actual number of bytes that were read, which may be less than
                    maxBytesToRead if the stream is exhausted before it gets that far
    */
    int read (void* destBuffer, int maxBytesToRead) override;

    /** Returns true if the stream has no more data to read. */
    bool isExhausted() override;

    /** Returns the offset of the next byte that will be read from the stream.
        @see setPosition
    */
    int64 getPosition() override;

    /** Tries to move the current read position of the stream.

        The position is an absolute number of bytes from the stream's start.

        For a WebInputStream, this method will fail if wantedPos is smaller
        than the current position. If wantedPos is greater than the current
        position, then calling setPosition is the same as calling read, i.e.
        the skipped data will still be downloaded, although skipped bytes will
        be discarded immediately.

        @returns  true if the stream manages to reposition itself correctly
        @see getPosition
    */
    bool setPosition (int64 wantedPos) override;

 private:
    static void createHeadersAndPostData (const URL&, String&, MemoryBlock&);
    static StringPairArray parseHttpHeaders (const String& headerData);

    class Pimpl;
    friend class Pimpl;

    Pimpl* const pimpl;
    bool hasCalledConnect;

    JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (WebInputStream)
};

} // namespace juce

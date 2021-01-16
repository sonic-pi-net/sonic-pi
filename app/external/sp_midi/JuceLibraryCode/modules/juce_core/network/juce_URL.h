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

class WebInputStream;

//==============================================================================
/**
    Represents a URL and has a bunch of useful functions to manipulate it.

    This class can be used to launch URLs in browsers, and also to create
    InputStreams that can read from remote http or ftp sources.

    @tags{Core}
*/
class JUCE_API  URL
{
public:
    //==============================================================================
    /** Creates an empty URL. */
    URL();

    /** Creates a URL from a string.
        This will parse any embedded parameters after a '?' character and store them
        in the list (see getParameterNames etc). If you don't want this to happen, you
        can use createWithoutParsing().
    */
    URL (const String& url);

    URL (const URL&) = default;
    URL& operator= (const URL&) = default;
    URL (URL&&) = default;
    URL& operator= (URL&&) = default;

    /** Creates URL referring to a local file on your disk using the file:// scheme. */
    explicit URL (File);

    /** Destructor. */
    ~URL() = default;

    /** Compares two URLs.
        All aspects of the URLs must be identical for them to match, including any parameters,
        upload files, etc.
    */
    bool operator== (const URL&) const;
    bool operator!= (const URL&) const;

    //==============================================================================
    /** Returns a string version of the URL.

        If includeGetParameters is true and any parameters have been set with the
        withParameter() method, then the string will have these appended on the
        end and url-encoded.
    */
    String toString (bool includeGetParameters) const;

    /** Returns true if the URL is an empty string. */
    bool isEmpty() const noexcept;

    /** True if it seems to be valid. */
    bool isWellFormed() const;

    /** Returns just the domain part of the URL.
        E.g. for "http://www.xyz.com/foobar", this will return "www.xyz.com".
    */
    String getDomain() const;

    /** Returns the path part of the URL.
        E.g. for "http://www.xyz.com/foo/bar?x=1", this will return "foo/bar".

        If includeGetParameters is true and any parameters have been set with the
        withParameter() method, then the string will have these appended on the
        end and url-encoded.
    */
    String getSubPath (bool includeGetParameters = false) const;

    /** If any parameters are set, returns these URL encoded, including the "?"
     *  prefix.
    */
    String getQueryString() const;

    /** Returns the scheme of the URL.
        E.g. for "http://www.xyz.com/foobar", this will return "http". (It won't
        include the colon).
    */
    String getScheme() const;

    /** Returns true if this URL refers to a local file. */
    bool isLocalFile() const;

    /** Returns the file path of the local file to which this URL refers to.
        If the URL does not represent a local file URL (i.e. the URL's scheme is not 'file')
        then this method will assert.

        This method also supports converting Android's content:// URLs to
        local file paths.

        @see isLocalFile
    */
    File getLocalFile() const;

    /** Returns the file name. For all but Android's content:// scheme, it will
        simply return the last segment of the URL.
        E.g. for "http://www.xyz.com/foo/bar.txt", this will return "bar.txt".

        For Android's content:// scheme, it will attempt to resolve the filename
        located under the URL.
    */
    String getFileName() const;

    /** Attempts to read a port number from the URL.
        @returns the port number, or 0 if none is explicitly specified.
    */
    int getPort() const;

    /** Returns a new version of this URL with a different domain and path.
        E.g. if the URL is "http://www.xyz.com/foo?x=1" and you call this with
        "abc.com/zzz", it'll return "http://abc.com/zzz?x=1".
        @see withNewSubPath
    */
    URL withNewDomainAndPath (const String& newFullPath) const;

    /** Returns a new version of this URL with a different sub-path.
        E.g. if the URL is "http://www.xyz.com/foo?x=1" and you call this with
        "bar", it'll return "http://www.xyz.com/bar?x=1".
        @see withNewDomainAndPath
    */
    URL withNewSubPath (const String& newPath) const;

    /** Attempts to return a URL which is the parent folder containing this URL.
        If there isn't a parent, this method will just return a copy of this URL.
    */
    URL getParentURL() const;

    /** Returns a new URL that refers to a sub-path relative to this one.
        E.g. if the URL is "http://www.xyz.com/foo" and you call this with
        "bar", it'll return "http://www.xyz.com/foo/bar". Note that there's no way for
        this method to know whether the original URL is a file or directory, so it's
        up to you to make sure it's a directory. It also won't attempt to be smart about
        the content of the childPath string, so if this string is an absolute URL, it'll
        still just get bolted onto the end of the path.

        @see File::getChildFile
    */
    URL getChildURL (const String& subPath) const;

    //==============================================================================
    /** Returns a copy of this URL, with a GET or POST parameter added to the end.

        Any control characters in the value will be encoded.
        e.g. calling "withParameter ("amount", "some fish") for the url "www.fish.com"
        would produce a new url whose toString(true) method would return
        "www.fish.com?amount=some+fish".

        @see getParameterNames, getParameterValues
    */
    URL withParameter (const String& parameterName,
                       const String& parameterValue) const;

    /** Returns a copy of this URL, with a set of GET or POST parameters added.
        This is a convenience method, equivalent to calling withParameter for each value.
        @see withParameter
    */
    URL withParameters (const StringPairArray& parametersToAdd) const;

    /** Returns a copy of this URL, with a file-upload type parameter added to it.

        When performing a POST where one of your parameters is a binary file, this
        lets you specify the file.

        Note that the filename is stored, but the file itself won't actually be read
        until this URL is later used to create a network input stream. If you want to
        upload data from memory, use withDataToUpload().

        @see withDataToUpload
    */
    URL withFileToUpload (const String& parameterName,
                          const File& fileToUpload,
                          const String& mimeType) const;

    /** Returns a copy of this URL, with a file-upload type parameter added to it.

        When performing a POST where one of your parameters is a binary file, this
        lets you specify the file content.
        Note that the filename parameter should not be a full path, it's just the
        last part of the filename.

        @see withFileToUpload
    */
    URL withDataToUpload (const String& parameterName,
                          const String& filename,
                          const MemoryBlock& fileContentToUpload,
                          const String& mimeType) const;

    /** Returns an array of the names of all the URL's parameters.

        E.g. for the url "www.fish.com?type=haddock&amount=some+fish", this array would
        contain two items: "type" and "amount".

        You can call getParameterValues() to get the corresponding value of each
        parameter. Note that the list can contain multiple parameters with the same name.

        @see getParameterValues, withParameter
    */
    const StringArray& getParameterNames() const noexcept       { return parameterNames; }

    /** Returns an array of the values of all the URL's parameters.

        E.g. for the url "www.fish.com?type=haddock&amount=some+fish", this array would
        contain two items: "haddock" and "some fish".

        The values returned will have been cleaned up to remove any escape characters.

        You can call getParameterNames() to get the corresponding name of each
        parameter. Note that the list can contain multiple parameters with the same name.

        @see getParameterNames, withParameter
    */
    const StringArray& getParameterValues() const noexcept      { return parameterValues; }

    /** Returns a copy of this URL, with a block of data to send as the POST data.

        If you're setting the POST data, be careful not to have any parameters set
        as well, otherwise it'll all get thrown in together, and might not have the
        desired effect.

        If the URL already contains some POST data, this will replace it, rather
        than being appended to it.

        This data will only be used if you specify a post operation when you call
        createInputStream().
    */
    URL withPOSTData (const String& postData) const;

    /** Returns a copy of this URL, with a block of data to send as the POST data.

        If you're setting the POST data, be careful not to have any parameters set
        as well, otherwise it'll all get thrown in together, and might not have the
        desired effect.

        If the URL already contains some POST data, this will replace it, rather
        than being appended to it.

        This data will only be used if you specify a post operation when you call
        createInputStream().
    */
    URL withPOSTData (const MemoryBlock& postData) const;

    /** Returns the data that was set using withPOSTData(). */
    String getPostData() const                                      { return postData.toString(); }

    /** Returns the data that was set using withPOSTData() as MemoryBlock. */
    const MemoryBlock& getPostDataAsMemoryBlock() const noexcept    { return postData; }

    //==============================================================================
    /** Tries to launch the system's default browser to open the URL.
        Returns true if this seems to have worked.
    */
    bool launchInDefaultBrowser() const;

    //==============================================================================
    /** Takes a guess as to whether a string might be a valid website address.
        This isn't foolproof!
    */
    static bool isProbablyAWebsiteURL (const String& possibleURL);

    /** Takes a guess as to whether a string might be a valid email address.
        This isn't foolproof!
    */
    static bool isProbablyAnEmailAddress (const String& possibleEmailAddress);

    //==============================================================================
    /** This callback function can be used by the createInputStream() method.

        It allows your app to receive progress updates during a lengthy POST operation. If you
        want to continue the operation, this should return true, or false to abort.
    */
    using OpenStreamProgressCallback = bool (void* context, int bytesSent, int totalBytes);

    /** Attempts to open a stream that can read from this URL.

        Note that this method will block until the first byte of data has been received or an
        error has occurred.

        Note that on some platforms (Android, for example) it's not permitted to do any network
        action from the message thread, so you must only call it from a background thread.

        Unless the URL represents a local file, this method returns an instance of a
        WebInputStream. You can use dynamic_cast to cast the return value to a WebInputStream
        which allows you more fine-grained control of the transfer process.

        If the URL represents a local file, then this method simply returns a FileInputStream.

        @param doPostLikeRequest if true, the parameters added to this class will be transferred
                                 via the HTTP headers which is typical for POST requests. Otherwise
                                 the parameters will be added to the URL address. Additionally,
                                 if the parameter httpRequestCmd is not specified (or empty) then this
                                 parameter will determine which HTTP request command will be used
                                 (POST or GET).
        @param progressCallback  if this is not a nullptr, it lets you supply a callback function
                                 to keep track of the operation's progress. This can be useful
                                 for lengthy POST operations, so that you can provide user feedback.
        @param progressCallbackContext  if a callback is specified, this value will be passed to
                                 the function
        @param extraHeaders      if not empty, this string is appended onto the headers that
                                 are used for the request. It must therefore be a valid set of HTML
                                 header directives, separated by newlines.
        @param connectionTimeOutMs  if 0, this will use whatever default setting the OS chooses. If
                                 a negative number, it will be infinite. Otherwise it specifies a
                                 time in milliseconds.
        @param responseHeaders   if this is non-null, all the (key, value) pairs received as headers
                                 in the response will be stored in this array
        @param statusCode        if this is non-null, it will get set to the http status code, if one
                                 is known, or 0 if a code isn't available
        @param numRedirectsToFollow specifies the number of redirects that will be followed before
                                 returning a response (ignored for Android which follows up to 5 redirects)
        @param httpRequestCmd    Specify which HTTP Request to use. If this is empty, then doPostRequest
                                 will determine the HTTP request.
        @returns    a valid input stream, or nullptr if there was an error trying to open it.
     */
    std::unique_ptr<InputStream> createInputStream (bool doPostLikeRequest,
                                                    OpenStreamProgressCallback* progressCallback = nullptr,
                                                    void* progressCallbackContext = nullptr,
                                                    String extraHeaders = {},
                                                    int connectionTimeOutMs = 0,
                                                    StringPairArray* responseHeaders = nullptr,
                                                    int* statusCode = nullptr,
                                                    int numRedirectsToFollow = 5,
                                                    String httpRequestCmd = {}) const;

    /** Attempts to open an output stream to a URL for writing

        This method can only be used for certain scheme types such as local files
        and content:// URIs on Android.
    */
    std::unique_ptr<OutputStream> createOutputStream() const;

    //==============================================================================
    /** Represents a download task.
        Returned by downloadToFile to allow querying and controlling the download task.
    */
    class JUCE_API  DownloadTask
    {
    public:
        /** Used to receive callbacks for download progress */
        struct JUCE_API  Listener
        {
            virtual ~Listener();

            /** Called when the download has finished. Be aware that this callback may
                come on an arbitrary thread. */
            virtual void finished (URL::DownloadTask* task, bool success) = 0;

            /** Called periodically by the OS to indicate download progress.
                Beware that this callback may come on an arbitrary thread.
            */
            virtual void progress (URL::DownloadTask* task, int64 bytesDownloaded, int64 totalLength);
        };

        /** Releases the resources of the download task, unregisters the listener
            and cancels the download if necessary. */
        virtual ~DownloadTask();

        /** Returns the total length of the download task. This may return -1 if the length
            was not returned by the server. */
        int64 getTotalLength() const                      { return contentLength; }

        /** Returns the number of bytes that have been downloaded so far. */
        int64 getLengthDownloaded() const                 { return downloaded; }

        /** Returns true if the download finished or there was an error. */
        bool isFinished() const                           { return finished; }

        /** Returns the status code of the server's response.
            This will only be valid after the download has finished.
            @see isFinished
        */
        int statusCode() const                            { return httpCode; }

        /** Returns true if there was an error. */
        inline bool hadError() const                      { return error; }

        /** Returns the target file location that was provided in URL::downloadToFile. */
        File getTargetLocation() const                    { return targetLocation; }

    protected:
        int64 contentLength = -1, downloaded = 0;
        bool finished = false, error = false;
        int httpCode = -1;
        File targetLocation;

        DownloadTask();

    private:
        friend class URL;
        static std::unique_ptr<DownloadTask> createFallbackDownloader (const URL&, const File&, const String&, Listener*, bool);

    public:
       #if JUCE_IOS
        /** internal **/
        static void juce_iosURLSessionNotify (const String&);
       #endif

    private:
        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (DownloadTask)
    };

    /** Download the URL to a file.

        This method attempts to download the URL to a given file location.

        Using this method to download files on mobile is less flexible but more reliable
        than using createInputStream or WebInputStreams as it will attempt to download the file
        using a native OS background network task. Such tasks automatically deal with
        network re-connections and continuing your download while your app is suspended.
    */
    std::unique_ptr<DownloadTask> downloadToFile (const File& targetLocation,
                                                  String extraHeaders = String(),
                                                  DownloadTask::Listener* listener = nullptr,
                                                  bool usePostCommand = false);

    //==============================================================================
    /** Tries to download the entire contents of this URL into a binary data block.

        If it succeeds, this will return true and append the data it read onto the end
        of the memory block.

        Note that on some platforms (Android, for example) it's not permitted to do any network
        action from the message thread, so you must only call it from a background thread.

        @param destData         the memory block to append the new data to
        @param usePostCommand   whether to use a POST command to get the data (uses
                                a GET command if this is false)
        @see readEntireTextStream, readEntireXmlStream
    */
    bool readEntireBinaryStream (MemoryBlock& destData,
                                 bool usePostCommand = false) const;

    /** Tries to download the entire contents of this URL as a string.

        If it fails, this will return an empty string, otherwise it will return the
        contents of the downloaded file. If you need to distinguish between a read
        operation that fails and one that returns an empty string, you'll need to use
        a different method, such as readEntireBinaryStream().

        Note that on some platforms (Android, for example) it's not permitted to do any network
        action from the message thread, so you must only call it from a background thread.

        @param usePostCommand   whether to use a POST command to get the data (uses
                                a GET command if this is false)
        @see readEntireBinaryStream, readEntireXmlStream
    */
    String readEntireTextStream (bool usePostCommand = false) const;

    /** Tries to download the entire contents of this URL and parse it as XML.

        If it fails, or if the text that it reads can't be parsed as XML, this will
        return nullptr.

        Note that on some platforms (Android, for example) it's not permitted to do any network
        action from the message thread, so you must only call it from a background thread.

        @param usePostCommand   whether to use a POST command to get the data (uses
                                a GET command if this is false)

        @see readEntireBinaryStream, readEntireTextStream
    */
    std::unique_ptr<XmlElement> readEntireXmlStream (bool usePostCommand = false) const;

    //==============================================================================
    /** Adds escape sequences to a string to encode any characters that aren't
        legal in a URL.

        E.g. any spaces will be replaced with "%20".

        This is the opposite of removeEscapeChars().

        @param stringToAddEscapeCharsTo The string to escape.
        @param isParameter              If true then the string is going to be
                                        used as a parameter, so it also encodes
                                        '$' and ',' (which would otherwise be
                                        legal in a URL.
        @param roundBracketsAreLegal    Technically round brackets are ok in URLs,
                                        however, some servers (like AWS) also want
                                        round brackets to be escaped.

        @see removeEscapeChars
    */
    static String addEscapeChars (const String& stringToAddEscapeCharsTo,
                                  bool isParameter,
                                  bool roundBracketsAreLegal = true);

    /** Replaces any escape character sequences in a string with their original
        character codes.

        E.g. any instances of "%20" will be replaced by a space.

        This is the opposite of addEscapeChars().

        @see addEscapeChars
    */
    static String removeEscapeChars (const String& stringToRemoveEscapeCharsFrom);

    /** Returns a URL without attempting to remove any embedded parameters from the string.
        This may be necessary if you need to create a request that involves both POST
        parameters and parameters which are embedded in the URL address itself.
    */
    static URL createWithoutParsing (const String& url);

private:
    //==============================================================================
    friend class WebInputStream;

    String url;
    MemoryBlock postData;
    StringArray parameterNames, parameterValues;

    static File fileFromFileSchemeURL (const URL&);
    String getDomainInternal (bool) const;

    struct Upload  : public ReferenceCountedObject
    {
        Upload (const String&, const String&, const String&, const File&, MemoryBlock*);
        String parameterName, filename, mimeType;
        File file;
        std::unique_ptr<MemoryBlock> data;

        JUCE_DECLARE_NON_COPYABLE_WITH_LEAK_DETECTOR (Upload)
    };

    ReferenceCountedArray<Upload> filesToUpload;

  #if JUCE_IOS
    struct Bookmark : public ReferenceCountedObject
    {
        using Ptr = ReferenceCountedObjectPtr<Bookmark>;

        Bookmark (void*);
        ~Bookmark();

        void* data;
    };

    Bookmark::Ptr bookmark;

    friend void setURLBookmark (URL&, void*);
    friend void* getURLBookmark (URL&);
  #endif

    URL (const String&, int);
    void init();
    void addParameter (const String&, const String&);
    void createHeadersAndPostData (String&, MemoryBlock&) const;
    URL withUpload (Upload*) const;

    JUCE_LEAK_DETECTOR (URL)
};

} // namespace juce
